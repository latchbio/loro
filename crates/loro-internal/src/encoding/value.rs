use std::sync::Arc;

use enum_as_inner::EnumAsInner;
use fxhash::FxHashMap;
use loro_common::{
    ContainerID, ContainerType, Counter, InternalString, LoroError, LoroResult, LoroValue, PeerID,
    TreeID, ID,
};

use crate::encoding::encode_reordered::MAX_COLLECTION_SIZE;

use super::arena::{DecodedArenas, EncodedRegisters};

#[derive(Debug)]
pub enum ValueKind {
    Null,           // 0
    True,           // 1
    False,          // 2
    I64,            // 3
    F64,            // 4
    Str,            // 5
    Binary,         // 6
    ContainerType,  // 7
    DeleteOnce,     // 8
    DeleteSeq,      // 9
    DeltaInt,       // 10
    Array,          // 11
    Map,            // 12
    LoroValue,      // 13
    LoroValueArray, // 14
    MarkStart,      // 15

    Future(FutureValueKind),
}

#[derive(Debug)]
pub enum FutureValueKind {
    // TreeMove, // 0b10000016
    // start from 16
    Unknown(u8),
}

impl ValueKind {
    pub(super) fn to_u8(&self) -> u8 {
        match self {
            ValueKind::Null => 0,
            ValueKind::True => 1,
            ValueKind::False => 2,
            ValueKind::I64 => 3,
            ValueKind::F64 => 4,
            ValueKind::Str => 5,
            ValueKind::Binary => 6,
            ValueKind::ContainerType => 7,
            ValueKind::DeleteOnce => 8,
            ValueKind::DeleteSeq => 9,
            ValueKind::DeltaInt => 10,
            ValueKind::Array => 11,
            ValueKind::Map => 12,
            ValueKind::LoroValue => 13,
            ValueKind::LoroValueArray => 14,
            ValueKind::MarkStart => 15,
            ValueKind::Future(future_value_kind) => match future_value_kind {
                // FutureValueKind::TreeMove => 16 | 0x80,
                FutureValueKind::Unknown(u8) => *u8 | 0x80,
            },
        }
    }

    pub(super) fn from_u8(mut kind: u8) -> Self {
        kind &= 0x7F;
        match kind {
            0 => ValueKind::Null,
            1 => ValueKind::True,
            2 => ValueKind::False,
            3 => ValueKind::I64,
            4 => ValueKind::F64,
            5 => ValueKind::Str,
            6 => ValueKind::Binary,
            7 => ValueKind::ContainerType,
            8 => ValueKind::DeleteOnce,
            9 => ValueKind::DeleteSeq,
            10 => ValueKind::DeltaInt,
            11 => ValueKind::Array,
            12 => ValueKind::Map,
            13 => ValueKind::LoroValue,
            14 => ValueKind::LoroValueArray,
            15 => ValueKind::MarkStart,
            // 16 => ValueKind::Future(FutureValueKind::TreeMove),
            _ => ValueKind::Future(FutureValueKind::Unknown(kind)),
        }
    }
}

#[derive(Debug, EnumAsInner)]
pub enum Value<'a> {
    Null,
    True,
    False,
    I64(i64),
    F64(f64),
    Str(&'a str),
    Binary(&'a [u8]),
    ContainerIdx(usize),
    DeleteOnce,
    DeleteSeq,
    DeltaInt(i32),
    Array(Vec<Value<'a>>),
    LoroValueArray(Vec<LoroValue>),
    #[allow(clippy::enum_variant_names)]
    LoroValue(LoroValue),
    Map(FxHashMap<InternalString, Value<'a>>),
    MarkStart(MarkStart<'a>),
    Future {
        bytes_length: usize,
        value: FutureValue<'a>,
    },
}

#[derive(Debug)]
pub enum FutureValue<'a> {
    // TreeMove(EncodedTreeMove),
    // The future value cannot depend on the arena for encoding.
    Unknown { kind: u8, data: &'a [u8] },
}

impl<'a> Value<'a> {
    pub fn kind(&self) -> ValueKind {
        match self {
            Value::Null => ValueKind::Null,
            Value::True => ValueKind::True,
            Value::False => ValueKind::False,
            Value::DeleteOnce => ValueKind::DeleteOnce,
            Value::I64(_) => ValueKind::I64,
            Value::ContainerIdx(_) => ValueKind::ContainerType,
            Value::F64(_) => ValueKind::F64,
            Value::Str(_) => ValueKind::Str,
            Value::DeleteSeq { .. } => ValueKind::DeleteSeq,
            Value::DeltaInt(_) => ValueKind::DeltaInt,
            Value::Array(_) => ValueKind::Array,
            Value::Map(_) => ValueKind::Map,
            Value::LoroValue(_) => ValueKind::LoroValue,
            Value::LoroValueArray(_) => ValueKind::LoroValueArray,
            Value::MarkStart { .. } => ValueKind::MarkStart,
            Value::Binary(_) => ValueKind::Binary,
            Value::Future {
                bytes_length: _,
                value,
            } => match value {
                // FutureValue::TreeMove(..) => ValueKind::Future(FutureValueKind::TreeMove),
                FutureValue::Unknown { kind, data: _ } => {
                    ValueKind::Future(FutureValueKind::Unknown(*kind))
                }
            },
        }
    }
    fn decode_without_arena<'r: 'a>(
        future_kind: FutureValueKind,
        value_reader: &'r mut ValueReader,
    ) -> LoroResult<Self> {
        let bytes_length = value_reader.read_usize()?;
        let value = match future_kind {
            // FutureValueKind::TreeMove => FutureValue::TreeMove(value_reader.read_tree_move()?),
            FutureValueKind::Unknown(kind) => FutureValue::Unknown {
                kind,
                data: value_reader.take_bytes(bytes_length),
            },
        };
        Ok(Value::Future {
            bytes_length,
            value,
        })
    }

    pub(super) fn decode<'r: 'a>(
        kind: ValueKind,
        value_reader: &'r mut ValueReader,
        arenas: &'a DecodedArenas<'a>,
        id: ID,
    ) -> LoroResult<Self> {
        Ok(match kind {
            ValueKind::Null => Value::Null,
            ValueKind::True => Value::True,
            ValueKind::False => Value::False,
            ValueKind::I64 => Value::I64(value_reader.read_i64()?),
            ValueKind::F64 => Value::F64(value_reader.read_f64()?),
            ValueKind::Str => Value::Str(value_reader.read_str()?),
            ValueKind::Binary => Value::Binary(value_reader.read_binary()?),
            ValueKind::ContainerType => Value::ContainerIdx(value_reader.read_usize()?),
            ValueKind::DeleteOnce => Value::DeleteOnce,
            ValueKind::DeleteSeq => Value::DeleteSeq,
            ValueKind::DeltaInt => Value::DeltaInt(value_reader.read_i32()?),
            ValueKind::LoroValueArray => {
                let len = value_reader.read_usize()?;
                let mut ans = Vec::with_capacity(len);
                for _ in 0..len {
                    let loro_value =
                        value_reader.read_value_type_and_content(&arenas.keys.keys, id)?;
                    ans.push(loro_value);
                }
                Value::LoroValueArray(ans)
            }
            ValueKind::LoroValue => {
                Value::LoroValue(value_reader.read_value_type_and_content(&arenas.keys, id)?)
            }
            ValueKind::Array => unimplemented!(),
            ValueKind::Map => unimplemented!(),
            ValueKind::MarkStart => {
                Value::MarkStart(value_reader.read_mark(&arenas.keys.keys, id)?)
            }
            ValueKind::Future(future_kind) => {
                Self::decode_without_arena(future_kind, value_reader)?
            }
        })
    }

    fn encode_without_registers(value: FutureValue, value_writer: &mut ValueWriter) -> ValueKind {
        match value {
            // FutureValue::TreeMove(tree) => {
            //     let bytes = write_tree_move(&tree);
            //     // value_writer.write_usize(bytes.len());
            //     value_writer.write_binary(&bytes);
            //     ValueKind::Future(FutureValueKind::TreeMove)
            // }
            FutureValue::Unknown { kind, data } => {
                value_writer.write_binary(data);
                ValueKind::Future(FutureValueKind::Unknown(kind))
            }
        }
    }

    pub(super) fn encode(
        self,
        value_writer: &mut ValueWriter,
        registers: &mut EncodedRegisters,
    ) -> ValueKind {
        match self {
            Value::Null => ValueKind::Null,
            Value::True => ValueKind::True,
            Value::False => ValueKind::False,
            Value::I64(x) => {
                value_writer.write_i64(x);
                ValueKind::I64
            }
            Value::F64(x) => {
                value_writer.write_f64(x);
                ValueKind::F64
            }
            Value::Str(x) => {
                value_writer.write_str(x);
                ValueKind::Str
            }
            Value::Binary(x) => {
                value_writer.write_binary(x);
                ValueKind::Binary
            }
            Value::ContainerIdx(x) => {
                value_writer.write_usize(x);
                ValueKind::ContainerType
            }
            Value::DeleteOnce => ValueKind::DeleteOnce,
            Value::DeleteSeq => ValueKind::DeleteSeq,
            Value::DeltaInt(x) => {
                value_writer.write_i32(x);
                ValueKind::DeltaInt
            }
            Value::LoroValueArray(arr) => {
                value_writer.write_usize(arr.len());
                for value in arr {
                    value_writer.write_value_type_and_content(&value, registers);
                }
                ValueKind::LoroValueArray
            }
            Value::LoroValue(x) => {
                value_writer.write_value_type_and_content(&x, registers);
                ValueKind::LoroValue
            }
            Value::Array(x) => {
                value_writer.write_array(x, registers);
                ValueKind::Array
            }
            Value::Map(x) => {
                value_writer.write_map(x, registers);
                ValueKind::Map
            }
            Value::MarkStart(x) => {
                value_writer.write_mark(x, registers);
                ValueKind::MarkStart
            }
            Value::Future {
                bytes_length: _,
                value,
            } => Self::encode_without_registers(value, value_writer),
        }
    }
}

#[derive(Debug)]
pub struct MarkStart<'a> {
    pub len: u32,
    pub key: &'a InternalString,
    pub value: LoroValue,
    pub info: u8,
}

pub struct EncodedTreeMove {
    pub subject_peer: PeerID,
    pub subject_cnt: Counter,
    pub is_parent_null: bool,
    pub parent_peer: PeerID,
    pub parent_cnt: Counter,
}

// impl EncodedTreeMove {
//     pub fn from_op(op: &TreeOp) -> Self {
//         Self {
//             subject_peer: op.target.peer,
//             subject_cnt: op.target.counter,
//             is_parent_null: op.parent.is_none(),
//             parent_peer: op.parent.map(|x| x.peer).unwrap_or(0),
//             parent_cnt: op.parent.map(|x| x.counter).unwrap_or(0),
//         }
//     }

//     pub fn as_tree_op(&self) -> TreeOp {
//         TreeOp {
//             target: TreeID::new(self.subject_peer, self.subject_cnt),
//             parent: if self.is_parent_null {
//                 None
//             } else {
//                 Some(TreeID::new(self.parent_peer, self.parent_cnt))
//             },
//         }
//     }
// }

pub struct ValueWriter {
    buffer: Vec<u8>,
}

pub struct ValueReader<'a> {
    raw: &'a [u8],
}

impl<'a> ValueReader<'a> {
    pub fn new(raw: &'a [u8]) -> Self {
        ValueReader { raw }
    }

    pub fn read_value_type_and_content(
        &mut self,
        keys: &[InternalString],
        id: ID,
    ) -> LoroResult<LoroValue> {
        let kind = self.read_u8()?;
        self.read_value_content(ValueKind::from_u8(kind), keys, id)
    }

    pub fn read_value_content(
        &mut self,
        kind: ValueKind,
        keys: &[InternalString],
        id: ID,
    ) -> LoroResult<LoroValue> {
        Ok(match kind {
            ValueKind::Null => LoroValue::Null,
            ValueKind::True => LoroValue::Bool(true),
            ValueKind::False => LoroValue::Bool(false),
            ValueKind::I64 => LoroValue::I64(self.read_i64()?),
            ValueKind::F64 => LoroValue::Double(self.read_f64()?),
            ValueKind::Str => LoroValue::String(Arc::new(self.read_str()?.to_owned())),
            ValueKind::DeltaInt => LoroValue::I64(self.read_i64()?),
            ValueKind::Array => {
                let len = self.read_usize()?;
                if len > MAX_COLLECTION_SIZE {
                    return Err(LoroError::DecodeDataCorruptionError);
                }
                let mut ans = Vec::with_capacity(len);
                for i in 0..len {
                    ans.push(self.recursive_read_value_type_and_content(keys, id.inc(i as i32))?);
                }
                ans.into()
            }
            ValueKind::Map => {
                let len = self.read_usize()?;
                if len > MAX_COLLECTION_SIZE {
                    return Err(LoroError::DecodeDataCorruptionError);
                }
                let mut ans = FxHashMap::with_capacity_and_hasher(len, Default::default());
                for _ in 0..len {
                    let key_idx = self.read_usize()?;
                    let key = keys
                        .get(key_idx)
                        .ok_or(LoroError::DecodeDataCorruptionError)?
                        .to_string();
                    let value = self.recursive_read_value_type_and_content(keys, id)?;
                    ans.insert(key, value);
                }
                ans.into()
            }
            ValueKind::Binary => LoroValue::Binary(Arc::new(self.read_binary()?.to_owned())),
            ValueKind::ContainerType => {
                let u8 = self.read_u8()?;
                let container_id = ContainerID::new_normal(
                    id,
                    ContainerType::try_from_u8(u8).unwrap_or(ContainerType::Unknown(u8)),
                );

                LoroValue::Container(container_id)
            }
            a => unreachable!("Unexpected value kind {:?}", a),
        })
    }

    /// Read a value that may be very deep efficiently.
    ///
    /// This method avoids using recursive calls to read deeply nested values.
    /// Otherwise, it may cause stack overflow.
    fn recursive_read_value_type_and_content(
        &mut self,
        keys: &[InternalString],
        id: ID,
    ) -> LoroResult<LoroValue> {
        #[derive(Debug)]
        enum Task {
            Init,
            ReadList {
                left: usize,
                vec: Vec<LoroValue>,
                key_idx_in_parent: usize,
            },
            ReadMap {
                left: usize,
                map: FxHashMap<String, LoroValue>,
                key_idx_in_parent: usize,
            },
        }
        impl Task {
            fn should_read(&self) -> bool {
                !matches!(
                    self,
                    Self::ReadList { left: 0, .. } | Self::ReadMap { left: 0, .. }
                )
            }

            fn key_idx(&self) -> usize {
                match self {
                    Self::ReadList {
                        key_idx_in_parent, ..
                    } => *key_idx_in_parent,
                    Self::ReadMap {
                        key_idx_in_parent, ..
                    } => *key_idx_in_parent,
                    _ => unreachable!(),
                }
            }

            fn into_value(self) -> LoroValue {
                match self {
                    Self::ReadList { vec, .. } => vec.into(),
                    Self::ReadMap { map, .. } => map.into(),
                    _ => unreachable!(),
                }
            }
        }
        let mut stack = vec![Task::Init];
        while let Some(mut task) = stack.pop() {
            if task.should_read() {
                let key_idx = if matches!(task, Task::ReadMap { .. }) {
                    self.read_usize()?
                } else {
                    0
                };
                let kind = self.read_u8()?;
                let kind = ValueKind::from_u8(kind);
                let value = match kind {
                    ValueKind::Null => LoroValue::Null,
                    ValueKind::True => LoroValue::Bool(true),
                    ValueKind::False => LoroValue::Bool(false),
                    ValueKind::I64 => LoroValue::I64(self.read_i64()?),
                    ValueKind::F64 => LoroValue::Double(self.read_f64()?),
                    ValueKind::Str => LoroValue::String(Arc::new(self.read_str()?.to_owned())),
                    ValueKind::DeltaInt => LoroValue::I64(self.read_i64()?),
                    ValueKind::LoroValueArray => {
                        let len = self.read_usize()?;
                        if len > MAX_COLLECTION_SIZE {
                            return Err(LoroError::DecodeDataCorruptionError);
                        }

                        let ans = Vec::with_capacity(len);
                        stack.push(task);
                        stack.push(Task::ReadList {
                            left: len,
                            vec: ans,
                            key_idx_in_parent: key_idx,
                        });
                        continue;
                    }
                    ValueKind::Map => {
                        let len = self.read_usize()?;
                        if len > MAX_COLLECTION_SIZE {
                            return Err(LoroError::DecodeDataCorruptionError);
                        }

                        let ans = FxHashMap::with_capacity_and_hasher(len, Default::default());
                        stack.push(task);
                        stack.push(Task::ReadMap {
                            left: len,
                            map: ans,
                            key_idx_in_parent: key_idx,
                        });
                        continue;
                    }
                    ValueKind::Binary => {
                        LoroValue::Binary(Arc::new(self.read_binary()?.to_owned()))
                    }
                    ValueKind::ContainerType => {
                        let u8 = self.read_u8()?;
                        let container_id = ContainerID::new_normal(
                            id,
                            ContainerType::try_from_u8(u8).unwrap_or(ContainerType::Unknown(u8)),
                        );

                        LoroValue::Container(container_id)
                    }
                    a => unreachable!("Unexpected value kind {:?}", a),
                };

                task = match task {
                    Task::Init => {
                        return Ok(value);
                    }
                    Task::ReadList {
                        mut left,
                        mut vec,
                        key_idx_in_parent,
                    } => {
                        left -= 1;
                        vec.push(value);
                        let task = Task::ReadList {
                            left,
                            vec,
                            key_idx_in_parent,
                        };
                        if left != 0 {
                            stack.push(task);
                            continue;
                        }

                        task
                    }
                    Task::ReadMap {
                        mut left,
                        mut map,
                        key_idx_in_parent,
                    } => {
                        left -= 1;
                        let key = keys
                            .get(key_idx)
                            .ok_or(LoroError::DecodeDataCorruptionError)?
                            .to_string();
                        map.insert(key, value);
                        let task = Task::ReadMap {
                            left,
                            map,
                            key_idx_in_parent,
                        };
                        if left != 0 {
                            stack.push(task);
                            continue;
                        }
                        task
                    }
                };
            }

            let key_index = task.key_idx();
            let value = task.into_value();
            if let Some(last) = stack.last_mut() {
                match last {
                    Task::Init => {
                        return Ok(value);
                    }
                    Task::ReadList { left, vec, .. } => {
                        *left -= 1;
                        vec.push(value);
                    }
                    Task::ReadMap { left, map, .. } => {
                        *left -= 1;
                        let key = keys
                            .get(key_index)
                            .ok_or(LoroError::DecodeDataCorruptionError)?
                            .to_string();
                        map.insert(key, value);
                    }
                }
            } else {
                return Ok(value);
            }
        }

        unreachable!();
    }

    pub fn read_i64(&mut self) -> LoroResult<i64> {
        leb128::read::signed(&mut self.raw).map_err(|_| LoroError::DecodeDataCorruptionError)
    }

    pub fn read_u64(&mut self) -> LoroResult<u64> {
        leb128::read::unsigned(&mut self.raw).map_err(|_| LoroError::DecodeDataCorruptionError)
    }

    #[allow(unused)]
    pub fn read_i32(&mut self) -> LoroResult<i32> {
        leb128::read::signed(&mut self.raw)
            .map(|x| x as i32)
            .map_err(|_| LoroError::DecodeDataCorruptionError)
    }

    fn read_f64(&mut self) -> LoroResult<f64> {
        if self.raw.len() < 8 {
            return Err(LoroError::DecodeDataCorruptionError);
        }

        let mut bytes = [0; 8];
        bytes.copy_from_slice(&self.raw[..8]);
        self.raw = &self.raw[8..];
        Ok(f64::from_be_bytes(bytes))
    }

    pub fn read_usize(&mut self) -> LoroResult<usize> {
        Ok(leb128::read::unsigned(&mut self.raw)
            .map_err(|_| LoroError::DecodeDataCorruptionError)? as usize)
    }

    pub fn read_str(&mut self) -> LoroResult<&'a str> {
        let len = self.read_usize()?;
        if self.raw.len() < len {
            return Err(LoroError::DecodeDataCorruptionError);
        }

        let ans = std::str::from_utf8(&self.raw[..len]).unwrap();
        self.raw = &self.raw[len..];
        Ok(ans)
    }

    fn read_u8(&mut self) -> LoroResult<u8> {
        if self.raw.is_empty() {
            return Err(LoroError::DecodeDataCorruptionError);
        }

        let ans = self.raw[0];
        self.raw = &self.raw[1..];
        Ok(ans)
    }

    pub fn read_binary(&mut self) -> LoroResult<&'a [u8]> {
        let len = self.read_usize()?;
        if self.raw.len() < len {
            return Err(LoroError::DecodeDataCorruptionError);
        }

        let ans = &self.raw[..len];
        self.raw = &self.raw[len..];
        Ok(ans)
    }

    pub fn read_mark<'s: 'm, 'm>(
        &mut self,
        keys: &'s [InternalString],
        id: ID,
    ) -> LoroResult<MarkStart<'m>> {
        let info = self.read_u8()?;
        let len = self.read_usize()?;
        let key_idx = self.read_usize()?;
        let value = self.read_value_type_and_content(keys, id)?;
        Ok(MarkStart {
            len: len as u32,
            key: keys
                .get(key_idx)
                .ok_or(LoroError::DecodeDataCorruptionError)?,
            value,
            info,
        })
    }

    pub fn take_bytes(&mut self, len: usize) -> &'a [u8] {
        let ans = &self.raw[..len];
        self.raw = &self.raw[len..];
        ans
    }

    pub fn read_tree_move(&mut self) -> LoroResult<EncodedTreeMove> {
        let subject_peer = self.read_u64()?;
        let subject_cnt = self.read_i32()?;
        let is_parent_null = self.read_u8()? != 0;
        let mut parent_peer = 0;
        let mut parent_cnt = 0;
        if !is_parent_null {
            parent_peer = self.read_u64()?;
            parent_cnt = self.read_i32()?;
        }

        Ok(EncodedTreeMove {
            subject_peer,
            subject_cnt,
            is_parent_null,
            parent_peer,
            parent_cnt,
        })
    }
}

impl ValueWriter {
    pub fn new() -> Self {
        ValueWriter { buffer: Vec::new() }
    }

    pub fn write_value_type_and_content(
        &mut self,
        value: &LoroValue,
        registers: &mut EncodedRegisters,
    ) -> (ValueKind, usize) {
        let len = self.write_u8(get_loro_value_kind(value).to_u8());
        let (kind, l) = self.write_value_content(value, registers);
        (kind, len + l)
    }

    pub fn write_value_content(
        &mut self,
        value: &LoroValue,
        registers: &mut EncodedRegisters,
    ) -> (ValueKind, usize) {
        match value {
            LoroValue::Null => (ValueKind::Null, 0),
            LoroValue::Bool(true) => (ValueKind::True, 0),
            LoroValue::Bool(false) => (ValueKind::False, 0),
            LoroValue::I64(value) => (ValueKind::I64, self.write_i64(*value)),
            LoroValue::Double(value) => (ValueKind::F64, self.write_f64(*value)),
            LoroValue::String(value) => (ValueKind::Str, self.write_str(value)),
            LoroValue::List(value) => {
                let mut len = self.write_usize(value.len());
                for value in value.iter() {
                    let (_, l) = self.write_value_type_and_content(value, registers);
                    len += l;
                }
                (ValueKind::Array, len)
            }
            LoroValue::Map(value) => {
                let mut len = self.write_usize(value.len());
                for (key, value) in value.iter() {
                    let key_idx = registers.key.register(&key.as_str().into());
                    len += self.write_usize(key_idx);
                    let (_, l) = self.write_value_type_and_content(value, registers);
                    len += l;
                }
                (ValueKind::Map, len)
            }
            LoroValue::Binary(value) => (ValueKind::Binary, self.write_binary(value)),
            LoroValue::Container(c) => (
                ValueKind::ContainerType,
                self.write_u8(c.container_type().to_u8()),
            ),
        }
    }

    pub fn write_i64(&mut self, value: i64) -> usize {
        let len = self.buffer.len();
        leb128::write::signed(&mut self.buffer, value).unwrap();
        self.buffer.len() - len
    }

    fn write_i32(&mut self, value: i32) -> usize {
        let len = self.buffer.len();
        leb128::write::signed(&mut self.buffer, value as i64).unwrap();
        self.buffer.len() - len
    }

    fn write_u64(&mut self, value: u64) -> usize {
        let len = self.buffer.len();
        leb128::write::unsigned(&mut self.buffer, value).unwrap();
        self.buffer.len() - len
    }

    fn write_usize(&mut self, value: usize) -> usize {
        let len = self.buffer.len();
        leb128::write::unsigned(&mut self.buffer, value as u64).unwrap();
        self.buffer.len() - len
    }

    fn write_f64(&mut self, value: f64) -> usize {
        let len = self.buffer.len();
        self.buffer.extend_from_slice(&value.to_be_bytes());
        self.buffer.len() - len
    }

    fn write_str(&mut self, value: &str) -> usize {
        let len = self.buffer.len();
        self.write_usize(value.len());
        self.buffer.extend_from_slice(value.as_bytes());
        self.buffer.len() - len
    }

    fn write_u8(&mut self, value: u8) -> usize {
        let len = self.buffer.len();
        self.buffer.push(value);
        self.buffer.len() - len
    }

    pub fn write_kind(&mut self, kind: ValueKind) -> usize {
        let len = self.buffer.len();
        self.write_u8(kind.to_u8());
        self.buffer.len() - len
    }

    fn write_array(&mut self, value: Vec<Value>, registers: &mut EncodedRegisters) -> usize {
        let len = self.buffer.len();
        self.write_usize(value.len());
        for value in value {
            self.write_kind(value.kind());
            value.encode(self, registers);
        }
        self.buffer.len() - len
    }

    fn write_map(
        &mut self,
        value: FxHashMap<InternalString, Value>,
        registers: &mut EncodedRegisters,
    ) -> usize {
        let len = self.buffer.len();
        self.write_usize(value.len());
        for (key, value) in value {
            let key_idx = registers.key.register(&key);
            self.write_usize(key_idx);
            self.write_kind(value.kind());
            value.encode(self, registers);
        }
        self.buffer.len() - len
    }

    fn write_binary(&mut self, value: &[u8]) -> usize {
        let len = self.buffer.len();
        self.write_usize(value.len());
        self.buffer.extend_from_slice(value);
        self.buffer.len() - len
    }

    fn write_mark(&mut self, mark: MarkStart, registers: &mut EncodedRegisters) -> usize {
        let key_idx = registers.key.register(mark.key);
        let len = self.buffer.len();
        self.write_u8(mark.info);
        self.write_usize(mark.len as usize);
        self.write_usize(key_idx);
        self.write_value_type_and_content(&mark.value, registers);
        self.buffer.len() - len
    }

    pub(crate) fn finish(self) -> Vec<u8> {
        self.buffer
    }
}

fn write_tree_move(op: &EncodedTreeMove) -> Vec<u8> {
    let mut writer = ValueWriter::new();
    writer.write_u64(op.subject_peer);
    writer.write_i32(op.subject_cnt);
    writer.write_u8(op.is_parent_null as u8);
    if op.is_parent_null {
        return writer.finish();
    }

    writer.write_u64(op.parent_peer);
    writer.write_i32(op.parent_cnt);
    writer.finish()
}

fn get_loro_value_kind(value: &LoroValue) -> ValueKind {
    match value {
        LoroValue::Null => ValueKind::Null,
        LoroValue::Bool(true) => ValueKind::True,
        LoroValue::Bool(false) => ValueKind::False,
        LoroValue::I64(_) => ValueKind::I64,
        LoroValue::Double(_) => ValueKind::F64,
        LoroValue::String(_) => ValueKind::Str,
        LoroValue::List(_) => ValueKind::LoroValueArray,
        LoroValue::Map(_) => ValueKind::Map,
        LoroValue::Binary(_) => ValueKind::Binary,
        LoroValue::Container(_) => ValueKind::ContainerType,
    }
}
