use std::any::Any;

use enum_as_inner::EnumAsInner;
use rle::{HasLength, Mergable, Sliceable};
use serde::{Deserialize, Serialize};

use crate::{
    container::{
        list::list_op::{InnerListOp, ListOp},
        map::MapSet,
        tree::tree_op::TreeOp,
    },
    encoding::OwnedValue,
};

/// @deprecated
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum ContentType {
    /// See [`crate::container::text::TextContent`]
    List,
    /// See [`crate::container::map::MapInsertContent`]
    Map,
    /// Users can define their own content types.
    Custom(u16),
}

#[derive(EnumAsInner, Debug, Clone)]
pub enum InnerContent {
    // The future content should not use any encoded arena context.
    Future(FutureInnerContent),
}

#[derive(EnumAsInner, Debug, Clone)]
pub enum FutureInnerContent {
    List(InnerListOp),
    Map(MapSet),
    Tree(TreeOp),
    Unknown { op_len: usize, value: OwnedValue },
}

// Note: It will be encoded into binary format, so the order of its fields should not be changed.
#[derive(EnumAsInner, Debug, PartialEq, Serialize, Deserialize)]
pub enum RawOpContent<'a> {
    #[serde(untagged)]
    Future(FutureRawOpContent<'a>),
}

#[derive(EnumAsInner, Debug, PartialEq, Serialize, Deserialize)]
pub enum FutureRawOpContent<'a> {
    Map(MapSet),
    List(ListOp<'a>),
    Tree(TreeOp),
    Unknown { op_len: usize, value: OwnedValue },
}

impl<'a> Clone for RawOpContent<'a> {
    fn clone(&self) -> Self {
        match self {
            Self::Future(f) => Self::Future(match f {
                FutureRawOpContent::Map(arg0) => FutureRawOpContent::Map(arg0.clone()),
                FutureRawOpContent::List(arg0) => FutureRawOpContent::List(arg0.clone()),
                FutureRawOpContent::Tree(arg0) => FutureRawOpContent::Tree(*arg0),
                FutureRawOpContent::Unknown { op_len, value } => FutureRawOpContent::Unknown {
                    op_len: *op_len,
                    value: value.clone(),
                },
            }),
        }
    }
}

impl<'a> RawOpContent<'a> {
    pub fn to_static(&self) -> RawOpContent<'static> {
        match self {
            Self::Future(f) => RawOpContent::Future(match f {
                FutureRawOpContent::Map(arg0) => FutureRawOpContent::Map(arg0.clone()),
                FutureRawOpContent::List(arg0) => match arg0 {
                    ListOp::Insert { slice, pos } => FutureRawOpContent::List(ListOp::Insert {
                        slice: slice.to_static(),
                        pos: *pos,
                    }),
                    ListOp::Delete(x) => FutureRawOpContent::List(ListOp::Delete(*x)),
                    ListOp::StyleStart {
                        start,
                        end,
                        key,
                        value,
                        info,
                    } => FutureRawOpContent::List(ListOp::StyleStart {
                        start: *start,
                        end: *end,
                        key: key.clone(),
                        value: value.clone(),
                        info: *info,
                    }),
                    ListOp::StyleEnd => FutureRawOpContent::List(ListOp::StyleEnd),
                },
                FutureRawOpContent::Tree(arg0) => FutureRawOpContent::Tree(*arg0),
                FutureRawOpContent::Unknown { op_len, value } => FutureRawOpContent::Unknown {
                    op_len: *op_len,
                    value: value.clone(),
                },
            }),
        }
    }
}

/// @deprecated
pub trait MergeableContent {
    fn is_mergable_content(&self, other: &dyn InsertContentTrait) -> bool;
    fn merge_content(&mut self, other: &dyn InsertContentTrait);
}

/// @deprecated
pub trait SliceableContent {
    fn slice_content(&self, from: usize, to: usize) -> Box<dyn InsertContentTrait>;
}

/// @deprecated
pub trait CloneContent {
    fn clone_content(&self) -> Box<dyn InsertContentTrait>;
}

/// @deprecated
pub trait InsertContentTrait:
    HasLength + std::fmt::Debug + Any + MergeableContent + SliceableContent + CloneContent
{
    fn id(&self) -> ContentType;
    // TODO: provide an encoding method
}

impl<T: Sliceable + InsertContentTrait> SliceableContent for T {
    fn slice_content(&self, from: usize, to: usize) -> Box<dyn InsertContentTrait> {
        Box::new(self.slice(from, to))
    }
}

impl<T: Clone + InsertContentTrait> CloneContent for T {
    fn clone_content(&self) -> Box<dyn InsertContentTrait> {
        Box::new(self.clone())
    }
}

impl<'a> HasLength for RawOpContent<'a> {
    fn content_len(&self) -> usize {
        match self {
            RawOpContent::Future(f) => match f {
                FutureRawOpContent::Map(x) => x.content_len(),
                FutureRawOpContent::List(x) => x.content_len(),
                FutureRawOpContent::Tree(x) => x.content_len(),
                FutureRawOpContent::Unknown { op_len, .. } => *op_len,
            },
        }
    }
}

impl<'a> Sliceable for RawOpContent<'a> {
    fn slice(&self, from: usize, to: usize) -> Self {
        match self {
            RawOpContent::Future(f) => RawOpContent::Future(match f {
                FutureRawOpContent::Map(x) => FutureRawOpContent::Map(x.slice(from, to)),
                FutureRawOpContent::List(x) => FutureRawOpContent::List(x.slice(from, to)),
                FutureRawOpContent::Tree(x) => FutureRawOpContent::Tree(x.slice(from, to)),
                FutureRawOpContent::Unknown { .. } => unreachable!(),
            }),
        }
    }
}

impl<'a> Mergable for RawOpContent<'a> {
    fn is_mergable(&self, other: &Self, _conf: &()) -> bool
    where
        Self: Sized,
    {
        match (self.as_future().unwrap(), other.as_future().unwrap()) {
            (FutureRawOpContent::Map(x), FutureRawOpContent::Map(y)) => x.is_mergable(y, &()),
            (FutureRawOpContent::List(x), FutureRawOpContent::List(y)) => x.is_mergable(y, &()),
            _ => false,
        }
    }

    fn merge(&mut self, _other: &Self, _conf: &())
    where
        Self: Sized,
    {
        match self {
            RawOpContent::Future(f) => match f {
                FutureRawOpContent::Map(x) => match _other {
                    RawOpContent::Future(FutureRawOpContent::Map(y)) => x.merge(y, &()),
                    _ => unreachable!(),
                },
                FutureRawOpContent::List(x) => match _other {
                    RawOpContent::Future(FutureRawOpContent::List(y)) => x.merge(y, &()),
                    _ => unreachable!(),
                },
                FutureRawOpContent::Tree(x) => match _other {
                    RawOpContent::Future(FutureRawOpContent::Tree(y)) => x.merge(y, &()),
                    _ => unreachable!(),
                },
                FutureRawOpContent::Unknown { .. } => unreachable!(),
            },
        };
    }
}

impl HasLength for InnerContent {
    fn content_len(&self) -> usize {
        match self {
            InnerContent::Future(f) => match f {
                FutureInnerContent::List(list) => list.atom_len(),
                FutureInnerContent::Map(_) => 1,
                FutureInnerContent::Tree(_) => 1,
                FutureInnerContent::Unknown { op_len, .. } => *op_len,
            },
        }
    }
}

impl Sliceable for InnerContent {
    fn slice(&self, from: usize, to: usize) -> Self {
        match self {
            InnerContent::Future(f) => InnerContent::Future(match f {
                a @ FutureInnerContent::Map(_) => a.clone(),
                a @ FutureInnerContent::Tree(_) => a.clone(),
                FutureInnerContent::List(x) => FutureInnerContent::List(x.slice(from, to)),
                FutureInnerContent::Unknown { .. } => unreachable!(),
            }),
        }
    }
}

impl Mergable for InnerContent {
    fn is_mergable(&self, other: &Self, _conf: &()) -> bool
    where
        Self: Sized,
    {
        match (self, other) {
            (
                InnerContent::Future(FutureInnerContent::List(x)),
                InnerContent::Future(FutureInnerContent::List(y)),
            ) => x.is_mergable(y, &()),
            _ => false,
        }
    }

    fn merge(&mut self, _other: &Self, _conf: &())
    where
        Self: Sized,
    {
        match self {
            InnerContent::Future(FutureInnerContent::List(x)) => match _other {
                InnerContent::Future(FutureInnerContent::List(y)) => x.merge(y, &()),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}
