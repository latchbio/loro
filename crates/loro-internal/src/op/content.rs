use std::any::Any;

use enum_as_inner::EnumAsInner;
use rle::{HasLength, Mergable, Sliceable};
use serde::{Deserialize, Serialize};

use crate::{
    // container::{
    //         list::list_op::{InnerListOp, ListOp},
    //         map::MapSet,
    //         tree::tree_op::TreeOp,
    //     },
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
    // List(InnerListOp),
    // Map(MapSet),
    // Tree(TreeOp),
    // The future content should not use any encoded arena context.
    Future(FutureInnerContent),
}

#[derive(EnumAsInner, Debug, Clone)]
pub enum FutureInnerContent {
    Unknown { op_len: usize, value: OwnedValue },
}

// Note: It will be encoded into binary format, so the order of its fields should not be changed.
#[derive(EnumAsInner, Debug, PartialEq, Serialize, Deserialize)]
pub enum RawOpContent {
    // Map(MapSet),
    // List(ListOp<'a>),
    // Tree(TreeOp),
    #[serde(untagged)]
    Future(FutureRawOpContent),
}

#[derive(EnumAsInner, Debug, PartialEq, Serialize, Deserialize)]
pub enum FutureRawOpContent {
    Unknown { op_len: usize, value: OwnedValue },
}

impl Clone for RawOpContent {
    fn clone(&self) -> Self {
        match self {
            // Self::Map(arg0) => Self::Map(arg0.clone()),
            // Self::List(arg0) => Self::List(arg0.clone()),
            // Self::Tree(arg0) => Self::Tree(*arg0),
            Self::Future(f) => Self::Future(match f {
                FutureRawOpContent::Unknown { op_len, value } => FutureRawOpContent::Unknown {
                    op_len: *op_len,
                    value: value.clone(),
                },
            }),
        }
    }
}

impl RawOpContent {
    pub fn to_static(&self) -> RawOpContent {
        match self {
            // Self::Map(arg0) => RawOpContent::Map(arg0.clone()),
            // Self::List(arg0) => match arg0 {
            //     ListOp::Insert { slice, pos } => RawOpContent::List(ListOp::Insert {
            //         slice: slice.to_static(),
            //         pos: *pos,
            //     }),
            //     ListOp::Delete(x) => RawOpContent::List(ListOp::Delete(*x)),
            //     ListOp::StyleStart {
            //         start,
            //         end,
            //         key,
            //         value,
            //         info,
            //     } => RawOpContent::List(ListOp::StyleStart {
            //         start: *start,
            //         end: *end,
            //         key: key.clone(),
            //         value: value.clone(),
            //         info: *info,
            //     }),
            //     ListOp::StyleEnd => RawOpContent::List(ListOp::StyleEnd),
            // },
            // Self::Tree(arg0) => RawOpContent::Tree(*arg0),
            Self::Future(f) => RawOpContent::Future(match f {
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

impl HasLength for RawOpContent {
    fn content_len(&self) -> usize {
        match self {
            // RawOpContent::Map(x) => x.content_len(),
            // RawOpContent::List(x) => x.content_len(),
            // RawOpContent::Tree(x) => x.content_len(),
            RawOpContent::Future(f) => match f {
                FutureRawOpContent::Unknown { op_len, .. } => *op_len,
            },
        }
    }
}

impl Sliceable for RawOpContent {
    fn slice(&self, from: usize, to: usize) -> Self {
        match self {
            // RawOpContent::Map(x) => RawOpContent::Map(x.slice(from, to)),
            // RawOpContent::List(x) => RawOpContent::List(x.slice(from, to)),
            // RawOpContent::Tree(x) => RawOpContent::Tree(x.slice(from, to)),
            RawOpContent::Future(f) => match f {
                FutureRawOpContent::Unknown { .. } => unreachable!(),
            },
        }
    }
}

impl Mergable for RawOpContent {
    fn is_mergable(&self, other: &Self, _conf: &()) -> bool
    where
        Self: Sized,
    {
        match (self, other) {
            // (RawOpContent::Map(x), RawOpContent::Map(y)) => x.is_mergable(y, &()),
            // (RawOpContent::List(x), RawOpContent::List(y)) => x.is_mergable(y, &()),
            _ => false,
        }
    }

    fn merge(&mut self, _other: &Self, _conf: &())
    where
        Self: Sized,
    {
        match self {
            // RawOpContent::Map(x) => match _other {
            //     RawOpContent::Map(y) => x.merge(y, &()),
            //     _ => unreachable!(),
            // },
            // RawOpContent::List(x) => match _other {
            //     RawOpContent::List(y) => x.merge(y, &()),
            //     _ => unreachable!(),
            // },
            // RawOpContent::Tree(x) => match _other {
            //     RawOpContent::Tree(y) => x.merge(y, &()),
            //     _ => unreachable!(),
            // },
            RawOpContent::Future(f) => match f {
                FutureRawOpContent::Unknown { .. } => unreachable!(),
            },
        }
    }
}

impl HasLength for InnerContent {
    fn content_len(&self) -> usize {
        match self {
            // InnerContent::List(list) => list.atom_len(),
            // InnerContent::Map(_) => 1,
            // InnerContent::Tree(_) => 1,
            InnerContent::Future(f) => match f {
                FutureInnerContent::Unknown { op_len, .. } => *op_len,
            },
        }
    }
}

impl Sliceable for InnerContent {
    fn slice(&self, from: usize, to: usize) -> Self {
        match self {
            // a @ InnerContent::Map(_) => a.clone(),
            // a @ InnerContent::Tree(_) => a.clone(),
            // InnerContent::List(x) => InnerContent::List(x.slice(from, to)),
            InnerContent::Future(f) => match f {
                FutureInnerContent::Unknown { .. } => unreachable!(),
            },
        }
    }
}

impl Mergable for InnerContent {
    fn is_mergable(&self, other: &Self, _conf: &()) -> bool
    where
        Self: Sized,
    {
        match (self, other) {
            // (InnerContent::List(x), InnerContent::List(y)) => x.is_mergable(y, &()),
            _ => false,
        }
    }

    fn merge(&mut self, _other: &Self, _conf: &())
    where
        Self: Sized,
    {
        match self {
            // InnerContent::List(x) => match _other {
            //     InnerContent::List(y) => x.merge(y, &()),
            //     _ => unreachable!(),
            // },
            _ => unreachable!(),
        }
    }
}
