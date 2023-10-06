use std::ops::Range;

use generic_btree::rle::{HasLength, Mergeable, Sliceable};
use loro_common::{Counter, HasId, IdSpan, ID};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Copy, Serialize, Deserialize)]
pub(crate) struct RichtextChunk {
    start: u32,
    end: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub(crate) enum RichtextChunkKind {
    Text,
    Symbol,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum RichtextChunkValue {
    Text(Range<u32>),
    Symbol(u32),
    Unknown(u32),
}

impl RichtextChunk {
    pub(crate) const UNKNOWN_START: u32 = u32::MAX;
    pub(crate) const SYMBOL_START: u32 = u32::MAX - 1;

    #[inline]
    pub fn new_text(range: Range<u32>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }

    #[inline]
    pub fn new_symbol(idx: u32) -> Self {
        Self {
            start: Self::SYMBOL_START,
            end: idx,
        }
    }

    #[inline]
    pub fn new_unknown(len: u32) -> Self {
        Self {
            start: Self::UNKNOWN_START,
            end: len,
        }
    }

    #[inline]
    pub(crate) fn kind(&self) -> RichtextChunkKind {
        match self.start {
            Self::SYMBOL_START => RichtextChunkKind::Symbol,
            Self::UNKNOWN_START => RichtextChunkKind::Unknown,
            _ => RichtextChunkKind::Text,
        }
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        match self.start {
            Self::UNKNOWN_START => self.end as usize,
            Self::SYMBOL_START => 1,
            _ => (self.end - self.start) as usize,
        }
    }

    #[inline]
    pub(crate) fn value(&self) -> RichtextChunkValue {
        match self.start {
            Self::UNKNOWN_START => RichtextChunkValue::Unknown(self.end),
            Self::SYMBOL_START => RichtextChunkValue::Symbol(self.end),
            _ => RichtextChunkValue::Text(self.start..self.end),
        }
    }
}

impl Mergeable for RichtextChunk {
    fn can_merge(&self, rhs: &Self) -> bool {
        match (self.kind(), rhs.kind()) {
            (RichtextChunkKind::Text, RichtextChunkKind::Text) => self.end == rhs.start,
            (RichtextChunkKind::Unknown, RichtextChunkKind::Unknown) => true,
            _ => false,
        }
    }

    fn merge_right(&mut self, rhs: &Self) {
        match (self.kind(), rhs.kind()) {
            (RichtextChunkKind::Text, RichtextChunkKind::Text) => self.end = rhs.end,
            (RichtextChunkKind::Unknown, RichtextChunkKind::Unknown) => self.end += rhs.end,
            _ => unreachable!(),
        }
    }

    fn merge_left(&mut self, left: &Self) {
        match (self.kind(), left.kind()) {
            (RichtextChunkKind::Text, RichtextChunkKind::Text) => self.start = left.start,
            (RichtextChunkKind::Unknown, RichtextChunkKind::Unknown) => self.end += left.end,
            _ => unreachable!(),
        }
    }
}

impl HasLength for RichtextChunk {
    #[inline(always)]
    fn rle_len(&self) -> usize {
        self.len()
    }
}

impl Sliceable for RichtextChunk {
    fn _slice(&self, range: Range<usize>) -> Self {
        match self.kind() {
            RichtextChunkKind::Text => {
                assert!(range.len() <= self.len());
                Self {
                    start: self.start + range.start as u32,
                    end: self.start + range.end as u32,
                }
            }
            RichtextChunkKind::Symbol => {
                assert_eq!(range.len(), 1);
                *self
            }
            RichtextChunkKind::Unknown => {
                assert!(range.len() <= self.len());
                Self {
                    start: Self::UNKNOWN_START,
                    end: range.len() as u32,
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub(super) struct FugueSpan {
    pub id: ID,
    /// The status at the current version
    pub status: Status,
    /// The status at the `after` version
    /// It's used when calculating diff
    pub after_status: Option<Status>,
    pub origin_left: Option<ID>,
    pub origin_right: Option<ID>,
    pub content: RichtextChunk,
}

impl FugueSpan {
    #[inline(always)]
    pub fn id_span(&self) -> IdSpan {
        IdSpan::new(
            self.id.peer,
            self.id.counter,
            self.id.counter + self.content.len() as Counter,
        )
    }
}

impl Sliceable for FugueSpan {
    fn _slice(&self, range: Range<usize>) -> Self {
        Self {
            id: self.id.inc(range.start as Counter),
            status: self.status,
            after_status: self.after_status,
            origin_left: if range.start == 0 {
                self.origin_left
            } else {
                Some(self.id.inc((range.start - 1) as Counter))
            },
            origin_right: self.origin_right,
            content: self.content._slice(range),
        }
    }
}

impl HasLength for FugueSpan {
    #[inline(always)]
    fn rle_len(&self) -> usize {
        self.content.len()
    }
}

impl Mergeable for FugueSpan {
    fn can_merge(&self, rhs: &Self) -> bool {
        self.id.peer == rhs.id.peer
            && self.status == rhs.status
            && self.after_status == rhs.after_status
            && Some(self.id) == rhs.origin_left
            && self.id.counter + self.content.len() as Counter == rhs.id.counter
            && self.origin_right == rhs.origin_right
            && self.content.can_merge(&rhs.content)
    }

    fn merge_right(&mut self, rhs: &Self) {
        self.content.merge_right(&rhs.content);
    }

    fn merge_left(&mut self, left: &Self) {
        self.id = left.id;
        self.content.merge_left(&left.content);
    }
}

impl FugueSpan {
    pub fn new(id: ID, content: RichtextChunk) -> Self {
        Self {
            id,
            status: Status::default(),
            after_status: None,
            origin_left: None,
            origin_right: None,
            content,
        }
    }

    #[inline(always)]
    pub fn is_activated(&self) -> bool {
        self.status.is_activated()
    }

    #[inline]
    pub fn activated_len(&self) -> usize {
        if self.is_activated() {
            self.content.len()
        } else {
            0
        }
    }
}

impl HasId for FugueSpan {
    fn id_start(&self) -> ID {
        self.id
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Hash, Copy)]
pub(super) struct Status {
    /// is this span from a future operation
    pub future: bool,
    pub delete_times: i16,
}

impl Status {
    #[inline(always)]
    pub fn is_activated(&self) -> bool {
        self.delete_times == 0 && !self.future
    }
}
