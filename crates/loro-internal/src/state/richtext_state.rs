use generic_btree::rle::HasLength;
use loro_common::LoroValue;

use crate::{
    arena::SharedArena,
    container::richtext::RichtextState as InnerState,
    container::{list::list_op, richtext::richtext_state::RichtextStateChunk},
    delta::DeltaItem,
    event::Diff,
    op::{Op, RawOp},
};

use super::ContainerState;

#[derive(Debug)]
pub struct RichtextState {
    state: InnerState,
    in_txn: bool,
    undo_stack: Vec<UndoItem>,
}

impl Clone for RichtextState {
    fn clone(&self) -> Self {
        Self {
            state: self.state.clone(),
            in_txn: false,
            undo_stack: Vec::new(),
        }
    }
}

#[derive(Debug)]
enum UndoItem {
    Insert {
        index: u32,
        len: u32,
    },
    Delete {
        index: u32,
        content: RichtextStateChunk,
    },
}

impl ContainerState for RichtextState {
    fn apply_diff(&mut self, diff: &mut Diff, arena: &SharedArena) {
        let Diff::RichtextRaw(richtext) = diff else {
            unreachable!()
        };

        let mut index = 0;
        for span in richtext.vec.iter() {
            match span {
                crate::delta::DeltaItem::Retain { len, meta } => {
                    index += len;
                }
                crate::delta::DeltaItem::Insert { value, meta } => {
                    match value {
                        RichtextStateChunk::Text { unicode_len, text } => {
                            self.state.insert_elem_at_entity_index(
                                index as usize,
                                RichtextStateChunk::Text {
                                    unicode_len: *unicode_len,
                                    text: text.clone(),
                                },
                            );
                        }
                        RichtextStateChunk::Style { style, anchor_type } => {
                            todo!("should handle style annotation")
                        }
                    }
                    self.undo_stack.push(UndoItem::Insert {
                        index: index as u32,
                        len: value.rle_len() as u32,
                    });
                    index += value.rle_len();
                }
                crate::delta::DeltaItem::Delete { len, meta } => {
                    let content = self.state.drain_by_entity_index(index, *len);
                    for span in content {
                        self.undo_stack.push(UndoItem::Delete {
                            index: index as u32,
                            content: span,
                        })
                    }
                }
            }
        }
    }

    fn apply_op(&mut self, _: &RawOp, op: &Op, arena: &SharedArena) {
        match &op.content {
            crate::op::InnerContent::List(l) => match l {
                list_op::InnerListOp::Insert { slice, pos } => {
                    self.state.insert(
                        *pos,
                        arena.slice_by_unicode(slice.0.start as usize..slice.0.end as usize),
                    );
                }
                list_op::InnerListOp::Delete(del) => {
                    self.state.delete(del.pos as usize, del.len as usize);
                }
                list_op::InnerListOp::Style { start, end, style } => {
                    self.state
                        .mark(*start as usize..*end as usize, style.clone());
                }
            },
            _ => unreachable!(),
        }
    }

    fn to_diff(&self) -> Diff {
        let mut delta = crate::delta::Delta::new();
        for span in self.state.iter_chunk() {
            delta.vec.push(DeltaItem::Insert {
                value: span.clone(),
                meta: (),
            })
        }

        Diff::RichtextRaw(delta)
    }

    fn start_txn(&mut self) {
        self.in_txn = true;
    }

    fn abort_txn(&mut self) {
        self.in_txn = false;
        self.undo_all();
    }

    fn commit_txn(&mut self) {
        self.in_txn = false;
        self.undo_stack.clear();
    }

    // value is a list
    fn get_value(&self) -> LoroValue {
        todo!()
    }
}

impl RichtextState {
    fn undo_all(&mut self) {
        while let Some(item) = self.undo_stack.pop() {
            match item {
                UndoItem::Insert { index, len } => {
                    let _ = self
                        .state
                        .drain_by_entity_index(index as usize, len as usize);
                }
                UndoItem::Delete { index, content } => {
                    match content {
                        RichtextStateChunk::Text { .. } => {}
                        RichtextStateChunk::Style { .. } => {
                            unimplemented!("should handle style annotation")
                        }
                    }

                    self.state
                        .insert_elem_at_entity_index(index as usize, content);
                }
            }
        }
    }
}
