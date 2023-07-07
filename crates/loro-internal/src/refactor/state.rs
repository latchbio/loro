use enum_dispatch::enum_dispatch;
use fxhash::FxHashMap;

use crate::{container::ContainerID, event::Diff, version::Frontiers, VersionVector};

use super::arena::SharedArena;

mod list_state;
mod map_state;
mod text_state;

use list_state::ListState;
use map_state::MapState;
use text_state::TextState;

#[derive(Clone)]
pub struct AppState {
    vv: VersionVector,
    frontiers: Frontiers,
    state: FxHashMap<ContainerID, State>,
    arena: SharedArena,
}

#[enum_dispatch]
pub trait ContainerState: Clone {
    fn apply_diff(&mut self, diff: Diff);

    /// Start a transaction
    ///
    /// The transaction may be aborted later, then all the ops during this transaction need to be undone.
    fn start_txn(&mut self);
    fn abort_txn(&mut self);
    fn commit_txn(&mut self);
}

#[enum_dispatch(ContainerState)]
#[derive(Clone)]
pub enum State {
    ListState,
    MapState,
    TextState,
}

pub struct AppStateDiff {
    pub changes: Vec<ContainerStateDiff>,
}

pub struct ContainerStateDiff {
    pub idx: ContainerID,
    pub diff: Diff,
}

impl AppState {
    pub fn new() -> Self {
        Self {
            vv: VersionVector::default(),
            frontiers: Frontiers::default(),
            state: FxHashMap::default(),
            arena: SharedArena::default(),
        }
    }

    pub fn apply_diff(&mut self, diff: &AppStateDiff) {
        todo!()
    }
}
