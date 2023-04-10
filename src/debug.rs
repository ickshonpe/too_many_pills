use super::*;
use bevy::prelude::*;

pub(crate) struct DebugPlugin;

impl Plugin for DebugPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            Update,
            (
                state_changed_report_system::<AppState>,
                state_changed_report_system::<GameState>,
            ),
        );
    }
}

fn state_changed_report_system<S: States>(state: Res<State<S>>) {
    if state.is_changed() {
        println!("state -> {:?}", state.get());
    }
}
