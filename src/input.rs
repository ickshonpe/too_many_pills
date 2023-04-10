use bevy::ecs::system::SystemParam;
use bevy::prelude::*;
use bevy::utils::HashMap;
//use bevy_pkv::PkvStore;

use serde::{Deserialize, Serialize};
use std::marker::PhantomData;

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug, Serialize, Deserialize)]
pub enum PlayerCommand {
    MenuLeft,
    MenuRight,
    MenuUp,
    MenuDown,
    MoveLeft,
    MoveRight,
    RotateLeft,
    RotateRight,
    FastDrop,
    Select,
}

impl PlayerCommand {
    const ALL: [PlayerCommand; 10] = [
        Self::MenuLeft,
        Self::MenuRight,
        Self::MenuUp,
        Self::MenuDown,
        Self::MoveLeft,
        Self::MoveRight,
        Self::RotateLeft,
        Self::RotateRight,
        Self::FastDrop,
        Self::Select,
    ];
}

#[derive(Serialize, Deserialize, Deref, DerefMut, Resource)]
pub struct PlayerInputMap {
    pub bindings: HashMap<PlayerCommand, Vec<KeyCode>>,
}

impl Default for PlayerInputMap {
    fn default() -> Self {
        let keys = [
            (KeyCode::Left, PlayerCommand::MenuLeft),
            (KeyCode::Right, PlayerCommand::MenuRight),
            (KeyCode::Up, PlayerCommand::MenuUp),
            (KeyCode::Down, PlayerCommand::MenuDown),
            (KeyCode::Left, PlayerCommand::MoveLeft),
            (KeyCode::Right, PlayerCommand::MoveRight),
            (KeyCode::X, PlayerCommand::RotateLeft),
            (KeyCode::Z, PlayerCommand::RotateRight),
            (KeyCode::Down, PlayerCommand::FastDrop),
            (KeyCode::Space, PlayerCommand::Select),
            (KeyCode::Return, PlayerCommand::Select),
        ];
        let mut bindings: HashMap<PlayerCommand, Vec<KeyCode>> = PlayerCommand::ALL
            .map(|c| (c, vec![]))
            .into_iter()
            .collect();
        for (key, command) in keys {
            bindings.get_mut(&command).unwrap().push(key);
        }
        println!("{:#?}", bindings);
        Self { bindings }
    }
}

// impl PlayerInputMap {
//     const INPUTMAP_PKV_KEY: &str = "inputmap_pkv_key";
//     pub fn store(&self, pkv: &mut bevy_pkv::PkvStore) {
//         if let Err(set_error) = pkv.set(Self::INPUTMAP_PKV_KEY, &self) {
//             warn!("failed to store input map");
//             warn!("error: {:?}", set_error);
//         }
//     }

//     pub fn load(pkv: &mut bevy_pkv::PkvStore) -> Self {
//         if let Ok(input_map) = pkv.get::<PlayerInputMap>(Self::INPUTMAP_PKV_KEY) {
//             input_map
//         } else {
//             let input_map = PlayerInputMap::default();
//             if let Err(set_error) = pkv.set(Self::INPUTMAP_PKV_KEY, &input_map) {
//                 warn!("failed to store input map");
//                 warn!("error: {:?}", set_error);
//             }
//             input_map
//         }
//     }
// }

#[derive(SystemParam)]
pub struct PlayerInput<'w, 's> {
    pub input_map: Res<'w, PlayerInputMap>,
    pub input: Res<'w, Input<KeyCode>>,
    #[system_param(ignore)]
    phantom: PhantomData<fn() -> &'s ()>,
}

impl PlayerInput<'_, '_> {
    pub fn pressed(&self, button: PlayerCommand) -> bool {
        if let Some(keys) = self.input_map.get(&button) {
            keys.iter().any(|&key| self.input.pressed(key))
        } else {
            false
        }
    }

    pub fn just_pressed(&self, button: PlayerCommand) -> bool {
        if let Some(keys) = self.input_map.get(&button) {
            keys.iter().any(|&key| self.input.just_pressed(key))
        } else {
            false
        }
    }

    pub fn just_released(&self, button: PlayerCommand) -> bool {
        if let Some(keys) = self.input_map.get(&button) {
            keys.iter().any(|&key| self.input.just_released(key))
        } else {
            false
        }
    }
}

// fn load_inputmap(
//     mut commands: Commands,
//     mut pkv_store: ResMut<PkvStore>,
// ) {
//     commands.insert_resource(
//         PlayerInputMap::load(&mut pkv_store)
//     );
// }

pub struct PlayerInputPlugin;

impl Plugin for PlayerInputPlugin {
    fn build(&self, app: &mut App) {
        app
            //.add_startup_system(load_inputmap);
            .init_resource::<PlayerInputMap>();
    }
}
