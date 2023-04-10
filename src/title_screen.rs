use bevy::prelude::*;

use crate::AppState;
use crate::FONT_BOLD_PATH;

pub(crate) struct TitleScreenPlugin;

impl Plugin for TitleScreenPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(OnEnter(AppState::TitleScreen), (spawn_title_screen,));

        app.add_systems(OnExit(AppState::TitleScreen), (despawn_title_screen,));
    }
}

#[derive(Component)]
struct TitleScreenElement;

fn despawn_title_screen(mut commands: Commands, query: Query<Entity, With<TitleScreenElement>>) {
    for entity in query.iter() {
        commands.entity(entity).despawn_recursive();
    }
}

fn spawn_title_screen(mut commands: Commands, asset_server: Res<AssetServer>) {
    let title_font = TextStyle {
        font: asset_server.load(FONT_BOLD_PATH),
        font_size: 30.,
        color: Color::WHITE,
    };

    let option_font = TextStyle {
        font: asset_server.load(FONT_BOLD_PATH),
        font_size: 30.,
        color: Color::WHITE,
    };

    let root = commands
        .spawn((
            NodeBundle {
                style: Style {
                    flex_basis: Val::Percent(100.),
                    align_items: AlignItems::Center,
                    justify_content: JustifyContent::Center,
                    ..Default::default()
                },
                ..default()
            },
            TitleScreenElement,
        ))
        .id();

    let panel = commands.spawn(NodeBundle {
        style: Style {
            flex_direction: FlexDirection::Column,
            align_items: AlignItems::Center,
            justify_content: JustifyContent::SpaceBetween,
            size: Size::new(Val::Px(200.), Val::Px(600.)),
            padding: UiRect::all(Val::Px(10.)),
            ..Default::default()
        },
        background_color: Color::BLACK.with_a(0.25).into(),
        ..Default::default()
    }).id();

    commands.entity(root).add_child(panel);

    let title = commands
        .spawn(TextBundle::from_section("PILLS", title_font))
        .id();

    commands.entity(panel).add_child(title);

    let menu_panel = commands
        .spawn((
            NodeBundle {
                style: Style {
                    size: Size::width(Val::Percent(100.)),
                    align_items: AlignItems::Stretch,
                    justify_content: JustifyContent::Center,
                    flex_direction: FlexDirection::Column,
                    gap: Size::all(Val::Px(10.)),
                    ..Default::default()
                },
                ..default()
            },
            TitleScreenElement,
        ))
        .id();

    commands.entity(panel).add_child(menu_panel);

    for option in Menu::ALL.into_iter() {
        let item = spawn_button(
            &mut commands,
            option_font.clone(),
            option.label().into(),
            option,
        );
        commands.entity(menu_panel).add_child(item);
    }
}

#[derive(Component, Clone, Copy, Default, Eq, PartialEq)]
enum Menu {
    #[default]
    Start,
    Quit,
}

#[derive(Resource, Default)]
struct SelectedOption(Menu);

impl Menu {
    const ALL: [Menu; 2] = {
        use Menu::*;
        [Start, Quit]
    };

    fn label(self) -> &'static str {
        match self {
            Menu::Start => "Start",
            Menu::Quit => "Quit",
        }
    }
}

fn spawn_button<T: Component>(
    commands: &mut Commands,
    text_style: TextStyle,
    label: String,
    marker: T,
) -> Entity {
    commands
        .spawn((
            NodeBundle {
                style: Style {
                    align_items: AlignItems::Center,
                    justify_content: JustifyContent::Center,
                    padding: UiRect::vertical(Val::Px(3.)),
                    ..Default::default()
                },
                background_color: Color::NAVY.into(),
                ..Default::default()
            },
            marker,
        ))
        .with_children(|parent| {
            parent.spawn(TextBundle::from_section(label, text_style));
        })
        .id()
}
fn input_system(
    keyboard_input_events: Res<Input<KeyCode>>,
    mut selected_option: ResMut<SelectedOption>,
    mut event_reader: Local<EventReader<KeyboardInput>>,
) {
    for event in event_reader.iter(&keyboard_input_events) {
        if let Some(key_code) = event.key_code {
            if event.state == ElementState::Pressed {
                match key_code {
                    KeyCode::Up => {
                        if selected_option.index > 0 {
                            selected_option.index -= 1;
                        }
                    }
                    KeyCode::Down => {
                        if selected_option.index < Menu::ALL.len() - 1 {
                            selected_option.index += 1;
                        }
                    }
                    KeyCode::Space => {
                        // Emit an event or set a flag to handle the selection.
                    }
                    _ => {}
                }
            }
        }
    }
}

// UI update system
fn update_ui(
    selected_option: Res<SelectedOption>,
    mut query: Query<(&Menu, &mut BackgroundColor, &mut Text)>,
) {
    for (menu, mut style, mut text) in query.iter_mut() {
        if selected_option.0 == *menu {
            // Set the selected menu option to the reversed colors
            style.0 = Color::WHITE.into();
            text.sections[0].style.color = Color::NAVY;
        } else {
            // Set the unselected menu option to the original colors
            style.0 = Color::NAVY.into();
            text.sections[0].style.color = Color::WHITE;
        }
    }
}

// Handle selection system
fn handle_selection(
    keyboard_input_events: Res<Input<KeyCode>>,
    selected_option: Res<SelectedOption>,
    mut app_state: ResMut<State<AppState>>,
) {
    for event in event_reader.iter(&keyboard_input_events) {
        if let Some(key_code) = event.key_code {
            if event.state == ElementState::Pressed && key_code == KeyCode::Space {
                match Menu::ALL[selected_option.index] {
                    Menu::Start => {
                        app_state.set(AppState::Game).unwrap();
                    }
                    Menu::Quit => {
                        app_state.set(AppState::Exit).unwrap();
                    }
                }
            }
        }
    }
}
