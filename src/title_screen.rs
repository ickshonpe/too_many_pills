use bevy::app::AppExit;
use bevy::prelude::*;

use crate::FONT_PATH;
use crate::Level;
use crate::input::PlayerCommand;
use crate::input::PlayerInput;
use crate::AppState;
use crate::GameState;
use crate::FONT_BOLD_PATH;

pub(crate) struct TitleScreenPlugin;

impl Plugin for TitleScreenPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<SelectedOption>();
        app.add_systems(OnEnter(AppState::TitleScreen), (spawn_title_screen,));

        app.add_systems(OnExit(AppState::TitleScreen), (despawn_title_screen,));

        app.add_systems(
            Update,
            (menu_input, update_ui).run_if(in_state(AppState::TitleScreen)),
        );
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
        font: asset_server.load(FONT_PATH),
        font_size: 25.,
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

    let panel = commands
        .spawn(NodeBundle {
            style: Style {
                flex_direction: FlexDirection::Column,
                align_items: AlignItems::Center,
                justify_content: JustifyContent::SpaceBetween,
                size: Size::new(Val::Px(220.), Val::Px(400.)),
                padding: UiRect::all(Val::Px(10.)),
                ..Default::default()
            },
            background_color: Color::BLACK.with_a(0.25).into(),
            ..Default::default()
        })
        .id();

    commands.entity(root).add_child(panel);

    let title = commands
        .spawn(TextBundle::from_section("too many\npills", title_font)
        .with_text_alignment(TextAlignment::Center))
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
    Start = 0,
    Level = 1,
    Quit = 2,
}

#[derive(Resource, Default, Deref, DerefMut, PartialEq, Eq)]
struct SelectedOption(Menu);

impl Menu {
    const ALL: [Menu; 3] = {
        use Menu::*;
        [Start, Level, Quit]
    };

    fn label(self) -> &'static str {
        match self {
            Menu::Start => "Start",
            Menu::Level => "Level: 1",
            Menu::Quit => "Quit",
        }
    }

    fn previous(self) -> Self {
        if 0 < self as usize {
            Menu::ALL[self as usize - 1]
        } else {
            self
        }
    }

    fn next(self) -> Self {
        Menu::ALL.get(self as usize + 1).copied().unwrap_or(self)
    }
}

fn spawn_button<T: Component + Copy>(
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
            parent.spawn((TextBundle::from_section(label, text_style), marker));
        })
        .id()
}

fn menu_input(
    input: PlayerInput,
    mut selected_option: ResMut<SelectedOption>,
    mut next_state: ResMut<NextState<AppState>>,
    mut exit_event: EventWriter<AppExit>,
    mut level: ResMut<Level>,
) {
    let mut selected = selected_option.0;
    if input.just_pressed(PlayerCommand::MenuUp) {
        selected = selected.previous();
    } else if input.just_pressed(PlayerCommand::MenuDown) {
        selected = selected.next()
    }

    selected_option.set_if_neq(SelectedOption(selected));

    if input.just_pressed(PlayerCommand::Select) {
        match selected_option.0 {
            Menu::Start => {
                next_state.set(AppState::Game);
            }
            Menu::Level => {
                level.0 += 1;
                if 9 < level.0 {
                    level.0 = 1;
                }
            }
            Menu::Quit => {
                exit_event.send(AppExit);
            }
        }
    }
}

// UI update system
fn update_ui(
    selected_option: Res<SelectedOption>,
    mut color_query: Query<(&Menu, &mut BackgroundColor)>,
    mut text_query: Query<(&Menu, &mut Text)>,
    level: Res<Level>,
) {
    for (menu, mut color) in color_query.iter_mut() {
        if selected_option.0 == *menu {
            // Set the selected menu option to the reversed colors
            color.0 = Color::WHITE.into();
        } else {
            // Set the unselected menu option to the original colors
            color.0 = Color::NAVY.into();
        }
    }

    for (menu, mut text) in text_query.iter_mut() {
        if selected_option.0 == *menu {
            text.sections[0].style.color = Color::NAVY;
        } else {
            text.sections[0].style.color = Color::WHITE;
        }

        if *menu == Menu::Level {
            text.sections[0].value = format!("level: {}", level.0);
        }
    } 
}
