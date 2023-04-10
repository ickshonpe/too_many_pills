#![allow(clippy::type_complexity)]
#![allow(clippy::too_many_arguments)]
mod debug;
mod input;
mod title_screen;

use bevy::a11y::accesskit::Orientation;
use bevy::core_pipeline::clear_color::ClearColorConfig;
use bevy::ecs::system::SystemParam;
use bevy::math::vec2;
use bevy::math::vec3;
use bevy::math::Affine3A;
use bevy::math::Mat3A;
use bevy::prelude::*;
use bevy::reflect::TypeUuid;
use bevy::render::texture::DEFAULT_IMAGE_HANDLE;
use bevy::render::Extract;
use bevy::sprite::ExtractedSprite;
use bevy::sprite::ExtractedSprites;
use bevy::sprite::SpriteSystem;
use bevy::text;
use bevy::utils::HashSet;
use bevy::window::WindowResolution;
use bimap::BiMap;
use debug::DebugPlugin;
use input::PlayerCommand;
use input::PlayerInput;
use rand::distributions::Uniform;
use rand::seq::SliceRandom;
use rand::thread_rng;
use rand::Rng;
use std::collections::VecDeque;
use std::f32::consts::FRAC_PI_2;
use std::f32::consts::PI;
use std::f32::consts::TAU;
use std::marker::PhantomData;
use std::thread::spawn;
const BUG_SPRITE_PATHS: [&str; 3] = [
    "sprites/bug-d.png",
    "sprites/bug-b.png",
    "sprites/bug-c.png",
];
const PILL_SPRITE_PATH: &str = "sprites/pill.png";
const PILL_BROKE_SPRITE_PATH: &str = "sprites/pill-broke.png";
const FONT_PATH: &str = "fonts/slkscre.ttf";
const FONT_BOLD_PATH: &str = "fonts/slkscreb.ttf";
const WALL_TILE_SET_DIR: &str = "sprites/wall_set_b";
const BACK_TILE_SET_DIR: &str = "sprites/back_set";

const CELL_SIZE: Vec2 = Vec2::new(16., 16.);

const PILL_PALETTE: [Color; 3] = [
    Color::Rgba {
        red: 0.843,
        green: 0.482,
        blue: 0.729,
        alpha: 1.0,
    },
    Color::Rgba {
        red: 0.388,
        green: 0.608,
        blue: 1.0,
        alpha: 1.0,
    },
    Color::Rgba {
        red: 0.9884,
        green: 0.949,
        blue: 0.212,
        alpha: 1.0,
    },
];

#[derive(Component, Copy, Clone, Debug, Hash, Eq, PartialEq)]
enum Flavour {
    Red = 0,
    Blue = 1,
    Yellow = 2,
}

impl Flavour {
    const ALL: [Flavour; 3] = [Flavour::Red, Flavour::Blue, Flavour::Yellow];
    fn color(self) -> Color {
        PILL_PALETTE[self as usize]
    }

    fn bug_sprite_path(self) -> &'static str {
        BUG_SPRITE_PATHS[self as usize]
    }
}

#[derive(Component)]
struct GameUiElement;

#[derive(Component)]
struct Germ;

const SCORE_ZEROS: usize = 7;

#[derive(Resource)]
struct GameRules {
    horizontal_move_delay: f32,
    drop_delay: f32,
    matches: i16,
    fast_drop_multiplier: f32,
}

impl Default for GameRules {
    fn default() -> Self {
        Self {
            horizontal_move_delay: 0.1,
            drop_delay: 0.5,
            matches: 4,
            fast_drop_multiplier: 6.0,
        }
    }
}

#[derive(Component, Copy, Clone, Debug, Default, Hash, Eq, PartialEq)]
struct Pos {
    x: i16,
    y: i16,
}

const fn pos(x: i16, y: i16) -> Pos {
    Pos { x, y }
}

impl Pos {
    const ZERO: Self = pos(0, 0);
    const fn translate(self, x: i16, y: i16) -> Self {
        pos(self.x + x, self.y + y)
    }
    const fn up(self) -> Self {
        self.translate(0, 1)
    }
    const fn down(self) -> Self {
        self.translate(0, -1)
    }
    const fn left(self) -> Self {
        self.translate(-1, 0)
    }
    const fn right(self) -> Self {
        self.translate(1, 0)
    }
    const fn step(self, dir: Dir) -> Self {
        match dir {
            Dir::Up => self.up(),
            Dir::Down => self.down(),
            Dir::Left => self.left(),
            Dir::Right => self.right(),
        }
    }
    /// component wise min
    fn min(mut self, other: Self) -> Self {
        self.x = self.x.min(other.x);
        self.y = self.y.min(other.y);
        self
    }
    /// component wise max
    fn max(mut self, other: Self) -> Self {
        self.x = self.x.max(other.x);
        self.y = self.y.max(other.y);
        self
    }
}

#[derive(Copy, Clone, Debug, Default, Hash, Eq, PartialEq)]
struct Dim {
    w: i16,
    h: i16,
}
#[derive(Copy, Clone, Debug, Default, Hash, Eq, PartialEq)]
struct Grid {
    pos: Pos,
    dim: Dim,
}

const fn dim(w: i16, h: i16) -> Dim {
    Dim { w, h }
}
const fn grid(pos: Pos, dim: Dim) -> Grid {
    Grid { pos, dim }
}

impl Grid {
    fn x_range(self) -> std::ops::Range<i16> {
        self.pos.x..self.max_x()
    }
    fn y_range(self) -> std::ops::Range<i16> {
        self.pos.y..self.max_y()
    }
    // fn iter(self) -> impl Iterator<Item = Pos> {
    //     self.x_range()
    //         .flat_map(move |x| self.y_range().map(move |y| pos(x, y)))
    // }
    fn expand(self, dim: Dim) -> Self {
        grid(self.pos.translate(-dim.w, -dim.h), self.dim.expand(dim))
    }
    fn contains(self, pos: Pos) -> bool {
        self.x_range().contains(&pos.x) && self.y_range().contains(&pos.y)
    }

    const fn max_x(self) -> i16 {
        self.pos.x + self.dim.w
    }
    const fn max_y(self) -> i16 {
        self.pos.y + self.dim.h
    }
}

struct GridIter {
    x_range: std::ops::Range<i16>,
    y_range: std::ops::Range<i16>,
    current_x: i16,
    current_y: i16,
}

impl Iterator for GridIter {
    type Item = Pos;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_y < self.y_range.end {
            let pos = pos(self.current_x, self.current_y);
            self.current_x += 1;

            if self.current_x == self.x_range.end {
                self.current_x = self.x_range.start;
                self.current_y += 1;
            }

            Some(pos)
        } else {
            None
        }
    }
}

impl IntoIterator for Grid {
    type Item = Pos;
    type IntoIter = GridIter;

    fn into_iter(self) -> Self::IntoIter {
        GridIter {
            x_range: self.x_range(),
            y_range: self.y_range(),
            current_x: self.pos.x,
            current_y: self.pos.y,
        }
    }
}

impl Dim {
    const ONE: Self = Self::uniform(1);
    const fn uniform(d: i16) -> Self {
        dim(d, d)
    }
    fn grid(self) -> Grid {
        grid(Pos::ZERO, self)
    }
    fn expand(self, expansion: Self) -> Self {
        dim(self.w + expansion.w * 2, self.h + expansion.h * 2)
    }
}

impl From<Pos> for Vec2 {
    fn from(Pos { x, y }: Pos) -> Self {
        vec2(x as f32, y as f32)
    }
}

impl From<Dim> for Vec2 {
    fn from(Dim { w, h }: Dim) -> Self {
        vec2(w as f32, h as f32)
    }
}

#[derive(Component, Copy, Clone, Default, Debug)]
struct Marker<T> {
    _phantom: PhantomData<fn() -> T>,
}

#[derive(Component)]
struct PivotPill;
#[derive(Component)]
struct ActivePill;

#[derive(Component, Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
enum GameSet {
    Animation,
    Input,
    RotatePill,
    DropPill,
    MovePill,
    SpawnPill,
    Jar,
    SwapEnds,
    GamePlay,
    Effects,
    UpdateVitalsPill,
    UpdateVitalsGerm,
}

#[derive(Component)]
pub struct PillSpriteQueueMarker;

#[derive(Resource)]
struct Organ {
    cells: BiMap<Pos, Entity>,
    spawn_area: Grid,
}

impl Organ {
    fn new(spawn_area: Grid) -> Self {
        Self {
            cells: BiMap::default(),
            spawn_area,
        }
    }

    fn get(&self, pos: Pos) -> Option<Entity> {
        self.cells.get_by_left(&pos).copied()
    }

    fn insert(&mut self, pos: Pos, entity: Entity) -> bimap::Overwritten<Pos, Entity> {
        self.cells.insert(pos, entity)
    }

    fn insert_joined(&mut self, pill: [(Pos, Entity); 2]) -> [bimap::Overwritten<Pos, Entity>; 2] {
        let set: HashSet<Entity> = pill.iter().map(|(_, e)| *e).collect();
        pill.map(|(pos, entity)| self.insert(pos, entity))
    }

    fn remove(&mut self, pos: Pos) -> Option<Entity> {
        self.cells.remove_by_left(&pos).map(|(_, e)| e)
    }

    fn remove_entity(&mut self, entity: Entity) -> Option<(Pos, Entity)> {
        self.cells.remove_by_right(&entity)
    }

    fn is_empty(&self, pos: Pos) -> bool {
        !self.cells.contains_left(&pos)
    }

    fn bounds(&self) -> Grid {
        let mut min = pos(i16::MAX, i16::MAX);
        let mut max = pos(i16::MIN, i16::MIN);
        for &cell in self.cells.left_values() {
            min = min.min(cell);
            max = max.max(cell);
        }
        grid(min, dim(max.x - min.x + 1, max.y - min.y + 1))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default, States)]
enum AppState {
    #[default]
    TitleScreen,
    Game,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default, States)]
enum GameState {
    #[default]
    Inactive,
    /// pill falling
    PillFalling,
    /// pill landed on surface
    PillLanded,
    Collapsing,
    Wait,
    GameOver,
}

#[derive(Resource, Clone, Copy, Debug, Default)]
struct Score(u64);

#[derive(Resource, Clone, Copy, Debug, Default)]
struct HighScore(u64);

#[derive(Resource, Default)]
struct ComboKillCounter(u64);

fn reset_bug_kill_counter(mut counter: ResMut<ComboKillCounter>) {
    counter.0 = 0;
}

fn main() {
    let mut app = App::new();
    app.add_plugins(
        DefaultPlugins
            .set(ImagePlugin::default_nearest())
            .set(WindowPlugin {
                primary_window: Some(Window {
                    resolution: WindowResolution::new(800., 900.),
                    title: "pills".to_string(),
                    resizable: false,
                    ..Default::default()
                }),
                ..Default::default()
            }),
    )
    .add_plugin(title_screen::TitleScreenPlugin)
    .add_plugin(input::PlayerInputPlugin)
    .init_resource::<PillCommands>()
    .init_resource::<GameRules>()
    .init_resource::<ShadowOffset>()
    .init_resource::<Score>()
    .init_resource::<HighScore>()
    .init_resource::<ComboKillCounter>()
    .init_resource::<PillQueue>()
    .init_resource::<Vitals>()
    .init_resource::<Danger>()
    .init_resource::<Level>()
    .init_resource::<CauseOfDeath>()
    .add_event::<KilledSomethingEvent>()
    .add_event::<SpawnExplosionEvent>()
    .add_event::<ScorePointsEvent>()
    .add_systems(Startup, setup)
    .add_systems(
        OnEnter(AppState::Game),
        (setup_game, spawn_score_display, spawn_vitals_display,
        spawn_level_display),
    )
    .add_systems(
        OnEnter(GameState::GameOver),
        spawn_game_over_message
    )
    .add_systems(
        Update,
        wait_game_over.run_if(in_state(GameState::GameOver))
    )
    .add_systems(OnExit(AppState::Game), despawn_game_elements)
    .add_systems(
        Update,
        wait_system
            .run_if(in_state(GameState::Wait))
            .run_if(in_state(AppState::Game)),
    )
    .add_systems(Update, bevy::window::close_on_esc)
    .add_systems(
        OnEnter(GameState::PillFalling),
        (reset_bug_kill_counter, spawn_new_pill).run_if(in_state(AppState::Game)),
    )
    .add_systems(OnEnter(GameState::Collapsing), collapse_pills)
    .add_systems(
        PreUpdate,
        (update_pill_timers
            .before(GameSet::SpawnPill)
            .run_if(in_state(AppState::Game)))
        .run_if(in_state(GameState::PillFalling)),
    )
    .add_state::<AppState>()
    .add_state::<GameState>()
    .add_systems(
        Update,
        (
            update_score_display::<Score>,
            update_score_display::<HighScore>,
            update_danger,
            update_vitals_display,
            update_level_display,
        )
            .after(GameSet::Effects)
            .run_if(in_state(AppState::Game)),
    )
    .add_systems(
        Update,
        (
            explode_on_killed,
            update_kill_count_and_score,
            update_vitals_on_kill_pill
                .in_set(GameSet::UpdateVitalsPill)
                .before(GameSet::UpdateVitalsGerm),
            update_vitals_on_kill_germ
                .in_set(GameSet::UpdateVitalsGerm)
                .after(GameSet::UpdateVitalsPill),
        )
            .after(GameSet::GamePlay)
            .before(GameSet::Effects)
            .run_if(in_state(AppState::Game)),
    )
    .add_systems(
        Update,
        (
            spawn_explosions.after(GameSet::Jar),
            velocity_physics.in_set(GameSet::Animation),
            fade_out.in_set(GameSet::Animation),
            update_flippers.in_set(GameSet::Animation),
            update_score,
        )
            .after(GameSet::GamePlay)
            .in_set(GameSet::Effects)
            .run_if(in_state(AppState::Game)),
    )
    .add_systems(
        PreUpdate,
        (update_despawn_timers,).run_if(in_state(AppState::Game)),
    )
    .add_systems(
        Update,
        (
            handle_input
                .in_set(GameSet::Input)
                .before(GameSet::RotatePill)
                .before(GameSet::MovePill),
            orient_pill
                .in_set(GameSet::RotatePill)
                .after(GameSet::Input),
            swap_pill_ends
                .in_set(GameSet::SwapEnds)
                .after(GameSet::Input)
                .before(GameSet::DropPill),
            move_pill
                .in_set(GameSet::MovePill)
                .after(GameSet::RotatePill)
                .before(GameSet::DropPill)
                .after(GameSet::Input),
            drop_pill
                .in_set(GameSet::DropPill)
                .after(GameSet::RotatePill)
                .after(GameSet::Input),
        )
            .in_set(GameSet::GamePlay)
            .run_if(in_state(GameState::PillFalling))
            .run_if(in_state(AppState::Game)),
    )
    .add_systems(
        Update,
        (match_flavour_lines
            .in_set(GameSet::Jar)
            .in_set(GameSet::GamePlay),)
            .run_if(in_state(GameState::PillLanded))
            .run_if(in_state(AppState::Game)),
    );
    if let Ok(render_app) = app.get_sub_app_mut(bevy::render::RenderApp) {
        render_app.add_systems(
            ExtractSchedule,
            (
                extract_organ_sprites,
                extract_wall_sprites,
                extract_pill_queue,
            )
                .after(SpriteSystem::ExtractSprites)
                .run_if(|state: Extract<Res<State<AppState>>>| *state.get() == AppState::Game),
        );
    }

    //app.add_plugin(DebugPlugin);

    app.run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    let mut camera_2d_bundle = Camera2dBundle::default();
    camera_2d_bundle.camera_2d.clear_color = ClearColorConfig::Custom(Color::MAROON);
    commands.spawn(camera_2d_bundle);

    commands.insert_resource(PillSpriteHandle(asset_server.load(PILL_SPRITE_PATH)));

    commands.spawn((PillSpriteQueueMarker, VisibilityBundle::default()));
}

fn setup_game(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    _rules: Res<GameRules>,
    mut camera_query: Query<&mut Transform, With<Camera2d>>,
    mut next_state: ResMut<NextState<GameState>>,
    mut pill_queue: ResMut<PillQueue>,
    mut danger: ResMut<Danger>,
    mut vitals: ResMut<Vitals>,
    level: Res<Level>,
) {
    *danger = Danger::default();
    *vitals = Vitals::default();

    *pill_queue = pill_queue.new();
    let organ_inner_area = dim(9, 18).grid();
    let spawn_area = grid(pos(4, 16), dim(1, 2));
    let germ_max_y = 14;
    let mut camera_transform = camera_query.single_mut();
    camera_transform.scale = Vec2::splat(0.5).extend(1.0);
    let ct = 75. * Vec2::X + 0.5 * CELL_SIZE * Vec2::from(organ_inner_area.dim) + 24. * Vec2::Y;
    camera_transform.translation.x = ct.x;
    camera_transform.translation.y = ct.y;

    commands.insert_resource(OrganGeometry {
        translation: Vec3::ZERO,
        cell_size: CELL_SIZE,
    });

    let mut jar = Organ::new(spawn_area);
    let wall_textures = load_typed_folder(&asset_server, WALL_TILE_SET_DIR).unwrap();
    let back_textures = load_typed_folder(&asset_server, BACK_TILE_SET_DIR).unwrap();
    spawn_organ_walls(
        &mut commands,
        &wall_textures,
        &back_textures,
        organ_inner_area,
        spawn_area.pos.up().up(),
        10,
        &mut jar,
    );

    let spawns = organ_inner_area
        .into_iter()
        .filter(|pos| pos.y <= germ_max_y);

    spawn_germs(&mut commands, 9 + 9 * level.0 as usize, &mut jar, spawns, &asset_server);

    commands.insert_resource(jar);
    next_state.set(GameState::PillFalling);
}

fn spawn_score_display(mut commands: Commands, asset_server: Res<AssetServer>) {
    let font = asset_server.load(FONT_PATH);
    let bold_font = asset_server.load(FONT_BOLD_PATH);
    let text_style_1 = TextStyle {
        font_size: 20.,
        font,
        color: Color::ANTIQUE_WHITE,
    };
    let text_style_2 = TextStyle {
        font_size: 20.,
        font: bold_font,
        color: Color::YELLOW,
    };
    commands
        .spawn(NodeBundle {
            style: Style {
                flex_basis: Val::Percent(100.),
                justify_content: JustifyContent::SpaceBetween,
                ..Default::default()
            },
            ..Default::default()
        })
        .insert(GameUiElement)
        .with_children(|parent| {
            parent
                .spawn(NodeBundle {
                    style: Style {
                        size: Size::new(Val::Px(160.), Val::Px(80.)),
                        margin: UiRect::all(Val::Px(20.)),
                        flex_direction: FlexDirection::Column,
                        align_items: AlignItems::Center,
                        justify_content: JustifyContent::Center,
                        gap: Size::all(Val::Px(10.)),
                        ..Default::default()
                    },
                    background_color: Color::NAVY.with_a(0.2).into(),
                    ..Default::default()
                })
                .with_children(|parent| {
                    parent.spawn(TextBundle::from_section("score", text_style_1.clone()));
                    parent
                        .spawn(TextBundle::from_section(
                            format_score(0),
                            text_style_2.clone(),
                        ))
                        .insert(Marker::<Score>::default());
                });

            parent
                .spawn(NodeBundle {
                    style: Style {
                        size: Size::new(Val::Px(160.), Val::Px(80.)),
                        margin: UiRect::all(Val::Px(20.)),
                        flex_direction: FlexDirection::Column,
                        align_items: AlignItems::Center,
                        justify_content: JustifyContent::Center,
                        gap: Size::all(Val::Px(10.)),
                        ..Default::default()
                    },
                    background_color: Color::NAVY.with_a(0.2).into(),
                    ..Default::default()
                })
                .with_children(|parent| {
                    parent.spawn(TextBundle::from_section("hiscore", text_style_1.clone()));
                    parent
                        .spawn(TextBundle::from_section(
                            format_score(0),
                            text_style_2.clone(),
                        ))
                        .insert(Marker::<HighScore>::default());
                });
        });
}

#[derive(Resource, Default)]
struct Vitals([f32; 3]);

impl Vitals {
    fn clamp(&mut self) {
        for i in 0..3 {
            self.0[i] = self.0[i].clamp(0., 1.1);
        }
    }
    fn inc(&mut self, flavour: Flavour) {
        for f in Flavour::ALL {
            self.0[f as usize] += if f == flavour { 0.06 } else { -0.01 };
            self.clamp();
        }
    }

    fn dec(&mut self, flavour: Flavour) {
        for f in Flavour::ALL {
            self.0[f as usize] += if f == flavour { -0.4 } else { 0.01 };
        }
        self.clamp();
    }

    fn get(&self, flavour: Flavour) -> f32 {
        self.0[flavour as usize]
    }

    fn is_dead(&self) -> Option<String> {
        for flavour in Flavour::ALL {
            if 1. <= self.get(flavour) {
                let cause = match flavour {
                    Flavour::Red => "catastrophic gastric implosion",
                    Flavour::Blue => "encephalon emancipation syndrome",
                    Flavour::Yellow => "total bone disintegration",
                };
                return Some(cause.into());
            }
        }
        None
    }
}

#[derive(Resource)]
struct Level(u64);

impl Default for Level {
    fn default() -> Self {
        Self(1)
    }
}

#[derive(Component)]
struct LevelLabel;

fn spawn_level_display(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    level: Res<Level>,
) {
    let text_style = TextStyle {
        font_size: 14.,
        font: asset_server.load(FONT_PATH),
        color: Color::WHITE,
    };

    let num_style = TextStyle {
        font_size: 14.,
        font: asset_server.load(FONT_BOLD_PATH),
        color: Color::YELLOW,
    };

    commands.spawn(NodeBundle {
        style: Style {
            position_type: PositionType::Absolute,
            bottom: Val::Px(10.),
            size: Size::width(Val::Percent(100.)),
            justify_content: JustifyContent::Center,
            padding: UiRect::vertical(Val::Px(2.)),
            ..Default::default()
            
        },
        background_color: Color::BLACK.with_a(0.85).into(),
        ..Default::default()
    }).insert(GameUiElement)
    .with_children(|parent| {
        parent.spawn(TextBundle::from_section("Level ", text_style));
        parent.spawn(TextBundle::from_section(level.0.to_string(), num_style)).insert(LevelLabel);
    });
}

fn update_level_display( 
    level: Res<Level>,
    mut text_query: Query<&mut Text, With<LevelLabel>>,
) {
    if level.is_changed() {
        for mut text in text_query.iter_mut() {
            text.sections[0].value = level.0.to_string();
        }
    
    }
}


#[derive(Component)]
struct VitalDisplay(usize);

fn spawn_vitals_display(mut commands: Commands, asset_server: Res<AssetServer>) {
    let vitals = Vitals::default();
    let root = commands
        .spawn(NodeBundle {
            style: Style {
                size: Size::new(Val::Percent(100.0), Val::Percent(100.0)),
                position_type: PositionType::Absolute,
                justify_content: JustifyContent::End,
                align_items: AlignItems::Center,
                ..Default::default()
            },
            ..Default::default()
        })
        .insert(GameUiElement)
        .id();
    let panel = commands
        .spawn(NodeBundle {
            style: Style {
                flex_direction: FlexDirection::Column,
                size: Size::width(Val::Px(300.)),
                margin: UiRect::all(Val::Px(10.)),
                gap: Size::all(Val::Px(10.)),
                align_items: AlignItems::Stretch,
                justify_content: JustifyContent::Center,
                ..Default::default()
            },
            ..Default::default()
        })
        .id();
    commands.entity(root).add_child(panel);

    let labels = [
        "metabolic elasticity",
        "cranial viscosity",
        "skeletal resonance",
    ];

    let text_style = TextStyle {
        font_size: 20.,
        font: asset_server.load(FONT_PATH),
        color: Color::WHITE,
    };

    for i in 0..3 {
        let label = labels[i];
        let flavour = Flavour::ALL[i];
        let inner = commands
            .spawn(NodeBundle {
                style: Style {
                    flex_direction: FlexDirection::Column,
                    gap: Size::all(Val::Px(5.)),
                    align_items: AlignItems::Stretch,
                    padding: UiRect::all(Val::Px(5.)),
                    ..Default::default()
                },
                background_color: Color::NAVY.with_a(0.2).into(),
                ..Default::default()
            })
            .with_children(|parent| {
                parent.spawn(TextBundle::from_section(label, text_style.clone()));

                parent
                    .spawn(NodeBundle {
                        style: Style {
                            flex_direction: FlexDirection::RowReverse,
                            size: Size::height(Val::Px(20.)),
                            ..default()
                        },
                        background_color: flavour.color().into(),
                        ..Default::default()
                    })
                    .with_children(|parent| {
                        parent.spawn((
                            NodeBundle {
                                style: Style {
                                    size: Size::width(Val::Percent(100.)),
                                    ..default()
                                },
                                background_color: Color::BLACK.with_a(0.8).into(),
                                ..default()
                            },
                            flavour,
                        ));
                    });
            })
            .id();
        commands.entity(panel).add_child(inner);
    }

    commands.insert_resource(vitals);
}

fn update_vitals_display(vitals: Res<Vitals>, mut bar_query: Query<(&mut Style, &Flavour)>) {
    if vitals.is_changed() {
        for (mut style, flavour) in bar_query.iter_mut() {
            let value = vitals.get(*flavour);
            style.size.width = Val::Percent((1. - value) * 100.);
        }
    }
}

fn gray(value: f32) -> Color {
    Color::rgb(value, value, value)
}

fn spawn_organ_walls(
    commands: &mut Commands,
    wall_textures: &[Handle<Image>],
    back_textures: &[Handle<Image>],
    organ_inner_area: Grid,
    pipe_point: Pos,
    pipe_pipe_len: i16,
    organ: &mut Organ,
) {
    let rng = &mut thread_rng();
    let inner = organ_inner_area;
    let outer = inner.expand(Dim::ONE);
    let mut pipe = HashSet::default();

    for y in pipe_point.y..pipe_point.y + pipe_pipe_len {
        let pos = pos(pipe_point.x, y);
        pipe.insert(pos);
        commands.spawn((
            back_textures.choose(rng).unwrap().clone(),
            pos,
            Depth::PillPipe,
            Tint(gray(0.4)),
            Dir::random(rng),
            Flip::random(rng),
            Visibility::Visible,
            ComputedVisibility::default(),
        ));
        commands.spawn((
            WallBundle::random(rng, wall_textures, pos.left(), Depth::PipeWall)
                .tint(Color::rgb(0.8, 0.8, 1.0))
                .add_shadow(),
            WallEffect(-4. * Vec2::X),
        ));
        commands.spawn((
            WallBundle::random(rng, wall_textures, pos.right(), Depth::PipeWall)
                .tint(Color::rgb(0.8, 0.8, 1.0))
                .add_shadow(),
            WallEffect(4. * Vec2::X),
        ));
    }

    for pos in outer.into_iter().filter(|pos| !pipe.contains(pos)) {
        if inner.contains(pos) {
            commands.spawn((
                back_textures.choose(rng).unwrap().clone(),
                pos,
                Depth::OrganBack,
                Tint(gray(0.7)),
                Dir::random(rng),
                Flip::random(rng),
                Visibility::Visible,
                ComputedVisibility::default(),
            ));
        } else {
            let wx = if pos.x == outer.pos.x {
                -1.
            } else if pos.x == inner.max_x() {
                1.
            } else {
                0.
            };
            let wy = if pos.y == outer.pos.y {
                -1.
            } else if pos.y == inner.max_y() {
                1.
            } else {
                0.
            };

            let w = 8. * vec2(wx, wy);

            let id = commands
                .spawn((
                    WallBundle::random(rng, wall_textures, pos, Depth::OrganWall).add_shadow(),
                    WallEffect(w),
                ))
                .id();
            organ.insert(pos, id);
        }
    }
}

#[derive(Resource)]
struct Images(Vec<Handle<Image>>);

fn load_typed_folder<T: TypeUuid + 'static + Send + Sync>(
    asset_server: &AssetServer,
    path: &str,
) -> Result<Vec<Handle<T>>, Box<dyn std::error::Error>> {
    let handles = asset_server.load_folder(path)?;
    let image_handles: Vec<Handle<T>> = handles.into_iter().map(|handle| handle.typed()).collect();
    Ok(image_handles)
}

fn spawn_germs(
    commands: &mut Commands,
    count: usize,
    organ: &mut Organ,
    germ_spawn_points: impl IntoIterator<Item = Pos>,
    asset_server: &AssetServer,
) {
    let mut rng = rand::thread_rng();
    let mut cells: Vec<Pos> = germ_spawn_points.into_iter().collect();
    cells.shuffle(&mut rng);
    let n = count.min(cells.len());
    for (i, cell) in cells.into_iter().take(n).enumerate() {
        let flavour = Flavour::ALL[i % Flavour::ALL.len()];
        let bug_id = commands
            .spawn(
                (
                    FlavouredSpriteBundle::new(
                        asset_server.load(flavour.bug_sprite_path()),
                        cell,
                        flavour,
                    )
                    .depth(Depth::Bug),
                    Germ,
                    Flip::default(),
                    Flipper {
                        remaining: 0.35,
                        total: 0.35,
                    },
                    Exploder,
                )
                    .add_shadow(),
            )
            .id();
        organ.insert(cell, bug_id);
    }
}

#[derive(Component)]
struct DropTimer(f32);

#[derive(Component)]
struct PillPart;

#[derive(Component)]
struct Joined(Entity);

fn spawn_new_pill(
    organ: OrganMap,
    active_pill_query: Query<Entity, With<ActivePill>>,
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    rules: Res<GameRules>,
    mut pill_queue: ResMut<PillQueue>,
) { 
    let flavours = pill_queue.pop_next_pill();
    let position = organ.map.spawn_area.pos;
    let pill_bottom = flavours[0];
    let pill_top = flavours[1];
    let pill_texture = asset_server.load(PILL_SPRITE_PATH);
    if active_pill_query.is_empty() {
        let a = commands.spawn_empty().id();
        let b = commands.spawn_empty().id();
        commands.entity(a).insert(
            (
                FlavouredSpriteBundle::new(pill_texture.clone(), position, pill_bottom)
                    .depth(Depth::Pill)
                    .orientation(Dir::Down)
                    .color(pill_bottom.color()),
                PivotPill,
                ActivePill,
                DropTimer(rules.drop_delay),
                XMoveTimer(rules.horizontal_move_delay),
                PillPart,
                Joined(b),
                Exploder,
            )
                .add_shadow(),
        );
        commands.entity(b).insert(
            (
                FlavouredSpriteBundle::new(pill_texture, position.up(), pill_top)
                    .depth(Depth::Pill)
                    .orientation(Dir::Up)
                    .color(pill_top.color()),
                ActivePill,
                PillPart,
                Joined(a),
                Exploder,
            )
                .add_shadow(),
        );
    }
}

fn update_pill_timers(
    time: Res<Time>,
    pill_commands: Res<PillCommands>,
    rules: Res<GameRules>,
    mut drop_timer_query: Query<&mut DropTimer>,
    mut horizontal_timer_query: Query<&mut XMoveTimer>,
) {
    for mut t in drop_timer_query.iter_mut() {
        let multiplier = if pill_commands.drop {
            rules.fast_drop_multiplier
        } else {
            1.
        };
        t.0 -= time.delta_seconds() * multiplier;
    }
    for mut t in horizontal_timer_query.iter_mut() {
        t.0 -= time.delta_seconds();
    }
}

#[derive(Bundle)]
struct WallBundle {
    texture: Handle<Image>,
    pos: Pos,
    depth: Depth,
    dir: Dir,
    flip: Flip,
    tint: Tint,
    visibility: Visibility,
    computed_visibility: ComputedVisibility,
    wall: Wall,
}

impl WallBundle {
    fn new_wall(texture: Handle<Image>, pos: Pos) -> Self {
        Self {
            texture,
            pos,
            depth: Depth::OrganWall,
            tint: Color::WHITE.into(),
            dir: Dir::Up,
            flip: Flip::default(),
            visibility: Visibility::Visible,
            computed_visibility: ComputedVisibility::default(),
            wall: Wall,
        }
    }

    fn new_back(texture: Handle<Image>, pos: Pos) -> Self {
        Self {
            texture,
            pos,
            depth: Depth::OrganBack,
            tint: Color::WHITE.into(),
            dir: Dir::Up,
            flip: Flip::default(),
            visibility: Visibility::Visible,
            computed_visibility: ComputedVisibility::default(),
            wall: Wall,
        }
    }

    fn random(rng: &mut impl Rng, textures: &[Handle<Image>], pos: Pos, depth: Depth) -> Self {
        Self {
            texture: textures.choose(rng).unwrap().clone(),
            pos,
            depth,
            tint: Color::WHITE.into(),
            dir: Dir::random(rng),
            flip: Flip::random(rng),
            visibility: Visibility::Visible,
            computed_visibility: ComputedVisibility::default(),
            wall: Wall,
        }
    }

    fn tint(mut self, color: Color) -> Self {
        self.tint = Tint(color);
        self
    }
}

#[derive(Bundle)]
struct RectBundle {
    texture: Handle<Image>,
    pos: Pos,
    depth: Depth,
    dir: Dir,
    flip: Flip,
    tint: Tint,
    visibility: Visibility,
    computed_visibility: ComputedVisibility,
}

impl RectBundle {
    fn new(color: Color, pos: Pos, depth: Depth) -> Self {
        RectBundle {
            texture: DEFAULT_IMAGE_HANDLE.typed(),
            pos,
            depth,
            tint: color.into(),
            dir: Dir::Up,
            flip: Flip::default(),
            visibility: Visibility::Visible,
            computed_visibility: ComputedVisibility::default(),
        }
    }
}

#[derive(Bundle)]
struct FlavouredSpriteBundle {
    texture: Handle<Image>,
    pos: Pos,
    depth: Depth,
    orientation: Dir,
    tint: Tint,
    flavour: Flavour,
    visibility: Visibility,
    computed_visibility: ComputedVisibility,
}

impl FlavouredSpriteBundle {
    fn new(texture: Handle<Image>, pos: Pos, flavour: Flavour) -> Self {
        FlavouredSpriteBundle {
            texture,
            pos,
            depth: Depth::Top,
            orientation: Dir::Up,
            tint: Color::WHITE.into(),
            flavour,
            visibility: Visibility::Visible,
            computed_visibility: ComputedVisibility::default(),
        }
    }

    fn depth(mut self, depth: Depth) -> Self {
        self.depth = depth;
        self
    }

    fn orientation(mut self, orientation: Dir) -> Self {
        self.orientation = orientation;
        self
    }

    fn color(mut self, color: Color) -> Self {
        self.tint = color.into();
        self
    }
}

#[derive(Component, Default, Copy, Clone)]
struct Tint(Color);

impl From<Color> for Tint {
    fn from(color: Color) -> Self {
        Tint(color)
    }
}

fn orient_pill(
    pill_commands: ResMut<PillCommands>,
    mut pivot_pill_query: Query<(&Pos, &mut Dir), (With<PivotPill>, With<ActivePill>)>,
    mut end_pill_query: Query<(&mut Pos, &mut Dir), (Without<PivotPill>, With<ActivePill>)>,
    organ_map: OrganMap,
) {
    let Some(dir) = pill_commands.dir else { return; };
    if dir.is_horizontal() {
        let (mut end_pos, mut end_dir) = end_pill_query.single_mut();
        let (pivot_pos, mut pivot_dir) = pivot_pill_query.single_mut();
        if dir == *end_dir {
            return;
        }
        let new_dir = if dir == Dir::Left {
            end_dir.rotate_left()
        } else {
            end_dir.rotate_right()
        };
        let new_end = pivot_pos.step(new_dir);
        if organ_map.is_empty(new_end) {
            *end_pos = new_end;
            *end_dir = new_dir;
            *pivot_dir = new_dir.opposite();
        }
    }
}

fn swap_mut<T>(mut a: Mut<T>, mut b: Mut<T>) {
    std::mem::swap(&mut (*a), &mut (*b));
}

fn swap_pill_ends(
    pill_commands: Res<PillCommands>,
    mut pivot_pill_query: Query<
        (&mut Flavour, &mut Tint, &mut Handle<Image>),
        (With<PivotPill>, With<ActivePill>),
    >,
    mut end_pill_query: Query<
        (&mut Flavour, &mut Tint, &mut Handle<Image>),
        (Without<PivotPill>, With<ActivePill>),
    >,
) {
    let (ef, et, ei) = end_pill_query.single_mut();
    let (pf, pt, pi) = pivot_pill_query.single_mut();
    if let Some(dir) = pill_commands.dir {
        if !dir.is_horizontal() {
            swap_mut(ef, pf);
            swap_mut(et, pt);
            swap_mut(ei, pi);
        }
    }
}

#[derive(Resource, Default)]
struct PillCommands {
    dir: Option<Dir>,
    x_move: i16,
    immediate_move: bool,
    drop: bool,
}

fn handle_input(keyboard: Res<Input<KeyCode>>, mut pill_command: ResMut<PillCommands>) {
    pill_command.x_move = 0;
    let obinds = [
        (KeyCode::W, Dir::Up),
        (KeyCode::A, Dir::Left),
        (KeyCode::S, Dir::Down),
        (KeyCode::D, Dir::Right),
    ];

    pill_command.dir = None;
    for (key, dir) in obinds {
        if keyboard.just_pressed(key) {
            pill_command.dir = Some(dir);
        }
    }

    pill_command.immediate_move = false;
    if keyboard.pressed(KeyCode::Left) {
        pill_command.x_move -= 1;
        if keyboard.just_pressed(KeyCode::Left) {
            pill_command.immediate_move = true;
        }
    }
    if keyboard.pressed(KeyCode::Right) {
        pill_command.x_move += 1;
        if keyboard.just_pressed(KeyCode::Right) {
            pill_command.immediate_move = true;
        }
    }

    pill_command.drop = false;
    if keyboard.pressed(KeyCode::Down) {
        pill_command.drop = true;
    }
}

#[derive(Component)]
struct XMoveTimer(f32);

fn move_pill(
    game_config: Res<GameRules>,
    mut pivot_pill_query: Query<(&mut Pos, &mut XMoveTimer), (With<PivotPill>, With<ActivePill>)>,
    mut end_pill_query: Query<&mut Pos, (Without<PivotPill>, With<ActivePill>)>,
    pill_commands: Res<PillCommands>,
    //jar: Res<Organ>,
    organ: OrganMap,
) {
    let (mut pivot_pos, mut delay) = pivot_pill_query.single_mut();
    let mut end_pos = end_pill_query.single_mut();

    if pill_commands.x_move != 0 {
        if delay.0 < 0. || pill_commands.immediate_move == true {
            let new_pivot_pos = pivot_pos.translate(pill_commands.x_move, 0);
            let new_end_pos = end_pos.translate(pill_commands.x_move, 0);
            if [new_pivot_pos, new_end_pos]
                .iter()
                .all(|&pos| organ.map.is_empty(pos))
            {
                *pivot_pos = new_pivot_pos;
                *end_pos = new_end_pos;
                delay.0 = game_config.horizontal_move_delay;
            }
        }
    }
}

fn drop_pill(
    mut pivot_query: Query<(Entity, &mut Pos, &mut DropTimer), (With<ActivePill>, With<PivotPill>)>,
    mut end_query: Query<(Entity, &mut Pos), (With<ActivePill>, Without<PivotPill>)>,
    rules: Res<GameRules>,
    mut organ: OrganMap,
    mut commands: Commands,
    mut next_game_state: ResMut<NextState<GameState>>,
) {
    let (pivot, mut pivot_pos, mut drop_timer) = pivot_query.single_mut();
    let (end, mut end_pos) = end_query.single_mut();
    if drop_timer.0 < 0. {
        drop_timer.0 = rules.drop_delay;
        let next_pivot_pos = pivot_pos.down();
        let next_end_pos = end_pos.down();
        if organ.map.is_empty(next_pivot_pos) && organ.map.is_empty(next_end_pos) {
            *pivot_pos = next_pivot_pos;
            *end_pos = next_end_pos;
        } else {
            organ
                .map
                .insert_joined([(*pivot_pos, pivot), (*end_pos, end)]);

            commands
                .entity(pivot)
                .remove::<(ActivePill, PivotPill, DropTimer, XMoveTimer)>();
            commands.entity(end).remove::<ActivePill>();
            next_game_state.set(GameState::PillLanded);
        }
    }
}

/// find rows and columns of pills and bugs with matching flavours
fn find_flavour_lines(
    flavour: impl Fn(Pos) -> Option<Flavour>,
    bounds: Grid,
    min_flavour_line_len: i16,
) -> HashSet<Pos> {
    let mut out = HashSet::default();

    for x in bounds.x_range() {
        for y in bounds.y_range() {
            let p = flavour(pos(x, y));
            if p.is_none() {
                continue;
            }
            let p = p.unwrap();

            // Check vertical
            if y + min_flavour_line_len <= bounds.max_y() {
                let all_match = (y + 1..y + min_flavour_line_len)
                    .map(|y| pos(x, y))
                    .all(|q| flavour(q) == Some(p));

                if all_match {
                    for y in y..y + min_flavour_line_len {
                        out.insert(pos(x, y));
                    }
                }
            }

            // Check horizontal
            if x + min_flavour_line_len <= bounds.max_x() {
                let all_match = (x + 1..x + min_flavour_line_len)
                    .map(|x| pos(x, y))
                    .all(|q| flavour(q) == Some(p));

                if all_match {
                    for x in x..x + min_flavour_line_len {
                        out.insert(pos(x, y));
                    }
                }
            }
        }
    }
    out
}

fn match_flavour_lines(
    asset_server: Res<AssetServer>,
    mut commands: Commands,
    mut organ: OrganMap,
    rules: Res<GameRules>,
    mut next_state: ResMut<NextState<GameState>>,
    mut joined_query: Query<&Joined>,
    mut image_query: Query<&mut Handle<Image>>,
    mut killed_event: EventWriter<KilledSomethingEvent>,
    vitals: Res<Vitals>,
    mut cause_of_death: ResMut<CauseOfDeath>,
) {
    let matches = find_flavour_lines(|pos| organ.flavour(pos), organ.map.bounds(), rules.matches);
    let mut removed = HashSet::default();
    if !matches.is_empty() {
        for pos in matches {
            let entity = organ.map.remove(pos).unwrap();
            if let Ok(join) = joined_query.get_mut(entity) {
                if !removed.contains(&join.0) {
                    let mut image = image_query.get_mut(join.0).unwrap();
                    *image = asset_server.load(PILL_BROKE_SPRITE_PATH);
                    commands.entity(join.0).remove::<Joined>();
                }
            }
            killed_event.send(KilledSomethingEvent(entity));
            commands.entity(entity).despawn();
            removed.insert(entity);
        }
        commands.insert_resource(NextStateTimer {
            remaining: 0.4,
            next: GameState::Collapsing,
        });
        next_state.set(GameState::Wait);
    } else {        
        if !organ.map.spawn_area.into_iter().all(|pos| organ.is_empty(pos)) {
            cause_of_death.0 = "too many pills".into();
            next_state.set(GameState::GameOver);
        } else if let Some(s) = vitals.is_dead() {
            cause_of_death.0 = s;
            next_state.set(GameState::GameOver);
        } else {
            commands.insert_resource(NextStateTimer {
                remaining: 0.4,
                next: GameState::PillFalling,
            });
            next_state.set(GameState::Wait);
        }
    }
}

#[derive(Resource, Default)] 
struct CauseOfDeath(String);

struct KilledSomethingEvent(Entity);

#[derive(Component)]
struct Exploder;

fn explode_on_killed(
    mut kill_events: EventReader<KilledSomethingEvent>,
    mut spawn_explosion: EventWriter<SpawnExplosionEvent>,
    query: Query<(&Pos, &Flavour), With<Exploder>>,
) {
    for KilledSomethingEvent(id) in kill_events.iter() {
        if let Ok((pos, flavour)) = query.get(*id) {
            spawn_explosion.send(SpawnExplosionEvent {
                pos: *pos,
                color: flavour.color(),
            });
        }
    }
}

fn update_kill_count_and_score(
    mut kill_events: EventReader<KilledSomethingEvent>,
    mut score_points: EventWriter<ScorePointsEvent>,
    mut combo_kill_counter: ResMut<ComboKillCounter>,
    query: Query<(), With<Germ>>,
) {
    for KilledSomethingEvent(id) in kill_events.iter() {
        if query.contains(*id) {
            combo_kill_counter.0 += 1;
            let mut points = 0;
            for i in 1..=combo_kill_counter.0 {
                points += 10 * i;
            }
            score_points.send(ScorePointsEvent(points));
        }
    }
}

fn update_vitals_on_kill_pill(
    mut vitals: ResMut<Vitals>,
    flavour_query: Query<&Flavour, Without<Germ>>,
    mut kill_events: EventReader<KilledSomethingEvent>,
) {
    for &KilledSomethingEvent(id) in kill_events.iter() {
        if let Ok(flavour) = flavour_query.get(id) {
            if flavour_query.contains(id) {
                vitals.inc(*flavour);
            }
        }
    }
}

fn update_vitals_on_kill_germ(
    mut vitals: ResMut<Vitals>,
    flavour_query: Query<&Flavour, With<Germ>>,
    mut kill_events: EventReader<KilledSomethingEvent>,
) {
    for &KilledSomethingEvent(id) in kill_events.iter() {
        if let Ok(flavour) = flavour_query.get(id) {
            if flavour_query.contains(id) {
                vitals.dec(*flavour);
            }
        }
    }
}

#[derive(Component, Clone, Copy, Debug, Default, PartialEq, Eq)]
enum Dir {
    /// sprites should point up
    /// flat end of the pill at bottom
    #[default]
    Up,
    /// flat end of the pill at top
    Down,
    /// flat end of the pill at right
    Left,
    /// flat end of the pill at left
    Right,
}

impl Dir {
    const ALL: [Self; 4] = {
        use Dir::*;
        [Up, Right, Down, Left]
    };
    fn random<R: Rng>(rng: &mut R) -> Self {
        *Self::ALL.choose(rng).unwrap()
    }
    fn angle(self) -> f32 {
        match self {
            Dir::Up => 0.,
            Dir::Down => PI,
            Dir::Left => FRAC_PI_2,
            Dir::Right => -FRAC_PI_2,
        }
    }

    fn opposite(self) -> Dir {
        match self {
            Dir::Up => Dir::Down,
            Dir::Down => Dir::Up,
            Dir::Left => Dir::Right,
            Dir::Right => Dir::Left,
        }
    }

    fn is_horizontal(self) -> bool {
        matches!(self, Dir::Left | Dir::Right)
    }

    fn rotate_left(self) -> Self {
        match self {
            Dir::Up => Dir::Left,
            Dir::Left => Dir::Down,
            Dir::Down => Dir::Right,
            Dir::Right => Dir::Up,
        }
    }

    fn rotate_right(self) -> Self {
        match self {
            Dir::Up => Dir::Right,
            Dir::Right => Dir::Down,
            Dir::Down => Dir::Left,
            Dir::Left => Dir::Up,
        }
    }
}

#[derive(Resource)]
struct OrganGeometry {
    translation: Vec3,
    cell_size: Vec2,
}

impl OrganGeometry {
    #[inline]
    fn map_pos(&self, pos: Pos) -> Vec3 {
        ((0.5 + Vec2::from(pos)) * self.cell_size).extend(0.) + self.translation
    }
}

#[derive(Component, Copy, Clone, Debug, Eq, PartialEq)]
enum Depth {
    Shadow,
    OrganBack,
    PipeWall,
    OrganWall,
    Bug,
    Pill,
    Top,
    PillPipe,
}

impl Depth {
    fn z(self) -> f32 {
        match self {
            Depth::OrganBack => 0.,
            Depth::PillPipe => 1.,
            Depth::Shadow => 2.,
            Depth::PipeWall => 90.,
            Depth::OrganWall => 100.,
            Depth::Bug => 10.,
            Depth::Pill => 20.,
            Depth::Top => 500.,
        }
    }
}

#[derive(Default, Clone, Copy, Component)]
struct Flip {
    x: bool,
    y: bool,
}

impl Flip {
    fn random<R: Rng>(rng: &mut R) -> Self {
        Self {
            x: rng.gen(),
            y: rng.gen(),
        }
    }
}

#[derive(Component)]
struct Shadow;

trait Shadowed: Sized {
    fn add_shadow(self) -> (Self, Shadow) {
        (self, Shadow)
    }
}

impl<B> Shadowed for B where B: Bundle {}

const SHADOW_OFFSET: Vec3 = vec3(3., -3., 0.);

#[derive(Resource)]
struct ShadowOffset(Vec3);

impl Default for ShadowOffset {
    fn default() -> Self {
        Self(SHADOW_OFFSET)
    }
}
const SHADOW_ALPHA: f32 = 0.5;

fn extract_organ_sprites(
    mut extracted_sprites: ResMut<ExtractedSprites>,
    organ_geometry: Extract<Res<OrganGeometry>>,
    shadow_offset: Extract<Res<ShadowOffset>>,
    sprite_query: Extract<
        Query<
            (
                Entity,
                &ComputedVisibility,
                &Pos,
                &Handle<Image>,
                &Dir,
                &Tint,
                &Depth,
                Option<&Flip>,
                Option<&Shadow>,
            ),
            Without<WallEffect>,
        >,
    >,
    time: Extract<Res<Time>>,
) {
    let s = (1.0 + time.elapsed_seconds().sin()) / 2.;
    for (entity, vis, pos, image, orientation, &Tint(color), depth, flip, shadow) in
        sprite_query.iter()
    {
        if !vis.is_visible() {
            continue;
        };

        let flip = flip.copied().unwrap_or_default();
        let translation = organ_geometry.map_pos(*pos);
        let size = organ_geometry.cell_size;
        let m = Mat3A::from_rotation_z(orientation.angle());

        if shadow.is_some() {
            extracted_sprites.sprites.push(ExtractedSprite {
                entity,
                color: Color::BLACK.with_a(SHADOW_ALPHA),
                transform: Affine3A {
                    matrix3: m,
                    translation: (translation + shadow_offset.0 + Depth::Shadow.z() * Vec3::Z)
                        .into(),
                }
                .into(),
                rect: None,
                custom_size: Some(size),
                flip_x: flip.x,
                flip_y: flip.y,
                image_handle_id: image.id(),
                anchor: Vec2::ZERO,
            });
        }

        extracted_sprites.sprites.push(ExtractedSprite {
            entity,
            color,
            transform: Affine3A {
                matrix3: m,
                translation: (translation + depth.z() * Vec3::Z).into(),
            }
            .into(),
            rect: None,
            custom_size: Some(size),
            flip_x: flip.x,
            flip_y: flip.y,
            image_handle_id: image.id(),
            anchor: Vec2::ZERO,
        });
    }
}

#[derive(Resource)]
#[derive(Debug)]
struct Danger {
    rate: f32,
    pow: f32,
    rate_target: f32,
    pow_target: f32,
}

fn update_danger(time: Res<Time>, vitals: Res<Vitals>, mut danger: ResMut<Danger>) {
    let a = vitals.0[0];
    let b = vitals.0[1];
    let c = vitals.0[2];
    danger.rate_target = 1. + 7. * a + 3. * c;
    danger.pow_target = 1. + 2.5 * b + 1.5 * c;

    let dr = danger.rate_target - danger.rate;
    let dp = danger.pow_target - danger.pow;
    danger.rate += dr * time.delta_seconds();
    danger.pow += dp * time.delta_seconds();

}

impl Default for Danger {
    fn default() -> Self {
        Self {
            rate: 1.,
            pow: 1.,
            rate_target: 1.,
            pow_target: 1.,
        }
    }
}

fn extract_wall_sprites(
    mut extracted_sprites: ResMut<ExtractedSprites>,
    organ_geometry: Extract<Res<OrganGeometry>>,
    shadow_offset: Extract<Res<ShadowOffset>>,
    sprite_query: Extract<
        Query<(
            Entity,
            &ComputedVisibility,
            &Pos,
            &Handle<Image>,
            &Tint,
            &Depth,
            Option<&Flip>,
            Option<&Shadow>,
            &WallEffect,
        )>,
    >,
    time: Extract<Res<Time>>,
    danger: Extract<Res<Danger>>,
) {
    let r = danger.rate;
    let a = 1. * danger.pow;
    let s = a * (1.0 + (r * time.elapsed_seconds()).cos()) / 2.;

    for (entity, vis, pos, image, &Tint(color), depth, flip, shadow, wall_effect) in
        sprite_query.iter()
    {
        if !vis.is_visible() {
            continue;
        };

        let flip = flip.copied().unwrap_or_default();
        let size = organ_geometry.cell_size + wall_effect.0.abs() * s;
        let translation = organ_geometry.map_pos(*pos) + (0.5 * wall_effect.0 * s).extend(0.);
        if shadow.is_some() {
            extracted_sprites.sprites.push(ExtractedSprite {
                entity,
                color: Color::BLACK.with_a(SHADOW_ALPHA),
                transform: Affine3A {
                    translation: (translation + shadow_offset.0 + Depth::Shadow.z() * Vec3::Z)
                        .into(),
                    ..Default::default()
                }
                .into(),
                rect: None,
                custom_size: Some(size),
                flip_x: flip.x,
                flip_y: flip.y,
                image_handle_id: image.id(),
                anchor: Vec2::ZERO,
            });
        }

        extracted_sprites.sprites.push(ExtractedSprite {
            entity,
            color,
            transform: Affine3A {
                translation: (translation + depth.z() * Vec3::Z).into(),
                ..Default::default()
            }
            .into(),
            rect: None,
            custom_size: Some(size),
            flip_x: flip.x,
            flip_y: flip.y,
            image_handle_id: image.id(),
            anchor: Vec2::ZERO,
        });
    }
}

#[derive(Resource)]
struct PillSpriteHandle(Handle<Image>);

fn extract_pill_queue(
    mut extracted_sprites: ResMut<ExtractedSprites>,
    organ_geometry: Extract<Res<OrganGeometry>>,
    shadow_offset: Extract<Res<ShadowOffset>>,
    pill_queue: Extract<Res<PillQueue>>,
    pill_handle: Extract<Res<PillSpriteHandle>>,
    organ: Extract<Res<Organ>>,
    query: Extract<Query<Entity, With<PillSpriteQueueMarker>>>,
) {
    let mut cursor = organ.spawn_area.pos.up();
    let entity = query.single();

    for &pill in pill_queue.pills.iter() {
        for (i, flavour) in pill.into_iter().enumerate() {
            cursor = cursor.up();
            let translation = organ_geometry.map_pos(cursor);
            let orientation = if i == 0 { Dir::Down } else { Dir::Up };
            let m = Mat3A::from_rotation_z(orientation.angle());
            extracted_sprites.sprites.push(ExtractedSprite {
                entity,
                color: Color::BLACK.with_a(SHADOW_ALPHA),
                transform: Affine3A {
                    matrix3: m,
                    translation: (translation + shadow_offset.0 + Depth::Shadow.z() * Vec3::Z)
                        .into(),
                    ..Default::default()
                }
                .into(),
                rect: None,
                custom_size: Some(organ_geometry.cell_size),
                flip_x: false,
                flip_y: false,
                image_handle_id: pill_handle.0.id(),
                anchor: Vec2::ZERO,
            });

            extracted_sprites.sprites.push(ExtractedSprite {
                entity,
                color: flavour.color(),
                transform: Affine3A {
                    matrix3: m,
                    translation: (translation + Depth::Pill.z() * Vec3::Z).into(),
                    ..Default::default()
                }
                .into(),
                rect: None,
                custom_size: Some(organ_geometry.cell_size),
                flip_x: false,
                flip_y: false,
                image_handle_id: pill_handle.0.id(),
                anchor: Vec2::ZERO,
            });
        }
    }
}

#[derive(Resource)]
pub struct NextStateTimer<T> {
    remaining: f32,
    next: T,
}

fn wait_system(
    time: Res<Time>,
    mut next: ResMut<NextStateTimer<GameState>>,
    mut next_state: ResMut<NextState<GameState>>,
) {
    next.remaining -= time.delta_seconds();
    if next.remaining < 0. {
        next_state.set(next.next);
    }
}

fn collapse_pills(
    mut commands: Commands,
    mut jar: ResMut<Organ>,
    mut pill_query: Query<&mut Pos, With<PillPart>>,
    join_query: Query<&Joined>,
    mut next_state: ResMut<NextState<GameState>>,
) {
    let mut drop = false;
    for row in jar.bounds().y_range() {
        let spaces: Vec<(Pos, Entity)> = jar
            .bounds()
            .x_range()
            .map(|x| pos(x, row))
            .filter(|&pos| jar.is_empty(pos))
            .filter_map(|pos| jar.get(pos.up()).map(|e| (pos, e)))
            .filter(|(_, e)| pill_query.contains(*e))
            .collect();
        let set: HashSet<Entity> = spaces.iter().map(|(_, e)| e).copied().collect();
        for (pos, entity_above) in spaces.into_iter() {
            if let Ok(Joined(other)) = join_query.get(entity_above) {
                if let Ok(other_pos) = pill_query.get(*other) {
                    if set.contains(other) || other_pos.y != pos.up().y {
                        jar.remove_entity(entity_above);
                        *pill_query.get_mut(entity_above).unwrap() = pos;
                        jar.insert(pos, entity_above);
                        drop = true;
                    }
                }
            } else {
                jar.remove_entity(entity_above);
                *pill_query.get_mut(entity_above).unwrap() = pos;
                jar.insert(pos, entity_above);
                drop = true;
            }
        }
    }

    if drop {
        commands.insert_resource(NextStateTimer {
            remaining: 0.25,
            next: GameState::Collapsing,
        });
    } else {
        commands.insert_resource(NextStateTimer {
            remaining: 0.4,
            next: GameState::PillLanded,
        });
    }
    next_state.set(GameState::Wait);
}

struct SpawnExplosionEvent {
    pos: Pos,
    color: Color,
}

#[derive(Component, Clone, Copy)]
struct Velocity(Vec2);

fn velocity_physics(time: Res<Time>, mut query: Query<(&Velocity, &mut Transform)>) {
    for (v, mut tf) in query.iter_mut() {
        let v = v.0 * time.delta_seconds();
        tf.translation.x += v.x;
        tf.translation.y += v.y;
    }
}

#[derive(Component, Clone, Copy)]
struct FadeOut(f32);

fn fade_out(time: Res<Time>, mut query: Query<(&mut Sprite, &FadeOut)>) {
    for (mut s, f) in query.iter_mut() {
        if let Some(ref mut c) = s.custom_size {
            *c *= 1. - f.0 * time.delta_seconds();
        }
    }
}

fn spawn_explosions(
    mut explosion_events: EventReader<SpawnExplosionEvent>,
    mut commands: Commands,
    organ_geometry: Res<OrganGeometry>,
) {
    let rng = &mut thread_rng();
    let o = organ_geometry.cell_size.x;
    let range_distribution = Uniform::new(0., 0.35 * o);
    let angle_distribution = Uniform::new(0., TAU);
    let speed_distribution = Uniform::new(0.5 * o, 0.9 * o);

    for e in explosion_events.iter() {
        commands
            .spawn((
                SpatialBundle {
                    transform: Transform::from_translation(organ_geometry.map_pos(e.pos)),
                    ..Default::default()
                },
                DespawnTimer(0.66),
            ))
            .with_children(|parent| {
                for _ in 0..20 {
                    let s: f32 = *[3., 4., 5.].choose(rng).unwrap();
                    let displacement = rng.sample(range_distribution);
                    let dir = rng.sample(angle_distribution);
                    let speed = rng.sample(speed_distribution);
                    let q = Quat::from_rotation_z(dir);
                    parent
                        .spawn(SpatialBundle {
                            transform: Transform::from_rotation(q),
                            ..Default::default()
                        })
                        .with_children(|parent| {
                            parent.spawn((
                                SpriteBundle {
                                    sprite: Sprite {
                                        color: e.color,
                                        custom_size: Some(Vec2::splat(s)),
                                        ..Default::default()
                                    },
                                    transform: Transform {
                                        translation: displacement * Vec3::Y
                                            + Depth::Top.z() * Vec3::Z,
                                        ..Default::default()
                                    },
                                    ..Default::default()
                                },
                                Velocity(speed * Vec2::Y),
                                FadeOut(1.75),
                            ));
                        });

                    parent
                        .spawn(SpatialBundle {
                            transform: Transform {
                                translation: SHADOW_OFFSET,
                                rotation: q,
                                ..Default::default()
                            },
                            ..Default::default()
                        })
                        .with_children(|parent| {
                            parent.spawn((
                                SpriteBundle {
                                    sprite: Sprite {
                                        color: Color::BLACK.with_a(SHADOW_ALPHA),
                                        custom_size: Some(Vec2::splat(s)),
                                        ..Default::default()
                                    },
                                    transform: Transform {
                                        translation: displacement * Vec3::Y
                                            + Depth::Shadow.z() * Vec3::Z,
                                        ..Default::default()
                                    },
                                    ..Default::default()
                                },
                                Velocity(speed * Vec2::Y),
                                FadeOut(1.75),
                            ));
                        });
                }
            });
    }
}

#[derive(Component)]
struct Wall;

#[derive(Copy, Clone, Debug)]
enum Tile {
    Germ(Flavour),
    Pill(Flavour),
    Wall,
}

impl Tile {
    fn flavour(self) -> Option<Flavour> {
        match self {
            Tile::Germ(f) => Some(f),
            Tile::Pill(f) => Some(f),
            Tile::Wall => None,
        }
    }
}

#[derive(SystemParam)]
struct OrganMap<'w, 's> {
    map: ResMut<'w, Organ>,
    wall_query: Query<'w, 's, Entity, With<Wall>>,
    pill_query: Query<'w, 's, Entity, (With<PillPart>, Without<ActivePill>)>,
    germ_query: Query<'w, 's, Entity, With<Germ>>,
    flavour_query: Query<'w, 's, &'static mut Flavour, Without<ActivePill>>,
}

impl<'w, 's> OrganMap<'w, 's> {
    fn tile(&self, pos: Pos) -> Option<Tile> {
        self.id(pos).and_then(|entity| {
            if self.wall_query.get(entity).is_ok() {
                Some(Tile::Wall)
            } else if self.pill_query.get(entity).is_ok() {
                Some(Tile::Pill(self.flavour(pos).unwrap()))
            } else if self.germ_query.get(entity).is_ok() {
                Some(Tile::Germ(self.flavour(pos).unwrap()))
            } else {
                None
            }
        })
    }

    fn id(&self, pos: Pos) -> Option<Entity> {
        self.map.get(pos)
    }

    fn is_empty(&self, pos: Pos) -> bool {
        self.map.is_empty(pos)
    }

    fn flavour(&self, pos: Pos) -> Option<Flavour> {
        self.id(pos)
            .map(|id| self.flavour_query.get(id).copied().ok())
            .flatten()
    }
}

#[derive(Component)]
struct DespawnTimer(f32);

fn update_despawn_timers(
    mut commands: Commands,
    time: Res<Time>,
    mut query: Query<(Entity, &mut DespawnTimer)>,
) {
    query.for_each_mut(|(e, mut dt)| {
        dt.0 -= time.delta_seconds();
        if dt.0 < 0. {
            commands.entity(e).despawn_recursive();
        }
    });
}

#[derive(Component)]
struct Flipper {
    remaining: f32,
    total: f32,
}

fn update_flippers(time: Res<Time>, mut flip_query: Query<(&mut Flip, &mut Flipper)>) {
    flip_query.for_each_mut(|(mut flip, mut flipper)| {
        flipper.remaining -= time.delta_seconds();
        if flipper.remaining < 0. {
            flipper.remaining = flipper.total;
            flip.x = !flip.x;
        }
    });
}

#[derive(Component)]
struct WallEffect(Vec2);

struct ScorePointsEvent(u64);

fn update_score(
    mut event_reader: EventReader<ScorePointsEvent>,
    mut score: ResMut<Score>,
    mut high_score: ResMut<HighScore>,
) {
    for points_event in event_reader.iter() {
        score.0 += points_event.0;
    }
    high_score.0 = high_score.0.max(score.0);
}

fn format_score(points: u64) -> String {
    format!("{:0width$}", points, width = SCORE_ZEROS)
}

trait Valuable<T> {
    fn value(&self) -> T;
}

impl Valuable<u64> for Score {
    fn value(&self) -> u64 {
        self.0
    }
}

impl Valuable<u64> for HighScore {
    fn value(&self) -> u64 {
        self.0
    }
}

fn update_score_display<T: Resource + Valuable<u64>>(
    score: Res<T>,
    mut score_text_query: Query<&mut Text, With<Marker<T>>>,
) {
    if score.is_changed() {
        for mut text in score_text_query.iter_mut() {
            text.sections[0].value = format_score(score.value());
        }
    }
}

fn sprite_rect(min: Vec3, size: Vec2, color: Color) -> SpriteBundle {
    SpriteBundle {
        sprite: Sprite {
            color,
            custom_size: Some(size),
            anchor: bevy::sprite::Anchor::BottomLeft,
            ..Default::default()
        },
        transform: Transform::from_translation(min),
        ..Default::default()
    }
}

fn spawn_bordered_sprite_rect(
    commands: &mut Commands,
    min: Vec3,
    size: Vec2,
    border_thickness: f32,
    border_color: Color,
    color: Color,
) -> Entity {
    commands
        .spawn(sprite_rect(min, size, border_color))
        .with_children(|parent| {
            parent.spawn(sprite_rect(
                min + Vec2::splat(border_thickness).extend(0.),
                size - 2.0 * Vec2::splat(border_thickness),
                color,
            ));
        })
        .id()
}

#[derive(Resource, Default)]
struct PillQueue {
    pills: VecDeque<[Flavour; 2]>,
}

impl PillQueue {
    fn new(&mut self) -> Self {
        let rng = &mut thread_rng();
        let mut pills = VecDeque::new();
        for i in 0..10 {
            pills.push_back([
                Flavour::ALL.choose(rng).copied().unwrap(),
                Flavour::ALL.choose(rng).copied().unwrap(),
            ]);
        }
        Self { pills }
    }

    fn pop_next_pill(&mut self) -> [Flavour; 2] {
        let rng = &mut thread_rng();
        self.pills.push_back([
            Flavour::ALL.choose(rng).copied().unwrap(),
            Flavour::ALL.choose(rng).copied().unwrap(),
        ]);
        self.pills.pop_front().unwrap()
    }
}

fn despawn_game_elements(
    mut commands: Commands,
    query: Query<Entity, Or<(With<GameUiElement>, With<Pos>, With<Sprite>)>>,
) {
    for entity in query.iter() {
        commands.entity(entity).despawn_recursive();
    }
}

fn spawn_game_over_message(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    cause: Res<CauseOfDeath>,
) {
    let text_style = TextStyle {
        font_size: 50.,
        font: asset_server.load(FONT_BOLD_PATH),
        color: Color::RED,
    };

    let c_text_style = TextStyle {
        font_size: 40.,
        font: asset_server.load(FONT_PATH),
        color: Color::FUCHSIA,
    };

    commands.spawn((
        NodeBundle {
        style: Style {
            position_type: PositionType::Absolute,
            left: Val::Px(0.),
            top: Val::Px(0.),
            size: Size::all(Val::Percent(100.)),
            align_items: AlignItems::Center,
            justify_content: JustifyContent::Center,
            flex_direction: FlexDirection::Column,
            ..Default::default()
        },
        background_color: Color::BLACK.with_a(0.5).into(),
        ..Default::default()
        },
        GameUiElement,
    )).with_children(|parent| {
        parent.spawn(TextBundle::from_section( 
             "PATIENT DECEASED",
             text_style.clone(),
        ).with_background_color(Color::BLACK.with_a(0.5)));
        parent.spawn(NodeBundle {
            style: Style {
                margin: UiRect {
                    top: Val::Px(10.),
                    bottom: Val::Px(35.),
                    ..default()
                },
                padding: UiRect::all(Val::Px(5.)),
                ..Default::default()
            },
            background_color: Color::BLACK.with_a(0.95).into(),
            ..Default::default()
        }).with_children(|parent| {
            parent.spawn(TextBundle::from_section(
                    cause.0.clone(),
                    c_text_style,
            ));
        });

        parent.spawn(TextBundle::from_section(
                 "\n\nGAME OVER", 
                 text_style
        ).with_background_color(Color::BLACK.with_a(0.5)));
    });   
}


fn wait_game_over(
    time: Res<Time>,
    mut delay: Local<Option<f32>>,
    mut c: Local<f32>,
    mut next_state: ResMut<NextState<AppState>>,
    mut game_state: ResMut<NextState<GameState>>,
    input: PlayerInput,
    mut organ_map: ResMut<Organ>,
    mut commands: Commands,
    query: Query<(Entity, &Pos, &Flavour),  Or<(With<PillPart>, With<Germ>)>>,
    mut event_writer: EventWriter<SpawnExplosionEvent>,
) {
    
    if delay.is_none() {
        *delay = Some(5.);
        *c = 0.1;
    }

    *c -= time.delta_seconds();
    if *c < 0. {
        let rng = &mut thread_rng();
        *c = rng.gen_range(0.05 .. 0.2);
        if let Some((e, p, f)) = query.iter().next() {
            commands.entity(e).despawn();
            organ_map.remove_entity(e);
            event_writer.send(SpawnExplosionEvent { pos: *p, color: f.color() });
        }
    }

    if let Some(ref mut t) = *delay {
        *t -= time.delta_seconds();
        if *t < 0. || input.just_pressed(PlayerCommand::Select) {
            *delay = None;
            next_state.set(AppState::TitleScreen);
            game_state.set(GameState::Inactive);
        }
    }
}