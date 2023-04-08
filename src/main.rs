#![allow(clippy::type_complexity)]
#![allow(clippy::too_many_arguments)]

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
use bevy::utils::HashSet;
use bevy::window::WindowResolution;
use bimap::BiMap;
use rand::seq::SliceRandom;
use rand::thread_rng;
use rand::Rng;
use std::f32::consts::FRAC_PI_2;
use std::f32::consts::PI;

const BUG_SPRITE_PATHS: [&str; 3] = [
    "sprites/bug-a.png",
    "sprites/bug-b.png",
    "sprites/bug-c.png",
];
const PILL_SPRITE_PATH: &str = "sprites/pill.png";
const PILL_BROKE_SPRITE_PATH: &str = "sprites/pill-broke.png";
const FONT_PATH: &str = "fonts/slkscre.ttf";
const FONT_BOLD_PATH: &str = "fonts/slkscreb.ttf";

const WALL_TILE_SET_DIR: &str = "sprites/wall_set_b";
const BACK_TILE_SET_DIR: &str = "sprites/back_set";

const JAR_SIZE: Dim = dim(9, 18);
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
struct Bug;

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
    fn iter(self) -> impl Iterator<Item = Pos> {
        self.x_range()
            .flat_map(move |x| self.y_range().map(move |y| pos(x, y)))
    }
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

#[derive(Component)]
struct PivotPill;
#[derive(Component)]
struct ActivePill;

#[derive(Component, Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
enum GameSet {
    Input,
    RotatePill,
    DropPill,
    MovePill,
    SpawnPill,
    Jar,
    SwapEnds,
}

#[derive(Resource)]
struct Jar {
    cells: BiMap<Pos, Entity>,
    bounds: Grid,
}

impl Jar {
    fn new(dim: Dim) -> Self {
        Self {
            cells: BiMap::default(),
            bounds: dim.grid(),
        }
    }

    fn get(&self, pos: Pos) -> Option<Entity> {
        if self.bounds.contains(pos) {
            self.cells.get_by_left(&pos).copied()
        } else {
            None
        }
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
            && (self.bounds.contains(pos)
                || (self.bounds.x_range().contains(&pos.x) && self.bounds.max_y() <= pos.y))
    }

    fn down_all<'a>(&mut self, targets: impl Iterator<Item = &'a Pos>) {
        let mut ids = vec![];
        for pos in targets {
            ids.push((pos.down(), self.remove(*pos).unwrap()));
        }

        for (pos, id) in ids {
            self.insert(pos, id);
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default, States)]
enum AppState {
    #[default]
    Game,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default, States)]
enum GameState {
    /// pill falling
    #[default]
    PillFalling,
    /// pill landed on surface
    PillLanded,
    Collapsing,
    Wait,
}

fn main() {
    let mut app = App::new();
    app.add_plugins(
        DefaultPlugins
            .set(ImagePlugin::default_nearest())
            .set(WindowPlugin {
                primary_window: Some(Window {
                    resolution: WindowResolution::new(400., 800.),
                    title: "pills".to_string(),
                    resizable: false,
                    ..Default::default()
                }),
                ..Default::default()
            }),
    )
    .init_resource::<PillCommands>()
    .init_resource::<GameRules>()
    .init_resource::<ShadowOffset>()
    .add_systems(Startup, (setup, spawn_score_display))
    .add_systems(Update, wait_system.run_if(in_state(GameState::Wait)))
    .add_systems(OnEnter(GameState::PillFalling), spawn_new_pill)
    .add_systems(OnEnter(GameState::Collapsing), collapse_pills)
    .add_systems(
        PreUpdate,
        (update_pill_timers.before(GameSet::SpawnPill),).run_if(in_state(GameState::PillFalling)),
    )
    .add_state::<GameState>()
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
            .run_if(in_state(GameState::PillFalling)),
    )
    .add_systems(
        Update,
        (search_jar_for_flavour_lines.in_set(GameSet::Jar),)
            .run_if(in_state(GameState::PillLanded)),
    );

    if let Ok(render_app) = app.get_sub_app_mut(bevy::render::RenderApp) {
        render_app.add_systems(
            ExtractSchedule,
            (extract_organ_sprites.after(SpriteSystem::ExtractSprites),),
        );
    }

    app.run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>, _rules: Res<GameRules>) {
    let mut camera_2d_bundle = Camera2dBundle::default();
    camera_2d_bundle.transform.scale *= Vec2::splat(0.5).extend(1.0);
    camera_2d_bundle.transform.translation +=
        (0.5 * CELL_SIZE * Vec2::from(JAR_SIZE) + 24. * Vec2::Y).extend(0.);
    camera_2d_bundle.camera_2d.clear_color = ClearColorConfig::Custom(Color::MAROON);
    commands.spawn(camera_2d_bundle);
    commands.insert_resource(OrganGeometry {
        translation: Vec3::ZERO,
        cell_size: CELL_SIZE,
    });

    let mut jar = Jar::new(JAR_SIZE);
    let wall_textures = load_typed_folder(&asset_server, WALL_TILE_SET_DIR).unwrap();
    let back_textures = load_typed_folder(&asset_server, BACK_TILE_SET_DIR).unwrap();
    spawn_organ(&mut commands, &wall_textures, &back_textures, JAR_SIZE);

    spawn_germs(&mut commands, 10, &mut jar, &asset_server);
    commands.insert_resource(jar);
}
#[derive(Component)]
struct ScoreText;

#[derive(Component)]
struct HighScoreText;

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
                    background_color: Color::GRAY.with_a(0.1).into(),
                    ..Default::default()
                })
                .with_children(|parent| {
                    parent.spawn(TextBundle::from_section("score", text_style_1.clone()));
                    parent
                        .spawn(TextBundle::from_section("000000", text_style_2.clone()))
                        .insert(ScoreText);
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
                    background_color: Color::GRAY.with_a(0.1).into(),
                    ..Default::default()
                })
                .with_children(|parent| {
                    parent.spawn(TextBundle::from_section("hiscore", text_style_1.clone()));
                    parent
                        .spawn(TextBundle::from_section("000000", text_style_2.clone()))
                        .insert(HighScoreText);
                });
        });
}

fn gray(value: f32) -> Color {
    Color::rgb(value, value, value)
}

fn spawn_organ(
    commands: &mut Commands,
    wall_textures: &[Handle<Image>],
    back_textures: &[Handle<Image>],
    dim: Dim,
) {
    let rng = &mut thread_rng();
    let inner = dim.grid();
    let outer = inner.expand(Dim::ONE);
    for pos in outer.iter() {
        if inner.contains(pos) {
            commands.spawn(TileBundle {
                tint: Tint(gray(0.75)),
                ..TileBundle::random(rng, back_textures, pos, Depth::OrganBack)
            });
        } else if pos.y < inner.max_y() {
            commands
                .spawn(TileBundle::random(rng, wall_textures, pos, Depth::OrganWall).add_shadow());
        } else {
            continue;
        };
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

fn spawn_germs(commands: &mut Commands, count: usize, jar: &mut Jar, asset_server: &AssetServer) {
    let mut rng = rand::thread_rng();
    let bounds = Grid {
        dim: Dim {
            h: jar.bounds.dim.h - 3,
            ..jar.bounds.dim
        },
        ..jar.bounds
    };
    let mut cells: Vec<Pos> = bounds.iter().collect();
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
                    Bug,
                )
                    .add_shadow(),
            )
            .id();
        jar.insert(cell, bug_id);
    }
}

#[derive(Component)]
struct DropTimer(f32);

#[derive(Component)]
struct PillPart;

#[derive(Component)]
struct Joined(Entity);

fn spawn_new_pill(
    active_pill_query: Query<Entity, With<ActivePill>>,
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    rules: Res<GameRules>,
) {
    let rng = &mut thread_rng();
    let position = pos(4, 20);
    let pill_bottom = *Flavour::ALL.choose(rng).unwrap();
    let pill_top = *Flavour::ALL.choose(rng).unwrap();
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
struct TileBundle {
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

impl TileBundle {
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

impl TileBundle {
    fn new(texture: Handle<Image>, pos: Pos) -> Self {
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
    jar: Res<Jar>,
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
        if jar.is_empty(new_end) {
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

    if keyboard.pressed(KeyCode::Left) {
        pill_command.x_move -= 1;
    }
    if keyboard.pressed(KeyCode::Right) {
        pill_command.x_move += 1;
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
    jar: Res<Jar>,
) {
    let (mut pivot_pos, mut delay) = pivot_pill_query.single_mut();
    let mut end_pos = end_pill_query.single_mut();

    if delay.0 < 0. && pill_commands.x_move != 0 {
        let new_pivot_pos = pivot_pos.translate(pill_commands.x_move, 0);
        let new_end_pos = end_pos.translate(pill_commands.x_move, 0);
        if [new_pivot_pos, new_end_pos]
            .iter()
            .all(|&pos| jar.is_empty(pos))
        {
            *pivot_pos = new_pivot_pos;
            *end_pos = new_end_pos;
            delay.0 = game_config.horizontal_move_delay;
        }
    }
}

fn drop_pill(
    mut pivot_query: Query<(Entity, &mut Pos, &mut DropTimer), (With<ActivePill>, With<PivotPill>)>,
    mut end_query: Query<(Entity, &mut Pos), (With<ActivePill>, Without<PivotPill>)>,
    rules: Res<GameRules>,
    mut jar: ResMut<Jar>,
    mut commands: Commands,
    mut next_game_state: ResMut<NextState<GameState>>,
) {
    let (pivot, mut pivot_pos, mut drop_timer) = pivot_query.single_mut();
    let (end, mut end_pos) = end_query.single_mut();
    if drop_timer.0 < 0. {
        drop_timer.0 = rules.drop_delay;
        let next_pivot_pos = pivot_pos.down();
        let next_end_pos = end_pos.down();
        if jar.is_empty(next_pivot_pos) && jar.is_empty(next_end_pos) {
            *pivot_pos = next_pivot_pos;
            *end_pos = next_end_pos;
        } else {
            jar.insert_joined([(*pivot_pos, pivot), (*end_pos, end)]);
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
    pills: impl Fn(Pos) -> Option<Flavour>,
    bounds: Grid,
    min_flavour_line_len: i16,
) -> HashSet<Pos> {
    let mut out = HashSet::default();

    for x in bounds.x_range() {
        for y in bounds.y_range() {
            let p = pills(pos(x, y));
            if p.is_none() {
                continue;
            }
            let p = p.unwrap();

            // Check vertical
            if y + min_flavour_line_len <= bounds.max_y() {
                let all_match = (y + 1..y + min_flavour_line_len)
                    .map(|y| pos(x, y))
                    .all(|q| pills(q) == Some(p));

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
                    .all(|q| pills(q) == Some(p));

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

fn search_jar_for_flavour_lines(
    asset_server: Res<AssetServer>,
    mut commands: Commands,
    pill_bugs_query: Query<&Flavour>,
    mut jar: ResMut<Jar>,
    rules: Res<GameRules>,
    mut next_state: ResMut<NextState<GameState>>,
    mut joined_query: Query<&Joined>,
    mut image_query: Query<&mut Handle<Image>>,
) {
    let matches = find_flavour_lines(
        |pos| {
            jar.get(pos)
                .map(|entity| pill_bugs_query.get(entity).cloned().unwrap())
        },
        jar.bounds,
        rules.matches,
    );
    let mut removed = HashSet::default();
    if !matches.is_empty() {
        for pos in matches {
            let entity = jar.remove(pos).unwrap();
            if let Ok(join) = joined_query.get_mut(entity) {
                if !removed.contains(&join.0) {
                    let mut image = image_query.get_mut(join.0).unwrap();
                    *image = asset_server.load(PILL_BROKE_SPRITE_PATH);
                    commands.entity(join.0).remove::<Joined>();
                }
            }
            commands.entity(entity).despawn();
            removed.insert(entity);
        }
        commands.insert_resource(NextStateTimer {
            remaining: 0.4,
            next: GameState::Collapsing,
        });
        next_state.set(GameState::Wait);
    } else {
        commands.insert_resource(NextStateTimer {
            remaining: 0.4,
            next: GameState::PillFalling,
        });
        next_state.set(GameState::Wait);
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
    OrganWall,
    Bug,
    Pill,
    Top,
}

impl Depth {
    fn z(self) -> f32 {
        match self {
            Depth::OrganBack => 0.,
            Depth::Shadow => 1.,
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

fn extract_organ_sprites(
    mut extracted_sprites: ResMut<ExtractedSprites>,
    organ_geometry: Extract<Res<OrganGeometry>>,
    shadow_offset: Extract<Res<ShadowOffset>>,
    sprite_query: Extract<
        Query<(
            Entity,
            &ComputedVisibility,
            &Pos,
            &Handle<Image>,
            &Dir,
            &Tint,
            &Depth,
            Option<&Flip>,
            Option<&Shadow>,
        )>,
    >,
) {
    for (entity, vis, pos, image, orientation, &Tint(color), depth, flip, shadow) in
        sprite_query.iter()
    {
        if !vis.is_visible() {
            continue;
        };

        let flip = flip.copied().unwrap_or_default();
        let translation = organ_geometry.map_pos(*pos);

        if shadow.is_some() {
            extracted_sprites.sprites.push(ExtractedSprite {
                entity,
                color: Color::BLACK.with_a(0.5),
                transform: Affine3A {
                    matrix3: Mat3A::from_rotation_z(orientation.angle()),
                    translation: (translation + shadow_offset.0 + Depth::Shadow.z() * Vec3::Z)
                        .into(),
                }
                .into(),
                rect: None,
                custom_size: Some(organ_geometry.cell_size),
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
                matrix3: Mat3A::from_rotation_z(orientation.angle()),
                translation: (translation + depth.z() * Vec3::Z).into(),
            }
            .into(),
            rect: None,
            custom_size: Some(organ_geometry.cell_size),
            flip_x: flip.x,
            flip_y: flip.y,
            image_handle_id: image.id(),
            anchor: Vec2::ZERO,
        });
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
    next.remaining -= time.elapsed_seconds();
    if next.remaining < 0. {
        next_state.set(next.next);
    }
}

fn collapse_pills(
    mut commands: Commands,
    mut jar: ResMut<Jar>,
    mut pill_query: Query<&mut Pos, With<PillPart>>,
    join_query: Query<&Joined>,
    mut next_state: ResMut<NextState<GameState>>,
) {
    let mut drop = false;
    for row in jar.bounds.y_range() {
        let spaces: Vec<(Pos, Entity)> = jar
            .bounds
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
            remaining: 0.4,
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

struct SpawnExplosionMessage {
    pos: Pos,
    color: Color,
}

fn spawn_explosions(
    mut explosion_events: EventReader<SpawnExplosionMessage>,
    mut commands: Commands,
    organ_geometry: Res<OrganGeometry>,
) {
    let rng = &mut thread_rng();
}

#[derive(Component)]
struct Wall;

#[derive(SystemParam)]
struct JarManager<'w, 's> {
    jar: ResMut<'w, Jar>,
    pill_query: Query<'w, 's, &'static PillPart>,
    pos_query: Query<'w, 's, &'static mut Pos>,
}
