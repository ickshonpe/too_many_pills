use std::f32::consts::FRAC_PI_2;
use std::f32::consts::PI;
use std::hash::Hasher;
use bevy::core_pipeline::clear_color::ClearColorConfig;
use bevy::math::Affine3A;
use bevy::math::Mat3A;
use bevy::math::vec2;
use bevy::prelude::*;
use bevy::render::Extract;
use bevy::render::texture::DEFAULT_IMAGE_HANDLE;
use bevy::sprite::ExtractedSprite;
use bevy::sprite::ExtractedSprites;
use bevy::sprite::SpriteSystem;
use bevy::utils::HashMap;
use bevy::utils::HashSet;
use bevy::window::WindowResolution;
use rand::seq::SliceRandom;
use rand::thread_rng;

const BUG_SPRITE_PATHS: [&str; 3] = ["sprites/bug-a.png", "sprites/bug-b.png", "sprites/bug-c.png"];
const PILL_SPRITE_PATH: &str = "sprites/pill.png";
const FONT_PATH: &str = "fonts/slkscre.ttf";
const FONT_BOLD_PATH: &str = "fonts/slkscreb.ttf";

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

#[derive(Component)]
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
enum Flavour {
    Red=0,
    Blue=1,
    Yellow=2,
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
            horizontal_move_delay: 0.15,
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

#[derive(Component)]
#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
enum GameSet {
    Input,
    RotatePill,
    DropPill,
    MovePill,
    SpawnPill,
    Jar,
    SwapEnds,
    Wait,
}

struct Joined(Entity, Entity);

impl Joined {
    fn contains(&self, entity: Entity) -> bool {
        self.0 == entity || self.1 == entity
    }
}

impl PartialEq for Joined {
    fn eq(&self, other: &Self) -> bool {
        (self.0 == other.0 && self.1 == other.1) || (self.0 == other.1 && self.1 == other.0)
    }
}

impl Eq for Joined {}

impl std::hash::Hash for Joined {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::cmp::min(&self.0, &self.1).hash(state);
        std::cmp::max(&self.0, &self.1).hash(state);
    }
}

#[derive(Resource)]
struct Jar {
    entities: HashMap<Pos, Entity>,
    bounds: Grid,
    joined: HashSet<Joined>,
}

impl Jar {
    fn new(dim: Dim) -> Self {
        Self {
            entities: HashMap::default(),
            bounds: dim.grid(),
            joined: HashSet::default(),
        }
    }

    fn get(&self, pos: Pos) -> Option<Entity> {
        if self.bounds.contains(pos) {
            self.entities.get(&pos).copied()
        } else {
            None
        }
    }

    fn insert(&mut self, pos: Pos, entity: Entity) -> Option<Entity> {
        self.entities.insert(pos, entity)
    }

    fn insert_pill(&mut self, pill: [(Pos, Entity); 2]) -> Vec<Option<Entity>> {
        self.joined.insert(Joined(pill[0].1, pill[1].1));
        pill
            .map(|(pos, entity)| self.insert(pos, entity))
            .into()
    }

    fn remove(&mut self, pos: Pos) -> Option<Entity> {
        let entity = self.entities.remove(&pos);
        if let Some(entity) = entity {
            self.joined.retain(|joined| !joined.contains(entity));
        }   
        entity
    }

    fn get_joined(&self, entity: Entity) -> Option<Entity> {
        self.joined
            .iter()
            .find(|joined| joined.contains(entity))
            .map(|joined| {
                if joined.0 == entity {
                    joined.1
                } else {
                    joined.0
                }
            })
    }

    fn is_empty(&self, pos: Pos) -> bool {
        !self.entities.contains_key(&pos)
            && (self.bounds.contains(pos)
                || (self.bounds.x_range().contains(&pos.x) && self.bounds.max_y() <= pos.y))
    }

    fn down_all<'a>(&mut self, targets: impl Iterator<Item=&'a Pos>) {
        let mut ids = vec![];
        for pos in targets {
            ids.push((pos.down(), self.entities.remove(&pos).unwrap()));
        }

        for (pos, id) in ids {
            self.insert(pos, id);
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default, States)]
enum AppState {
    #[default]
    Game
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default, States)]
enum GameState {
    /// pill falling
    #[default]
    PillFalling,
    /// pill landed on surface
    PillLanded,
    Collapsing,
    Wait
}



fn main() {
    let mut app = App::new();
    app
        .add_plugins(
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
        .add_systems(Startup, (
            setup,
            spawn_score_display,
        ))
        .add_systems(Update, wait_system.run_if(in_state(GameState::Wait)))
        .add_systems(OnEnter(GameState::PillFalling), spawn_new_pill)
        .add_systems(OnEnter(GameState::Collapsing), collapse_pills)
        .add_systems(PreUpdate, 
            (
                update_pill_timers.before(GameSet::SpawnPill),
            ).run_if(in_state(GameState::PillFalling)
        ))
        .add_state::<GameState>()
        .add_systems(
            Update,
            (
                handle_input
                    .in_set(GameSet::Input)
                    .before(GameSet::RotatePill)
                    .before(GameSet::MovePill)
                    ,
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
              
            ).run_if(in_state(GameState::PillFalling)),
        )
        .add_systems(
            Update,
            (
                search_jar_for_flavour_lines.in_set(GameSet::Jar),
            ).run_if(in_state(GameState::PillLanded))
        );

    if let Ok(render_app) = app.get_sub_app_mut(bevy::render::RenderApp) {
            render_app
            .add_systems(
                ExtractSchedule,
                (
                    extract_jar_sprites
                    .after(SpriteSystem::ExtractSprites),
                ),
            );            
    }

    app.run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>, _rules: Res<GameRules>) {
    let mut camera_2d_bundle = Camera2dBundle::default();
    camera_2d_bundle.transform.scale *= Vec2::splat(0.5).extend(1.0);
    camera_2d_bundle.transform.translation +=
        (0.5 * CELL_SIZE * Vec2::from(JAR_SIZE) + 24. * Vec2::Y).extend(0.);
    camera_2d_bundle.camera_2d.clear_color = ClearColorConfig::Custom(Color::BLACK);
    commands.spawn(camera_2d_bundle);
    commands.insert_resource(JarGeometry {
        translation: Vec3::ZERO,
        cell_size: CELL_SIZE,
    });

    let mut jar = Jar::new(JAR_SIZE);
    spawn_jar(
        &mut commands,
        JAR_SIZE,
        Color::CYAN,
        &[Color::BLACK],
    );
    
    spawn_germs(
        &mut commands,
        30,
        &asset_server,
        &mut jar,
    );
    commands.insert_resource(jar);

}
#[derive(Component)]
struct ScoreText;

#[derive(Component)]
struct HighScoreText;

fn spawn_score_display(
    mut commands: Commands,
    asset_server: Res<AssetServer>
) {
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
    commands.spawn(NodeBundle {
        style: Style {
            flex_basis: Val::Percent(100.),
            justify_content: JustifyContent::SpaceBetween,
            ..Default::default()
        },
        ..Default::default()
    }).with_children(|parent| {
        parent.spawn(NodeBundle {
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
        }).with_children(|parent| {
            parent.spawn(TextBundle::from_section("score", text_style_1.clone()));
            parent.spawn(TextBundle::from_section("000000", text_style_2.clone())).insert(ScoreText);
        });

        parent.spawn(NodeBundle {
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
        }).with_children(|parent| {
            parent.spawn(TextBundle::from_section("hiscore", text_style_1.clone()));
            parent.spawn(TextBundle::from_section("000000", text_style_2.clone())).insert(HighScoreText);
        });

    });


}

fn spawn_jar(
    commands: &mut Commands,
    dim: Dim,
    front: Color,
    back: &[Color],
) {
    let inner = dim.grid();

    let outer = inner.expand(Dim::ONE);
    for pos in outer.iter() {
        let (color, depth) = if inner.contains(pos) {
            let color = back[(pos.x + pos.y + 2) as usize % back.len()];
            (color, Depth::JarBack)
        } else if pos.y < inner.max_y() {
            let c = 1.0 - 0.25 * ((dim.h as f32 + 2.) - pos.y as f32) / dim.h as f32;
            (Color::rgb(front.r() * c, front.g() * c, front.b() * c), Depth::JarWall)
        } else {
            continue;
        };
        commands.spawn(RectBundle::new(color, pos, depth));
    }
}

fn spawn_germs(
    commands: &mut Commands,
    count: usize,
    asset_server: &AssetServer,
    jar: &mut Jar,
) {
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
            .spawn((
                JarSpriteBundle::new(
                    asset_server.load(flavour.bug_sprite_path()),
                    cell,
                    flavour
                ).depth(Depth::Bug),                  
                Bug,
            ))
            .id();
        jar.insert(cell, bug_id);
    }
}

#[derive(Component)]
struct DropTimer(f32);

#[derive(Component)]
struct PillPart;

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
        commands.spawn((
            JarSpriteBundle::new(
                pill_texture.clone(),
                position,
                pill_bottom,
            )
            .depth(Depth::Pill)
            .orientation(Dir::Down)
            .color(pill_bottom.color()),
            PivotPill,
            ActivePill,
            DropTimer(rules.drop_delay),
            XMoveTimer(rules.horizontal_move_delay),
            PillPart,
        ));
        commands.spawn((
            JarSpriteBundle::new(
                pill_texture,
                position.up(),
                pill_top,
            )
            .depth(Depth::Pill)
            .orientation(Dir::Up)
            .color(pill_top.color()),
            ActivePill,
            PillPart,
        ));
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
        let multiplier = if pill_commands.drop { rules.fast_drop_multiplier } else { 1. };
        t.0 -= time.delta_seconds() * multiplier;
    }
    for mut t in horizontal_timer_query.iter_mut() {
        t.0 -= time.delta_seconds();
    } 
}

#[derive(Bundle)]
struct RectBundle {
    texture: Handle<Image>,
    pos: Pos,
    depth: Depth,
    orientation: Dir,
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
            orientation: Dir::Up,
            visibility: Visibility::Visible,
            computed_visibility: ComputedVisibility::default()
        }
    }
}


#[derive(Bundle)]
struct JarSpriteBundle {
    texture: Handle<Image>,
    pos: Pos,
    depth: Depth,
    orientation: Dir,
    tint: Tint,
    flavour: Flavour,
    visibility: Visibility,
    computed_visibility: ComputedVisibility,
}

impl JarSpriteBundle {
    fn new(texture: Handle<Image>, pos: Pos, flavour: Flavour) -> Self {
        JarSpriteBundle {
            texture,
            pos,
            depth: Depth::Top,
            orientation: Dir::Up,
            tint: Color::WHITE.into(),
            flavour,
            visibility: Visibility::Visible,
            computed_visibility: ComputedVisibility::default()
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



#[derive(Component, Default)]
#[derive(Copy, Clone)]
struct Tint(Color);

impl From<Color> for Tint {
    fn from(color: Color) -> Self {
        Tint(color)
    }
}

fn orient_pill(
    pill_commands: ResMut<PillCommands>,
    mut pivot_pill_query: Query<
        (&Pos, &mut Dir),
        (With<PivotPill>, With<ActivePill>),
    >,
    mut end_pill_query: Query<(&mut Pos, &mut Dir), (Without<PivotPill>, With<ActivePill>)>,
    jar: Res<Jar>,
) {
    let Some(dir) = pill_commands.dir else { return; };
    if dir.is_horizontal() {
        let (mut end_pos, mut end_dir) = end_pill_query.single_mut();
        let (pivot_pos, mut pivot_dir) = pivot_pill_query.single_mut();
        if dir == *end_dir { return }
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

fn swap_pill_ends(
    pill_commands: Res<PillCommands>,
    mut pivot_pill_query: Query<
        (&mut Flavour, &mut Tint, &mut Handle<Image>),
        (With<PivotPill>, With<ActivePill>),
    >,
    mut end_pill_query: Query<(&mut Flavour, &mut Tint, &mut Handle<Image>), (Without<PivotPill>, With<ActivePill>)>,
){
    let (mut ef, mut et, mut ei) = end_pill_query.single_mut();
    let (mut pf, mut pt, mut pi) = pivot_pill_query.single_mut();
    if let Some(dir) = pill_commands.dir {
        if !dir.is_horizontal() {
            let temp = *ef;
            *ef = *pf;
            *pf = temp;

            let temp = *et;
            *et = *pt;
            *pt = temp;

            let temp = ei.clone();
            *ei = pi.clone();
            *pi = temp;
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
    mut pivot_pill_query: Query<
        (&mut Pos, &mut XMoveTimer),
        (With<PivotPill>, With<ActivePill>),
    >,
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
            jar.insert_pill([(*pivot_pos, pivot), (*end_pos, end)]);
            commands.entity(pivot)
                .remove::<(
                    ActivePill,
                    PivotPill,
                    DropTimer,
                    XMoveTimer,
                )>();
            commands.entity(end)
                .remove::<ActivePill>();
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
    mut commands: Commands,
    pill_bugs_query: Query<&Flavour>,
    mut jar: ResMut<Jar>,
    rules: Res<GameRules>,
    mut next_state: ResMut<NextState<GameState>>,
) {
    let matches = find_flavour_lines(|pos| 
        jar.get(pos).map(|entity| pill_bugs_query.get(entity).cloned().unwrap()), jar.bounds, rules.matches);
    if !matches.is_empty() {
        for pos in matches {
            let entity = jar.remove(pos).unwrap();
            commands.entity(entity).despawn();
        }
    }
    next_state.set(GameState::Collapsing);
}

#[derive(Component)]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
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
struct JarGeometry {
    translation: Vec3,
    cell_size: Vec2,
}

#[derive(Component)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Depth {
    JarBack,
    JarWall,
    Bug,
    Pill,
    Top,
}

impl Depth {
    fn z(self) -> f32 {
        match self {
            Depth::JarBack => 0.,
            Depth::JarWall => 100.,
            Depth::Bug => 10.,
            Depth::Pill => 20.,
            Depth::Top => 500.,
        }
    }
}

fn extract_jar_sprites(
    mut extracted_sprites: ResMut<ExtractedSprites>,
    jar: Extract<Res<JarGeometry>>,
    sprite_query: Extract<
        Query<(
            Entity,
            &ComputedVisibility,
            &Pos,
            &Handle<Image>,
            &Dir,
            &Tint,
            &Depth,
        )>,
    >,
) {
    for (entity, vis, pos, image, orientation, &Tint(color), depth) in sprite_query.iter() {  
        if !vis.is_visible() { continue };

        extracted_sprites.sprites.push(ExtractedSprite {
            entity,
            color,
            transform: Affine3A {
                matrix3: Mat3A::from_rotation_z(orientation.angle()), 
                translation: (jar.translation + ((0.5 + Vec2::from(*pos)) * jar.cell_size).extend(depth.z())).into(),
            }.into(),
            rect: None,
            // Pass the custom size
            custom_size: Some(jar.cell_size),
            flip_x: false,
            flip_y: false,
            image_handle_id: image.id(),
            anchor: Vec2::ZERO,
        });
    }
}

fn find_droppable_pills(jar: &Jar, is_pill: impl Fn(Entity) -> bool) -> Vec<Entity> {
    let mut droppable_pills = Vec::new();

    for (&pos, &entity) in jar.entities.iter() {
        if is_pill(entity) && pos.y > 0 && jar.is_empty(pos.down()) {
            if let Some(joined_entity) = jar.get_joined(entity) {
                let joined_pos = jar.entities.iter().find(|(_, &e)| e == joined_entity).map(|(&p, _)| p);

                if let Some(joined_pos) = joined_pos {
                    if joined_pos.y > 0 && jar.is_empty(joined_pos.down()) {
                        droppable_pills.push(entity);
                        droppable_pills.push(joined_entity);
                    }
                } else {
                    droppable_pills.push(entity);
                }
            } else {
                droppable_pills.push(entity);
            }
        }
    }

    droppable_pills
}

#[derive(Resource)]
pub struct NextStateTimer<T>{
    remaining: f32, 
    next: T
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
    mut next_state: ResMut<NextState<GameState>>,
) {
    let droppable_pills = find_droppable_pills(&jar, |id| pill_query.contains(id));
    if !droppable_pills.is_empty() {
        jar.down_all(droppable_pills.iter().map(|entity| pill_query.get(*entity).unwrap()));
        for entity in droppable_pills {
            if let Ok(mut pos) = pill_query.get_mut(entity) {
                *pos = pos.down();
            }
        }
        commands.insert_resource(NextStateTimer{ remaining: 0.4, next: GameState::Collapsing });
        next_state.set(GameState::Wait);
    } else {
        commands.insert_resource(NextStateTimer{ remaining: 0.4, next: GameState::PillFalling });
        next_state.set(GameState::Wait);
    }
}