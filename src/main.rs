use std::backtrace;
use std::f32::consts::PI;

use bevy::math::vec2;
use bevy::prelude::*;

use bevy::utils::HashMap;
use bevy::window::WindowResolution;
use rand::seq::SliceRandom;
use rand::thread_rng;

const BUG_SPRITE_PATHS: [&str; 3] = ["sprites/bug-a.png", "sprites/bug-b.png", "sprites/bug-c.png"];
const PILL_SPRITE_PATH: &str = "sprites/pill.png";
const FONT: &str = "fonts/slkscre.ttf";
const FONT_BOLD: &str = "fonts/slkscreb.ttf";

const JAR_SIZE: Dim = dim(8, 16);
const CELL_SIZE: Vec2 = Vec2::new(16., 16.);

const BUG_DEPTH: f32 = 100.;
const PILL_DEPTH: f32 = 150.;

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

#[derive(Resource)]
struct GameRules {
    rotate_delay: f32,
    horizontal_move_delay: f32,
    drop_delay: f32,
}

impl Default for GameRules {
    fn default() -> Self {
        Self {
            horizontal_move_delay: 0.15,
            rotate_delay: 0.1,
            drop_delay: 0.5,
        }
    }
}

#[derive(Component, Copy, Clone, Debug, Default, Hash, Eq, PartialEq)]
pub struct Pos {
    x: i16,
    y: i16,
}

pub const fn pos(x: i16, y: i16) -> Pos {
    Pos { x, y }
}

impl Pos {
    pub const ZERO: Self = pos(0, 0);
    pub const fn translate(self, x: i16, y: i16) -> Self {
        pos(self.x + x, self.y + y)
    }
    pub const fn up(self) -> Self {
        self.translate(0, 1)
    }
    pub const fn down(self) -> Self {
        self.translate(0, -1)
    }
    pub const fn left(self) -> Self {
        self.translate(-1, 0)
    }
    pub const fn right(self) -> Self {
        self.translate(1, 0)
    }
}

#[derive(Copy, Clone, Debug, Default, Hash, Eq, PartialEq)]
pub struct Dim {
    pub w: i16,
    pub h: i16,
}
#[derive(Copy, Clone, Debug, Default, Hash, Eq, PartialEq)]
pub struct Grid {
    pub pos: Pos,
    pub dim: Dim,
}

pub const fn dim(w: i16, h: i16) -> Dim {
    Dim { w, h }
}
pub const fn grid(pos: Pos, dim: Dim) -> Grid {
    Grid { pos, dim }
}

impl Grid {
    pub fn x_range(self) -> std::ops::Range<i16> {
        self.pos.x..self.max_x()
    }
    pub fn y_range(self) -> std::ops::Range<i16> {
        self.pos.y..self.max_y()
    }
    pub fn iter(self) -> impl Iterator<Item = Pos> {
        self.x_range()
            .flat_map(move |x| self.y_range().map(move |y| pos(x, y)))
    }
    pub fn expand(self, dim: Dim) -> Self {
        grid(self.pos.translate(-dim.w, -dim.h), self.dim.expand(dim))
    }
    pub fn contains(self, pos: Pos) -> bool {
        self.x_range().contains(&pos.x) && self.y_range().contains(&pos.y)
    }

    pub const fn max_x(self) -> i16 {
        self.pos.x + self.dim.w
    }
    pub const fn max_y(self) -> i16 {
        self.pos.y + self.dim.h
    }

    pub fn cover_points(points: &[Pos]) -> Self {
        let mut min_x = i16::MAX;
        let mut min_y = i16::MAX;
        let mut max_x = i16::MIN;
        let mut max_y = i16::MIN;
        for pos in points {
            min_x = pos.x.min(min_x);
            min_y = pos.y.min(min_y);
            max_x = pos.x.max(max_x);
            max_y = pos.y.max(max_y);
        }
        Self {
            pos: pos(min_x, min_y),
            dim: dim(max_x - min_x + 1, max_y - min_y + 1),
        }
    }
}

impl Dim {
    pub const ONE: Self = Self::uniform(1);
    pub const fn uniform(d: i16) -> Self {
        dim(d, d)
    }
    pub fn grid(self) -> Grid {
        grid(Pos::ZERO, self)
    }
    pub fn expand(self, expansion: Self) -> Self {
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

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
enum GameSet {
    Input,
    RotatePill,
    DropPill,
    UpdateCoords,
    MovePill,
    SpawnPill,
}

#[derive(Resource)]
struct Jar {
    entities: HashMap<Pos, Entity>,
    bounds: Grid,
}

impl Jar {
    fn new(dim: Dim) -> Self {
        Self {
            entities: HashMap::default(),
            bounds: dim.grid(),
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

    fn remove(&mut self, pos: Pos) -> Option<Entity> {
        self.entities.remove(&pos)
    }

    fn clear(&mut self) {
        self.entities.clear()
    }

    fn is_empty(&self, pos: Pos) -> bool {
        !self.entities.contains_key(&pos)
            && (self.bounds.contains(pos)
                || (self.bounds.x_range().contains(&pos.x) && self.bounds.max_y() <= pos.y))
    }
}

fn main() {
    App::new()
        .add_plugins(
            DefaultPlugins
                .set(ImagePlugin::default_nearest())
                .set(WindowPlugin {
                    primary_window: Some(Window {
                        resolution: WindowResolution::new(600., 800.),
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
        .add_systems(PreUpdate, 
            (
                update_timers.before(GameSet::SpawnPill),
                spawn_new_pill.in_set(GameSet::SpawnPill)
            )
        )
        .add_systems(
            Update,
            (
                // spawn_new_pill.in_set(GameSet::SpawnPill)
                //     .before(GameSet::Input)
                //     .before(GameSet::MovePill)
                //     .before(GameSet::RotatePill),
                // apply_system_buffers.in_set(GameSet::SpawnPill),
                handle_input
                    .in_set(GameSet::Input)
                    .before(GameSet::RotatePill),
                rotate_pill
                    .in_set(GameSet::RotatePill)
                    .before(GameSet::UpdateCoords),
                move_pill
                    .in_set(GameSet::MovePill)
                    .after(GameSet::RotatePill)
                    .before(GameSet::DropPill),
                update_coords
                    .in_set(GameSet::UpdateCoords)
                    .after(GameSet::RotatePill),
                drop_pill
                    .in_set(GameSet::DropPill)
                    .after(GameSet::RotatePill)
                    .before(GameSet::UpdateCoords),
            ),
        )
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>, rules: Res<GameRules>) {
    let mut camera_2d_bundle = Camera2dBundle::default();
    camera_2d_bundle.transform.scale *= Vec2::splat(0.5).extend(1.0);
    camera_2d_bundle.transform.translation +=
        (0.5 * CELL_SIZE * Vec2::from(JAR_SIZE) + 32. * Vec2::Y).extend(0.);
    commands.spawn(camera_2d_bundle);
    let mut jar = Jar::new(JAR_SIZE);
    spawn_jar(
        &mut commands,
        JAR_SIZE,
        CELL_SIZE,
        Color::CYAN,
        &[Color::BLACK],
        0.,
    );
    let bug_textures = BUG_SPRITE_PATHS
        .iter()
        .map(|&path| asset_server.load(path))
        .collect::<Vec<_>>();
    spawn_germs(
        &mut commands,
        CELL_SIZE,
        BUG_DEPTH,
        30,
        &bug_textures,
        &mut jar,
    );
    // spawn_pill(
    //     &mut commands,
    //     PILL_PALETTE[0],
    //     PILL_PALETTE[1],
    //     PILL_DEPTH,
    //     pos(4, 20),
    //     asset_server.load(PILL_SPRITE_PATH),
    //     &rules,
    // );
    commands.insert_resource(jar);
}

pub struct ScoreText;
pub struct HighScoreText;

fn spawn_score_display(
    mut commands: Commands,
    asset_server: Res<AssetServer>
) {
    let font = asset_server.load("fonts/slkscre.ttf");
    let bold_font = asset_server.load("fonts/slkscreb.ttf");
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
                size: Size::new(Val::Px(200.), Val::Px(100.)),
                margin: UiRect::all(Val::Px(10.)),
                flex_direction: FlexDirection::Column,
                align_items: AlignItems::Center,
                justify_content: JustifyContent::Center,
                gap: Size::all(Val::Px(10.)),
                ..Default::default()
            },
            background_color: Color::BLACK.with_a(0.1).into(),
            ..Default::default()
        }).with_children(|parent| {
            parent.spawn(TextBundle::from_section("score", text_style_1.clone()));
            parent.spawn(TextBundle::from_section("000000", text_style_2.clone()));
        });

        parent.spawn(NodeBundle {
            style: Style {
                size: Size::new(Val::Px(200.), Val::Px(100.)),
                margin: UiRect::all(Val::Px(10.)),
                flex_direction: FlexDirection::Column,
                align_items: AlignItems::Center,
                justify_content: JustifyContent::Center,
                gap: Size::all(Val::Px(10.)),
                ..Default::default()
            },
            background_color: Color::BLACK.with_a(0.1).into(),
            ..Default::default()
        }).with_children(|parent| {
            parent.spawn(TextBundle::from_section("hiscore", text_style_1.clone()));
            parent.spawn(TextBundle::from_section("000000", text_style_2.clone()));
        });;

    });


}

fn spawn_jar(
    commands: &mut Commands,
    dim: Dim,
    cell_size: Vec2,
    front: Color,
    back: &[Color],
    depth: f32,
) {
    let inner = dim.grid();

    let outer = inner.expand(Dim::ONE);
    for pos in outer.iter() {
        let color = if inner.contains(pos) {
            let color = back[(pos.x + pos.y + 2) as usize % back.len()];
            color
        } else if pos.y < inner.max_y() {
            let c = 1.0 - 0.25 * ((dim.h as f32 + 2.) - pos.y as f32) / dim.h as f32;
            Color::rgb(front.r() * c, front.g() * c, front.b() * c)
        } else {
            continue;
        };
        commands.spawn((
            SpriteBundle {
                sprite: Sprite {
                    color,
                    custom_size: Some(cell_size),
                    ..Default::default()
                },
                transform: Transform::from_translation(depth * Vec3::Z),
                ..Default::default()
            },
            pos,
        ));
    }
}

fn spawn_germs(
    commands: &mut Commands,
    cell_size: Vec2,
    z: f32,
    count: usize,
    textures: &[Handle<Image>],
    jar: &mut Jar,
) {
    let mut rng = rand::thread_rng();
    let mut cells: Vec<Pos> = jar.bounds.iter().collect();
    cells.shuffle(&mut rng);
    let n = count.min(cells.len());
    for cell in cells.into_iter().take(n) {
        let bug_id = commands
            .spawn((
                SpriteBundle {
                    sprite: Sprite {
                        custom_size: Some(cell_size),
                        ..Default::default()
                    },
                    texture: textures.choose(&mut rng).unwrap().clone(),
                    transform: Transform::from_translation(z * Vec3::Z),
                    ..Default::default()
                },
                cell,
            ))
            .id();
        jar.insert(cell, bug_id);
    }
}

#[derive(Component)]
struct DropTimer(f32);

fn spawn_new_pill(
    active_pill_query: Query<Entity, With<ActivePill>>,
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    rules: Res<GameRules>,
) {
    let rng = &mut thread_rng();
    let position = pos(4, 20);
    let color_bottom = *PILL_PALETTE.choose(rng).unwrap();
    let color_top = *PILL_PALETTE.choose(rng).unwrap();
    let pill_texture = asset_server.load(PILL_SPRITE_PATH);
    if active_pill_query.is_empty() {
        println!("spawning new pill");
        commands.spawn((
            CellSprite::new(position)
                .color(color_bottom)
                .texture(pill_texture.clone())
                .z(PILL_DEPTH)
                .rotate(PI),
            PivotPill,
            ActivePill,
            DropTimer(rules.drop_delay),
            HorizontalTimer(rules.horizontal_move_delay),
            RotationTimer(rules.rotate_delay),
        ));
    
        commands.spawn((
            CellSprite::new(position.up())
                .color(color_top)
                .texture(pill_texture)
                .z(PILL_DEPTH),
            ActivePill,
        ));
        
        commands.add(|world: &mut World| { println!("processed commands"); });
    }
}

fn update_coords(mut coords_query: Query<(&Pos, &mut Transform)>) {
    for (&pos, mut transform) in coords_query.iter_mut() {
        let t = (0.5 + Vec2::from(pos)) * CELL_SIZE;
        transform.translation.x = t.x;
        transform.translation.y = t.y;
    }
}

#[derive(Component)]
struct RotationTimer(f32);

fn update_timers(
    time: Res<Time>,
    mut drop_timer_query: Query<&mut DropTimer>,
    mut horizontal_timer_query: Query<&mut HorizontalTimer>,
    mut rotation_timer_query: Query<&mut RotationTimer>,
) {
    for mut t in drop_timer_query.iter_mut() {
        t.0 -= time.delta_seconds();
    }
    for mut t in horizontal_timer_query.iter_mut() {
        t.0 -= time.delta_seconds();
    }
    for mut t in rotation_timer_query.iter_mut() {
        t.0 -= time.delta_seconds();
    }
}

#[derive(Bundle, Default)]
pub struct CellSprite {
    #[bundle]
    sprite_bundle: SpriteBundle,
    pos: Pos,
}

impl CellSprite {
    fn new(pos: Pos) -> Self {
        Self {
            sprite_bundle: SpriteBundle {
                sprite: Sprite {
                    custom_size: Some(CELL_SIZE),
                    ..Default::default()
                },
                ..Default::default()
            },
            pos,
        }
    }

    fn flip_x(mut self) -> Self {
        self.sprite_bundle.sprite.flip_x = !self.sprite_bundle.sprite.flip_x;
        self
    }

    fn flip_y(mut self) -> Self {
        self.sprite_bundle.sprite.flip_y = !self.sprite_bundle.sprite.flip_y;
        self
    }

    fn color(mut self, color: Color) -> Self {
        self.sprite_bundle.sprite.color = color;
        self
    }

    fn texture(mut self, texture: Handle<Image>) -> Self {
        self.sprite_bundle.texture = texture;
        self
    }

    fn z(mut self, z: f32) -> Self {
        self.sprite_bundle.transform.translation.z = z;
        self
    }

    fn rotate(mut self, radians: f32) -> Self {
        self.sprite_bundle
            .transform
            .rotate(Quat::from_rotation_z(radians));
        self
    }
}

fn rotate_pill(
    pill_commands: Res<PillCommands>,
    mut pivot_pill_query: Query<
        (&Pos, &mut RotationTimer, &mut Transform),
        (With<PivotPill>, With<ActivePill>),
    >,
    mut end_pill_query: Query<(&mut Pos, &mut Transform), (Without<PivotPill>, With<ActivePill>)>,
    jar: Res<Jar>,
    rules: Res<GameRules>,
) {
    if pill_commands.rotate == 0 {
        return;
    }
    let (pivot_pos, mut rotation_timer, mut pivot_transform) = pivot_pill_query.single_mut();
    if 0. < rotation_timer.0 {
        return;
    }
    let (mut end_pos, mut end_transform) = end_pill_query.single_mut();
    let dir_index: i32 = if end_pos.x < pivot_pos.x {
        3
    } else if end_pos.y < pivot_pos.y {
        2
    } else if pivot_pos.x < end_pos.x {
        1
    } else {
        0
    };
    let dir_index = if pill_commands.rotate < 0 {
        (dir_index - 1).rem_euclid(4)
    } else {
        (dir_index + 1).rem_euclid(4)
    };
    let (new_pos_fn, r, s) = [
        (
            Pos::up as fn(Pos) -> Pos,
            Quat::from_rotation_z(PI),
            Quat::IDENTITY,
        ),
        (
            Pos::right,
            Quat::from_rotation_z(PI / 2.),
            Quat::from_rotation_z(-PI / 2.),
        ),
        (Pos::down, Quat::IDENTITY, Quat::from_rotation_z(PI)),
        (
            Pos::left,
            Quat::from_rotation_z(-PI / 2.),
            Quat::from_rotation_z(PI / 2.),
        ),
    ][dir_index as usize];
    let new_pos = new_pos_fn(*pivot_pos);
    if jar.is_empty(new_pos) && jar.is_empty(*pivot_pos) {
        rotation_timer.0 = rules.rotate_delay;
        pivot_transform.rotation = r;
        *end_pos = new_pos;
        end_transform.rotation = s;
    }
}


#[derive(Resource, Default)]
pub struct PillCommands {
    rotate: i16,
    x_move: i16,
    drop: bool,
}

fn handle_input(keyboard: Res<Input<KeyCode>>, mut pill_command: ResMut<PillCommands>) {
    pill_command.rotate = 0;
    pill_command.x_move = 0;
    if keyboard.pressed(KeyCode::Z) {
        pill_command.rotate -= 1;
    }
    if keyboard.pressed(KeyCode::X) {
        pill_command.rotate += 1;
    }

    if keyboard.pressed(KeyCode::Left) {
        pill_command.x_move -= 1;
    }
    if keyboard.pressed(KeyCode::Right) {
        pill_command.x_move += 1;
    }
}

#[derive(Component)]
pub struct HorizontalTimer(f32);

fn move_pill(
    game_config: Res<GameRules>,
    mut pivot_pill_query: Query<
        (&mut Pos, &mut HorizontalTimer),
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
    pill_commands: ResMut<PillCommands>,
    mut pivot_query: Query<(Entity, &mut Pos, &mut DropTimer), (With<ActivePill>, With<PivotPill>)>,
    mut end_query: Query<(Entity, &mut Pos), (With<ActivePill>, Without<PivotPill>)>,
    rules: Res<GameRules>,
    mut jar: ResMut<Jar>,
    mut commands: Commands,
) {
    let (pivot, mut pivot_pos, mut drop_timer) = pivot_query.single_mut();
    let (end, mut end_pos) = end_query.single_mut();
    if pill_commands.drop || drop_timer.0 < 0. {
        drop_timer.0 = rules.drop_delay;
        let next_pivot_pos = pivot_pos.down();
        let next_end_pos = end_pos.down();
        if jar.is_empty(next_pivot_pos) && jar.is_empty(next_end_pos) {
            *pivot_pos = next_pivot_pos;
            *end_pos = next_end_pos;
        } else {
            jar.insert(*pivot_pos, pivot);
            jar.insert(*end_pos, end);
            commands.entity(pivot)
                .remove::<(
                    ActivePill,
                    PivotPill,
                    DropTimer,
                    HorizontalTimer,
                    RotationTimer,
                )>();
            commands.entity(end)
                .remove::<ActivePill>();
        }
    }
}

