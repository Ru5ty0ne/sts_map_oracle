#![feature(unchecked_math)]
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::path::Path;
use RoomType::*;

#[derive(PartialEq, Eq, Hash)]
struct Point {
    x: usize,
    y: usize,
}

impl Point {
    fn get_parents<'a>(&self, map: &'a Map) -> &'a Vec<Point> {
        &map[self.y][self.x].parents
    }

    fn get_node<'a>(&self, map: &'a Map) -> &'a MapRoomNode {
        &map[self.y][self.x]
    }
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Debug, Clone)]
struct MapEdge {
    src_x: i32,
    src_y: i32,
    dst_x: i32,
    dst_y: i32,
}

impl MapEdge {
    fn new(src_x: i32, src_y: i32, dst_x: i32, dst_y: i32) -> Self {
        Self {
            src_x,
            src_y,
            dst_x,
            dst_y,
        }
    }
}

impl PartialOrd for MapEdge {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for MapEdge {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.dst_x.cmp(&other.dst_x) {
            Ordering::Equal => self.dst_y.cmp(&other.dst_y),
            res => res,
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Debug, Clone, Hash, Copy)]
enum RoomType {
    EventRoom,
    MonsterRoom,
    MonsterRoomElite,
    RestRoom,
    ShopRoom,
    TreasureRoom,
}

struct MapRoomNode {
    x: i32,
    y: i32,
    edges: BTreeSet<MapEdge>,
    parents: Vec<Point>,
    class: Option<RoomType>,
}

impl PartialEq for MapRoomNode {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y
    }
}
impl Eq for MapRoomNode {}

impl MapRoomNode {
    fn new(x: i32, y: i32) -> Self {
        Self {
            x,
            y,
            edges: BTreeSet::new(),
            parents: vec![],
            class: None,
        }
    }
}

type Map = Vec<Vec<MapRoomNode>>;

#[derive(Debug)]
struct Random {
    seed0: u64,
    seed1: u64,
}

impl Random {
    fn new(seed: i64) -> Self {
        let mut seed = seed as u64;
        if seed == 0 {
            seed = i64::MIN as u64;
        }
        let seed0 = murmur_hash3(seed);
        let seed1 = murmur_hash3(seed0);
        Self { seed0, seed1 }
    }

    fn next_u64(&mut self) -> u64 {
        let mut s1 = self.seed0;
        let s0 = self.seed1;
        self.seed0 = s0;
        s1 ^= s1 << 23;
        self.seed1 = s1 ^ s0 ^ s1 >> 17 ^ s0 >> 26;
        unsafe { s0.unchecked_add(self.seed1) }
    }

    fn next_shuffle(&mut self, n: u64) -> usize {
        self.next_u64_capped(n) as usize
    }

    fn next_u64_capped(&mut self, n: u64) -> u64 {
        loop {
            let bits = self.next_u64() >> 1;
            let value = bits % n;
            if bits + n >= value + 1 {
                return value;
            }
        }
    }

    fn next_i32(&mut self, n: u64) -> i32 {
        self.next_u64_capped(n + 1) as i32
    }

    fn rand_range(&mut self, min: i32, max: i32) -> i32 {
        min + self.next_i32((max - min) as u64)
    }
}

fn murmur_hash3(x: u64) -> u64 {
    let mut x = x;
    x ^= x >> 33;
    unsafe {
        x = x.unchecked_mul(0xff51afd7ed558ccd);
        x ^= x >> 33;
        x = x.unchecked_mul(0xc4ceb9fe1a85ec53);
    }
    x ^= x >> 33;
    x
}

//Trim edges with common destination on first floor
fn filter_redutant_edges(map: Map) -> Map {
    let mut map = map;
    let mut existing_edges: HashSet<Point> = HashSet::new();
    let mut delete_list: Vec<(usize, MapEdge)> = vec![];
    for (i, node) in map[0].iter().enumerate() {
        for edge in node.edges.iter() {
            for prev in existing_edges.iter() {
                if prev.x == edge.dst_x as usize && prev.y == edge.dst_y as usize {
                    delete_list.push((i, edge.clone()));
                }
            }
            existing_edges.insert(Point {
                x: edge.dst_x as usize,
                y: edge.dst_y as usize,
            });
        }
    }
    for del in delete_list.iter() {
        map[0][del.0].edges.remove(&del.1);
    }
    map
}

fn generate_dungeon(height: i32, width: i32, path_density: i32, rng: &mut Random) -> Map {
    let mut map: Map = create_nodes(height, width);
    map = create_paths(map, path_density, rng);
    filter_redutant_edges(map)
}

fn create_paths(nodes: Map, path_density: i32, rng: &mut Random) -> Map {
    let mut nodes = nodes;
    assert!(!nodes.is_empty());
    assert!(!nodes[0].is_empty());
    let row_size = (nodes[0].len() - 1) as i32;
    let mut first_starting_node = -1i32;
    for i in 0..path_density {
        let mut starting_node = rng.rand_range(0, row_size);
        if i == 0 {
            first_starting_node = starting_node;
        }
        while starting_node == first_starting_node && i == 1 {
            starting_node = rng.rand_range(0, row_size);
        }
        let tmp_edge = MapEdge::new(starting_node, -1, starting_node, 0);
        nodes = _create_paths(nodes, &tmp_edge, rng);
    }
    nodes
}

fn _create_paths(nodes: Map, edge: &MapEdge, rng: &mut Random) -> Map {
    let mut nodes = nodes;
    let min;
    let max;
    let current_node: &MapRoomNode = &nodes[edge.dst_y as usize][edge.dst_x as usize];
    if edge.dst_y + 1 >= nodes.len() as i32 {
        return nodes;
    }
    let row_width = nodes[edge.dst_y as usize].len();
    let row_end_node = row_width - 1;
    if edge.dst_x == 0 {
        min = 0;
        max = 1;
    } else if edge.dst_x == row_end_node as i32 {
        min = -1;
        max = 0;
    } else {
        min = -1;
        max = 1;
    }
    let mut new_edge_x = edge.dst_x + rng.rand_range(min, max);
    let new_edge_y = edge.dst_y + 1;
    let target_node_candidate: &MapRoomNode = &nodes[new_edge_y as usize][new_edge_x as usize];
    let mut target_coord_candidate = Point {
        x: new_edge_x as usize,
        y: new_edge_y as usize,
    };
    let min_ancestor_gap = 3i32;
    let max_ancestor_gap = 5i32;
    let current_node_coord = Point {
        y: current_node.y as usize,
        x: current_node.x as usize,
    };

    for parent in target_node_candidate.parents.iter() {
        if &current_node_coord != parent {
            let ancestor: Option<&Point> =
                get_common_ancestor(&nodes, parent, &current_node_coord, max_ancestor_gap);
            if let Some(ancestor) = ancestor {
                let ancestor_gap = new_edge_y - ancestor.y as i32;
                if ancestor_gap < min_ancestor_gap {
                    match target_coord_candidate.x.cmp(&(current_node.x as usize)) {
                        Ordering::Greater => {
                            new_edge_x = edge.dst_x + rng.rand_range(-1, 0);
                            if new_edge_x < 0 {
                                new_edge_x = edge.dst_x;
                            }
                        }
                        Ordering::Equal => {
                            new_edge_x = edge.dst_x + rng.rand_range(-1, 1);
                            if new_edge_x > row_end_node as i32 {
                                new_edge_x = edge.dst_x - 1;
                            } else if new_edge_x < 0 {
                                new_edge_x = edge.dst_x + 1;
                            }
                        }
                        Ordering::Less => {
                            new_edge_x = edge.dst_x + rng.rand_range(0, 1);
                            if new_edge_x > row_end_node as i32 {
                                new_edge_x = edge.dst_x
                            }
                        }
                    }

                    target_coord_candidate = Point {
                        x: new_edge_x as usize,
                        y: new_edge_y as usize,
                    };
                    continue;
                }
            }
        }
    }
    //eliminating edge crosses
    if edge.dst_x != 0 {
        let left_node = &nodes[edge.dst_y as usize][(edge.dst_x - 1) as usize];
        let right_edge_of_left_node = left_node.edges.iter().last();
        if let Some(right_edge_of_left_node) = right_edge_of_left_node {
            if right_edge_of_left_node.dst_x > new_edge_x {
                new_edge_x = right_edge_of_left_node.dst_x;
            }
        }
    }
    if edge.dst_x < row_end_node as i32 {
        let right_node = &nodes[edge.dst_y as usize][edge.dst_x as usize + 1];
        let left_edge_of_right_node = right_node.edges.iter().next();
        if let Some(left_edge_of_right_node) = left_edge_of_right_node {
            if left_edge_of_right_node.dst_x < new_edge_x {
                new_edge_x = left_edge_of_right_node.dst_x;
            }
        }
    }

    let current_node = &mut nodes[edge.dst_y as usize][edge.dst_x as usize];
    let new_edge = MapEdge::new(edge.dst_x, edge.dst_y, new_edge_x, new_edge_y);
    let copy_edge = new_edge.clone();
    current_node.edges.insert(new_edge);

    let target_node_candidate = &mut nodes[new_edge_y as usize][new_edge_x as usize];
    target_node_candidate.parents.push(Point {
        x: edge.dst_x as usize,
        y: edge.dst_y as usize,
    });
    _create_paths(nodes, &copy_edge, rng)
}

fn get_common_ancestor<'a>(
    map: &'a Map,
    node1: &'a Point,
    node2: &'a Point,
    max_depth: i32,
) -> Option<&'a Point> {
    assert!(node1.y == node2.y);
    assert!(node1.x != node2.x);
    let mut l_node: &Point;
    let mut r_node: &Point;
    //Bug from game's original codebase. It definitely should be "node1.x < node2.x". Retained for backwards compatibility
    if node1.x < node2.y {
        l_node = &node1;
        r_node = &node2;
    } else {
        l_node = &node2;
        r_node = &node1;
    }
    let mut current_y: i32 = node1.y as i32;
    while current_y >= 0 && current_y as i32 >= node1.y as i32 - max_depth {
        if l_node.get_parents(map).is_empty() || r_node.get_parents(map).is_empty() {
            return None;
        }
        l_node = get_nodes_with_max_x(l_node.get_parents(map));
        r_node = get_nodes_with_min_x(r_node.get_parents(map));
        if l_node == r_node {
            return Some(l_node);
        }
        current_y -= 1;
    }
    None
}

fn get_nodes_with_max_x(points: &[Point]) -> &Point {
    assert!(!points.is_empty());
    let mut max = &points[0];
    for point in points.iter() {
        if point.x > max.x {
            max = point;
        }
    }
    max
}

fn get_nodes_with_min_x(points: &[Point]) -> &Point {
    assert!(!points.is_empty());
    let mut min = &points[0];
    for point in points.iter() {
        if point.x < min.x {
            min = point;
        }
    }
    min
}

fn create_nodes(height: i32, width: i32) -> Map {
    let mut nodes: Map = vec![];
    for y in 0..height {
        let mut row = vec![];
        for x in 0..width {
            row.push(MapRoomNode::new(x, y));
        }
        nodes.push(row);
    }
    nodes
}

fn get_room_symbol(t: &Option<RoomType>) -> &str {
    match t {
        None => "*",
        Some(x) => match x {
            RestRoom => "R",
            ShopRoom => "$",
            MonsterRoom => "M",
            EventRoom => "?",
            MonsterRoomElite => "E",
            TreasureRoom => "T",
        },
    }
}

fn print_map(nodes: &Map) {
    let mut s = String::new();
    let mut row_num = nodes.len() - 1;
    let left_padding_size = 5;
    loop {
        s.push_str(&format!("\n {}", &padding_genrator(left_padding_size)));
        for node in nodes[row_num].iter() {
            let (mut right, mut mid, mut left) = (" ", " ", " ");
            for edge in node.edges.iter() {
                match edge.dst_x.cmp(&node.x) {
                    Ordering::Equal => mid = "|",
                    Ordering::Less => left = "\\",
                    Ordering::Greater => right = "/",
                };
            }
            s.push_str(&format!("{}{}{}", left, mid, right));
        }
        s.push_str(&format!("\n{} ", &row_num.to_string()));
        s.push_str(&padding_genrator(
            left_padding_size - row_num.to_string().len(),
        ));
        for node in nodes[row_num].iter() {
            let mut node_symbol = " ";
            if row_num == nodes.len() - 1 {
                for lower_node in nodes[row_num - 1].iter() {
                    for edge in lower_node.edges.iter() {
                        if edge.dst_x == node.x {
                            node_symbol = get_room_symbol(&node.class);
                        }
                    }
                }
            } else if !node.edges.is_empty() {
                node_symbol = get_room_symbol(&node.class);
            }
            s.push_str(&format!(" {} ", node_symbol));
        }
        if row_num == 0 {
            break;
        }
        row_num -= 1;
    }
    println!("{}", &s);
}

fn padding_genrator(n: usize) -> String {
    let mut s = String::new();
    for _i in 0..n {
        s.push(' ');
    }
    s
}

fn generate_room_type(
    room_chances: &HashMap<RoomType, f64>,
    available_room_count: usize,
) -> Vec<RoomType> {
    let mut acc = vec![];
    #[cfg(debug_assertions)]
    println!("Rooms: {:?}", &available_room_count);
    let rooms_type_q = [ShopRoom, RestRoom, MonsterRoomElite, EventRoom];
    for t in rooms_type_q.iter() {
        let chance = room_chances.get(&t).unwrap();
        let rooms = (chance * available_room_count as f64).round() as usize;
        #[cfg(debug_assertions)]
        println!("{:?}: {:?}", &t, &rooms);
        for _i in 0..rooms {
            acc.push(*t);
        }
    }
    #[cfg(debug_assertions)]
    println!("{:?}: {:?}", &MonsterRoom, available_room_count - acc.len());
    acc
}
fn rule_assignable_to_row(n: &MapRoomNode, room: &RoomType) -> bool {
    let applicable_rooms = [RestRoom, MonsterRoomElite];
    if n.y <= 4 && applicable_rooms.contains(room) {
        return false;
    }
    if n.y >= 13 && room == &RestRoom {
        return false;
    }
    true
}

fn rule_sibling_matches(sibs: &[&MapRoomNode], room: &RoomType) -> bool {
    let applicable_rooms = [
        RestRoom,
        TreasureRoom,
        ShopRoom,
        MonsterRoomElite,
        MonsterRoom,
        EventRoom,
    ];
    for sib in sibs.iter() {
        if sib.class.is_some()
            && applicable_rooms.contains(&room)
            && room == sib.class.as_ref().unwrap()
        {
            return true;
        }
    }
    false
}

fn get_next_room_type(map: &Map, n: &MapRoomNode, room_list: &[RoomType]) -> Option<RoomType> {
    let parents = &n.parents;
    let siblings = get_siblings(map, n);
    for room in room_list.iter() {
        if rule_assignable_to_row(n, room) {
            if !rule_parent_matches(map, parents, room) && !rule_sibling_matches(&siblings, room) {
                return Some(*room);
            }
            // unreachable
            if n.y == 0 {
                return Some(*room);
            }
        }
    }
    None
}

fn rule_parent_matches(map: &Map, parents: &[Point], room: &RoomType) -> bool {
    let applicable_rooms = [RestRoom, TreasureRoom, ShopRoom, MonsterRoomElite];
    for parent in parents.iter() {
        if let Some(class) = parent.get_node(map).class {
            if applicable_rooms.contains(&class) && room == &class {
                return true;
            }
        }
    }
    false
}

fn get_siblings<'a>(map: &'a Map, node: &'a MapRoomNode) -> Vec<&'a MapRoomNode> {
    let mut siblings = vec![];
    for parent in node.parents.iter() {
        for edge in parent.get_node(map).edges.iter() {
            let sib_node = &map[edge.dst_y as usize][edge.dst_x as usize];
            if sib_node != node {
                siblings.push(sib_node);
            }
        }
    }
    siblings
}

fn assign_rooms_to_nodes(map: Map, room_list: &mut Vec<RoomType>) -> Map {
    let mut map = map;
    let height = map.len();
    let width = map[0].len();
    for y in 0..height {
        for x in 0..width {
            let node = &map[y][x];
            if !node.edges.is_empty() && node.class.is_none() {
                if let Some(room_to_be_set) = get_next_room_type(&map, node, room_list) {
                    let pos = room_list.iter().position(|&x| x == room_to_be_set).unwrap();
                    room_list.remove(pos);
                    map[y][x].class = Some(room_to_be_set);
                }
            }
        }
    }
    if !room_list.is_empty() {
        #[cfg(debug_assertions)]
        println!("Leftovers: {:?}", &room_list);
    }
    map
}

fn last_minute_node_checker(map: Map) -> Map {
    let mut map = map;
    for row in map.iter_mut() {
        for node in row.iter_mut() {
            if !node.edges.is_empty() && node.class.is_none() {
                #[cfg(debug_assertions)]
                println!(
                    "Room [{:?},{:?}] was empty. Now populated with monsters.",
                    &node.y, &node.x
                );
                node.class = Some(MonsterRoom);
            }
        }
    }
    map
}

fn count_connected_nodes(map: &Map) -> usize {
    let mut res = 0;
    for row in map.iter() {
        for node in row.iter() {
            if !node.edges.is_empty() && node.class.is_none() {
                res += 1;
            }
        }
    }
    res
}

fn distribute_rooms_across_map(map: Map, room_list: Vec<RoomType>, rng: &mut Random) -> Map {
    let mut map = map;
    let mut room_list = room_list;
    let node_count = count_connected_nodes(&map);
    while room_list.len() < node_count {
        room_list.push(MonsterRoom);
    }
    shuffle(&mut room_list, rng);
    map = assign_rooms_to_nodes(map, &mut room_list);
    map = last_minute_node_checker(map);
    map
}

#[derive(Serialize, Deserialize, Debug)]
struct DumpNode {
    x: i32,
    y: i32,
    class: RoomType,
}

#[derive(Serialize, Deserialize, Debug)]
struct DumpMap {
    edges: Vec<MapEdge>,
    nodes: Vec<DumpNode>,
}

fn dump_map(map: &Map, path: &Path) {
    let mut edges = vec![];
    let mut nodes = vec![];
    for row in map.iter() {
        for node in row.iter() {
            if let Some(t) = node.class {
                if !node.parents.is_empty() || !node.edges.is_empty() {
                    nodes.push(DumpNode {
                        x: node.x,
                        y: node.y,
                        class: t,
                    });
                }
                for edge in node.edges.iter() {
                    edges.push(edge.clone());
                }
            }
        }
    }
    let dump = DumpMap { edges, nodes };
    let serialized = serde_json::to_string(&dump).unwrap();
    write_to_file(&serialized, path);
}

fn write_to_file(s: &str, path: &Path) {
    use std::fs::File;
    use std::io::prelude::*;

    let display = path.display();
    let mut file = match File::create(&path) {
        Err(why) => panic!("couldn't create {}: {}", display, why),
        Ok(file) => file,
    };

    match file.write_all(s.as_bytes()) {
        Err(why) => panic!("couldn't write to {}: {}", display, why),
        Ok(_) => println!("Map saved to {}", display),
    }
}

fn shuffle<T: std::fmt::Debug>(list: &mut Vec<T>, rng: &mut Random) {
    let size = list.len();
    for i in (2..=size).rev() {
        let tmp = rng.next_shuffle(i as u64);
        list.swap(tmp, i - 1);
    }
}

pub fn generate_and_print_maps(
    seed: i64,
    map_height: i32,
    map_width: i32,
    path_density: i32,
    path: &Option<&Path>,
) {
    let acts = [1, 200, 600];
    for (i, act) in acts.iter().enumerate() {
        println!("\n\nAct {:?}", i + 1);
        let mut rng = Random::new(seed + act);
        let mut map = generate_dungeon(map_height, map_width, path_density, &mut rng);
        let mut count = 0usize;
        for row in map.iter() {
            for n in row.iter() {
                if n.edges.is_empty() || n.y as usize == map.len() - 1 {
                    continue;
                }
                count += 1;
            }
        }
        map[0]
            .iter_mut()
            .for_each(|node| node.class = Some(MonsterRoom));
        map[8]
            .iter_mut()
            .for_each(|node| node.class = Some(TreasureRoom));
        let map_size = map.len();
        map[map_size - 1]
            .iter_mut()
            .for_each(|node| node.class = Some(RestRoom));

        let room_chances: HashMap<RoomType, f64> = [
            (ShopRoom, 0.05),
            (RestRoom, 0.12),
            (EventRoom, 0.22),
            (MonsterRoomElite, 0.08 * 1.6), //x1.6 for ascensionLevel >= 1
        ]
        .iter()
        .cloned()
        .collect();
        let room_list = generate_room_type(&room_chances, count);
        map = distribute_rooms_across_map(map, room_list, &mut rng);
        print_map(&map);
        if let Some(path) = path {
            if path.exists() {
                let file_name = format!("{:?}_Act{:?}.json", seed, i + 1);
                let path = path.join(file_name);
                dump_map(&map, &path);
            } else {
                println!("Can't save map because path {:?} doesn't exist", &path);
            }
        }
    }
}
