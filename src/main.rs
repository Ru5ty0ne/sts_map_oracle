#![feature(unchecked_math)]
use clap::{App, Arg};
use std::path::Path;

use sts_map_oracle::dump_map;
use sts_map_oracle::format_map;
use sts_map_oracle::generate_maps;
use sts_map_oracle::Map;

fn main() {
    let matches = App::new("sts_map_oracle")
        .version("1.0.0")
        .author("Rusty 0ne <4fun.and.job.offers@gmail.com>")
        .about("Predicts map layout by seed")
        .arg(
            Arg::with_name("seed")
                .short("s")
                .long("seed")
                .required(true)
                .help("Seed for random number generator")
                .takes_value(true)
                .allow_hyphen_values(true),
        )
        .arg(
            Arg::with_name("path")
                .short("p")
                .long("path")
                .help("Path for saving maps in json format")
                .takes_value(true),
        )
        .get_matches();

    let map_height = 15i32;
    let map_width = 7i32;
    let path_density = 6i32;
    let seed = match matches.value_of("seed").unwrap().parse::<i64>() {
        Ok(num) => num,
        Err(_) => {
            println!("Can't parse seed");
            return;
        }
    };
    let path = &matches.value_of("path").map(Path::new);

    let maps: Vec<Map> = generate_maps(seed, map_height, map_width, path_density);

    for (i, map) in maps.iter().enumerate() {
        println!("\n\nAct {:?}\n{}", i + 1, format_map(&map));
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
