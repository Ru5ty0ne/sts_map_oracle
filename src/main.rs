#![feature(unchecked_math)]
use clap::{App, Arg};
use std::path::Path;

use sts_map_oracle;

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

    sts_map_oracle::generate_and_print_maps(
        seed,
        map_height,
        map_width,
        path_density,
        &matches.value_of("path").map(Path::new),
    );
}
