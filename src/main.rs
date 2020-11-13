// MIT/Apache2 License

mod m64;

use m64::{MFrame, MHeader, MRecord};
use std::{env, error::Error, fs, io::prelude::*, path::PathBuf, process};
use tinyvec::ArrayVec;

fn main() {
    // the first argument ought to be the operation to do
    let operation = env::args().nth(1).unwrap_or_else(|| {
        eprintln!("Program requires at least one argument as an operation");
        process::exit(1)
    });

    match operation.as_str() {
        "stat" => {
            let (name, record) = open_record(2, "stat");
            println!(
                "Summary of Recording {}
{}",
                name, record.header
            );
        }
        "frames" => {
            let (name, record) = open_record(2, "frames");
            println!("Input frames of Recording {}", name);
            record.frames.into_iter().enumerate().for_each(|(i, f)| {
                println!("Frame #{}: {}", i, f);
            });
        }
        "cut" => {
            let (name, mut record) = open_record(2, "cut");
            let (_, mut outfile) = open_file(3, "cut", true);
            let posns: [usize; 2] = env::args()
                .skip(4)
                .take(2)
                .map(|p| p.parse().unwrap())
                .collect::<ArrayVec<[usize; 2]>>()
                .into_inner();
            let [posn1, posn2] = posns;
            record.frames = record
                .frames
                .into_iter()
                .skip(posn1)
                .take(posn2 - posn1)
                .collect();
            record.header.frame_count = record.frames.len() as _;
            record
                .write_to(&mut outfile)
                .expect("Failed to write to file");
        }
        operation => {
            eprintln!("Unknown operation: {}", operation);
            process::exit(1);
        }
    }
}

#[inline]
fn open_file(index: usize, operation: &'static str, write: bool) -> (PathBuf, fs::File) {
    let path = env::args_os().nth(index).unwrap_or_else(|| {
        eprintln!(
            "Operation {} requires an argument in position {}",
            operation, index
        );
        process::exit(1)
    });
    let path: PathBuf = path.into();

    let file = if write {
        fs::File::create(&path)
    } else {
        fs::File::open(&path)
    }
    .unwrap_or_else(|e| {
        eprintln!("Unable to open file: {}", e);
        process::exit(1)
    });

    (path, file)
}

#[inline]
fn open_record(index: usize, operation: &'static str) -> (String, MRecord) {
    let (mut path, mut file) = open_file(index, operation, false);

    // read bytes
    let mut buf = Vec::new();
    file.read_to_end(&mut buf).unwrap_or_else(|e| {
        eprintln!("Unable to read from file: {}", e);
        process::exit(1)
    });

    let record = MRecord::from_bytes(&buf).unwrap_or_else(|e| {
        eprintln!("Failed to parse M64 file: {}", e);
        process::exit(1)
    });

    (
        path.file_name().unwrap().to_string_lossy().into_owned(),
        record,
    )
}
