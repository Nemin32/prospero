use std::io::Write;
use std::{
    fs::OpenOptions,
    ops::Neg,
    sync::{Arc, RwLock},
};

use generators::{circle, difference, rectange, union};
use instruction::{Instruction, generate_register_mapping};
use interval::{Interval, IntervalSign};
use opcode::{OpCode, Value};
use optimizers::prune;
use parser::generate_bytecode;
use quadtree::Quadtree;
use rayon::{iter, prelude::*};

mod generators;
mod instruction;
mod interval;
mod opcode;
mod optimizers;
mod parser;
mod quadtree;

const RESOLUTION: u16 = 1024;
const DELTA: f32 = 1.0 / (RESOLUTION as f32);

fn interpret(instructions: &[Instruction], len: usize, x: f32, y: f32) -> f32 {
    use OpCode::*;

    let mut map: Vec<f32> = iter::repeat(0f32).take(len + 1).collect();

    fn extract(map: &[f32], key: Value) -> f32 {
        match key {
            Value::Address(addr) => map[addr],
            Value::Literal(lit) => lit,
        }
    }

    for Instruction { out, op } in instructions.iter() {
        let value = match *op {
            VarY => y,
            VarX => x,
            Add(k1, k2) => extract(&map, k1) + extract(&map, k2),
            Sub(k1, k2) => extract(&map, k1) - extract(&map, k2),
            Mul(k1, k2) => extract(&map, k1) * extract(&map, k2),
            Div(k1, k2) => extract(&map, k1) / extract(&map, k2),
            Neg(k) => extract(&map, k).neg(),
            Const(cnst) => cnst,
            Square(k) => extract(&map, k).powi(2),
            Sqrt(k) => extract(&map, k).sqrt(),
            Max(k1, k2) => f32::max(extract(&map, k1), extract(&map, k2)),
            Min(k1, k2) => f32::min(extract(&map, k1), extract(&map, k2)),
        };

        map[*out] = value;
    }

    map[0]
}

fn rayon_process(instructions: Vec<Instruction>, len: usize) -> Vec<Vec<bool>> {
    let shared_instructions: Arc<RwLock<Vec<Instruction>>> = Arc::new(RwLock::new(instructions));

    // Precompute matrix
    let mut values = Vec::new();
    let mut n: f32 = -1.0;
    while n <= 1.0 {
        values.push(n);
        n += DELTA;
    }

    // Compute pixels
    let pixels = values
        .clone()
        .par_iter()
        .map(|y| {
            let insts = shared_instructions.clone();

            values
                .par_iter()
                .map(|x| {
                    let insts = insts.clone();
                    let val = interpret(
                        &insts.read().expect("Couldn't get read lock to mutex."),
                        len,
                        *x,
                        -*y,
                    );

                    val.is_sign_positive()
                })
                .collect()
        })
        .collect::<Vec<Vec<bool>>>();

    pixels
}

fn write_image_bool(pixels: Vec<Vec<bool>>) {
    let mut output = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open("./output.pbm")
        .expect("Can't open output file.");

    write!(output, "P1 {} {} ", RESOLUTION * 2 + 1, RESOLUTION * 2).unwrap();

    for row in pixels {
        let line = row
            .par_iter()
            .map(|e| if *e { "0" } else { "1" })
            .collect::<Vec<_>>();
        writeln!(output, "{}", line.join(" ")).unwrap();
    }
}

fn finalize(instructions: &[Instruction], pixels: &mut [Vec<IntervalSign>], max_interval: f32) {
    let flen = pixels.len() as f32;
    pixels.iter_mut().enumerate().for_each(|(y, row)| {
        row.iter_mut().enumerate().for_each(|(x, elem)| {
            if let IntervalSign::Indeterminate = elem {
                let x_coord = (x as f32 / flen - 0.5) * max_interval;
                let y_coord = (y as f32 / flen - 0.5) * max_interval;

                *elem = interpret(instructions, instructions.len(), x_coord, y_coord).into();
            }
        })
    });
}

fn write_image_sign(pixels: Vec<Vec<IntervalSign>>) {
    let mut output = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open("./output.pgm")
        .expect("Can't open output file.");

    write!(output, "P2 {} {} 3 ", RESOLUTION, RESOLUTION).unwrap();

    for row in pixels.iter().rev() {
        let line = row
            .par_iter()
            .map(|e| match *e {
                IntervalSign::Border => "1",
                IntervalSign::Negative => "0",
                IntervalSign::Indeterminate => "2",
                IntervalSign::Positive => "3",
            })
            .collect::<Vec<_>>();
        writeln!(output, "{}", line.join(" ")).unwrap();
    }
}

fn main() {
    //let input = String::from("(max (- (sqrt (+ (square x) (square y))) 1) (- 0.5 (sqrt (+ (square (- x 1)) (square y)))))");

    /*
    let pos = [
        // Upper half
        (-0.775, 0.7),
        (-0.5, 0.5),
        (0.0, 0.75),
        (0.5, 0.5),
        (0.775, 0.7),
        // Lower half
        (-0.65, -0.9),
        (-0.35, -1.0),
        (0.0, -1.05),
        (0.35, -1.0),
        (0.65, -0.9),

    ].map(|(x,y)| circle(x,y, 0.025));

    let pos2 = [
        (0.0,0.0),
        (-0.15, -0.25),
        (0.15, -0.25),
        //

        (-0.05, -0.125),
        (0.05, -0.125),
        (0.0, -0.2),

    ].map(|(x,y)| circle(x, y, 0.001));


    let input = difference(circle(0.0,0.0, 1.0), circle(1.0,0.0, 0.5));
    let input = difference(input, circle(-1.0, 0.0, 0.5));
    let input = pos.iter().fold(input, |acc, elem| difference(acc, elem.to_owned()));
    let input = pos2.iter().fold(input, |acc, elem| difference(acc, elem.to_owned()));
    */

    const S: f32 = 0.1 / 8.0;
    let M = [
        (0, 0, 2, 8),
        (5, 0, 2, 8),
        (2, 4, 1, 3),
        (3, 3, 1, 3),
        (4, 4, 1, 3),
    ]
    .map(|(x, y, w, h)| rectange(x as f32 * S, y as f32 * S, w as f32 * S, h as f32 * S))
    .into_iter()
    .reduce(|acc, elem| union(acc.to_owned(), elem.to_owned()))
    .map(|e| e.to_owned())
    .expect("Expected input string");

    let input = M; //rectange(-0.5, 0.0, 1.0, 1.0);
    println!("{input}");
    let instructions = generate_bytecode(input);

    //return;
    // Read file
    // let file = fs::read_to_string("./prospero.vm").expect("File to be present.");

    // Parse opcodes
    // let instructions: Vec<Instruction> = file.par_lines().map(|e| e.into()).collect();
    println!("Pre-opt Len: {}", instructions.len());
    let instructions = optimizers::optimize(&instructions);
    // Doesn't work for some reason.
    // let instructions = prune(&instructions);
    let (instructions, len) = generate_register_mapping(&instructions);

    println!("Len: {}", instructions.len());

    //let len = instructions.len();

    if false {
        for elem in &instructions {
            println!("{}", elem);
        }
    }

    let max_interval = 1.25f32;

    let x = Interval {
        start: -max_interval,
        end: max_interval,
    };

    let y = Interval {
        start: -max_interval,
        end: max_interval,
    };

    let mut buffer =
        vec![vec![IntervalSign::Indeterminate; RESOLUTION as usize]; RESOLUTION as usize];

    let mut qt = Quadtree::new(x, y);

    for _ in 0..12 {
        qt.split(&instructions, len);
    }

    // println!("{:?}, {:?}", qt, qt.rectangle_size(1.5, RESOLUTION as usize));

    qt.blit(&instructions, len, max_interval, &mut buffer);
    finalize(&instructions, &mut buffer, max_interval);
    //qt.draw_borders(max_interval, &mut buffer);

    write_image_sign(buffer);

    //let pixels = rayon_process(instructions, len);
    //write_image(pixels);
}
