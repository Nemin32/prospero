use std::io::Write;
use std::{
    fs::{self, OpenOptions},
    ops::Neg,
    sync::{Arc, RwLock},
};

use rayon::prelude::*;

const RESOLUTION: u16 = 1024;
const DELTA: f64 = 1.0 / (RESOLUTION as f64);

#[derive(Debug, Clone, Copy)]
enum OpCode {
    // Variables
    VarY,
    VarX,
    // Arithm.
    Add(usize, usize),
    Sub(usize, usize),
    Mul(usize, usize),
    // Unary
    Neg(usize),
    Const(f64),
    Square(usize),
    Sqrt(usize),
    // Compare
    Max(usize, usize),
    Min(usize, usize),
}

impl From<&str> for OpCode {
    fn from(value: &str) -> Self {
        use OpCode::*;

        let parts = value.split(" ").collect::<Vec<_>>();
        let code = parts[1];
        let args = &parts[2..];

        if code == "const" {
            let arg = args[0].parse::<f64>().unwrap();
            return Const(arg);
        }

        let args = args
            .iter()
            .map(|e| usize::from_str_radix(e.trim_start_matches("_"), 16).unwrap())
            .collect::<Vec<usize>>();

        match code {
            "var-y" => VarY,
            "var-x" => VarX,
            "add" => Add(args[0], args[1]),
            "sub" => Sub(args[0], args[1]),
            "mul" => Mul(args[0], args[1]),
            "neg" => Neg(args[0]),
            "square" => Square(args[0]),
            "sqrt" => Sqrt(args[0]),
            "min" => Min(args[0], args[1]),
            "max" => Max(args[0], args[1]),
            _ => unreachable!("Unexpected code: {}", code),
        }
    }
}

fn interpret(opcodes: &[OpCode], x: f64, y: f64) -> f64 {
    use OpCode::*;

    let mut map: Vec<f64> = Vec::with_capacity(opcodes.len());

    for opcode in opcodes {
        let value = match *opcode {
            VarY => y,
            VarX => x,
            Add(k1, k2) => map[k1] + map[k2],
            Sub(k1, k2) => map[k1] - map[k2],
            Mul(k1, k2) => map[k1] * map[k2],
            Neg(k) => map[k].neg(),
            Const(cnst) => cnst,
            Square(k) => map[k] * map[k],
            Sqrt(k) => map[k].sqrt(),
            Max(k1, k2) => map[k1].max(map[k2]),
            Min(k1, k2) => map[k1].min(map[k2]),
        };

        map.push(value);
    }

    map[opcodes.len() - 1]
}

fn main() {
    // Read file
    let file = fs::read_to_string("./prospero.vm").expect("File to be present.");

    // Parse opcodes
    let opcodes: Vec<OpCode> = file.par_lines().map(|e| e.into()).collect();
    let shared_ops: Arc<RwLock<Vec<OpCode>>> = Arc::new(RwLock::new(opcodes));

    // Precompute matrix
    let mut values = Vec::new();
    let mut n: f64 = -1.0;
    while n <= 1.0 {
        values.push(n);
        n += DELTA;
    }

    // Compute pixels
    let pixels = values
        .clone()
        .par_iter()
        .map(|y| {
            let ops = shared_ops.clone();
            values
                .par_iter()
                .map(|x| {
                    let val = interpret(&ops.read().unwrap(), *x, -*y);
                    val.is_sign_positive()
                })
                .collect()
        })
        .collect::<Vec<Vec<bool>>>();

    // Write file
    let mut output = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open("./output.pbm")
        .unwrap();

    write!(output, "P1 {} {} ", RESOLUTION * 2 + 1, RESOLUTION * 2).unwrap();

    for row in pixels {
        let line = row
            .par_iter()
            .map(|e| if *e { "1" } else { "0" })
            .collect::<Vec<_>>();
        writeln!(output, "{}", line.join(" ")).unwrap();
    }
}
