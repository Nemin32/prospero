use std::io::Write;
use std::{
    fs::{self, OpenOptions},
    ops::Neg,
    sync::{Arc, RwLock},
    thread,
};

const RESOLUTION: u16 = 1000;
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
    let file = fs::read_to_string("./prospero.vm").expect("File to be present.");
    let opcodes: Vec<OpCode> = file.lines().map(|e| e.into()).collect();
    let shared_ops: Arc<RwLock<Vec<OpCode>>> = Arc::new(RwLock::new(opcodes));
    let mut threads = Vec::new();

    let mut y: f64 = -1.0;
    while y <= 1.0 {
        let opcodes = shared_ops.clone();
        threads.push(thread::spawn(move || {
            let mut row: Vec<bool> = Vec::with_capacity(usize::from(RESOLUTION) * 2);
            let opcodes = opcodes.read().unwrap();

            let mut x: f64 = -1.0;
            while x <= 1.0 {
                let value = interpret(&opcodes, x, -y);
                row.push(value.is_sign_positive());
                x += DELTA;
            }

            ((y * (RESOLUTION as f64)) as i64, row)
        }));

        y += DELTA;
    }

    let mut final_rows = threads
        .into_iter()
        .map(|r| r.join().unwrap())
        .collect::<Vec<(i64, Vec<bool>)>>();

    final_rows.sort_unstable_by_key(|(i, _)| *i);

    let mut output = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open("./output.pbm")
        .unwrap();

    write!(output, "P1 {} {} ", RESOLUTION * 2, RESOLUTION * 2).unwrap();

    for (_, row) in final_rows {
        let line = row
            .iter()
            .map(|e| if *e { "1" } else { "0" })
            .collect::<Vec<_>>();
        write!(output, "{}", line.join(" ")).unwrap();
    }
}
