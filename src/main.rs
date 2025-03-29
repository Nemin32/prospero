use std::{collections::HashMap, fs, ops::Neg};

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

        let parts = value.split_ascii_whitespace().collect::<Vec<_>>();
        let code = *parts.get(1).expect("Expected opcode to be present.");

        let arg1 = parts.get(2).map(|e| e.trim_start_matches("_"));

        let arg1u = arg1
            .as_ref()
            .map(|inner| usize::from_str_radix(&inner, 16).ok())
            .flatten();
        let arg1f = arg1
            .as_ref()
            .map(|inner| inner.parse::<f64>().ok())
            .flatten();

        let arg2 = parts
            .get(3)
            .map(|e| e.trim_start_matches("_"))
            .map(|e| usize::from_str_radix(&e, 16).expect(&format!("Failed to parse: {}", e)));

        match code {
            "var-y" => VarY,
            "var-x" => VarX,
            "add" => {
                let arg2 = arg2.expect("Expected two arguments for Add");
                Add(arg1u.expect("Expected number argument for Add"), arg2)
            }
            "sub" => {
                let arg2 = arg2.expect("Expected two arguments for Sub");
                Sub(arg1u.expect("Expected number argument for Sub"), arg2)
            }
            "mul" => {
                let arg2 = arg2.expect("Expected two arguments for Mul");
                Mul(arg1u.expect("Expected number argument for Mul"), arg2)
            }
            "const" => Const(arg1f.expect("Expected float in Const.")),
            "neg" => Neg(arg1u.expect("Expected usize for Neg.")),
            "square" => Square(arg1u.expect("Expected usize for Square.")),
            "sqrt" => Sqrt(arg1u.expect("Expected usize for Sqrt.")),
            "min" => {
                let arg2 = arg2.expect("Expected two arguments for Min");
                Min(arg1u.expect("Expected number argument for Min"), arg2)
            }
            "max" => {
                let arg2 = arg2.expect("Expected two arguments for Max");
                Max(arg1u.expect("Expected number argument for Max"), arg2)
            }
            _ => {
                panic!("Unexpected code: {}", code)
            }
        }
    }
}

fn interpret(opcodes: &Vec<OpCode>, x: f64, y: f64) -> f64 {
    let mut map: Vec<f64> = Vec::with_capacity(opcodes.len());

    for (i, opcode) in opcodes.iter().enumerate() {
        let value = match opcode {
            OpCode::VarY => y,
            OpCode::VarX => x,
            OpCode::Add(k1, k2) => map[*k1] + map[*k2],
            OpCode::Sub(k1, k2) => map[*k1] - map[*k2],
            OpCode::Mul(k1, k2) => map[*k1] * map[*k2],
            OpCode::Neg(k) => map[*k].neg(),
            OpCode::Const(cnst) => *cnst,
            OpCode::Square(k) => map[*k].powi(2),
            OpCode::Sqrt(k) => map[*k].sqrt(),
            OpCode::Max(k1, k2) => map[*k1].max(map[*k2]),
            OpCode::Min(k1, k2) => map[*k1].min(map[*k2]),
        };

        map.insert(i, value);
    }

    map[opcodes.len() - 1]
}

fn main() {
    let file = fs::read_to_string("./prospero.vm").expect("File to be present.");

    let opcodes: Vec<OpCode> = file.lines().map(|e| e.into()).collect();

    for opcode in &opcodes {
        println!("{:?}", opcode);
    }

    const DELTA: f64 = 1.0 / 100.0;
    let mut y: f64 = -1.0;
    let mut x: f64 = -1.0;

    while y <= 1.0 {
        while x <= 1.0 {
            let value = interpret(&opcodes, x, -y);

            if value.is_sign_positive() {
                print!(".");
            } else {
                print!("#");
            }

            x += DELTA;
        }

        println!();
        y += DELTA;
        x = -1.0;
    }
}
