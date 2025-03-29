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
    let mut map: HashMap<usize, f64> = HashMap::new();

    for (i, opcode) in opcodes.iter().enumerate() {
        match opcode {
            OpCode::VarY => {
                map.insert(i, x);
            }
            OpCode::VarX => {
                map.insert(i, y);
            }
            OpCode::Add(k1, k2) => {
                map.insert(i, map.get(k1).unwrap() + map.get(k2).unwrap());
            }
            OpCode::Sub(k1, k2) => {
                map.insert(i, map.get(k1).unwrap() - map.get(k2).unwrap());
            }
            OpCode::Mul(k1, k2) => {
                map.insert(i, map.get(k1).unwrap() * map.get(k2).unwrap());
            }
            OpCode::Neg(k) => {
                map.insert(i, map.get(k).unwrap().neg());
            }
            OpCode::Const(cnst) => {
                map.insert(i, *cnst);
            }
            OpCode::Square(k) => {
                map.insert(i, map.get(k).unwrap().powi(2));
            }
            OpCode::Sqrt(k) => {
                map.insert(i, map.get(k).unwrap().sqrt());
            }
            OpCode::Max(k1, k2) => {
                map.insert(i, map.get(k1).unwrap().max(*map.get(k2).unwrap()));
            }
            OpCode::Min(k1, k2) => {
                map.insert(i, map.get(k1).unwrap().min(*map.get(k2).unwrap()));
            }
        }
    }

    *map.get(&(opcodes.len() - 1)).unwrap()
}

fn main() {
    let file = fs::read_to_string("./prospero.vm").expect("File to be present.");

    let opcodes: Vec<OpCode> = file.lines().map(|e| e.into()).collect();

    for opcode in &opcodes {
        println!("{:?}", opcode);
    }

    const delta: f64 = 1.0 / 100.0;
    let mut y: f64 = -1.0;
    let mut x: f64 = -1.0;

    while y <= 1.0 {
        while x <= 1.0 {
            let value = interpret(&opcodes, -y, x);

            if value.is_sign_positive() {
                print!(".");
            } else {
                print!("#");
            }

            x += delta;
        }

        println!();
        y += delta;
        x = -1.0;
    }
}
