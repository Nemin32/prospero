use std::io::Write;
use std::{
    fs::{self, OpenOptions},
    ops::Neg,
    sync::{Arc, RwLock},
};

use rayon::prelude::*;

const RESOLUTION: u16 = 1024;
const DELTA: f32 = 1.0 / (RESOLUTION as f32);

#[derive(Debug, Clone, Copy, PartialEq)]
enum Value {
    Address(usize),
    Literal(f32),
}

#[derive(Debug, Clone, Copy)]
enum OpCode {
    // Variables
    VarY,
    VarX,
    // Arithm.
    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    // Unary
    Neg(Value),
    Const(f32),
    Square(Value),
    Sqrt(Value),
    // Compare
    Max(Value, Value),
    Min(Value, Value),
    //Extend
    FuseMultiplyAdd(Value, Value, Value),
}

#[derive(Debug, Clone, Copy)]
struct Instruction {
    out: usize,
    op: OpCode,
}

impl From<&str> for Instruction {
    fn from(value: &str) -> Self {
        use OpCode::*;

        let parts = value.split(" ").collect::<Vec<_>>();
        let out = usize::from_str_radix(parts[0].trim_start_matches("_"), 16).unwrap();
        let code = parts[1];
        let args = &parts[2..];

        if code == "const" {
            let arg = args[0].parse::<f32>().unwrap();
            return Instruction {
                out,
                op: Const(arg),
            };
        }

        let args = args
            .iter()
            .map(|e| Value::Address(usize::from_str_radix(e.trim_start_matches("_"), 16).unwrap()))
            .collect::<Vec<Value>>();

        let op = match code {
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
        };

        Instruction { out, op }
    }
}

impl Instruction {
    fn inline_literal(self) -> Self {
        use OpCode::*;
        use Value::*;

        Instruction {
            out: self.out,
            op: match self.op {
                Add(Literal(v1), Literal(v2)) => Const(v1 + v2),
                Sub(Literal(v1), Literal(v2)) => Const(v1 - v2),
                Mul(Literal(v1), Literal(v2)) => Const(v1 * v2),
                Neg(Literal(c)) => Const(-c),
                Sqrt(Literal(c)) => Const(c.sqrt()),
                Square(Literal(c)) => Const(c * c),
                FuseMultiplyAdd(Literal(v1), Literal(v2), Literal(v3)) => {
                    Const(f32::mul_add(v1, v2, v3))
                }
                Max(Literal(v1), Literal(v2)) => Const(f32::max(v1, v2)),
                Min(Literal(v1), Literal(v2)) => Const(f32::min(v1, v2)),
                orig => orig,
            },
        }
    }
}

fn inline_consts(instructions: &[Instruction]) -> Vec<Instruction> {
    use OpCode::*;
    use Value::*;

    let extract_other = |other: Value| -> Value {
        if let Address(other_addr) = other {
            match instructions[other_addr].op {
                Const(c) => Literal(c),
                _ => Address(other_addr),
            }
        } else {
            other
        }
    };

    instructions
        .iter()
        .map(|inst| {
            let new_op = match inst.op {
                OpCode::VarY => VarY,
                OpCode::VarX => VarX,
                OpCode::Add(value, value1) => Add(extract_other(value), extract_other(value1)),
                OpCode::Sub(value, value1) => Sub(extract_other(value), extract_other(value1)),
                OpCode::Mul(value, value1) => Mul(extract_other(value), extract_other(value1)),
                OpCode::Neg(value) => Neg(extract_other(value)),
                OpCode::Const(val) => Const(val),
                OpCode::Square(value) => Square(extract_other(value)),
                OpCode::Sqrt(value) => Sqrt(extract_other(value)),
                OpCode::Max(value, value1) => Max(extract_other(value), extract_other(value1)),
                OpCode::Min(value, value1) => Min(extract_other(value), extract_other(value1)),
                OpCode::FuseMultiplyAdd(value, value1, value2) => FuseMultiplyAdd(
                    extract_other(value),
                    extract_other(value1),
                    extract_other(value2),
                ),
            };

            Instruction {
                out: inst.out,
                op: new_op,
            }
        })
        .collect()
}

fn optimize_literal(instructions: &[Instruction]) -> Vec<Instruction> {
    instructions
        .iter()
        .map(|inst| inst.inline_literal())
        .collect()
}

fn optimize(instructions: &[Instruction]) -> Vec<Instruction> {
    use OpCode::*;
    use Value::*;

    let mut new_insts: Vec<(bool, Instruction)> = Vec::with_capacity(instructions.len());

    let instructions = inline_consts(instructions);
    let instructions = optimize_literal(&instructions);
    let instructions = inline_consts(&instructions);

    for inst in instructions {
        let (changed, op): (bool, OpCode) = match inst.op {
            orig @ Add(Address(addr), other) | orig @ Add(other, Address(addr)) => {
                match new_insts[addr].1.op {
                    Neg(v) => (true, Sub(other, v)),
                    Mul(v1, v2) => (true, FuseMultiplyAdd(v1, v2, other)),
                    Const(c) => (true, Add(Literal(c), other)),
                    _ => (false, orig),
                }
            }
            orig @ Mul(Address(addr), other) | orig @ Mul(other, Address(addr)) => {
                match new_insts[addr].1.op {
                    Const(c) => (true, Mul(Literal(c), other)),
                    _ => (false, orig),
                }
            }
            orig @ Sub(Address(addr), other) => match new_insts[addr].1.op {
                Const(c) => (true, Sub(Literal(c), other)),
                _ => (false, orig),
            },
            orig @ Sub(other, Address(addr)) => match new_insts[addr].1.op {
                Const(c) => (true, Sub(other, Literal(c))),
                _ => (false, orig),
            },
            orig @ Square(Address(addr)) => match new_insts[addr].1.op {
                Sqrt(Literal(v)) => (true, Const(v)),
                _ => (false, orig),
            },
            orig @ Sqrt(Address(addr)) => match new_insts[addr].1.op {
                Square(Literal(v)) => (true, Const(v)),
                _ => (false, orig),
            },
            _ => (false, inst.op.to_owned()),
        };

        new_insts.push((changed, Instruction { out: inst.out, op }))
    }

    let has_changed = new_insts.iter().any(|(b, _)| *b);
    let just_insts = new_insts
        .iter()
        .map(|(_, i)| *i)
        .collect::<Vec<Instruction>>();

    if has_changed {
        println!("Doing next round.");
        optimize(&just_insts)
    } else {
        just_insts.to_vec()
    }
}

/// Takes in a list of instructions and generates its mathematical representation recursively.
/// Not much point to it beyond being curious what the actual equation looks like.
#[allow(dead_code)]
fn unroll(instructions: &[Instruction], index: Value) -> String {
    use OpCode::*;

    match index {
        Value::Literal(lit) => format!("{}", lit),
        Value::Address(addr) => match instructions[addr].op {
            VarY => String::from("Y"),
            VarX => String::from("X"),
            Add(k1, k2) => format!(
                "({} + {})",
                unroll(instructions, k1),
                unroll(instructions, k2)
            ),
            Sub(k1, k2) => format!(
                "({} - {})",
                unroll(instructions, k1),
                unroll(instructions, k2)
            ),
            Mul(k1, k2) => format!(
                "({} - {})",
                unroll(instructions, k1),
                unroll(instructions, k2)
            ),
            Neg(k) => format!("-({})", unroll(instructions, k)),
            Const(cnst) => format!("{}", cnst),
            Square(k) => format!("({})^2", unroll(instructions, k)),
            Sqrt(k) => format!("sqrt({})", unroll(instructions, k)),
            Max(k1, k2) => format!(
                "max({}, {})",
                unroll(instructions, k1),
                unroll(instructions, k2)
            ),
            Min(k1, k2) => format!(
                "min({}, {})",
                unroll(instructions, k1),
                unroll(instructions, k2)
            ),
            FuseMultiplyAdd(k1, k2, k3) => format!(
                "({} * {}) + ({})",
                unroll(instructions, k1),
                unroll(instructions, k2),
                unroll(instructions, k3)
            ),
        },
    }
}

#[allow(dead_code)]
fn interpret(instructions: &[Instruction], x: f32, y: f32) -> f32 {
    use OpCode::*;

    let mut map: Vec<f32> = Vec::with_capacity(instructions.len());

    fn extract(map: &[f32], key: Value) -> f32 {
        match key {
            Value::Address(addr) => map[addr],
            Value::Literal(lit) => lit,
        }
    }

    for (i, Instruction { out: _, op }) in instructions.iter().enumerate() {
        let value = match *op {
            VarY => y,
            VarX => x,
            Add(k1, k2) => extract(&map, k1) + extract(&map, k2),
            Sub(k1, k2) => extract(&map, k1) - extract(&map, k2),
            Mul(k1, k2) => extract(&map, k1) * extract(&map, k2),
            Neg(k) => extract(&map, k).neg(),
            Const(cnst) => cnst,
            Square(k) => extract(&map, k).powi(2),
            Sqrt(k) => extract(&map, k).sqrt(),
            Max(k1, k2) => f32::max(extract(&map, k1), extract(&map, k2)),
            Min(k1, k2) => f32::min(extract(&map, k1), extract(&map, k2)),
            FuseMultiplyAdd(k1, k2, k3) => {
                f32::mul_add(extract(&map, k1), extract(&map, k2), extract(&map, k3))
            }
        };

        map.insert(i, value);
    }

    map[instructions.len() - 1]
}

#[allow(dead_code)]
fn interpret_memo(instructions: &mut [Instruction], index: Value, x: f32, y: f32) -> f32 {
    use OpCode::*;

    match index {
        Value::Literal(lit) => lit,
        Value::Address(addr) => {
            let value = match instructions[addr].op {
                VarX => x,
                VarY => y,
                Const(c) => c,
                Add(v1, v2) => {
                    interpret_memo(instructions, v1, x, y) + interpret_memo(instructions, v2, x, y)
                }
                Sub(v1, v2) => {
                    interpret_memo(instructions, v1, x, y) - interpret_memo(instructions, v2, x, y)
                }
                Mul(v1, v2) => {
                    interpret_memo(instructions, v1, x, y) * interpret_memo(instructions, v2, x, y)
                }
                Neg(v) => interpret_memo(instructions, v, x, y).neg(),
                Square(v) => interpret_memo(instructions, v, x, y).powi(2),
                Sqrt(v) => interpret_memo(instructions, v, x, y).sqrt(),
                Max(v1, v2) => f32::max(
                    interpret_memo(instructions, v1, x, y),
                    interpret_memo(instructions, v2, x, y),
                ),
                Min(v1, v2) => f32::min(
                    interpret_memo(instructions, v1, x, y),
                    interpret_memo(instructions, v2, x, y),
                ),
                FuseMultiplyAdd(v1, v2, v3) => f32::mul_add(
                    interpret_memo(instructions, v1, x, y),
                    interpret_memo(instructions, v2, x, y),
                    interpret_memo(instructions, v3, x, y),
                ),
            };

            instructions[addr] = Instruction {
                out: instructions[addr].out,
                op: Const(value),
            };

            value
        }
    }
}

fn main() {
    // Read file
    let file = fs::read_to_string("./prospero.vm").expect("File to be present.");

    // Parse opcodes
    let instructions: Vec<Instruction> = file.par_lines().map(|e| e.into()).collect();
    let instructions = optimize(&instructions);

    for op in &instructions {
        println!("{} - {:?}", op.out, op.op);
    }

    let len = instructions.len() - 1;
    println!("{}", unroll(&instructions, Value::Address(len)));

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
                    let insts = insts.read().unwrap().to_owned();
                    //let len = insts.len();
                    let val = interpret(&insts, *x, -*y);

                    //let val = interpret_memo(&mut insts, Value::Address(len - 1), *x, -*y);

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
            .map(|e| if *e { "0" } else { "1" })
            .collect::<Vec<_>>();
        writeln!(output, "{}", line.join(" ")).unwrap();
    }
}
