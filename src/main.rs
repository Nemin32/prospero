use std::io::Write;
use std::{
    fs::{self, OpenOptions},
    ops::Neg,
    sync::{Arc, RwLock},
};

use rayon::prelude::*;

const RESOLUTION: u16 = 1024;
const DELTA: f32 = 1.0 / (RESOLUTION as f32);

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
    Const(f32),
    Square(usize),
    Sqrt(usize),
    // Compare
    Max(usize, usize),
    Min(usize, usize),
    // Extend
    ConstAdd(usize, f32),
    ConstSub(usize, f32),
    SubConst(f32, usize),
    ConstMul(usize, f32),
}

#[derive(Clone, Copy)]
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
            .map(|e| usize::from_str_radix(e.trim_start_matches("_"), 16).unwrap())
            .collect::<Vec<usize>>();

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

fn optimize(instructions: &[Instruction]) -> Vec<Instruction> {
    use OpCode::*;

    let mut optimized: Vec<Instruction> = Vec::new();

    for window in instructions.chunks(2) {
        let value = match window {
            [
                Instruction {
                    out: out1,
                    op: Const(c),
                },
                Instruction {
                    out: out2,
                    op: Add(addr1, addr2),
                },
            ] if out1 == addr1 || out1 == addr2 => vec![
                Instruction {
                    out: *out1,
                    op: Const(*c),
                },
                Instruction {
                    out: *out2,
                    op: if out1 == addr1 {
                        ConstAdd(*addr2, *c)
                    } else {
                        ConstAdd(*addr1, *c)
                    },
                },
            ],
            [
                Instruction {
                    out: out1,
                    op: Const(c),
                },
                Instruction {
                    out: out2,
                    op: Sub(addr1, addr2),
                },
            ] if out1 == addr1 || out1 == addr2 => vec![
                Instruction {
                    out: *out1,
                    op: Const(*c),
                },
                Instruction {
                    out: *out2,
                    op: if out1 == addr1 {
                        SubConst(*c, *addr2)
                    } else {
                        ConstSub(*addr1, *c)
                    },
                },
            ],
            [
                Instruction {
                    out: out1,
                    op: Const(c),
                },
                Instruction {
                    out: out2,
                    op: Mul(addr1, addr2),
                },
            ] if out1 == addr1 || out1 == addr2 => vec![
                Instruction {
                    out: *out1,
                    op: Const(*c),
                },
                Instruction {
                    out: *out2,
                    op: if out1 == addr1 {
                        ConstMul(*addr2, *c)
                    } else {
                        ConstMul(*addr1, *c)
                    },
                },
            ],
            _ => window.to_vec(),
        };

        optimized.extend(value);
    }

    optimized
}

/// Takes in a list of instructions and generates its mathematical representation recursively.
/// Not much point to it beyond being curious what the actual equation looks like.
#[allow(dead_code)]
fn unroll(instructions: &[Instruction], index: usize) -> String {
    use OpCode::*;

    match instructions[index].op {
        VarY => String::from("Y"),
        VarX => String::from("X"),
        Add(k1, k2) => format!(
            "{} + {}",
            unroll(instructions, k1),
            unroll(instructions, k2)
        ),
        Sub(k1, k2) => format!(
            "{} - {}",
            unroll(instructions, k1),
            unroll(instructions, k2)
        ),
        Mul(k1, k2) => format!(
            "{} - {}",
            unroll(instructions, k1),
            unroll(instructions, k2)
        ),
        Neg(k) => format!("-{}", unroll(instructions, k)),
        Const(cnst) => format!("{}", cnst),
        Square(k) => format!("{}^2", unroll(instructions, k)),
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
        ConstAdd(k, v) => format!("{} + {}", unroll(instructions, k), v),
        ConstSub(k, v) => format!("{} - {}", unroll(instructions, k), v),
        SubConst(v, k) => format!("{} - {}", v, unroll(instructions, k)),
        ConstMul(k, v) => format!("{} * {}", unroll(instructions, k), v),
    }
}

fn interpret(instructions: &[Instruction], x: f32, y: f32) -> f32 {
    use OpCode::*;

    let mut map: Vec<f32> = Vec::with_capacity(instructions.len());

    for (i, Instruction { out: _, op }) in instructions.iter().enumerate() {
        let value = match *op {
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
            ConstAdd(k, v) => map[k] + v,
            ConstSub(k, v) => map[k] - v,
            SubConst(v, k) => v - map[k],
            ConstMul(k, v) => map[k] * v,
        };

        map.insert(i, value);
    }

    map[instructions.len() - 1]
}

fn main() {
    // Read file
    let file = fs::read_to_string("./prospero.vm").expect("File to be present.");

    // Parse opcodes
    let instructions: Vec<Instruction> = file.par_lines().map(|e| e.into()).collect();
    let instructions = optimize(&instructions);

    /*
       for op in &opcodes {
           println!("{} - {:?}", op.out, op.op);
       }
    */

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
                    let insts = insts.read().unwrap();
                    let val = interpret(&insts, *x, -*y);

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
