use std::io::Write;
use std::{
    fs::{self, OpenOptions},
    ops::Neg,
    sync::{Arc, RwLock},
};

use instruction::Instruction;
use opcode::{OpCode, Value};
use rayon::prelude::*;

mod instruction;
mod opcode;
mod optimizers;

const RESOLUTION: u16 = 1024;
const DELTA: f32 = 1.0 / (RESOLUTION as f32);

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
    let file = fs::read_to_string("./test.vm").expect("File to be present.");

    // Parse opcodes
    let instructions: Vec<Instruction> = file.par_lines().map(|e| e.into()).collect();
    let instructions = optimizers::optimize(&instructions);

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
                    //let len = insts.len();
                    let val = interpret(&insts.read().unwrap(), *x, -*y);

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
