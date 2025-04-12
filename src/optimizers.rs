use crate::Instruction;
use crate::opcode::Value;
use crate::*;

fn inline_consts(instructions: &[Instruction]) -> impl Iterator<Item = Instruction> {
    use OpCode::*;

    instructions.iter().map(|inst| {
        let extract = |other: Value| -> Value { other.extract_literal(instructions) };
        let new_op = match inst.op {
            OpCode::VarY => VarY,
            OpCode::VarX => VarX,
            OpCode::Add(value, value1) => Add(extract(value), extract(value1)),
            OpCode::Sub(value, value1) => Sub(extract(value), extract(value1)),
            OpCode::Mul(value, value1) => Mul(extract(value), extract(value1)),
            OpCode::Neg(value) => Neg(extract(value)),
            OpCode::Const(val) => Const(val),
            OpCode::Square(value) => Square(extract(value)),
            OpCode::Sqrt(value) => Sqrt(extract(value)),
            OpCode::Max(value, value1) => Max(extract(value), extract(value1)),
            OpCode::Min(value, value1) => Min(extract(value), extract(value1)),
        };

        Instruction {
            out: inst.out,
            op: new_op,
        }
    })
}

pub fn optimize(instructions: &[Instruction]) -> Vec<Instruction> {
    use OpCode::*;
    use Value::*;

    let mut new_insts: Vec<Instruction> = Vec::with_capacity(instructions.len());

    let instructions = inline_consts(instructions).map(|inst| inst.inline_literal());

    let mut changed = false;

    for inst in instructions {
        let op: Option<OpCode> = match inst.op {
            Add(Address(addr), other) | Add(other, Address(addr)) => match new_insts[addr].op {
                Neg(v) => Some(Sub(other, v)),
                Const(c) => Some(Add(Literal(c), other)),
                _ => None,
            },
            Mul(Address(addr), other) | Mul(other, Address(addr)) => match new_insts[addr].op {
                Const(c) => Some(Mul(Literal(c), other)),
                _ => None,
            },
            Sub(Address(addr), other) => match new_insts[addr].op {
                Const(c) => Some(Sub(Literal(c), other)),
                _ => None,
            },
            Sub(other, Address(addr)) => match new_insts[addr].op {
                Const(c) => Some(Sub(other, Literal(c))),
                _ => None,
            },
            Square(Address(addr)) => match new_insts[addr].op {
                Sqrt(Literal(v)) => Some(Const(v)),
                _ => None,
            },
            Sqrt(Address(addr)) => match new_insts[addr].op {
                Square(Literal(v)) => Some(Const(v)),
                _ => None,
            },
            _ => None,
        };

        changed = changed || op.is_some();

        new_insts.push(Instruction {
            out: inst.out,
            op: op.unwrap_or(inst.op),
        })
    }

    if changed {
        println!("Doing next round.");
        optimize(&new_insts)
    } else {
        new_insts
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
        },
    }
}
