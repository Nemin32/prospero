use crate::Instruction;
use crate::opcode::Value;
use crate::*;

fn inline_consts(instructions: &[Instruction]) -> impl Iterator<Item = Instruction> {
    use OpCode::*;

    instructions.iter().map(|inst| {
        let extract = |other: Value| -> Value { other.extract_literal(instructions) };
        let new_op = match inst.op {
            VarY => VarY,
            VarX => VarX,
            Add(value, value1) => Add(extract(value), extract(value1)),
            Sub(value, value1) => Sub(extract(value), extract(value1)),
            Mul(value, value1) => Mul(extract(value), extract(value1)),
            Div(value, value1) => Div(extract(value), extract(value1)),
            Neg(value) => Neg(extract(value)),
            Const(val) => Const(val),
            Square(value) => Square(extract(value)),
            Sqrt(value) => Sqrt(extract(value)),
            Max(value, value1) => Max(extract(value), extract(value1)),
            Min(value, value1) => Min(extract(value), extract(value1)),
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
            Neg(Address(addr)) => match new_insts[addr].op {
                Neg(Literal(v)) => Some(Const(v)),
                _ => None,
            },
            Min(val1, Address(addr)) => match new_insts[addr].op {
                Min(val2, val3) if val1 == val2 => Some(Min(val1, val3)),
                _ => None,
            },
            Max(val1, Address(addr)) => match new_insts[addr].op {
                Max(val2, val3) if val1 == val2 => Some(Max(val1, val3)),
                _ => None,
            },
            Min(Literal(val1), Literal(val2)) | Max(Literal(val1), Literal(val2)) => {
                if val1 == val2 {
                    Some(Const(val1))
                } else {
                    None
                }
            }
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

pub fn prune(instructions: &[Instruction]) -> Vec<Instruction> {
    let len = instructions.len() - 1;

    let mut inst_map: Vec<(bool, &Instruction)> = instructions
        .iter()
        .map(|inst| (false, inst))
        .collect();

    inst_map[len].0 = true;

    for i in (0..=len).rev() {
        let valid = inst_map[i].0;
        let op = inst_map[i].1.op;

        if valid {
            use OpCode::*;

            match op {
                Add(value, value1)
                | Sub(value, value1)
                | Mul(value, value1)
                | Div(value, value1)
                | Min(value, value1)
                | Max(value, value1) => {
                    if let Value::Address(addr) = value {
                        inst_map[addr].0 = true;
                    }
                    if let Value::Address(addr1) = value1 {
                        inst_map[addr1].0 = true;
                    }
                }

                Neg(Value::Address(value))
                | Square(Value::Address(value))
                | Sqrt(Value::Address(value)) => {
                    inst_map[value].0 = true;
                }
                _ => {}
            };
        }
    }

    for (valid, inst) in &inst_map {
        println!("{} = {}", if *valid {"Y"} else {"N"}, inst);
    }

    let new_insts = inst_map
        .iter()
        .filter(|(b, _)| *b)
        .map(|(_, inst)| *inst.to_owned())
        .collect();

    new_insts
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
                "({} * {})",
                unroll(instructions, k1),
                unroll(instructions, k2)
            ),
            Div(k1, k2) => format!(
                "({} / {})",
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
