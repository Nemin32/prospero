use std::{fmt::Display, iter};

use crate::{opcode::Value, *};

#[derive(Debug, Clone, Copy)]
pub struct Instruction {
    pub out: usize,
    pub op: OpCode,
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
    pub fn inline_literal(self) -> Self {
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
                Max(Literal(v1), Literal(v2)) => Const(f32::max(v1, v2)),
                Min(Literal(v1), Literal(v2)) => Const(f32::min(v1, v2)),
                orig => orig,
            },
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.out, self.op)
    }
}

#[derive(Clone, Copy)]
pub struct Liveness {
    pub defined: usize,
    pub last_used: usize,
}

impl Liveness {
    pub fn is_live(&self, idx: usize) -> bool {
        idx > self.defined && idx <= self.last_used
    }
}

pub fn generate_liveness(insts: &[Instruction]) -> Vec<Liveness> {
    let len = insts.len();
    let mut last_used_list = iter::repeat(None).take(len).collect::<Vec<Option<usize>>>();

    insts.iter().rev().enumerate().for_each(|(i, elem)| {
        let mut push_value = |val: Value| {
            match val {
                Value::Address(addr) => {
                    if last_used_list[addr].is_none() {
                        last_used_list[addr] = Some(len - i - 1);
                    }
                }
                Value::Literal(_) => {}
            };
        };

        match elem.op {
            OpCode::Add(value, value1) => {
                push_value(value);
                push_value(value1);
            }
            OpCode::Sub(value, value1) => {
                push_value(value);
                push_value(value1);
            }
            OpCode::Mul(value, value1) => {
                push_value(value);
                push_value(value1);
            }
            OpCode::Square(value) => {
                push_value(value);
            }
            OpCode::Sqrt(value) => {
                push_value(value);
            }
            OpCode::Max(value, value1) => {
                push_value(value);
                push_value(value1);
            }
            OpCode::Min(value, value1) => {
                push_value(value);
                push_value(value1);
            }
            _ => {}
        };
    });

    last_used_list
        .iter()
        .enumerate()
        .map(|(i, elem)| Liveness {
            defined: i,
            last_used: elem.unwrap_or(i),
        })
        .collect()
}

type RegisterMapping = Option<(usize, Liveness)>;

fn get_or_insert_index(
    register_mapping: &mut Vec<RegisterMapping>,
    out: usize,
    expires: Liveness,
) -> usize {
    // Try to find the elem in the array...
    let in_array_pos = register_mapping
        .iter()
        .position(|elem| matches!(elem, Some((idx, _)) if *idx == out));

    // ...if found return it...
    if let Some(pos) = in_array_pos {
        return pos;
    }

    // ...otherwise try to find an empty slot...
    let empty_spot = register_mapping.iter().position(Option::is_none);

    match empty_spot {
        Some(index) => {
            // ...if found, put the current elem there.
            register_mapping[index] = Some((out, expires));
            index
        }
        None => {
            // ...otherwise add a new elem.
            register_mapping.push(Some((out, expires)));
            register_mapping.len() - 1
        }
    }
}

#[inline]
fn clean_registers(register_mapping: &mut [RegisterMapping], current_index: usize) {
    for elem in register_mapping.iter_mut() {
        if let Some((_, liveness)) = *elem {
            if !liveness.is_live(current_index) {
                *elem = None;
            }
        }
    }
}

pub fn generate_register_mapping(
    insts: &[Instruction],
    livenesses: &[Liveness],
) -> (Vec<Instruction>, usize) {
    // If these aren't the same, we messed up somewhere.
    assert!(insts.len() == livenesses.len());

    let len = insts.len();

    let mut register_mapping: Vec<RegisterMapping> = {
        let last = (insts[len - 1], livenesses[len - 1]);
        vec![Some((last.0.out, last.1))]
    };

    let new_insts: Vec<Instruction> = insts
        .iter()
        .enumerate()
        .rev()
        .map(|(i, inst)| {
            let liveness = livenesses[i];
            let new_out = get_or_insert_index(&mut register_mapping, inst.out, liveness);

            clean_registers(&mut register_mapping, i);

            let mut add_registers = |value: Value| -> Value {
                match value {
                    Value::Address(addr) => {
                        let addr_liveness = livenesses[addr];
                        let reg = get_or_insert_index(&mut register_mapping, addr, addr_liveness);

                        Value::Address(reg)
                    }
                    lit => lit,
                }
            };

            use OpCode::*;
            let new_op: OpCode = match inst.op {
                Add(value, value1) => Add(add_registers(value), add_registers(value1)),
                Sub(value, value1) => Sub(add_registers(value), add_registers(value1)),
                Mul(value, value1) => Mul(add_registers(value), add_registers(value1)),
                Neg(value) => Neg(add_registers(value)),
                Square(value) => Square(add_registers(value)),
                Sqrt(value) => Sqrt(add_registers(value)),
                Max(value, value1) => Max(add_registers(value), add_registers(value1)),
                Min(value, value1) => Min(add_registers(value), add_registers(value1)),
                other => other,
            };

            Instruction {
                out: new_out,
                op: new_op,
            }
        })
        .collect();

    (
        new_insts.into_iter().rev().collect(),
        register_mapping.len(),
    )
}
