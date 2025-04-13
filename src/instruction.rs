use std::fmt::Display;

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

type Mapping = Vec<Option<usize>>;

fn get_or_add_ssa(mapping: &mut Mapping, idx: usize) -> usize {
    let index_pos = mapping
        .iter()
        .position(|elem| matches!(elem, Some(inner) if *inner == idx));

    if let Some(inner) = index_pos {
        return inner;
    }

    let empty_pos = mapping.iter().position(|elem| elem.is_none());

    if let Some(inner) = empty_pos {
        mapping[inner] = Some(idx);
        return inner;
    }

    mapping.push(Some(idx));
    mapping.len() - 1
}

fn clean_registers(mapping: &mut Mapping, current_index: usize, before_binding: bool) {
    for elem in mapping.iter_mut() {
        if let Some(idx) = elem {
            if before_binding {
                if *idx > current_index {
                    *elem = None;
                }
            } else if *idx >= current_index {
                *elem = None;
            }
        }
    }
}

pub fn generate_register_mapping(insts: &[Instruction]) -> (Vec<Instruction>, usize) {
    let mut mapping: Mapping = vec![Some(insts.len())];
    let new_insts: Vec<Instruction> = insts.iter().rev().map(|elem| {
        clean_registers(&mut mapping, elem.out, true);

        let new_out = get_or_add_ssa(&mut mapping, elem.out);

        clean_registers(&mut mapping, elem.out, false);

        let mut add_register = |value: Value| -> Value {
            match value {
                Value::Address(addr) => {
                    Value::Address(get_or_add_ssa(&mut mapping, addr))
                },
                lit => lit
            }
        };

        use OpCode::*;
        let new_op = match elem.op {
            OpCode::Add(value, value1) => Add(add_register(value), add_register(value1)),
            OpCode::Sub(value, value1) => Sub(add_register(value), add_register(value1)),
            OpCode::Mul(value, value1) => Mul(add_register(value), add_register(value1)),
            OpCode::Neg(value) => Neg(add_register(value)),
            OpCode::Square(value) => Square(add_register(value)),
            OpCode::Sqrt(value) => Sqrt(add_register(value)),
            OpCode::Max(value, value1) => Max(add_register(value), add_register(value1)),
            OpCode::Min(value, value1) => Min(add_register(value), add_register(value1)),
            op => op,
        };

        Instruction { out: new_out, op: new_op }
    }).collect();

    (new_insts.into_iter().rev().collect(), mapping.len())
}
