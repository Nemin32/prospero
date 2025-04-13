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