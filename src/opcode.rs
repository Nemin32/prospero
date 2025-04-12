use std::fmt::Display;

use crate::instruction::Instruction;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Address(usize),
    Literal(f32),
}

impl Value {
    pub fn extract_literal(self, instructions: &[Instruction]) -> Value {
        use Value::*;

        if let Address(addr) = self {
            match instructions[addr].op {
                OpCode::Const(c) => Literal(c),
                _ => self,
            }
        } else {
            self
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Address(addr) => write!(f, "${}", addr),
            Self::Literal(lit) => write!(f, "{}", lit),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
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

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            OpCode::VarY => String::from("VARIABLE_Y"),
            OpCode::VarX => String::from("VARIABLE_X"),
            OpCode::Add(value, value1) => format!("ADD {} {}", value, value1),
            OpCode::Sub(value, value1) => format!("SUB {} {}", value, value1),
            OpCode::Mul(value, value1) => format!("MUL {} {}", value, value1),
            OpCode::Neg(value) => format!("NEGATE {}", value),
            OpCode::Const(cnst) => format!("CONSTANT {}", cnst),
            OpCode::Square(value) =>format!("SQUARE {}", value), 
            OpCode::Sqrt(value) => format!("SQROOT {}", value),
            OpCode::Max(value, value1) => format!("MAX {} {}", value, value1),
            OpCode::Min(value, value1) => format!("MIN {} {}", value, value1),
            OpCode::FuseMultiplyAdd(value, value1, value2) => format!("FMA {} {} {}", value, value1, value2),
        };

        write!(f, "{}", text)
    }
}