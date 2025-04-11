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