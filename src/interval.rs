use std::ops::{Add, Div, Mul, Neg, Sub};

use crate::{instruction::Instruction, opcode::{OpCode, Value}};

#[derive(Clone, Copy, Debug)]
struct Interval {
    start: f32,
    end: f32,
}

impl Add for Interval {
    type Output = Interval;

    fn add(self, rhs: Self) -> Self::Output {
        Interval {
            start: self.start + rhs.start,
            end: self.end + rhs.end,
        }
    }
}

impl Sub for Interval {
    type Output = Interval;

    fn sub(self, rhs: Self) -> Self::Output {
        Interval {
            start: self.start - rhs.end,
            end: self.end - rhs.start,
        }
    }
}

impl Mul for Interval {
    type Output = Interval;

    fn mul(self, rhs: Self) -> Self::Output {
        let ss = self.start * rhs.start;
        let se = self.start * rhs.end;
        let es = self.end * rhs.start;
        let ee = self.end * rhs.end;

        Interval {
            start: f32::min(f32::min(ss, se), f32::min(es, ee)),
            end: f32::max(f32::max(ss, se), f32::max(es, ee)),
        }
    }
}

impl Div for Interval {
    type Output = Interval;

    fn div(self, rhs: Self) -> Self::Output {
        self.mul(Interval {
            start: rhs.end.recip(),
            end: rhs.start.recip(),
        })
    }
}

impl Interval {
    pub fn sqrt(self) -> Self {
        Interval {
            start: self.start.sqrt(),
            end: self.end.sqrt(),
        }
    }

    pub fn min(self, rhs: Self) -> Self {
        Interval {
            start: f32::min(self.start, rhs.start),
            end: f32::min(self.end, rhs.end),
        }
    }

    pub fn max(self, rhs: Self) -> Self {
        Interval {
            start: f32::max(self.start, rhs.start),
            end: f32::max(self.end, rhs.end),
        }
    }

    pub fn neg(self) -> Self {
        Interval { start: self.start.neg(), end: self.end.neg() }
    }
}


pub fn interpret_interal(insts: &[Instruction], x: Interval, y: Interval) -> Interval {
    use OpCode::*;

    let mut map: Vec<Interval> = Vec::with_capacity(insts.len());

    fn extract(map: &[Interval], key: Value) -> Interval {
        match key {
            Value::Address(addr) => map[addr],
            Value::Literal(lit) => Interval { start: lit, end: lit },
        }
    }

    for (i, Instruction { out, op }) in insts.iter().enumerate() {
        let value = match *op {
            VarY => y,
            VarX => x,
            Add(k1, k2) => extract(&map, k1) + extract(&map, k2),
            Sub(k1, k2) => extract(&map, k1) - extract(&map, k2),
            Mul(k1, k2) => extract(&map, k1) * extract(&map, k2),
            Neg(k) => extract(&map, k).neg(),
            Const(cnst) => Interval { start: cnst, end: cnst },
            Square(k) => {let val = extract(&map, k); val*val},
            Sqrt(k) => extract(&map, k).sqrt(),
            Max(k1, k2) => Interval::max(extract(&map, k1), extract(&map, k2)),
            Min(k1, k2) => Interval::min(extract(&map, k1), extract(&map, k2)),
        };

        map[*out] = value;
    }

    *map.last().unwrap()
}