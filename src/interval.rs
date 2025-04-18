use std::{iter, ops::{Add, Div, Mul, Neg, Sub}};

use crate::{
    instruction::Instruction,
    opcode::{OpCode, Value},
};

#[derive(Clone, Copy, Debug)]
pub struct Interval {
    pub start: f32,
    pub end: f32,
}

impl Add for Interval {
    type Output = Interval;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.start + rhs.start, self.end + rhs.end)
    }
}

impl Sub for Interval {
    type Output = Interval;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(self.start - rhs.end, self.end - rhs.start)
    }
}

impl Mul for Interval {
    type Output = Interval;

    fn mul(self, rhs: Self) -> Self::Output {
        let ss = self.start * rhs.start;
        let se = self.start * rhs.end;
        let es = self.end * rhs.start;
        let ee = self.end * rhs.end;

        let start = f32::min(f32::min(ss, se), f32::min(es, ee));
        let end = f32::max(f32::max(ss, se), f32::max(es, ee));

        Self::new(start, end)
    }
}

impl Div for Interval {
    type Output = Interval;

    fn div(self, rhs: Self) -> Self::Output {
        self.mul(Self::new(rhs.end.recip(), rhs.start.recip()))
    }
}

impl Interval {
    const EMPTY: Self = Self {
        start: 0.0,
        end: 0.0,
    };

    pub fn new(start: f32, end: f32) -> Interval {
        assert!(start <= end, "{} - {}", start, end);
        Interval { start, end }
    }

    pub fn sqrt(self) -> Self {
        // Since we can't square root negatives, we assume the interval must be greater or equal to zero.
        let start = if self.start >= 0.0 {
            self.start.sqrt()
        } else {
            0.0
        };
        let end = if self.end >= 0.0 {
            self.end.sqrt()
        } else {
            0.0
        };

        Self::new(start, end)
    }

    pub fn min(self, rhs: Self) -> Self {
        Self::new(f32::min(self.start, rhs.start), f32::min(self.end, rhs.end))
    }

    pub fn max(self, rhs: Self) -> Self {
        Self::new(f32::max(self.start, rhs.start), f32::max(self.end, rhs.end))
    }

    pub fn neg(self) -> Self {
        Self::new(self.end.neg(), self.start.neg())
    }
}

impl From<f32> for Interval {
    fn from(value: f32) -> Self {
        Interval::new(value, value)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum IntervalSign {
    Positive,
    Negative,
    Indeterminate,
    Border,
}

impl From<Interval> for IntervalSign {
    fn from(val: Interval) -> Self {
        if val.start >= 0.0 {
            return IntervalSign::Positive;
        }

        if val.end < 0.0 {
            return IntervalSign::Negative;
        }

        IntervalSign::Indeterminate
    }
}

impl From<f32> for IntervalSign {
    fn from(value: f32) -> Self {
        if value.is_sign_positive() {
            return IntervalSign::Positive;
        }

        if value.is_sign_negative() {
            return IntervalSign::Negative;
        }

        IntervalSign::Indeterminate
    }
}

pub fn interpret_interval(insts: &[Instruction], len: usize, x: Interval, y: Interval) -> Interval {
    use OpCode::*;

    let mut map: Vec<Interval> = iter::repeat(Interval::EMPTY).take(len).collect();

    fn extract(map: &[Interval], key: Value) -> Interval {
        match key {
            Value::Address(addr) => map[addr],
            Value::Literal(lit) => lit.into(),
        }
    }

    for Instruction { out, op } in insts {
        let value = match *op {
            VarY => y,
            VarX => x,
            Add(k1, k2) => extract(&map, k1) + extract(&map, k2),
            Sub(k1, k2) => extract(&map, k1) - extract(&map, k2),
            Mul(k1, k2) => extract(&map, k1) * extract(&map, k2),
            Neg(k) => extract(&map, k).neg(),
            Const(cnst) => cnst.into(),
            Square(k) => {
                let val = extract(&map, k);
                val * val
            }
            Sqrt(k) => extract(&map, k).sqrt(),
            Max(k1, k2) => Interval::max(extract(&map, k1), extract(&map, k2)),
            Min(k1, k2) => Interval::min(extract(&map, k1), extract(&map, k2)),
        };

        map[*out] = value;
    }

    *map.first().expect("Interval interp is empty.")
}
