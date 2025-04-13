use std::ops::{Add, Div, Mul, Neg, Sub};

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
        if value >= 0.0 {
            return IntervalSign::Positive;
        }

        if value < 0.0 {
            return IntervalSign::Negative;
        }

        IntervalSign::Indeterminate
    }
}

#[derive(Clone, Debug)]
pub struct Quadtree {
    x: Interval,
    y: Interval,
    sign: Option<IntervalSign>,

    tl: Option<Box<Quadtree>>,
    tr: Option<Box<Quadtree>>,
    bl: Option<Box<Quadtree>>,
    br: Option<Box<Quadtree>>,
}

#[derive(Clone, Copy, Debug)]
pub struct Rectangle {
    pub x: usize,
    pub y: usize,
    pub width: usize,
    pub height: usize,
}

impl Quadtree {
    pub fn new(x: Interval, y: Interval) -> Self {
        Quadtree {
            x,
            y,
            sign: None,
            tl: None,
            tr: None,
            bl: None,
            br: None,
        }
    }

    pub fn split(&mut self, insts: &[Instruction]) {
        if self.sign.is_none() {
            self.sign = Some(self.get_sign(insts));
        }

        // If we're already certain of this quadrant's sign, no need to split.
        if self.sign != Some(IntervalSign::Indeterminate) {
            return;
        }

        match self {
            Quadtree {
                x: _,
                y: _,
                sign: _,
                tl: Some(tl),
                tr: Some(tr),
                bl: Some(bl),
                br: Some(br),
            } => {
                tl.split(insts);
                tr.split(insts);
                bl.split(insts);
                br.split(insts);
            }
            _ => {
                let xhalf = (self.x.end - self.x.start) / 2.0;
                let yhalf = (self.y.end - self.y.start) / 2.0;

                let top_y = Interval::new(self.y.start, self.y.start + yhalf);
                let bottom_y = Interval::new(self.y.start + yhalf, self.y.end);
                let left_x = Interval::new(self.x.start, self.x.start + xhalf);
                let right_x = Interval::new(self.x.start + xhalf, self.x.end);

                self.tl = Some(Box::new(Quadtree::new(left_x, top_y)));
                self.tr = Some(Box::new(Quadtree::new(right_x, top_y)));
                self.bl = Some(Box::new(Quadtree::new(left_x, bottom_y)));
                self.br = Some(Box::new(Quadtree::new(right_x, bottom_y)));
            }
        }
    }

    pub fn get_sign(&self, insts: &[Instruction]) -> IntervalSign {
        self.sign
            .unwrap_or_else(|| interpret_interval(insts, self.x, self.y).into())
    }

    pub fn rectangle_size(&self, max_inteval: f32, pic_width: usize) -> Rectangle {
        let pic_width = pic_width as f32;
        let tmax = 2.0 * max_inteval;

        let x = (f32::max(0f32, (self.x.start + max_inteval) / tmax) * pic_width).floor() as usize;
        let y = (f32::max(0f32, (self.y.start + max_inteval) / tmax) * pic_width).floor() as usize;
        let width =
            (f32::max(0f32, (self.x.end + max_inteval) / tmax) * pic_width).floor() as usize;
        let height =
            (f32::max(0f32, (self.y.end + max_inteval) / tmax) * pic_width).floor() as usize;

        Rectangle {
            x,
            y,
            width,
            height,
        }
    }

    pub fn draw_borders(&self, max_inteval: f32, buffer: &mut Vec<Vec<IntervalSign>>) {
        match self {
            Quadtree {
                x: _,
                y: _,
                sign: _,
                tl: Some(tl),
                tr: Some(tr),
                bl: Some(bl),
                br: Some(br),
            } => {
                tl.draw_borders(max_inteval, buffer);
                tr.draw_borders(max_inteval, buffer);
                bl.draw_borders(max_inteval, buffer);
                br.draw_borders(max_inteval, buffer);
            }
            _ => {
                let rect = self.rectangle_size(max_inteval, buffer.len());
                let len = buffer.len() - 1;

                for y in rect.y.max(0)..rect.height.min(len) {
                    buffer[y][rect.x.min(len)] = IntervalSign::Border;
                    buffer[y][rect.width.min(len)] = IntervalSign::Border;
                }

                for x in rect.x.max(0)..rect.width.min(len) {
                    buffer[rect.y.min(len)][x] = IntervalSign::Border;
                    buffer[rect.height.min(len)][x] = IntervalSign::Border;
                }
            }
        }
    }

    pub fn blit(
        &self,
        insts: &[Instruction],
        max_inteval: f32,
        buffer: &mut Vec<Vec<IntervalSign>>,
    ) {
        match self {
            Quadtree {
                x: _,
                y: _,
                sign: _,
                tl: Some(tl),
                tr: Some(tr),
                bl: Some(bl),
                br: Some(br),
            } => {
                tl.blit(insts, max_inteval, buffer);
                tr.blit(insts, max_inteval, buffer);
                bl.blit(insts, max_inteval, buffer);
                br.blit(insts, max_inteval, buffer);
            }
            _ => {
                let sign = self.get_sign(insts);
                let rect = self.rectangle_size(max_inteval, buffer.len());

                buffer
                    .iter_mut()
                    .skip(rect.y)
                    .take(rect.height - rect.y)
                    .for_each(|elem| {
                        elem.iter_mut()
                            .skip(rect.x)
                            .take(rect.width - rect.x)
                            .for_each(|inner| *inner = sign);
                    });
            }
        }
    }
}

pub fn interpret_interval(insts: &[Instruction], x: Interval, y: Interval) -> Interval {
    use OpCode::*;

    let mut map: Vec<Interval> = Vec::with_capacity(insts.len());

    fn extract(map: &[Interval], key: Value) -> Interval {
        match key {
            Value::Address(addr) => map[addr],
            Value::Literal(lit) => lit.into(),
        }
    }

    for Instruction { out: _, op } in insts {
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

        map.push(value);
    }

    *map.last().unwrap()
}
