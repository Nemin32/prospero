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
        Interval {
            start: self.start.neg(),
            end: self.end.neg(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum IntervalSign {
    Positive,
    Negative,
    Indeterminate,
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

#[derive(Clone, Debug)]
pub struct Quadtree {
    x: Interval,
    y: Interval,

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
            tl: None,
            tr: None,
            bl: None,
            br: None,
        }
    }

    pub fn split(&mut self) {
        let xhalf = (self.x.end - self.x.start) / 2.0;
        let yhalf = (self.y.end - self.y.start) / 2.0;

        let top_y = Interval {
            start: self.y.start,
            end: self.y.start + yhalf,
        };

        let bottom_y = Interval {
            start: self.y.start + yhalf,
            end: self.y.end,
        };

        let left_x = Interval {
            start: self.x.start,
            end: self.x.start + xhalf,
        };

        let right_x = Interval {
            start: self.x.start + xhalf,
            end: self.x.end,
        };

        self.tl = Some(Box::new(Quadtree::new(left_x, top_y)));
        self.tr = Some(Box::new(Quadtree::new(right_x, top_y)));
        self.bl = Some(Box::new(Quadtree::new(left_x, bottom_y)));
        self.br = Some(Box::new(Quadtree::new(right_x, bottom_y)));
    }

    pub fn get_sign(&self, insts: &[Instruction]) -> IntervalSign {
        interpret_interal(insts, self.x, self.y).into()
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

pub fn interpret_interal(insts: &[Instruction], x: Interval, y: Interval) -> Interval {
    use OpCode::*;

    let mut map: Vec<Interval> = Vec::with_capacity(insts.len());

    fn extract(map: &[Interval], key: Value) -> Interval {
        match key {
            Value::Address(addr) => map[addr],
            Value::Literal(lit) => Interval {
                start: lit,
                end: lit,
            },
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
            Const(cnst) => Interval {
                start: cnst,
                end: cnst,
            },
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
