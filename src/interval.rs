use std::ops::{Add, Div, Mul, Sub};

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
}
