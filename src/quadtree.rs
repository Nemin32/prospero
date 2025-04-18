use crate::{
    instruction::Instruction,
    interval::{Interval, IntervalSign, interpret_interval},
};

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

    pub fn split(&mut self, insts: &[Instruction], len: usize) {
        if self.sign.is_none() {
            self.sign = Some(self.get_sign(insts, len));
        }

        // If we're already certain of this quadrant's sign, no need to split.
        if self.sign != Some(IntervalSign::Indeterminate) {
            return;
        }

        if let Quadtree {
            x: _,
            y: _,
            sign: _,
            tl: Some(tl),
            tr: Some(tr),
            bl: Some(bl),
            br: Some(br),
        } = self
        {
            tl.split(insts, len);
            tr.split(insts, len);
            bl.split(insts, len);
            br.split(insts, len);
        } else {
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

    pub fn get_sign(&self, insts: &[Instruction], len: usize) -> IntervalSign {
        self.sign
            .unwrap_or_else(|| interpret_interval(insts, len, self.x, self.y).into())
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

    #[allow(dead_code)]
    pub fn draw_borders(&self, max_inteval: f32, buffer: &mut Vec<Vec<IntervalSign>>) {
        if let Quadtree {
            x: _,
            y: _,
            sign: _,
            tl: Some(tl),
            tr: Some(tr),
            bl: Some(bl),
            br: Some(br),
        } = self
        {
            tl.draw_borders(max_inteval, buffer);
            tr.draw_borders(max_inteval, buffer);
            bl.draw_borders(max_inteval, buffer);
            br.draw_borders(max_inteval, buffer);
        } else {
            let rect = self.rectangle_size(max_inteval, buffer.len());
            let len = buffer.len() - 1;

            for row in &mut buffer[rect.y.max(0)..rect.height.min(len)] {
                row[rect.x.min(len)] = IntervalSign::Border;
                row[rect.width.min(len)] = IntervalSign::Border;
            }

            for x in rect.x.max(0)..rect.width.min(len) {
                buffer[rect.y.min(len)][x] = IntervalSign::Border;
                buffer[rect.height.min(len)][x] = IntervalSign::Border;
            }
        }
    }

    pub fn blit(
        &self,
        insts: &[Instruction],
        len: usize,
        max_inteval: f32,
        buffer: &mut Vec<Vec<IntervalSign>>,
    ) {
        if let Quadtree {
            x: _,
            y: _,
            sign: _,
            tl: Some(tl),
            tr: Some(tr),
            bl: Some(bl),
            br: Some(br),
        } = self
        {
            tl.blit(insts, len, max_inteval, buffer);
            tr.blit(insts, len, max_inteval, buffer);
            bl.blit(insts, len, max_inteval, buffer);
            br.blit(insts, len, max_inteval, buffer);
        } else {
            let sign = self.get_sign(insts, len);
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
