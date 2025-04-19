use std::fmt::{Display, write};

pub fn intersection(first: String, second: String) -> String {
    format!("(min {first} {second})")
}

pub fn union(first: String, second: String) -> String {
    format!("(max {first} {second})")
}

pub fn difference(first: String, second: String) -> String {
    format!("(max {first} (- {second}))")
}

enum Var {
    X,
    Y,
}

impl Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::X => "x",
                Self::Y => "y",
            }
        )
    }
}

fn normalize_sub(var: Var, num: f32) -> String {
    // var - num
    if num == 0.0 {
        return var.to_string();
    }

    if num.is_sign_negative() {
        format!("(+ {var} {})", num.abs())
    } else {
        format!("(- {var} {num})")
    }
}

fn normalize_sub_r(num: f32, var: Var) -> String {
    // num - var
    if num == 0.0 {
        return format!("(- {var})");
    }

    format!(
        "(- {} {var})",
        if num.is_sign_negative() {
            format!("(- {})", num.abs())
        } else {
            num.to_string()
        }
    )
}

pub fn circle(x: f32, y: f32, rad: f32) -> String {
    let x_pos = {
        if x == 0.0 {
            "(square x)".to_string()
        } else if x.is_sign_positive() {
            format!("(square (- x {x}))")
        } else {
            format!("(square (+ x {}))", x.abs())
        }
    };

    let y_pos = {
        if y == 0.0 {
            "(square y)".to_string()
        } else if y.is_sign_positive() {
            format!("(square (- y {y}))")
        } else {
            format!("(square (+ y {}))", y.abs())
        }
    };

    format!("(- (+ {x_pos} {y_pos}) {rad})")
}

pub fn rectangle(x: f32, y: f32, w: f32, h: f32) -> String {
    let x_left = normalize_sub(Var::X, x);
    let x_right = normalize_sub_r(w + x, Var::X);

    let y_top = normalize_sub(Var::Y, y);
    let y_bottom = normalize_sub_r(h + y, Var::Y);

    intersection(intersection(x_left, x_right), intersection(y_top, y_bottom))
}
