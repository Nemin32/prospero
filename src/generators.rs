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

pub fn union(first: String, second: String) -> String {
    format!("(max {first} {second})")
}

pub fn difference(first: String, second: String) -> String {
    format!("(max {first} (- {second}))")
}
