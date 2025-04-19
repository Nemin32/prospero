use std::collections::HashMap;

use crate::generators::{rectangle, union};

type RectDef = (u8, u8, u8, u8);

fn generate_alpahbet() -> HashMap<char, Vec<RectDef>> {
    let mut hm: HashMap<char, Vec<RectDef>> = HashMap::new();

    /*

    ##...##
    ###.###
    #######
    #######
    ##.#.##
    ##...##
    ##...##
    ##...##

    */

    hm.insert(
        'm',
        vec![
            (0, 0, 2, 8),
            (5, 0, 2, 8),
            (2, 2, 1, 3),
            (3, 3, 1, 3),
            (4, 2, 1, 3),
        ],
    );

    /*
    
    ######..
    #.##.#..
    ..##....
    ..##....
    ..##....
    ..##....
    .####...
    ........
    
    */

    hm.insert('t', vec![
        (0,0, 1,2),
        (1,0, 1,1),
        (2,0, 2,7),
        (4,0, 1,1),
        (5,0, 1,2),
        (1,6, 1,1),
        (4,6, 1,1)
    ]);

    /*
    
    ..###...
    .##.##..
    ##...##.
    ##...##.
    ##...##.
    ##...##.
    .##.##..
    ..###...

    */

    hm.insert('o', vec![
        (2, 0, 3, 1),
        (1, 1, 2, 1),
        (4, 1, 2, 1),
        (0, 2, 2, 4),
        (5, 2, 2, 4),
        (1, 6, 2, 1),
        (4, 6, 2, 1),
        (2, 7, 3, 1),
    ]);

    /*
    
    #####...
    .##.##..
    .##..##.
    .##..##.
    .##..##.
    .##.##..
    #####...
    ........
    
    */

    hm.insert('d', vec![
        (0,0, 5,1),
        (1,1, 2,5),
        (4,1, 2,1),
        (5,2, 2,3),
        (4,5, 2,1),
        (0,6, 5,1),
    ]);

    /*

    ..##....
    .####...
    .####...
    ..##....
    ..##....
    ........
    ..##....
    ........

    */

    hm.insert(
        '!',
        vec![(1, 1, 1, 2), (2, 0, 2, 5), (4, 1, 1, 2), (2, 6, 2, 1)],
    );

    hm
}

pub fn make_letter(rects: &[RectDef], sx: f32, sy: f32, scale: f32) -> String {
    const FACTOR: f32 = 8.0;
    let scale = scale / FACTOR;

    rects
        .iter()
        .cloned()
        .map(|(x, y, w, h)| {
            rectangle(
                (x as f32) * scale + sx,
                (FACTOR - (y+h) as f32) * scale + sy,
                w as f32 * scale,
                (h as f32) * scale,
            )
        })
        .reduce(union)
        .unwrap_or(String::new())
}

pub fn draw_text(text: String, sx: f32, sy: f32, scale: f32) -> String {
    let alphabet = generate_alpahbet();
    let mut letters = Vec::new();

    for (pos, char) in text.chars().enumerate() {
        let def = alphabet
            .get(&char)
            .unwrap_or(alphabet.get(&'!').expect("Need ! char."));

        let letter = make_letter(def, sx + (pos as f32 * scale), sy, scale);

        letters.push(letter);
    }

    letters.into_iter().reduce(union).expect("Expected text")
}
