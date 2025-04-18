use std::{collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    instruction::Instruction,
    opcode::{OpCode, Value},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Lexeme {
    Eof,
    X,
    Y,
    Min,
    Max,
    Add,
    Mul,
    Div,
    Sqrt,
    Square,
    Neg,
    OParen,
    CParen,
    Number(f32),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Ast {
    Number(f32),
    X,
    Y,
    Add(Rc<Ast>, Rc<Ast>),
    Sub(Rc<Ast>, Rc<Ast>),
    Mul(Rc<Ast>, Rc<Ast>),
    Div(Rc<Ast>, Rc<Ast>),
    Min(Rc<Ast>, Rc<Ast>),
    Max(Rc<Ast>, Rc<Ast>),
    Sqrt(Rc<Ast>),
    Square(Rc<Ast>),
    Neg(Rc<Ast>),
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ast::X => write!(f, "X"),
            Ast::Y => write!(f, "Y"),
            Ast::Number(num) => write!(f, "{num}"),
            Ast::Add(ast, ast1) => write!(f, "(+ {ast} {ast1})"),
            Ast::Sub(ast, ast1) => write!(f, "(- {ast} {ast1})"),
            Ast::Mul(ast, ast1) => write!(f, "(* {ast} {ast1})"),
            Ast::Div(ast, ast1) => write!(f, "(/ {ast} {ast1})"),
            Ast::Min(ast, ast1) => write!(f, "(min {ast} {ast1})"),
            Ast::Max(ast, ast1) => write!(f, "(max {ast} {ast1})"),
            Ast::Sqrt(ast) => write!(f, "(sqrt {ast})"),
            Ast::Square(ast) => write!(f, "(square {ast})"),
            Ast::Neg(ast) => write!(f, "(- {ast})"),
        }
    }
}

#[inline(always)]
fn is_separator(char: &char) -> bool {
    *char == ' ' || *char == ')' || *char == '('
}

fn parse_lexeme(input: &[char], start: usize) -> (Lexeme, usize) {
    if start >= input.len() {
        return (Lexeme::Eof, start);
    }

    let mut start = start;
    while start < input.len() && input[start] == ' ' {
        start += 1;
    }

    use Lexeme::*;
    let one_char = match input[start] {
        'x' | 'X' => Some(X),
        'y' | 'Y' => Some(Y),
        '(' => Some(OParen),
        ')' => Some(CParen),
        '+' => Some(Add),
        '-' => Some(Neg),
        '*' => Some(Mul),
        '/' => Some(Div),
        _ => None,
    };

    if let Some(lex) = one_char {
        return (lex, start + 1);
    }

    let mut end = start;

    while end < input.len() && !is_separator(&input[end]) {
        end += 1;
    }

    let raw_lexeme = input[start..end].iter().collect::<String>();
    let lexeme = match raw_lexeme.as_str() {
        "min" => Min,
        "max" => Max,
        "sqrt" => Sqrt,
        "square" => Square,
        "" => Eof,
        other => Number(
            other
                .parse()
                .unwrap_or_else(|_| panic!("Expected to find float, got {}", other)),
        ),
    };

    (lexeme, end)
}

#[derive(Debug)]
pub enum ParseError {
    Eof,
    Unexpected,
}

fn expect(lexemes: &[Lexeme], index: usize, expected: Lexeme) -> Result<usize, ParseError> {
    if index >= lexemes.len() {
        Err(ParseError::Eof)
    } else if lexemes[index] == expected {
        Ok(index + 1)
    } else {
        Err(ParseError::Unexpected)
    }
}

fn get(lexemes: &[Lexeme], index: usize) -> Result<Lexeme, ParseError> {
    if index >= lexemes.len() {
        Err(ParseError::Eof)
    } else {
        Ok(lexemes[index])
    }
}

fn parse(lexemes: &[Lexeme], start: usize) -> Result<(Ast, usize), ParseError> {
    if start >= lexemes.len() {
        return Err(ParseError::Eof);
    }

    if let Lexeme::Number(num) = lexemes[start] {
        return Ok((Ast::Number(num), start + 1));
    } else if let Lexeme::X = lexemes[start] {
        return Ok((Ast::X, start + 1));
    } else if let Lexeme::Y = lexemes[start] {
        return Ok((Ast::Y, start + 1));
    }

    let index = expect(lexemes, start, Lexeme::OParen)?;
    let kind = get(lexemes, index)?;
    let index = index + 1;

    let (first, index) = parse(lexemes, index)?;

    let (ast, index): (Ast, usize) = match kind {
        Lexeme::Eof => Err(ParseError::Eof)?,
        Lexeme::Min => {
            let (second, index) = parse(lexemes, index)?;
            (Ast::Min(Rc::new(first), Rc::new(second)), index)
        }
        Lexeme::Max => {
            let (second, index) = parse(lexemes, index)?;
            (Ast::Max(Rc::new(first), Rc::new(second)), index)
        }
        Lexeme::Add => {
            let (second, index) = parse(lexemes, index)?;
            (Ast::Add(Rc::new(first), Rc::new(second)), index)
        }
        Lexeme::Mul => {
            let (second, index) = parse(lexemes, index)?;
            (Ast::Mul(Rc::new(first), Rc::new(second)), index)
        }
        Lexeme::Div => {
            let (second, index) = parse(lexemes, index)?;
            (Ast::Div(Rc::new(first), Rc::new(second)), index)
        }
        Lexeme::Sqrt => (Ast::Sqrt(Rc::new(first)), index),
        Lexeme::Square => (Ast::Square(Rc::new(first)), index),
        Lexeme::Neg => {
            if get(lexemes, index)? != Lexeme::CParen {
                let (second, index) = parse(lexemes, index)?;
                (Ast::Sub(Rc::new(first), Rc::new(second)), index)
            } else {
                (Ast::Neg(Rc::new(first)), index)
            }
        }
        _ => Err(ParseError::Unexpected)?,
    };

    let index = expect(lexemes, index, Lexeme::CParen)?;

    Ok((ast, index))
}

fn hash_ast(ast: &Rc<Ast>, hashmap: &mut HashMap<String, Rc<Ast>>) {
    let key = ast.to_string();
    if hashmap.get(&key).is_none() {
        let ast_clone = ast.clone();
        match ast_clone.as_ref() {
            Ast::Sub(ast, ast1)
            | Ast::Add(ast, ast1)
            | Ast::Mul(ast, ast1)
            | Ast::Div(ast, ast1)
            | Ast::Min(ast, ast1)
            | Ast::Max(ast, ast1) => {
                hash_ast(ast, hashmap);
                hash_ast(ast1, hashmap);
            }
            Ast::Sqrt(ast) | Ast::Square(ast) | Ast::Neg(ast) => {
                hash_ast(ast, hashmap);
            }
            _ => {}
        };

        hashmap.insert(key, ast.to_owned());
    }
}

fn emit(
    ast: &Ast,
    container: &mut Vec<Instruction>,
    hashmap: &mut HashMap<String, usize>,
) -> usize {
    let str = ast.to_string();
    if let Some(num) = hashmap.get(&str) {
        return *num;
    }

    let op: OpCode = match ast {
        Ast::Number(num) => OpCode::Const(*num),
        Ast::X => OpCode::VarX,
        Ast::Y => OpCode::VarY,
        Ast::Add(ast, ast1) => {
            let first = emit(ast.as_ref(), container, hashmap);
            let second = emit(ast1.as_ref(), container, hashmap);

            OpCode::Add(Value::Address(first), Value::Address(second))
        }
        Ast::Sub(ast, ast1) => {
            let first = emit(ast.as_ref(), container, hashmap);
            let second = emit(ast1.as_ref(), container, hashmap);

            OpCode::Sub(Value::Address(first), Value::Address(second))
        }
        Ast::Mul(ast, ast1) => {
            let first = emit(ast.as_ref(), container, hashmap);
            let second = emit(ast1.as_ref(), container, hashmap);

            OpCode::Mul(Value::Address(first), Value::Address(second))
        }
        Ast::Div(ast, ast1) => {
            let first = emit(ast.as_ref(), container, hashmap);
            let second = emit(ast1.as_ref(), container, hashmap);

            OpCode::Div(Value::Address(first), Value::Address(second))
        }
        Ast::Min(ast, ast1) => {
            let first = emit(ast.as_ref(), container, hashmap);
            let second = emit(ast1.as_ref(), container, hashmap);

            OpCode::Min(Value::Address(first), Value::Address(second))
        }
        Ast::Max(ast, ast1) => {
            let first = emit(ast.as_ref(), container, hashmap);
            let second = emit(ast1.as_ref(), container, hashmap);

            OpCode::Max(Value::Address(first), Value::Address(second))
        }

        Ast::Sqrt(ast) => {
            let index = emit(ast.as_ref(), container, hashmap);
            OpCode::Sqrt(Value::Address(index))
        }
        Ast::Square(ast) => {
            let index = emit(ast.as_ref(), container, hashmap);
            OpCode::Square(Value::Address(index))
        }
        Ast::Neg(ast) => {
            let index = emit(ast.as_ref(), container, hashmap);
            OpCode::Neg(Value::Address(index))
        }
        _ => panic!("Div is not handled"),
    };

    hashmap.insert(str, container.len());

    container.push(Instruction {
        out: container.len(),
        op,
    });

    container.len() - 1
}

fn get_or_replace(ast: &Ast, hashmap: &HashMap<String, Rc<Ast>>) -> Rc<Ast> {
    hashmap
        .get(&ast.to_string())
        .map(|e| e.to_owned())
        .unwrap_or_else(|| Rc::new(replace(ast, hashmap)))
}

fn replace(ast: &Ast, hashmap: &HashMap<String, Rc<Ast>>) -> Ast {
    match ast {
        Ast::Add(ast, ast1) => {
            let left = get_or_replace(ast, hashmap);
            let right = get_or_replace(ast1, hashmap);

            Ast::Add(left, right)
        }
        Ast::Sub(ast, ast1) => {
            let left = get_or_replace(ast, hashmap);
            let right = get_or_replace(ast1, hashmap);

            Ast::Sub(left, right)
        }
        Ast::Mul(ast, ast1) => {
            let left = get_or_replace(ast, hashmap);
            let right = get_or_replace(ast1, hashmap);

            Ast::Mul(left, right)
        }
        Ast::Div(ast, ast1) => {
            let left = get_or_replace(ast, hashmap);
            let right = get_or_replace(ast1, hashmap);

            Ast::Div(left, right)
        }
        Ast::Sqrt(ast) => Ast::Sqrt(get_or_replace(ast, hashmap)),
        Ast::Square(ast) => Ast::Square(get_or_replace(ast, hashmap)),
        Ast::Neg(ast) => Ast::Neg(get_or_replace(ast, hashmap)),
        ast => ast.clone(),
    }
}

pub fn generate_bytecode(input: String) -> Vec<Instruction> {
    let chars = input.chars().collect::<Vec<char>>();

    let mut lexemes = Vec::new();

    let mut start = 0;
    loop {
        let (lexeme, end) = parse_lexeme(&chars, start);
        start = end;

        if let Lexeme::Eof = lexeme {
            break;
        }

        lexemes.push(lexeme);
    }

    let result = parse(&lexemes, 0);

    let (ast, _) = result.expect("Expected Ast to be present");

    println!("{}", ast);
    let arc = Rc::new(ast);
    let mut map: HashMap<String, Rc<Ast>> = HashMap::new();

    hash_ast(&arc, &mut map);
    let new_ast = replace(&arc.clone(), &map);
    let mut instructions = Vec::new();
    emit(&new_ast, &mut instructions, &mut HashMap::new());

    instructions
}
