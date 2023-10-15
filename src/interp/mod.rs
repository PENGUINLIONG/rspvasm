use anyhow::{anyhow, bail, Result};
use std::{collections::HashMap, io::{BufWriter}, fmt::Write};

#[derive(Debug, Clone)]
struct Object {
    attrs: HashMap<String, Atom>,
}

#[derive(Debug, Clone)]
enum Atom {
    None,
    Bool(bool),
    Int(u32),
    Float(f32),
    String(String),
    Object(Object),
}

#[derive(Debug, Clone)]
enum UnaryOp {
    Neg,
    Not,
}
#[derive(Debug, Clone)]
enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone)]
enum Func {
    Constant(Atom),
    Print(Vec<Func>),
}

struct Interpreter<'a, W: Write> {
    //root_obj: Object,
    stack: Vec<Vec<Atom>>,
    out_stream: &'a mut W,
}
impl<'a, W: Write> Interpreter<'a, W> {
    pub fn new(out_stream: &'a mut W) -> Self {
        Interpreter {
            stack: vec![vec![]],
            out_stream,
        }
    }

    fn push(&mut self) -> usize {
        self.stack.push(Vec::new());
        self.stack.len()
    }
    fn pop(&mut self, depth: usize) -> Vec<Atom> {
        assert_eq!(depth, self.stack.len());
        self.stack.pop().unwrap()
    }

    pub fn interpret_func(&mut self, func: &Func) -> Result<()> {
        let out = match func {
            Func::Constant(x) => {
                x.clone()
            },
            Func::Print(args) => {
                let depth = self.push();
                for arg in args.iter() {
                    self.interpret_func(arg)?;
                }
                let args = self.pop(depth);

                for arg in args {
                    match arg {
                        Atom::None => {
                            writeln!(self.out_stream, "None")?;
                        }
                        Atom::Bool(x) => {
                            writeln!(self.out_stream, "{}", x)?;
                        }
                        Atom::Int(x) => {
                            writeln!(self.out_stream, "{}", x)?;
                        }
                        Atom::Float(x) => {
                            writeln!(self.out_stream, "{}", x)?;
                        }
                        Atom::String(x) => {
                            writeln!(self.out_stream, "{}", x)?;
                        }
                        Atom::Object(x) => {
                            self.out_stream.write_fmt(format_args!("{:?}", x))?;
                        }
                    }
                }

                Atom::None
            }
        };

        self.stack.last_mut().unwrap().push(out);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_interp_print() {
        let mut log = String::new();
        let mut interp = Interpreter::new(&mut log);

        let func = Func::Print(vec![
            Func::Constant(Atom::Bool(true)),
            Func::Constant(Atom::Bool(false)),
            Func::Constant(Atom::Int(123)),
            Func::Constant(Atom::Float(1.5)),
            Func::Constant(Atom::String("abc".to_string())),
        ]);

        let expect = r"
true
false
123
1.5
abc
".trim_start();
        interp.interpret_func(&func).unwrap();
        assert_eq!(expect, log);
    }
}
