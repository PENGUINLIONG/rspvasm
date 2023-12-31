use anyhow::{anyhow, bail, Result};
use clap::parser::IdsRef;
use num_traits::ToBytes;
use std::{collections::HashMap, fmt::Write, io::BufWriter, rc::Rc};

#[derive(Debug, Clone)]
struct Object {
    attrs: HashMap<String, Atom>,
}

fn bytes_to_words(bytes: &[u8], words: &mut Vec<u32>) {
    let words_ = bytes.chunks(4).map(|chunk| {
        let mut word = 0;
        for (i, byte) in chunk.iter().enumerate() {
            word |= (*byte as u32) << (i * 8);
        }
        word
    });

    words.extend(words_);
}

type IdRef = u32;

#[derive(Debug, Clone)]
enum Atom {
    None,
    Bool(bool),
    Int(i32),
    Float(f32),
    String(String),
    IdRef(IdRef),
    Object(Object),
}
impl Atom {
    pub fn push_to_words(&self, words: &mut Vec<u32>) -> Result<()> {
        let bytes = match self {
            Atom::None => bail!("cannot push None to words"),
            Atom::Bool(x) => bail!("cannot push Bool to words"),
            Atom::Int(x) => {
                let bytes = x.to_ne_bytes();
                bytes_to_words(&bytes, words);
            }
            Atom::Float(x) => {
                let bytes = x.to_ne_bytes();
                bytes_to_words(&bytes, words);
            }
            Atom::String(x) => {
                let mut bytes = x.as_bytes().to_vec();
                bytes.push(0);
                bytes_to_words(&bytes, words);
            }
            Atom::IdRef(x) => {
                words.push(*x);
            }
            Atom::Object(x) => bail!("cannot push Object to words"),
        };

        Ok(())
    }

    pub fn to_idref(&self) -> Result<u32> {
        match self {
            Atom::Int(x) => Ok(*x as u32),
            _ => bail!("cannot convert {:?} to idref", self),
        }
    }
    pub fn to_i32(&self) -> Result<i32> {
        match self {
            Atom::Int(x) => Ok(*x),
            _ => bail!("cannot convert {:?} to i32", self),
        }
    }
    pub fn to_f32(&self) -> Result<f32> {
        match self {
            Atom::Float(x) => Ok(*x),
            _ => bail!("cannot convert {:?} to f32", self),
        }
    }
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
    Atom {
        atom: Atom,
    },
    Print {
        args: Vec<Func>,
    },
    Emit {
        op: Box<Func>,
        operands: Vec<Func>,
        has_result_id: bool,
        result_type: Option<Box<Func>>,
    },
    Store {
        name: String,
        value: Box<Func>,
    },
    Load {
        name: String,
    },
    Binary {
        binary_op: BinaryOp,
        arg0: Box<Func>,
        arg1: Box<Func>,
    },
    /*
    Unary {
        unary_op: UnaryOp,
        arg0: Box<Func>,
    },
    Binary {
        binary_op: BinaryOp,
        arg0: Box<Func>,
        arg1: Box<Func>,
    },
    Jump {
        label: String,
    },
    JumpConditional {
        label: String,
        cond: Box<Func>,
    },
    Label {
        label: String,
    },
    */
}

#[derive(Debug, Clone)]
struct Instr {
    op: u16,
    result_id: Option<u32>,
    result_type: Option<u32>,
    operands: Vec<u32>,
}

#[derive(Debug, Clone)]
pub struct IdContext {
    counter: u32,
}
impl IdContext {
    pub fn new() -> Self {
        Self { counter: 1 }
    }

    pub fn alloc(&mut self) -> IdRef {
        let idref = self.counter;
        self.counter += 1;
        idref
    }
}

struct Interpreter<'a, W: Write> {
    //root_obj: Object,
    stack: Vec<Vec<Atom>>,
    states: HashMap<String, Atom>,
    id_ctxt: IdContext,
    out_stream: &'a mut W,
    out_instrs: Vec<Instr>,
}
impl<'a, W: Write> Interpreter<'a, W> {
    pub fn new(out_stream: &'a mut W) -> Self {
        Interpreter {
            stack: vec![vec![]],
            states: HashMap::new(),
            id_ctxt: IdContext::new(),
            out_stream,
            out_instrs: Vec::new(),
        }
    }

    fn alloc_id(&mut self) -> u32 {
        self.id_ctxt.alloc()
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
            Func::Atom { atom } => atom.clone(),
            Func::Print { args } => {
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
                        Atom::IdRef(x) => {
                            writeln!(self.out_stream, "%{}", x)?;
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
            Func::Emit {
                op,
                operands,
                has_result_id,
                result_type,
            } => {
                let mut has_result_type = false;

                let depth = self.push();
                self.interpret_func(op)?;
                if let Some(result_type) = result_type {
                    self.interpret_func(result_type)?;
                    has_result_type = true;
                }
                for operand in operands.iter() {
                    self.interpret_func(operand)?;
                }
                let mut args = self.pop(depth);

                let mut args = args.drain(..);
                let op = {
                    let x = args.next().unwrap().to_i32()?;
                    assert!(x >= 0 && x <= u16::MAX as i32);
                    x as u16
                };
                let result_id = has_result_id.then(|| self.alloc_id());
                let result_type = has_result_type.then(|| args.next().unwrap().to_idref().unwrap());

                let mut operands = Vec::new();
                for arg in args {
                    arg.push_to_words(&mut operands)?;
                }

                let instr = Instr {
                    op,
                    result_id,
                    result_type,
                    operands,
                };

                self.out_instrs.push(instr);

                if let Some(result_id) = result_id {
                    Atom::IdRef(result_id)
                } else {
                    Atom::None
                }
            }
            Func::Store { name, value } => {
                let depth = self.push();
                self.interpret_func(value)?;
                let mut args = self.pop(depth);

                let value = args.remove(0);

                self.states.insert(name.clone(), value);

                Atom::None
            }
            Func::Load { name } => {
                let value = self
                    .states
                    .get(name)
                    .ok_or_else(|| anyhow!("name not found: {}", name))?
                    .clone();
                value
            }
            Func::Binary {
                binary_op,
                arg0,
                arg1,
            } => {
                let depth = self.push();
                self.interpret_func(arg0)?;
                self.interpret_func(arg1)?;
                let mut args = self.pop(depth);

                let arg0 = args.remove(0);
                let arg1 = args.remove(0);

                match binary_op {
                    BinaryOp::Add => match (&arg0, &arg1) {
                        (Atom::Int(x), Atom::Int(y)) => Atom::Int(x + y),
                        (Atom::Float(x), Atom::Float(y)) => Atom::Float(x + y),
                        _ => bail!("cannot add {:?} and {:?}", arg0, arg1),
                    },
                    _ => unimplemented!(),
                }
            }
        };

        self.stack.last_mut().unwrap().push(out);

        Ok(())
    }

    pub fn interpret_func_seq(&mut self, funcs: &[Func]) -> Result<()> {
        for func in funcs.iter() {
            self.interpret_func(func)?;
        }

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

        let func = Func::Print {
            args: vec![
                Func::Atom {
                    atom: Atom::Bool(true),
                },
                Func::Atom {
                    atom: Atom::Bool(false),
                },
                Func::Atom {
                    atom: Atom::Int(123),
                },
                Func::Atom {
                    atom: Atom::Float(1.5),
                },
                Func::Atom {
                    atom: Atom::String("abc".to_string()),
                },
            ],
        };

        let expect = r"
true
false
123
1.5
abc
"
        .trim_start();
        interp.interpret_func(&func).unwrap();
        assert_eq!(expect, log);
    }

    #[test]
    fn test_load_store() {
        let mut log = String::new();
        let mut interp = Interpreter::new(&mut log);

        let func = vec![
            Func::Store {
                name: "x".to_string(),
                value: Box::new(Func::Atom {
                    atom: Atom::Int(123),
                }),
            },
            Func::Print {
                args: vec![Func::Load {
                    name: "x".to_string(),
                }],
            },
        ];

        let expect = r"
123
"
        .trim_start();
        interp.interpret_func_seq(&func).unwrap();
        assert_eq!(expect, log);
    }
}
