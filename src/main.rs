use std::{io::{self, Write}, fs};
use clap::Parser;

mod compiler;

#[cfg(test)]
mod tests;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Arguments {
    #[arg()]
    input: String,
    #[arg(short, long)]
    output: Option<String>,
}

fn main() {
    let args = Arguments::parse();
    
    let code = std::fs::read_to_string(&args.input).unwrap();

    let spirv = compiler::Compiler::compile(&code);

    let mut w = {
        let f = fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(args.output.unwrap_or(args.input.to_string() + ".spv"))
            .unwrap();
        io::BufWriter::new(f)
    };
    let bytes = spirv.to_words()
        .iter()
        .flat_map(|x| {
            x.to_le_bytes()
        })
        .collect::<Vec<_>>();

    w.write_all(&bytes).unwrap();
}
