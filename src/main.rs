use std::{env, fs, process::exit};
use novo::scanner::{lexer::lex, position::Located};
extern crate novo;

fn main() {
    let mut args = env::args().skip(1);
    if let Some(path) = args.next() {
        let text = fs::read_to_string(&path).map_err(|err| {
            eprintln!("ERROR: {err}");
            exit(1);
        }).unwrap();
        let tokens = lex(&text).map_err(|Located { value: err, pos }| {
            eprintln!("ERROR {path}:{}:{}: {err}", pos.ln.start + 1, pos.col.start + 1);
            exit(1);
        }).unwrap();
        dbg!(tokens);
    } else {
        eprintln!("{}", USAGE);
        exit(1);
    }
}

pub const USAGE: &str = r#"USAGE:
    novi <input.no> [FLAGS] - run input file
    FLAGS:
        -t, -tokens    display generated tokens
        -a, -ast       display generated AST
        -c, -code      display generated code
"#;