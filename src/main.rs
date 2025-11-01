mod core_lang;

#[derive(Debug)]
pub enum CompilerError {
    ParseError(String),
    Other(String),
}

fn main() {
    let path = "";
    println!("Parsing {}...", path);

    match parse_core_file(path) {
        Ok(ast) => {
            println!("AST:");
            println!("{}", core_lang::show_program(ast));
        }
        Err(CompilerError::ParseError(msg)) => {
            println!("ParseError: {}", msg);
        }
        Err(e) => {
            println!("Other error: {:?}", e);
        }
    }
}