mod lexer;
mod ast;
mod parser;

use std::fs;
use std::process::Command;
use std::env;
use std::path::Path;
use std::path::PathBuf;
use std::time::SystemTime;

fn find_it(exe_name: String) -> Option<PathBuf> {
    let final_exe_name = if cfg!(target_os = "windows") {
        exe_name + ".exe"
    }
    else {
        exe_name
    };

    env::var_os("PATH").and_then(|paths| {
        env::split_paths(&paths).filter_map(|dir| {
            let full_path = dir.join(&final_exe_name);
            if full_path.is_file() {
                Some(full_path)
            } else {
                None
            }
        }).next()
    })
}

fn main() {

    let lex: lexer::Lexer = lexer::Lexer::new(fs::read_to_string("test.rs").expect("Cannot open test.rs"));

    let mut par: parser::Parser = parser::Parser::new(lex);

    let final_codegen = par.start();

    fs::write("output.rs", final_codegen).expect("Unable to write file");

    let get_lld_linker = if let Some(path) = find_it("lld".to_string()) {
        path.into_os_string().into_string().unwrap()
    }
    else {
        "".to_string()
    };

    let mut arguments: Vec<String> = Vec::new();

    if get_lld_linker != "" {
        arguments.push("-C".to_string());
        arguments.push("link-arg=-fuse-ld=lld".to_string());

        println!("Compiling with \"lld\"...");
    }
    else {
        println!("Compiling with default linker...");
    }

    arguments.push("output.rs".to_string());

    let now = SystemTime::now();

    let comp_output = Command::new("rustc-clif").args(arguments).output().unwrap();

    // No color :(
    println!("{}", String::from_utf8_lossy(&comp_output.stderr));

    println!("Compilation finished in: {}s", now.elapsed().unwrap().as_secs_f64());
}
