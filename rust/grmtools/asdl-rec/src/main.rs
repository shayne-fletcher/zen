use clap::Parser;

use lrlex::lrlex_mod;
use lrpar::lrpar_mod;

lrlex_mod!("asdl.l");
lrpar_mod!("asdl.y");

#[derive(Debug, Parser)]
#[clap(
    name = "asdl_to_rust",
    about = r#"
Generate Rust datatypes for abstract syntax defined by ASDL.
"#
)]
struct Opts {
    src: std::path::PathBuf,
}

fn parse_opts() -> Result<Opts, std::io::Error> {
    let opts = Opts::parse();
    let src = &opts.src;
    std::fs::metadata(src)?;
    Ok(opts)
}

fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync + 'static>> {
    let opts = parse_opts()?;

    let lexerdef = asdl_l::lexerdef();
    let asdl = std::fs::read_to_string(&opts.src)?;
    let lexer = lexerdef.lexer(&asdl);
    let (res, errs) = asdl_y::parse(&lexer);
    for e in errs {
        println!("{}", e.pp(&lexer, &asdl_y::token_epp));
    }
    match res {
        Some(r) => println!("Result: {:#?}", r),
        _ => eprintln!("Unable to evaluate expression."),
    }
    Ok(())
}
