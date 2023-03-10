mod error;
mod display;

use colored::Colorize;
use error::REPLError;
use lang::error::ParseError;
use display::ColoredExpr;
use rustyline::{error::ReadlineError, Editor};

const HISTORY_PATH: &str = ".flow_history";

fn to_readline_error(e: ReadlineError) -> REPLError {
    match e {
        ReadlineError::Interrupted => REPLError::ReadlineError("CTRL-C".to_string()),
        ReadlineError::Eof => REPLError::ReadlineError("CTRL-D".to_string()),
        err => REPLError::ReadlineError(err.to_string()),
    }
}

pub fn read(rl: &mut Editor<()>) -> Result<String, REPLError> {
    let prompt = format!("{} ", "flow>".bright_black());
    let input = rl.readline(&prompt).map_err(to_readline_error)?;

    Ok(input)
}

fn main() {
    let config = rustyline::Config::builder()
        .auto_add_history(true)
        .color_mode(rustyline::ColorMode::Enabled)
        .build();

    let mut rl = rustyline::Editor::<()>::with_config(config).unwrap();

    rl.load_history(HISTORY_PATH).unwrap_or_default();

    loop {
        let result = read(&mut rl).and_then(|input| lang::parsing::parse(&input).map_err(ParseError::into));
        match result {
            Ok(expr) => println!("{}", ColoredExpr::new(expr)),
            Err(err) => {
                eprintln!("{}", err);
                match err {
                    REPLError::ReadlineError(_) => {
                        break;
                    }
                    _ => {}
                }
            }
        }
    }

    rl.save_history(HISTORY_PATH).unwrap();
}
