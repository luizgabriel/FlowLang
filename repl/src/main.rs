mod error;

use colored::Colorize;
use error::REPLError;
use lang::{
    evaluation::{EvalError, Evaluator, ValueEnvironment},
    parsing::{parse, ParseError},
};
use rustyline::{error::ReadlineError, Editor};

const HISTORY_PATH: &str = ".flow_history";

pub fn read(rl: &mut Editor<()>) -> Result<String, ReadlineError> {
    let prompt = format!("{} ", "flow>".bright_black());
    rl.readline(&prompt)
}

fn main() {
    let config = rustyline::Config::builder()
        .auto_add_history(true)
        .color_mode(rustyline::ColorMode::Enabled)
        .build();

    let mut rl = rustyline::Editor::<()>::with_config(config).unwrap();

    rl.load_history(HISTORY_PATH).unwrap_or_default();

    let mut env = ValueEnvironment::new_with_std();

    loop {
        let result = read(&mut rl)
            .map_err(ReadlineError::into)
            .and_then(|input| parse(&input).map_err(ParseError::into))
            .and_then(|expr| expr.eval(env.clone()).map_err(EvalError::into));

        match result {
            Ok((value, next_env)) => {
                println!("{}", value);
                env = next_env;
            }
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
