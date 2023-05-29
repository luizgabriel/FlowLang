mod error;
mod repl;
mod colors;

use colored::Colorize;
use error::REPLError;

use lang::evaluation::{Evaluator, Value};
use lang::parsing::parse_program;
use crate::colors::Colored;
use crate::repl::REPLState;

const HISTORY_PATH: &str = ".flow_history";

fn read_parse_eval(state: &mut REPLState) -> Result<Value, REPLError> {
    let input = state.rl
        .readline(&"flow> ".green())
        .map_err(REPLError::from)?;

    let expr = parse_program(&input).map_err(REPLError::from)?;

    let (value, next_env) = expr.eval(state.env.clone()).map_err(REPLError::from)?;

    state.env = next_env;

    Ok(value)
}

fn main() {
    let mut state = REPLState::new();
    state.rl.load_history(HISTORY_PATH).unwrap_or_default();

    loop {
        let result = read_parse_eval(&mut state);

        match result {
            Ok(value) => match value {
                    Value::Unit => {}
                    _ => {
                        println!("{}", Colored::new(value).to_string());
                    }
                }
            Err(err) => match err {
                REPLError::Readline(_) => {
                    break;
                }
                _ => {
                    eprintln!("{}", err);
                }
            },
        }
    }

    state.rl.save_history(HISTORY_PATH).unwrap();
}
