mod error;

use colored::Colorize;
use error::REPLError;
use lang::{
    evaluation::{context::ValueEnvironment, error::EvalError, value::Value, Evaluator},
    parsing::{error::ParseError, parse_module},
};
use rustyline::{error::ReadlineError, Editor};

const HISTORY_PATH: &str = ".flow_history";

struct REPLState {
    rl: Editor<()>,
    pub env: ValueEnvironment,
}

impl REPLState {
    pub fn new() -> Self {
        let config = rustyline::Config::builder()
            .auto_add_history(true)
            .color_mode(rustyline::ColorMode::Enabled)
            .build();

        let rl = rustyline::Editor::<()>::with_config(config).unwrap();

        let env = ValueEnvironment::new_with_std();

        Self { rl, env }
    }

    pub fn readline(&mut self, prompt: &str) -> Result<String, ReadlineError> {
        self.rl.readline(prompt)
    }

    pub fn load_history(&mut self) -> Result<(), ReadlineError> {
        self.rl.load_history(HISTORY_PATH)
    }

    pub fn save_history(&mut self) -> Result<(), ReadlineError> {
        self.rl.save_history(HISTORY_PATH)
    }
}

fn read_parse_eval(state: &mut REPLState) -> Result<Value, REPLError> {
    let input = state
        .readline(&"flow> ".green())
        .map_err(ReadlineError::into)?;

    let expr = parse_module(&input).map_err(ParseError::into)?;

    let (value, next_env) = expr.eval(state.env.clone()).map_err(EvalError::into)?;

    state.env = next_env;

    Ok(value)
}

fn main() {
    let mut state = REPLState::new();
    state.load_history().unwrap_or_default();

    loop {
        let result = read_parse_eval(&mut state);

        match result {
            Ok(value) => {
                println!("{}", value);
            }
            Err(err) => match err {
                REPLError::ReadlineError(_) => {
                    break;
                }
                _ => {
                    eprintln!("{}", err);
                }
            },
        }
    }

    state.save_history().unwrap();
}
