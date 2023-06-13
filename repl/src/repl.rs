use colored::Colorize;
use lang::evaluation::ValueEnvironment;
use lang::parsing::{parse_program, ParseError};
use rustyline::completion::Completer;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::history::DefaultHistory;
use rustyline::validate::{ValidationContext, ValidationResult, Validator};
use rustyline::{Context, Editor, Helper};
use std::borrow::Cow;
use std::borrow::Cow::Owned;

#[derive(Default)]
pub struct REPLHelper();

impl Validator for REPLHelper {
    fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        let result = parse_program(ctx.input());

        match result {
            Ok(_) => Ok(ValidationResult::Valid(None)),
            Err(e) => match e {
                ParseError::NotFullyParsed(_) => Ok(ValidationResult::Incomplete),
                ParseError::Failed(e) => Ok(ValidationResult::Invalid(Some(e))),
            },
        }
    }
}

impl Helper for REPLHelper {}

impl Highlighter for REPLHelper {
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        let _ = pos;
        Owned(line.bright_green().to_string())
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        let _ = (line, pos);
        true
    }
}

impl Hinter for REPLHelper {
    type Hint = String;

    fn hint(&self, line: &str, pos: usize, ctx: &Context<'_>) -> Option<Self::Hint> {
        let _ = (line, pos, ctx);
        None
    }
}

impl Completer for REPLHelper {
    type Candidate = String;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        ctx: &Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        let _ = (line, pos, ctx);
        Ok((0, vec![]))
    }
}

pub struct REPLState {
    pub rl: Editor<REPLHelper, DefaultHistory>,
    pub env: ValueEnvironment,
}

impl REPLState {
    pub fn new() -> Self {
        let config = rustyline::Config::builder()
            .auto_add_history(true)
            .color_mode(rustyline::ColorMode::Enabled)
            .build();

        let mut rl = Editor::with_config(config).unwrap();
        rl.set_helper(Some(REPLHelper::default()));

        let env = ValueEnvironment::default().import_std();

        Self { rl, env }
    }
}