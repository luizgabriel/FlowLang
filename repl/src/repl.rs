use rustyline::Editor;
use lang::evaluation::env::ValueEnvironment;

pub struct REPLState {
    pub rl: Editor<()>,
    pub env: ValueEnvironment,
}

impl REPLState {
    pub fn new() -> Self {
        let config = rustyline::Config::builder()
            .auto_add_history(true)
            .color_mode(rustyline::ColorMode::Enabled)
            .build();

        let rl = Editor::<()>::with_config(config).unwrap();

        let env = ValueEnvironment::new().import_std();

        Self { rl, env }
    }
}