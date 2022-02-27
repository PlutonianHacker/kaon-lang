use std::path::PathBuf;

use clap::{Arg, Command};
use termcolor::ColorChoice;

#[derive(Debug)]
pub enum Coloring {
    Auto,
    Always,
    Never,
}

pub struct Args {
    pub file: Option<PathBuf>,
    pub color: Coloring,
}

impl Default for Args {
    fn default() -> Self {
        Self::new()
    }
}

impl Args {
    pub fn new() -> Self {
        let version = &format!("v{}", env!("CARGO_PKG_VERSION"))[..];

        let app = Command::new("kaon-lang")
            .about("A little scripting language, written in Rust.")
            .version(version)
            .arg(
                Arg::new("FILE.kaon")
                    .help("Run the provided script.")
                    .index(1),
            )
            .arg(
                Arg::new("color")
                    .long("color")
                    .value_name("WHEN")
                    .possible_values(&["always", "auto", "never"])
                    .default_value("auto")
                    .overrides_with("color")
                    .min_values(0)
                    .require_equals(true)
                    .default_missing_value("always")
                    .help("Coloring: auto, always or never"),
            );

        let matches = app.get_matches();

        let file = matches.value_of("FILE.kaon").map(PathBuf::from);

        let color = match matches.value_of("color") {
            Some("always") => Coloring::Always,
            Some("auto") => Coloring::Auto,
            Some("never") => Coloring::Never,
            _ => Coloring::Always,
        };

        Self { file, color }
    }

    pub fn color_preference(&self) -> ColorChoice {
        match self.color {
            Coloring::Auto => ColorChoice::Auto,
            Coloring::Always => ColorChoice::Always,
            Coloring::Never => ColorChoice::Never,
        }
    }
}
