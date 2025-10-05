use std::path::{Path, PathBuf};

use clap::{
    Args, CommandFactory, Error, Parser, Subcommand, ValueEnum, ValueHint, error::ErrorKind,
};

mod emit;
mod printer;
mod theme;

use emit::emit;

const CONFIG_FILE_NAME: &str = "sky.toml";

/// Sky programming language compiler
#[derive(Parser, Debug)]
#[command(name = "sky", version, about)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Compile project or file
    Build,
    /// Compile project or file and run
    Run,
    /// Show intermediate artifacts
    Emit(EmitArgs),
    /// Check project or file for errors
    Check,
}

#[derive(Args, Debug)]
struct EmitArgs {
    /// Path to the file to emit. If empty, entire project will be analized.
    #[arg(value_name = "PATH", value_hint = ValueHint::AnyPath)]
    path: Option<PathBuf>,
    /// Stage to show
    #[arg(value_enum, long, default_value_t = Stage::AST)]
    stage: Stage,
    /// Output format
    #[arg(value_enum, long, default_value_t = Format::Text)]
    format: Format,
    /// Enable ANSI colors
    #[arg(value_enum, long, default_value_t = EnableAsni::Auto)]
    ansi: EnableAsni,
}

#[derive(Debug, Clone, ValueEnum)]
enum Stage {
    AST,
    HIR,
    MIR,
    LlvmIR,
    Asm,
    ObjectFIle,
    Binary,
}

#[derive(Debug, Clone, ValueEnum)]
enum Format {
    Json,
    Text,
}

#[derive(Debug, Clone, ValueEnum)]
pub enum EnableAsni {
    Auto,
    Always,
    Never,
}

#[derive(Debug, Clone)]
pub enum PathType {
    File,
    Dir,
    Project,
}

fn resolve_root(path: Option<&Path>) -> Option<(PathBuf, PathType)> {
    fn find_toml(mut p: &Path) -> Option<PathBuf> {
        loop {
            let candidate = p.join(CONFIG_FILE_NAME);
            if candidate.exists() {
                return Some(p.to_path_buf());
            }
            match p.parent() {
                Some(parent) => p = parent,
                None => return None,
            }
        }
    }

    match path {
        Some(p) if p.is_file() => Some((p.to_path_buf(), PathType::File)),
        Some(p) if p.is_dir() => Some((p.to_path_buf(), PathType::Dir)),
        _ => find_toml(std::env::current_dir().unwrap().as_path())
            .map(|path| (path, PathType::Project)),
    }
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Command::Emit(args) => {
            let Some((root, ty)) = resolve_root(args.path.as_deref()) else {
                if let Some(path) = args.path.as_deref() {
                    Error::raw(ErrorKind::Io, format!("Path {path:?} is Invalid\n"))
                        .with_cmd(&Cli::command())
                        .exit();
                } else {
                    Error::raw(ErrorKind::Io, format!("Can't find {CONFIG_FILE_NAME}\n"))
                        .with_cmd(&Cli::command())
                        .exit();
                }
            };
            emit(root, ty, args.format, args.stage, args.ansi);
        }
        _ => unimplemented!(),
    }
}
