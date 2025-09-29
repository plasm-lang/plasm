use std::fs::File;
use std::io::BufReader;
use std::path::PathBuf;

use ast::ASTParser;
use tokenizer::{CharIndicesIter, tokenize};

use crate::{Format, PathType, Stage};

pub fn emit(path: PathBuf, ty: PathType, format: Format, stage: Stage) {
    match ty {
        PathType::File => {
            let Ok(file) = File::open(&path) else {
                eprintln!("Failed to open {}", path.display());
                return;
            };

            let reader = BufReader::new(file);
            let char_iter = CharIndicesIter::new(reader);
            let token_iter = tokenize(char_iter);

            match stage {
                Stage::Ast => {
                    let parser = ASTParser::new(token_iter);
                    let (ast, errors) = parser.parse();

                    if errors.is_empty() {
                        match format {
                            Format::Json => {
                                let json = serde_json::to_string_pretty(&ast).unwrap();
                                println!("{json}");
                            }
                            Format::Text => {
                                println!("{ast}");
                            }
                        }
                    } else {
                        unimplemented!();
                    }
                }
                _ => unimplemented!(),
            }
        }
        _ => unimplemented!(),
    }
}
