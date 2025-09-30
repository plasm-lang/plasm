use std::fs::File;
use std::io::BufReader;
use std::path::PathBuf;

use ast::ASTParser;
use diagnostic::ErrorMessage;
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
            let mut token_iter = tokenize(char_iter);

            match stage {
                Stage::Ast => {
                    let (ast, errors) = {
                        let parser = ASTParser::new(&mut token_iter);
                        parser.parse()
                    };

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
                        match format {
                            Format::Json => {
                                unimplemented!()
                            }
                            Format::Text => {
                                for error in errors {
                                    let Ok(error_message) = ErrorMessage::new(
                                        error,
                                        token_iter.lines_table_ref().clone(),
                                        path.clone(),
                                    ) else {
                                        eprintln!("Failed to read {}", path.display());
                                        continue;
                                    };
                                    eprintln!("{error_message}");
                                }
                            }
                        }
                    }
                }
                _ => unimplemented!(),
            }
        }
        _ => unimplemented!(),
    }
}
