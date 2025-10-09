use std::fs::File;
use std::io::BufReader;
use std::path::PathBuf;

use ast::parse;
use diagnostic::{ErrorMessage, ErrorType, LinesTable, Spanned};
use hir::ast_to_hir;
use tokenizer::{CharIndicesIter, tokenize};

use super::printer::Printer;
use crate::{EnableAsni, Format, PathType, Stage};

pub fn emit(path: PathBuf, ty: PathType, format: Format, stage: Stage, ansi: EnableAsni) {
    let mut printer = Printer::new(ansi);

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
                Stage::AST => {
                    let (ast, errors) = parse(&mut token_iter);
                    if !errors.is_empty() {
                        print_errors(
                            errors,
                            path,
                            token_iter.lines_table_ref().clone(),
                            format,
                            &mut printer,
                        );
                        return;
                    }

                    match format {
                        Format::Json => {
                            let json = serde_json::to_string_pretty(&ast).unwrap();
                            println!("{json}");
                        }
                        Format::Text => {
                            println!("{ast}");
                        }
                    }
                }
                Stage::HIR => {
                    let (ast, parsing_errors) = parse(&mut token_iter);
                    if !parsing_errors.is_empty() {
                        print_errors(
                            parsing_errors,
                            path.clone(),
                            token_iter.lines_table_ref().clone(),
                            format,
                            &mut printer,
                        );
                        return;
                    }

                    let (hir, translation_errors) = ast_to_hir(ast);
                    if !translation_errors.is_empty() {
                        print_errors(
                            translation_errors,
                            path,
                            token_iter.lines_table_ref().clone(),
                            format,
                            &mut printer,
                        );
                        return;
                    }

                    match format {
                        Format::Json => {
                            let json = serde_json::to_string_pretty(&hir).unwrap();
                            println!("{json}");
                        }
                        Format::Text => {
                            println!("{hir}");
                        }
                    }
                }
                _ => unimplemented!(),
            }
        }
        _ => unimplemented!(),
    }
}

fn print_errors<E>(
    errors: Vec<Spanned<E>>,
    path: PathBuf,
    lines_table: LinesTable,
    format: Format,
    printer: &mut Printer,
) where
    E: std::error::Error + std::fmt::Display + ErrorType,
{
    match format {
        Format::Json => {
            unimplemented!()
        }
        Format::Text => {
            for error in errors {
                let error_message = ErrorMessage::new(error, lines_table.clone(), path.clone());
                if printer.error(error_message).is_err() {
                    eprintln!("Failed to read {}", path.display());
                    continue;
                };
            }
        }
    }
}
