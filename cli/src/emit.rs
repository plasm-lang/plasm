use std::fs::File;
use std::io::BufReader;
use std::path::PathBuf;

use ast::parse;
use codegen::{mir_to_asm_string, mir_to_llvm_ir_string};
use diagnostic::{ErrorMessage, ErrorType, LinesTable, Spanned};
use hir::ast_to_hir;
use mir::hir_to_mir;
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

            let (ast, parsing_errors) = parse(&mut token_iter);

            if !parsing_errors.is_empty() {
                print_errors(
                    parsing_errors,
                    path,
                    token_iter.lines_table_ref().clone(),
                    format,
                    &mut printer,
                );
                return;
            }

            if stage == Stage::AST {
                print_data(&ast, format);
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

            if stage == Stage::HIR {
                print_data(&hir, format);
                return;
            }

            let mir = hir_to_mir(hir);

            if stage == Stage::MIR {
                print_data(&mir, format);
                return;
            }

            if stage == Stage::LlvmIR {
                let llvm_ir = mir_to_llvm_ir_string(mir);
                print_data(&llvm_ir, format);
                return;
            }

            if stage == Stage::Asm {
                let asm = mir_to_asm_string(mir);
                print_data(&asm, format);
                return;
            }

            unimplemented!()
        }
        _ => unimplemented!(),
    }
}

fn print_data<T: ?Sized + std::fmt::Display + serde::Serialize>(data: &T, fmt: Format) {
    let string = match fmt {
        Format::Json => serde_json::to_string_pretty(data).unwrap(),
        Format::Text => data.to_string(),
    };
    println!("{}", string);
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
