use std::{
    io::{Result, Stderr, Stdout, Write, stderr, stdout},
    str::FromStr,
};

use anstream::AutoStream;
use anstyle::Reset;

use super::theme::Theme;
use crate::EnableAsni;

use diagnostic::{ErrorMessage, ErrorType};
use tokenizer::{Comment, Token, tokenize};
use utils::primitive_types::PrimitiveType;

const LINES_BEFORE: usize = 5;

pub struct Printer {
    theme: Theme,
    stdout: AutoStream<Stdout>,
    stderr: AutoStream<Stderr>,
}

impl Printer {
    pub fn new(enable_ansi: EnableAsni) -> Self {
        let stdout = match enable_ansi {
            EnableAsni::Auto => AutoStream::auto(stdout()),
            EnableAsni::Always => AutoStream::always(stdout()),
            EnableAsni::Never => AutoStream::never(stdout()),
        };
        let stderr = match enable_ansi {
            EnableAsni::Auto => AutoStream::auto(stderr()),
            EnableAsni::Always => AutoStream::always(stderr()),
            EnableAsni::Never => AutoStream::never(stderr()),
        };
        Self {
            theme: Theme::DRACULA,
            stdout,
            stderr,
        }
    }

    fn paint_code(&self, code: String) -> String {
        tokenize(code.char_indices())
            .map(|(token, _span)| match token {
                Token::Keyword(_) => {
                    format!("{}{}{}", self.theme.keyword.render(), token, Reset.render())
                }
                Token::Bracket(_) => {
                    format!("{}{}{}", self.theme.bracket.render(), token, Reset.render())
                }
                Token::Number(_) => {
                    format!("{}{}{}", self.theme.literal.render(), token, Reset.render())
                }
                Token::Identifier(ref s) => {
                    if let Ok(ty) = PrimitiveType::from_str(s) {
                        format!(
                            "{}{}{}",
                            self.theme.built_in_type.render(),
                            ty,
                            Reset.render()
                        )
                    } else {
                        format!("{}{}{}", self.theme.code.render(), token, Reset.render())
                    }
                }
                Token::Comment(Comment::SingleLine(_)) => {
                    format!("{}{}{}", self.theme.comment.render(), token, Reset.render())
                }
                Token::Comment(Comment::MultiLine(_)) => format!("{token}")
                    .split("\n")
                    .map(|line| {
                        format!("{}{}{}", self.theme.comment.render(), line, Reset.render())
                    })
                    .collect::<Vec<_>>()
                    .join("\n"),
                _ => format!("{}{}{}", self.theme.code.render(), token, Reset.render()),
            })
            .collect()
    }

    pub fn error<E: std::error::Error + std::fmt::Display + ErrorType>(
        &mut self,
        msg: ErrorMessage<E>,
    ) -> Result<()> {
        let (code_slice, start_line_num) = msg.extract_code_snippet(LINES_BEFORE)?;

        let code_slice = self.paint_code(code_slice);

        // Print the error type and message
        let err_ty = msg.error.error_type();
        writeln!(
            self.stderr,
            "{err_ty_color}{err_ty}{r}: {bold}{err_sub_ty}{r}: {msg}",
            err_ty = err_ty,
            err_ty_color = self.theme.error.bold().render(),
            err_sub_ty = msg.error.error_sub_type(),
            msg = msg.error,
            bold = anstyle::Style::new().bold().render(),
            r = Reset.render()
        )?;

        // Print the file path and line/column info
        let arrow_body = "-".repeat(err_ty.len() - 1);
        writeln!(
            self.stderr,
            "{arrow_body}>  {color}{}:{}:{}{r}",
            msg.file_path.display(),
            msg.lines_table.line(msg.error.span.start),
            msg.lines_table.column(msg.error.span.start),
            color = self.theme.path.render(),
            r = Reset.render()
        )?;

        // Print the code slice with line numbers
        let num_width = (start_line_num + LINES_BEFORE + 1).to_string().len();
        let code_with_lines = code_slice
            .lines()
            .enumerate()
            .map(|(i, line)| {
                format!(
                    "{color}{:>width$} |{r} {}",
                    i + start_line_num,
                    line,
                    width = num_width,
                    color = self.theme.line_number.render(),
                    r = Reset.render()
                )
            })
            .collect::<Vec<_>>()
            .join("\n");
        writeln!(self.stderr, "{code_with_lines}")?;

        // Underline the error span
        let col = msg.lines_table.column(msg.error.span.start);
        let len = (msg.error.span.end - msg.error.span.start).max(1);
        writeln!(
            self.stderr,
            "{:>width$} {:>col$}{color}/{:^<len$}\\{r}",
            "",
            "",
            "",
            width = num_width,
            col = col,
            len = len,
            color = self.theme.error.render(),
            r = Reset.render()
        )?;
        writeln!(self.stderr)
    }
}
