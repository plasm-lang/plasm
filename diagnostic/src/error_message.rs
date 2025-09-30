use std::fmt::{Display, Formatter, Result};
use std::io::{Read, Seek, SeekFrom};
use std::path::PathBuf;

use super::lines_table::LinesTable;
use super::span::Spanned;

const LINES_BEFORE: usize = 5;

pub struct ErrorMessage<E: std::error::Error + Display> {
    error: Spanned<E>,
    lines_table: LinesTable,
    file_path: PathBuf,
    code_slice: String,
    start_line_num: usize,
}

impl<E: std::error::Error + Display> ErrorMessage<E> {
    pub fn new(
        error: Spanned<E>,
        lines_table: LinesTable,
        file_path: PathBuf,
    ) -> std::io::Result<Self> {
        let (code_slice, start_line_num) = {
            let mut file = std::fs::File::open(&file_path)?;

            let err_line = lines_table.line(error.span.start);
            let start_offset = lines_table.offset(err_line - LINES_BEFORE).unwrap_or(0);
            let fallback_end_offset = file.metadata()?.len() as usize;
            let end_offset = lines_table
                .offset(err_line + 1)
                .unwrap_or(fallback_end_offset);
            let len = end_offset - start_offset;

            file.seek(SeekFrom::Start(start_offset as u64))?;
            let mut buf = vec![0u8; len];
            file.read_exact(&mut buf)?;
            let code = String::from_utf8(buf)
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;
            (code, lines_table.line(start_offset))
        };

        Ok(Self {
            error,
            lines_table,
            file_path,
            code_slice,
            start_line_num,
        })
    }
}

impl<E: std::error::Error + Display> Display for ErrorMessage<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "Error: {}", self.error)?;
        writeln!(
            f,
            "-----> {}:{}:{}",
            self.file_path.display(),
            self.lines_table.line(self.error.span.start),
            self.lines_table.column(self.error.span.start),
        )?;

        // Print the code slice with line numbers
        let num_width = (self.start_line_num + LINES_BEFORE + 1).to_string().len();
        let code_with_lines = self
            .code_slice
            .lines()
            .enumerate()
            .map(|(i, line)| {
                format!(
                    "{:>width$} | {}",
                    i + self.start_line_num,
                    line,
                    width = num_width
                )
            })
            .collect::<Vec<_>>()
            .join("\n");
        writeln!(f, "{code_with_lines}")?;

        // Underline the error span
        let col = self.lines_table.column(self.error.span.start);
        let len = (self.error.span.end - self.error.span.start).max(1);
        writeln!(
            f,
            "{:>width$} {:>col$}/{:^<len$}\\",
            "",
            "",
            "",
            width = num_width,
            col = col,
            len = len,
        )
    }
}
