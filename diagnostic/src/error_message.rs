use std::fmt::Display;
use std::io::{Read, Seek, SeekFrom};
use std::path::PathBuf;

use super::lines_table::LinesTable;
use super::span::Spanned;

pub struct ErrorMessage<E: std::error::Error + Display> {
    pub error: Spanned<E>,
    pub lines_table: LinesTable,
    pub file_path: PathBuf,
}

impl<E: std::error::Error + Display + ErrorType> ErrorMessage<E> {
    pub fn new(error: Spanned<E>, lines_table: LinesTable, file_path: PathBuf) -> Self {
        Self {
            error,
            lines_table,
            file_path,
        }
    }

    pub fn extract_code_snippet(&self, lines_before: usize) -> std::io::Result<(String, usize)> {
        let mut file = std::fs::File::open(&self.file_path)?;

        let err_line = self.lines_table.line(self.error.span.start);
        let start_offset = self
            .lines_table
            .offset(err_line.wrapping_sub(lines_before))
            .unwrap_or(0);
        let fallback_end_offset = file.metadata()?.len() as usize;
        let end_offset = self
            .lines_table
            .offset(err_line + 1)
            .unwrap_or(fallback_end_offset);
        let len = end_offset.wrapping_sub(start_offset);

        file.seek(SeekFrom::Start(start_offset as u64))?;
        let mut buf = vec![0u8; len];
        file.read_exact(&mut buf)?;
        let mut code = String::from_utf8(buf)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;

        if code.ends_with("\n") {
            code.pop();
        }

        Ok((code, self.lines_table.line(start_offset)))
    }
}

pub trait ErrorType {
    fn error_type(&self) -> &'static str;
    fn error_sub_type(&self) -> &'static str;
}
