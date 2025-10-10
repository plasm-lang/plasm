#[derive(Debug, Clone)]
pub struct LinesTable {
    offsets: Vec<usize>,
}

impl LinesTable {
    pub fn new() -> Self {
        LinesTable { offsets: vec![0] }
    }

    pub fn offsets(&self) -> &[usize] {
        &self.offsets
    }

    pub fn len(&self) -> usize {
        self.offsets.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 1
    }

    pub fn add_line(&mut self, start_offset: usize) {
        self.offsets.push(start_offset);
    }

    pub fn last(&self) -> usize {
        self.offsets().last().copied().unwrap_or(0)
    }

    pub fn line_index(&self, offset: usize) -> usize {
        self.offsets
            .partition_point(|&start| start <= offset)
            .saturating_sub(1)
    }

    pub fn line(&self, offset: usize) -> usize {
        self.line_index(offset) + 1
    }

    pub fn column(&self, offset: usize) -> usize {
        let line_index = self.line_index(offset);
        offset - self.offsets[line_index] + 1
    }

    pub fn offset(&self, line: usize) -> Option<usize> {
        self.offsets.get(line.wrapping_sub(1)).copied()
    }
}

impl Default for LinesTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn table_from_text(s: &str) -> LinesTable {
        let mut t = LinesTable::new();
        for (i, b) in s.bytes().enumerate() {
            if b == b'\n' {
                t.add_line(i + 1);
            }
        }
        t
    }

    #[test]
    fn single_line_basic() {
        let t = table_from_text("hello");
        assert_eq!(t.offsets(), &[0]);

        assert_eq!(t.line_index(0), 0);
        assert_eq!(t.line(0), 1);

        assert_eq!(t.line_index(3), 0);
        assert_eq!(t.line(3), 1);

        assert_eq!(t.line_index(10), 0);
        assert_eq!(t.line(10), 1);
    }

    #[test]
    fn multi_line_boundaries() {
        let t = table_from_text("abc\nxy\nZ");
        assert_eq!(t.offsets(), &[0, 4, 7]);

        assert_eq!(t.line_index(0), 0);
        assert_eq!(t.line_index(2), 0);
        assert_eq!(t.line(2), 1);

        assert_eq!(t.line_index(3), 0);
        assert_eq!(t.line(3), 1);

        assert_eq!(t.line_index(4), 1);
        assert_eq!(t.line(4), 2);

        assert_eq!(t.line_index(5), 1);
        assert_eq!(t.line(5), 2);

        assert_eq!(t.line_index(6), 1);
        assert_eq!(t.line(6), 2);

        assert_eq!(t.line_index(7), 2);
        assert_eq!(t.line(7), 3);
        assert_eq!(t.line_index(100), 2);
        assert_eq!(t.line(100), 3);
    }

    #[test]
    fn manual_offsets() {
        let mut t = LinesTable::new();
        t.add_line(10);
        t.add_line(20);
        t.add_line(30);
        assert_eq!(t.offsets(), &[0, 10, 20, 30]);

        for off in [0usize, 1, 9] {
            assert_eq!(t.line_index(off), 0);
            assert_eq!(t.line(off), 1);
        }

        assert_eq!(t.line_index(10), 1);
        assert_eq!(t.line(10), 2);

        for off in [11usize, 19] {
            assert_eq!(t.line_index(off), 1);
            assert_eq!(t.line(off), 2);
        }

        assert_eq!(t.line_index(20), 2);
        assert_eq!(t.line(20), 3);
        assert_eq!(t.line_index(25), 2);
        assert_eq!(t.line(25), 3);

        assert_eq!(t.line_index(30), 3);
        assert_eq!(t.line(30), 4);
        assert_eq!(t.line_index(999), 3);
        assert_eq!(t.line(999), 4);
    }

    #[test]
    fn is_empty_semantics() {
        let t = LinesTable::new();

        assert!(t.is_empty());
        assert_eq!(t.len(), 1);

        assert_eq!(t.line_index(0), 0);
        assert_eq!(t.line(0), 1);
    }
}
