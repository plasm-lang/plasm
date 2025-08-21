#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
}

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

    pub fn add_line(&mut self, start_offset: usize) {
        self.offsets.push(start_offset);
    }

    pub fn last(&self) -> usize {
        self.offsets().last().copied().unwrap_or(0)
    }
}
