#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

impl Span {
    pub fn with_end_from(self, other: Span) -> Self {
        let mut result = self;
        result.end = other.end;
        result
    }
}
