#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn with_end_from(self, other: Span) -> Self {
        let mut result = self;
        result.end = other.end;
        result
    }

    pub fn empty_at(pos: usize) -> Self {
        Span {
            start: pos,
            end: pos,
        }
    }

    pub fn empty_from_start(span: Span) -> Self {
        Self::empty_at(span.start)
    }

    pub fn empty_from_end(span: Span) -> Self {
        Self::empty_at(span.end)
    }
}
