#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub u32);

#[derive(Debug, Clone)]
pub  struct IdGen {
    next: u32,
}

impl IdGen {
    pub fn new() -> Self {
        Self { next: 0 }
    }

    pub fn next_id(&mut self) -> NodeId {
        let id = NodeId(self.next);
        self.next += 1;
        id
    }
}

impl Iterator for IdGen {
    type Item = NodeId;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next_id())
    }
}

impl Default for IdGen {
    fn default() -> Self {
        Self::new()
    }
}
