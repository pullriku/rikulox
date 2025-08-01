use crate::{bitset::BitSet, list::EntryRef};

pub trait Trace {
    fn trace(&self, tracer: &mut Tracer<Self>)
    where
        Self: Sized;
}

#[derive(Debug, Clone)]
pub struct Tracer<T: Trace> {
    pub(crate) worklist: Vec<EntryRef<T>>,
    pub(crate) marked_objects: BitSet,
}

impl<T: Trace> Tracer<T> {
    pub fn new() -> Self {
        Self {
            worklist: Vec::new(),
            marked_objects: BitSet::with_capacity(0),
        }
    }

    pub fn visit(&mut self, entry_ref: EntryRef<T>) {
        if self.marked_objects.contains(entry_ref.as_usize()) {
            return;
        }
        self.marked_objects.insert(entry_ref.as_usize());

        self.worklist.push(entry_ref);
    }

    pub fn clear(&mut self) {
        self.worklist.clear();
        self.marked_objects.clear();
    }
}

impl<T: Trace> Default for Tracer<T> {
    fn default() -> Self {
        Self::new()
    }
}
