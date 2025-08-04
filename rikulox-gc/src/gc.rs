use std::marker;

use crate::{
    list::{EntryRef, FreeList},
    trace::{Trace, Tracer},
};

pub struct Gc<T: Trace, F, I>
where
    F: Fn() -> I,
    I: Iterator<Item = EntryRef<T>>,
{
    heap: Heap<T>,
    alloc_count: u32,
    gc_threshold: u32,
    get_roots: F,
}

impl<T: Trace, F, I> Gc<T, F, I>
where
    F: Fn() -> I,
    I: Iterator<Item = EntryRef<T>>,
{
    pub fn new(gc_threshold: u32, get_roots: F) -> Self {
        Self {
            heap: Heap::new(),
            alloc_count: 0,
            gc_threshold,
            get_roots,
        }
    }

    pub fn alloc(&mut self, value: T) -> EntryRef<T> {
        self.alloc_count += 1;

        if self.alloc_count == self.gc_threshold {
            self.alloc_count = 0;
            let roots = (self.get_roots)();
            self.heap.mark(roots);
            self.heap.sweep();
        }

        self.heap.alloc(value)
    }
}

#[derive(Debug, Clone)]
pub struct Heap<T: Trace> {
    list: FreeList<T>,
    tracer: Tracer<T>,
}

impl<T: Trace> Heap<T> {
    pub fn new() -> Self {
        Self {
            list: FreeList::new(),
            tracer: Tracer::<T>::new(),
        }
    }

    pub fn alloc(&mut self, value: T) -> EntryRef<T> {
        self.list.alloc(value)
    }

    pub fn mark(&mut self, roots: impl Iterator<Item = EntryRef<T>>) {
        for root in roots {
            self.tracer.visit(root);
        }

        while let Some(entry_ref) = self.tracer.worklist.pop() {
            if let Some(value) = self.list.get(entry_ref) {
                value.trace(&mut self.tracer);
            }
        }
    }

    pub fn sweep(&mut self) {
        let set = &self.tracer.marked_objects;

        for i in 0..self.list.capacity() {
            if !set.contains(i) {
                self.list.dealloc(EntryRef {
                    index: i,
                    _phantom: marker::PhantomData,
                });
            }
        }

        self.tracer.clear();
    }

    pub fn get(&self, entry_ref: EntryRef<T>) -> Option<&T> {
        self.list.get(entry_ref)
    }

    pub fn get_mut(&mut self, entry_ref: EntryRef<T>) -> Option<&mut T> {
        self.list.get_mut(entry_ref)
    }
}

impl<T: Trace> Default for Heap<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::trace::{Trace, Tracer};

    #[derive(Debug, PartialEq)]
    struct Dummy(i32);

    impl Trace for Dummy {
        fn trace(&self, _tracer: &mut Tracer<Dummy>) {}
    }

    #[test]
    fn new_and_default_are_empty() {
        let h1: Heap<Dummy> = Heap::new();
        let h2: Heap<Dummy> = Heap::default();
        assert_eq!(h1.list.capacity(), 0);
        assert_eq!(h1.list.occupied_len(), 0);
        assert_eq!(h2.list.capacity(), 0);
        assert_eq!(h2.list.occupied_len(), 0);
    }

    #[test]
    fn alloc_increases_capacity_and_len() {
        let mut h: Heap<Dummy> = Heap::new();
        let r = h.alloc(Dummy(7));
        assert!(h.list.capacity() >= 1);
        assert_eq!(h.list.occupied_len(), 1);
        assert_eq!(h.list.get(r).map(|d| d.0), Some(7));
    }

    #[test]
    fn mark_and_sweep_unreachable() {
        let mut h: Heap<Dummy> = Heap::new();
        let r1 = h.alloc(Dummy(1));
        let r2 = h.alloc(Dummy(2));
        assert_eq!(h.list.occupied_len(), 2);
        h.mark(std::iter::once(r1));
        h.sweep();
        assert_eq!(h.list.occupied_len(), 1);
        assert_eq!(h.list.get(r1).map(|d| d.0), Some(1));
        assert_eq!(h.list.get(r2), None);
    }

    #[test]
    fn sweep_with_no_roots_clears_all() {
        let mut h: Heap<Dummy> = Heap::new();
        let _ = h.alloc(Dummy(3));
        let _ = h.alloc(Dummy(4));
        assert_eq!(h.list.occupied_len(), 2);
        h.mark(std::iter::empty());
        h.sweep();
        assert_eq!(h.list.occupied_len(), 0);
    }
}
