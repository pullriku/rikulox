use std::{fmt::Debug, marker};

#[derive(Debug, Clone)]
pub struct FreeList<T> {
    entries: Vec<Entry<T>>,
    next_free: Option<EntryRef<T>>,
    occupied_len: usize,
}

impl<T> FreeList<T> {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            next_free: None,
            occupied_len: 0,
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        let mut list = Self::new();
        list.reserve(capacity);
        list
    }

    fn reserve(&mut self, additional: usize) {
        self.entries.reserve(additional);

        while self.entries.len() < self.entries.capacity() {
            let last_index = self.entries.len();
            let next_free = self.next_free.replace(EntryRef {
                index: last_index,
                _phantom: marker::PhantomData,
            });
            self.entries.push(Entry::Free { next_free });
        }
    }

    fn grow(&mut self) {
        const MIN_CAPACITY: usize = 16;
        const GROW_FACTOR: usize = 2;
        let additional = (self.entries.len() * (GROW_FACTOR - 1)).max(MIN_CAPACITY);
        self.reserve(additional);
    }

    pub fn alloc(&mut self, value: T) -> EntryRef<T> {
        self.try_alloc(value).unwrap_or_else(|value| {
            self.grow();
            self.try_alloc(value).ok().unwrap()
        })
    }

    pub fn try_alloc(&mut self, value: T) -> Result<EntryRef<T>, T> {
        if let Some(entry_ref) = self.next_free {
            let next_free = match self.entries[entry_ref.index] {
                Entry::Free { next_free } => next_free,
                Entry::Occupied { .. } => unreachable!(),
            };
            self.next_free = next_free;
            self.entries[entry_ref.index] = Entry::Occupied(value);
            self.occupied_len += 1;
            Ok(entry_ref)
        } else {
            Err(value)
        }
    }

    pub fn occupied_len(&self) -> usize {
        self.occupied_len
    }

    pub fn capacity(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.occupied_len == 0
    }

    pub fn dealloc(&mut self, entry_ref: EntryRef<T>) {
        let entry = &mut self.entries[entry_ref.index];

        if matches!(entry, Entry::Occupied { .. }) {
            let next_free = self.next_free.replace(entry_ref);
            *entry = Entry::Free { next_free };
            self.occupied_len -= 1;
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (EntryRef<T>, &T)> + '_ {
        self.entries
            .iter()
            .enumerate()
            .filter_map(|(i, e)| match e {
                Entry::Occupied(value) => Some((
                    EntryRef {
                        index: i,
                        _phantom: marker::PhantomData,
                    },
                    value,
                )),
                Entry::Free { .. } => None,
            })
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (EntryRef<T>, &mut T)> + '_ {
        self.entries
            .iter_mut()
            .enumerate()
            .filter_map(|(i, e)| match e {
                Entry::Occupied(value) => Some((
                    EntryRef {
                        index: i,
                        _phantom: marker::PhantomData,
                    },
                    value,
                )),
                Entry::Free { .. } => None,
            })
    }

    pub fn get(&self, entry_ref: EntryRef<T>) -> Option<&T> {
        match &self.entries[entry_ref.index] {
            Entry::Occupied(value) => Some(value),
            Entry::Free { .. } => None,
        }
    }

    pub fn get_mut(&mut self, entry_ref: EntryRef<T>) -> Option<&mut T> {
        match &mut self.entries[entry_ref.index] {
            Entry::Occupied(value) => Some(value),
            Entry::Free { .. } => None,
        }
    }
}

impl<T> Default for FreeList<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub enum Entry<T> {
    Free { next_free: Option<EntryRef<T>> },
    Occupied(T),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct EntryRef<T>
where
    T: ?Sized,
{
    pub(crate) index: usize,
    pub(crate) _phantom: marker::PhantomData<*mut T>,
}

impl<T> EntryRef<T> {
    pub fn as_usize(&self) -> usize {
        self.index
    }
}

impl<T> Copy for EntryRef<T> {}
impl<T> Clone for EntryRef<T> {
    fn clone(&self) -> Self {
        *self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_is_empty() {
        let fl: FreeList<i32> = FreeList::new();
        assert_eq!(fl.capacity(), 0);
        assert_eq!(fl.occupied_len(), 0);
        assert!(fl.is_empty());
    }

    #[test]
    fn default_equals_new() {
        let fl1: FreeList<i32> = Default::default();
        let fl2: FreeList<i32> = FreeList::new();
        assert_eq!(fl1.capacity(), fl2.capacity());
        assert_eq!(fl1.occupied_len(), fl2.occupied_len());
    }

    #[test]
    fn with_capacity_sets_capacity() {
        let fl: FreeList<i32> = FreeList::with_capacity(10);
        assert_eq!(fl.capacity(), 10);
        assert_eq!(fl.occupied_len(), 0);
    }

    #[test]
    fn try_alloc_error() {
        let mut fl: FreeList<i32> = FreeList::new();
        assert_eq!(fl.try_alloc(5), Err(5));
    }

    #[test]
    fn grow_alloc_from_new() {
        let mut fl: FreeList<i32> = FreeList::new();
        let r = fl.alloc(42);
        assert_eq!(fl.capacity(), 16);
        assert_eq!(fl.occupied_len(), 1);
        assert_eq!(fl.get(r), Some(&42));
    }

    #[test]
    fn alloc_and_get() {
        let mut fl = FreeList::with_capacity(2);
        let r1 = fl.alloc(10);
        assert_eq!(fl.occupied_len(), 1);
        assert_eq!(fl.get(r1), Some(&10));
        let r2 = fl.alloc(20);
        assert_eq!(fl.occupied_len(), 2);
        assert_eq!(fl.get(r2), Some(&20));
    }

    #[test]
    fn dealloc_and_reuse() {
        let mut fl = FreeList::with_capacity(1);
        let r1 = fl.alloc(1);
        assert_eq!(fl.occupied_len(), 1);
        fl.dealloc(r1);
        assert_eq!(fl.occupied_len(), 0);
        let r2 = fl.alloc(2);
        assert_eq!(r2.as_usize(), r1.as_usize());
        assert_eq!(fl.get(r2), Some(&2));
    }

    #[test]
    fn iter_and_iter_mut() {
        let mut fl = FreeList::with_capacity(3);
        let _r1 = fl.alloc(1);
        let _r2 = fl.alloc(2);
        let vals: Vec<_> = fl.iter().map(|(_, v)| *v).collect();
        assert_eq!(vals, vec![2, 1]);
        for (_r, v) in fl.iter_mut() {
            *v *= 10;
        }

        for (r, _v) in fl.iter() {
            let val = fl.get(r).copied().unwrap();
            assert_eq!(val % 10, 0);
        }
    }

    #[test]
    fn get_mut_modifies() {
        let mut fl = FreeList::with_capacity(1);
        let r = fl.alloc(100);
        if let Some(v) = fl.get_mut(r) {
            *v = 200;
        }
        assert_eq!(fl.get(r), Some(&200));
    }
}
