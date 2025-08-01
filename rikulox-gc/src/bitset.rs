#[derive(Debug, Clone)]
pub struct BitSet {
    data: Vec<u64>,
}

impl BitSet {
    pub fn new() -> Self {
        Self::with_capacity(0)
    }

    pub fn with_capacity(n_bits: usize) -> Self {
        let vec_len = n_bits.div_ceil(64);
        BitSet {
            data: vec![0; vec_len],
        }
    }

    pub fn insert(&mut self, pos: usize) {
        let idx = pos / 64;
        let off = pos % 64;
        if idx >= self.data.len() {
            self.data.resize(idx + 1, 0);
        }
        self.data[idx] |= 1 << off;
    }

    pub fn remove(&mut self, pos: usize) {
        let idx = pos / 64;
        let off = pos % 64;
        if idx < self.data.len() {
            self.data[idx] &= !(1 << off);
        }
    }

    pub fn contains(&self, pos: usize) -> bool {
        let idx = pos / 64;
        let off = pos % 64;
        if idx < self.data.len() {
            (self.data[idx] >> off) & 1 == 1
        } else {
            false
        }
    }

    pub fn clear(&mut self) {
        self.data.clear();
    }
}

impl Default for BitSet {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_is_empty() {
        let bs = BitSet::new();

        assert!(!bs.contains(0));
        assert!(!bs.contains(10));
        assert!(!bs.contains(1000));
    }

    #[test]
    fn test_with_capacity() {
        let bs = BitSet::with_capacity(130);
        assert!(bs.data.len() >= 3);
        assert!(!bs.contains(129));
        assert!(!bs.contains(usize::MAX));
    }

    #[test]
    fn test_insert_and_contains() {
        let mut bs = BitSet::new();
        bs.insert(0);
        bs.insert(63);
        bs.insert(64);
        bs.insert(130);

        assert!(bs.contains(0));
        assert!(bs.contains(63));
        assert!(bs.contains(64));
        assert!(bs.contains(130));

        assert!(!bs.contains(1));
        assert!(!bs.contains(129));
    }

    #[test]
    fn test_remove() {
        let mut bs = BitSet::new();
        bs.insert(5);
        assert!(bs.contains(5));
        bs.remove(5);
        assert!(!bs.contains(5));

        bs.remove(1000);
    }

    #[test]
    fn test_resize_on_insert() {
        let mut bs = BitSet::with_capacity(10);
        let old_len = bs.data.len();
        assert_eq!(old_len, 1);

        bs.insert(200);
        assert!(bs.contains(200));
        assert!(bs.data.len() > old_len);
    }

    #[test]
    fn test_clear() {
        let mut bs = BitSet::new();
        bs.insert(1);
        bs.insert(100);
        assert!(bs.contains(1));
        assert!(bs.contains(100));
        bs.clear();

        assert!(!bs.contains(1));
        assert!(!bs.contains(100));
        assert_eq!(bs.data.len(), 0);
    }

    #[test]
    fn test_default() {
        let bs1 = BitSet::default();
        let bs2 = BitSet::new();

        assert_eq!(bs1.data.len(), bs2.data.len());
        assert_eq!(bs1.data, bs2.data);
    }
}
