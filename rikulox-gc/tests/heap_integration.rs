use rikulox_gc::{
    gc::Heap,
    list::EntryRef,
    trace::{Trace, Tracer},
};

use std::{cell::Cell, rc::Rc};

#[derive(Debug)]
struct Node {
    #[allow(dead_code)]
    value: i32,
    next: Rc<Cell<Option<EntryRef<Node>>>>,
    drops: Rc<Cell<usize>>,
}

impl Trace for Node {
    fn trace(&self, tracer: &mut Tracer<Self>) {
        if let Some(next) = self.next.get() {
            tracer.visit(next);
        }
    }
}

impl Drop for Node {
    fn drop(&mut self) {
        self.drops.set(self.drops.get() + 1);
    }
}

#[test]
fn mark_sweep_reclaims_unreachable_and_reuses_slots() {
    let mut heap: Heap<Node> = Heap::new();
    let drops = Rc::new(Cell::new(0));
    let r1 = heap.alloc(Node {
        value: 1,
        next: Rc::new(Cell::new(None)),
        drops: drops.clone(),
    });
    let r2 = heap.alloc(Node {
        value: 2,
        next: Rc::new(Cell::new(None)),
        drops: drops.clone(),
    });
    let r3 = heap.alloc(Node {
        value: 3,
        next: Rc::new(Cell::new(Some(r2))),
        drops: drops.clone(),
    });

    heap.mark(std::iter::once(r3));
    heap.sweep();

    let r4 = heap.alloc(Node {
        value: 4,
        next: Rc::new(Cell::new(None)),
        drops: drops.clone(),
    });
    assert_eq!(r4.as_usize(), r1.as_usize());
    assert_ne!(r4.as_usize(), r2.as_usize());
    assert_ne!(r4.as_usize(), r3.as_usize());
}

#[test]
fn mark_with_multiple_roots_preserves_all() {
    let drops = Rc::new(Cell::new(0));
    let mut heap: Heap<Node> = Heap::new();

    let r1_next = Rc::new(Cell::new(None));
    let r1 = heap.alloc(Node {
        value: 1,
        next: r1_next.clone(),
        drops: drops.clone(),
    });
    let r2_next = Rc::new(Cell::new(None));
    let r2 = heap.alloc(Node {
        value: 2,
        next: r2_next.clone(),
        drops: drops.clone(),
    });
    let r3_next = Rc::new(Cell::new(Some(r2)));
    let r3 = heap.alloc(Node {
        value: 3,
        next: r3_next.clone(),
        drops: drops.clone(),
    });

    heap.mark([r1, r3].into_iter());
    heap.sweep();

    assert_eq!(drops.get(), 0);

    heap.mark(std::iter::empty());
    heap.sweep();

    assert_eq!(drops.get(), 3);
}

#[test]
fn cycle_is_collected_when_unreachable() {
    let drops = Rc::new(Cell::new(0));
    let mut heap: Heap<Node> = Heap::new();

    let r1_next = Rc::new(Cell::new(None));
    let r1 = heap.alloc(Node {
        value: 1,
        next: r1_next.clone(),
        drops: drops.clone(),
    });
    let r2_next = Rc::new(Cell::new(None));
    let r2 = heap.alloc(Node {
        value: 2,
        next: r2_next.clone(),
        drops: drops.clone(),
    });
    let r3_next = Rc::new(Cell::new(None));
    let r3 = heap.alloc(Node {
        value: 3,
        next: r3_next.clone(),
        drops: drops.clone(),
    });

    // create cycle r1 -> r2 -> r3 -> r1
    r1_next.set(Some(r2));
    r2_next.set(Some(r3));
    r3_next.set(Some(r1));

    heap.mark(std::iter::once(r1));
    heap.sweep();

    assert_eq!(drops.get(), 0);

    heap.mark(std::iter::empty());
    heap.sweep();

    assert_eq!(drops.get(), 3);
}

#[test]
fn sweep_with_empty_roots_frees_all_objects() {
    let drops = Rc::new(Cell::new(0));
    let mut heap: Heap<Node> = Heap::new();

    let r1 = heap.alloc(Node {
        value: 1,
        next: Rc::new(Cell::new(None)),
        drops: drops.clone(),
    });
    let r2 = heap.alloc(Node {
        value: 2,
        next: Rc::new(Cell::new(None)),
        drops: drops.clone(),
    });
    let r3 = heap.alloc(Node {
        value: 3,
        next: Rc::new(Cell::new(None)),
        drops: drops.clone(),
    });

    heap.mark(std::iter::empty());
    heap.sweep();

    assert_eq!(drops.get(), 3);

    let mut indices_after = vec![
        heap.alloc(Node {
            value: 4,
            next: Rc::new(Cell::new(None)),
            drops: drops.clone(),
        })
        .as_usize(),
        heap.alloc(Node {
            value: 5,
            next: Rc::new(Cell::new(None)),
            drops: drops.clone(),
        })
        .as_usize(),
        heap.alloc(Node {
            value: 6,
            next: Rc::new(Cell::new(None)),
            drops: drops.clone(),
        })
        .as_usize(),
    ];
    indices_after.sort_unstable();
    let mut expected = vec![r1.as_usize(), r2.as_usize(), r3.as_usize()];
    expected.sort_unstable();
    assert_eq!(indices_after, expected);
}

#[test]
fn freed_slots_reused_in_lifo_order() {
    let drops = Rc::new(Cell::new(0));
    let mut heap: Heap<Node> = Heap::new();

    let r1 = heap.alloc(Node {
        value: 1,
        next: Rc::new(Cell::new(None)),
        drops: drops.clone(),
    });
    let r2 = heap.alloc(Node {
        value: 2,
        next: Rc::new(Cell::new(None)),
        drops: drops.clone(),
    });
    let r3 = heap.alloc(Node {
        value: 3,
        next: Rc::new(Cell::new(None)),
        drops: drops.clone(),
    });

    heap.mark(std::iter::once(r2));
    heap.sweep();

    let r4 = heap.alloc(Node {
        value: 4,
        next: Rc::new(Cell::new(None)),
        drops: drops.clone(),
    });
    let r5 = heap.alloc(Node {
        value: 5,
        next: Rc::new(Cell::new(None)),
        drops: drops.clone(),
    });

    assert_eq!(r4.as_usize(), r1.as_usize());
    assert_eq!(r5.as_usize(), r3.as_usize());
}

#[test]
fn mark_without_sweep_does_not_free_objects() {
    let drops = Rc::new(Cell::new(0));
    let mut heap: Heap<Node> = Heap::new();

    let r1 = heap.alloc(Node {
        value: 1,
        next: Rc::new(Cell::new(None)),
        drops: drops.clone(),
    });
    let r2 = heap.alloc(Node {
        value: 2,
        next: Rc::new(Cell::new(None)),
        drops: drops.clone(),
    });

    heap.mark(std::iter::empty());
    let r3 = heap.alloc(Node {
        value: 3,
        next: Rc::new(Cell::new(None)),
        drops: drops.clone(),
    });

    assert_ne!(r3.as_usize(), r1.as_usize());
    assert_ne!(r3.as_usize(), r2.as_usize());

    heap.sweep();
    assert_eq!(drops.get(), 3);

    let r4 = heap.alloc(Node {
        value: 4,
        next: Rc::new(Cell::new(None)),
        drops: drops.clone(),
    });
    assert!(
        [r1.as_usize(), r2.as_usize(), r3.as_usize()]
            .iter()
            .any(|&i| i == r4.as_usize())
    );
}
