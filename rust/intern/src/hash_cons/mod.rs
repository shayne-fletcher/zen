// [Type-Safe Modular Hash-Consing](https://www.lri.fr/~filliatr/ftp/publis/hash-consing2.pdf)
#![allow(dead_code)]
use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::rc::Rc;
use std::rc::Weak;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::sync::Mutex;

pub fn hello() -> &'static str {
    "hello"
}

struct ConsedImpl<T> {
    node: T,
    tag: u64,
}

impl<T: fmt::Debug> fmt::Debug for ConsedImpl<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.node.fmt(f)
    }
}

impl<T> Deref for ConsedImpl<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.node
    }
}

impl<T> PartialEq for ConsedImpl<T> {
    fn eq(&self, other: &Self) -> bool {
        self.tag == other.tag
    }
}

impl<T> Eq for ConsedImpl<T> {}

impl<T> Hash for ConsedImpl<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.tag.hash(state);
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Consed<T>(Rc<ConsedImpl<T>>);

impl<T> Deref for Consed<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T: fmt::Debug> fmt::Debug for Consed<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

#[derive(Debug)]
pub struct Conser<T> {
    next_tag: AtomicU64,
    table: Mutex<HashMap<T, Weak<ConsedImpl<T>>>>,
}

impl<T: Eq + Hash + Clone> Conser<T> {
    pub fn new() -> Self {
        Conser {
            next_tag: AtomicU64::new(0),
            table: Mutex::new(HashMap::new()),
        }
    }

    pub fn mk<Q: ?Sized>(&self, x: &Q) -> Consed<T>
    where
        T: Borrow<Q>, // T can be borrowed as a Q
        // e.g. T as String, and Q as str
        // It's very important that T and Q have implementations of
        // the Hash and Eq traits that produce identical results
        Q: ToOwned<Owned = T> + Hash + Eq, // you can promote a Q to a T
    {
        let mut table = self.table.lock().unwrap();
        // This is where we rely on the hash results of T and Q being
        // identical.
        let rc = table.get(x).and_then(Weak::upgrade);
        match rc {
            Some(rc) => Consed(rc),
            None => {
                let x = x.to_owned();
                let tag = self.next_tag.fetch_add(1, Ordering::Relaxed);
                let consed = ConsedImpl {
                    node: x.clone(),
                    tag,
                };
                let consed = Rc::new(consed);
                table.insert(x, Rc::downgrade(&consed));
                Consed(consed)
            }
        }
    }
}
