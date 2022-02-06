use crate::hash_cons::Consed;
use std::ops::Deref;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol(Consed<String>);

impl Deref for Symbol {
    type Target = String;

    fn deref(&self) -> &String {
        &self.0
    }
}

impl From<Consed<String>> for Symbol {
    fn from(sym: Consed<String>) -> Symbol {
        Symbol(sym)
    }
}
