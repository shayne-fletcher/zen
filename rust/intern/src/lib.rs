pub mod hash_cons;
pub mod symbol;

#[cfg(test)]
mod tests {
    use super::hash_cons::*;
    use super::symbol::*;

    struct Symbols {
        symbols: Conser<String>,
    }

    impl Symbols {
        fn new() -> Self {
            Self {
                symbols: Conser::new(),
            }
        }

        fn mk_symbol(&self, symbol: &str) -> Symbol {
            Symbol::from(self.symbols.mk(symbol))
        }
    }

    #[test]
    fn symbol() {
        let tab = Symbols::new();
        let sym = tab.mk_symbol("hello");
        assert_eq!("hello", *sym);
        assert_eq!(sym, tab.mk_symbol("hello"));
        assert_ne!(sym, tab.mk_symbol("goodbye"));
    }
}
