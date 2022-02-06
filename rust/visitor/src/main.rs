extern crate visitor;
use std::marker::PhantomData;
use visitor::expr::*;

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//

struct Harvest<'a> {
    pub terms: Vec<&'a Term>,
    pub phantom: PhantomData<&'a Term>,
}
impl<'a> Harvest<'a> {
    fn new() -> Harvest<'a> {
        Harvest {
            terms: Vec::new(),
            phantom: PhantomData,
        }
    }
}
impl<'a> Visitor<'a> for Harvest<'a> {
    fn object(&mut self) -> &mut dyn Visitor<'a> {
        self
    }
    fn visit_term(&mut self, t: &'a Term) {
        self.terms.push(t);
        t.recurse(self.object());
    }
}

pub fn main() {
    let t = parse("1 + 2 + (3 +(-8))").unwrap();
    let mut harvest = Harvest::new();
    harvest.visit_term(&t);
    println!("{:?}", harvest.terms);
}
