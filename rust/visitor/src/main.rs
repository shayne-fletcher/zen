extern crate visitor;
use visitor::expr::*;

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//

struct Harvest<'a> {
    pub acc: Vec<&'a Term>,
}
impl<'a> Harvest<'a> {
    fn new(t: &'a Term) -> Harvest<'a> {
        let mut h = Harvest { acc: Vec::new() };
        t.accept(&mut h);
        h
    }
}
impl<'a> Visitor<'a> for Harvest<'a> {
    fn object(&mut self) -> &mut dyn Visitor<'a> {
        self
    }
    fn visit_term(&mut self, t: &'a Term) {
        self.acc.push(t);
        t.recurse(self.object());
    }
}

fn harvest(t: &Term) -> Vec<&Term> {
    Harvest::new(&t).acc
}

pub fn main() {
    println!("{:?}", harvest(&(parse("1 + 2 + (3 +(-8))").unwrap())))
}
