#![allow(unused_mut)]
#![allow(incomplete_features)]
#![feature(generic_associated_types)]
/*
class Functor f where
  fmap :: (a -> b) -> F a -> F b
*/
trait Functor {
    type Unwrapped;
    type Wrapped<B>: Functor;

    fn map<F, B>(self, f: F) -> Self::Wrapped<B>
    where
        F: FnMut(Self::Unwrapped) -> B;
}

impl<A> Functor for Option<A> {
    type Unwrapped = A;
    type Wrapped<B> = Option<B>;

    fn map<F: FnMut(A) -> B, B>(self, mut f: F) -> Option<B> {
        self.map(f)
    }
}

trait Semigroup {
    fn append(self, rhs: Self) -> Self;
}

impl Semigroup for String {
    fn append(mut self, rhs: Self) -> Self {
        self = self + &rhs;
        self
    }
}

fn main() {
    println!("Hello world!")
}
