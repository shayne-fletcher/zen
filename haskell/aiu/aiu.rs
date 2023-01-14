trait TyCon {
    type Of<T>: HasTyCon<Param = T, GetTyCon = Self>;
}

trait HasTyCon {
    type Param;
    type GetTyCon: TyConAccepting<Self::Param, Applied = Self>;
}

trait TyConAccepting<T>: TyCon {
    type Applied;
}

impl<F: TyCon + ?Sized, T> TyConAccepting<T> for F {
    type Applied = Self::Of<T>;
}

trait HasTyConExt: HasTyCon + Sized {
    fn into_con_ty(self) -> <Self::GetTyCon as TyCon>::Of<Self::Param> {
        fn identity<F: TyCon, T>(x: <F as TyConAccepting<T>>::Applied) -> F::Of<T> {
            x
        }
        identity::<Self::GetTyCon, Self::Param>(self)
    }
    fn as_con_ty(&self) -> &<Self::GetTyCon as TyCon>::Of<Self::Param> {
        fn identity<F: TyCon, T>(x: &<F as TyConAccepting<T>>::Applied) -> &F::Of<T> {
            x
        }
        let s_ = self as &<Self::GetTyCon as TyConAccepting<Self::Param>>::Applied;
        identity::<Self::GetTyCon, _>(self)
    }
    fn as_con_ty_mut(&mut self) -> &mut <Self::GetTyCon as TyCon>::Of<Self::Param> {
        fn identity<F: TyCon, T>(x: &mut <F as TyConAccepting<T>>::Applied) -> &mut F::Of<T> {
            x
        }
        identity::<Self::GetTyCon, _>(self)
    }

    fn from_con_ty(this: <Self::GetTyCon as TyCon>::Of<Self::Param>) -> Self {
        fn identity<F: TyCon, T>(x: F::Of<T>) -> <F as TyConAccepting<T>>::Applied {
            x
        }
        identity::<Self::GetTyCon, _>(this)
    }
}
impl<T: HasTyCon> HasTyConExt for T {}

// newtype Fix f = In { out :: f (Fix f) }
struct Fix<F: TyCon>(std::rc::Rc<F::Of<Fix<F>>>);

impl<F: TyCon> Fix<F> {
    // inj :: Functor f => f (Fix f) -> Fix f
    // inj = In
    fn inj(x: impl HasTyCon<GetTyCon = F, Param = Fix<F>>) -> Self {
        Fix(std::rc::Rc::new(x.into_con_ty()))
    }

    // proj :: Functor f => Fix f -> f (Fix f)
    // proj = out
    fn proj(Fix(x): &Self) -> &F::Of<Fix<F>> {
        x
    }
}

enum Cons<A, Recur> {
    Nil,
    Cons(std::rc::Rc<A>, Recur),
}

struct Cons_<A>(std::marker::PhantomData<A>);
impl<A> TyCon for Cons_<A> {
    type Of<Recur> = Cons<A, Recur>;
}

impl<A, Recur> HasTyCon for Cons<A, Recur> {
    type Param = Recur;
    type GetTyCon = Cons_<A>;
}

type Term<A> = Fix<Cons_<A>>;

fn cons<A>(x: A, xs: Term<A>) -> Term<A> {
    Fix::inj(Cons::Cons(std::rc::Rc::new(x), xs))
}

fn nil<A>() -> Term<A> {
    Fix::inj(Cons::Nil)
}

fn main() {
    //let _: Term<isize> = Fix::new(Cons::Cons(std::rc::Rc::new(1), Fix::new(Cons::Nil)));
    let _: Term<isize> = cons(1, nil());
    ()
}
