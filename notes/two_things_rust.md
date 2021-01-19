# Two things in Rust

Two things I needed to learn before Rust made sense to me.

## 1 Pattern binding modes

I don't remember reading about this in [the book](https://doc.rust-lang.org/book/). Default binding modes come into play when **non-reference** patterns are encountered.

#### Example

```rust

let x = Some(3);
let y: &Option<i32> = &x;
match y {
    Some(a) -> {
    // `y` is deref'd and `a` is bound as `ref a`
    }
    None => {}
}
```
The default binding mode starts as `move`. Each time a reference is matched using a *non-reference pattern*; it will automatically derefence the vaue and update the default binding mode
- If the reference is `&val`, set the default binding mode to `ref`
- If the reference is `&mut val`: if the current default binding is `ref`, it should remain `ref`. Otherwise, set the current binding mode to `ref mut`.

### Example

- Example

```rust
match (&Some(3)) {
    Some(p) =>
      // This pattern is a "non-reference pattern".
      // Dereference the `&` and shift the default binding
      // mode to `ref`. `p` is read as `ref p` and given type `i32`.
   x => {
     // In this arm, we are still in `move` mode by default, so `x`
     // has type `&Option<32>`
   }
}
```
- Desugared

```rust
    match(&Some(3)) {
      &Some(ref p) => {
         ...
      },
      x => {
         ...
      },
    }
```

There's quite a lot to remember. See [2005-match-ergonomics](https://github.com/rust-lang/rfcs/blob/master/text/2005-match-ergonomics.md) rustlang RFC.

## 2. [Implict Deref Coercisons with Functions and Methods](https://doc.rust-lang.org/book/ch15-02-deref.html#implicit-deref-coercions-with-functions-and-methods)

This is another ergonomics feature that saves on explicit `&`s and `*`s when writing function and method calls. When we pass a reference to a function or method call `deref` implicitly as needed to coerce to the parameter target type.

- Example:

```
fn hello(name: &str) {
    println!("Hello, {}", name);
}

fn main() {
    let m = MyBox::new(String::from("Rust"));
    hello(&m);
}
```

- Example:

```
pub struct Point {
    x: Vec<i32>,
    y: Vec<i32>,
}

impl Point {
    pub fn x(&self) -> &Vec<i32> {
        match self {
            &Point { ref x, .. } => x,
        }
    }
}

fn use_i32(_: &i32) -> () {}

fn use_vi32(_: &Vec<i32>) -> () {}

fn use_str(_: &str) -> () {}

fn use_strr(_: &&String) -> () {}

fn main() {
    let p: Point = Point {
        x: vec![],
        y: vec![],
    };

    let rp: &Point = &p;
    let rrp: &&Point = &rp;

    println!("p.x = {:?}", rrp.x());

    let _s: &str = "foo";
    let s: String = String::from(_s);
    let bs: Box<String> = Box::new(s.clone());
    let bsr: Box<&String> = Box::new(&s);
    let bi32: Box<Vec<i32>> = Box::new(vec![]);

    use_i32(&&&&1i32);

    use_str(bs.deref());
    use_str(&s);
    use_strr(bsr.deref());
    let r: &&String = &bsr;
    let r2: &String = r.deref();

    use_str(r2);
    use_str(&bsr);
    use_vi32(&bi32);

    let p = Point {
        x: vec![1, 2, 3],
        y: vec![4, 5, 6],
    };
    match &p {
        Point { x, y } => {
            use_vi32(x);
            use_vi32(y);
            println!("{:?}, {:?}", x, y);
        }
    }
    println!("{:?}, {:?}", p.x, p.y);
}
```
