use arith_final_tagless::arith_final_tagless;

fn main() {
    match arith_final_tagless::parse::expr::<i64>("8 + -(1 + 2)") {
        Ok((s, x)) => println!("s = {}, x = {:?}", s, x),
        Err(e) => println!("{}", e),
    }
    match arith_final_tagless::parse::expr::<arith_final_tagless::Term>("8 + -(1 + 2)") {
        Ok((s, x)) => println!("s = {}, x = {:?}", s, x),
        Err(e) => println!("{}", e),
    }
    match arith_final_tagless::parse::expr::<String>("8 + -(1 + 2)") {
        Ok((s, x)) => println!("s = {}, x = {:?}", s, x),
        Err(e) => println!("{}", e),
    }
}
