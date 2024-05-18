use rand::Rng;
use ring::{
    digest::{Context, SHA256},
    error::Unspecified,
};

fn main() -> Result<(), Unspecified> {
    let sample_size = 10;
    let mut rng = rand::thread_rng();
    for _ in 0..sample_size {
        let mut data: [u8; 32] = [0; 32];
        rng.fill(&mut data);
        let mut context = Context::new(&SHA256);
        context.update(&data);
        let hash = context.finish();
        println!("{}", hex::encode(hash.as_ref()));
    }

    Ok(())
}
