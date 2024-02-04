use std::fs;
use std::path::Path;
use std::path::PathBuf;

// '../../../security/flare/if/impl.rs ->'
// 'd1/d2/d3/security/flare/if/impl.rs'
fn map_path(path:&str, pre: &str) -> PathBuf {
    let mut i: i32 = 0;
    let mut rem: &str = path;
    let mut components: Vec<String> = vec![];
    while let Some(left) = rem.strip_prefix("../") {
        i = i + 1;
        components.push(format!("{}{}", pre, i));
        rem = left;
    }
    Path::new(&components.join("/")).join(rem)
}

fn main() -> std::io::Result<()> {
    let path: &str = "../../../security/flare/if/impl.rs";
    let mapped_path: PathBuf = map_path(path, "d");

    fs::create_dir_all(mapped_path.parent().unwrap())?;

    Ok(())
}
