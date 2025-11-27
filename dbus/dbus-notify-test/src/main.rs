use std::collections::HashMap;
use std::error::Error;

use zbus::{zvariant::Value, Connection};

// Although we use `tokio` here, you can use any async runtime of choice.
#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let connection = Connection::session().await?;

    let m = connection
        .call_method(
            Some("org.freedesktop.Notifications"),
            "/org/freedesktop/Notifications",
            Some("org.freedesktop.Notifications"),
            "Notify",
            &(
                "my-app",
                0u32,
                "dialog-information",
                "A summary",
                "Some body",
                vec![""; 0],
                HashMap::<&str, &Value>::new(),
                5000,
            ),
        )
        .await?;
    let reply: u32 = m.body().deserialize().unwrap();
    dbg!(reply);
    Ok(())
}
