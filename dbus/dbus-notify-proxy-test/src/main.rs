use std::collections::HashMap;
use std::error::Error;

use zbus::{proxy, zvariant::Value, Connection};

#[proxy(
    default_service = "org.freedesktop.Notifications",
    default_path = "/org/freedesktop/Notifications"
)]
trait Notifications {
    // Call the org.freedesktop.Notifications.Notify D-bus method
    fn notify(
        &self,
        app_name: &str,
        replaces_id: u32,
        app_icon: &str,
        summary: &str,
        body: &str,
        actions: &[&str],
        hints: HashMap<&str, &Value<'_>>,
        expire_timeout: i32,
    ) -> zbus::Result<u32>;
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let connection = Connection::session().await?;

    let proxy = NotificationsProxy::new(&connection).await?;
    let reply = proxy
        .notify(
            "my-app",
            0,
            "dialog-information",
            "A summary",
            "Some body",
            &[],
            HashMap::new(),
            5000,
        )
        .await?;
    dbg!(reply);

    Ok(())
}
