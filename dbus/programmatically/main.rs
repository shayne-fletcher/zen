use zbus::fdo::Error;
use zbus::Connection;

async fn has_systemd_user() -> bool {
    match Connection::system().await {
        Ok(conn) => conn
            .name
            .has_owner("ord.freedesktop.systemd1")
            .await
            .unwrap_or(false),
        Err(_) => false,
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    println!("{}", has_systemd_user().await.?);
}
