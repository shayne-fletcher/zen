use zbus::{proxy, Connection, Result};

#[proxy(
    default_service = "org.freedesktop.systemd1",
    default_path = "/org/freedesktop/systemd1",
    interface = "org.freedesktop.systemd1.Manager"
)]
trait SystemdManager {
    #[zbus(property)]
    fn architecture(&self) -> Result<String>;
    #[zbus(property)]
    fn environment(&self) -> Result<Vec<String>>;
    #[zbus(property)]
    fn log_level(&self) -> Result<String>;
}

#[tokio::main]
async fn main() -> Result<()> {
    let connection = Connection::system().await?;

    let proxy = SystemdManagerProxy::new(&connection).await?;
    println!("Host architecture: {}", proxy.architecture().await?);
    println!("Log level: {}", proxy.log_level().await?);
    println!("Environment variables:");
    for env in proxy.environment().await? {
        println!("    {}", env);
    }

    Ok(())
}
