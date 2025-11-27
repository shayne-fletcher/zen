use anyhow::Result;

#[cfg(target_os = "linux")]
mod sysd {
    use anyhow::Result;
    use zbus::fdo::DBusProxy;
    use zbus::Connection;
    use zbus_names::BusName;

    pub async fn has_systemd_user() -> Result<bool> {
        let conn = Connection::session().await?;
        let dbus: DBusProxy<'_> = DBusProxy::new(&conn).await?;
        let name: BusName = "org.freedesktop.systemd1"
            .try_into()
            .map_err(|e| zbus::Error::from(e))?;
        Ok(dbus.name_has_owner(name).await?)
    }
}

#[cfg(not(target_os = "linux"))]
mod sysd {
    use anyhow::Result;

    pub async fn has_systemd_user() -> Result<bool> {
        Ok(false)
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    println!("{}", sysd::has_systemd_user().await?);
    Ok(())
}
