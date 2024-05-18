use tokio::io::AsyncReadExt;
use tokio::io::AsyncWriteExt;
use tokio::net::TcpListener;

//use std::sync::Arc;
//use std::tokio::Mutex;

use tracing::instrument;

#[tokio::main]
async fn main() -> std::io::Result<()> {
    let subscriber = tracing_subscriber::FmtSubscriber::new();
    let _ = tracing::subscriber::set_global_default(subscriber)
        .map_err(|_err| eprintln!("Unable to set global default subscriber"));

    let listener = TcpListener::bind("127.0.0.1:8080").await?;
    tracing::info!("echo server up");
    loop {
        let (stream, _) = listener.accept().await?;
        tokio::spawn(async move { handle_connection(stream).await });
    }
}

#[instrument]
#[inline(never)]
async fn handle_connection(mut stream: tokio::net::TcpStream) -> Result<(), std::io::Error> {
    tracing::info!("new connection");
    loop {
        let mut buf = [0; 1024];
        let n = stream.read(&mut buf).await?;
        if n == 0 {
            break;
        }
        stream.write_all(&buf[..n]).await?;
    }
    Ok::<(), std::io::Error>(())
}
