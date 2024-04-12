use tokio::io::AsyncBufReadExt;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncWriteExt;
use tokio::net::TcpStream;

#[tokio::main]
async fn main() -> std::io::Result<()> {
    let mut stream = TcpStream::connect("127.0.0.1:8080").await?;

    let mut stdout = tokio::io::stdout();
    let mut reader = tokio::io::BufReader::new(tokio::io::stdin());

    //let _ = reader.lines();

    loop {
        stdout.write_all(b"> ").await?;
        stdout.flush().await.ok();

        let mut line = Vec::new();
        let n = reader.read_until(b'\n', &mut line).await?;
        if n == 0 {
            break;
        }
        if *line.first().unwrap() == b'\n' {
            continue;
        }

        stream.write_all(&line[..n - 1]).await?;

        let mut buf = [0; 1024];
        let n = stream.read(&mut buf).await?;
        stdout.write_all(&buf[..n]).await?;
        stdout.flush().await.unwrap();
        loop {
            match stream.try_read(&mut buf) {
                Ok(0) => break,
                Ok(n) => {
                    stdout.write_all(&buf[..n]).await?;
                    stdout.flush().await.unwrap();
                    buf.iter_mut().for_each(|m| *m = 0);
                }
                Err(ref e) if e.kind() == tokio::io::ErrorKind::WouldBlock => {
                    stdout.write_all(b"\n").await?;
                    stdout.flush().await.unwrap();
                    break;
                }
                Err(e) => {
                    return Err(e.into());
                }
            }
        }
        buf.iter_mut().for_each(|m| *m = 0);
    }

    Ok::<(), std::io::Error>(())
}
