use tokio::io::AsyncBufReadExt;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncWriteExt;

#[tokio::main]
async fn main() -> std::io::Result<()> {
    let mut stream = tokio::net::TcpStream::connect("127.0.0.1:8080").await?;
    println!("echo client connected to server. type some lines. type crtl+D to exit.");

    let mut lines = tokio::io::BufReader::new(tokio::io::stdin()).lines();
    let mut stdout = tokio::io::stdout();

    loop {
        match lines.next_line().await? {
            None => break,
            Some(line) if line.len() == 0 => continue,
            Some(line) if line.len() > 0 => {
                stream.write_all(&line[..].as_bytes()).await?;

                let mut buf = [0; 1024];
                let n = stream.read(&mut buf).await?;
                stdout.write_all(&buf[..n]).await?;
                stdout.flush().await?;
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
                            stdout.flush().await?;
                            break;
                        }
                        Err(e) => return Err(e.into()),
                    }
                }
                buf.iter_mut().for_each(|m| *m = 0);
            }
            Some(_) => panic!(),
        }
    }

    Ok(())
}
