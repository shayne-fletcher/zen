use tokio::io::AsyncBufReadExt;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncWriteExt;

use rand::Rng;

use clap::Parser;
use clap::Subcommand;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    Repl,
    Random {
        delay: u64,
        duration: u64,
        #[arg(default_value_t = 1)]
        num_connections: u64,
    },
}

#[tokio::main]
async fn main() -> std::io::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        None => (),
        Some(Commands::Random {
            delay,
            duration,
            num_connections,
        }) => {
            println!("echo client connecting to server to send lines.");
            let mut handles = Vec::new();
            for _i in 0..num_connections {
                handles.push(tokio::spawn(async move {
                    let mut stream = tokio::net::TcpStream::connect("127.0.0.1:8080")
                        .await
                        .unwrap();
                    match tokio::time::timeout(
                        tokio::time::Duration::from_secs(duration),
                        command_random(&mut stream, delay),
                    )
                    .await
                    {
                        Ok(Ok(())) | Ok(Err(_)) => panic!("the impossible happened"),
                        Err(tokio::time::error::Elapsed { .. }) => (),
                    }
                }));
            }
            for handle in handles {
                handle.await.unwrap();
            }
        }
        Some(Commands::Repl) => {
            let mut stream = tokio::net::TcpStream::connect("127.0.0.1:8080").await?;
            command_repl(&mut stream).await.unwrap()
        }
    }
    Ok(())
}

async fn command_random(
    stream: &mut tokio::net::TcpStream,
    delay: u64,
) -> Result<(), std::io::Error> {
    loop {
        let line: String =
            std::iter::repeat_with(|| char::from_u32(rand::thread_rng().gen_range(32..126)))
                .map(|x| x.unwrap())
                .take(rand::thread_rng().gen_range(1..77))
                .collect();

        stream.write_all(&line[..].as_bytes()).await?;

        let mut buf = [0; 1];
        let n = stream.read(&mut buf).await?;
        tokio::io::stdout().write_all(&buf[..n]).await?;
        tokio::io::stdout().flush().await?;
        loop {
            match stream.try_read(&mut buf) {
                Ok(0) => break,
                Ok(n) => {
                    tokio::io::stdout().write_all(&buf[..n]).await?;
                    tokio::io::stdout().flush().await.unwrap();
                    tokio::time::sleep(tokio::time::Duration::from_millis(delay)).await;
                    buf.iter_mut().for_each(|m| *m = 0);
                }
                Err(ref e) if e.kind() == tokio::io::ErrorKind::WouldBlock => {
                    tokio::io::stdout().write_all(b"\n").await?;
                    tokio::io::stdout().flush().await?;
                    break;
                }
                Err(e) => return Err(e.into()),
            }
        }
        buf.iter_mut().for_each(|m| *m = 0);
    }
}

async fn command_repl(stream: &mut tokio::net::TcpStream) -> Result<(), std::io::Error> {
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
    Ok::<(), std::io::Error>(())
}
