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
    Random { delay: u64 },
}

#[tokio::main]
async fn main() -> std::io::Result<()> {
    let cli = Cli::parse();

    let mut stream = tokio::net::TcpStream::connect("127.0.0.1:8080").await?;
    println!("echo client connected to server. type some lines. type crtl+D to exit.");

    let mut lines = tokio::io::BufReader::new(tokio::io::stdin()).lines();
    let mut stdout = tokio::io::stdout();

    match &cli.command {
        None => (),
        Some(Commands::Random { delay }) => {
            loop {
                let line = std::iter::repeat_with(|| {
                    char::from_u32(rand::thread_rng().gen_range(32..126))
                } /*rand::random::<u8>()*/)
                .map(|x| x.unwrap())
                .take(50)
                .collect::<String>();

                stream.write_all(&line[..].as_bytes()).await?;

                let mut buf = [0; 1];
                let n = stream.read(&mut buf).await?;
                stdout.write_all(&buf[..n]).await?;
                stdout.flush().await?;
                loop {
                    match stream.try_read(&mut buf) {
                        Ok(0) => break,
                        Ok(n) => {
                            stdout.write_all(&buf[..n]).await?;
                            stdout.flush().await.unwrap();
                            tokio::time::sleep(tokio::time::Duration::from_millis(*delay)).await;
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
        }
        Some(Commands::Repl) => loop {
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
        },
    }
    Ok(())
}
