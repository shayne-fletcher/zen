use std::sync::Arc;
use tokio::sync::Mutex;

struct Table {
    forks: Vec<Mutex<()>>,
}

struct Philosopher {
    name: String,
    left: usize,
    right: usize,
}

impl Philosopher {
    fn new(name: &str, left: usize, right: usize) -> Self {
        Self {
            name: name.to_owned(),
            left,
            right,
        }
    }

    async fn eat(&self, table: &Arc<Table>) {
        let _left = table.forks[self.left].lock().await;
        tokio::time::sleep(tokio::time::Duration::from_millis(1000)).await;
        let _right = table.forks[self.right].lock().await;
        println!("{} is eating.", self.name);
        tokio::time::sleep(tokio::time::Duration::from_millis(1000)).await;
        println!("{} is done eating.", self.name)
    }
}

#[tokio::main]
async fn main() {
    let table = Arc::new(Table {
        forks: vec![
            Mutex::new(()),
            Mutex::new(()),
            Mutex::new(()),
            Mutex::new(()),
            Mutex::new(()),
        ],
    });

    let philosophers = vec![
        Philosopher::new("Judith Butler", 0, 1),
        Philosopher::new("Gilles Deleuze", 1, 2),
        Philosopher::new("Karl Marx", 2, 3),
        Philosopher::new("John Locke", 3, 4),
        Philosopher::new("Michael Foucalt", 0, 4),
    ];

    let mut handles = Vec::new();
    for philosopher in philosophers {
        let table = table.clone();
        handles.push(tokio::spawn(async move {
            philosopher.eat(&table).await;
        }));
    }

    for handle in handles {
        handle.await.unwrap();
    }
}
