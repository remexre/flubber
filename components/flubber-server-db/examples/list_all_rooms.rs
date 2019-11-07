use anyhow::{bail, Result};
use flubber_server_db::Database;

#[tokio::main]
async fn main() -> Result<()> {
    femme::start(log::LevelFilter::Debug).unwrap();
    let mut args = std::env::args_os().collect::<Vec<_>>();
    if args.len() != 2 {
        bail!("Usage: {:?} db-path", &args[0])
    }
    let path = args.remove(1);

    let mut db = Database::open(path)?;
    for plugin in db.list_plugins().await? {
        println!("{}", plugin);
        for id in db.list_rooms(plugin.clone()).await? {
            let room = db.get_room(plugin.clone(), id).await?.unwrap();
            println!("- {}", room.name);
        }
    }

    Ok(())
}
