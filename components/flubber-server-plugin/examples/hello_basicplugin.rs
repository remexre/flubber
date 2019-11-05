use anyhow::{bail, Result};
use flubber_plugin_proto::{MessageContent, NewMessage, RoomIDOrUserID};
use flubber_server_plugin::BasicPlugin;
use log::info;
use serde_json::Value as Json;
use tokio::prelude::*;

#[tokio::main]
async fn main() -> Result<()> {
    femme::start(log::LevelFilter::Debug).unwrap();
    let mut args = std::env::args_os().collect::<Vec<_>>();
    let argv0 = args.remove(0);
    if args.is_empty() {
        bail!("Usage: {:?} cmd args...", argv0)
    }
    let cmd = args.remove(0);

    let plugin = BasicPlugin::new(cmd, args).await?;
    let sender = plugin.sender();
    tokio::spawn(plugin.for_each(|upd| {
        info!("Got update: {:#?}", upd);
        tokio::future::ready(())
    }));

    let room = sender.room_lookup("#general".to_string()).await?;
    info!("Got room = {:?}", room);
    sender.room_join(room.clone()).await?;
    sender
        .message_send(NewMessage {
            recipient: RoomIDOrUserID::Room(room),
            attachments: vec![],
            content: MessageContent::Text("Hello, world!".to_string()),
            extra: Json::Null,
        })
        .await?;

    Ok(())
}
