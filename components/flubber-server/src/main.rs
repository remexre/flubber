use anyhow::Result;
use flubber_server::{start_http, Config, Database, Plugin};
use std::path::PathBuf;
use tokio::runtime::Runtime;

#[derive(Debug, structopt::StructOpt)]
struct Args {
    /// Increases the verbosity of logging.
    #[structopt(short = "v", long = "verbose", parse(from_occurrences))]
    verbosity: usize,

    /// Chooses the configuration file to use.
    #[structopt(short = "c", long = "config", default_value = "flubber.toml")]
    config_file: PathBuf,
}

#[paw::main]
fn main(args: Args) -> Result<()> {
    femme::start(match args.verbosity {
        0 => log::LevelFilter::Warn,
        1 => log::LevelFilter::Info,
        2 => log::LevelFilter::Debug,
        _ => log::LevelFilter::Trace,
    })?;

    let runtime = Runtime::new()?;
    runtime.block_on(async {
        let mut config = Config::watch(args.config_file).await?;
        log::info!("{:#?}", config.recv().await);
        Ok(())
    })
}
