use anyhow::{anyhow, Context, Result};
use flubber_server::{start_http, Backend, Config, Database};
use futures_util::stream::select_all;
use std::path::PathBuf;
use tokio::{future::ready, prelude::*, runtime::Runtime};

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
        0 => log::LevelFilter::Info,
        1 => log::LevelFilter::Debug,
        _ => log::LevelFilter::Trace,
    })?;

    let runtime = Runtime::new()?;
    runtime.block_on(async {
        let mut configs = Config::watch(args.config_file).await?;
        let config = configs
            .recv()
            .await
            .ok_or_else(|| anyhow!("Config watcher died?"))?;

        let database = Database::open(config.database.file)?;

        let backends = config
            .backends
            .into_iter()
            .map(|(name, mut config)| {
                if let Some(path) = config.env_file {
                    config.env.extend(
                        // TODO: This is sync. It also only runs during initialization, but
                        // still...
                        parse_dotenv(path)
                            .with_context(|| format!("Failed to load env file for {}", name))?,
                    );
                }
                Ok((name, Backend::new(config.cmd, config.args, config.env)))
            })
            .collect::<Result<Vec<_>>>()?;
        let senders = backends
            .iter()
            .map(|(name, backend)| (name.clone(), backend.sender()))
            .collect::<Vec<_>>();
        let updates = select_all(
            backends
                .into_iter()
                .map(|(name, backend)| backend.map(move |u| (name.clone(), u))),
        );

        if let Some(web) = config.web {
            runtime.spawn(start_http(database.clone(), (web.addr, web.port)));
        }

        runtime.spawn(configs.for_each(|config| {
            log::warn!("Configuration updates NYI.");
            log::info!("config = {:?}", config);
            ready(())
        }));

        log::info!("{:#?}", senders);
        log::info!("{:#?}", updates);

        Ok(())
    })
}

#[allow(deprecated)]
fn parse_dotenv(path: PathBuf) -> Result<Vec<(String, String)>> {
    dotenv::from_path_iter(path)?
        .map(|r| r.map_err(Into::into))
        .collect()
}
