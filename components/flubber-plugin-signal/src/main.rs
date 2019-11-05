use anyhow::Result;
use flubber_plugin::Parent;
use log::info;

#[derive(Debug, structopt::StructOpt)]
struct Args {
    /// Disables logging.
    #[structopt(short = "q", long = "quiet")]
    quiet: bool,

    /// Increases log verbosity. May be specified multiple times.
    #[structopt(parse(from_occurrences), short = "v", long = "verbose")]
    verbosity: u64,
}

#[paw::main]
fn main(args: Args) -> Result<()> {
    if !args.quiet {
        use log::LevelFilter;
        femme::start(match args.verbosity {
            0 => LevelFilter::Warn,
            1 => LevelFilter::Info,
            2 => LevelFilter::Debug,
            _ => LevelFilter::Trace,
        })
        .unwrap();
    }

    let runtime = tokio::runtime::Runtime::new()?;
    info!("=== before runtime ===");
    runtime.block_on(async {
        let parent = Parent::new_from_cargo_toml().await?;
        info!("args = {:?}", args);

        Ok(())
    })
}
