use flubber_plugin::sync::start_plugin;
use libsignal_protocol::{crypto::DefaultCrypto, Context};
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
fn main(args: Args) {
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

    let (reqs, updates) = start_plugin(
        env!("CARGO_PKG_NAME").to_string(),
        env!("CARGO_PKG_VERSION_MAJOR").parse().unwrap(),
        env!("CARGO_PKG_VERSION_MINOR").parse().unwrap(),
        env!("CARGO_PKG_VERSION_PATCH").parse().unwrap(),
    );

    let ctx = Context::new(DefaultCrypto).unwrap();
}
