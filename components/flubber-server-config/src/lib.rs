//! The config file for the Flubber server, plus configuration updates.
#![deny(
    bad_style,
    bare_trait_objects,
    const_err,
    dead_code,
    improper_ctypes,
    legacy_directory_ownership,
    missing_debug_implementations,
    missing_docs,
    no_mangle_generic_items,
    non_shorthand_field_patterns,
    overflowing_literals,
    path_statements,
    patterns_in_fns_without_body,
    plugin_as_library,
    private_in_public,
    safe_extern_statics,
    trivial_casts,
    trivial_numeric_casts,
    unconditional_recursion,
    // unions_with_drop_fields,
    unsafe_code,
    unused,
    unused_allocation,
    unused_comparisons,
    unused_extern_crates,
    unused_import_braces,
    unused_parens,
    unused_qualifications,
    unused_results,
    while_true
)]

use anyhow::{Context, Result};
use serde_derive::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    net::IpAddr,
    path::{Path, PathBuf},
};
use tokio::{fs::File, prelude::*, sync::watch};

/// The configuration of a server.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Config {
    /// The database configuration.
    pub database: DatabaseConfig,

    /// The backends the server should start.
    pub backends: HashMap<String, BackendConfig>,

    /// The web server's configuration.
    pub web: Option<WebConfig>,
}

impl Config {
    /// Loads configuration from a TOML file at the given path.
    pub async fn load(path: impl AsRef<Path>) -> Result<Config> {
        let mut file = File::open(path)
            .await
            .context("Failed to open config file")?;
        let mut s = String::new();
        let _ = file
            .read_to_string(&mut s)
            .await
            .context("Failed to read config file")?;
        let config = toml::from_str(&s).context("Failed to parse config file")?;
        Ok(config)
    }

    /// Returns a watch channel of updated configs.
    ///
    /// Currently, watches for `SIGHUP`, and reparses the config when it gets one.
    #[cfg(unix)]
    pub async fn watch(path: PathBuf) -> Result<watch::Receiver<Config>> {
        use tokio_net::signal::unix as signal;

        let config = Config::load(&path)
            .await
            .context("Failed to load initial config")?;
        let (send, recv) = watch::channel(config);
        let watch = signal::signal(signal::SignalKind::hangup())
            .context("Failed to listen for SIGHUPs")?
            .filter_map(move |()| {
                let path = path.clone();
                async move {
                    match Config::load(path).await {
                        Ok(config) => Some(Ok(config)),
                        Err(err) => {
                            log::error!("Failed to load config: {}", err);
                            None
                        }
                    }
                }
            })
            .forward(send)
            .map(|r| match r {
                Ok(()) => log::error!("Config watcher died"),
                Err(err) => log::error!("Config watcher died: {}", err),
            });

        drop(tokio::spawn(watch));
        Ok(recv)
    }
}

/// The configuration of the database.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct DatabaseConfig {
    /// The file to use for the database. Defaults to "flubber.db",
    #[serde(default = "default_database_file")]
    pub file: PathBuf,
}

/// The configuration of a single backend.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct BackendConfig {
    /// The command used to start the backend.
    pub cmd: String,

    /// Arguments passed to the command.
    #[serde(default)]
    pub args: Vec<String>,

    /// Additional environment variables to pass.
    #[serde(default)]
    pub env: HashMap<String, String>,

    /// A `.env` file to load for the subprocess. This overrides `env`.
    #[serde(rename = "env-file")]
    pub env_file: Option<PathBuf>,
}

/// The configuration for the web server.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct WebConfig {
    /// The address the web server should serve on. Defaults to localhost.
    #[serde(default = "localhost")]
    pub addr: IpAddr,

    /// The port the web server should serve on.
    pub port: u16,
}

fn default_database_file() -> PathBuf {
    "flubber.db".into()
}

fn localhost() -> IpAddr {
    "::1".parse().unwrap()
}
