use std::{env, io::Write};

mod args;
mod cmd;
mod logger;
mod util;

fn main() -> anyhow::Result<()> {
    let rustlog = env::var("RUST_LOG").unwrap_or_else(|_| String::new());
    let level = match &*rustlog {
        "" | "off" => log::LevelFilter::Off,
        "error" => log::LevelFilter::Error,
        "warn" => log::LevelFilter::Warn,
        "info" => log::LevelFilter::Info,
        "debug" => log::LevelFilter::Debug,
        "trace" => log::LevelFilter::Trace,
        unk => anyhow::bail!("unrecognized log level '{}'", unk),
    };
    logger::Logger::init()?;
    log::set_max_level(level);

    if let Err(err) = cmd::run(&mut lexopt::Parser::from_env()) {
        if std::env::var("RUST_BACKTRACE").map_or(false, |v| v == "1") {
            writeln!(&mut std::io::stderr(), "{:?}", err).unwrap();
        } else {
            writeln!(&mut std::io::stderr(), "{:#}", err).unwrap();
        }
        std::process::exit(1);
    }
    Ok(())
}
