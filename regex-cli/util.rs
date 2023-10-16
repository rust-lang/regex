use std::{
    io::{self, Write},
    path::Path,
    process::Command,
};

use {anyhow::Context, bstr::BString};

/// Time an arbitrary operation.
pub fn timeit<T>(run: impl FnOnce() -> T) -> (T, std::time::Duration) {
    let start = std::time::Instant::now();
    let t = run();
    (t, start.elapsed())
}

/// Convenient time an operation that returns a result by packing the duration
/// into the `Ok` variant.
pub fn timeitr<T, E>(
    run: impl FnOnce() -> Result<T, E>,
) -> Result<(T, std::time::Duration), E> {
    let (result, time) = timeit(run);
    let t = result?;
    Ok((t, time))
}

/// Run rustfmt on the given file.
pub fn rustfmt<P: AsRef<Path>>(path: P) -> anyhow::Result<()> {
    let path = path.as_ref();
    let out = Command::new("rustfmt")
        .arg(path)
        .output()
        .context("rustfmt command failed")?;
    anyhow::ensure!(
        out.status.success(),
        "rustfmt {}: {}",
        path.display(),
        BString::from(out.stderr),
    );
    Ok(())
}

/// A somewhat silly little thing that prints an aligned table of key-value
/// pairs. Keys can be any string and values can be anything that implements
/// Debug.
///
/// This table is used to print little bits of useful information about stuff.
#[derive(Debug)]
pub struct Table {
    pairs: Vec<(String, Box<dyn std::fmt::Debug>)>,
}

impl Table {
    pub fn empty() -> Table {
        Table { pairs: vec![] }
    }

    pub fn add<D: std::fmt::Debug + 'static>(
        &mut self,
        label: &str,
        value: D,
    ) {
        self.pairs.push((label.to_string(), Box::new(value)));
    }

    pub fn print<W: io::Write>(&self, wtr: W) -> io::Result<()> {
        let mut wtr = tabwriter::TabWriter::new(wtr)
            .alignment(tabwriter::Alignment::Right);
        for (label, value) in self.pairs.iter() {
            writeln!(wtr, "{label}:\t{value:?}")?;
        }
        wtr.flush()
    }
}
