use std::env;
use std::fs;
use std::path::Path;
use std::process;

const DEFAULT_URL: &str = "https://raw.githubusercontent.com/microsoft/language-server-protocol/gh-pages/_specifications/lsp/3.17/metaModel/metaModel.json";

fn main() {
    if let Err(error) = run() {
        eprintln!("fetch-lsp-metamodel: {error}");
        process::exit(1);
    }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let mut args = env::args().skip(1);
    let output = args
        .next()
        .unwrap_or_else(|| "packages/lsp-types/metamodel/metaModel.full.json".to_owned());
    let url = args.next().unwrap_or_else(|| DEFAULT_URL.to_owned());

    if let Some(parent) = Path::new(&output).parent() {
        fs::create_dir_all(parent)?;
    }

    let body = ureq::get(&url).call()?.into_string()?;
    let parsed: serde_json::Value = serde_json::from_str(&body)?;
    let formatted = serde_json::to_string_pretty(&parsed)?;
    fs::write(&output, format!("{formatted}\n"))?;
    Ok(())
}
