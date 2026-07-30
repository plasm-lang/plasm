//: This code will scan source code and emit TODO warnings.
//: I did it because I always forget about my TODOs...

use std::fs;
use std::io::{BufRead, BufReader};
use std::path::Path;

fn main() {
    let members = [
        "ast",
        "cli",
        "codegen",
        "diagnostic",
        "hir",
        "mir",
        "orchestrator",
        "tokenizer",
        "utils",
    ];

    for member in &members {
        let member_src = Path::new("..").join(member).join("src");

        if member_src.exists() {
            println!("cargo:rerun-if-changed={}", member_src.display());
            visit_dirs(&member_src);
        }
    }
}

fn visit_dirs(dir: &Path) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                visit_dirs(&path);
            } else if path.extension().is_some_and(|ext| ext == "rs") {
                scan_file(&path);
            }
        }
    }
}

fn scan_file(path: &Path) {
    let file = match fs::File::open(path) {
        Ok(f) => f,
        _ => return,
    };
    let reader = BufReader::new(file);

    for (index, line) in reader.lines().map_while(Result::ok).enumerate() {
        if let Some(pos) = line.find("// TODO: ") {
            let msg = line[pos + 9..].trim();

            let clean_path = path.strip_prefix("../").unwrap_or(path);

            println!(
                "cargo:warning={}:{}: TODO: {}",
                clean_path.display(),
                index + 1,
                msg
            );
        }
    }
}
