use std::collections::BTreeMap;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

// FIXME(eddyb) autodetect this via `c++ -E -v` output.
const SYSTEM_INCLUDE_PATHS: &[&str] = &["/usr/include"];

const INCLUDE_PATHS: &[&str] = &[];

fn main() -> Result<(), std::io::Error> {
    let headers = cprep::headers::Headers::with_search_paths(
        INCLUDE_PATHS
            .iter()
            .chain(SYSTEM_INCLUDE_PATHS)
            .map(PathBuf::from)
            .collect(),
    );
    let builtin_defines = {
        let native = std::process::Command::new("c++")
            .args(&["-E", "-dM", "-xc++", "-std=c++17", "-"])
            .stdin(std::process::Stdio::null())
            .output()?;
        cprep::sources::SourceFile {
            path: PathBuf::new(),
            phase3_group: cprep::phase3::Phase3::new(&String::from_utf8(native.stdout).unwrap())
                .group(),
        }
    };
    let builtin_defines = {
        let mut phase4 = cprep::phase4::Phase4::new(&builtin_defines, &headers);
        phase4.expand();
        phase4
    };

    if let Some(path) = std::env::args().nth(1) {
        let src = cprep::sources::SourceFile::load(PathBuf::from(path))?;
        for tok in cprep::phase4::Phase4::with_defines_from(&src, &builtin_defines).expand() {
            print!("{}", tok);
        }
        return Ok(());
    }

    let mut found_headers = BTreeMap::new();

    for &include_path in INCLUDE_PATHS.iter().chain(SYSTEM_INCLUDE_PATHS) {
        let include_path = Path::new(include_path);

        for entry in WalkDir::new(include_path).follow_links(true) {
            let entry = entry?;
            if entry.file_type().is_file() {
                let relative_path = entry.path().strip_prefix(include_path).unwrap();

                found_headers.insert(
                    relative_path.to_path_buf(),
                    headers
                        .include(
                            cprep::headers::IncludeStyle::Angled,
                            &cprep::sources::SourceFile {
                                path: PathBuf::new(),
                                phase3_group: cprep::phase3::Group { parts: vec![] },
                            },
                            relative_path,
                            None,
                        )
                        .unwrap(),
                );
            }
        }
    }

    eprintln!("Found {} headers", found_headers.len());

    {
        let mut dot = std::fs::File::create("graph.dot")?;
        writeln!(dot, "digraph g {{")?;
        for (relative_path, header) in &found_headers {
            let node = relative_path.display().to_string();
            writeln!(dot, "    {:?};", node)?;
            for include in cprep::phase4::Phase4::new(&header.src, &headers).scan_for_includes() {
                if !found_headers.contains_key(Path::new(&include)) {
                    writeln!(dot, "    {:?} [style=dashed];", include)?
                }
                writeln!(dot, "    {:?} -> {:?};", node, include)?;
            }
            writeln!(dot, "")?;
        }
        writeln!(dot, "}}")?;
    }

    Ok(())
}
