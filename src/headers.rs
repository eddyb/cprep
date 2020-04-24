use crate::sources::SourceFile;
use elsa::FrozenMap;
use std::path::{Path, PathBuf};

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum IncludeStyle {
    Angled,
    Quoted,
}

pub struct Header {
    pub src: SourceFile,
    search_path_idx: Option<usize>,
}

pub struct Headers {
    search_paths: Vec<PathBuf>,
    headers: FrozenMap<PathBuf, Box<Header>>,
}

impl Headers {
    pub fn with_search_paths(search_paths: Vec<PathBuf>) -> Self {
        Headers {
            search_paths,
            headers: FrozenMap::new(),
        }
    }

    fn search(
        &self,
        style: IncludeStyle,
        enclosing_src_file: &SourceFile,
        name: &Path,
        start_after: Option<&Header>,
    ) -> Option<(Option<usize>, PathBuf)> {
        if style == IncludeStyle::Quoted && start_after.is_none() {
            if let Some(parent) = enclosing_src_file.path.parent() {
                let path = parent.join(name);
                if path.is_file() {
                    return Some((None, path));
                }
            }
        }

        let start_idx = start_after
            .and_then(|h| Some(h.search_path_idx? + 1))
            .unwrap_or(0);
        for (i, search_path) in self.search_paths.iter().enumerate().skip(start_idx) {
            let path = search_path.join(name);
            if path.is_file() {
                return Some((Some(i), path));
            }
        }
        None
    }

    pub fn has_include(
        &self,
        style: IncludeStyle,
        enclosing_src_file: &SourceFile,
        name: impl AsRef<Path>,
        start_after: Option<&Header>,
    ) -> bool {
        self.search(style, enclosing_src_file, name.as_ref(), start_after)
            .is_some()
    }

    pub fn include(
        &self,
        style: IncludeStyle,
        enclosing_src_file: &SourceFile,
        name: impl AsRef<Path>,
        start_after: Option<&Header>,
    ) -> Option<&Header> {
        let (search_path_idx, path) =
            self.search(style, enclosing_src_file, name.as_ref(), start_after)?;
        Some(match self.headers.get(&path) {
            Some(header) => header,
            None => {
                let src = SourceFile::load(path.clone())
                    .map_err(|err| {
                        eprintln!(
                            "error: could not include <{}>: failed to read `{}`: {}",
                            name.as_ref().display(),
                            path.display(),
                            err
                        )
                    })
                    .ok()?;
                self.headers.insert(
                    path,
                    Box::new(Header {
                        src,
                        search_path_idx,
                    }),
                )
            }
        })
    }
}
