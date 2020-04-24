use crate::phase3::{Group, Phase3};
use std::io;
use std::path::PathBuf;

pub struct SourceFile {
    pub path: PathBuf,
    pub phase3_group: Group,
}

impl SourceFile {
    pub fn load(path: PathBuf) -> io::Result<Self> {
        let contents = std::fs::read_to_string(&path)?;
        Ok(SourceFile {
            path,
            phase3_group: Phase3::new(&contents).group(),
        })
    }
}
