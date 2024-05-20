use std::io::{ BufRead, BufReader, ErrorKind };

pub fn file_to_string(path: &str) -> String {
    match std::fs::read_to_string(path) {
        Ok(contents) => contents,
        Err(err) => {
            match err.kind() {
                ErrorKind::NotFound => panic!("File not found: {path}"),
                ErrorKind::PermissionDenied => panic!("Permission denied to read file: {path}"),
                _ => panic!("Error reading file {path}: {err}"),
            }
        }
    }
}

pub fn file_lines(path: &str) -> Vec<String> {
    match std::fs::File::open(path) {
        Ok(file) => {
            let reader = BufReader::new(file);
            let mut lines = Vec::new();
            for line in reader.lines() {
                match line {
                    Ok(contents) => lines.push(contents),
                    Err(err) => panic!("Error reading line from file {path}: {err}"),
                }
            }
            lines
        },
        Err(err) => {
            match err.kind() {
                ErrorKind::NotFound => panic!("File not found: {path}"),
                ErrorKind::PermissionDenied => panic!("Permission denied to read file: {path}"),
                _ => panic!("Error reading file {path}: {err}"),
            }
        }
    }
}
