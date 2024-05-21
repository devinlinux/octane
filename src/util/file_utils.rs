use std::io::ErrorKind;

pub fn file_to_string(path: &str) -> String {
    match std::fs::read_to_string(path) {
        Ok(contents) => contents,
        Err(err) => {
            handle_io_error(path, err);
            std::process::exit(1)
        },
    }
}

fn handle_io_error(path: &str, err: std::io::Error) {
    match err.kind() {
        ErrorKind::NotFound => eprintln!("File not found {path}"),
        ErrorKind::PermissionDenied => eprintln!("Inadequate permissions to read file {path}"),
        _ => eprintln!("Error reading file {path}: {err}"),
    }
}
