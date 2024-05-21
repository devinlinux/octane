use std::io::ErrorKind;

pub fn file_to_string(path: &str) -> String {
    match std::fs::read_to_string(path) {
        Ok(contents) => contents,
        Err(err) => {
            handle_io_error(path, err);
            String::new()
        },
    }
}

fn handle_io_error(path: &str, err: std::io::Error) {
    match err.kind() {
        ErrorKind::NotFound => panic!("File not found {path}"),
        ErrorKind::PermissionDenied => panic!("Inadequate permissions to read file {path}"),
        _ => panic!("Error reading file {path}: {err}"),
    }
}
