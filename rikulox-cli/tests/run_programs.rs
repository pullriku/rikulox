use std::fs;

#[test]
fn run_programs() {
    let dir = fs::read_dir("../program").unwrap();

    for entry in dir {
        let path = entry.unwrap().path();
        if path.is_file() && path.extension().unwrap() == "rlx" {
            rikulox_cli::run_file(&path).unwrap();
        }
    }
}
