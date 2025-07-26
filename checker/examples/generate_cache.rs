//! This generates a cache from a definition file

use std::{
	env,
	fs::{read_to_string, write},
	path::PathBuf,
};

use ezno_checker::{generate_cache, synthesis::EznoParser};

fn main() {
	
	let input = env::args().nth(1).or_else(|| {Some("./checker/definitions/overrides.d.ts".to_string())}).unwrap();
	let output = env::args().nth(2).or_else(|| {Some("./checker/definitions/internal.ts.d.bin".to_string())}).unwrap();

	let input = env::args().nth(1).or_else(|| {Some("./checker/definitions/indexing-test.d.ts".to_string())}).unwrap();
	let output = env::args().nth(2).or_else(|| {Some("./checker/definitions/indexing-test.d.ts.d.bin".to_string())}).unwrap();

	// This reader, doesn't lookup in the cache
	let reader = |path: &std::path::Path| read_to_string(path).ok();

	let cache = generate_cache::<_, EznoParser>(
		PathBuf::from(input).as_path(), 
		&reader, 
		()
	);
	let result = write(output, cache);
	
	match result {
		Ok(()) => {
			eprintln!("Cache generated 🏧💵✅");
		},
		Err(e) => {
			eprintln!("Failed to write cache: {}", e);
		}
	}
}