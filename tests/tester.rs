extern crate num;
extern crate teko;

use std::rc::Rc;

use teko::data_structures::Coredata;
use teko::interpret::interpret;
use teko::parse::parse_file;

use num::BigInt;

#[test]
fn main() {
	integer("addition-0.tko", "0");
	integer("addition-1.tko", "0");
	integer("addition-2.tko", "3");
	integer("addition-3.tko", "6");
	integer("addition-4.tko", "10");
}

// //////////////////////////////////////////////////////////
// Utility functions
// //////////////////////////////////////////////////////////

fn file2result(filename: &str) -> Rc<teko::data_structures::Sourcedata> {
	let program = parse_file(&(String::from("tests/") + filename)).ok().unwrap();
	let env = interpret(program);
	env.result.clone()
}

fn integer(filename: &str, number: &str) {
	let result = file2result(filename);
	assert_eq![result.1, Coredata::Integer(BigInt::parse_bytes(number.as_bytes(), 10).unwrap())];
}

