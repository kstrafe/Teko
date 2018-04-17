extern crate num;
extern crate teko;

use std::rc::Rc;

use teko::data_structures::Coredata;
use teko::interpret::interpret;
use teko::parse::parse_file;

use num::BigInt;

#[test]
fn main() {
	boolean("boolean-0.tko", true);
	boolean("boolean-1.tko", false);
	boolean("boolean-2.tko", true);
	boolean("boolean-3.tko", true);
	boolean("boolean-4.tko", true);
	boolean("boolean-5.tko", false);
	boolean("boolean-6.tko", true);

	boolean("local-does-not-leak.tko", true);
	boolean("local-does-not-leak-tail.tko", true);
	boolean("local-does-not-leak-active.tko", true);

	error("divide-by-zero.tko");
	error("define-0.tko");
	error("define-1.tko");
	error("define-2.tko");

	integer("addition-0.tko", "0");
	integer("addition-1.tko", "0");
	integer("addition-2.tko", "3");
	integer("addition-3.tko", "6");
	integer("addition-4.tko", "10");
}

// //////////////////////////////////////////////////////////
// Utility functions
// //////////////////////////////////////////////////////////

fn boolean(filename: &str, value: bool) {
	let result = &file2result(&filename).1;
	if let Coredata::Boolean(true) = *result {
		assert![value];
	} else if let Coredata::Boolean(false) = *result {
		assert![!value];
	} else {
		assert![false];
	}
}

fn error(filename: &str) {
	if let Coredata::Error(_) = file2result(filename).1 {
		assert![true];
	} else {
		assert![false];
	}
}

fn file2result(filename: &str) -> Rc<teko::data_structures::Sourcedata> {
	let program = parse_file(&(String::from("tests/") + filename))
		.ok()
		.unwrap();
	let env = interpret(program);
	env.get_result()
}

fn integer(filename: &str, number: &str) {
	let result = file2result(filename);
	assert_eq![
		result.1,
		Coredata::Integer(BigInt::parse_bytes(number.as_bytes(), 10).unwrap())
	];
}
