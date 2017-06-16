use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;
use super::VEC_CAPACITY;

use num::bigint::BigInt;
use num::rational::BigRational;
use num::Complex;
use num::FromPrimitive;

use data_structures::{Commands, Data, Env, Source};

fn collect_arguments_data(mut args: Rc<Data>) -> Vec<Rc<Data>> {
	let mut arguments = Vec::with_capacity(3);
	loop {
		if let &Data::Pair(_, ref head, ref tail) = &*args.clone() {
			arguments.push(head.clone());
			args = tail.clone();
		} else {
			break;
		}
	}
	arguments
}

fn collect_arguments(mut args: Rc<Data>) -> Vec<String> {
	let mut arguments = Vec::with_capacity(3);
	loop {
		if let &Data::Pair(_, ref head, ref tail) = &*args.clone() {
			if let &Data::Symbol(_, ref string) = &**head {
				arguments.push(string.clone());
				args = tail.clone();
			} else {
				break;
			}
		} else {
			break;
		}
	}
	arguments
}

macro_rules! create_commands {
	($($i:expr => $t:tt),*; $($i2:ident => $t2:tt),*) => {
		[
			$(($i.into(), vec![Rc::new(Data::Internal(Source::default(), Commands::$t))])),*,
			$((stringify!($i2).into(), vec![Rc::new(Data::Internal(Source::default(), Commands::$t2))])),*
		]
	};
}

pub fn interpret(program: Vec<Rc<Data>>) {
	let env = Env {
		content: create_commands! {
							"+" => Plus,
							"-" => Minus,
							"*" => Multiply,
							"/" => Divide,
							"^" => Power,
							"%" => Modulo
							;
							def => Define,
							set => Set,
							unw => Unwind,
							win => Wind
						}.iter().cloned().collect(),
		call_stack:   Vec::with_capacity(VEC_CAPACITY),
		params:       Vec::with_capacity(VEC_CAPACITY),
		return_value: Rc::new(Data::Null(Source::default())),
	};
	eval(program, env);
}

fn eval(mut program: Vec<Rc<Data>>, mut env: Env) {
	program.reverse();
	println!["program: {:#?}\nenv: {:#?}", program, env];
	while let Some(top) = program.pop() {
		match &*top {
			&Data::Pair(ref source, ref head, ref tail) => {
				program.push(Rc::new(Data::Internal(tail.get_source(), Commands::Prepare(tail.clone()))));
				program.push(head.clone());
			},
			&Data::Internal(_, Commands::Prepare(ref data)) => {
			},
			&Data::Internal(_, Commands::Call) => {
			},
			&Data::Symbol(ref source, ref string) => {
				if let Some(number) = BigInt::parse_bytes(string.as_bytes(), 10) {
					env.return_value = Rc::new(Data::Integer(source.clone(), number));
				} else {
					env.return_value = env.content.get(string).unwrap().last().unwrap().clone();
				}
			},
			other => {
				env.return_value = top.clone();
			},
		}
		println!["program: {:#?}\nenv: {:#?}", program, env];
	}
	println!["final env: {:#?}", env];
}

#[cfg(test)]
mod tests {
	use super::*;
	use parse::parse_file;
	#[test]
	fn test() {
		let p = parse_file("input").ok().unwrap();
		interpret(p);
	}
}
