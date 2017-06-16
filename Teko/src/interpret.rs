use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;
use super::VEC_CAPACITY;

use num::bigint::BigInt;
use num::rational::BigRational;
use num::Complex;
use num::FromPrimitive;

use data_structures::{Commands, Data, Env, Source, Program, Sourcedata,
                      Coredata};

pub fn interpret(program: Program) {
	let env = Env {
		content:      [].iter().cloned().collect(),
		call_stack:   Vec::with_capacity(VEC_CAPACITY),
		params:       Vec::with_capacity(VEC_CAPACITY),
		return_value: Rc::new(Sourcedata(Source::default(), Coredata::Null)),
	};
	eval(program, env);
}

fn eval(mut program: Program, mut env: Env) {
	program.reverse();
	while let Some(top) = program.pop() {
		// top is of type Rc<Sourcedata>
		match &*top {
			&Sourcedata(ref source, Coredata::Pair(ref head, ref tail)) => {
				program.push(Rc::new(Sourcedata(tail.0.clone(), Coredata::Internal(Commands::Prepare(tail.clone())))));
				program.push(head.clone());
			},
			&Sourcedata(ref source, Coredata::Internal(Commands::Prepare(ref data))) => {
			},
			&Sourcedata(ref source, Coredata::Internal(Commands::Call)) => {
			},
			&Sourcedata(ref source, Coredata::Symbol(ref string)) => {
				if let Some(number) = BigInt::parse_bytes(string.as_bytes(), 10) {
					env.return_value = Rc::new(Sourcedata(source.clone(), Coredata::Integer(number)));
				} else {
					env.return_value = env.content.get(string).unwrap().last().unwrap().clone();
				}
			},
			other => {
				env.return_value = top.clone();
			},
		}
	}
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
