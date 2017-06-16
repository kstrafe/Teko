use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;
use super::VEC_CAPACITY;

use num::bigint::BigInt;
use num::rational::BigRational;
use num::Complex;
use num::FromPrimitive;

use data_structures::{Commands, Env, Source, Program, Sourcedata,
                      Coredata, Statement, Macro};

fn plus(top:     &Statement,
        program: &mut Program,
        env:     &mut Env) {
	env.params.last().expect("State machine should ensure this exists");
	println!("Adding");
}

pub fn interpret(program: Program) {
	let env = Env {
		content:      [("+".into(), vec![Rc::new(Sourcedata(
																								 Source::default(),
																								 Coredata::Macro(Macro::Builtin(plus))))])].iter().cloned().collect(),
		call_stack:   Vec::with_capacity(VEC_CAPACITY),
		params:       Vec::with_capacity(VEC_CAPACITY),
		return_value: Rc::new(Sourcedata(Source::default(), Coredata::Null)),
	};
	eval(program, env);
}

fn eval(mut program: Program, mut env: Env) {
	program.reverse();
	while let Some(top) = program.pop() {
		println!["HELLO {}", program.len()];
		println!["{}", top];
		match &*top {
			&Sourcedata(ref source, Coredata::Pair(ref head, ref tail)) => {
				program.push(Rc::new(Sourcedata(tail.0.clone(), Coredata::Internal(Commands::Prepare(tail.clone())))));
				program.push(head.clone());
				println!["Expanding"];
			},
			&Sourcedata(ref source, Coredata::Internal(Commands::Prepare(ref data))) => {
				println!["Prepare"];
			},
			&Sourcedata(ref source, Coredata::Internal(Commands::Call)) => {
				println!["Call"];
			},
			&Sourcedata(ref source, Coredata::Symbol(ref string)) => {
				if let Some(number) = BigInt::parse_bytes(string.as_bytes(), 10) {
					env.return_value = Rc::new(Sourcedata(source.clone(), Coredata::Integer(number)));
				} else {
					env.return_value = env.content.get(string).unwrap().last().unwrap().clone();
				}
				println!["Atom"];
			},
			other => {
				println!["Otherwise"];
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
