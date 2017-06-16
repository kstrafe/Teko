use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;
use super::VEC_CAPACITY;

use num::bigint::BigInt;
use num::rational::BigRational;
use num::Complex;
use num::FromPrimitive;

use data_structures::{Commands, Env, Source, Program, Sourcedata,
                      Coredata, Statement, Macro, Function};

fn plus(top:     &Statement,
        program: &mut Program,
        env:     &mut Env) {
	env.params.last().expect("State machine should ensure this exists");
	println!("Adding");
}

fn collect_pair_into_vec(data: &Rc<Sourcedata>) -> Vec<Rc<Sourcedata>> {
	let mut to_return = vec![];
	let mut current   = data.clone();
	loop {
		current = if let &Sourcedata(_, Coredata::Pair(ref head, ref tail)) = &*current {
			to_return.push(head.clone());
			tail.clone()
		} else {
			break;
		}
	}
	to_return
}

pub fn interpret(program: Program) {
	let env = Env {
		content:      [("+".into(), vec![Rc::new(Sourcedata(
																								 Source::default(),
																								 Coredata::Function(Function::Builtin(plus))))])].iter().cloned().collect(),
		call_stack:   Vec::with_capacity(VEC_CAPACITY),
		params:       Vec::with_capacity(VEC_CAPACITY),
		return_value: Rc::new(Sourcedata(Source::default(), Coredata::Null)),
	};
	eval(program, env);
}

fn eval(mut program: Program, mut env: Env) {
	program.reverse();
	while let Some(top) = program.pop() {
		println!["PROGRAM LENGTH: {}", program.len()];
		println!["{}", top];
		match &*top {
			&Sourcedata(ref source, Coredata::Pair(ref head, ref tail)) => {
				println!["Expanding"];
				program.push(Rc::new(Sourcedata(tail.0.clone(), Coredata::Internal(Commands::Prepare(tail.clone())))));
				program.push(head.clone());
			},
			&Sourcedata(ref source, Coredata::Internal(Commands::Parameterize)) => {
				println!["Parameterize"];
				env.params.last_mut().expect("Guaranteed to exist").push(env.return_value.clone());
			},
			&Sourcedata(ref source, Coredata::Internal(Commands::Prepare(ref arguments))) => {
				match &*env.return_value {
					&Sourcedata(_, Coredata::Function(..)) => {
						println!["Prepare function"];
						env.params.push(vec!());
						env.call_stack.push(env.return_value.clone());
						program.push(Rc::new(Sourcedata(Source::default(),
							Coredata::Internal(Commands::Call))));
						for argument in collect_pair_into_vec(arguments).iter() {
							program.push(Rc::new(Sourcedata(Source::default(),
								Coredata::Internal(Commands::Parameterize))));
							program.push(argument.clone());
						}
					},
					&Sourcedata(_, Coredata::Macro(..)) => {
						println!["Prepare macro"];
						env.params.push(vec!(arguments.clone()));
					},
					_ => {
						println!["Prepare unknown"];
						panic!("Can't prepare unknown");
					},
				}
			},
			&Sourcedata(ref source, Coredata::Internal(Commands::Call)) => {
				println!["Call"];
				match &*env.call_stack.pop().expect("YesNO") {
					&Sourcedata(_, Coredata::Function(Function::Builtin(ref transfer))) => {
						transfer(&top, &mut program, &mut env);
					},
					&Sourcedata(_, Coredata::Function(Function::Library(ref arguments, ref transfer))) => {
						unimplemented!{};
					},
					_ => {
						panic!("unknown transfer function");
					},
				}
			},
			&Sourcedata(ref source, Coredata::Symbol(ref string)) => {
				if let Some(number) = BigInt::parse_bytes(string.as_bytes(), 10) {
					println!["Atom number"];
					env.return_value = Rc::new(Sourcedata(source.clone(), Coredata::Integer(number)));
				} else {
					println!["Atom reference"];
					env.return_value = env.content.get(string).unwrap().last().unwrap().clone();
				}
			},
			other => {
				println!["Other element on stack"];
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
	fn test_interpreter() {
		let p = parse_file("input").ok().unwrap();
		interpret(p);
	}
}
