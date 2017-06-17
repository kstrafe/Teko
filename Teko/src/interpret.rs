use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;
use super::VEC_CAPACITY;

use num::bigint::ToBigInt;
use num::bigint::BigInt;
use num::rational::BigRational;
use num::Complex;
use num::FromPrimitive;

use data_structures::{Commands, Env, Source, Program, Sourcedata,
                      Coredata, Statement, Macro, Function};

fn plus(top:     &Statement,
        program: &mut Program,
        env:     &mut Env) {
	let arguments = env.params.last().expect("The state machine should ensure this exists");
	let mut sum = 0.to_bigint().expect("Constant zero should always be parsed correctly");
	for argument in arguments.iter() {
		match &**argument {
			&Sourcedata(_, Coredata::Complex(ref complex)) => {
				unimplemented![];
			},
			&Sourcedata(_, Coredata::Integer(ref integer)) => {
				sum = sum + integer;
			},
			&Sourcedata(_, Coredata::Rational(ref rational)) => {
				unimplemented![];
			},
			_ => {
				unimplemented![];
			},
		}
	}
	println!["plus: {}", sum];
	env.result = Rc::new(Sourcedata(Source::default(), Coredata::Integer(sum)));
}

fn minus(top:     &Statement,
         program: &mut Program,
         env:     &mut Env) {
	let arguments = env.params.last().expect("The state machine should ensure this exists");
	let mut sum = 0.to_bigint().expect("Constant zero should always be parsed correctly");
	if arguments.len() == 1 {
		for argument in arguments.iter() {
			match &**argument {
				&Sourcedata(_, Coredata::Complex(ref complex)) => {
					unimplemented![];
				},
				&Sourcedata(_, Coredata::Integer(ref integer)) => {
					sum = sum - integer;
				},
				&Sourcedata(_, Coredata::Rational(ref rational)) => {
					unimplemented![];
				},
				_ => {
					unimplemented![];
				},
			}
		}
	} else if arguments.len() > 1 {
		let mut first = true;
		for argument in arguments.iter() {
			match &**argument {
				&Sourcedata(_, Coredata::Complex(ref complex)) => {
					unimplemented![];
				},
				&Sourcedata(_, Coredata::Integer(ref integer)) => {
					if first {
						sum = integer.clone();
					} else {
						sum = sum - integer;
					}
				},
				&Sourcedata(_, Coredata::Rational(ref rational)) => {
					unimplemented![];
				},
				_ => {
					unimplemented![];
				},
			}
			first = false;
		}
	}
	println!["minus: {}", sum];
	env.result = Rc::new(Sourcedata(Source::default(), Coredata::Integer(sum)));
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
		store:  [("+".into(), vec![Rc::new(Sourcedata(Source::default(),
		                                              Coredata::Function(Function::Builtin(plus))))]),
		         ("-".into(), vec![Rc::new(Sourcedata(Source::default(),
		                                              Coredata::Function(Function::Builtin(minus))))])].iter().cloned().collect(),
		calls:  Vec::with_capacity(VEC_CAPACITY),
		params: Vec::with_capacity(VEC_CAPACITY),
		result: Rc::new(Sourcedata(Source::default(), Coredata::Null)),
	};
	eval(program, env);
}

fn eval(mut program: Program, mut env: Env) {
	program.reverse();
	macro_rules! push { ($t:tt) => { program.push(Rc::new(Sourcedata(Source::default(), Coredata::Internal(Commands::$t)))); }; }
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
				env.params.last_mut().expect("Guaranteed to exist").push(env.result.clone());
			},
			&Sourcedata(ref source, Coredata::Internal(Commands::Prepare(ref arguments))) => {
				match &*env.result {
					&Sourcedata(_, Coredata::Function(..)) => {
						println!["Prepare function"];
						env.params.push(vec!());
						env.calls.push(env.result.clone());
						program.push(Rc::new(Sourcedata(Source::default(), Coredata::Internal(Commands::Call(env.result.clone())))));
						for argument in collect_pair_into_vec(arguments).iter().rev() {
							push!(Parameterize);
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
			&Sourcedata(ref source, Coredata::Internal(Commands::Call(ref statement))) => {
				println!["Call"];
				match &**statement {
					&Sourcedata(_, Coredata::Function(Function::Builtin(ref transfer))) => {
						transfer(&top, &mut program, &mut env);
						env.params.pop();
					},
					&Sourcedata(_, Coredata::Function(Function::Library(ref arguments, ref transfer))) => {
						// Not finished
						program.extend(collect_pair_into_vec(transfer));
						env.params.pop();
					},
					_ => {
						panic!("unknown transfer function");
					},
				}
			},
			&Sourcedata(ref source, Coredata::Symbol(ref string)) => {
				if let Some(number) = BigInt::parse_bytes(string.as_bytes(), 10) {
					println!["Atom number"];
					env.result = Rc::new(Sourcedata(source.clone(), Coredata::Integer(number)));
				} else {
					println!["Atom reference"];
					env.result = env.store.get(string).unwrap().last().unwrap().clone();
				}
			},
			other => {
				println!["Other element on stack"];
				env.result = top.clone();
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
