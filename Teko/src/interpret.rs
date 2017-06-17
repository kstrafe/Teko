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

/// Macro::Builtin types are called with env.result as input, so (macro a b c) has env.result = (a b c).
/// Macro::Library types are called by binding input to the macro's input variable.
/// Function::Builtin types are called with env.params containing evaluated parameters.
/// Function::Library types are called with env.params containing evaluated parameters, bound to parameters.
fn deparameterize(program: &mut Program,
                  params:  &Vec<String>) -> Vec<String> {
	if let Some(top) = program.pop() {
		let mut to_add = vec![];
		match top.1 {
			Coredata::Internal(Commands::Deparameterize(ref content)) => {
				for parameter in params {
					if content.contains(parameter) {
						// Do nothing
					} else {
						to_add.push(parameter.clone());
					}
				}
				to_add.extend(content.iter().cloned());
				to_add
			},
			_ => {
				program.push(top.clone());
				params.clone()
			},
		}
	} else {
		params.clone()
	}
}

fn make_macro(top:     &Statement,
              program: &mut Program,
              env:     &mut Env) {
	let args = env.result.clone();
	let params = match args.head().1 {
		Coredata::Symbol(ref string) => {
			string.clone()
		},
		_ => {
			panic!("Wrong use of macro");
		},
	};
	let mut code = collect_pair_into_vec(&args.tail());
	code.reverse();
	env.result = Rc::new(Sourcedata(Source::default(), Coredata::Macro(Macro::Library(params, code))));
	println!["Created macro object"];
}

fn function(top:     &Statement,
            program: &mut Program,
            env:     &mut Env) {
	let args = env.result.clone();
	let params = collect_pair_into_vec_string(&args.head());
	let mut code = collect_pair_into_vec(&args.tail());
	code.reverse();
	env.result = Rc::new(Sourcedata(Source::default(), Coredata::Function(Function::Library(params, code))));
	println!["Created function object"];
}

fn set(top:     &Statement,
       program: &mut Program,
       env:     &mut Env) {
	unimplemented!();
}

fn define(top:     &Statement,
          program: &mut Program,
          env:     &mut Env) {
	println!["Inside define"];
	{
		let arguments = env.result.clone();
		program.push(Rc::new(Sourcedata(Source::default(),
		                                Coredata::Internal(Commands::Call(Rc::new(Sourcedata(Source::default(),
		                                                                                     Coredata::Function(Function::Builtin(define_internal)))))))));
		program.push(Rc::new(Sourcedata(Source::default(), Coredata::Internal(Commands::Parameterize))));
		match arguments.tail().1 {
			Coredata::Pair(ref heado, _) => {
				program.push(heado.clone());
			},
			_ => {
				panic!{"it cant be"};
			}
		}
		program.push(Rc::new(Sourcedata(Source::default(), Coredata::Internal(Commands::Parameterize))));
		match arguments.head().1 {
			Coredata::Symbol(ref string) => {
				program.push(Rc::new(Sourcedata(Source::default(), Coredata::String(string.clone()))));
			},
			_ => {
				panic!("Define did not get a symbol!");
			},
		}
	}
	env.params.push(vec!());
}

fn define_internal(top:     &Statement,
                   program: &mut Program,
                   env:     &mut Env) {
	println!["define_internal"];
	let args = env.params.last().expect("Must be defined by previous macro");
	println!["Arglen: {}", args.len()];
	println!["Arg0: {}", args[0]];
	println!["Arg1: {}", args[1]];
	match args[0].1 {
		Coredata::String(ref string) => {
			env.store.insert(string.clone(), vec!(args[1].clone()));
		},
		_ => {
			unimplemented!();
		},
	}
	println!("dfine internal {}", args.len());
}

fn sleep(top:     &Statement,
         program: &mut Program,
         env:     &mut Env) {
	use std::{thread, time};
	use num::ToPrimitive;
	let arguments = env.params.last().expect("The state machine should ensure this exists").first().expect("Srs guys");
	match arguments.1 {
		Coredata::Integer(ref value) => {
			thread::sleep(time::Duration::from_millis(value.to_u64().expect("Handling non numbers not implemented yet")));
		},
		_ => {
		},
	}
}

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
			ref a => {
				println!["{}", a];
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
	println!["Length in minus: {}", env.params.last().unwrap().len()];
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
					println!["Subtracting {}", integer];
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

fn multiply(top:     &Statement,
            program: &mut Program,
            env:     &mut Env) {
	let arguments = env.params.last().expect("The state machine should ensure this exists");
	let mut sum = 1.to_bigint().expect("Constant zero should always be parsed correctly");
	for argument in arguments.iter() {
		match &**argument {
			&Sourcedata(_, Coredata::Complex(ref complex)) => {
				unimplemented![];
			},
			&Sourcedata(_, Coredata::Integer(ref integer)) => {
				sum = sum * integer;
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

fn divide(top:     &Statement,
          program: &mut Program,
          env:     &mut Env) {
	let arguments = env.params.last().expect("The state machine should ensure this exists");
	let mut sum = 1.to_bigint().expect("Constant zero should always be parsed correctly");
	if arguments.len() == 1 {
		for argument in arguments.iter() {
			match &**argument {
				&Sourcedata(_, Coredata::Complex(ref complex)) => {
					unimplemented![];
				},
				&Sourcedata(_, Coredata::Integer(ref integer)) => {
					sum = sum / integer;
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
						sum = sum / integer;
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
	} else {
		// Arity mismatch
		unimplemented!();
	}
	println!["plus: {}", sum];
	env.result = Rc::new(Sourcedata(Source::default(), Coredata::Integer(sum)));
}

fn collect_pair_into_vec_string(data: &Rc<Sourcedata>) -> Vec<String> {
	let data = collect_pair_into_vec(data);
	let mut ret = vec!();
	for i in data.iter() {
		match &**i {
			&Sourcedata(_, Coredata::Symbol(ref string)) => {
				ret.push(string.clone());
			},
			_ => {
				panic!{"Not a symbol"};
			},
		}
	}
	ret
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
		                                              Coredata::Function(Function::Builtin(minus))))]),
		         ("*".into(), vec![Rc::new(Sourcedata(Source::default(),
		                                              Coredata::Function(Function::Builtin(multiply))))]),
		         ("/".into(), vec![Rc::new(Sourcedata(Source::default(),
		                                              Coredata::Function(Function::Builtin(divide))))]),
		         ("sleep".into(), vec![Rc::new(Sourcedata(Source::default(),
		                                                  Coredata::Function(Function::Builtin(sleep))))]),
		         ("define".into(), vec![Rc::new(Sourcedata(Source::default(),
		                                                   Coredata::Macro(Macro::Builtin(define))))]),
		         ("set!".into(), vec![Rc::new(Sourcedata(Source::default(),
		                                                 Coredata::Macro(Macro::Builtin(set))))]),
		         ("mo".into(), vec![Rc::new(Sourcedata(Source::default(),
		                                               Coredata::Macro(Macro::Builtin(make_macro))))]),
		         ("fn".into(), vec![Rc::new(Sourcedata(Source::default(),
		                                               Coredata::Macro(Macro::Builtin(function))))])].iter().cloned().collect(),
		params: Vec::with_capacity(VEC_CAPACITY),
		result: Rc::new(Sourcedata(Source::default(), Coredata::Null)),
	};
	eval(program, env);
}

/// Evaluates a program with a given environment
///
/// The `program` is considered completely evaluated when it is empty. The result of the program
/// is stored in `env.result`.
fn eval(mut program: Program, mut env: Env) {
	program.reverse(); // TODO: Do this in the parser instead, doesn't fit in here.
	macro_rules! push { ($t:tt) => { program.push(Rc::new(Sourcedata(Source::default(), Coredata::Internal(Commands::$t)))); }; }
	while let Some(top) = program.pop() {
		println!["PROGRAM LENGTH: {}", program.len()];
		println!["{}", top];
		match &*top {
			&Sourcedata(ref source, Coredata::Pair(ref head, ref tail)) => {
				println!["Expanding"];
				// TODO Use Option<Source> instead, since here we're not interested in the source of an Internal
				program.push(Rc::new(Sourcedata(tail.0.clone(), Coredata::Internal(Commands::Prepare(tail.clone())))));
				program.push(head.clone());
			},
			&Sourcedata(_, Coredata::Internal(Commands::Parameterize)) => {
				println!["Parameterize"];
				// TODO Unwrap in case of parameter failure
				env.params.last_mut().expect("The parameter stack should exist").push(env.result.clone());
			},
			&Sourcedata(ref source, Coredata::Internal(Commands::Prepare(ref arguments))) => {
				match &*env.result.clone() {
					&Sourcedata(_, Coredata::Function(..)) => {
						println!["Prepare function"];
						env.params.push(vec![]);
						program.push(Rc::new(Sourcedata(env.result.0.clone(), Coredata::Internal(Commands::Call(env.result.clone())))));
						// TODO see if this can be unrolled and made faster
						for argument in collect_pair_into_vec(arguments).iter().rev() {
							push!(Parameterize);
							program.push(argument.clone());
						}
					},
					&Sourcedata(_, Coredata::Macro(Macro::Builtin(ref transfer))) => {
						println!["Prepare macro"];
						env.result = arguments.clone();
						transfer(&top, &mut program, &mut env);
					},
					&Sourcedata(_, Coredata::Macro(Macro::Library(ref bound, ref code))) => {
						if env.store.contains_key(bound) {
							env.store.get_mut(bound).unwrap().push(arguments.clone());
						} else {
							env.store.insert(bound.clone(), vec![arguments.clone()]);
						}
						program.push(Rc::new(Sourcedata(Source::default(), Coredata::Internal(Commands::Evaluate))));
						let next = Rc::new(Sourcedata(Source::default(), Coredata::Internal(Commands::Deparameterize(deparameterize(&mut program, &vec![bound.clone()])))));
						program.push(next);
						program.extend(code.iter().cloned());
					},
					_ => {
						println!["Prepare unknown"];
						panic!("Can't prepare unknown");
					},
				}
			},
			&Sourcedata(ref source, Coredata::Internal(Commands::Evaluate)) => {
				program.push(env.result.clone());
			},
			&Sourcedata(ref source, Coredata::Internal(Commands::Call(ref statement))) => {
				println!["Call"];
				match &**statement {
					&Sourcedata(_, Coredata::Function(Function::Builtin(ref transfer))) => {
						transfer(&top, &mut program, &mut env);
						env.params.pop();
					},
					&Sourcedata(_, Coredata::Function(Function::Library(ref arguments, ref transfer))) => {
						let mut counter = 0;
						for arg in arguments.iter() {
							if env.store.contains_key(arg) {
								env.store.get_mut(arg).unwrap().push(env.params.last().unwrap()[counter].clone());
							} else {
								env.store.insert(arg.clone(), vec![env.params.last().unwrap()[counter].clone()]);
							}
							counter += 1;
						}
						env.params.pop();
						let next = Rc::new(Sourcedata(Source::default(), Coredata::Internal(Commands::Deparameterize(deparameterize(&mut program, arguments)))));
						program.push(next);
						// program.push(Rc::new(Sourcedata(Source::default(), Coredata::Internal(Commands::Deparameterize(arguments.clone())))));
						program.extend(transfer.iter().cloned());
					},
					_ => {
						panic!("unknown transfer function");
					},
				}
			},
			&Sourcedata(ref source, Coredata::Internal(Commands::Deparameterize(ref arguments))) => {
				print!["Deparameterizing: "];
				for arg in arguments {
					print!["{}, ", arg];
					env.store.get_mut(arg).expect("Should exist in the argument store!").pop();
					if env.store.get(arg).unwrap().is_empty() {
						env.store.remove(arg);
					}
				}
				println![""];
			},
			&Sourcedata(ref source, Coredata::Symbol(ref string)) => {
				print!["Atom: "];
				if let Some(number) = BigInt::parse_bytes(string.as_bytes(), 10) {
					println!["number"];
					env.result = Rc::new(Sourcedata(source.clone(), Coredata::Integer(number)));
				} else {
					println!["reference"];
					env.result = env.store.get(string).unwrap().last().unwrap().clone();
				}
			},
			other => {
				println!["Other"];
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
