use std::rc::Rc;
use super::VEC_CAPACITY;

use num::bigint::ToBigInt;
use num::bigint::BigInt;

use data_structures::{Boolean, Commands, Env, Program, Sourcedata, Coredata, Statement, Macro,
                      Function};

/// Quote elements
///
/// A builtin macro always stores the tail of the invocation inside `env.result`, so this macro is
/// empty; it doesn't need to do anything.
pub fn quote(_: &mut Program, _: &mut Env) {
	println!["Created quoted list"];
}

pub fn eval_expose(program: &mut Program, env: &mut Env) {
	let error: Option<_> = if let Some(args) = env.params.last() {
		if args.len() != 1 {
			Some("eval: arity mismatch")
		} else {
			if let Some(arg) = args.first() {
				program.push(arg.clone());
				None
			} else {
				Some("eval: arity mismatch")
			}
		}
	} else {
		Some("eval: parameter stack empty")
	};
	if let Some(error) = error {
		unwind_with_error_message(error, program, env);
	}
}

/// Create a string
///
/// Creates a string from the given symbols by inserting single spaces inbetween each symbol.
/// TODO: Allow subexpressions; implement string interpolation and non-printable
/// character insertion.
pub fn string(_: &mut Program, env: &mut Env) {
	let vec = collect_pair_into_vec_string(&env.result);
	env.result = Rc::new(Sourcedata(None, Coredata::String(vec.join(" "))));
	println!["Created string"];
}

pub fn wind(program: &mut Program, env: &mut Env) {
	println!["Wind macro"];
	let args = env.result.clone();
	let code = collect_pair_into_vec(&args);
	program.push(Rc::new(Sourcedata(None, Coredata::Internal(Commands::Wind))));
	program.extend(code.iter().cloned());
}

pub fn unwind(program: &mut Program, env: &mut Env) {
	println!["Unwind macro"];
	env.result = env.params.last().unwrap().last().unwrap().clone();
	while let Some(top) = program.pop() {
		match top.1 {
			Coredata::Internal(Commands::Deparameterize(ref arguments)) => {
				pop_parameters(program, env, arguments);
			}
			Coredata::Internal(Commands::Call(..)) => {
				env.params.pop();
			}
			Coredata::Internal(Commands::Wind) => {
				break;
			}
			_ => {}
		}
	}
}

pub fn make_unwind() -> Statement {
	let sub = Rc::new(Sourcedata(None, Coredata::Function(Function::Builtin(unwind))));
	Rc::new(Sourcedata(None, Coredata::Internal(Commands::Call(sub))))
}

pub fn unwind_with_error_message(string: &str, program: &mut Program, env: &mut Env) {
	let sub = Rc::new(Sourcedata(None, Coredata::String(string.into())));
	env.params.push(vec![Rc::new(Sourcedata(None, Coredata::Error(sub)))]);
	program.push(make_unwind());
}

pub fn error(program: &mut Program, env: &mut Env) {
	if let Some(args) = env.params.last() {
		if args.len() >= 2 {
			env.result =
				Rc::new(Sourcedata(None,
				                   Coredata::Error(Rc::new(Sourcedata(None,
				                                                      Coredata::String("Arity \
				                                                                        mismatch; \
				                                                                        Too \
				                                                                        many \
				                                                                        arguments \
				                                                                        to error"
					                                                      .into()))))));
			program.push(make_unwind());
		} else {
			if let Some(arg) = args.first() {
				env.result = Rc::new(Sourcedata(None, Coredata::Error(arg.clone())));
			} else {
				env.result =
					Rc::new(Sourcedata(None,
					                   Coredata::Error(Rc::new(Sourcedata(None, Coredata::Null)))));
			}
		}
	} else {
		panic!["The parameter list does not contain a list; this is an internal error that \
		        should not happen"];
	}
}

pub fn not(program: &mut Program, env: &mut Env) {
	let args = env.params.last().expect("Should exist by virtue of functions");
	if args.len() != 1 {
		program.push(make_unwind());
		println!["Should have a single arg"];
	} else {
		if let Coredata::Null = args.first().unwrap().1 {
			env.result = Rc::new(Sourcedata(None, Coredata::Symbol("true".into())));
		} else {
			env.result = Rc::new(Sourcedata(None, Coredata::Null));
		}
	}
}

pub fn head(_: &mut Program, env: &mut Env) {
	let args = env.params.last().expect("Should exist by virtue of functions");
	if args.len() != 1 {
		panic!("should have only a single arg");
	} else {
		env.result = args.first().unwrap().head().clone();
	}
}

pub fn tail(_: &mut Program, env: &mut Env) {
	let args = env.params.last().expect("Should exist by virtue of functions");
	if args.len() != 1 {
		panic!("should have only a single arg");
	} else {
		env.result = args.first().unwrap().tail().clone();
	}
	println!["Took tail"];
}

pub fn pair(_: &mut Program, env: &mut Env) {
	let args = env.params.last().expect("Should exist by virtue of functions");
	if args.len() != 2 {
		panic!("should have two args");
	} else {
		env.result = Rc::new(Sourcedata(None,
		                                Coredata::Pair(args.first().unwrap().clone(),
		                                               args.get(1).unwrap().clone())));
	}
	println!["Took tail"];
}

pub fn make_macro(_: &mut Program, env: &mut Env) {
	let args = env.result.clone();
	let params = match args.head().1 {
		Coredata::Symbol(ref string) => string.clone(),
		_ => {
			panic!("Wrong use of macro");
		}
	};
	let code = collect_pair_into_vec(&args.tail());
	env.result = Rc::new(Sourcedata(None, Coredata::Macro(Macro::Library(params, code))));
	println!["Created macro object"];
}

pub fn function(_: &mut Program, env: &mut Env) {
	let args = env.result.clone();
	let params = collect_pair_into_vec_string(&args.head());
	let code = collect_pair_into_vec(&args.tail());
	env.result = Rc::new(Sourcedata(None, Coredata::Function(Function::Library(params, code))));
	println!["Created function object"];
}

pub fn set(_: &mut Program, _: &mut Env) {
	unimplemented!();
}

pub fn define(program: &mut Program, env: &mut Env) {
	{
		let arguments = env.result.clone();
		let sub = Rc::new(Sourcedata(None, Coredata::Function(Function::Builtin(define_internal))));
		program.push(Rc::new(Sourcedata(None, Coredata::Internal(Commands::Call(sub)))));
		program.push(Rc::new(Sourcedata(None, Coredata::Internal(Commands::Parameterize))));
		match arguments.tail().1 {
			Coredata::Pair(ref heado, _) => {
				program.push(heado.clone());
			}
			_ => {
				panic!{"it cant be"};
			}
		}
		program.push(Rc::new(Sourcedata(None, Coredata::Internal(Commands::Parameterize))));
		match arguments.head().1 {
			Coredata::Symbol(ref string) => {
				program.push(Rc::new(Sourcedata(None, Coredata::String(string.clone()))));
			}
			_ => {
				panic!("Define did not get a symbol!");
			}
		}
	}
	env.params.push(vec![]);
}

pub fn if_conditional(program: &mut Program, env: &mut Env) {
	let arguments = env.result.clone();
	program.push(Rc::new(Sourcedata(None,
	                                Coredata::Internal(Commands::If(arguments.tail().head(),
	                                                                arguments.tail()
		                                                                .tail()
		                                                                .head())))));
	program.push(arguments.head());
}

pub fn define_internal(_: &mut Program, env: &mut Env) {
	let args = env.params.last().expect("Must be defined by previous macro");
	match args[0].1 {
		Coredata::String(ref string) => {
			env.store.insert(string.clone(), vec![args[1].clone()]);
		}
		_ => {
			unimplemented!();
		}
	}
}

pub fn sleep(_: &mut Program, env: &mut Env) {
	use std::{thread, time};
	use num::ToPrimitive;
	println!["Sleep func"];
	let arguments = env.params
		.last()
		.expect("The state machine should ensure this exists")
		.first()
		.expect("Srs guys");
	match arguments.1 {
		Coredata::Integer(ref value) => {
			thread::sleep(time::Duration::from_millis(value.to_u64()
				.expect("Handling non numbers not implemented yet")));
		}
		_ => {}
	}
}

pub fn geq(_: &mut Program, env: &mut Env) {
	let arguments = env.params.last().expect("The state machine should ensure this exists");
	let mut last = None;
	let mut result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
	for argument in arguments.iter() {
		match &**argument {
			&Sourcedata(_, Coredata::Complex(_)) => {
				unimplemented![];
			}
			&Sourcedata(_, Coredata::Integer(ref integer)) => {
				if let Some(previous) = last {
					if previous >= integer {
						// Do nothing
					} else {
						result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::False)));
						break;
					}
					last = Some(integer);
				} else {
					last = Some(integer);
				}
			}
			&Sourcedata(_, Coredata::Rational(_)) => {
				unimplemented![];
			}
			ref a => {
				println!["{}", a];
				unimplemented![];
			}
		}
	}
	env.result = result;
}

pub fn plus(_: &mut Program, env: &mut Env) {
	let arguments = env.params.last().expect("The state machine should ensure this exists");
	let mut sum = 0.to_bigint().expect("Constant zero should always be parsed correctly");
	for argument in arguments.iter() {
		match &**argument {
			&Sourcedata(_, Coredata::Complex(_)) => {
				unimplemented![];
			}
			&Sourcedata(_, Coredata::Integer(ref integer)) => {
				sum = sum + integer;
			}
			&Sourcedata(_, Coredata::Rational(_)) => {
				unimplemented![];
			}
			ref a => {
				println!["{}", a];
				unimplemented![];
			}
		}
	}
	env.result = Rc::new(Sourcedata(None, Coredata::Integer(sum)));
}

pub fn minus(_: &mut Program, env: &mut Env) {
	let arguments = env.params.last().expect("The state machine should ensure this exists");
	let mut sum = 0.to_bigint().expect("Constant zero should always be parsed correctly");
	if arguments.len() == 1 {
		for argument in arguments.iter() {
			match &**argument {
				&Sourcedata(_, Coredata::Complex(_)) => {
					unimplemented![];
				}
				&Sourcedata(_, Coredata::Integer(ref integer)) => {
					sum = sum - integer;
				}
				&Sourcedata(_, Coredata::Rational(_)) => {
					unimplemented![];
				}
				_ => {
					unimplemented![];
				}
			}
		}
	} else if arguments.len() > 1 {
		let mut first = true;
		for argument in arguments.iter() {
			match &**argument {
				&Sourcedata(_, Coredata::Complex(_)) => {
					unimplemented![];
				}
				&Sourcedata(_, Coredata::Integer(ref integer)) => {
					if first {
						sum = integer.clone();
					} else {
						sum = sum - integer;
					}
				}
				&Sourcedata(_, Coredata::Rational(_)) => {
					unimplemented![];
				}
				_ => {
					unimplemented![];
				}
			}
			first = false;
		}
	}
	env.result = Rc::new(Sourcedata(None, Coredata::Integer(sum)));
}

pub fn multiply(_: &mut Program, env: &mut Env) {
	let arguments = env.params.last().expect("The state machine should ensure this exists");
	let mut sum = 1.to_bigint().expect("Constant zero should always be parsed correctly");
	for argument in arguments.iter() {
		match &**argument {
			&Sourcedata(_, Coredata::Complex(_)) => {
				unimplemented![];
			}
			&Sourcedata(_, Coredata::Integer(ref integer)) => {
				sum = sum * integer;
			}
			&Sourcedata(_, Coredata::Rational(_)) => {
				unimplemented![];
			}
			_ => {
				unimplemented![];
			}
		}
	}
	println!["plus: {}", sum];
	env.result = Rc::new(Sourcedata(None, Coredata::Integer(sum)));
}

pub fn divide(_: &mut Program, env: &mut Env) {
	let arguments = env.params.last().expect("The state machine should ensure this exists");
	let mut sum = 1.to_bigint().expect("Constant zero should always be parsed correctly");
	if arguments.len() == 1 {
		for argument in arguments.iter() {
			match &**argument {
				&Sourcedata(_, Coredata::Complex(_)) => {
					unimplemented![];
				}
				&Sourcedata(_, Coredata::Integer(ref integer)) => {
					sum = sum / integer;
				}
				&Sourcedata(_, Coredata::Rational(_)) => {
					unimplemented![];
				}
				_ => {
					unimplemented![];
				}
			}
		}
	} else if arguments.len() > 1 {
		let mut first = true;
		for argument in arguments.iter() {
			match &**argument {
				&Sourcedata(_, Coredata::Complex(_)) => {
					unimplemented![];
				}
				&Sourcedata(_, Coredata::Integer(ref integer)) => {
					if first {
						sum = integer.clone();
					} else {
						sum = sum / integer;
					}
				}
				&Sourcedata(_, Coredata::Rational(_)) => {
					unimplemented![];
				}
				_ => {
					unimplemented![];
				}
			}
			first = false;
		}
	} else {
		// Arity mismatch
		unimplemented!();
	}
	println!["plus: {}", sum];
	env.result = Rc::new(Sourcedata(None, Coredata::Integer(sum)));
}

pub fn collect_pair_into_vec_string(data: &Rc<Sourcedata>) -> Vec<String> {
	let data = collect_pair_into_vec(data);
	let mut ret = vec![];
	for i in data.iter() {
		match &**i {
			&Sourcedata(_, Coredata::Symbol(ref string)) => {
				ret.push(string.clone());
			}
			_ => {
				panic!{"Not a symbol"};
			}
		}
	}
	ret.reverse();
	ret
}

pub fn collect_pair_into_vec(data: &Rc<Sourcedata>) -> Vec<Rc<Sourcedata>> {
	let mut to_return = vec![];
	let mut current = data.clone();
	loop {
		current = if let &Sourcedata(_, Coredata::Pair(ref head, ref tail)) = &*current {
			to_return.push(head.clone());
			tail.clone()
		} else {
			break;
		}
	}
	to_return.reverse();
	to_return
}

pub fn pop_parameters(_: &mut Program, env: &mut Env, args: &Vec<String>) {
	for arg in args {
		print!["{}, ", arg];
		env.store.get_mut(arg).expect("Should exist in the argument store!").pop();
		if env.store.get(arg).unwrap().is_empty() {
			env.store.remove(arg);
		}
	}
}
