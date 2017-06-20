//! Builtins for extending Teko
//!
//! Builtins are functions and constants that can be used from the interpreter.
//! If you want to add a builtin function to the interpreter you need to create
//! a function here and add it to the library table (also in this file).
//!
//! Each function/macro is of the form `fn(program: &mut Program, env: &mut Env)`.
//! This gives virtually complete control over the system by allowing manipulation
//! of the program stack and the environment.
//!
//! The program stack `.top()` contains the next datum to be evaluated. So pushing
//! and popping operations are very useful.
//!
//! Env has three elements: `store`, `params`, and `result`. When a function is called,
//! all arguments are stored in `params`. When a macro is called, the parse tree is
//! located in `result`. Note that params is a vector of a vector, because nested function
//! calls will need to store arguments there, functioning like a stack.
//!
//! You always want to put the result of your computation inside `env.result`.
//! You don't need to clear `params` or `program` manually, that's done by the VM for you.

use std::rc::Rc;
use std::collections::HashMap;
use num::bigint::ToBigInt;
use data_structures::{Boolean, Commands, Env, Program, Sourcedata, Coredata, Macro, Function};
use utilities::*;

// //////////////////////////////////////////////////////////
// Standard Library Table
// //////////////////////////////////////////////////////////

/// Create the builtin library table.
///
/// The table contains mappings from strings to arbitrary data, functions, and macros.
/// At the start of interpreting a program this table gets loaded into memory (by `fn interpret`,
/// not `fn eval`).
/// So if you want to create a function "f" you add an entry `Function : "f" => some_name`,
/// and call it using `(f)` in Teko. You'll also need to declare the function `some_name`
/// that actually implements your functionality.
pub fn create_builtin_library_table() -> HashMap<String, Program> {
	construct_builtins! {
		// This section contains non-functions and non-macros
		{
			"true" => Coredata::Boolean(Boolean::True),
			"false" => Coredata::Boolean(Boolean::False),
		}
		// The rest of the table defines functions and macros
		Function : "+" => plus,
		Function : "-" => subtract,
		Function : "*" => multiply,
		Function : "/" => divide,
		Function : ">=" => geq,
		Function : "not" => not,
		Function : "error" => error,
		Function : "error?" => is_error,
		Function : "head" => head,
		Function : "tail" => tail,
		Function : "pair" => pair,
		Function : "sleep" => sleep,
		Function : "unwind" => unwind,
		Function : "eval" => eval_expose,
		Function : "write" => write,
		Macro : "'" => quote,
		Macro : "\"" => string,
		Macro : "if" => if_conditional,
		Macro : "set!" => set,
		Macro : "wind" => wind,
		Macro : "define" => define,
		Macro : "fn" => function,
		Macro : "mo" => make_macro,
	}
}

// //////////////////////////////////////////////////////////
// Standard Library Entries
// //////////////////////////////////////////////////////////

fn define_internal(_: &mut Program, env: &mut Env) {
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

fn define(program: &mut Program, env: &mut Env) {
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

fn divide(_: &mut Program, env: &mut Env) {
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
	env.result = Rc::new(Sourcedata(None, Coredata::Integer(sum)));
}

fn error(program: &mut Program, env: &mut Env) {
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

/// Evaluates the argument as if it's a program.
fn eval_expose(program: &mut Program, env: &mut Env) {
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
		make_unwind_with_error_message(error, program, env);
	}
}

fn function(_: &mut Program, env: &mut Env) {
	let args = env.result.clone();
	let params = collect_pair_of_symbols_into_vec_string(&args.head());
	let code = collect_pair_into_vec(&args.tail());
	env.result = Rc::new(Sourcedata(None, Coredata::Function(Function::Library(params, code))));
}

fn geq(_: &mut Program, env: &mut Env) {
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
			_ => {
				unimplemented![];
			}
		}
	}
	env.result = result;
}

fn head(program: &mut Program, env: &mut Env) {
	let error = if let Some(args) = env.params.last() {
		if args.len() != 1 {
			Some(format!["head: arity mismatch, expected 1 argument but got {}", args.len()])
		} else {
			env.result = args.first().unwrap().head().clone();
			None
		}
	} else {
		Some(format!["head: parameter stack is empty"])
	};

	if let Some(error) = error {
		make_unwind_with_error_message(&error, program, env);
	}
}

fn if_conditional(program: &mut Program, env: &mut Env) {
	let arguments = env.result.clone();
	program.push(Rc::new(Sourcedata(None,
	                                Coredata::Internal(Commands::If(arguments.tail().head(),
	                                                                arguments.tail()
		                                                                .tail()
		                                                                .head())))));
	program.push(arguments.head());
}

fn is_error(_: &mut Program, env: &mut Env) {
	if let Coredata::Error(_) = env.params.last().unwrap().first().unwrap().1 {
		env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
	} else {
		env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::False)));
	}
}

fn make_macro(_: &mut Program, env: &mut Env) {
	let args = env.result.clone();
	let params = match args.head().1 {
		Coredata::Symbol(ref string) => string.clone(),
		_ => {
			panic!("Wrong use of macro");
		}
	};
	let code = collect_pair_into_vec(&args.tail());
	env.result = Rc::new(Sourcedata(None, Coredata::Macro(Macro::Library(params, code))));
}

fn multiply(_: &mut Program, env: &mut Env) {
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
	env.result = Rc::new(Sourcedata(None, Coredata::Integer(sum)));
}


fn not(program: &mut Program, env: &mut Env) {
	let args = env.params.last().expect("Should exist by virtue of functions");
	if args.len() != 1 {
		program.push(make_unwind());
	} else {
		if let Coredata::Boolean(Boolean::False) = args.first().unwrap().1 {
			env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
		} else {
			env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::False)));
		}
	}
}

fn pair(_: &mut Program, env: &mut Env) {
	let args = env.params.last().expect("Should exist by virtue of functions");
	if args.len() != 2 {
		panic!("should have two args");
	} else {
		env.result = Rc::new(Sourcedata(None,
		                                Coredata::Pair(args.first().unwrap().clone(),
		                                               args.get(1).unwrap().clone())));
	}
}

fn plus(_: &mut Program, env: &mut Env) {
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
			_ => {
				unimplemented![];
			}
		}
	}
	env.result = Rc::new(Sourcedata(None, Coredata::Integer(sum)));
}

/// Quote elements
///
/// A builtin macro always stores the tail of the invocation inside `env.result`, so this macro is
/// empty; it doesn't need to do anything.
fn quote(_: &mut Program, _: &mut Env) {}

fn set(_: &mut Program, _: &mut Env) {
	unimplemented!();
}

fn sleep(_: &mut Program, env: &mut Env) {
	use std::{thread, time};
	use num::ToPrimitive;
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

/// Create a string
///
/// Creates a string from the given symbols by inserting single spaces inbetween each symbol.
/// TODO: Allow subexpressions; implement string interpolation and non-printable
/// character insertion.
fn string(_: &mut Program, env: &mut Env) {
	let vec = collect_pair_of_symbols_into_vec_string(&env.result);
	env.result = Rc::new(Sourcedata(None, Coredata::String(vec.join(" "))));
}

fn subtract(_: &mut Program, env: &mut Env) {
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

fn tail(_: &mut Program, env: &mut Env) {
	let args = env.params.last().expect("Should exist by virtue of functions");
	if args.len() != 1 {
		panic!("should have only a single arg");
	} else {
		env.result = args.first().unwrap().tail().clone();
	}
}

fn wind(program: &mut Program, env: &mut Env) {
	let args = env.result.clone();
	let code = collect_pair_into_vec(&args);
	program.push(Rc::new(Sourcedata(None, Coredata::Internal(Commands::Wind))));
	program.extend(code.iter().cloned());
}

fn write(_: &mut Program, env: &mut Env) {
	for i in env.params.last().unwrap() {
		println!["EU: {}", i];
	}
}
