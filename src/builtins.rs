//! Builtins for extending Teko
//!
//! Builtins are functions and constants that can be used from the interpreter.
//! If you want to add a builtin function to the interpreter you need to create
//! a function here and add it to the library table (also in this file).
//!
//! Each function/macro is of the form `fn(program: &mut Program, env: &mut Env) -> Option<String>`.
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
use num::{zero, one};
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
		Function : "=" => eq,
		Function : "<" => lt,
		Function : "and" => and,
		Function : "or" => or,
		Function : "@trace" => trace,
		Function : "exit" => exit,
		Function : "not" => not,
		Function : "error" => error,
		Function : "same?" => is_symbol_eq,
		Function : "error?" => is_error,
		Function : "symbol?" => is_symbol,
		Function : "head" => head,
		Function : "tail" => tail,
		Function : "pair" => pair,
		Function : "pair?" => is_pair,
		Function : "@sleep" => sleep,
		Function : "unwind" => unwind,
		Function : "@variable-count" => at_variable_count,
		Function : "@program-count" => at_program_count,
		Function : "eval" => eval_expose,
		Function : "write" => write,
		Macro : "quote" => quote,
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

fn and(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		for arg in args {
			if let Coredata::Boolean(Boolean::False) = arg.1 {
				env.result = arg.clone();
				return None;
			} else {
				continue;
			}
		}
	};
	env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
	None
}
/// Count the stack size. Useful for checking if Tail Call Optimization works.
fn at_program_count(program: &mut Program, env: &mut Env) -> Option<String> {
	let count = program.len();
	env.result = Rc::new(Sourcedata(None, Coredata::Integer(count.into())));
	None
}

/// Count the amount of active variables in the program.
fn at_variable_count(_: &mut Program, env: &mut Env) -> Option<String> {
	let mut count = 0;
	for i in &env.params {
		count += i.len();
	}
	for (_, values) in &env.store {
		count += values.len();
	}
	env.result = Rc::new(Sourcedata(None, Coredata::Integer(count.into())));
	None
}

fn define_internal(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		match args[0].1 {
			Coredata::String(ref string) => {
				env.store.insert(string.clone(), vec![args[1].clone()]);
			}
			_ => {
				panic!["define_internal error: params doesn't contain a string"];
			}
		}
	} else {
		panic!["define_internal error: params is empty"];
	}
	None
}

fn define(program: &mut Program, env: &mut Env) -> Option<String> {
	{
		let arguments = env.result.clone();
		let sub = Rc::new(Sourcedata(None, Coredata::Function(Function::Builtin(define_internal))));
		program.push(Rc::new(Sourcedata(None, Coredata::Internal(Commands::Call(sub)))));
		program.push(Rc::new(Sourcedata(None, Coredata::Internal(Commands::Parameterize))));
		match arguments.tail().1 {
			Coredata::Pair(ref heado, _) => {
				program.push(heado.clone());
			}
			Coredata::Null => {
				unwind_with_error_message("", program, env);
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
	None
}

fn divide(_: &mut Program, env: &mut Env) -> Option<String> {
	let arguments = env.params
		.last()
		.expect("The state machine should ensure this exists");
	let mut sum = one();
	if arguments.len() == 1 {
		for argument in arguments.iter() {
			match &**argument {
				&Sourcedata(_, Coredata::Integer(ref value)) => {
					sum = sum / value;
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
				&Sourcedata(_, Coredata::Integer(ref value)) => {
					if first {
						sum = value.clone();
					} else {
						sum = sum / value;
					}
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
	None
}

fn eq(_: &mut Program, env: &mut Env) -> Option<String> {
	let arguments = env.params
		.last()
		.expect("The state machine should ensure this exists");
	let mut last = None;
	let mut result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
	for argument in arguments.iter() {
		match &**argument {
			&Sourcedata(_, Coredata::Integer(ref integer)) => {
				if let Some(previous) = last {
					if previous == integer {
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
			_ => {
				unimplemented![];
			}
		}
	}
	env.result = result;
	None
}

fn error(program: &mut Program, env: &mut Env) -> Option<String> {
	let error = if let Some(args) = env.params.last() {
		if args.len() >= 2 {
			Some("Arity mismatch; Too many arguments to error")
		} else {
			if let Some(arg) = args.first() {
				env.result = Rc::new(Sourcedata(None, Coredata::Error(arg.clone())));
				None
			} else {
				env.result =
					Rc::new(Sourcedata(None,
					                   Coredata::Error(Rc::new(Sourcedata(None, Coredata::Null)))));
				None
			}
		}
	} else {
		Some("The parameter list does not contain a list; this is an internal error that should \
		      not happen")
	};
	if let Some(error) = error {
		unwind_with_error_message(error, program, env);
	}
	None
}

/// Evaluates the argument as if it's a program.
fn eval_expose(program: &mut Program, env: &mut Env) -> Option<String> {
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
	None
}

fn exit(program: &mut Program, env: &mut Env) -> Option<String> {
	use num::ToPrimitive;

	let error = if let Some(args) = env.params.last() {
		if args.len() <= 1 {
			if let Some(arg) = args.last() {
				match arg.1 {
					Coredata::Integer(ref value) => {
						if let Some(value) = value.to_i32() {
							::std::process::exit(value);
						} else {
							panic!["Unable to determine exit code"];
						}
					}
					_ => Some("exit: argument not integer".into()),
				}
			} else {
				::std::process::exit(0);
			}
		} else {
			Some(format!["exit: arity error, expecting 0 or 1 arguments, got {}", args.len()])
		}
	} else {
		Some("parameter stack not present for a call".into())
	};
	if let Some(error) = error {
		unwind_with_error_message(&error, program, env);
	}
	None
}

fn function(_: &mut Program, env: &mut Env) -> Option<String> {
	let args = env.result.clone();
	let params = collect_pair_of_symbols_into_vec_string(&args.head());
	let code = collect_pair_into_vec(&args.tail());
	env.result = Rc::new(Sourcedata(None, Coredata::Function(Function::Library(params, code))));
	None
}

fn head(program: &mut Program, env: &mut Env) -> Option<String> {
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
		unwind_with_error_message(&error, program, env);
	}
	None
}

fn if_conditional(program: &mut Program, env: &mut Env) -> Option<String> {
	let arguments = env.result.clone();
	program.push(Rc::new(Sourcedata(None,
	                                Coredata::Internal(Commands::If(arguments.tail().head(),
	                                                                arguments.tail()
		                                                                .tail()
		                                                                .head())))));
	program.push(arguments.head());
	None
}

fn is_error(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Coredata::Error(_) = env.params.last().unwrap().first().unwrap().1 {
		env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
	} else {
		env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::False)));
	}
	None
}

fn is_pair(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Coredata::Pair(..) = env.params.last().unwrap().first().unwrap().1 {
		env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
	} else {
		env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::False)));
	}
	None
}

fn is_symbol_eq(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Coredata::Symbol(ref a) = env.params.last().unwrap().first().unwrap().1 {
		if let Coredata::Symbol(ref b) = env.params.last().unwrap()[1].1 {
			if a == b {
				env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
				return None;
			}
		}
	}
	env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::False)));
	None
}

fn is_symbol(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Coredata::Symbol(_) = env.params.last().unwrap().first().unwrap().1 {
		env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
	} else {
		env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::False)));
	}
	None
}

/// The less-than function, traditionally named
fn lt(_: &mut Program, env: &mut Env) -> Option<String> {
	let arguments = env.params
		.last()
		.expect("The state machine should ensure this exists");
	let mut last = None;
	let mut result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
	for argument in arguments.iter() {
		match &**argument {
			&Sourcedata(_, Coredata::Integer(ref integer)) => {
				if let Some(previous) = last {
					if previous < integer {
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
			_ => {
				unimplemented![];
			}
		}
	}
	env.result = result;
	None
}

fn make_macro(_: &mut Program, env: &mut Env) -> Option<String> {
	let args = env.result.clone();
	let params = match args.head().1 {
		Coredata::Symbol(ref string) => string.clone(),
		_ => {
			panic!("Wrong use of macro");
		}
	};
	let code = collect_pair_into_vec(&args.tail());
	env.result = Rc::new(Sourcedata(None, Coredata::Macro(Macro::Library(params, code))));
	None
}

fn multiply(_: &mut Program, env: &mut Env) -> Option<String> {
	let arguments = env.params
		.last()
		.expect("The state machine should ensure this exists");
	let mut sum = one();
	for argument in arguments.iter() {
		match &**argument {
			&Sourcedata(_, Coredata::Integer(ref value)) => {
				sum = sum * value;
			}
			_ => {
				unimplemented![];
			}
		}
	}
	env.result = Rc::new(Sourcedata(None, Coredata::Integer(sum)));
	None
}

fn not(program: &mut Program, env: &mut Env) -> Option<String> {
	let error = if let Some(args) = env.params.last() {
		if args.len() != 1 {
			Some("not: arity mismatch")
		} else {
			if let Coredata::Boolean(Boolean::False) = args.first().unwrap().1 {
				env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
			} else {
				env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::False)));
			}
			None
		}
	} else {
		Some("not: parameter stack corrupted")
	};
	if let Some(error) = error {
		unwind_with_error_message(error, program, env);
	}
	None
}

fn or(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		for arg in args {
			if let Coredata::Boolean(Boolean::False) = arg.1 {
				continue;
			} else {
				env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
				return None;
			}
		}
	};
	env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::False)));
	None
}

fn pair(_: &mut Program, env: &mut Env) -> Option<String> {
	let args = env.params
		.last()
		.expect("Should exist by virtue of functions");
	if args.len() != 2 {
		panic!("should have two args");
	} else {
		env.result = Rc::new(Sourcedata(None,
		                                Coredata::Pair(args.first().unwrap().clone(),
		                                               args.get(1).unwrap().clone())));
	}
	None
}

fn plus(_: &mut Program, env: &mut Env) -> Option<String> {
	let arguments = env.params
		.last()
		.expect("The state machine should ensure this exists");
	let mut sum = zero(); // BigInt::from_slice(Sign::NoSign, &[0]);
	for argument in arguments.iter() {
		match &**argument {
			&Sourcedata(_, Coredata::Integer(ref value)) => {
				sum = sum + value;
			}
			_ => {
				return Some("+: type error".into());
			}
		}
	}
	env.result = Rc::new(Sourcedata(None, Coredata::Integer(sum)));
	None
}

/// Quote elements
///
/// A builtin macro always stores the tail of the invocation inside `env.result`, so this macro is
/// empty; it doesn't need to do anything.
fn quote(_: &mut Program, _: &mut Env) -> Option<String> {
	None
}

fn set(_: &mut Program, _: &mut Env) -> Option<String> {
	unimplemented!();
}

fn sleep(_: &mut Program, env: &mut Env) -> Option<String> {
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
	None
}

/// Create a string
///
/// Creates a string from the given symbols by inserting single spaces inbetween each symbol.
/// TODO: Allow subexpressions; implement string interpolation and non-printable
/// character insertion.
fn string(_: &mut Program, env: &mut Env) -> Option<String> {
	let vec = collect_pair_of_symbols_into_vec_string(&env.result);
	env.result = Rc::new(Sourcedata(None, Coredata::String(vec.join(" "))));
	None
}

fn subtract(_: &mut Program, env: &mut Env) -> Option<String> {
	let arguments = env.params
		.last()
		.expect("The state machine should ensure this exists");
	let mut sum = zero();
	if arguments.len() == 1 {
		for argument in arguments.iter() {
			match &**argument {
				&Sourcedata(_, Coredata::Integer(ref value)) => {
					sum = sum - value;
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
				&Sourcedata(_, Coredata::Integer(ref value)) => {
					if first {
						sum = value.clone();
					} else {
						sum = sum - value;
					}
				}
				_ => {
					unimplemented![];
				}
			}
			first = false;
		}
	}
	env.result = Rc::new(Sourcedata(None, Coredata::Integer(sum)));
	None
}

fn tail(_: &mut Program, env: &mut Env) -> Option<String> {
	let args = env.params
		.last()
		.expect("Should exist by virtue of functions");
	if args.len() != 1 {
		panic!("should have only a single arg");
	} else {
		env.result = args.first().unwrap().tail().clone();
	}
	None
}

/// Print a stack trace.
///
/// The stack trace will not print tail call optimized calls, so there may
/// be some calls missing here. Since the requirement is for the program
/// to be unbounded in the amount of tail calls, there's no way to definitively
/// store all calls.
pub fn trace(program: &mut Program, _: &mut Env) -> Option<String> {
	for i in program.iter().rev() {
		if let &Sourcedata(Some(ref source), Coredata::Internal(Commands::Deparameterize(..))) =
		       &**i {
			println!["{}", source];
		}
	}
	None
}

fn wind(program: &mut Program, env: &mut Env) -> Option<String> {
	let args = env.result.clone();
	let code = collect_pair_into_vec(&args);
	program.push(Rc::new(Sourcedata(None, Coredata::Internal(Commands::Wind))));
	program.extend(code.iter().cloned());
	None
}

fn write(_: &mut Program, env: &mut Env) -> Option<String> {
	for i in env.params.last().unwrap() {
		println!["write: {}", i];
	}
	None
}
