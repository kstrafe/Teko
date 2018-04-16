//! Builtins for extending Teko
//!
//! Builtins are functions and constants that can be used from the interpreter.
//! If you want to add a builtin function to the interpreter you need to create
//! a function here and add it to the library table (also in this file).
//!
//! Each function/macro is of the form `fn(program: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)>`.
//! This gives virtually complete control over the system by allowing manipulation
//! of the program stack and the environment.
//!
//! The program stack `.top()` contains the next datum to be evaluated. So pushing
//! and popping operations are very useful.
//!
//! `Env` has three elements: `store`, `params`, and `result`. When a function is called,
//! all args are stored in `params`. When a macro is called, the parse tree is
//! located in `result`. Note that params is a vector of a vector, because nested function
//! calls will need to store args there, functioning like a stack.
//!
//! You always want to put the result of your computation inside `env.result`.
//! You don't need to clear `params` or `program` manually, that's done by the VM for you.


// //////////////////////////////////////////////////////////
// std imports
// //////////////////////////////////////////////////////////
use std::char;
use std::collections::HashMap;
use std::io::{self, Read};
use std::rc::Rc;
use std::{time, thread};
use std::usize;

// //////////////////////////////////////////////////////////
// Internal data structures used by Teko
// //////////////////////////////////////////////////////////
use data_structures::*;
use parse::*;
use user::*;
use utilities::*;

// //////////////////////////////////////////////////////////
// External libraries
// //////////////////////////////////////////////////////////
use num::{BigInt, one, ToPrimitive, zero};

// //////////////////////////////////////////////////////////
// Standard Library Table
// //////////////////////////////////////////////////////////

const HELP: &str = "To see all current variables in scope enter: (@variables)
to exit press CTRL-D, CTRL-C, or: (exit)";

/// Create the builtin library table.
///
/// The table contains mappings from strings to arbitrary data, functions, and macros.
/// At the start of interpreting a program this table gets loaded into memory (by `fn interpret`,
/// not `fn eval`).
/// So if you want to create a function "f" you add an entry `Function : "f" => some_name`,
/// and call it using `(f)` in Teko. You'll also need to declare the function `some_name`
/// that actually implements your functionality.
///
/// For user-defined functions and types please see `user/mod.rs`.
pub fn create_builtin_library_table() -> HashMap<String, Program> {
	construct_builtins! {
		// This section contains non-functions and non-macro
		// constants
		{
			"help" => Coredata::String(HELP.into()),
		}
		// The rest of the table defines functions and macros
		// Numerics
		Function : "+" => plus,
		Function : "-" => subtract,
		Function : "*" => multiply,
		Function : "/" => divide,
		Function : "=" => eq,
		Function : "<" => lt,
		Function : ">" => gt,
		// Boolean logic
		Function : "and" => and,
		Function : "or" => or,
		Function : "not" => not,
		// Error handling
		Function : "error" => error,
		Function : "error?" => is_error,
		Macro    : "wind" => wind,
		Function : "unwind" => unwind,
		// Lisp primitives
		Macro    : "if" => if_conditional,
		Macro    : "quote" => quote,
		Function : "same?" => is_data_eq,
		Function : "symbol?" => is_symbol,
		Function : "head" => head,
		Function : "tail" => tail,
		Function : "cell" => cell,
		Function : "cell?" => is_cell,
		Macro    : "function" => function,
		Macro    : "macro" => make_macro,
		// Some useful features
		Macro    : "define" => define,
		Macro    : "local" => local,
		Macro    : "set!" => set,
		Macro    : "program" => program,
		Function : "read" => read,
		Function : "eval" => eval_expose,
		Function : "list" => list,
		Function : "len" => list_length,
		Function : "->string" => to_string,
		Function : "write" => write,
		Function : "print" => print,
		Function : "doc" => doc,
		Macro    : "\"" => string,
		Function : "exit" => exit,
		Function : "function-code" => function_code,
		Function : "function-parameters" => function_parameters,
		Function : "load" => load,
		Function : "current-time-milliseconds" => current_time_milliseconds,
		// Useful builtins
		Function : "@program-count" => at_program_count,
		Function : "@msleep" => msleep,
		Function : "@trace" => trace,
		Function : "@variable-count" => at_variable_count,
		Function : "@variables" => at_variables,
	}
}

// //////////////////////////////////////////////////////////
// Standard Library Entries
// //////////////////////////////////////////////////////////

macro_rules! teko_simple_function {
	($name:ident $args:ident : $low:expr => $high:expr => $code:block) => {
		#[allow(unused_comparisons)]
		#[allow(redundant_closure_call)]
		fn $name(_: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
			if let Some($args) = env.params.last() {
				if $args.len() < $low || $args.len() > $high {
					return Some((None, arity_mismatch($low, $high, $args.len())));
				}
				let result = (|| $code)();
				match result {
					Ok(result) => {
						env.result = result;
						None
					}
					Err((source, error)) => Some((source, error)),
				}
			} else {
				Some((None, "fatal: parameter stack empty".into()))
			}
		}
	};
}

macro_rules! teko_simple_macro {
	($name:ident $arg:ident : $low:expr => $high:expr => $code:block) => {
		#[allow(unused_comparisons)]
		#[allow(redundant_closure_call)]
		fn $name(_: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
			let $arg = env.result.clone();
			let len = $arg.len();
			if let Some(len) = len {
				if len < $low || len > $high {
					return Some((None, arity_mismatch($low, $high, len)));
				}
			} else {
				return Some((None, "macro: input not Cell or Null()".into()));
			}
			let result = (|| $code)();
			match result {
				Ok(result) => {
					env.result = result;
					None
				}
				Err((source, error)) => Some((source, error)),
			}
		}
	};
}

macro_rules! extype {
	($src:expr, $($expected:ident) or *, $data:expr) => {
		{
			$(
				if let Sourcedata(_, Coredata::$expected(..)) = *$data.clone() {
					assert![false];
				}
			)*
			($src.clone(), format!["expected {} but got {}", stringify![$($expected) or *], data_name(&$data)])
		}
	};
}

/// Logical AND.
teko_simple_function!(and args : 0 => usize::MAX => {
	for arg in args {
		if let Coredata::Boolean(false) = arg.1 {
			return Ok(arg.clone());
		} else {
			continue;
		}
	}
	Ok(rcs(Coredata::Boolean(true)))
});

/// Count the stack size. Useful for checking if Tail Call Optimization works.
fn at_program_count(program: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
	let count = program.len();
	env.result = rcs(Coredata::Integer(count.into()));
	None
}

/// Count the amount of active variables in the program.
fn at_variable_count(_: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
	let mut count = 0;
	for i in &env.params {
		count += i.len();
	}
	for values in env.store.values() {
		count += values.len();
	}
	env.result = rcs(Coredata::Integer(count.into()));
	None
}

/// Find all active variables in the dynamic scope.
fn at_variables(_: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
	env.result = rcs(Coredata::Null());
	for key in env.store.keys() {
		env.result = rcs(Coredata::Cell(
			rcs(Coredata::Symbol(Symbol::from(key))),
			env.result.clone(),
		));
	}
	None
}

/// Used by define to perform the final step of assigning.
fn define_internal(_: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
	if let Some(args) = env.params.last() {
		if let Some(symbol) = args.first() {
			match **symbol {
				Sourcedata(ref source, Coredata::String(ref string)) => {
					if let Some(rhs) = args.get(1) {
						if env.store.contains_key(string) {
							return Some((
								source.clone(),
								format!["variable already exists: {}", string],
							));
						}
						env.store.insert(string.clone(), vec![rhs.clone()]);
					} else {
						return Some((source.clone(), arity_mismatch(2, 2, 1)));
					}
				}
				Sourcedata(ref source, ..) => {
					return Some(extype![source, String, symbol]);
				}
			}
		} else {
			return Some((None, arity_mismatch(2, 2, 0)));
		}
	} else {
		return Some((None, "no arg stack".into()));
	}
	None
}

/// Define a local variable by pushing and deparameterizing
fn local(program: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
	{
		let args = env.result.clone();
		let sub = rcs(Coredata::Function(Function::Builtin(
			local_internal,
			"@local-internal".into(),
		)));
		let push = if let Some(ref tail) = args.tail() {
			match tail.1 {
				Coredata::Cell(ref head, _) => {
					vec![
						rcs(Coredata::Internal(Commands::Call(sub))),
						rcs(Coredata::Internal(Commands::Param)),
						head.clone(),
					]
				}
				Coredata::Null() => {
					return Some((None, arity_mismatch(2, 2, 1)));
				}
				_ => {
					return Some((None, format!["expecting Cell but got: {}", tail]));
				}
			}
		} else {
			return Some((None, arity_mismatch(2, 2, 0)));
		};
		if let Some(head) = args.head() {
			match *head {
				Sourcedata(ref source, Coredata::Symbol(ref symbol)) => {
					program.extend(push);
					program.push(rc(Sourcedata(
						source.clone(),
						Coredata::Internal(Commands::Param),
					)));
					let t: &str = symbol.into();
					program.push(rc(
						Sourcedata(source.clone(), Coredata::String(t.to_string())),
					));
				}
				Sourcedata(ref source, ..) => {
					return Some(extype![source, Symbol, head]);
				}
			}
		} else {
			return Some((None, arity_mismatch(2, 2, 1)));
		}
	}
	env.params.push(vec![]);
	None
}

/// Used by define to perform the final step of assigning.
fn local_internal(program: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
	if let Some(args) = env.params.last() {
		if let Some(symbol) = args.first() {
			match **symbol {
				Sourcedata(ref source, Coredata::String(ref string)) => {
					if let Some(rhs) = args.get(1) {
						// Find earliest Depar
						// Problem is what if we're inside a new function?
						// That's fine, since we have a new depar
						if let Some(depar) = find_earliest_depar(program) {
							if depar.contains(string) {
								env.store.get_mut(string).unwrap().push(rhs.clone());
								// Just overwrite them. It's fine
							} else {
								depar.push(string.clone());
								if env.store.contains_key(string) {
									env.store.get_mut(string).unwrap().push(rhs.clone());
								} else {
									env.store.insert(string.clone(), vec![rhs.clone()]);
								}
							}
						} else if env.store.contains_key(string) {
								return Some((
									source.clone(),
									format!["variable already exists: {}", string],
								));
						} else {
							env.store.insert(string.clone(), vec![rhs.clone()]);
						}
					} else {
						return Some((source.clone(), arity_mismatch(2, 2, 1)));
					}
				}
				Sourcedata(ref source, ..) => {
					return Some(extype![source, String, symbol]);
				}
			}
		} else {
			return Some((None, arity_mismatch(2, 2, 0)));
		}
	} else {
		return Some((None, "no arg stack".into()));
	}
	None
}

/// Define a variable to be some value.
fn define(program: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
	{
		let args = env.result.clone();
		let sub = rcs(Coredata::Function(Function::Builtin(
			define_internal,
			"@define-internal".into(),
		)));
		let push = if let Some(ref tail) = args.tail() {
			match tail.1 {
				Coredata::Cell(ref head, _) => {
					vec![
						rcs(Coredata::Internal(Commands::Call(sub))),
						rcs(Coredata::Internal(Commands::Param)),
						head.clone(),
					]
				}
				Coredata::Null() => {
					return Some((None, arity_mismatch(2, 2, 1)));
				}
				_ => {
					return Some((None, format!["expecting Cell but got: {}", tail]));
				}
			}
		} else {
			return Some((None, arity_mismatch(2, 2, 0)));
		};
		if let Some(head) = args.head() {
			match *head {
				Sourcedata(ref source, Coredata::Symbol(ref symbol)) => {
					program.extend(push);
					program.push(rc(Sourcedata(
						source.clone(),
						Coredata::Internal(Commands::Param),
					)));
					let t: &str = symbol.into();
					program.push(rc(
						Sourcedata(source.clone(), Coredata::String(t.to_string())),
					));
				}
				Sourcedata(ref source, ..) => {
					return Some(extype![source, Symbol, head]);
				}
			}
		} else {
			return Some((None, arity_mismatch(2, 2, 1)));
		}
	}
	env.params.push(vec![]);
	None
}

/// Mathematical division of integers.
teko_simple_function!(divide args : 1 => usize::MAX => {
	let mut sum = one();
	if args.len() == 1 {
		for arg in args.iter() {
			match **arg {
				Sourcedata(ref src, Coredata::Integer(ref value)) => {
					if value == &zero::<BigInt>() {
						return Err((src.clone(), "argument is zero".into()));
					}
					sum = sum / value;
				}
				Sourcedata(ref src, ..) => {
					return Err(extype![src, Integer, arg]);
				}
			}
		}
	} else if args.len() > 1 {
		let mut first = true;
		for arg in args.iter() {
			match **arg {
				Sourcedata(ref src, Coredata::Integer(ref value)) => {
					if first {
						sum = value.clone();
					} else {
						if value == &zero::<BigInt>() {
							return Err((src.clone(), "argument is zero".into()));
						}
						sum = sum / value;
					}
				}
				Sourcedata(ref src, ..) => {
					return Err(extype![src, Integer, arg]);
				}
			}
			first = false;
		}
	}
	Ok(rcs(Coredata::Integer(sum)))
});

/// Retrieve the first statement of a function or macro.
teko_simple_function!(doc args : 1 => 1 => {
	let arg = args.first().unwrap();
	match **arg {
		Sourcedata(_, Coredata::Function(Function::Library(_, ref stats))) |
		Sourcedata(_, Coredata::Macro(Macro::Library(_, ref stats))) => {
			if stats.is_empty() {
				Ok(rcs(Coredata::Null()))
			} else {
				Ok(stats.last().unwrap().clone())
			}
		}
		Sourcedata(ref src, ..) => {
			Err(extype![src, Function, arg])
		}
	}
});

/// Integer equality comparison.
teko_simple_function!(eq args : 0 => usize::MAX => {
	let mut last = None;
	let mut result = rcs(Coredata::Boolean(true));
	for arg in args.iter() {
		match **arg {
			Sourcedata(_, Coredata::Integer(ref integer)) => {
				if let Some(previous) = last {
					if previous == integer {
						// Do nothing
					} else {
						result = rcs(Coredata::Boolean(false));
						break;
					}
					last = Some(integer);
				} else {
					last = Some(integer);
				}
			}
			Sourcedata(ref src, ..) => {
				return Err(extype![src, Integer, arg])
			}
		}
	}
	Ok(result)
});

/// Error constructor.
///
/// Error is its own type in Teko.
teko_simple_function!(error args : 0 => 1 => {
	if let Some(arg) = args.first() {
		Ok(rcs(Coredata::Error(arg.clone())))
	} else {
		Ok(rcs(Coredata::Error(rcs(Coredata::Null()))))
	}
});

teko_simple_function!(function_code args : 1 => 1 => {
	use utilities::program_to_cells;
	let mut top = rcs(Coredata::Null());
	match **args.first().unwrap() {
		Sourcedata(ref src, Coredata::Function(Function::Builtin(..))) => {
			return Err((src.clone(), format!["expected Function but got {}", data_name(args.first().unwrap())]));
		}
		Sourcedata(ref src, Coredata::Function(Function::Library(ref params, ref program))) => {
			top = program_to_cells(program);
		}
		Sourcedata(ref src, ..) => {
			return Err(extype![src, Function, args.first().unwrap()]);
		}
	}
	Ok(top)
});

teko_simple_function!(function_parameters args : 1 => 1 => {
	use utilities::program_to_cells;
	let mut top = rcs(Coredata::Null());
	match **args.first().unwrap() {
		Sourcedata(ref src, Coredata::Function(Function::Builtin(..))) => {
			return Err((src.clone(), format!["expected Function but got {}", data_name(args.first().unwrap())]));
		}
		Sourcedata(ref src, Coredata::Function(Function::Library(ref params, ref program))) => {
			for i in params.iter().rev() {
				top = rcs(Coredata::Cell(rcs(Coredata::Symbol(i.clone())), top));
			}
		}
		Sourcedata(ref src, ..) => {
			return Err(extype![src, Function, args.first().unwrap()]);
		}
	}
	Ok(top)
});

/// Evals the argument as if it's a program.
fn eval_expose(program: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
	if let Some(args) = env.params.last() {
		if args.len() != 1 {
			Some((None, arity_mismatch(1, 1, args.len())))
		} else if let Some(arg) = args.first() {
			program.push(arg.clone());
			None
		} else {
			Some((None, arity_mismatch(1, 1, args.len())))
		}
	} else {
		Some((None, "no argument stack".into()))
	}
}

/// Exit the entire program.
teko_simple_function!(exit args : 0 => 1 => {
	if let Some(arg) = args.last() {
		match **arg {
			Sourcedata(ref src, Coredata::Integer(ref value)) => {
				if let Some(value) = value.to_i32() {
					::std::process::exit(value);
				} else {
					Err((src.clone(), "unable to convert number to value".into()))
				}
			}
			Sourcedata(ref src, ..) => {
				Err(extype![src, Integer, arg])
			}
		}
	} else {
		::std::process::exit(0);
	}
});

/// Construct a function object with dynamic scope.
teko_simple_macro!(function args : 2 => usize::MAX => {
	if let Some(head) = args.head() {
		let params = if let Some(params) = collect_cell_of_symbols_into_vec(&head) {
			params
		} else {
			return Err((None, "parameter list contains non-symbols".into()));
		};
		if let Some(tail) = args.tail() {
			let code = collect_cell_into_revvec(&tail);
			Ok(rcs(Coredata::Function(Function::Library(params, code))))
		} else {
			Err((None, "tail is empty".into()))
		}
	} else {
		Err((None, "parameter list is not a list".into()))
	}
});

/// The greater-than function for comparing integers.
teko_simple_function!(gt args : 0 => usize::MAX => {
	let mut last = None;
	let mut result = rcs(Coredata::Boolean(true));
	for arg in args.iter() {
		match **arg {
			Sourcedata(_, Coredata::Integer(ref integer)) => {
				if let Some(previous) = last {
					if previous > integer {
						// Do nothing
					} else {
						result = rcs(Coredata::Boolean(false));
						break;
					}
					last = Some(integer);
				} else {
					last = Some(integer);
				}
			}
			Sourcedata(ref src, ..) => {
				return Err(extype![src, Integer, arg]);
			}
		}
	}
	Ok(result)
});

/// Take the head of a cell.
///
/// If the argument is not a cell then this will unwind with
/// an error.
teko_simple_function!(head args : 1 => 1 => {
	let arg = args.first().unwrap();
	if let Some(head) = arg.head() {
		Ok(head.clone())
	} else {
		return Err(extype![arg.0, Cell, arg]);
	}
});

/// Conditional branching primitive.
fn if_conditional(program: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
	let arg = env.result.clone();
	if let Some(head) = arg.head() {
		if let Some(tail) = arg.tail() {
			if let Some(head_of_tail) = tail.head() {
				if let Some(tail_of_tail) = tail.tail() {
					if let Some(head_of_tail_of_tail) = tail_of_tail.head() {
						program.push(rcs(Coredata::Internal(
							Commands::If(head_of_tail, head_of_tail_of_tail),
						)));
						program.push(head);
						return None;
					} else {
						Some((None, arity_mismatch(3, 3, 2)))
					}
				} else {
					Some((None, arity_mismatch(3, 3, 1)))
				}
			} else {
				Some((None, arity_mismatch(3, 3, 1)))
			}
		} else {
			Some((None, arity_mismatch(3, 3, 1)))
		}
	} else {
		Some((None, arity_mismatch(3, 3, 0)))
	}
}

/// Check if data is the same.
teko_simple_function!(is_data_eq args : 0 => usize::MAX => {
	let mut last = None;
	let mut result = rcs(Coredata::Boolean(true));
	for arg in args.iter() {
		let data = &arg.1;
		if let Some(previous) = last {
			if previous == data {
				// Do nothing
			} else {
				result = rcs(Coredata::Boolean(false));
				break;
			}
			last = Some(data);
		} else {
			last = Some(data);
		}
	}
	Ok(result)
});

/// Check if a value is an error type.
teko_simple_function!(is_error args : 1 => 1 => {
	let arg = args.first().unwrap();
	if let Coredata::Error(_) = arg.1 {
		Ok(rcs(Coredata::Boolean(true)))
	} else {
		Ok(rcs(Coredata::Boolean(false)))
	}
});

/// Check if the value is a cell type.
teko_simple_function!(is_cell args : 1 => 1 => {
	let arg = args.first().unwrap();
	if let Coredata::Cell(..) = arg.1 {
		Ok(rcs(Coredata::Boolean(true)))
	} else {
		Ok(rcs(Coredata::Boolean(false)))
	}
});

/// Check if the value is a symbol.
teko_simple_function!(is_symbol args : 1 => 1 => {
	let arg = args.first().unwrap();
	if let Coredata::Symbol(_) = arg.1 {
		Ok(rcs(Coredata::Boolean(true)))
	} else {
		Ok(rcs(Coredata::Boolean(false)))
	}
});

/// Compute the length of a list.
teko_simple_function!(list_length args : 1 => 1 => {
	let arg = args.first().unwrap();
	if let Some(len) = arg.len() {
		Ok(rcs(Coredata::Integer(len.into())))
	} else {
		Err(extype![arg.0, String or Cell, arg])
	}
});

/// Construct a list (nested cell) of items.
teko_simple_function!(list args : 0 => usize::MAX => {
	let mut result = rcs(Coredata::Null());
	for arg in args.iter().rev() {
		result = rcs(Coredata::Cell(arg.clone(), result));
	}
	Ok(result)
});

/// Load a file
fn load(program: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
	use parse::parse_file;
	assert![env.params.len() == 1];
	let input = &**env.params.last().unwrap().first().unwrap();
	if let Coredata::String(ref string) = input.1 {
		let parse = parse_file(string);
		match parse {
			Ok(tree) => {
				program.extend(tree);
				None
			}
			Err(e) => { Some((input.0.clone(), format!["{:?}", e])) }
		}
	} else {
		println!["{}", data_name(&input)];
		Some((input.0.clone(), "expected String but got X".to_string()))
	}
}

/// The less-than function for comparing integers.
teko_simple_function!(lt args : 0 => usize::MAX => {
	let mut last = None;
	let mut result = rcs(Coredata::Boolean(true));
	for arg in args.iter() {
		match **arg {
			Sourcedata(_, Coredata::Integer(ref integer)) => {
				if let Some(previous) = last {
					if previous < integer {
						// Do nothing
					} else {
						result = rcs(Coredata::Boolean(false));
						break;
					}
					last = Some(integer);
				} else {
					last = Some(integer);
				}
			}
			_ => {
				return Err(extype![arg.0, Integer, arg]);
			}
		}
	}
	Ok(result)
});

/// The macro value constructor.
teko_simple_macro!(make_macro args : 2 => usize::MAX => {
	let head = args.head().unwrap();
	let tail = args.tail().unwrap();
	let params = match *head {
		Sourcedata(_, Coredata::Symbol(ref string)) => string.clone(),
		_ => {
			return Err(extype![head.0, Symbol, head]);
		}
	};
	let code = collect_cell_into_revvec(&tail);
	Ok(rcs(Coredata::Macro(Macro::Library(params, code))))
});

/// Integer multiplication.
teko_simple_function!(multiply args : 0 => usize::MAX => {
	let mut sum = one();
	for arg in args.iter() {
		match **arg {
			Sourcedata(_, Coredata::Integer(ref value)) => {
				sum = sum * value;
			}
			_ => {
				return Err(extype![arg.0, Integer, arg]);
			}
		}
	}
	Ok(rcs(Coredata::Integer(sum)))
});

/// Boolean NOT.
teko_simple_function!(not args : 1 => 1 => {
	let arg = args.first().unwrap();
	if let Coredata::Boolean(false) = arg.1 {
		Ok(rcs(Coredata::Boolean(true)))
	} else {
		Ok(rcs(Coredata::Boolean(false)))
	}
});

/// Boolean (inclusive) OR.
teko_simple_function!(or args : 0 => usize::MAX => {
	for arg in args {
		if let Coredata::Boolean(false) = arg.1 {
			continue;
		} else {
			return Ok(rcs(Coredata::Boolean(true)));
		}
	}
	Ok(rcs(Coredata::Boolean(false)))
});

/// Cell value constructor.
///
/// The second argument must be a `Cell` or `Null()`, else it will
/// unwind with an error.
teko_simple_function!(cell args : 2 => 2 => {
	let arg1 = &args[0];
	let arg2 = &args[1];
	if let Coredata::Cell(..) = arg2.1 {
		// Ok TODO replace with check is_cell_or_null(...)
	} else if let Coredata::Null(..) = arg2.1 {
		// Ok
	} else {
		return Err(extype![arg2.0, Cell or Null, arg2]);
	}
	Ok(rcs(Coredata::Cell(arg1.clone(), arg2.clone())))
});

teko_simple_function!(current_time_milliseconds args : 0 => 0 => {
	use time;
	use num::bigint::ToBigInt;
	let ts = time::get_time();
	let millis = ts.sec * 1000 + (ts.nsec as i64) / 1_000_000;
	Ok(rcs(Coredata::Integer(millis.to_bigint().unwrap())))
});


/// Integer addition. `(+ Integer*) => Integer`
teko_simple_function!(plus args : 0 => usize::MAX => {
	let mut sum = zero();
	for arg in args.iter() {
		match **arg {
			Sourcedata(_, Coredata::Integer(ref value)) => {
				sum = sum + value;
			}
			_ => {
				return Err(extype![arg.0, Integer, arg]);
			}
		}
	}
	Ok(rcs(Coredata::Integer(sum)))
});

/// Print all arguments to standard output.
///
/// Does not put strings on the write form, however,
/// strings inside structures are still printed in their written form: (" X).
teko_simple_function!(print args : 1 => usize::MAX => {
	for arg in args {
		if let Coredata::String(ref value) = arg.1 {
			println!["{}", value];
		} else {
			println!["{}", arg];
		}
	}
	Ok(args.last().unwrap().clone())
});

/// Quote elements
///
/// A builtin macro always stores the tail of the invocation inside `env.result`, so this macro is
/// empty; it doesn't need to do anything.
fn quote(_: &mut Program, _: &mut Env) -> Option<(Option<Source>, String)> {
	None
}

fn read(_: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
	let mut parser = ParseState::from("tty");
	for ch in io::stdin().bytes() {
		if let Ok(ch) = ch {
			if let Err(state) = parse_character(ch as char, &mut parser) {
				let crp = Some(state.current_read_position.clone());
				if let Some(error) = state.error {
					return Some((crp, format!["parse error: {}", error]));
				} else {
					return Some((crp, "parse error".into()));
				}
			}
			if is_ready_to_finish(&parser) {
				let result = finish_parsing_characters(parser);
				if let Ok(tree) = result {
					match tree.first() {
						Some(tree) => env.result = tree.clone(),
						None => return Some((None, "parse error: ".into())),
					}
				}
				break;
			}
		} else {
			return Some((None, "unable to read standard input".into()));
		}
	}
	None
}

/// Used by set internal to set variables.
fn set_internal(_: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
	if let Some(args) = env.params.last() {
		if let Some(symbol) = args.first() {
			match **symbol {
				Sourcedata(ref source, Coredata::String(ref string)) => {
					if let Some(rhs) = args.get(1) {
						if !env.store.contains_key(string) {
							return Some((
								source.clone(),
								format!["variable does not exist, {}", string],
							));
						}
						env.store.insert(string.clone(), vec![rhs.clone()]);
					} else {
						return Some((None, arity_mismatch(2, 2, 1)));
					}
				}
				_ => {
					return Some(extype![symbol.0, String, symbol]);
				}
			}
		} else {
			return Some((None, arity_mismatch(2, 2, 0)));
		}
	} else {
		return Some((None, "no arg stack".into()));
	}
	None
}

/// Set a variable in the environment.
fn set(program: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
	{
		let args = env.result.clone();
		let sub = rcs(Coredata::Function(
			Function::Builtin(set_internal, "@set-internal".into()),
		));
		if let Some(ref tail) = args.tail() {
			match tail.1 {
				Coredata::Cell(ref heado, _) => {
					program.push(rcs(Coredata::Internal(Commands::Call(sub))));
					program.push(rcs(Coredata::Internal(Commands::Param)));
					program.push(heado.clone());
				}
				Coredata::Null() => {
					return Some((None, arity_mismatch(2, 2, 0)));
				}
				_ => {
					return Some(extype![tail.0, Cell, tail]);
				}
			}
		} else {
			return Some((None, arity_mismatch(2, 2, 0)));
		}
		program.push(rcs(Coredata::Internal(Commands::Param)));
		if let Some(head) = args.head() {
			match *head {
				Sourcedata(ref source, Coredata::Symbol(ref symbol)) => {
					program.push(Rc::new(
						Sourcedata(source.clone(), Coredata::String(Into::<&str>::into(symbol).to_string())),
					));
				}
				_ => {
					return Some(extype![head.0, Cell, head]);
				}
			}
		} else {
			return Some((None, arity_mismatch(2, 2, 1)));
		}
	}
	env.params.push(vec![]);
	None
}

/// Sleep for a given number of milliseconds.
teko_simple_function!(msleep args : 1 => 1 => {
	let arg = args.first().unwrap();
	match **arg {
		Sourcedata(ref src, Coredata::Integer(ref value)) => {
			if let Some(value) = value.to_u64() {
				thread::sleep(time::Duration::from_millis(value));
			} else {
				return Err((src.clone(), "unable to convert number to value".into()));
			}
		}
		_ => {
			return Err(extype![arg.0, Integer, arg]);
		}
	}
	Ok(arg.clone())
});

fn program(program: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
	program.extend(collect_cell_into_revvec(&env.result));
	None
}

/// Create a string
///
/// Creates a string from the given symbols by inserting single spaces inbetween each symbol.
teko_simple_macro!(string arg : 0 => usize::MAX => {
	let data = {
		let mut data = collect_cell_into_revvec(&arg);
		data.reverse();
		data
	};
	let mut ret = String::from("");
	let mut last_was_symbol = false;
	for i in data {
		match *i {
			Sourcedata(_, Coredata::Symbol(ref string)) => {
				if last_was_symbol {
					ret.push(' ');
				}
				ret.push_str(string.into());
				last_was_symbol = true;
			}
			Sourcedata(ref src, Coredata::Cell(ref head, ref tail)) => {
				let repeats = if let Coredata::Null() = tail.1 {
					1
				} else if let Sourcedata(ref src, Coredata::Cell(ref head, ref tail)) = **tail {
					if let Sourcedata(ref src, Coredata::Symbol(ref value)) = **head {
						let t: &str = value.into();
						let code = t.parse::<u32>();
						if let Ok(code) = code {
							code
						} else {
							return Err((src.clone(), format![
								"unable to parse value to unsigned 32-bit integer: {:?}",
								value,
							]));
						}
					} else {
						return Err((src.clone(), format![
							"tail is not a cell: {}",
							tail,
						]));
					}
				} else {
					return Err((src.clone(), "string character only accepts a one or two arguments".into()));
				};
				if let Sourcedata(ref src, Coredata::Symbol(ref value)) = **head {
					let t: &str = value.into();
					let code = t.parse::<u32>();
					if let Ok(code) = code {
						if let Some(code) = char::from_u32(code) {
							for _ in 0..repeats {
								ret.push(code);
							}
						} else {
							return Err((src.clone(), "value is not a valid character value".into()));
						}
					} else {
						return Err((src.clone(), "value is not an unsigned 32-bit value".into()));
					}
				}
				last_was_symbol = false;
			}
			_ => {
				return Err((None, "input is not atom or cell".into()));
			}
		}
	}
	Ok(rcs(Coredata::String(ret)))
});

/// Integer subtraction.
teko_simple_function!(subtract args : 1 => usize::MAX => {
	let mut sum = zero();
	if args.len() == 1 {
		for arg in args.iter() {
			match **arg {
				Sourcedata(_, Coredata::Integer(ref value)) => {
					sum = sum - value;
				}
				_ => {
					return Err(extype![arg.0, Integer, arg]);
				}
			}
		}
	} else if args.len() > 1 {
		let mut first = true;
		for arg in args.iter() {
			match **arg {
				Sourcedata(_, Coredata::Integer(ref value)) => {
					if first {
						sum = value.clone();
					} else {
						sum = sum - value;
					}
				}
				_ => {
					return Err(extype![arg.0, Integer, arg]);
				}
			}
			first = false;
		}
	} else {
		return Err((None, arity_mismatch(1, usize::MAX, 0)));
	}
	Ok(rcs(Coredata::Integer(sum)))
});

/// Take the tail of a cell.
///
/// If the argument is not a cell, then an error will be unwound.
teko_simple_function!(tail args : 1 => 1 => {
	let arg = args.first().unwrap();
	if let Some(tail) = arg.tail() {
		Ok(tail.clone())
	} else {
		return Err(extype![arg.0, Cell, arg]);
	}
});

/// Convert data structures to a string.
teko_simple_function!(to_string args : 1 => 1 => {
	let arg = args.first().unwrap();
	Ok(rcs(Coredata::String(format!["{}", arg])))
});

/// Return a stack trace.
///
/// The stack trace will not show tail call optimized calls, so there may
/// be some calls missing here. Since the requirement is for the program
/// to be unbounded in the amount of tail calls, there's no way to definitively
/// store all calls.
fn trace(program: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
	env.result = internal_trace(program, env);
	None
}

/// Set up a "catch-all" that catches all errors
fn wind(program: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
	let args = env.result.clone();
	let code = collect_cell_into_revvec(&args);
	program.push(rcs(Coredata::Internal(Commands::Wind)));
	program.extend(code.iter().cloned());
	None
}

/// Write to standard output.
///
/// Writing is a symmetric operation together with read. This means that
/// writing an object, and then reading the result will give back the same
/// object, although it may be necessary to explicitly eval parts of the
/// object, the representation will always stay intact regardless of how
/// many reads and writes you apply to it.
teko_simple_function!(write args : 1 => usize::MAX => {
	for arg in args {
		println!["{}", arg];
	}
	Ok(args.last().unwrap().clone())
});
