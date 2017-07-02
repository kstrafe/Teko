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

// //////////////////////////////////////////////////////////
// Internal data structures used by Teko
// //////////////////////////////////////////////////////////
use data_structures::{Boolean, Commands, Coredata, Env, Function, Macro, Program, Sourcedata};
use utilities::*;

// //////////////////////////////////////////////////////////
// External libraries
// //////////////////////////////////////////////////////////
use num::{BigInt, one, zero};

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
pub fn create_builtin_library_table() -> HashMap<String, Program> {
	construct_builtins! {
		// This section contains non-functions and non-macros
		{
			"help" => Coredata::String(HELP.into()),
			"true" => Coredata::Boolean(Boolean::True),
			"false" => Coredata::Boolean(Boolean::False),
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
		Function : "pair" => pair,
		Function : "pair?" => is_pair,
		Macro    : "fn" => function,
		Macro    : "mo" => make_macro,
		// Some useful features
		Macro    : "def" => define,
		Macro    : "set!" => set,
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

/// Logical AND.
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
	} else {
		return Some("no arg stack".into());
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
	for values in env.store.values() {
		count += values.len();
	}
	env.result = Rc::new(Sourcedata(None, Coredata::Integer(count.into())));
	None
}

/// Find all active variables in the dynamic scope.
fn at_variables(_: &mut Program, env: &mut Env) -> Option<String> {
	env.result = Rc::new(Sourcedata(None, Coredata::Null));
	for key in env.store.keys() {
		env.result = Rc::new(Sourcedata(
			None,
			Coredata::Pair(
				Rc::new(Sourcedata(None, Coredata::Symbol(key.clone()))),
				env.result.clone(),
			),
		));
	}
	None
}

/// Used by define to perform the final step of assigning.
fn define_internal(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		if let Some(symbol) = args.first() {
			match **symbol {
				Sourcedata(ref source, Coredata::String(ref string)) => {
					if let Some(rhs) = args.get(1) {
						if env.store.contains_key(string) {
							if let Some(ref source) = *source {
								return Some(format![
									"can not define `{}', already exists, {}",
									string,
									source,
								]);
							} else {
								return Some(format!["can not define `{}', already exists", string]);
							}
						}
						env.store.insert(string.clone(), vec![rhs.clone()]);
					} else {
						return Some("arity mismatch, expecting 2 but got 1".into());
					}
				}
				Sourcedata(Some(ref source), ..) => {
					return Some(format![
						"expected String but got {}, {}",
						data_name(symbol),
						source,
					]);
				}
				_ => {
					return Some(format!["expected String but got {}", data_name(symbol)]);
				}
			}
		} else {
			return Some("arity mismatch, expecting 2 but got 0".into());
		}
	} else {
		return Some("no arg stack".into());
	}
	None
}

/// Define a variable to be some value.
fn define(program: &mut Program, env: &mut Env) -> Option<String> {
	{
		let args = env.result.clone();
		let sub = Rc::new(Sourcedata(
			None,
			Coredata::Function(Function::Builtin(
				define_internal,
				"@define-internal".into(),
			)),
		));
		if let Some(ref tail) = args.tail() {
			match tail.1 {
				Coredata::Pair(ref heado, _) => {
					program.push(Rc::new(
						Sourcedata(None, Coredata::Internal(Commands::Call(sub))),
					));
					program.push(Rc::new(
						Sourcedata(None, Coredata::Internal(Commands::Parameterize)),
					));
					program.push(heado.clone());
				}
				Coredata::Null => {
					return Some("arity mismatch, expecting 2 but got 0".into());
				}
				_ => {
					return Some(format!["expected Pair but got {}", tail]);
				}
			}
		} else {
			return Some("arity mismatch, expecting 2 but got 0".into());
		}
		program.push(Rc::new(
			Sourcedata(None, Coredata::Internal(Commands::Parameterize)),
		));
		if let Some(head) = args.head() {
			match *head {
				Sourcedata(ref source, Coredata::Symbol(ref string)) => {
					program.push(Rc::new(
						Sourcedata(source.clone(), Coredata::String(string.clone())),
					));
				}
				_ => {
					return Some(format!["expected Pair but got {}", head]);
				}
			}
		} else {
			return Some("arity mismatch, expecting 2 but got 1".into());
		}
	}
	env.params.push(vec![]);
	None
}

/// Mathematical division of integers.
fn divide(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		let mut sum = one();
		if args.len() == 1 {
			for arg in args.iter() {
				match **arg {
					Sourcedata(ref source, Coredata::Integer(ref value)) => {
						if value == &zero::<BigInt>() {
							return Some(format!["argument {} is zero", optional_source(source)]);
						}
						sum = sum / value;
					}
					Sourcedata(Some(ref source), ..) => {
						return Some(format![
							"expected Integer but got {}, {}",
							data_name(arg),
							source,
						]);
					}
					_ => {
						return Some(format!["expected Integer but got {}", data_name(arg)]);
					}
				}
			}
		} else if args.len() > 1 {
			let mut first = true;
			for arg in args.iter() {
				match **arg {
					Sourcedata(ref source, Coredata::Integer(ref value)) => {
						if first {
							sum = value.clone();
						} else {
							if value == &zero::<BigInt>() {
								return Some(
									format!["argument {} is zero", optional_source(source)],
								);
							}
							sum = sum / value;
						}
					}
					Sourcedata(Some(ref source), ..) => {
						return Some(format![
							"expected Integer but got {}, {}",
							data_name(arg),
							source,
						]);
					}
					_ => {
						return Some(format!["expected Integer but got {}", data_name(arg)]);
					}
				}
				first = false;
			}
		} else {
			return Some("arity mismatch, expecting >0 but got 0".into());
		}
		env.result = Rc::new(Sourcedata(None, Coredata::Integer(sum)));
		None
	} else {
		Some("no argument stack".into())
	}
}

/// Retrieve the first statement of a function or macro.
fn doc(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		if args.len() != 1 {
			Some(format![
				"arity mismatch, expecting 1 but got {}",
				args.len(),
			])
		} else if let Some(arg) = args.first() {
			match arg.1 {
				Coredata::Function(Function::Library(_, ref stats)) |
				Coredata::Macro(Macro::Library(_, ref stats)) => {
					if stats.is_empty() {
						env.result = rcs(Coredata::Null);
					} else {
						env.result = stats.last().unwrap().clone();
					}
					None
				}
				_ => {
					Some(format![
						"expected Function or Macro but got {}",
						data_name(arg),
					])
				}
			}
		} else {
			None
		}
	} else {
		None
	}
}

/// Integer equality comparison.
fn eq(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		let mut last = None;
		let mut result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
		for arg in args.iter() {
			match **arg {
				Sourcedata(_, Coredata::Integer(ref integer)) => {
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
				Sourcedata(Some(ref source), ..) => {
					return Some(format![
						"expected Integer but got {}, {}",
						data_name(arg),
						source,
					]);
				}
				_ => {
					return Some(format!["expected Integer but got {}", data_name(arg)]);
				}
			}
		}
		env.result = result;
		None
	} else {
		Some("no argument stack".into())
	}
}

/// Error constructor.
///
/// Error is its own type in Teko.
fn error(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		if args.len() >= 2 {
			Some("arity mismatch, expecting <2 but got >=2".into())
		} else if let Some(arg) = args.first() {
			env.result = Rc::new(Sourcedata(None, Coredata::Error(arg.clone())));
			None
		} else {
			env.result = Rc::new(Sourcedata(
				None,
				Coredata::Error(Rc::new(Sourcedata(None, Coredata::Null))),
			));
			None
		}
	} else {
		Some("no argument stack".into())
	}
}

/// Evaluates the argument as if it's a program.
fn eval_expose(program: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		if args.len() != 1 {
			Some(format![
				"arity mismatch, expecting 1 but got {}",
				args.len(),
			])
		} else if let Some(arg) = args.first() {
			program.push(arg.clone());
			None
		} else {
			Some("arity mismatch, expecting 1 but got 0".into())
		}
	} else {
		Some("no argument stack".into())
	}
}

/// Exit the entire program.
fn exit(_: &mut Program, env: &mut Env) -> Option<String> {
	use num::ToPrimitive;

	if let Some(args) = env.params.last() {
		if args.len() <= 1 {
			if let Some(arg) = args.last() {
				match **arg {
					Sourcedata(_, Coredata::Integer(ref value)) => {
						if let Some(value) = value.to_i32() {
							::std::process::exit(value);
						} else {
							return Some("unable to convert number to value".into());
						}
					}
					Sourcedata(Some(ref source), ..) => {
						return Some(format![
							"expected Integer but got {}, {}",
							data_name(arg),
							source,
						]);
					}
					_ => {
						return Some(format!["expected Integer but got {}", data_name(arg)]);
					}
				}
			} else {
				::std::process::exit(0);
			}
		} else {
			return Some(format![
				"arity mismatch, expecting 0 or 1 args but got {}",
				args.len(),
			]);
		}
	} else {
		return Some("no argument stack".into());
	}
}

/// Construct a function object with dynamic scope.
fn function(_: &mut Program, env: &mut Env) -> Option<String> {
	let args = env.result.clone();
	let params = if let Some(ref args) = args.head() {
		if let Some(params) = collect_pair_of_symbols_into_vec_string(args) {
			params
		} else {
			return Some("parameter list contains non-symbols".into());
		}
	} else {
		return Some("arity mismatch, expecting 2 but got 0".into());
	};
	let code = if let Some(ref code) = args.tail() {
		collect_pair_into_vec(code)
	} else {
		return Some("arity mismatch, expecting 2 but got 0".into());
	};
	env.result = Rc::new(Sourcedata(
		None,
		Coredata::Function(Function::Library(params, code)),
	));
	None
}

/// The greater-than function for comparing integers.
fn gt(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		let mut last = None;
		let mut result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
		for arg in args.iter() {
			match **arg {
				Sourcedata(_, Coredata::Integer(ref integer)) => {
					if let Some(previous) = last {
						if previous > integer {
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
				Sourcedata(Some(ref source), ..) => {
					return Some(format![
						"expected Integer but got {}, {}",
						data_name(arg),
						source,
					]);
				}
				_ => {
					return Some(format!["expected Integer but got {}", data_name(arg)]);
				}
			}
		}
		env.result = result;
	} else {
		return Some("no argument stack".into());
	}
	None
}

/// Take the head of a pair.
///
/// If the argument is not a pair then this will unwind with
/// an error.
fn head(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		if args.len() != 1 {
			Some(format![
				"arity mismatch, expected 1 arg but got {}",
				args.len(),
			])
		} else if let Some(arg) = args.first() {
			if let Some(head) = arg.head() {
				env.result = head.clone();
				return None;
			} else if let Sourcedata(Some(ref source), ..) = **arg {
				return Some(format![
					"expected Pair but got {}, {}",
					data_name(arg),
					source,
				]);
			} else {
				return Some(format!["expected Pair but got {}", data_name(arg)]);
			}
		} else {
			unreachable!();
		}
	} else {
		Some("no argument stack".into())
	}
}

/// Conditional branching primitive.
fn if_conditional(program: &mut Program, env: &mut Env) -> Option<String> {
	let arg = env.result.clone();
	if let Some(head) = arg.head() {
		if let Some(tail) = arg.tail() {
			if let Some(head_of_tail) = tail.head() {
				if let Some(tail_of_tail) = tail.tail() {
					if let Some(head_of_tail_of_tail) = tail_of_tail.head() {
						program.push(Rc::new(Sourcedata(
							None,
							Coredata::Internal(
								Commands::If(head_of_tail, head_of_tail_of_tail),
							),
						)));
						program.push(head);
						return None;
					}
				}
			}
		}
	}
	Some("arity mismatch, expecting 3".into())
}

/// Check if data is the same.
fn is_data_eq(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		let mut last = None;
		let mut result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
		for arg in args.iter() {
			let data = &arg.1;
			if let Some(previous) = last {
				if previous == data {
					// Do nothing
				} else {
					result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::False)));
					break;
				}
				last = Some(data);
			} else {
				last = Some(data);
			}
		}
		env.result = result;
		None
	} else {
		Some("no argument stack".into())
	}
}

/// Check if a value is an error type.
fn is_error(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		if args.len() != 1 {
			return Some(format![
				"arity mismatch, expecting 1 but got {}",
				args.len(),
			]);
		}
		if let Some(arg) = args.first() {
			if let Coredata::Error(_) = arg.1 {
				env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
			} else {
				env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::False)));
			}
		} else {
			return Some("arity mismatch, expecting 1 but got 0".into());
		}
	} else {
		return Some("no argument stack".into());
	}
	None
}

/// Check if the value is a pair type.
fn is_pair(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		if args.len() != 1 {
			return Some(format![
				"arity mismatch, expecting 1 but got {}",
				args.len(),
			]);
		}
		if let Some(arg) = args.first() {
			if let Coredata::Pair(..) = arg.1 {
				env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
			} else {
				env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::False)));
			}
		} else {
			return Some("arity mismatch, expecting 1 but got 0".into());
		}
	} else {
		return Some("no argument stack".into());
	}
	None
}

/// Check if the value is a symbol.
fn is_symbol(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		if let Some(arg) = args.first() {
			if let Coredata::Symbol(_) = arg.1 {
				env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
			} else {
				env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::False)));
			}
		}
	} else {
		return Some("no argument stack".into());
	}
	None
}

/// Compute the length of a list.
fn list_length(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		if args.len() != 1 {
			return Some(format!["arity mismatch, expected 1, got {}", args.len()]);
		}
		if let Some(arg) = args.first() {
			if let Some(len) = arg.len() {
				env.result = rcs(Coredata::Integer(len.into()));
			} else {
				return Some(format![
					"expected Pair or String but got {}",
					data_name(arg),
				]);
			}
		}
	} else {
		return Some("no argument stack".into());
	}
	None
}

/// Construct a list (nested pair) of items.
fn list(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		let mut result = rcs(Coredata::Null);
		for arg in args.iter().rev() {
			result = rcs(Coredata::Pair(arg.clone(), result));
		}
		env.result = result;
	} else {
		return Some("no argument stack".into());
	}
	None
}

/// The less-than function for comparing integers.
fn lt(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		let mut last = None;
		let mut result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
		for arg in args.iter() {
			match **arg {
				Sourcedata(_, Coredata::Integer(ref integer)) => {
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
				Sourcedata(Some(ref source), ..) => {
					return Some(format![
						"expected Integer but got {}, {}",
						data_name(arg),
						source,
					]);
				}
				_ => {
					return Some(format!["expected Integer but got {}", data_name(arg)]);
				}
			}
		}
		env.result = result;
	} else {
		return Some("no argument stack".into());
	}
	None
}

/// The macro value constructor.
fn make_macro(_: &mut Program, env: &mut Env) -> Option<String> {
	let arg = env.result.clone();
	if let Some(head) = arg.head() {
		if let Some(tail) = arg.tail() {
			let params = match *head {
				Sourcedata(_, Coredata::Symbol(ref string)) => string.clone(),
				Sourcedata(Some(ref source), ..) => {
					return Some(format![
						"expected Symbol but got {}, {}",
						data_name(&*head),
						source,
					]);
				}
				_ => {
					return Some(format!["expected Symbol but got {}", data_name(&*head)]);
				}
			};
			let code = collect_pair_into_vec(&tail);
			env.result = Rc::new(Sourcedata(
				None,
				Coredata::Macro(Macro::Library(params, code)),
			));
			return None;
		}
	}
	Some("arity mismatch, expecting 2".into())
}

/// Integer multiplication.
fn multiply(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		let mut sum = one();
		for arg in args.iter() {
			match **arg {
				Sourcedata(_, Coredata::Integer(ref value)) => {
					sum = sum * value;
				}
				Sourcedata(Some(ref source), ..) => {
					return Some(format![
						"expected Integer but got {}, {}",
						data_name(&*arg),
						source,
					]);
				}
				_ => {
					return Some(format!["expected Integer but got {}", data_name(&*arg)]);
				}
			}
		}
		env.result = Rc::new(Sourcedata(None, Coredata::Integer(sum)));
		None
	} else {
		Some("no argument stack".into())
	}
}

/// Boolean NOT.
fn not(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		if args.len() != 1 {
			Some(format!["arity mismatch, expected 1, got {}", args.len()])
		} else if let Some(arg) = args.first() {
			if let Coredata::Boolean(Boolean::False) = arg.1 {
				env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::True)));
			} else {
				env.result = Rc::new(Sourcedata(None, Coredata::Boolean(Boolean::False)));
			}
			None
		} else {
			unreachable!();
		}
	} else {
		Some("no argument stack".into())
	}
}

/// Boolean (inclusive) OR.
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

/// Pair value constructor.
///
/// The second argument must be a `Pair` or `Null`, else it will
/// unwind with an error.
fn pair(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		if args.len() != 2 {
			Some(format![
				"arity mismatch, expecting 2 but got {}",
				args.len(),
			])
		} else {
			if let Some(arg1) = args.first() {
				if let Some(arg2) = args.get(1) {
					if let Coredata::Pair(..) = arg2.1 {
					} else if let Coredata::Null = arg2.1 {
					} else if let Sourcedata(Some(ref source), ..) = **arg2 {
						return Some(format![
							"expected Pair or Null but got {}, {}",
							data_name(arg2),
							source,
						]);
					} else {
						return Some(format!["expected Pair or Null but got {}", data_name(arg2)]);
					}
					env.result =
						Rc::new(Sourcedata(None, Coredata::Pair(arg1.clone(), arg2.clone())));
				} else {
					unreachable!();
				}
			} else {
				unreachable!();
			}
			None
		}
	} else {
		Some("no argument stack".into())
	}
}

/// Integer addition. `(+ Integer*) => Integer`
fn plus(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		let mut sum = zero();
		for arg in args.iter() {
			match **arg {
				Sourcedata(_, Coredata::Integer(ref value)) => {
					sum = sum + value;
				}
				Sourcedata(Some(ref source), ..) => {
					return Some(format![
						"expected Integer but got {}, {}",
						data_name(&**arg),
						source,
					]);
				}
				Sourcedata(None, ..) => {
					return Some(format!["expected Integer but got {}", data_name(&**arg)]);
				}
			}
		}
		env.result = Rc::new(Sourcedata(None, Coredata::Integer(sum)));
		None
	} else {
		Some("no argument stack".into())
	}
}

/// Print all arguments to standard output.
///
/// Does not put strings on the write form, however,
/// strings inside structures are still printed in their written form: (" X).
fn print(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		for arg in args {
			if let Coredata::String(ref value) = arg.1 {
				println!["{}", value];
			} else {
				println!["{}", arg];
			}
		}
		None
	} else {
		Some("no argument stack".into())
	}
}

/// Quote elements
///
/// A builtin macro always stores the tail of the invocation inside `env.result`, so this macro is
/// empty; it doesn't need to do anything.
fn quote(_: &mut Program, _: &mut Env) -> Option<String> {
	None
}

fn read(_: &mut Program, env: &mut Env) -> Option<String> {
	use data_structures::ParseState;
	use parse::*;
	let mut parser = ParseState::from("tty");
	for ch in io::stdin().bytes() {
		if let Ok(ch) = ch {
			if let Err(state) = parse_character(ch as char, &mut parser) {
				break;
			}
			if is_ready_to_finish(&parser) {
				let result = finish_parsing_characters(parser);
				if let Ok(tree) = result {
					if let Some(tree) = tree.first() {
						env.result = tree.clone();
					} else {
						break;
					}
				}
				break;
			}
		} else {
			break;
		}
	}
	None
}

/// Used by set internall to set variables.
fn set_internal(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		if let Some(symbol) = args.first() {
			match **symbol {
				Sourcedata(ref source, Coredata::String(ref string)) => {
					if let Some(rhs) = args.get(1) {
						if !env.store.contains_key(string) {
							if let Some(ref source) = *source {
								return Some(format![
									"can not set! `{}', does not exist, {}",
									string,
									source,
								]);
							} else {
								return Some(format!["can not set! `{}', does not exist", string]);
							}
						}
						env.store.insert(string.clone(), vec![rhs.clone()]);
					} else {
						return Some("arity mismatch, expecting 2 but got 1".into());
					}
				}
				Sourcedata(Some(ref source), ..) => {
					return Some(format![
						"expected String but got {}, {}",
						data_name(symbol),
						source,
					]);
				}
				_ => {
					return Some(format!["expected String but got {}", data_name(symbol)]);
				}
			}
		} else {
			return Some("arity mismatch, expecting 2 but got 0".into());
		}
	} else {
		return Some("no arg stack".into());
	}
	None
}

/// Set a variable in the environment.
fn set(program: &mut Program, env: &mut Env) -> Option<String> {
	{
		let args = env.result.clone();
		let sub = Rc::new(Sourcedata(
			None,
			Coredata::Function(
				Function::Builtin(set_internal, "@set-internal".into()),
			),
		));
		if let Some(ref tail) = args.tail() {
			match tail.1 {
				Coredata::Pair(ref heado, _) => {
					program.push(Rc::new(
						Sourcedata(None, Coredata::Internal(Commands::Call(sub))),
					));
					program.push(Rc::new(
						Sourcedata(None, Coredata::Internal(Commands::Parameterize)),
					));
					program.push(heado.clone());
				}
				Coredata::Null => {
					return Some("arity mismatch, expecting 2 but got 0".into());
				}
				_ => {
					return Some(format!["expected Pair but got {}", tail]);
				}
			}
		} else {
			return Some("arity mismatch, expecting 2 but got 0".into());
		}
		program.push(Rc::new(
			Sourcedata(None, Coredata::Internal(Commands::Parameterize)),
		));
		if let Some(head) = args.head() {
			match *head {
				Sourcedata(ref source, Coredata::Symbol(ref string)) => {
					program.push(Rc::new(
						Sourcedata(source.clone(), Coredata::String(string.clone())),
					));
				}
				_ => {
					return Some(format!["expected Pair but got {}", head]);
				}
			}
		} else {
			return Some("arity mismatch, expecting 2 but got 1".into());
		}
	}
	env.params.push(vec![]);
	None
}

/// Sleep for a given number of milliseconds.
fn msleep(_: &mut Program, env: &mut Env) -> Option<String> {
	use std::{thread, time};
	use num::ToPrimitive;
	if let Some(args) = env.params.last() {
		if args.len() != 1 {
			return Some(format![
				"arity mismatch, expecting 1 but got {}",
				args.len(),
			]);
		}
		if let Some(arg) = args.first() {
			match **arg {
				Sourcedata(_, Coredata::Integer(ref value)) => {
					if let Some(value) = value.to_u64() {
						thread::sleep(time::Duration::from_millis(value));
					} else {
						return Some("unable to convert number to value".into());
					}
				}
				Sourcedata(Some(ref source), ..) => {
					return Some(format![
						"expected Integer but got {}, {}",
						data_name(arg),
						source,
					]);
				}
				ref arg @ Sourcedata(None, ..) => {
					return Some(format!["expected Integer but got {}", data_name(arg)]);
				}
			}
			None
		} else {
			unreachable!();
		}
	} else {
		Some("no argument stack".into())
	}
}

/// Create a string
///
/// Creates a string from the given symbols by inserting single spaces inbetween each symbol.
fn string(_: &mut Program, env: &mut Env) -> Option<String> {
	let data = {
		let mut data = collect_pair_into_vec(&env.result);
		data.reverse();
		data
	};
	let mut ret = String::from("");
	let mut last_symbol = false;
	for i in data {
		match *i {
			Sourcedata(_, Coredata::Symbol(ref string)) => {
				if last_symbol {
					ret.push(' ');
				}
				ret.push_str(string);
				last_symbol = true;
			}
			Sourcedata(_, Coredata::Pair(ref head, ref tail)) => {
				let repeats = if let Coredata::Null = tail.1 {
					1
				} else if let Sourcedata(ref source, Coredata::Pair(ref head, ref tail)) = **tail {
					if let Sourcedata(ref source, Coredata::Symbol(ref value)) = **head {
						let code = value.parse::<u32>();
						if let Ok(code) = code {
							code
						} else {
							return Some(format![
								"{}, unable to parse value to unsigned 32-bit integer: {}",
								optional_source(source),
								value,
							]);
						}
					} else {
						return Some(format![
							"{}, tail is not a pair: {}",
							optional_source(source),
							tail,
						]);
					}
				} else {
					return Some(
						"string character only accepts a one or two arguments".into(),
					);
				};
				if let Coredata::Symbol(ref value) = head.1 {
					let code = value.parse::<u32>();
					if let Ok(code) = code {
						if let Some(code) = char::from_u32(code) {
							for _ in 0..repeats {
								ret.push(code);
							}
						} else {
							return Some("value is not a valid character value".into());
						}
					} else {
						return Some("value is not an unsigned 32-bit value".into());
					}
				}
				last_symbol = false;
			}
			_ => {
				return None;
			}
		}
	}
	env.result = rcs(Coredata::String(ret));
	None
}

/// Integer subtraction.
fn subtract(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		let mut sum = zero();
		if args.len() == 1 {
			for arg in args.iter() {
				match **arg {
					Sourcedata(_, Coredata::Integer(ref value)) => {
						sum = sum - value;
					}
					Sourcedata(Some(ref source), ..) => {
						return Some(format![
							"expected Integer but got {}, {}",
							data_name(arg),
							source,
						]);
					}
					_ => {
						return Some(format!["expected Integer but got {}", data_name(arg)]);
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
					Sourcedata(Some(ref source), ..) => {
						return Some(format![
							"expected Integer but got {}, {}",
							data_name(arg),
							source,
						]);
					}
					_ => {
						return Some(format!["expected Integer but got {}", data_name(arg)]);
					}
				}
				first = false;
			}
		} else {
			return Some("arity mismatch, expecting >0 but got 0".into());
		}
		env.result = Rc::new(Sourcedata(None, Coredata::Integer(sum)));
		None
	} else {
		Some("no argument stack".into())
	}
}

/// Take the tail of a pair.
///
/// If the argument is not a pair, then an error will be unwound.
fn tail(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		if args.len() != 1 {
			return Some(format![
				"arity mismatch, expecting 1 but got {}",
				args.len(),
			]);
		} else if let Some(arg) = args.first() {
			if let Some(tail) = arg.tail() {
				env.result = tail;
				return None;
			} else if let Sourcedata(Some(ref source), ..) = **arg {
				return Some(format![
					"expected Pair but got {}, {}",
					data_name(arg),
					source,
				]);
			} else {
				return Some(format!["expected Pair but got {}", data_name(arg)]);
			}
		} else {
			unreachable!();
		}
	} else {
		Some("no argument stack".into())
	}
}

/// Convert data structures to a string.
fn to_string(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		for arg in args {
			env.result = rcs(Coredata::String(format!["{}", arg]));
		}
		None
	} else {
		Some("no argument stack".into())
	}
}

/// Return a stack trace.
///
/// The stack trace will not show tail call optimized calls, so there may
/// be some calls missing here. Since the requirement is for the program
/// to be unbounded in the amount of tail calls, there's no way to definitively
/// store all calls.
pub fn trace(program: &mut Program, env: &mut Env) -> Option<String> {
	env.result = internal_trace(program, env);
	None
}

/// Set up a "catch-all" that catches all errors
fn wind(program: &mut Program, env: &mut Env) -> Option<String> {
	let args = env.result.clone();
	let code = collect_pair_into_vec(&args);
	program.push(Rc::new(
		Sourcedata(None, Coredata::Internal(Commands::Wind)),
	));
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
fn write(_: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(args) = env.params.last() {
		for arg in args {
			println!["{}", arg];
		}
		None
	} else {
		Some("no argument stack".into())
	}
}
