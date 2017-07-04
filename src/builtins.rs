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
use std::usize;

// //////////////////////////////////////////////////////////
// Internal data structures used by Teko
// //////////////////////////////////////////////////////////
use data_structures::*;
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
///
/// For user-defined functions and types please see `user/mod.rs`.
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
		Function : "cell" => cell,
		Function : "cell?" => is_cell,
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

// Want to return Result<Rc<Sourcedata>, String>, I think this is the best way
// The functions should also be arg -> result, access to env... hmmm
// I don't know. Perhaps make define and the like builtins because it's rather
// fundamental. This makes the interface much simpler in the sense that a function
// becomes fn(args: Vec<Statement>) -> Result<Statement, String>, which also means
// no empty stack to worry about. So... as for macros...
// hmmm they kinda need to return multiple statements. Although I could create a
// macro that puts each statement on the stack. The empty macro? I don't know.
// Or rather the nary functor. Yeah that sounds right.
// So (program (write (+ 1 2 3)) 9) would do that, since all args are evaluated
// from left-to-right, there's no worry! Macros can still return multiple args!
// YAY!
// Ok so to summarize fn(args: Vec<Statement>) -> Result<Statement, String>
// Is a string really well-suited? It's an error after all, and it depends on the
// application and what the application wants to convey
// We already made sure to not have errors contain much structured data. I guess
// it's alright like this. We can also use the arity_mismatch to generate strings,
// which can be done automatically. I love the declarativeness. The same can be
// done for macros, which will just need to deconstruct the cell and count the
// amount of heads that are inside.
// Then there's the type of the arguments, which can also be specified, but I'll need
// to do it in such a way that it also deconstructs the arguments, that would be very useful.
// So for example: teko_simple_function!(myfunc(args : 0 => 4) [  Symbol(ref a) String(ref b) ... ] => code
// That'd be neat, we'd save so much on boilerplate code by doing that.
// I'll need to make define and unwind/wind something internal though. It's fine :], I reckon
// those are the only things to stay internal. If you REALLY want to hack you can still do it
// but this makes the openness for hacking just somewhat smaller.
// Plenty of macros use program, like if, but once these return instead, we have no problem.
// One problem I have is how (program) affects TCO, since it requires to be evaluated, which is
// annoying. So what's the solution? Do nothing? Well that's one option.. but hmmmmm
// It seems like we almost need this interface just to keep the core clean... however,...
// It's because of the calling convention, any symbol is just called, and some special
// symbols need access to the stack. Some even iterate over the program stack like unwind does,
// and yet it's still a normal function.
// So perhaps it's best to let a macro abstract itself over the core functions.
// We get best of both worlds: low-level control when we need it, and high-level cleanliness
// when requested :)
macro_rules! teko_simple_function {
	($name:ident $args:ident : $low:expr => $high:expr => $code:block) => {
		#[allow(unused_comparisons)]
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
					Err(error) => Some((None, error)),
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
		fn $name(_: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
			let $arg = env.result.clone();
			let len = $arg.len();
			if let Some(len) = len {
				if len < $low || len > $high {
					return Some((None, arity_mismatch($low, $high, len)));
				}
			} else {
				return Some((None, "macro: input not Cell or Null".into()));
			}
			let result = (|| $code)();
			match result {
				Ok(result) => {
					env.result = result;
					None
				}
				Err(error) => Some((None, error)),
			}
		}
	};
}

/// Logical AND.
teko_simple_function!(and args : 0 => usize::MAX => {
	for arg in args {
		if let Coredata::Boolean(Boolean::False) = arg.1 {
			return Ok(arg.clone());
		} else {
			continue;
		}
	}
	Ok(rcs(Coredata::Boolean(Boolean::True)))
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
	env.result = rcs(Coredata::Null);
	for key in env.store.keys() {
		env.result = rcs(Coredata::Cell(rcs(Coredata::Symbol(key.clone())), env.result.clone()));
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
							return Some((source.clone(), format!["variable already exists: {}", string]));
						}
						env.store.insert(string.clone(), vec![rhs.clone()]);
					} else {
						return Some((source.clone(), arity_mismatch(2, 2, 1)));
					}
				}
				Sourcedata(ref source, ..) => {
					return Some((source.clone(), format![
						"expected String but got {}",
						data_name(symbol)
					]));
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
		let sub =
			rcs(Coredata::Function(Function::Builtin(define_internal, "@define-internal".into())));
		let push = if let Some(ref tail) = args.tail() {
			match tail.1 {
				Coredata::Cell(ref head, _) => {
					vec![rcs(Coredata::Internal(Commands::Call(sub))),
					     rcs(Coredata::Internal(Commands::Parameterize)),
					     head.clone()]
				}
				Coredata::Null => {
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
				Sourcedata(ref source, Coredata::Symbol(ref string)) => {
					program.extend(push);
					program.push(rc(Sourcedata(
						source.clone(),
						Coredata::Internal(Commands::Parameterize),
					)));
					program.push(rc(Sourcedata(source.clone(), Coredata::String(string.clone()))));
				}
				Sourcedata(ref source, ..) => {
					return Some((source.clone(), format![
						"expected Symbol but got: {}",
						data_name(&*head)
					]));
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
				Sourcedata(ref source, Coredata::Integer(ref value)) => {
					if value == &zero::<BigInt>() {
						return Err(format!["argument {} is zero", optional_source(source)]);
					}
					sum = sum / value;
				}
				Sourcedata(Some(ref source), ..) => {
					return Err(format![
						"expected Integer but got {}, {}",
						data_name(arg),
						source,
					]);
				}
				_ => {
					return Err(format!["expected Integer but got {}", data_name(arg)]);
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
							return Err(
								format!["argument {} is zero", optional_source(source)],
							);
						}
						sum = sum / value;
					}
				}
				Sourcedata(Some(ref source), ..) => {
					return Err(format![
						"expected Integer but got {}, {}",
						data_name(arg),
						source,
					]);
				}
				_ => {
					return Err(format!["expected Integer but got {}", data_name(arg)]);
				}
			}
			first = false;
		}
	}
	Ok(rcs(Coredata::Integer(sum)))
});

/// Retrieve the first statement of a function or macro.
teko_simple_function!(doc args : 1 => 1 => {
	let arg = args.first().expect("function macro ensures existence");
	match arg.1 {
		Coredata::Function(Function::Library(_, ref stats)) |
		Coredata::Macro(Macro::Library(_, ref stats)) => {
			if stats.is_empty() {
				Ok(rcs(Coredata::Null))
			} else {
				Ok(stats.last().unwrap().clone())
			}
		}
		_ => {
			Err(format![
				"expected Function or Macro but got {}",
				data_name(arg),
			])
		}
	}
});

/// Integer equality comparison.
teko_simple_function!(eq args : 0 => usize::MAX => {
	let mut last = None;
	let mut result = rcs(Coredata::Boolean(Boolean::True));
	for arg in args.iter() {
		match **arg {
			Sourcedata(_, Coredata::Integer(ref integer)) => {
				if let Some(previous) = last {
					if previous == integer {
						// Do nothing
					} else {
						result = rcs(Coredata::Boolean(Boolean::False));
						break;
					}
					last = Some(integer);
				} else {
					last = Some(integer);
				}
			}
			Sourcedata(Some(ref source), ..) => {
				return Err(format![
					"expected Integer but got {}, {}",
					data_name(arg),
					source,
				]);
			}
			_ => {
				return Err(format!["expected Integer but got {}", data_name(arg)]);
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
		Ok(rcs(Coredata::Error(rcs(Coredata::Null))))
	}
});

/// Evaluates the argument as if it's a program.
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
	use num::ToPrimitive;

	if let Some(arg) = args.last() {
		match **arg {
			Sourcedata(_, Coredata::Integer(ref value)) => {
				if let Some(value) = value.to_i32() {
					::std::process::exit(value);
				} else {
					Err("unable to convert number to value".into())
				}
			}
			Sourcedata(Some(ref source), ..) => {
				Err(format![
					"expected Integer but got {}, {}",
					data_name(arg),
					source,
				])
			}
			_ => {
				Err(format!["expected Integer but got {}", data_name(arg)])
			}
		}
	} else {
		::std::process::exit(0);
	}
});

/// Construct a function object with dynamic scope.
teko_simple_macro!(function args : 2 => usize::MAX => {
	if let Some(head) = args.head() {
		let params = if let Some(params) = collect_cell_of_symbols_into_vec_string(&head) {
			params
		} else {
			return Err("parameter list contains non-symbols".into());
		};
		if let Some(tail) = args.tail() {
			let code = collect_cell_into_revvec(&tail);
			Ok(rcs(Coredata::Function(Function::Library(params, code))))
		} else {
			Err("tail is empty".into())
		}
	} else {
		Err("parameter list is not a list".into())
	}
});

/// The greater-than function for comparing integers.
teko_simple_function!(gt args : 0 => usize::MAX => {
	let mut last = None;
	let mut result = rcs(Coredata::Boolean(Boolean::True));
	for arg in args.iter() {
		match **arg {
			Sourcedata(_, Coredata::Integer(ref integer)) => {
				if let Some(previous) = last {
					if previous > integer {
						// Do nothing
					} else {
						result = rcs(Coredata::Boolean(Boolean::False));
						break;
					}
					last = Some(integer);
				} else {
					last = Some(integer);
				}
			}
			Sourcedata(Some(ref source), ..) => {
				return Err(format![
					"expected Integer but got {}, {}",
					data_name(arg),
					source,
				]);
			}
			_ => {
				return Err(format!["expected Integer but got {}", data_name(arg)]);
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
	} else if let Sourcedata(Some(ref source), ..) = **arg {
		Err(format![
			"expected Cell but got {}, {}",
			data_name(arg),
			source,
		])
	} else {
		Err(format!["expected Cell but got {}", data_name(arg)])
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
	let mut result = rcs(Coredata::Boolean(Boolean::True));
	for arg in args.iter() {
		let data = &arg.1;
		if let Some(previous) = last {
			if previous == data {
				// Do nothing
			} else {
				result = rcs(Coredata::Boolean(Boolean::False));
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
		Ok(rcs(Coredata::Boolean(Boolean::True)))
	} else {
		Ok(rcs(Coredata::Boolean(Boolean::False)))
	}
});

/// Check if the value is a cell type.
teko_simple_function!(is_cell args : 1 => 1 => {
	let arg = args.first().unwrap();
	if let Coredata::Cell(..) = arg.1 {
		Ok(rcs(Coredata::Boolean(Boolean::True)))
	} else {
		Ok(rcs(Coredata::Boolean(Boolean::False)))
	}
});

/// Check if the value is a symbol.
teko_simple_function!(is_symbol args : 1 => 1 => {
	let arg = args.first().unwrap();
	if let Coredata::Symbol(_) = arg.1 {
		Ok(rcs(Coredata::Boolean(Boolean::True)))
	} else {
		Ok(rcs(Coredata::Boolean(Boolean::False)))
	}
});

/// Compute the length of a list.
teko_simple_function!(list_length args : 1 => 1 => {
	let arg = args.first().unwrap();
	if let Some(len) = arg.len() {
		Ok(rcs(Coredata::Integer(len.into())))
	} else {
		Err(format![
			"expected Cell or String but got {}",
			data_name(arg),
		])
	}
});

/// Construct a list (nested cell) of items.
teko_simple_function!(list args : 0 => usize::MAX => {
	let mut result = rcs(Coredata::Null);
	for arg in args.iter().rev() {
		result = rcs(Coredata::Cell(arg.clone(), result));
	}
	Ok(result)
});

/// The less-than function for comparing integers.
teko_simple_function!(lt args : 0 => usize::MAX => {
	let mut last = None;
	let mut result = rcs(Coredata::Boolean(Boolean::True));
	for arg in args.iter() {
		match **arg {
			Sourcedata(_, Coredata::Integer(ref integer)) => {
				if let Some(previous) = last {
					if previous < integer {
						// Do nothing
					} else {
						result = rcs(Coredata::Boolean(Boolean::False));
						break;
					}
					last = Some(integer);
				} else {
					last = Some(integer);
				}
			}
			Sourcedata(Some(ref source), ..) => {
				return Err(format![
					"expected Integer but got {}, {}",
					data_name(arg),
					source,
				]);
			}
			_ => {
				return Err(format!["expected Integer but got {}", data_name(arg)]);
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
		Sourcedata(Some(ref source), ..) => {
			return Err(format![
				"expected Symbol but got {}, {}",
				data_name(&*head),
				source,
			]);
		}
		_ => {
			return Err(format!["expected Symbol but got {}", data_name(&*head)]);
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
			Sourcedata(Some(ref source), ..) => {
				return Err(format![
					"expected Integer but got {}, {}",
					data_name(&*arg),
					source,
				]);
			}
			_ => {
				return Err(format!["expected Integer but got {}", data_name(&*arg)]);
			}
		}
	}
	Ok(rcs(Coredata::Integer(sum)))
});

/// Boolean NOT.
teko_simple_function!(not args : 1 => 1 => {
	let arg = args.first().unwrap();
	if let Coredata::Boolean(Boolean::False) = arg.1 {
		Ok(rcs(Coredata::Boolean(Boolean::True)))
	} else {
		Ok(rcs(Coredata::Boolean(Boolean::False)))
	}
});

/// Boolean (inclusive) OR.
teko_simple_function!(or args : 0 => usize::MAX => {
	for arg in args {
		if let Coredata::Boolean(Boolean::False) = arg.1 {
			continue;
		} else {
			return Ok(rcs(Coredata::Boolean(Boolean::True)));
		}
	}
	Ok(rcs(Coredata::Boolean(Boolean::False)))
});

/// Cell value constructor.
///
/// The second argument must be a `Cell` or `Null`, else it will
/// unwind with an error.
teko_simple_function!(cell args : 2 => 2 => {
	let arg1 = args.first().unwrap();
	let arg2 = args.get(1).unwrap();
	if let Coredata::Cell(..) = arg2.1 {
		// Ok TODO replace with check is_cell_or_null(...)
	} else if let Coredata::Null = arg2.1 {
		// Ok
	} else if let Sourcedata(Some(ref source), ..) = **arg2 {
		return Err(format![
			"expected Cell or Null but got {}, {}",
			data_name(arg2),
			source,
		]);
	} else {
		return Err(format!["expected Cell or Null but got {}", data_name(arg2)]);
	}
	Ok(rcs(Coredata::Cell(arg1.clone(), arg2.clone())))
});

/// Integer addition. `(+ Integer*) => Integer`
teko_simple_function!(plus args : 0 => usize::MAX => {
	let mut sum = zero();
	for arg in args.iter() {
		match **arg {
			Sourcedata(_, Coredata::Integer(ref value)) => {
				sum = sum + value;
			}
			Sourcedata(Some(ref source), ..) => {
				return Err(format![
					"expected Integer but got {}, {}",
					data_name(&**arg),
					source,
				]);
			}
			Sourcedata(None, ..) => {
				return Err(format!["expected Integer but got {}", data_name(&**arg)]);
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
	use data_structures::ParseState;
	use parse::*;
	let mut parser = ParseState::from("tty");
	for ch in io::stdin().bytes() {
		if let Ok(ch) = ch {
			if let Err(state) = parse_character(ch as char, &mut parser) {
				if let Some(error) = state.error {
					return Some((None, format!["parse error: {}", error]));
				} else {
					return Some((None, format!["parse error"]));
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

/// Used by set internall to set variables.
fn set_internal(_: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)> {
	if let Some(args) = env.params.last() {
		if let Some(symbol) = args.first() {
			match **symbol {
				Sourcedata(ref source, Coredata::String(ref string)) => {
					if let Some(rhs) = args.get(1) {
						if !env.store.contains_key(string) {
							return Some((source.clone(), format![
								"variable does not exist, {}",
								string
							]));
						}
						env.store.insert(string.clone(), vec![rhs.clone()]);
					} else {
						return Some((None, arity_mismatch(2, 2, 1)));
					}
				}
				Sourcedata(ref src, ..) => {
					return Some((src.clone(), format![
						"expected String but got {}",
						data_name(symbol)
					]));
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
		let sub = rcs(Coredata::Function(Function::Builtin(set_internal, "@set-internal".into())));
		if let Some(ref tail) = args.tail() {
			match tail.1 {
				Coredata::Cell(ref heado, _) => {
					program.push(rcs(Coredata::Internal(Commands::Call(sub))));
					program.push(rcs(Coredata::Internal(Commands::Parameterize)));
					program.push(heado.clone());
				}
				Coredata::Null => {
					return Some((None, arity_mismatch(2, 2, 0)));
				}
				_ => {
					return Some((None, format!["expected Cell but got {}", tail]));
				}
			}
		} else {
			return Some((None, arity_mismatch(2, 2, 0)));
		}
		program.push(rcs(Coredata::Internal(Commands::Parameterize)));
		if let Some(head) = args.head() {
			match *head {
				Sourcedata(ref source, Coredata::Symbol(ref string)) => {
					program
						.push(Rc::new(Sourcedata(source.clone(), Coredata::String(string.clone()))));
				}
				Sourcedata(ref source, ..) => {
					return Some((source.clone(), format!["expected Cell but got {}", head]));
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
	use std::{thread, time};
	use num::ToPrimitive;
	let arg = args.first().unwrap();
	match **arg {
		Sourcedata(_, Coredata::Integer(ref value)) => {
			if let Some(value) = value.to_u64() {
				thread::sleep(time::Duration::from_millis(value));
			} else {
				return Err("unable to convert number to value".into());
			}
		}
		Sourcedata(Some(ref source), ..) => {
			return Err(format![
				"expected Integer but got {}, {}",
				data_name(arg),
				source,
			]);
		}
		ref arg @ Sourcedata(None, ..) => {
			return Err(format!["expected Integer but got {}", data_name(arg)]);
		}
	}
	Ok(arg.clone())
});

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
			Sourcedata(_, Coredata::Cell(ref head, ref tail)) => {
				let repeats = if let Coredata::Null = tail.1 {
					1
				} else if let Sourcedata(ref source, Coredata::Cell(ref head, ref tail)) = **tail {
					if let Sourcedata(ref source, Coredata::Symbol(ref value)) = **head {
						let code = value.parse::<u32>();
						if let Ok(code) = code {
							code
						} else {
							return Err(format![
								"{}, unable to parse value to unsigned 32-bit integer: {}",
								optional_source(source),
								value,
							]);
						}
					} else {
						return Err(format![
							"{}, tail is not a cell: {}",
							optional_source(source),
							tail,
						]);
					}
				} else {
					return Err("string character only accepts a one or two arguments".into());
				};
				if let Coredata::Symbol(ref value) = head.1 {
					let code = value.parse::<u32>();
					if let Ok(code) = code {
						if let Some(code) = char::from_u32(code) {
							for _ in 0..repeats {
								ret.push(code);
							}
						} else {
							return Err("value is not a valid character value".into());
						}
					} else {
						return Err("value is not an unsigned 32-bit value".into());
					}
				}
				last_symbol = false;
			}
			_ => {
				return Err("unable to parse input".into());
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
				Sourcedata(Some(ref source), ..) => {
					return Err(format![
						"expected Integer but got {}, {}",
						data_name(arg),
						source,
					]);
				}
				_ => {
					return Err(format!["expected Integer but got {}", data_name(arg)]);
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
					return Err(format![
						"expected Integer but got {}, {}",
						data_name(arg),
						source,
					]);
				}
				_ => {
					return Err(format!["expected Integer but got {}", data_name(arg)]);
				}
			}
			first = false;
		}
	} else {
		return Err(arity_mismatch(1, usize::MAX, 0));
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
	} else if let Sourcedata(Some(ref source), ..) = **arg {
		Err(format![
			"expected Cell but got {}, {}",
			data_name(arg),
			source,
		])
	} else {
		Err(format!["expected Cell but got {}", data_name(arg)])
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
