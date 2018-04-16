//! Evaluation and library functions.
//!
//! ```
//! extern crate teko;
//! extern crate num_traits;
//! use num_traits::cast::ToPrimitive;
//! fn main() {
//! 	let program = teko::parse::parse_string("(+ 1 2 4) (+ 1 2)").ok().unwrap();
//! 	let env = teko::interpret::interpret(program);
//! 	match env.get_result().1 {
//! 		teko::data_structures::Coredata::Integer(ref value) => {
//! 			assert_eq![value.to_i32().unwrap(), 3];
//! 		}
//! 		_ => {
//! 			panic!["Expected Integer but got a different data type"];
//! 		}
//! 	}
//! }
//! ```
use builtins::*;
use data_structures::*;
use data_structures::Sourcedata as Srcdata;
use data_structures::Coredata as Core;
use data_structures::Commands as Cmds;
use super::VEC_CAPACITY;
use utilities::*;

use num::BigInt;

/// Evals a program with a given environment.
///
/// The `program` is considered completely evaluated when it is empty. The result of the program
/// is stored using `env.set_result`. This function is mainly used to evaluate a program in some
/// environment context.
///
/// ```
/// extern crate teko;
/// extern crate num_traits;
/// use num_traits::cast::ToPrimitive;
/// fn main() {
/// 	let program = teko::parse::parse_string("(+ 1 2 4) (+ 1 2)").ok().unwrap();
/// 	let env = teko::interpret::initialize_environment_with_standard_library();
/// 	let env = teko::interpret::eval(program, env);
/// 	match env.get_result().1 {
/// 		teko::data_structures::Coredata::Integer(ref value) => {
/// 			assert_eq![value.to_i32().unwrap(), 3];
/// 		}
/// 		_ => {
/// 			panic!["Expected Integer but got a different data type"];
/// 		}
/// 	}
/// }
/// ```
pub fn eval(mut program: Program, mut env: Env) -> Env {
	macro_rules! ppush {
		($source:expr, $data:expr,) => { ppush![$source, $data] };
		($source:expr, $data:expr) => {
			program.push(rc(Srcdata($source.clone(), $data)))
		};
	}
	let true_obj = rcs(Coredata::Boolean(true));
	let false_obj = rcs(Coredata::Boolean(false));
	while let Some(top) = program.pop() {
		// This part requires some explanation. The program is simply a Vec containing
		// Rc<Srcdata>. The top element is interpreted and matches one of the cases in
		// this code. For expressions we want to expand the top of the stack like so:
		//
		// (a b c)  =>  b param c param call(a) deparam(b c)
		//
		// param pushes the last evaluation onto the 'parameter stack', so b followed by
		// param puts the result of 'b' on the parameter stack. Likewise for c.
		// Then we have (call a) which is of type Cmds::Call and will call the function.
		// A change to the variable a from expression b or c DOES NOT MATTER, because
		// call(a) stores the actual function, not a variable referencing the function!
		//
		// deparam removes the references from the store. Now the store is actually a
		// Map<String, Vec<_>>, where _ is any data. So when we call a function, we push
		// a variable onto the Vec in the store, and when we do a deparam we remove it.
		//
		// TCO is implemented by looking at the top when expanding a call:
		// suppose a calls (d b e) at its tail:
		//
		// b param e param call(d) deparam(b e) deparam(b c)
		//
		// Note how the deparams can be merged:
		//
		// b param e param call(d) deparam(b c e)
		//
		// This is the method by which TCO is implemented. Note that merging ensures that
		// the correct number of variables are popped from the store.
		let src = &top.0;
		match top.1 {
			// This is where a call of a function happens, remember (a b c) => b param c param call(a) deparam(b c)
			// Right now we're at the call stage: call(a) deparam(b c)
			// We check if the function is builtin or user-defined, and call it.
			Core::Internal(Cmds::Call(ref statement)) => {
				// This nesting should not be necessary, make call hold valid data!
				let source = &statement.0;
				match statement.1 {
					Core::Function(Function::Builtin(ref transfer, ..)) => {
						let error = transfer(&mut program, &mut env);
						env.params.pop();
						err(src, &error, &mut program, &mut env);
					}
					Core::Function(Function::Library(ref parameters, ref transfer)) => {
						if let Some(arguments) = env.params.pop() {
							if arguments.len() != parameters.len() {
								err(
									src,
									&Some((
										source.clone(),
										arity_mismatch(
											parameters.len(),
											parameters.len(),
											arguments.len(),
										),
									)),
									&mut program,
									&mut env,
								);
							} else {
								let cmd = Cmds::Depar(
									optimize_tail_call(&mut program, &mut env, parameters),
								);
								ppush![src, Core::Internal(cmd)];
								for (counter, parameter) in parameters.iter().enumerate() {
									env.push(parameter, arguments[counter].clone());
								}
								program.extend(transfer.iter().cloned());
							}
						}
					}
					// Can't actually happen, prepare checks it
					// The type system ought to reflect this
					_ => {
						err(
							src,
							&Some((None, "element not callable".into())),
							&mut program,
							&mut env,
						);
					}
				}
			}
			Core::Internal(Cmds::Depar(ref arguments)) => {
				pop_parameters(&mut program, &mut env, arguments);
			}
			Core::Internal(Cmds::Eval) => {
				program.push(env.get_result());
			}
			Core::Internal(Cmds::If(ref first, ref second)) => {
				if let Core::Boolean(false) = env.get_result().1 {
					program.push(second.clone());
				} else {
					program.push(first.clone());
				}
			}
			Core::Internal(Cmds::Param) => {
				env.paramize();
			}
			Core::Internal(Cmds::Prep(ref arguments)) => {
				let source = &env.get_result().0;
				match env.get_result().1 {
					Core::Function(..) => {
						env.params.push(vec![]);
						ppush![
							src,
							Core::Internal(Cmds::Call(env.get_result())),
						];
						for argument in collect_cell_into_revvec(arguments) {
							ppush![None, Core::Internal(Cmds::Param)];
							program.push(argument.clone());
						}
					}
					Core::Macro(Macro::Builtin(ref transfer, ..)) => {
						env.set_result(arguments.clone());
						let error = transfer(&mut program, &mut env);
						err(src, &error, &mut program, &mut env);
					}
					Core::Macro(Macro::Library(ref bound, ref code)) => {
						ppush![None, Core::Internal(Cmds::Eval)];
						let command = optimize_tail_call(&mut program, &mut env, &[bound.clone()]);
						let t: &str = bound.into();
						env.push(bound, arguments.clone());
						ppush![
							src,
							Core::Internal(Cmds::Depar(command)),
						];
						program.extend(code.iter().cloned());
					}
					_ => {
						err(
							src,
							&Some((source.clone(), "element not callable".into())),
							&mut program,
							&mut env,
						);
					}
				}
			}
			Core::Internal(Cmds::Wind) => {
				// Wind is used only as a marker when unwinding.
			}
			Core::Cell(ref head, ref tail) => {
				// (a b c) => a prep(b c), so if 'a' is a call then it also works:
				// ((a) b c) => (a) prep(b c)
				// In fact, this works for arbitrary expressions 'a'
				ppush![head.0, Core::Internal(Cmds::Prep(tail.clone()))];
				program.push(head.clone());
			}
			Core::Symbol(ref symbol) => {
				let string: &str = symbol.into();
				if let Some(number) = BigInt::parse_bytes(string.as_bytes(), 10) {
					env.set_result(rc(Srcdata(src.clone(), Core::Integer(number))));
				// TODO Just copy a reference to a global boolean, since these are immutable
				} else if string == "true" {
					env.set_result(true_obj.clone());
				} else if string == "false" {
					env.set_result(false_obj.clone());
				} else {
					let (error, result) = if let Some(value) = env.get(&Symbol::from(string)) {
						(None, Some(value.clone()))
					} else {
						(Some((src.clone(), not_found(string))), None)
					};
					if let Some(result) = result {
						env.set_result(result);
					} else {
						err(src, &error, &mut program, &mut env);
					}
				}
			}
			_ => {
				env.set_result(top.clone());
			}
		}
	}
	env
}

/// Initializes the environment with the standard library.
///
/// ```
/// extern crate teko;
/// let _: teko::data_structures::Env =
/// 	teko::interpret::initialize_environment_with_standard_library();
/// ```
pub fn initialize_environment_with_standard_library() -> Env {
	Env {
		store: create_builtin_library_table(),
		params: Vec::with_capacity(VEC_CAPACITY),
		result: rc(Srcdata(None, Core::Null())),
	}
}

/// Sets up a standard environment and evaluate the program.
///
/// Used to evaluate a program with the standard library and all builtins.
///
/// ```
/// extern crate teko;
/// extern crate num_traits;
/// use num_traits::cast::ToPrimitive;
/// fn main() {
/// 	let program = teko::parse::parse_string("(+ 1 2 4) (+ 1 2)").ok().unwrap();
/// 	let env = teko::interpret::interpret(program);
/// 	match env.get_result().1 {
/// 		teko::data_structures::Coredata::Integer(ref value) => {
/// 			assert_eq![value.to_i32().unwrap(), 3];
/// 		}
/// 		_ => {
/// 			panic!["Expected Integer but got a different data type"];
/// 		}
/// 	}
/// }
/// ```
pub fn interpret(program: Program) -> Env {
	let env = initialize_environment_with_standard_library();
	eval(program, env)
}

#[cfg(test)]
mod tests {
	use super::*;
	use parse::parse_file;
	#[test]
	fn test_interpreter() {
		let p = parse_file("examples/basic.tko").ok().unwrap();
		interpret(p);
	}
}
