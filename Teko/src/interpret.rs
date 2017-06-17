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

/// Optimizes tail calls by seeing if the current `params` can be merged with the top of the stack.
///
/// If the top of the stack contains `Commands::Deparameterize`, then the variables to be popped
/// are merged into that [top] object. This is all that's needed to optimize tail calls.
fn optimize_tail_call(program: &mut Program,
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

/// Quote elements
///
/// A builtin macro always stores the tail of the invocation inside `env.result`, so this macro is
/// empty; it doesn't need to do anything.
fn quote(top:     &Statement,
         program: &mut Program,
         env:     &mut Env) {
	println!["Created quoted list"];
}

/// Create a string
///
/// Creates a string from the given symbols by inserting single spaces inbetween each symbol.
/// TODO: Allow subexpressions; implement string interpolation and non-printable character insertion.
fn string(top:     &Statement,
          program: &mut Program,
          env:     &mut Env) {
	let vec = collect_pair_into_vec_string(&env.result);
	env.result = Rc::new(Sourcedata(None, Coredata::String(vec.join(" "))));
	println!["Created string"];
}

fn wind(top:     &Statement,
        program: &mut Program,
        env:     &mut Env) {
	println!["Wind macro"];
	let args = env.result.clone();
	let code = collect_pair_into_vec(&args);
	program.push(Rc::new(Sourcedata(None, Coredata::Internal(Commands::Wind))));
	program.extend(code.iter().cloned());
}

fn unwind(top:     &Statement,
          program: &mut Program,
          env:     &mut Env) {
	println!["Unwind macro"];
	env.result = env.params.last().unwrap().last().unwrap().clone();
	while let Some(top) = program.pop() {
		match top.1 {
			Coredata::Internal(Commands::Deparameterize(ref arguments)) => {
				pop_parameters(&top, program, env, arguments);
			},
			Coredata::Internal(Commands::Call(..)) => {
				env.params.pop();
			},
			Coredata::Internal(Commands::Wind) => {
				break;
			},
			_ => {
			},
		}
	}
}

fn make_unwind() -> Statement {
	Rc::new(Sourcedata(None, Coredata::Internal(Commands::Call(Rc::new(Sourcedata(None, Coredata::Function(Function::Builtin(unwind))))))))
}

fn error(top:     &Statement,
         program: &mut Program,
         env:     &mut Env) {
	if let Some(args) = env.params.last() {
		if args.len() >= 2 {
			env.result = Rc::new(Sourcedata(None, Coredata::Error(Rc::new(Sourcedata(None, Coredata::String("Arity mismatch; Too many arguments to error".into()))))));
			program.push(make_unwind());
		} else {
			if let Some(arg) = args.first() {
				env.result = Rc::new(Sourcedata(None, Coredata::Error(arg.clone())));
			} else {
				env.result = Rc::new(Sourcedata(None, Coredata::Error(Rc::new(Sourcedata(None, Coredata::Null)))));
			}
		}
	} else {
		panic!["The parameter list does not contain a list; this is an internal error that should not happen"];
	}
}

fn not(top:     &Statement,
       program: &mut Program,
       env:     &mut Env) {
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
	println!["Took head"];
}


fn head(top:     &Statement,
        program: &mut Program,
        env:     &mut Env) {
	let args = env.params.last().expect("Should exist by virtue of functions");
	if args.len() != 1 {
		panic!("should have only a single arg");
	} else {
		env.result = args.first().unwrap().head().clone();
	}
	println!["Took head"];
}

fn tail(top:     &Statement,
        program: &mut Program,
        env:     &mut Env) {
	let args = env.params.last().expect("Should exist by virtue of functions");
	if args.len() != 1 {
		panic!("should have only a single arg");
	} else {
		env.result = args.first().unwrap().tail().clone();
	}
	println!["Took tail"];
}

fn pair(top:     &Statement,
        program: &mut Program,
        env:     &mut Env) {
	let args = env.params.last().expect("Should exist by virtue of functions");
	if args.len() != 2 {
		panic!("should have two args");
	} else {
		env.result = Rc::new(Sourcedata(None, Coredata::Pair(args.first().unwrap().clone(),
		                                                                  args.get(1).unwrap().clone())));
	}
	println!["Took tail"];
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
	env.result = Rc::new(Sourcedata(None, Coredata::Macro(Macro::Library(params, code))));
	println!["Created macro object"];
}

fn function(top:     &Statement,
            program: &mut Program,
            env:     &mut Env) {
	let args = env.result.clone();
	let params = collect_pair_into_vec_string(&args.head());
	let mut code = collect_pair_into_vec(&args.tail());
	env.result = Rc::new(Sourcedata(None, Coredata::Function(Function::Library(params, code))));
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
		program.push(Rc::new(Sourcedata(None,
		                                Coredata::Internal(Commands::Call(Rc::new(Sourcedata(None,
		                                                                                     Coredata::Function(Function::Builtin(define_internal)))))))));
		program.push(Rc::new(Sourcedata(None, Coredata::Internal(Commands::Parameterize))));
		match arguments.tail().1 {
			Coredata::Pair(ref heado, _) => {
				program.push(heado.clone());
			},
			_ => {
				panic!{"it cant be"};
			}
		}
		program.push(Rc::new(Sourcedata(None, Coredata::Internal(Commands::Parameterize))));
		match arguments.head().1 {
			Coredata::Symbol(ref string) => {
				program.push(Rc::new(Sourcedata(None, Coredata::String(string.clone()))));
			},
			_ => {
				panic!("Define did not get a symbol!");
			},
		}
	}
	env.params.push(vec!());
}

fn if_conditional(top:     &Statement,
                  program: &mut Program,
                  env:     &mut Env) {
	println!["Inside if"];
	let arguments = env.result.clone();
	program.push(Rc::new(Sourcedata(None, Coredata::Internal(Commands::If(arguments.tail().head(), arguments.tail().tail().head())))));
	program.push(arguments.head());
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
	println!["Sleep func"];
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
	env.result = Rc::new(Sourcedata(None, Coredata::Integer(sum)));
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
	env.result = Rc::new(Sourcedata(None, Coredata::Integer(sum)));
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
	env.result = Rc::new(Sourcedata(None, Coredata::Integer(sum)));
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
	env.result = Rc::new(Sourcedata(None, Coredata::Integer(sum)));
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
	ret.reverse();
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
	to_return.reverse();
	to_return
}

fn pop_parameters(top:     &Statement,
                  program: &mut Program,
                  env:     &mut Env,
                  args:    &Vec<String>) {
	for arg in args {
		print!["{}, ", arg];
		env.store.get_mut(arg).expect("Should exist in the argument store!").pop();
		if env.store.get(arg).unwrap().is_empty() {
			env.store.remove(arg);
		}
	}
}

macro_rules! construct_builtins {
	($($t:tt : $e:expr => $i:ident),*,) => {
		construct_builtins![$($t : $e => $i),*]
	};
	($($t:tt : $e:expr => $i:ident),*) => {
		[
			$(
				($e.into(), vec![Rc::new(Sourcedata(None, Coredata::$t($t::Builtin($i))))])
			),*
		].iter().cloned().collect()
	};
}

pub fn interpret(program: Program) {
	let env = Env {
		store: construct_builtins! {
			// TODO: could be made even shorter by creating one space for functions and another for macros, or too much context to be readable?
			Function : "+" => plus,
			Function : "-" => minus,
			Function : "*" => multiply,
			Function : "/" => divide,
			Function : "not" => not,
			Function : "error" => error,
			Function : "head" => head,
			Function : "tail" => tail,
			Function : "pair" => pair,
			Function : "sleep" => sleep,
			Function : "unwind" => unwind,
			Macro : "'" => quote,
			Macro : "\"" => string,
			Macro : "if" => if_conditional,
			Macro : "set!" => set,
			Macro : "wind" => wind,
			Macro : "define" => define,
			Macro : "fn" => function,
			Macro : "mo" => make_macro,
		},
		params: Vec::with_capacity(VEC_CAPACITY),
		result: Rc::new(Sourcedata(None, Coredata::Null)),
	};
	eval(program, env);
}

/// Evaluates a program with a given environment
///
/// The `program` is considered completely evaluated when it is empty. The result of the program
/// is stored in `env.result`.
fn eval(mut program: Program, mut env: Env) {
	program.reverse(); // TODO: Do this in the parser instead, doesn't fit in here.
	macro_rules! push { ($t:tt) => { program.push(Rc::new(Sourcedata(None, Coredata::Internal(Commands::$t)))); }; }
	while let Some(top) = program.pop() {
		println!["PROGRAM LENGTH: {}", program.len()];
		println!["{}", top];
		match &*top {
			&Sourcedata(ref source, Coredata::Pair(ref head, ref tail)) => {
				println!["Expanding"];
				program.push(Rc::new(Sourcedata(tail.0.clone(), Coredata::Internal(Commands::Prepare(tail.clone())))));
				program.push(head.clone());
			},
			&Sourcedata(_, Coredata::Internal(Commands::Wind)) => {
				// Do nothing
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
						for argument in collect_pair_into_vec(arguments).iter() {
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
						program.push(Rc::new(Sourcedata(None, Coredata::Internal(Commands::Evaluate))));
						let next = Rc::new(Sourcedata(None, Coredata::Internal(Commands::Deparameterize(optimize_tail_call(&mut program, &vec![bound.clone()])))));
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
							// TODO Don't do this. It's dangerous: panics easily
							counter += 1;
						}
						env.params.pop();
						let next = Rc::new(Sourcedata(None, Coredata::Internal(Commands::Deparameterize(optimize_tail_call(&mut program, arguments)))));
						program.push(next);
						// program.push(Rc::new(Sourcedata(None, Coredata::Internal(Commands::Deparameterize(arguments.clone())))));
						program.extend(transfer.iter().cloned());
					},
					_ => {
						panic!("unknown transfer function");
					},
				}
			},
			&Sourcedata(ref source, Coredata::Internal(Commands::If(ref first, ref second))) => {
				if let Coredata::Null = env.result.1 {
					program.push(second.clone());
				} else {
					program.push(first.clone());
				}
			},
			&Sourcedata(ref source, Coredata::Internal(Commands::Deparameterize(ref arguments))) => {
				print!["Deparameterizing: "];
				pop_parameters(&top, &mut program, &mut env, arguments);
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
