use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;
use super::VEC_CAPACITY;

use num::bigint::BigInt;
use num::rational::BigRational;
use num::Complex;
use num::FromPrimitive;

use data_structures::{Commands, Data, Env, Source};

fn collect_arguments_data(mut args: Rc<Data>) -> Vec<Rc<Data>> {
	let mut arguments = Vec::with_capacity(3);
	loop {
		if let &Data::Pair(_, ref head, ref tail) = &*args.clone() {
			arguments.push(head.clone());
			args = tail.clone();
		} else {
			break;
		}
	}
	arguments
}

fn collect_arguments(mut args: Rc<Data>) -> Vec<String> {
	let mut arguments = Vec::with_capacity(3);
	loop {
		if let &Data::Pair(_, ref head, ref tail) = &*args.clone() {
			if let &Data::Symbol(_, ref string) = &**head {
				arguments.push(string.clone());
				args = tail.clone();
			} else {
				break;
			}
		} else {
			break;
		}
	}
	arguments
}

pub fn interpret(program: Vec<Rc<Data>>) {
	let env = Env {
		content:      [ //("+".into(), vec![Rc::new(Data::Null(Source::default()))])
		              ].iter().cloned().collect(),
		call_stack:   Vec::with_capacity(VEC_CAPACITY),
		params:       Vec::with_capacity(VEC_CAPACITY),
		return_value: Rc::new(Data::Null(Source::default())),
	};
	eval(program, env);
}

fn eval(mut program: Vec<Rc<Data>>, mut env: Env) {
	program.reverse();
	println!["program: {:#?}\nenv: {:#?}", program, env];
	while let Some(top) = program.pop() {
		match &*top {
			&Data::Null(..) => {
				env.return_value = top.clone();
			},
			////////////////////////////////////////////////////////////
			// Keyword forms and expressions                          //
			////////////////////////////////////////////////////////////
			&Data::Pair(ref source, ref head, ref tail) => {
				match &**head {
					&Data::Symbol(ref atom_source, ref string) => {
						match &string[..] {
							"define" => {
								if let &Data::Symbol(_, ref string) = &*tail.head() {
									program.push(Rc::new(Data::Internal(Source::default(), Commands::Define(string.clone()))));
									program.push(tail.tail().head());
								} else {
									panic!["define is of the form (define atom expr), expected an atom but got something else"];
								}
							},
							"set!" => {
								if let &Data::Symbol(_, ref string) = &*tail.head() {
									program.push(Rc::new(Data::Internal(Source::default(), Commands::Set(string.clone()))));
									program.push(tail.tail().head());
								} else {
									panic!["set! is of the form (define atom expr), expected an atom but got something else"];
								}
							},
							"fn" => {
								let args = tail.head();
								let code = tail.tail();
								env.return_value = Rc::new(Data::Function(source.clone(), collect_arguments(args), code));
							},
							"mo" => {
								let arg = tail.head();
								let code = tail.tail();
								if let &Data::Symbol(_, ref string) = &*arg {
									env.return_value = Rc::new(Data::Macro(source.clone(), string.clone(), code));
								} else {
									panic!["Can't have a non-symbol as an argument for a macro"];
								}
							},
							"if" => {
								program.push(Rc::new(Data::Internal(Source::default(), Commands::If(tail.tail().head(), tail.tail().tail().head()))));
								program.push(tail.head());
							},
							atom @ _ => {
								// Do auxilliary funccalls
								program.push(Rc::new(Data::Internal(source.clone(), Commands::Prepare(tail.clone()))));
								program.push(Rc::new(Data::Internal(atom_source.clone(), Commands::Pushcall)));
								program.push(head.clone());
							},
						}
					},
					expr @ _ => {
						// Do expressions
						program.push(Rc::new(Data::Internal(source.clone(), Commands::Prepare(tail.clone()))));
						program.push(Rc::new(Data::Internal(source.clone(), Commands::Pushcall)));
						program.push(head.clone());
					},
				}
			},
			////////////////////////////////////////////////////////////
			// Internal commands like +, -, and interpreter internals //
			////////////////////////////////////////////////////////////
			&Data::Internal(ref source, ref commands) => {
				match commands {
					&Commands::Define(ref string) => {
						let new_stack = if let Some(vector) = env.content.get_mut(string) {
							panic!("Can't re-define {}", string);
							false
						} else {
							true
						};
						if new_stack {
							env.content.insert(string.clone(), vec![env.return_value.clone()]);
						}
					},
					&Commands::Set(ref string) => {
						if let Some(vector) = env.content.get_mut(string) {
							*vector.last_mut().unwrap() = env.return_value.clone();
						} else {
							panic!["Can not set a variable that has never been declared"];
						}
					},
					&Commands::If(ref if_true, ref if_false) => {
						if let &Data::Null(..) = &*env.return_value {
							program.push(if_false.clone());
						} else {
							program.push(if_true.clone());
						}
					},
					&Commands::Prepare(ref data) => {
						match &**env.call_stack.last().unwrap() {
							&Data::Internal(_, Commands::Plus) => {
								program.push(Rc::new(Data::Internal(Source::default(), Commands::Call)));
								let args = collect_arguments_data(data.clone());
								for arg in args.iter() {
										program.push(Rc::new(Data::Internal(Source::default(), Commands::Parameterize)));
										program.push(arg.clone());
								}
								// program.push(Rc::new(Data::Internal(Source::default(), Commands::));
							},
							&Data::Internal(_, Commands::Minus) => {
								program.push(Rc::new(Data::Internal(Source::default(), Commands::Call)));
								let args = collect_arguments_data(data.clone());
								for arg in args.iter() {
										program.push(Rc::new(Data::Internal(Source::default(), Commands::Parameterize)));
										program.push(arg.clone());
								}
								// program.push(Rc::new(Data::Internal(Source::default(), Commands::));
							},
							_ => {
								panic!("Is not callable");
							},
						}
					},
					&Commands::Parameterize => {
						env.params.last_mut().unwrap().push(env.return_value.clone());
					},
					&Commands::Pushcall => {
						env.call_stack.push(env.return_value.clone());
						env.params.push(vec![]);
					},
					&Commands::Call => {
						match &**env.call_stack.last().unwrap() {
							&Data::Internal(_, Commands::Plus) => {
								let mut accumulator = BigInt::parse_bytes(b"0", 10).unwrap();
								for value in env.params.last().unwrap().iter() {
									match &**value {
										&Data::Integer(_, ref value) => {
											accumulator = accumulator + value;
										},
										_ => {
											panic!("Can't add non-integer");
										},
									}
								}
								env.return_value = Rc::new(Data::Integer(Source::default(), accumulator));
							},
							&Data::Internal(_, Commands::Minus) => {
								let mut accumulator = BigInt::parse_bytes(b"0", 10).unwrap();
								for value in env.params.last().unwrap().iter() {
									match &**value {
										&Data::Integer(_, ref value) => {
											accumulator = accumulator - value;
										},
										_ => {
											panic!("Can't add non-integer");
										},
									}
								}
								env.return_value = Rc::new(Data::Integer(Source::default(), accumulator));
							},
							_ => {
								panic!("Unhandled call");
							},
						}
						env.params.pop();
						env.call_stack.pop();
					},
					other @ _ => {
						panic!["Do not handle the command: {:#?}", other];
					},
				}
			},
			// A string on the stack is simply a number or a reference to some variable
			&Data::Symbol(ref source, ref string) => {
				match &string[..] {
					// This can easily be macrofied as
					// "+" => Plus
					// "-" => Minus
					// ...
					"+" => {
						env.return_value = Rc::new(Data::Internal(source.clone(), Commands::Plus));
					},
					"-" => {
						env.return_value = Rc::new(Data::Internal(source.clone(), Commands::Minus));
					},
					// Either parse a number or refer to an element in the hash set
					_ => {
						if let Some(number) = BigInt::parse_bytes(string.as_bytes(), 10) {
							print!["{}", number];
							env.return_value = Rc::new(Data::Integer(source.clone(), number));
						} else {
							env.return_value = env.content.get(string).unwrap().last().unwrap().clone();
						}
					},
				}
			},
			other @ _ => {
				panic!["Element should not exist on the program stack: {:#?}", other];
			},
		}
		// println!["program: {:#?}\nenv: {:#?}", program, env];
		println!["program: {:#?}\nenv: {:#?}", program, env];
	}
	println!["final env: {:#?}", env];
}

#[cfg(test)]
mod tests {
	use super::*;
	use parse::parse_file;
	#[test]
	fn test() {
		let p = parse_file("input").ok().unwrap();
		// println!["{:#?}", p];
		// p.iter().map(|x| println!["{}", x]).count();
		interpret(p);
		//println!["{:#?}", p.head()];
		//println!["{:#?}", p.tail()];
		//println!["{:#?}", p.head()];
	}
}
