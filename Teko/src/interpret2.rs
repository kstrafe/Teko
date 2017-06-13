use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;
use super::VEC_CAPACITY;

use num::bigint::BigInt;
use num::rational::BigRational;
use num::Complex;
use num::FromPrimitive;

#[derive(Clone, Debug)]
pub struct Source {
	pub line:   usize,
	pub column: usize,
	pub source: String,
}

impl Default for Source {
	fn default() -> Source {
		Source { line: 1, column: 1, source: "unknown".into() }
	}
}

// All these commands depend on the environment variable called Return, which is the
// result of the previously executed expression.
// So Define("x") simply means to map "x" => Return
#[derive(Clone, Debug)]
pub enum Commands {
	Define(String),              // Map String := Return
	Set(String),                 // Set value at String = Return
	If(Rc<Data>, Rc<Data>),      // If Return is non-null, run If.0, else run If.1
	Parameterize,                // Push Return into the function parameter list
	Deparameterize(Vec<String>), // Pop parameters from the environment, this happens after a function call is done
	Pushcall,                    // Push Return onto the environment's call stack
	Prepare(Rc<Data>),           // Use the top of the call stack and and prepare for a function or macro call
	Call,                        // Perform a function or macro call
	Empty,                       // Placeholder for nothing
}

#[derive(Clone, Debug)]
pub enum Data {
	Complex  (Source, Complex<BigRational>),
	Function (Source, Vec<String>, Rc<Data>),
	Integer  (Source, BigInt),
	Macro    (Source, String, Rc<Data>),
	Null     (Source),
	Pair     (Source, Rc<Data>, Rc<Data>),
	Internal (Source, Commands),
	Rational (Source, BigRational),
	String   (Source, String),
	Symbol   (Source, String),
}

#[derive(Debug)]
struct Env {
	content:      HashMap<String, Vec<Rc<Data>>>,
	call_stack:   Vec<Rc<Data>>,
	params:       Vec<Rc<Data>>,
	return_value: Rc<Data>,
}

impl Env {
	fn set_return(&mut self, data: Rc<Data>) {
		self.return_value = data;
	}

	fn set_content_to_return(&mut self, string: &String) {
		self.return_value = self.content.get(string).unwrap().first().unwrap().clone();
	}
}

impl fmt::Display for Data {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		enum ToDisplay<'a> {
			ToPrint(&'a Data),
			ClosingParenthesis,
		}
		let mut to_print: Vec<ToDisplay> = vec![ToDisplay::ToPrint(self)];
		let mut require_space = false;
		let mut top_level = true;
		while let Some(data) = to_print.pop() {
			match data {
				ToDisplay::ToPrint(data) => {
					match data {
						&Data::Complex  (_, ref complex) => {
							if require_space {
								write![f, " "]?;
							}
							write![f, "{}", complex]?;
							require_space = true;
						},
						&Data::Function (_, ref params, ref code) => {
							if require_space {
								write![f, " "]?;
							}
							write![f, "(fn ("]?;
							write![f, "{}", params.join(" ")]?;
							write![f, ")"]?;
							to_print.push(ToDisplay::ClosingParenthesis);
							to_print.push(ToDisplay::ToPrint(code));
							require_space = true;
						},
						&Data::Integer  (_, ref integer) => {
							if require_space {
								write![f, " "]?;
							}
							write![f, "{}", integer]?;
						},
						&Data::Macro    (_, ref param, ref code) => {
							if require_space {
								write![f, " "]?;
							}
							write![f, "(mo {}", param]?;
							to_print.push(ToDisplay::ClosingParenthesis);
							to_print.push(ToDisplay::ToPrint(code));
							require_space = true;
						},
						&Data::Null(..) => { write![f, "()"]?; require_space = true },
						&Data::Pair     (_, ref head, ref tail) => {
							if require_space {
								write![f, " "]?;
							}
							if top_level {
								write![f, "("]?;
								to_print.push(ToDisplay::ClosingParenthesis);
							}
							if let Data::Null(..) = **tail {
							} else {
								to_print.push(ToDisplay::ToPrint(tail));
							}
							if let Data::Pair(..) = **head {
								write![f, "("]?;
								to_print.push(ToDisplay::ClosingParenthesis);
							}
							to_print.push(ToDisplay::ToPrint(head));
							require_space = false;
						},
						&Data::Internal(..) => {
							write![f, "|"]?;
						},
						&Data::Rational (_, ref rational) => {
							if require_space {
								write![f, " "]?;
							}
							write![f, "{}", rational]?;
							require_space = true;
						},
						&Data::String   (_, ref string) => {
							if require_space {
								write![f, " "]?;
							}
							write![f, "{}", string]?;
							require_space = true;
						},
						&Data::Symbol   (_, ref string) => {
							if require_space {
								write![f, " "]?;
							}
							write![f, "{}", string]?;
							require_space = true;
						},
					}
				},
				ToDisplay::ClosingParenthesis => {
					write![f, ")"]?;
					require_space = true;
				},
			}
			top_level = false;
		}
		write!(f, "")
	}
}

pub fn interpret(program: Vec<Rc<Data>>) {
	let env = Env {
		content:      [("+".into(), vec![Rc::new(Data::Null(Source::default()))])
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
			&Data::Null(ref source, ..) => {
				env.return_value = top.clone();
			},
			&Data::Pair(_, ref head, ref tail) => {
				match &**head {
					&Data::Symbol(_, ref string) => {
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
								println!{"{}", code};
							},
							"mo" => {
								// Do macros
								unimplemented!();
							},
							"if" => {
								program.push(Rc::new(Data::Internal(Source::default(), Commands::If(tail.tail().head(), tail.tail().tail().head()))));
								program.push(tail.head());
							},
							atom @ _ => {
								// Do auxilliary funccalls
								program.push(Rc::new(Data::Internal(Source::default(), Commands::Pushcall)));
								program.push(head.clone());
							},
						}
					},
					expr @ _ => {
						// Do expressions
						unimplemented!();
					},
				}
			},
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
					&Commands::Pushcall => {
						env.call_stack.push(env.return_value.clone());
					},
					other @ _ => {
						panic!["Do not handle the command: {:#?}", other];
					},
				}
			},
			// A string on the stack is simply a number or a reference to some variable
			&Data::Symbol(ref source, ref string) => {
				if let Some(number) = BigInt::parse_bytes(string.as_bytes(), 10) {
					print!["{}", number];
					env.return_value = Rc::new(Data::Integer(source.clone(), number));
				} else {
					env.return_value = env.content.get(string).unwrap().last().unwrap().clone();
				}
			},
			other @ _ => {
				panic!["Element should not exist on the program stack: {:#?}", other];
			},
		}
		println!["program: {:#?}\nenv: {:#?}", program, env];
	}
	println!["final env: {:#?}", env];
}

// TODO: Can this macro be shortened even more?
macro_rules! make_mass_match {
	($i:ident $p:tt $d:expr, $r:ty, $m:ident $e:expr => $($t:tt),*) => {
		pub fn $i$p -> $r {
			match $d {
				$(&Data::$t (ref $m, ..) => { $e },)*
			}
		}
	};
	($n:tt $i:ident $p:tt $d:expr, $r:ty, $m:ident $e:expr => $($t:tt),*) => {
		pub fn $i$p -> $r {
			match $d {
				$(&$n Data::$t (ref $n $m, ..) => { $e },)*
			}
		}
	};
}

impl Data {
	pub fn head(&self) -> Rc<Data> {
		if let &Data::Pair(_, ref head, _) = self {
			head.clone()
		} else {
			Rc::new(Data::Null(Source::default()))
		}
	}
	pub fn tail(&self) -> Rc<Data> {
		if let &Data::Pair(_, _, ref tail) = self {
			tail.clone()
		} else {
			Rc::new(Data::Null(Source::default()))
		}
	}
	make_mass_match! {
		get_source(&self) self, Source, source source.clone()
		=> Complex, Function, Integer, Macro, Null, Pair, Internal, Rational, String, Symbol
	}
	make_mass_match! {
		mut set_source(&mut self, new_source: Source) self, (), source *source = new_source
		=> Complex, Function, Integer, Macro, Null, Pair, Internal, Rational, String, Symbol
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use parse2::parse_file;
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

