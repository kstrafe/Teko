use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;
use super::VEC_CAPACITY;
use parse2::parse_file;

use num::bigint::BigInt;
use num::rational::{Ratio, BigRational};
use num::Complex;

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

impl Source {
	fn unknown() -> Source {
		Source { line: 0, column: 0, source: "unknown".into() }
	}
}

#[derive(Clone, Debug)]
pub enum Data {
	Complex  (Source, Complex<BigRational>),
	Function { source: Source, parameters: Vec<String>, code: Rc<Data> },
	Integer  (Source, BigInt),
	Macro    { source: Source, parameter: String, code: Rc<Data> },
	Null,
	Pair     (Source, Rc<Data>, Rc<Data>),
	Placeholder (Source),
	Rational (Source, BigRational),
	String   (Source, String),
}

#[derive(Debug)]
struct Env {
	content:      HashMap<String, Vec<Rc<Data>>>,
	call_stack:   Vec<String>,
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
								write![f, " "];
							}
							write![f, "{}", complex];
							require_space = true;
						},
						&Data::Function { source: _, parameters: ref params, code: ref code } => {
							if require_space {
								write![f, " "];
							}
							write![f, "(fn ("];
							write![f, "{}", params.join(" ")];
							write![f, ")"];
							to_print.push(ToDisplay::ClosingParenthesis);
							to_print.push(ToDisplay::ToPrint(code));
							require_space = true;
						},
						&Data::Integer  (_, ref integer) => {
							if require_space {
								write![f, " "];
							}
							write![f, "{}", integer];
						},
						&Data::Macro    { source: _, parameter: ref param, code: ref code } => {
							if require_space {
								write![f, " "];
							}
							write![f, "(mo {}", param];
							to_print.push(ToDisplay::ClosingParenthesis);
							to_print.push(ToDisplay::ToPrint(code));
							require_space = true;
						},
						&Data::Null     => { write![f, "()"]; require_space = true; },
						&Data::Pair     (_, ref head, ref tail) => {
							if require_space {
								write![f, " "];
							}
							if top_level {
								write![f, "("];
								to_print.push(ToDisplay::ClosingParenthesis);
							}
							if let Data::Null = **tail {
							} else {
								to_print.push(ToDisplay::ToPrint(tail));
							}
							if let Data::Pair(..) = **head {
								write![f, "("];
								to_print.push(ToDisplay::ClosingParenthesis);
							}
							to_print.push(ToDisplay::ToPrint(head));
							require_space = false;
						},
						&Data::Placeholder(..) => {
							write![f, "|"];
						},
						&Data::Rational (_, ref rational) => {
							if require_space {
								write![f, " "];
							}
							write![f, "{}", rational];
							require_space = true;
						},
						&Data::String   (_, ref string) => {
							if require_space {
								write![f, " "];
							}
							write![f, "{}", string];
							require_space = true;
						},
					}
				},
				ToDisplay::ClosingParenthesis => {
					write![f, ")"];
					require_space = true;
				},
			}
			top_level = false;
		}
		write!(f, "")
	}
}

fn interpret(program: Vec<Rc<Data>>) {
	let env = Env {
		content:      [
		              ].iter().cloned().collect(),
		call_stack:   Vec::with_capacity(VEC_CAPACITY),
		params:       Vec::with_capacity(VEC_CAPACITY),
		return_value: Rc::new(Data::Null),
	};
	eval(program, env);
}

fn eval(mut program: Vec<Rc<Data>>, mut env: Env) {
	program.reverse();
	enum Commands {
		DefineReturnAs(String),
	}
	while let Some(top) = program.pop() {
		match &*top {
			&Data::Pair(_, ref head, ref tail) => {
			},
			&Data::String(_, ref string) => {
			},
			_ => {
				println!["top: {:#?}", top];
			},
		}
	}
}

impl Data {
	fn head(&self) -> Rc<Data> {
		if let &Data::Pair(_, ref head, _) = self {
			head.clone()
		} else {
			Rc::new(Data::Null)
		}
	}
	fn tail(&self) -> Rc<Data> {
		if let &Data::Pair(_, _, ref tail) = self {
			tail.clone()
		} else {
			Rc::new(Data::Null)
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	macro_rules! parse_formats_to {
		( $($x:expr),* ) => {
			$( assert_eq![parse_string($x).ok().unwrap(), $x]; ),*
		};
	}
	#[test]
	fn test() {
		let p = parse_file("input").ok().unwrap();
		println!["Returned: {:#?}", p];
		p.iter().map(|x| println!["{}", x]).count();
		//println!["{:#?}", p.head()];
		//println!["{:#?}", p.tail()];
		//println!["{:#?}", p.head()];
	}
}

