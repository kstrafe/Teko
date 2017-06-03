use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;
use super::VEC_CAPACITY;
use parse2::parse_string;

use num::bigint::BigInt;
use num::rational::{Ratio, BigRational};
use num::Complex;

// Observation: The program stack itself can only ever contain lists of strings, nothing more
enum Program {
	Expr(Vec<Program>),
	Atom(String),
}

// But, we also want to be able to manipulate program code in macros
// It would be easiest to encode the program as Data, using Pair. Then,
// if macros are to run, it'll be easy to just dump the Pair on the
// Parameter stack. Easy. `Code is Data'
// But,... we still need code annotations of source...
// So one example is to add a struct to each entry:

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
	Rational (Source, BigRational),
	String   (Source, String),
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
						&Data::Pair     (_, ref first, ref rest) => {
							if require_space {
								write![f, " "];
							}
							if top_level {
								write![f, "("];
								to_print.push(ToDisplay::ClosingParenthesis);
							}
							if let Data::Null = **rest {
							} else {
								to_print.push(ToDisplay::ToPrint(rest));
							}
							if let Data::Pair(..) = **first {
								write![f, "("];
								to_print.push(ToDisplay::ClosingParenthesis);
							}
							to_print.push(ToDisplay::ToPrint(first));
							require_space = false;
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

impl Data {
	fn first(&self) -> Rc<Data> {
		if let &Data::Pair(_, ref first, _) = self {
			first.clone()
		} else {
			Rc::new(Data::Null)
		}
	}
	fn rest(&self) -> Rc<Data> {
		if let &Data::Pair(_, _, ref rest) = self {
			rest.clone()
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
		let p = parse_string("((()))").ok().unwrap();
		println!["Returned: {:#?}", p];
		p.iter().map(|x| println!["{}", x]).count();
		//println!["{:#?}", p.first()];
		//println!["{:#?}", p.rest()];
		//println!["{:#?}", p.first()];
	}
}

