use std::collections::HashMap;
use std::rc::Rc;
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

impl Data {
	fn print(&self) {
		self.print_internal(0, false);
	}

	fn print_internal(&self, indent: usize, require_space: bool) -> bool {
		let this_require_space = match self {
			&Data::Complex  (_, ref complex) => { if require_space { print![" "]; } print!["{}", complex]; true },
			&Data::Function { source: _, parameters: ref params, code: ref code } => {
				if require_space {
					print![" "];
				}
				print!["(fn ("];
				for param in params {
					print!["{} ", param];
				}
				print![") "];
				code.print();
				print![") "];
				true
			},
			&Data::Integer  (_, ref integer) => { if require_space { print![" "]; } print!["{}", integer]; true },
			&Data::Macro    { source: _, parameter: ref param, code: ref code } => {
				if require_space {
					print![" "];
				}
				print!["(mo {} ", param];
				code.print();
				print![")"];
				true
			},
			&Data::Null     => { false },
			&Data::Pair     (_, ref first, ref rest) => {
				let (null1, null2) = {
					(if let Data::Null = **first { true } else { false }, if let Data::Null = **rest { true } else { false })
				};
				if null1 {
					if require_space {
						print![" "];
					}
					print!["("];
					rest.print_internal(0, false)
				} else if null2 {
					first.print_internal(0, require_space);
					print![")"];
					true
				} else {
					let propagate = first.print_internal(0, require_space);
					rest.print_internal(0, propagate)
				}
			},
			&Data::Rational (_, ref rational) => { if require_space { print![" "]; } print!["{}", rational]; true },
			&Data::String   (_, ref string) => { if require_space { print![" "]; } print!["{}", string]; true },
		};
		this_require_space
	}
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
	#[test]
	fn test() {
		let x = "(+ 5 42)";
		let p = parse_string("(+ 1 (* 9 4) 2 3) (kek def)").ok().unwrap();
		println!["{:#?}", p];
		let p = p.iter().map(|x| x.print()).count();
		//println!["{:#?}", p.first()];
		//println!["{:#?}", p.rest()];
		//println!["{:#?}", p.first()];
	}
}

