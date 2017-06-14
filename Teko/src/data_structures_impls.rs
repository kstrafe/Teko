use std::rc::Rc;
use std::collections::HashMap;
use std::fmt;
use num::bigint::BigInt;
use num::rational::BigRational;
use num::Complex;
use data_structures::{Commands, Data, Env, ParseState, Source};
use super::VEC_CAPACITY;

impl Default for Source {
	fn default() -> Source {
		Source { line: 1, column: 1, source: "unknown".into() }
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

impl Default for ParseState {
	fn default() -> ParseState {
		ParseState {
			current_read_position:         Source::default(),
			start_of_current_lexeme:       Source::default(),
			unmatched_opening_parentheses: Vec::with_capacity(VEC_CAPACITY),
			token: String::from(""),
			stack: Vec::with_capacity(VEC_CAPACITY),
			error: None,
		}
	}
}

impl ParseState {
	pub fn from_file(filename: &str) -> ParseState {
		let mut state = ParseState::default();
		state.current_read_position = Source {
			line:   1,
			column: 1,
			source: filename.into(),
		};
		state
	}
}
