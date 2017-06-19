use std::rc::Rc;
use std::fmt;
use data_structures::{Coredata, ParseState, Source, Sourcedata};
use super::VEC_CAPACITY;

// Implementor's checklist:
//
// ✓ Core expansion, parameterizations, and preparation
// ✓ Builtin Function calls
// ✓ Builtin Macro calls
// ✓ Tail call optimization
// ✓ If branching
// ✓ Integer parsing
// Rational parsing + promotion
// Complex parsing + promotion
// <, >, =, <=, >=, != number comparison
// Boolean not, and, or
// ✓ head/tail/pair
// ✓ wind/unwind
// ✓ ' quote
//   ` quasiquote
// ✓ " strings
// ✓ Add the error creation function
// ✓ Make Source data optional
// ✓ Macroize the initial environment (to clean up code)
//   Test different TCO strategies (HashSet, sorted Vec,..)
//   Implement powers for numbers
//   Replace all panics with unwinds
// ✓ Replace panics with unwinds in 'fn eval'
//   Formalize error messages and feedback (similar to rust errors, they are nice)
// ✓ Change transfer functions, do we need top?
//   Formalize errors (need line, column, file, standard format)
//   Implement a proper fmt::Display for Sourcedata
// ✓ Use booleans for If
// ✓ Easily add constants (pi, e, true, false)

impl fmt::Display for Sourcedata {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use data_structures::Coredata::*;
		use data_structures::Commands::*;
		use data_structures::Boolean;
		match self.1 {
			Boolean(Boolean::True) => write![f, "true"],
			Boolean(Boolean::False) => write![f, "false"],
			Complex(ref arg) => write![f, "{}", arg],
			Error(ref arg) => write![f, "(error {})", arg],
			Function(..) => write![f, "{}", line!()],
			Integer(ref arg) => write![f, "{}", arg],
			Internal(ref arg) => {
				write![f, "{}-", line!()]?;
				match *arg {
					Call(..) => write![f, "{}", line!()],
					Prepare(..) => write![f, "{}", line!()],
					Parameterize => write![f, "{}", line!()],
					Deparameterize(..) => write![f, "{}", line!()],
					If(..) => write![f, "{}", line!()],
					Wind => write![f, "{}", line!()],
					Evaluate => write![f, "{}", line!()],
					Empty => write![f, "{}", line!()],
				}
			}
			Macro(..) => write![f, "(mo {})", line!()],
			Null => write![f, "()"],
			Pair(ref arg, ref arg2) => write![f, "({} {})", arg, arg2],
			Rational(ref arg) => write![f, "{}", arg],
			String(ref arg) => write![f, "(\" {})", arg],
			Symbol(ref arg) => write![f, "{}", arg],
		}
	}
}

impl Default for Source {
	fn default() -> Source {
		Source {
			line: 1,
			column: 1,
			source: "unknown".into(),
		}
	}
}

impl fmt::Display for Source {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write![f, "{}:{}:{}", self.line, self.column, self.source]
	}
}

impl Sourcedata {
	pub fn head(&self) -> Rc<Sourcedata> {
		if let &Sourcedata(_, Coredata::Pair(ref head, _)) = self {
			head.clone()
		} else {
			Rc::new(Sourcedata(None, Coredata::Null))
		}
	}
	pub fn tail(&self) -> Rc<Sourcedata> {
		if let &Sourcedata(_, Coredata::Pair(_, ref tail)) = self {
			tail.clone()
		} else {
			Rc::new(Sourcedata(None, Coredata::Null))
		}
	}
}

impl Default for ParseState {
	fn default() -> ParseState {
		ParseState {
			current_read_position: Source::default(),
			start_of_current_lexeme: Source::default(),
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
			line: 1,
			column: 1,
			source: filename.into(),
		};
		state
	}
}