//! Utilities used by the implementation.

use std::{cmp, convert, fmt, io};
use std::error::Error;
use std::rc::Rc;
use std::usize;

use data_structures::{Commands, Coredata, Env, Function, Macro, ParseState, Program, Source,
                      Sourcedata};
use super::VEC_CAPACITY;
use user::user_data_name;

// //////////////////////////////////////////////////////////
// Impls
// //////////////////////////////////////////////////////////

impl cmp::PartialEq for Coredata {
	fn eq(&self, other: &Self) -> bool {
		use data_structures::Boolean;
		if self as *const Coredata == other as *const Coredata {
			return true;
		}
		match *self {
			Coredata::Boolean(Boolean::True) => {
				if let Coredata::Boolean(Boolean::True) = *other {
					true
				} else {
					false
				}
			}
			Coredata::Boolean(Boolean::False) => {
				if let Coredata::Boolean(Boolean::False) = *other {
					true
				} else {
					false
				}
			}
			Coredata::Error(ref lhs) => {
				if let Coredata::Error(ref rhs) = *other {
					lhs == rhs
				} else {
					false
				}
			}
			Coredata::Function(Function::Builtin(_, ref lhs)) => {
				if let Coredata::Function(Function::Builtin(_, ref rhs)) = *other {
					lhs == rhs
				} else {
					false
				}
			}
			Coredata::Function(Function::Library(ref lhsparams, ref lhscode)) => {
				if let Coredata::Function(Function::Library(ref rhsparams, ref rhscode)) = *other {
					lhsparams == rhsparams && lhscode == rhscode
				} else {
					false
				}
			}
			Coredata::Integer(ref lhs) => {
				if let Coredata::Integer(ref rhs) = *other {
					lhs == rhs
				} else {
					false
				}
			}
			Coredata::Macro(Macro::Builtin(_, ref lhs)) => {
				if let Coredata::Macro(Macro::Builtin(_, ref rhs)) = *other {
					lhs == rhs
				} else {
					false
				}
			}
			Coredata::Macro(Macro::Library(ref lhsparam, ref lhscode)) => {
				if let Coredata::Macro(Macro::Library(ref rhsparam, ref rhscode)) = *other {
					lhsparam == rhsparam && lhscode == rhscode
				} else {
					false
				}
			}
			Coredata::Internal(ref lhs) => {
				if let Coredata::Internal(ref rhs) = *other {
					lhs == rhs
				} else {
					false
				}
			}
			Coredata::Null => {
				if let Coredata::Null = *other {
					true
				} else {
					false
				}
			}
			Coredata::Cell(ref lhshead, ref lhstail) => {
				if let Coredata::Cell(ref rhshead, ref rhstail) = *other {
					lhshead == rhshead && lhstail == rhstail
				} else {
					false
				}
			}
			Coredata::String(ref lhs) => {
				if let Coredata::String(ref rhs) = *other {
					lhs == rhs
				} else {
					false
				}
			}
			Coredata::Symbol(ref lhs) => {
				if let Coredata::Symbol(ref rhs) = *other {
					lhs == rhs
				} else {
					false
				}
			}
			Coredata::User(ref lhs) => {
				if let Coredata::User(ref rhs) = *other {
					lhs == rhs
				} else {
					false
				}
			}
		}
	}
}

impl cmp::PartialEq for Sourcedata {
	fn eq(&self, other: &Self) -> bool {
		self.1 == other.1
	}
}

impl fmt::Debug for Function {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Function::Builtin(.., ref name) => {
				write![f, "{}", name]?;
			}
			Function::Library(ref params, ref code) => {
				write![f, "(fn ("]?;
				let mut first = true;
				for i in params.iter() {
					if first {
						write![f, "{}", i]?;
					} else {
						write![f, " {}", i]?;
					}
					first = false;
				}
				write![f, ")"]?;
				for i in code.iter().rev() {
					write![f, " {}", i]?;
				}
				write![f, ")"]?;
			}
		}
		Ok(())
	}
}

impl fmt::Debug for Macro {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Macro::Builtin(.., ref name) => {
				write![f, "{}", name]?;
			}
			Macro::Library(ref param, ref code) => {
				write![f, "(mo {}", param]?;
				for i in code.iter().rev() {
					write![f, " {}", i]?;
				}
				write![f, ")"]?;
			}
		}
		Ok(())
	}
}

/// Display for Sourcedata.
///
/// All Sourcedata can be written in a form such that it can be read again.
impl fmt::Display for Sourcedata {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use data_structures::{Boolean, Function, Macro};
		use data_structures::Commands::*;
		use data_structures::Coredata::*;
		let mut first = true;
		let null = &Sourcedata(None, Coredata::Null);
		let mut queue = Vec::with_capacity(VEC_CAPACITY);
		let mut spacer = false;
		queue.push(self);
		while let Some(elem) = queue.pop() {
			match elem.1 {
				Boolean(Boolean::True) => {
					if spacer {
						write![f, " "]?;
					}
					write![f, "true"]?;
					spacer = true;
				}
				Boolean(Boolean::False) => {
					if spacer {
						write![f, " "]?;
					}
					write![f, "false"]?;
					spacer = true;
				}
				Error(ref arg) => {
					if spacer {
						write![f, " "]?;
					}
					write![f, "(error"]?;
					if let Coredata::Cell(..) = arg.1 {
						write![f, " ("]?;
						queue.push(arg);
						spacer = false;
					} else if let Coredata::Null = arg.1 {
						write![f, ")"]?;
						spacer = true;
					} else {
						queue.push(null);
						queue.push(arg);
						spacer = true;
					}
				}
				Function(Function::Builtin(.., ref name)) => {
					if spacer {
						write![f, " "]?;
					}
					write![f, "{}", name]?;
					spacer = true;
				}
				Function(Function::Library(ref params, ref code)) => {
					if spacer {
						write![f, " "]?;
					}
					write![f, "(fn ("]?;
					let mut first = true;
					for i in params.iter() {
						if first {
							write![f, "{}", i]?;
						} else {
							write![f, " {}", i]?;
						}
						first = false;
					}
					write![f, ")"]?;
					for i in code.iter().rev() {
						write![f, " {}", i]?;
					}
					write![f, ")"]?;
					spacer = true;
				}
				Integer(ref arg) => {
					if spacer {
						write![f, " "]?;
					}
					write![f, "{}", arg]?;
					spacer = true;
				}
				Internal(ref arg) => {
					if spacer {
						write![f, " "]?;
					}
					match *arg {
						Call(ref callee) => write![f, "(@call {})", callee]?,
						Prepare(ref callee) => write![f, "(@prepare {})", callee]?,
						Parameterize => write![f, "(@parameterize)"]?,
						Deparameterize(ref params) => {
							write![f, "(@deparameterize"]?;
							for i in params.iter().rev() {
								write![f, " {}", i]?;
							}
							write![f, ")"]?;
						}
						If(ref former, ref latter) => write![f, "(@if {} {})", former, latter]?,
						Wind => write![f, "(@wind)"]?,
						Evaluate => write![f, "(@evaluate)"]?,
						Empty => write![f, "(@empty)"]?,
					}
					spacer = true;
				}
				Macro(Macro::Builtin(.., ref name)) => {
					if spacer {
						write![f, " "]?;
					}
					write![f, "{}", name]?;
					spacer = true;
				}
				Macro(Macro::Library(ref param, ref code)) => {
					if spacer {
						write![f, " "]?;
					}
					write![f, "(mo {}", param]?;
					for i in code.iter().rev() {
						write![f, " {}", i]?;
					}
					write![f, ")"]?;
					spacer = true;
				}
				Null => {
					if first {
						write![f, "()"]?;
					} else {
						write![f, ")"]?;
					}
				}
				Cell(ref head, ref tail) => {
					if spacer {
						write![f, " "]?;
					}
					if first {
						write![f, "("]?;
					}
					queue.push(tail);
					if let Coredata::Cell(..) = head.1 {
						write![f, "("]?;
						queue.push(head);
						spacer = false;
					} else if let Coredata::Null = head.1 {
						write![f, "()"]?;
						spacer = true;
					} else {
						queue.push(head);
						spacer = false;
					}
				}
				String(ref arg) => {
					if spacer {
						write![f, " "]?;
					}
					macro_rules! is_plainly_printable {
						($i:ident) => {
							// TODO remove () around cast: rustc panics because it thinks it's a generic
							!$i.is_whitespace() && $i != '(' && $i != ')' && $i as u32 > 0x1F &&
							(($i as u32) < 0x7F || $i as u32 > 0x9F)
						};
					}
					let mut l3: char = '_';
					let mut l2: char;
					let mut l1: char = ' ';
					let mut rle = 1; // rle = runtime-length-encoding
					write![f, "(\""]?;
					for ch in arg.chars().chain("_".chars()) {
						l2 = l1;
						l1 = ch;
						// Is l2 surrounded by printable characters?
						if !is_plainly_printable!(l2) {
							if is_plainly_printable!(l1) && is_plainly_printable!(l3) {
								match l2 {
									' ' => write![f, " "]?,
									_ => write![f, "({})", l2 as u32]?,
								}
							} else if l1 == l2 {
								rle += 1;
							} else {
								if rle == 1 {
									write![f, "({})", l2 as u32]?;
								} else {
									write![f, "({} {})", l2 as u32, rle]?;
								}
								rle = 1;
							}
						} else {
							// l2 is printable, so all is well
							write![f, "{}", l2]?;
						}
						l3 = l2;
					}
					write![f, ")"]?;
					spacer = true;
				}
				Symbol(ref arg) => {
					if spacer {
						write![f, " "]?;
					}
					write![f, "{}", arg]?;
					spacer = true;
				}
				User(ref user) => {
					if spacer {
						write![f, " "]?;
					}
					write![f, "{}", user]?;
					spacer = true;
				}
			}
			first = false;
		}
		Ok(())
	}
}

impl Sourcedata {
	/// Return the head of a cell, unwind if not a cell.
	pub fn head(&self) -> Option<Rc<Sourcedata>> {
		if let Sourcedata(_, Coredata::Cell(ref head, _)) = *self {
			Some(head.clone())
		} else {
			None
		}
	}
	/// Return the tail of a cell, unwind if not a cell.
	pub fn tail(&self) -> Option<Rc<Sourcedata>> {
		if let Sourcedata(_, Coredata::Cell(_, ref tail)) = *self {
			Some(tail.clone())
		} else {
			None
		}
	}
	/// Compute the size of the object in element count.
	pub fn len(&self) -> Option<usize> {
		if let Coredata::String(ref string) = self.1 {
			return Some(string.len());
		}
		let mut current = self;
		let mut length = 0;
		loop {
			match current.1 {
				Coredata::Cell(_, ref tail) => {
					length += 1;
					current = &*tail;
				}
				Coredata::Null => {
					return Some(length);
				}
				_ => {
					return None;
				}
			}
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

impl<'a> convert::From<&'a Source> for Rc<Sourcedata> {
	fn from(src: &'a Source) -> Rc<Sourcedata> {
		use data_structures::Coredata::*;
		rcs(Cell(
			rcs(Integer(src.line.into())),
			rcs(Cell(
				rcs(Integer(src.column.into())),
				rcs(Cell(rcs(String(src.source.clone())), rcs(Null))),
			)),
		))
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
	pub fn from(source: &str) -> ParseState {
		let mut state = ParseState::default();
		state.current_read_position = Source {
			line: 1,
			column: 1,
			source: source.into(),
		};
		state
	}
}

impl convert::From<io::Error> for ParseState {
	fn from(err: io::Error) -> Self {
		let mut state = ParseState::default();
		state.error = Some(err.description().into());
		state
	}
}

// //////////////////////////////////////////////////////////
// Utility functions
// //////////////////////////////////////////////////////////

pub fn arity_mismatch(expected_min: usize, expected_max: usize, got: usize) -> String {
	if expected_min == expected_max {
		format!["arity mismatch: expected {} but got {}", expected_min, got]
	} else if expected_min < expected_max && expected_min == 0 {
		format![
			"arity mismatch: expected <={} but got {}",
			expected_max,
			got,
		]
	} else if expected_min < expected_max && expected_max == usize::MAX {
		format![
			"arity mismatch: expected >={} but got {}",
			expected_min,
			got,
		]
	} else {
		format![
			"arity mismatch: expected >={} and <={} but got {}",
			expected_min,
			expected_max,
			got,
		]
	}
}

pub fn not_found(string: &str) -> String {
	format!["variable not found: {}", string]
}

/// Maps a linked list of data into a vector of data.
pub fn collect_cell_into_vec(data: &Rc<Sourcedata>) -> Vec<Rc<Sourcedata>> {
	let mut to_return = vec![];
	let mut current = data.clone();
	loop {
		current = if let Sourcedata(_, Coredata::Cell(ref head, ref tail)) = *current {
			to_return.push(head.clone());
			tail.clone()
		} else {
			break;
		}
	}
	to_return.reverse();
	to_return
}

/// Maps a linked list of symbols into a vector of strings.
pub fn collect_cell_of_symbols_into_vec_string(data: &Rc<Sourcedata>) -> Option<Vec<String>> {
	let data = collect_cell_into_vec(data);
	let mut ret = vec![];
	for i in data {
		match *i {
			Sourcedata(_, Coredata::Symbol(ref string)) => {
				ret.push(string.clone());
			}
			_ => {
				return None;
			}
		}
	}
	ret.reverse();
	Some(ret)
}

/// Takes the intersection of two sets.
pub fn compute_intersection<'a>(a: &'a [String], b: &'a [String]) -> Vec<&'a String> {
	let mut intersection: Vec<&'a String> = Vec::with_capacity(VEC_CAPACITY);
	for i in a {
		if b.contains(i) {
			intersection.push(i);
		}
	}
	intersection
}

/// Takes the union of two sets.
pub fn compute_union(a: &[String], b: &[String]) -> Vec<String> {
	let mut c = a.to_vec();
	for i in b {
		if !a.contains(i) {
			c.push(i.clone());
		}
	}
	c
}

/// Get the name associated with the data type.
pub fn data_name(data: &Sourcedata) -> String {
	use data_structures::Coredata::*;
	match data.1 {
		Boolean(..) => "Boolean",
		Cell(..) => "Cell",
		Error(..) => "Error",
		Function(..) => "Function",
		Integer(..) => "Integer",
		Internal(..) => "Internal",
		Macro(..) => "Macro",
		Null => "Null",
		String(..) => "String",
		Symbol(..) => "Symbol",
		User(ref user) => user_data_name(user),
	}.into()
}

/// Unwind and trace with an error message if it is Some.
///
/// Mixes unwind and tracing from an error's invocation. Any time an unwind
/// happens `env.result` will contain an error with a string containing the stack
/// trace an addition to the error provided.
pub fn err(source: &Option<Source>, error: &Option<String>, program: &mut Program, env: &mut Env) {
	let error = if let Some(ref error) = *error {
		program.push(rc(
			Sourcedata(source.clone(), Coredata::String(error.clone())),
		));
		let trace = internal_trace(program, env);
		Some(trace)
	} else {
		None
	};
	if let Some(error) = error {
		env.params.push(vec![rcs(Coredata::Error(error))]);
		unwind(program, env);
		if env.params.pop().is_none() {
			panic!["Stack corruption"];
		}
	}
}

/// Create a string of the entire program stack.
pub fn internal_trace(program: &mut Program, _: &mut Env) -> Rc<Sourcedata> {
	use data_structures::Coredata::*;
	let null = rcs(Coredata::Null);
	let mut lst = null.clone();
	for i in program.iter().rev() {
		if let Sourcedata(Some(ref source), ..) = **i {
			lst = rcs(Cell(
				rcs(Cell(source.into(), rcs(Cell(i.clone(), rcs(Null))))),
				lst.clone(),
			));
		} else {
			lst = rcs(Cell(
				rcs(Cell(rcs(Null), rcs(Cell(i.clone(), rcs(Null))))),
				lst.clone(),
			));
		}
	}
	lst
}

/// Optimizes tail calls by seeing if the current `params` can be merged with the top of the stack.
///
/// If the top of the stack contains `Commands::Deparameterize`, then the variables to be popped
/// are merged into that [top] object. This is all that's needed to optimize tail calls.
pub fn optimize_tail_call(program: &mut Program, env: &mut Env, params: &[String]) -> Vec<String> {
	if let Some(top) = program.pop() {
		match top.1 {
			Coredata::Internal(Commands::Deparameterize(ref content)) => {
				for i in compute_intersection(content, params) {
					if let Some(ref mut entry) = env.store.get_mut(i) {
						if entry.pop().is_some() {
							// OK
						} else {
							panic!["Store inconsistency; entry empty"];
						}
					} else {
						panic!["Store inconsistency; entry nonexistent"];
					}
				}
				compute_union(content, params)
			}
			_ => {
				program.push(top.clone()); // Put top back on the program stack
				params.to_vec()
			}
		}
	} else {
		params.to_vec()
	}
}

pub fn optional_source(source: &Option<Source>) -> String {
	if let Some(ref source) = *source {
		format!["{}", source]
	} else {
		String::from("_")
	}
}

// TODO change from panic to unwind, but can we be safe about such a serious error by
// unwinding? Maybe a stop function that freezes the interpreter...
/// Pops the specified parameters from the stack.
///
/// If the parameters do not exist then there's an internal programmer error and
/// this function will panic.
pub fn pop_parameters(_: &mut Program, env: &mut Env, args: &[String]) {
	for arg in args {
		if let Some(ref mut entry) = env.store.get_mut(arg) {
			if entry.pop().is_some() {
				// OK
			} else {
				panic!["Store entry was already empty"];
			}
		} else {
			panic!["Store entry does not exist"];
		}
		let is_empty = if let Some(entry) = env.store.get(arg) {
			entry.is_empty()
		} else {
			panic!["Store entry does not exist"];
		};
		if is_empty {
			env.store.remove(arg);
		}
	}
}

/// Alias for `Rc::new(_)`.
pub fn rc<T>(rc: T) -> Rc<T> {
	Rc::new(rc)
}

/// Alias for `Rc::new(Sourcedata(None, _))`.
pub fn rcs(rcs: Coredata) -> Rc<Sourcedata> {
	rc(Sourcedata(None, rcs))
}

/// Unwinds the stack until first wind is encountered.
///
/// Preserves stack consistency (pops parameters when necessary).
pub fn unwind(program: &mut Program, env: &mut Env) -> Option<String> {
	if let Some(param) = env.params.last() {
		if let Some(last) = param.last() {
			env.result = last.clone();
		}
	}
	while let Some(top) = program.pop() {
		match top.1 {
			Coredata::Internal(Commands::Deparameterize(ref arguments)) => {
				pop_parameters(program, env, arguments);
			}
			Coredata::Internal(Commands::Call(..)) => {
				env.params.pop();
			}
			Coredata::Internal(Commands::Wind) => {
				break;
			}
			_ => {}
		}
	}
	None
}
