//! Contains utilities used by the implementation
use std::rc::Rc;
use std::fmt;
use data_structures::{Commands, Coredata, Env, ParseState, Program, Source, Sourcedata};
use super::VEC_CAPACITY;

/// Implement the writer of sourcedata.
///
/// All Sourcedata can be written in a form such that it can be read again.
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
			Pair(ref arg, ref arg2) => write![f, "({} {}", arg, arg2],
			Rational(ref arg) => write![f, "{}", arg],
			String(ref arg) => write![f, "(\" {})", arg],
			Symbol(ref arg) => write![f, "{}", arg],
			User(..) => write![f, "{}", "user-defined"],
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

// //////////////////////////////////////////////////////////
// Utility functions
// //////////////////////////////////////////////////////////

/// Maps a linked list of data into a vector of data.
pub fn collect_pair_into_vec(data: &Rc<Sourcedata>) -> Vec<Rc<Sourcedata>> {
	let mut to_return = vec![];
	let mut current = data.clone();
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

/// Maps a linked list of symbols into a vector of strings.
pub fn collect_pair_of_symbols_into_vec_string(data: &Rc<Sourcedata>) -> Vec<String> {
	let data = collect_pair_into_vec(data);
	let mut ret = vec![];
	for i in data.iter() {
		match &**i {
			&Sourcedata(_, Coredata::Symbol(ref string)) => {
				ret.push(string.clone());
			}
			_ => {
				panic!{"Not a symbol"};
			}
		}
	}
	ret.reverse();
	ret
}

/// Prepare the stack for unwinding, with the result being an error message.
///
/// Note that preparing the stack means to push an the unwind call onto the stack.
/// This function doesn't unwind directly.
pub fn unwind_with_error_message(string: &str, program: &mut Program, env: &mut Env) {
	let sub = Rc::new(Sourcedata(None, Coredata::String(string.into())));
	env.params
		.push(vec![Rc::new(Sourcedata(None, Coredata::Error(sub)))]);
	unwind(program, env);
	if let None = env.params.pop() {
		panic!["Stack corruption"];
	}
}

// TODO change from panic to unwind, but can we be safe about such a serious error by
// unwinding? Maybe a stop function that freezes the interpreter...
/// Pops the specified parameters from the stack.
///
/// If the parameters do not exist then there's an internal programmer error and
/// this function will panic.
pub fn pop_parameters(_: &mut Program, env: &mut Env, args: &Vec<String>) {
	for arg in args {
		if let Some(ref mut entry) = env.store.get_mut(arg) {
			if let Some(_) = entry.pop() {
				// OK
			} else {
				panic!["Store entry was already empty"];
			}
		} else {
			panic!["Store entry does not exist"];
		}
		let is_empty = if let Some(ref entry) = env.store.get(arg) {
			if entry.is_empty() {
				true
			} else {
				false
			}
		} else {
			panic!["Store entry does not exist"];
		};
		if is_empty {
			env.store.remove(arg);
		}
	}
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

/// Takes the union of two sets.
pub fn compute_union(a: &Vec<String>, b: &Vec<String>) -> Vec<String> {
	let mut c = a.clone();
	for i in a {
		if !b.contains(i) {
			c.push(i.clone());
		}
	}
	c
}

/// Takes the intersection of two sets.
pub fn compute_intersection<'a>(a: &'a Vec<String>, b: &'a Vec<String>) -> Vec<&'a String> {
	let mut intersection: Vec<&'a String> = Vec::with_capacity(VEC_CAPACITY);
	for i in a {
		if b.contains(i) {
			intersection.push(i);
		}
	}
	intersection
}

pub fn err(source: &Option<Source>, error: &Option<String>, program: &mut Program, env: &mut Env) {
	let unwind = if let &Some(ref error) = error {
		if let &Some(ref source) = source {
			// trace(&mut program, &mut env);
			Some(format!["{} <= {}", error, source])
		} else {
			Some(format!["{} <= _", error])
		}
	} else {
		None
	};
	if let Some(error) = unwind {
		unwind_with_error_message(&error[..], program, env);
	}
}

/// Optimizes tail calls by seeing if the current `params` can be merged with the top of the stack.
///
/// If the top of the stack contains `Commands::Deparameterize`, then the variables to be popped
/// are merged into that [top] object. This is all that's needed to optimize tail calls.
pub fn optimize_tail_call(program: &mut Program,
                          env: &mut Env,
                          params: &Vec<String>)
                          -> Vec<String> {
	if let Some(top) = program.pop() {
		match top.1 {
			Coredata::Internal(Commands::Deparameterize(ref content)) => {
				for i in compute_intersection(content, params) {
					if let Some(ref mut entry) = env.store.get_mut(i) {
						if let Some(_) = entry.pop() {
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
				params.clone()
			}
		}
	} else {
		params.clone()
	}
}
