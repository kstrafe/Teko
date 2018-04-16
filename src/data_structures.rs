//! Data structures used by the Teko library

use std::collections::HashMap;
use std::rc::Rc;

use num::BigInt;

use user::Userdata;

use std::collections::HashSet;
use std::iter::Iterator;
use std::convert::Into;

/// A symbol is a string of characters that contains no whitespace nor parentheses
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct Symbol {
	value: String,
}

impl<'a> Into<&'a str> for &'a Symbol {
	fn into(self) -> &'a str {
		self.value.as_ref()
	}
}

// TODO change to TryFrom once stable
impl<'a> From<&'a str> for Symbol {
	fn from(string: &'a str) -> Symbol {
		// TODO check if the string is a valid symbol
		Symbol {
			value: string.to_string()
		}
	}
}

// TODO change to TryFrom once stable
impl<'a> From<&'a String> for Symbol {
	fn from(string: &'a String) -> Symbol {
		// TODO check if the string is a valid symbol
		Symbol {
			value: string.clone()
		}
	}
}

// TODO change to TryFrom once stable
impl From<String> for Symbol {
	fn from(string: String) -> Symbol {
		// TODO check if the string is a valid symbol
		Symbol {
			value: string
		}
	}
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Deparize {
	set: HashSet<Symbol>,
}

use std::collections;

impl Deparize {
	/// Check if the Symbol already exists in this Deparize and then insert it
	pub fn check_preexistence_and_merge_single(&mut self, symbol: &Symbol) -> bool {
		!self.set.insert(symbol.clone())
	}
	// TODO put into trait IntoIter
	pub fn into_iter<'a>(&'a self) -> collections::hash_set::Iter<Symbol> {
		self.set.iter()
	}
}

#[cfg(test)]
mod tests {
	#[test]
	fn test_deparize() {
		use super::*;
		let mut dep = Deparize::default();
		assert![!dep.check_preexistence_and_merge_single(&Symbol::from("nice"))];
		assert![dep.check_preexistence_and_merge_single(&Symbol::from("nice"))];
	}
}

/// Evaluation commands used internally by the interpreter
///
/// When put on the stack these values have different effects on the interpreter.
#[derive(Debug, PartialEq)]
pub enum Commands {
	Call(Statement),
	Prep(Statement),
	Param,
	Depar(Deparize),
	If(Statement, Statement),
	Wind,
	Eval,
}

/// Top level data structure used by the parser and interpreter
#[derive(Debug)]
pub struct Sourcedata(pub Option<Source>, pub Coredata);
/// Top level statements are reference counted `Sourcedata`
pub type Statement = Rc<Sourcedata>;
/// A program is an ordered sequence of `Statement`
pub type Program = Vec<Statement>;

/// Denotes a "transfer function" that transform the state of the program
///
pub type Transfer = fn(program: &mut Program, env: &mut Env) -> Option<(Option<Source>, String)>;
/// Boolean values

/// Function types that can be called by the interpreter
pub enum Function {
	/// A function written in the implementation language
	Builtin(Transfer, String),
	/// Parameter names with a sequence of statements that are inserted into the program when called
	Library(Vec<Symbol>, Program),
}

/// Macro types that can be called by the interpreter
pub enum Macro {
	/// A function written in the implementation language
	Builtin(Transfer, String),
	/// Parameter name with a sequence of statements that are inserted into the program when called
	Library(Symbol, Program),
}

/// Core data types of the Teko machine
#[derive(Debug)]
pub enum Coredata {
	// TODO Add complex number and representation symbol evaluation
	// TODO Add quoted form for writing out whatever in plain
	/// Denote true and false
	Boolean(bool),
	/// A pair of data items
	Cell(Rc<Sourcedata>, Rc<Sourcedata>),
	/// Error type
	Error(Statement),
	/// Function type
	Function(Function),
	/// Integer numbers
	Integer(BigInt),
	/// Internal commands (used by the implementation)
	Internal(Commands), // TODO remove this. It's not actually data
	/// Macro types
	Macro(Macro), 
	/// Null (an empty list)
	Null(),
	/// String type
	String(String),
	/// Symbol type. Can not contain any whitespace. Is a valid Teko atom.
	Symbol(Symbol),
}

/// Environment used by the implementation
pub struct Env {
	/// Maps variables to stacks of variables (Program)
	pub store: HashMap<Symbol, Program>,
	/// Parameter stack used for function calls
	pub params: Vec<Program>,
	/// Register used to store results of previous computations
	pub result: Statement,
}

impl Env {
	pub fn set_result(&mut self, value: Statement) {
		self.result = value;
	}
	pub fn get_result(&self) -> Statement {
		self.result.clone()
	}
	pub fn paramize(&mut self) {
		self.params.last_mut().unwrap().push(self.result.clone());
	}
	pub fn does_variable_exist(&self, symbol: &Symbol) -> bool {
		self.store.contains_key(symbol)
	}
	pub fn get(&self, symbol: &Symbol) -> Option<&Rc<Sourcedata>> {
		if let Some(value) = self.store.get(symbol) {
			value.last()
		} else {
			None
		}
	}
	pub fn push(&mut self, symbol: &Symbol, value: Rc<Sourcedata>) {
		if self.does_variable_exist(symbol) {
			self.store.get_mut(symbol).unwrap().push(value);
		} else {
			self.store.insert(symbol.clone(), vec![value]);
		}
	}
	pub fn pop(&mut self, symbol: &Symbol) -> Option<Rc<Sourcedata>> {
		if let Some(ref mut entry) = self.store.get_mut(symbol) {
			entry.pop()
		} else {
			None
		}
	}
}

/// State used by the parser internally
#[derive(Clone, Debug)]
pub struct ParseState {
	/// Most recent position in the stream being read
	pub current_read_position: Source,
	/// Last position where the beginning of a lexeme was initiated
	pub start_of_current_lexeme: Source,
	/// Stack of yet unmatched opening parentheses
	pub unmatched_opening_parentheses: Vec<Source>,
	/// The current lexeme being built into a token
	pub token: String,
	/// The stack of lists being built.
	pub stack: Vec<Program>,
	/// Error container, set to Some if the parser fails
	pub error: Option<String>,
}

/// Information about the source of data.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Source {
	/// Line number of the input, starts at 1
	pub line: usize,
	/// Column number of the input, starts at 1
	pub column: usize,
	/// Free-form string describing the source ("tty" from terminal, filename from file,..)
	pub source: String, // Change to Rc<String>?
}
