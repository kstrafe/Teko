//! Data structures used by the Teko library

use std::rc::Rc;
use std::collections::HashMap;
use num::bigint::BigInt;
use num::rational::BigRational;
use num::Complex;

/// Evaluation commands used internally by the interpreter
///
/// When put on the stack these values have different effects on the interpreter.
pub enum Commands {
	Call(Statement),
	Prepare(Statement),
	Parameterize,
	Deparameterize(Vec<String>),
	If(Statement, Statement),
	Wind,
	Evaluate,
	Empty,
}

/// Top level data structure used by the parser and interpreter
pub struct Sourcedata(pub Option<Source>, pub Coredata);
/// Top level statements are reference counted `Sourcedata`
pub type Statement = Rc<Sourcedata>;
/// A program is an ordered sequence of `Statement`
pub type Program = Vec<Statement>;

/// Denotes a "transfer function" that transform the state of the program
///
pub type Transfer = fn(program: &mut Program, env: &mut Env) -> Option<String>;
/// Boolean values
pub enum Boolean {
	True,
	False,
}
/// Function types that can be called by the interpreter
pub enum Function {
	/// A function written in the implementation language
	Builtin(Transfer),
	/// Parameter names with a sequence of statements that are inserted into the program when called
	Library(Vec<String>, Program),
}

/// Macro types that can be called by the interpreter
pub enum Macro {
	/// A function written in the implementation language
	Builtin(Transfer),
	/// Parameter name with a sequence of statements that are inserted into the program when called
	Library(String, Program),
}

pub enum Userdata {
}

/// Core data types of the Teko machine
pub enum Coredata {
	/// Denote true and false
	Boolean(Boolean),
	/// Complex numbers
	Complex(Complex<BigRational>),
	/// Error type
	Error(Statement),
	/// Function type
	Function(Function),
	/// Integer numbers
	Integer(BigInt),
	/// Internal commands (used by the implementation)
	Internal(Commands),
	/// Macro types
	Macro(Macro),
	/// Null (an empty list)
	Null,
	/// A pair of data items
	Pair(Rc<Sourcedata>, Rc<Sourcedata>),
	/// Rational numbers
	Rational(BigRational),
	/// String type
	String(String),
	/// Symbol type
	Symbol(String),
	/// Types defined by the user, for use by other
	/// builtins
	User(Userdata),
}

/// Environment used by the implementation
pub struct Env {
	/// Maps variables to stacks of variables (Program)
	pub store: HashMap<String, Program>,
	/// Parameter stack used for function calls
	pub params: Vec<Program>,
	/// Register used to store results of previous computations
	pub result: Statement,
}

/// State used by the parser internally
#[derive(Clone)]
pub struct ParseState {
	/// Most recent position in the stream being read
	pub current_read_position: Source,
	/// Last position where the beginning of a lexeme was initiated
	pub start_of_current_lexeme: Source,
	/// Stack of yet unmatched opening parentheses
	pub unmatched_opening_parentheses: Vec<Source>,
	/// The current lexeme being built into a token
	pub token: String,
	/// The output program
	pub stack: Program,
	/// Error container, set to Some if the parser fails
	pub error: Option<String>,
}

/// Information about the source of data.
#[derive(Clone, Debug)]
pub struct Source {
	/// Line number of the input, starts at 1
	pub line: usize,
	/// Column number of the input, starts at 1
	pub column: usize,
	/// Free-form string describing the source
	pub source: String,
}
