use std::rc::Rc;
use std::collections::HashMap;
use num::bigint::BigInt;
use num::rational::BigRational;
use num::Complex;

/// Primitive forms
#[derive(Clone, Debug)]
pub enum Commands {
	Refine,

	/// Used on the execution stack to map the `String` to the return value.
	/// The specification defines it as `env.String := Return`.
	/// A variable can only be defined once, this means that `Define("x") Define("x")`
	/// would unwind with an error message.
	Define(String),

	/// Used on the execution stack to set `env.String = Return`. Since `env.String` is
	/// a list, only the top element is changed.
	Set(String),

	/// Uses `Return` to decide which of the `Rc<Data>` values to interpret. If `Return` is
	/// `Data::Null` then the second `Rc<Data>` will be run, otherwise the first one will be
	/// run.
	If(Rc<Data>, Rc<Data>),

	/// Builtin functions
	Plus, Minus, Multiply, Divide,

	/// Perform an actual function or macro call, binding the values in `Env::params` to the
	/// parameter list in the top of `Env::call_stack`, and then calling the function.
	Call,

	/// Consider the top of `Env::call_stack` and decide whether the arguments in `Rc<Data>`
	/// need evaluation or not. If the top of `Env::call_stack` is a function, then all arguments
	/// must be evaluated before the function is called. Else - if the top of `Env::call_stack`
	/// is a macro, then the macro is simply called without evaluating `Rc<Data>`.
	Prepare(Rc<Data>),

	/// Push Return `Env::return_value` onto the function call list `Env::call_stack`.
	Pushcall,                    // Push Return onto the environment's call stack

	/// Push Return `Env::return_value` onto the function parameter list `Env::params`.
	Parameterize,

	/// Unwind the stack, skipping all operations, until the first `Wind` is encountered.
	/// Execution continues from there.
	Unwind,

	/// Used to mark the execution stack with a restore-point to which
	/// an `Unwind` will fall to.
	Wind,

	/// Used in the parser to denote left parentheses.
	Empty,

}

#[derive(Clone, Debug)]
pub enum Data {
	Complex  (Source, Complex<BigRational>),
	Function (Source, Vec<String>, Rc<Data>),
	Integer  (Source, BigInt),
	Internal (Source, Commands),
	Macro    (Source, String, Rc<Data>),
	Null     (Source),
	Pair     (Source, Rc<Data>, Rc<Data>),
	Rational (Source, BigRational),
	String   (Source, String),
	Symbol   (Source, String),
}

#[derive(Clone, Debug)]
pub struct Env {
	pub content:      HashMap<String, Vec<Rc<Data>>>,
	pub call_stack:   Vec<Rc<Data>>,
	pub params:       Vec<Vec<Rc<Data>>>,
	pub return_value: Rc<Data>,
}

#[derive(Clone, Debug)]
pub struct ParseState {
	pub current_read_position:         Source,
	pub start_of_current_lexeme:       Source,
	pub unmatched_opening_parentheses: Vec<Source>,
	pub token: String,
	pub stack: Vec<Rc<Data>>,
	pub error: Option<String>,
}

#[derive(Clone, Debug)]
pub struct Source {
	pub line:   usize,
	pub column: usize,
	pub source: String,
}
