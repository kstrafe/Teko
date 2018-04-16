//! Parsing interface for Teko.
//!
//! Provides utility functions as well as primitives for parsing Teko.
//!
//! ```
//! extern crate teko;
//! assert![teko::parse::parse_string("(+ 1 2 3) (' a (b) c)").is_ok()];
//! ```
use std::fs::File;
use std::io::Read;
use std::rc::Rc;

use data_structures::*;


// //////////////////////////////////////////////////////////
// # Implementation Details #
//
// This file contains a simple LL(1) parser for Teko. Here are
// the implementation details in simplified form.
//
// ## The Parser ##
//
// The parser starts out with the state (token, stack), and they
// look like this ("", [[]]).
//
// The lists inside the stack are called substacks.
//
// Upon encountering any character, it appends to the token. Suppose
// the character "a" is encountered, then the state becomes:
// ("", [[]]) :a:-> ("a", [[]])
//
// If the character is '(', ')', or any whitespace, the token is added
// to the last substack:
// ("a", [[]]) : :-> ("", [["a"]])
//
// When a left-parenthesis is spotted, a new substack is appended to the stack:
// ("", [["a"]]) :(:-> ("", [["a"], []])
//
// Now, when tokens are to be appended, they are moved to the last substack:
// ("", [["a"], []]) :b:-> ("", [["a"], ["b"]])
// ("", [["a"], ["b"]]) :c:-> ("", [["a"], ["b", "c"]])
//
// Finally, a right parenthesis moves the last substack into the end of the second to last substack:
// ("", [["a"], ["b", "c"]]) :):-> ("", [["a", ["b", "c"]]])
//
// Additionally, the parser keeps track of the location of the parsed code in the
// source using line and column numbers.
//
// ## Error Handling ##
//
// Errors are handled by returning `Result<Program, ParseState>`. Having the `ParseState`
// represent an error allows us to inspect exactly what went wrong where. The `ParseState`
// struct contains the variable `error` of type `Option<String>`, which describes the
// error.
//
// //////////////////////////////////////////////////////////

/// Parse a `File` into a `Program`
///
/// Utility function to easily parse a `File`.
pub fn parse_file(filename: &str) -> Result<Program, ParseState> {
	let mut file = match File::open(filename) {
		Ok(f) => f,
		Err(e) => {
			let mut state = ParseState::from(filename);
			state.error = Some(e.to_string());
			return Err(state);
		}
	};
	let mut contents = String::new();
	match file.read_to_string(&mut contents) {
		Ok(_) => {},
		Err(e) => {
			let mut state = ParseState::from(filename);
			state.error = Some(e.to_string());
			return Err(state);
		}
	}
	parse_string_with_state(&contents, ParseState::from(filename))
}

// //////////////////////////////////////////////////////////

/// Parse a `String` into a `Program`
///
/// Utility function to easily parse any `String`.
///
/// ```
/// extern crate teko;
/// assert![teko::parse::parse_string("(+ 1 2 3) (' a b c)").is_ok()];
/// ```
pub fn parse_string(string: &str) -> Result<Program, ParseState> {
	let state = ParseState::default();
	parse_string_with_state(string, state)
}

// //////////////////////////////////////////////////////////

fn parse_string_with_state(string: &str, mut state: ParseState) -> Result<Program, ParseState> {
	for character in string.chars() {
		parse_character(character, &mut state)?;
		if state.error.is_some() {
			break;
		}
	}
	finish_parsing_characters(state)
}

// //////////////////////////////////////////////////////////

/// Convert the parser into an actual program
///
/// This function should be called after a series of calls to `parse_character`.
/// It takes a `state` and finalizes it into a program.
/// See `parse_character` for an example.
///
/// ```
/// extern crate teko;
/// assert![teko::parse::finish_parsing_characters(
///         teko::data_structures::ParseState::default()).is_ok()];
/// ```
pub fn finish_parsing_characters(mut state: ParseState) -> Result<Program, ParseState> {
	whitespace(&mut state);
	if !state.unmatched_opening_parentheses.is_empty() {
		Err(set_error(&mut state, "Unmatched opening parenthesis"))
	} else if state.error.is_some() {
		Err(state)
	} else {
		if let Some(mut first) = state.stack.pop() {
			first.reverse();
			Ok(first.clone())
		} else {
			Err(state)
		}
	}
}

/// Let us know if the parser can safely call `finish_parsing_characters`.
///
/// When implement character feeds we may not be certain when input ends, so we
/// need this function to be able to check if we can safely get a parse state.
/// The nice thing about this is that we can always parse more later and put
/// the result into the interpreter with the same effect.
pub fn is_ready_to_finish(state: &ParseState) -> bool {
	state.unmatched_opening_parentheses.is_empty() && state.token.is_empty() &&
		!state.stack.last().unwrap().is_empty()
}

/// Check if the parser is empty.
pub fn is_empty(state: &ParseState) -> bool {
	state.stack.last().unwrap().is_empty()
}

/// Parses character-by-character to allow parsing from arbitrary character sources.
///
/// Mainly used to implement utility functions that feed characters.
///
/// ```
/// extern crate teko;
/// let mut state = teko::data_structures::ParseState::default();
/// for ch in "(+ 1 2 3) (' a b c)".chars() {
/// 	assert![teko::parse::parse_character(ch, &mut state).is_ok()];
/// }
/// assert![teko::parse::finish_parsing_characters(state).is_ok()];
/// ```
pub fn parse_character(character: char, state: &mut ParseState) -> Result<(), ParseState> {
	parse_internal(character, state)?;
	count_characters_and_lines(character, state);
	Ok(())
}

// //////////////////////////////////////////////////////////
// Internal                                                //
// //////////////////////////////////////////////////////////

fn count_characters_and_lines(character: char, state: &mut ParseState) {
	if character == '\n' {
		state.current_read_position.line += 1;
		state.current_read_position.column = 1;
	} else {
		state.current_read_position.column += 1;
	}
}

fn parse_internal(character: char, state: &mut ParseState) -> Result<(), ParseState> {
	if character.is_whitespace() {
		whitespace(state);
	} else if character == '(' {
		left_parenthesis(state);
	} else if character == ')' {
		right_parenthesis(state)?;
	} else {
		otherwise(character, state);
	}
	Ok(())
}

// //////////////////////////////////////////////////////////

fn whitespace(state: &mut ParseState) {
	move_token_to_stack_if_nonempty(state);
}

fn left_parenthesis(state: &mut ParseState) {
	move_token_to_stack_if_nonempty(state);
	copy_current_read_position_to_unmatched_opening_parentheses(state);
	state.stack.push(vec![]);
}

fn right_parenthesis(state: &mut ParseState) -> Result<(), ParseState> {
	move_token_to_stack_if_nonempty(state);
	let source = pop_previous_opening_parenthesis(state)?;
	let mut top = if let Some(mut top) = state.stack.pop() {
		top
	} else {
		return Err(state.clone());
	};
	let mut active = Rc::new(Sourcedata(Some(source), Coredata::Null()));
	for top in top.iter().rev() {
		active = Rc::new(Sourcedata(
			top.0.clone(),
			Coredata::Cell(top.clone(), active),
		));
	}

	if let Some(ref mut stack) = state.stack.last_mut() {
		stack.push(active);
		return Ok(());
	}
	return Err(set_error(state, "Last state stack unavailable"));
}

fn otherwise(character: char, state: &mut ParseState) {
	if state.token.is_empty() {
		state.start_of_current_lexeme = state.current_read_position.clone();
	}
	state.token.push(character);
}

// //////////////////////////////////////////////////////////

fn move_token_to_stack_if_nonempty(state: &mut ParseState) {
	if !state.token.is_empty() {
		let currlex = state.start_of_current_lexeme.clone();
		let currtok = state.token.clone();
		if let Some(ref mut stack) = state.stack.last_mut() {
			stack.push(Rc::new(
				Sourcedata(Some(currlex), Coredata::Symbol(Symbol::from(currtok))),
			));
		}
		clear_token(state);
	}
}

fn clear_token(state: &mut ParseState) {
	state.token.clear();
}

fn set_error(state: &mut ParseState, message: &str) -> ParseState {
	state.error = Some(String::from(message));
	state.clone()
}

fn copy_current_read_position_to_unmatched_opening_parentheses(state: &mut ParseState) {
	state.unmatched_opening_parentheses.push(
		state
			.current_read_position
			.clone(),
	);
}

fn pop_previous_opening_parenthesis(state: &mut ParseState) -> Result<Source, ParseState> {
	if let Some(source) = state.unmatched_opening_parentheses.pop() {
		Ok(source)
	} else {
		Err(set_error(state, "Unmatched closing parenthesis"))
	}
}

// //////////////////////////////////////////////////////////
// Tests                                                   //
// //////////////////////////////////////////////////////////
#[cfg(test)]
mod tests {
	use super::*;
	macro_rules! assert_oks {
		( $f:expr, $( $x:expr ),*, ) => { assert_oks![$f, $( $x ),*]; };
		( $f:expr, $( $x:expr ),* ) => { { $( assert![$f($x).is_ok()]; )* } };
	}
	macro_rules! assert_errs {
		( $f:expr, $( $x:expr ),*, ) => { assert_errs![$f, $( $x ),*]; };
		( $f:expr, $( $x:expr ),* ) => { { $( assert![$f($x).is_err()]; )* } };
	}

	#[test]
	fn assert_expressions_ok() {
		assert_oks![
			parse_string,
			"",
			" ",
			"  ",
			"[",
			"]",
			"{",
			"}",
			".",
			",",
			"'",
			"\"",
			"",
			" ",
			"  ",
			"[",
			"]>",
			"<{",
			"}|",
			".^",
			",-",
			"'Æ",
			"\"o\"\"",
			"()",
			" ()",
			"() ",
			" () ",
			" ( ) ",
			"test",
			"(test)",
			" (test)",
			"(test) ",
			" (test) ",
			"(test1 (test2))",
			"(test1 (test2 test3 test4) test5) test6",
		];
	}

	#[test]
	fn assert_expressions_err() {
		assert_errs![
			parse_string,
			"(",
			")",
			"(test",
			"test)",
			"(test1 (test2)",
			"(((((((()))))))",
			"(((((()))))))",
		];
	}
}
