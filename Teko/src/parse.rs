use std::fs::File;
use std::io::Read;
use std::rc::Rc;
use super::VEC_CAPACITY;

use data_structures::{Commands, Coredata, ParseState,
                      Program, Source, Sourcedata};

////////////////////////////////////////////////////////////

pub fn parse_file(filename: &str) -> Result<Program, ParseState> {
	let mut file = File::open(filename).ok().unwrap();
	let mut contents = String::new();
	file.read_to_string(&mut contents).ok();
	parse_string_with_state(&contents, ParseState::from_file(filename))
}

////////////////////////////////////////////////////////////

pub fn parse_string(string: &str) -> Result<Program, ParseState> {
	let state = ParseState::default();
	parse_string_with_state(string, state)
}

////////////////////////////////////////////////////////////

fn parse_string_with_state(string: &str, mut state: ParseState) -> Result<Program, ParseState> {
	for character in string.chars() {
		parse_character(character, &mut state)?;
		if state.error.is_some() {
			break;
		}
	}
	finish_parsing_characters(state)
}

////////////////////////////////////////////////////////////

pub fn finish_parsing_characters(mut state: ParseState) -> Result<Program, ParseState> {
	whitespace(&mut state);
	if ! state.unmatched_opening_parentheses.is_empty() {
		Err(set_error(&mut state, "Unmatched opening parenthesis"))
	} else if state.error.is_some() {
		Err(state)
	} else {
		Ok(state.stack)
	}
}

pub fn parse_character(character: char, state: &mut ParseState) -> Result<(), ParseState> {
	parse_internal(character, state)?;
	count_characters_and_lines(character, state);
	Ok(())
}

////////////////////////////////////////////////////////////
// Internal                                               //
////////////////////////////////////////////////////////////

fn count_characters_and_lines(character: char, state: &mut ParseState) {
	if character == '\n' {
		state.current_read_position.line   += 1;
		state.current_read_position.column =  1;
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

////////////////////////////////////////////////////////////

fn whitespace(state: &mut ParseState) {
	move_token_to_stack_if_nonempty(state);
}

fn left_parenthesis(state: &mut ParseState) {
	move_token_to_stack_if_nonempty(state);
	copy_current_read_position_to_unmatched_opening_parentheses(state);
	state.stack.push(Rc::new(Sourcedata(Some(state.current_read_position.clone()), Coredata::Internal(Commands::Empty))));
}

fn right_parenthesis(state: &mut ParseState) -> Result<(), ParseState> {
	move_token_to_stack_if_nonempty(state);
	pop_previous_opening_parenthesis(state)?;
	let mut active = Rc::new(Sourcedata(Some(state.current_read_position.clone()), Coredata::Null));
	let mut source = None;
	while let Some(top) = state.stack.pop() {
		match &*top {
			&Sourcedata(ref pair_source, Coredata::Internal(Commands::Empty)) => {
				source = pair_source.clone();
				break;
			},
			_ => {
				active = Rc::new(Sourcedata(top.0.clone(), Coredata::Pair(top.clone(), active)));
			},
		}
	}
	Rc::get_mut(&mut active).expect("There are no other references to the active set").0 = source;
	state.stack.push(active);
	Ok(())
}

fn otherwise(character: char, state: &mut ParseState) {
	if state.token.is_empty() {
		state.start_of_current_lexeme = state.current_read_position.clone();
	}
	state.token.push(character);
}

////////////////////////////////////////////////////////////

fn move_token_to_stack_if_nonempty(state: &mut ParseState) {
	if ! state.token.is_empty() {
		state.stack.push(Rc::new(Sourcedata(Some(state.start_of_current_lexeme.clone()), Coredata::Symbol(state.token.clone()))));
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
	state.unmatched_opening_parentheses.push(state.current_read_position.clone());
}

fn pop_previous_opening_parenthesis(state: &mut ParseState) -> Result<(), ParseState> {
	if ! state.unmatched_opening_parentheses.pop().is_some() {
		Err(set_error(state, "Unmatched closing parenthesis"))
	} else {
		Ok(())
	}
}

////////////////////////////////////////////////////////////
// Tests                                                  //
////////////////////////////////////////////////////////////

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
			"", " ", "  ", "[", "]", "{", "}", ".", ",", "'", "\"",
			"", " ", "  ", "[", "]>", "<{", "}|", ".^", ",-", "'Æ", "\"o\"\"",
			"()", " ()", "() ", " () ", " ( ) ",
			"test", "(test)", " (test)", "(test) ", " (test) ",
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
			"(test1 (test2)"
		];
	}
}
