extern crate num;

use std::fs::File;
use std::io::Read;

#[derive(Debug)]
pub enum List {
	Node(Vec<List>),
	Leaf(String),
}

#[derive(Clone, Copy, Debug)]
pub struct Position {
	line: usize,
	column: usize,
}

impl Default for Position {
	fn default() -> Position {
		Position {
			line: 1,
			column: 1,
		}
	}
}

#[derive(Debug)]
pub struct State {
	position: Position,
	unmatched_opening_parentheses: Vec<Position>,
	token: String,
	stack: Vec<List>,
	program: Vec<List>,
	error: Option<String>,
}

impl Default for State {
	fn default() -> State {
		State {
			position: Position::default(),
			unmatched_opening_parentheses: vec![],
			token: String::from(""),
			stack: vec![],
			program: vec![],
			error: None,
		}
	}
}

pub fn parse_file(filename: &str) -> Result<List, State> {
	let mut file = File::open(filename).ok().unwrap();
	let mut contents = String::new();
	file.read_to_string(&mut contents).ok();
	parse_string(&contents)
}

////////////////////////////////////////////////////////////

pub fn parse_string(string: &str) -> Result<List, State> {
	let mut state = State::default();
	for character in string.chars() {
		// println!["{:#?}", state];
		parse_character(character, &mut state);
		if state.error.is_some() {
			break;
		}
	}
	println!("{:#?}", state);
	finish_parsing_characters(state)
}

////////////////////////////////////////////////////////////

pub fn finish_parsing_characters(mut state: State) -> Result<List, State> {
	whitespace(&mut state);
		println!("{:#?}", state);
	if ! state.stack.is_empty() {
		println!("{:#?}", state);
		set_error(&mut state, "Unmatched opening parenthesis");
		Err(state)
	} else if state.error.is_some() {
		Err(state)
	} else {
		Ok(List::Node(state.program))
	}
}

pub fn parse_character(character: char, state: &mut State) {
	parse_internal(character, state);
	count_characters_and_lines(character, state);
}

////////////////////////////////////////////////////////////
// Internal                                               //
////////////////////////////////////////////////////////////

fn count_characters_and_lines(character: char, state: &mut State) {
	if character == '\n' {
		state.position.line   += 1;
		state.position.column =  1;
	} else {
		state.position.column += 1;
	}
}

fn parse_internal(character: char, state: &mut State) {
	if character.is_whitespace() {
		whitespace(state);
	} else if character == '(' {
		left_parenthesis(state);
	} else if character == ')' {
		right_parenthesis(state);
	} else {
		otherwise(character, state);
	}
}

////////////////////////////////////////////////////////////

fn whitespace(state: &mut State) {
	if state.token != "" {
		if state.stack.is_empty() {
			state.program.push(List::Leaf(state.token.clone()));
		} else {
			let mut last = state.stack.pop().unwrap();
			if let List::Node(ref mut vector) = last {
				vector.push(List::Leaf(state.token.clone()));
			}
			state.stack.push(last);
		}
		clear_token(state);
	}
}

fn left_parenthesis(state: &mut State) {
	whitespace(state);
	copy_location_to_last_open_location(state);
	state.stack.push(List::Node(vec![]));
}

fn right_parenthesis(state: &mut State) {
	whitespace(state);
	pop_previous_opening_parenthesis(state);
	if state.error.is_some() { return; }
	if state.stack.len() == 1 {
		move_stack_to_program(state);
	} else if state.stack.len() >= 2 {
		let last = state.stack.pop().unwrap();
		let second_last = state.stack.pop().unwrap();
		if let List::Node(mut vec) = second_last {
			vec.push(last);
			state.stack.push(List::Node(vec));
		}
	}
}

fn otherwise(character: char, state: &mut State) {
	state.token.push(character);
}

////////////////////////////////////////////////////////////

fn clear_token(state: &mut State) {
	state.token.clear();
}

fn move_stack_to_program(state: &mut State) {
	state.program.push(state.stack.pop().unwrap());
}

fn set_error(state: &mut State, message: &str) {
	state.token.clear();
	state.stack.clear();
	state.program.clear();
	state.error = Some(String::from(message));
}

fn copy_location_to_last_open_location(state: &mut State) {
	state.unmatched_opening_parentheses.push(state.position);
}

fn pop_previous_opening_parenthesis(state: &mut State) {
	if ! state.unmatched_opening_parentheses.pop().is_some() {
		set_error(state, "Unmatched closing parenthesis");
	}
}

////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
	use super::*;
	#[test]
	fn it_works() {
		println!["{:#?}", parse_string("(+ 1 2 3 (* 4 5) )+ (define (fn (x) (+ x y)))")];
	}
}
