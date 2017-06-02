extern crate num;

use std::fs::File;
use std::io::prelude::*;
use std::io::Read;

#[derive(Debug)]
enum List {
	Node(Vec<List>),
	Leaf(String),
}

#[derive(Clone, Copy, Debug)]
struct Position {
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
struct State {
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

fn parse_file(filename: &str) -> State {
	let mut file = File::open(filename).ok().unwrap();
	let mut contents = String::new();
	file.read_to_string(&mut contents).ok();
	parse_string(&contents)
}

fn parse_string(string: &str) -> State {
	let mut state = State::default();
	for character in string.chars() {
		println!["{:#?}", state];
		parse_character(character, &mut state);
		if state.error.is_some() {
			break;
		}
	}
	finish_parsing_characters(&mut state);
	state
}

////////////////////////////////////////////////////////////

fn finish_parsing_characters(state: &mut State) {
	whitespace(state);
}

fn parse_character(character: char, state: &mut State) {
	parse_internal(character, state);
	count_characters_and_lines(character, state);
}

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

fn copy_location_to_last_open_location(state: &mut State) {
	state.unmatched_opening_parentheses.push(state.position);
}

fn pop_previous_opening_parenthesis(state: &mut State) {
	state.unmatched_opening_parentheses.pop();
}

////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
	use super::*;
	#[test]
	fn it_works() {
		println!["{:#?}", parse_string("(+ 1 2 3 (* 4 5) ) +")];
	}
}
