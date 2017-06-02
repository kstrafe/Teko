use std::fs::File;
use std::io::Read;
use std::rc::Rc;
use super::VEC_CAPACITY;

use interpret2::Data;
use interpret2::Source;


#[derive(Debug)]
pub struct ParseState {
	position: Source,
	start_of_lexeme: Source,
	unmatched_opening_parentheses: Vec<Source>,
	token:   String,
	stack:   Vec<Data>,
	program: Vec<Data>,
	error:   Option<String>,
}

impl Default for ParseState {
	fn default() -> ParseState {
		ParseState {
			position: Source::default(),
			start_of_lexeme: Source::default(),
			unmatched_opening_parentheses: Vec::with_capacity(VEC_CAPACITY),
			token: String::from(""),
			stack: Vec::with_capacity(VEC_CAPACITY),
			program: Vec::with_capacity(VEC_CAPACITY),
			error: None,
		}
	}
}

pub fn parse_file(filename: &str) -> Result<Vec<Data>, ParseState> {
	let mut file = File::open(filename).ok().unwrap();
	let mut contents = String::new();
	file.read_to_string(&mut contents).ok();
	parse_string(&contents)
}

////////////////////////////////////////////////////////////

pub fn parse_string(string: &str) -> Result<Vec<Data>, ParseState> {
	let mut state = ParseState::default();
	for character in string.chars() {
		// println!["{:#?}", state];
		parse_character(character, &mut state);
		if state.error.is_some() {
			break;
		}
	}
	finish_parsing_characters(state)
}

////////////////////////////////////////////////////////////

pub fn finish_parsing_characters(mut state: ParseState) -> Result<Vec<Data>, ParseState> {
	whitespace(&mut state);
	if ! state.unmatched_opening_parentheses.is_empty() {
		set_error(&mut state, "Unmatched opening parenthesis");
		Err(state)
	} else if state.error.is_some() {
		Err(state)
	} else {
		Ok(state.stack)
	}
}

pub fn parse_character(character: char, state: &mut ParseState) {
	parse_internal(character, state);
	count_characters_and_lines(character, state);
}

////////////////////////////////////////////////////////////
// Internal                                               //
////////////////////////////////////////////////////////////

fn count_characters_and_lines(character: char, state: &mut ParseState) {
	if character == '\n' {
		state.position.line   += 1;
		state.position.column =  1;
	} else {
		state.position.column += 1;
	}
}

fn parse_internal(character: char, state: &mut ParseState) {
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

fn whitespace(state: &mut ParseState) {
	if ! state.token.is_empty() {
		state.stack.push(Data::Pair(state.position.clone(),
		                            Rc::new(Data::String(state.start_of_lexeme.clone(), state.token.clone())),
		                            Rc::new(Data::Null)));
		clear_token(state);
	}
}

fn left_parenthesis(state: &mut ParseState) {
	whitespace(state);
	copy_location_to_last_open_location(state);
	state.stack.push(Data::Pair(state.position.clone(), Rc::new(Data::Null), Rc::new(Data::Null)));
}

fn right_parenthesis(state: &mut ParseState) {
	whitespace(state);
	pop_previous_opening_parenthesis(state);
	// Now we need to pop until we encounter a P_NN:
	loop {
		// println!["{:#?}", state];
		if let Some(top) = state.stack.pop() {
			if let Some(second) = state.stack.last_mut() {
				match second {
					&mut Data::Pair(_, ref mut first, ref mut rest) => {
						// first, rest: &mut Rc<Data>
						//
						let (null1, null2) = {
							let first = Rc::get_mut(first).expect("The stack represents a tree; it can't be cyclic");
							let rest = Rc::get_mut(rest).expect("The stack represents a tree; it can't be cyclic");
							(if let Data::Null = *first { true } else { false }, if let Data::Null = *rest { true } else { false })
						};
						if null1 == true && null2 == false {
							*first = Rc::new(Data::Pair(Source::default(), Rc::new(Data::Null), rest.clone()));
							*rest = Rc::new(top);
						} else {
							*rest = Rc::new(top);
						}
						if null1 == true && null2 == true {
							break;
						}
						println!{"It's time to stop"};
					},
					_ => {},
				}
			} else {
				panic!("Unmatched close par dude like wtf");
			}
		} else {
			panic!("Unmatched close par dude");
		}
	}
}

fn otherwise(character: char, state: &mut ParseState) {
	if state.token.is_empty() {
		state.start_of_lexeme = state.position.clone();
	}
	state.token.push(character);
}

////////////////////////////////////////////////////////////

fn clear_token(state: &mut ParseState) {
	state.token.clear();
}

fn move_stack_to_program(state: &mut ParseState) {
	state.program.push(state.stack.pop().unwrap());
}

fn set_error(state: &mut ParseState, message: &str) {
	/* state.token.clear(); */
	/* state.stack.clear(); */
	/* state.program.clear(); */
	state.error = Some(String::from(message));
}

fn copy_location_to_last_open_location(state: &mut ParseState) {
	state.unmatched_opening_parentheses.push(state.position.clone());
}

fn pop_previous_opening_parenthesis(state: &mut ParseState) {
	if ! state.unmatched_opening_parentheses.pop().is_some() {
		set_error(state, "Unmatched closing parenthesis");
	}
}

////////////////////////////////////////////////////////////
// Tests                                                  //
////////////////////////////////////////////////////////////

/* #[cfg(test)] */
/* mod tests { */
/* 	use super::*; */
/* 	macro_rules! assert_oks { */
/* 		( $f:expr, $( $x:expr ),*, ) => { assert_oks![$f, $( $x ),*]; }; */
/* 		( $f:expr, $( $x:expr ),* ) => { { $( assert![$f($x).is_ok()]; )* } }; */
/* 	} */
/* 	macro_rules! assert_errs { */
/* 		( $f:expr, $( $x:expr ),*, ) => { assert_errs![$f, $( $x ),*]; }; */
/* 		( $f:expr, $( $x:expr ),* ) => { { $( assert![$f($x).is_err()]; )* } }; */
/* 	} */
/* 	#[test] */
/* 	fn assert_expressions_ok() { */
/* 		assert_oks![ */
/* 			parse_string, */
/* 			"", " ", "  ", "[", "]", "{", "}", ".", ",", "'", "\"", */
/* 			"test", */
/* 			"(test)", */
/* 			" (test)", */
/* 			"(test) ", */
/* 			" (test) ", */
/* 			"(test1 (test2))", */
/* 			"(test1 (test2 test3 test4) test5) test6", */
/* 		]; */
/* 	} */

/* 	#[test] */
/* 	fn assert_expressions_err() { */
/* 		assert_errs![ */
/* 			parse_string, */
/* 			"(", */
/* 			")", */
/* 			"(test", */
/* 			"test)", */
/* 			"(test1 (test2)" */
/* 		]; */
/* 	} */
/* } */
