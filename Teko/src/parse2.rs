use std::fs::File;
use std::io::Read;
use std::rc::Rc;
use super::VEC_CAPACITY;

use interpret2::Data;
use interpret2::Source;


#[derive(Debug)]
pub struct ParseState {
	current_read_position:        Source,
	start_of_current_lexeme: Source,
	unmatched_opening_parentheses: Vec<Source>,
	token:   String,
	stack:   Vec<Rc<Data>>,
	error:   Option<String>,
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

pub fn parse_file(filename: &str) -> Result<Vec<Rc<Data>>, ParseState> {
	let mut file = File::open(filename).ok().unwrap();
	let mut contents = String::new();
	file.read_to_string(&mut contents).ok();
	parse_string(&contents)
}

////////////////////////////////////////////////////////////

pub fn parse_string(string: &str) -> Result<Vec<Rc<Data>>, ParseState> {
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

pub fn finish_parsing_characters(mut state: ParseState) -> Result<Vec<Rc<Data>>, ParseState> {
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
		state.current_read_position.line   += 1;
		state.current_read_position.column =  1;
	} else {
		state.current_read_position.column += 1;
	}
}

fn parse_internal(character: char, state: &mut ParseState) {
	// println!["Before {:#?}", state];
	if character.is_whitespace() {
		whitespace(state);
	} else if character == '(' {
		left_parenthesis(state);
	} else if character == ')' {
		right_parenthesis(state);
	} else {
		otherwise(character, state);
	}
	// println!["After {:#?}", state];
}

////////////////////////////////////////////////////////////

fn whitespace(state: &mut ParseState) {
	move_token_to_stack(state);
}

fn move_token_to_stack(state: &mut ParseState) {
	if ! state.token.is_empty() {
		state.stack.push(Rc::new(Data::String(state.start_of_current_lexeme.clone(), state.token.clone())));
		clear_token(state);
	}
}

fn left_parenthesis(state: &mut ParseState) {
	move_token_to_stack(state);
	copy_current_read_position_to_unmatched_opening_parentheses(state);
	// println!["state.stack: {:#?}", state.stack];
	state.stack.push(Rc::new(Data::Pair(state.current_read_position.clone(), Rc::new(Data::Null), Rc::new(Data::Null))));
	// println!["state.stack: {:#?}", state.stack];
}

fn right_parenthesis(state: &mut ParseState) {
	move_token_to_stack(state);
	pop_previous_opening_parenthesis(state);
	loop {
		let top = state.stack.pop();
		if let Some(x) = top {
		} else {
			break;
		}
	}
		// println!["State: {:#?}", state];
		// This if checks the case '()', an empty list.
		// '(' pushes Pair(Null, Null) onto the stack. If no further elements are on the stack, then
		// we know that we have '()', which is encoded as Pair(Pair(Null, Null), Null)
		if let Some(top) = state.stack.last_mut() {
			if let Some(top) = Rc::get_mut(top) {
				match top {
					&mut Data::Pair(_, ref mut first, ref mut rest) => {
						let (null1, null2) = {
							(if let Data::Null = **first { true } else { false }, if let Data::Null = **rest { true } else { false })
						};
						if null1 == true && null2 == true {
							*first = Rc::new(Data::Pair(Source::default(), Rc::new(Data::Null), Rc::new(Data::Null)));
							break;
						}
					}
					&mut Data::String(_, _) => {},
					_ => {
						panic!("Internal: Top of the stack can't be anything but a Pair or String");
					},
				}
			}
		}
		if let Some(top) = state.stack.pop() {
			if let Some(second) = state.stack.last_mut() {
				if let Some(second) = Rc::get_mut(second) {
					match second {
						&mut Data::Pair(_, ref mut first, ref mut rest) => {
							// first, rest: &mut Rc<Data>
							let (null1, null2) = {
								const EXPECT: &str = "The stack represents a tree; it can't be cyclic";
								let first = Rc::get_mut(first).expect(EXPECT);
								let rest = Rc::get_mut(rest).expect(EXPECT);
								(if let Data::Null = *first { true } else { false }, if let Data::Null = *rest { true } else { false })
							};
							if null1 == true && null2 == true {
								*first = top;
							} else if null1 == false && null2 == true {
								*rest = top;
							} else {
								panic!("Cant occur");
							}
							if null1 == true && null2 == true {
								break;
							}
							// println!{"It's time to stop"};
						},
						_ => {},
					}
				} else {
					panic!("Unmatched 3close par dude like wtf");
				}
			} else {
				break;
				// panic!("Unmatched close par dude like wtf");
			}
		} else {
			break;
			panic!("Unmatched close par dude");
		}
	}
}

fn otherwise(character: char, state: &mut ParseState) {
	if state.token.is_empty() {
		state.start_of_current_lexeme = state.current_read_position.clone();
	}
	state.token.push(character);
}

////////////////////////////////////////////////////////////

fn clear_token(state: &mut ParseState) {
	state.token.clear();
}

fn set_error(state: &mut ParseState, message: &str) {
	/* state.token.clear(); */
	/* state.stack.clear(); */
	/* state.program.clear(); */
	state.error = Some(String::from(message));
}

fn copy_current_read_position_to_unmatched_opening_parentheses(state: &mut ParseState) {
	state.unmatched_opening_parentheses.push(state.current_read_position.clone());
}

fn pop_previous_opening_parenthesis(state: &mut ParseState) {
	if ! state.unmatched_opening_parentheses.pop().is_some() {
		set_error(state, "Unmatched closing parenthesis");
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
			"", " ", "  ", "[", "]>", "<{", "}|", ".^", ",-", "'", "\"",
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
