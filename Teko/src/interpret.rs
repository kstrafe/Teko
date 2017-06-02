use parse::List;
use std::collections::HashMap;
use std::rc::Rc;
use super::VEC_CAPACITY;

#[derive(Clone, Debug)]
struct Fn {
	parameters: Vec<String>,
	code:       List,
}

#[derive(Clone, Debug)]
struct Mo {
	parameter: String,
	code:      List,
}

#[derive(Clone, Debug)]
enum Data {
	Integer(i32),
	Rational(i32, i32),
	SingleFlonum(f32),
	DoubleFlonum(f32),
	Pair(Rc<Data>, Rc<Data>),
	List(List),
	Function(Fn),
	Macro(Mo),
	String(String),
	Vector(Vec<Rc<Data>>),
	Empty,

	Plus,
}

#[derive(Debug)]
struct Env {
	content:      HashMap<String, Vec<Rc<Data>>>,
	call_stack:   Vec<String>,
	params:       Vec<Rc<List>>,
	return_value: Rc<Data>,
}

impl Env {
	fn set_return(&mut self, data: Rc<Data>) {
		self.return_value = data;
	}

	fn set_content_to_return(&mut self, string: &String) {
		self.return_value = self.content.get(string).unwrap().first().unwrap().clone();
	}
}

pub fn interpret(mut program: Vec<List>) {
	let env = Env {
		content:      [(String::from("+"), vec![Rc::new(Data::Plus)])
		              ].iter().cloned().collect(),
		call_stack:   Vec::with_capacity(VEC_CAPACITY),
		params:       Vec::with_capacity(VEC_CAPACITY),
		return_value: Rc::new(Data::Empty),
	};
	let env = eval(program, env);
}

fn eval(mut program: Vec<List>, mut env: Env) -> Env {
	while ! program.is_empty() {
		println!["{:?}\n{:#?}", program, env];
		let top = program.remove(0);
		match top {
			List::DefineReturnAs(string) => {
				println!["DefineReturnAs({:?})", string];
				if env.content.contains_key(&string) {
					panic!["You can not define something that's already defined"];
				} else {
					env.content.insert(string, vec![env.return_value.clone()]);
				}
			},
			List::SetReturnAs(string) => {
				println!["SetReturnAs({:?})", string];
				if env.content.contains_key(&string) {
					env.content.get_mut(&string).unwrap().pop();
					env.content.get_mut(&string).unwrap().push(env.return_value.clone());
				} else {
					panic!["You can not set! something that's not defined"];
				}
			},
			List::Node(mut vec) => {
				let first = vec.remove(0);
				let mut rest  = vec;
				match first {
					List::Node(expr) => {
						program.insert(0, List::Call(rest));
						program.insert(0, List::ReturnToCallstack);
						program.insert(0, List::Node(expr));
					},
					List::Leaf(atom) => {
						match &atom[..] {
							"define" => {
								assert![rest.len() == 2];
								let expr = rest.pop().unwrap();
								let atom = match rest.pop().unwrap() {
									List::Leaf(string) => string,
									_ => panic!("Can't be!"),
								};
								program.insert(0, List::DefineReturnAs(atom));
								program.insert(0, expr);
							},
							"set!" => {
								assert![rest.len() == 2];
								let expr = rest.pop().unwrap();
								let atom = match rest.pop().unwrap() {
									List::Leaf(string) => string,
									_ => panic!("Can't be!"),
								};
								program.insert(0, List::SetReturnAs(atom));
								program.insert(0, expr);
							},
							otherwise => {
								program.insert(0, List::Call(rest));
								program.insert(0, List::ReturnToCallstack);
								// TODO; just load the function directly here
								program.insert(0, List::Leaf(String::from(otherwise)));
							},
						}
					},
					List::Node(expr) => {
						println!["EXPR"];
					},
					_ => {},
				}
			},
			List::Leaf(string)  => {
				if all_digits(&string) {
					let num = string.parse::<i32>().unwrap();
					env.set_return(Rc::new(Data::Integer(num)));
					println!["IS ALL DIGS {}", num];
				} else {
					let entry =
						if let Some(data) = env.content.get(&string) {
							if let Some(entry) = data.last() {
								entry.clone()
							} else {
								panic!("Entry set is empty, internal bug");
							}
						} else {
							panic!("Unable to find reference in hash");
						};
					env.set_return(entry);
				}
			},
			List::Call(args) => {
				match *env.return_value {
					Data::Macro(ref macro_object) => {},
					Data::Function(ref function) => {},
					Data::Plus => {
						// env.params.
						for i in args {
							//env.program.insert(0, );
							// env.program.insert(0, i);
						}
					},
					_ => {
					},
				}
			},
			_ => {},
		}
	}
	env
}

fn all_digits(string: &str) -> bool {
	string.chars().all(|c| c.is_digit(10))
}
