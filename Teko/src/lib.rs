#![feature(slice_patterns)]
extern crate num;

/* pub mod parse; */
/* pub mod interpret; */
pub mod interpret2;
pub mod parse2;

const VEC_CAPACITY: usize = 10;

/* #[cfg(test)] */
/* mod tests { */
/* 	use interpret::interpret; */
/* 	use parse::parse_string; */
/* 	use super::*; */
/* 	#[test] */
/* 	fn test() { */
/* 		interpret(parse_string("(define x 30) (set! x 5) (+ 1 2 x)").ok().unwrap()); */

/* 		/1* #[derive(Debug)] *1/ */
/* 		/1* enum List { *1/ */
/* 		/1* 	Cons(i32, RefCell<Rc<List>>), *1/ */
/* 		/1* 	Nil, *1/ */
/* 		/1* } *1/ */

/* 		/1* impl List { *1/ */
/* 		/1* 	fn tail(&self) -> Option<&RefCell<Rc<List>>> { *1/ */
/* 		/1* 		match *self { *1/ */
/* 		/1* 			List::Cons(_, ref item) => Some(item), *1/ */
/* 		/1* 			List::Nil => None, *1/ */
/* 		/1* 		} *1/ */
/* 		/1* 	} *1/ */
/* 		/1* } *1/ */
/* 		/1* for _ in 0..50000 { *1/ */
/* 		/1* 	let a = Rc::new(List::Cons(5, RefCell::new(Rc::new(List::Nil)))); *1/ */
/* 		/1* 	// println!("a initial rc count = {}", Rc::strong_count(&a)); *1/ */
/* 		/1* 	// println!("a next item = {:?}", a.tail()); *1/ */
/* 		/1* 	let b = Rc::new(List::Cons(10, RefCell::new(a.clone()))); *1/ */
/* 		/1* 	// println!("a rc count after b creation = {}", Rc::strong_count(&a)); *1/ */
/* 		/1* 	// println!("b initial rc count = {}", Rc::strong_count(&b)); *1/ */
/* 		/1* 	// println!("b next item = {:?}", b.tail()); *1/ */
/* 		/1* 	if let Some(ref link) = a.tail() { *1/ */
/* 		/1* 		*link.borrow_mut() = b.clone(); *1/ */
/* 		/1* 	} *1/ */
/* 		/1* 	// println!("b rc count after changing a = {}", Rc::strong_count(&b)); *1/ */
/* 		/1* 	// println!("a rc count after changing a = {}", Rc::strong_count(&a)); *1/ */
/* 		/1* } *1/ */
/* 	} */
/* } */
