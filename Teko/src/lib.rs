//! The Teko Programming Language implemented in Rust.
//!
//! This implementation provides parsing and evaluation utilities of the Teko programming language.
//! Teko belongs to the family of Lisp-1 languages and features dynamic scoping, first-class macros
//! , and tail call optimization.
//!
//! Teko is made to be used as an everyday scripting language. The language was designed to be
//! easy to implement yet useful. Comparing Teko to other Lisps reveals the core motivation:
//! to implement a super-minimal Lisp capable of being a fully fledged programming language.
//!
//! Teko is unique in that it's strict yet lacks interior mutability. This allows the
//! implementation to opt for reference counted garbage collection; which is desirable in
//! real-time applications as it doesn't cause unforeseen pauses in execution.
//!
//! Here is the iconic `hello world` in Teko:
//!
//! ```text
//! (" Hello world!)
//! ```
//! No functional-like language is complete without the definition of the tail-recursive factorial
//! function.
//!
//! ```text
//! (define factorial (fn (n accum)
//!                       (if (>= n 1)
//!                           (factorial (- n 1) (* n accum))
//!                           accum)))
//! (factorial 5 1)
//! ```
//! Example: using this library to interpret Teko:
//!
//! ```
//! extern crate teko;
//! extern crate num_traits;
//! use num_traits::cast::ToPrimitive;
//! fn main() {
//! 	let program = teko::parse::parse_string("
//! 	(define factorial (fn (n accum)
//! 	                      (if (>= n 1)
//! 	                         (factorial (- n 1) (* n accum))
//! 	                         accum)))
//! 	(write (factorial 5 1))").ok().unwrap();
//! 	let env = teko::interpret::interpret(program);
//! 	match env.result.1 {
//! 		teko::data_structures::Coredata::Integer(ref value) => {
//! 			assert_eq![value.to_i32().unwrap(), 120];
//! 		}
//! 		_ => {
//! 			panic!["Expected Integer but got a different data type"];
//! 		}
//! 	}
//! }
//! ```
//!
//! Note that `write` doesn't yield a result in the example above so the previous
//! result from `factorial` is left inside the environment instead.

// //////////////////////////////////////////////////////////
// ✓ Implementor's checklist:
//
// ✓ Core expansion, parameterizations, and preparation
// ✓ Builtin Function calls
// ✓ Builtin Macro calls
// ✓ Tail call optimization
// ✓ If branching
// ✓ Integer parsing
//   Rational parsing + promotion
//   Complex parsing + promotion
//   <, >, =, <=, >=, != number comparison
//   Boolean not, and, or
// ✓ head/tail/pair
// ✓ wind/unwind
// ✓ ' quote
//   ` quasiquote
// ✓ " strings
// ✓ Add the error creation function
// ✓ Make Source data optional
// ✓ Macroize the initial environment (to clean up code)
//   Test different TCO strategies (HashSet, sorted Vec,..)
//   Implement powers for numbers
//   Replace all panics with unwinds
// ✓ Replace panics with unwinds in 'fn eval'
//   Formalize error messages and feedback (similar to rust errors, they are nice)
// ✓ Change transfer functions, do we need top?
//   Implement a proper fmt::Display for Sourcedata
// ✓ Use booleans for If
// ✓ Easily add constants (pi, e, true, false)
// ✓ Sort the builtins.rs file by function names
//   Sort imports and uses where possible
//   Improve unwinding (do we need to pop params?)
// //////////////////////////////////////////////////////////

#![feature(slice_patterns)]
extern crate num;

#[macro_use]
mod macros;

pub mod builtins;
pub mod data_structures;
pub mod interpret;
pub mod parse;
pub mod utilities;

const VEC_CAPACITY: usize = 10;
