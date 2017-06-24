//! The Teko Programming Language implemented in Rust.
//!
//! # About #
//! This implementation provides parsing and evaluation utilities of the Teko programming language.
//! Teko belongs to the family of **Lisp-1** languages and features **dynamic scoping**,
//! **first-class macros**, and **tail call optimization**.
//!
//! Teko is made to be used as an everyday scripting language. The language was designed to be
//! easy to implement yet useful. Comparing Teko to other Lisps reveals the core motivation:
//! to implement a super-minimal Lisp capable of being a fully fledged programming language.
//!
//! Teko is has the property that it's **strictly evaluated** yet lacks **interior mutability**.
//! This allows
//! the implementation to opt for **reference counted** garbage collection - because cycles can't
//! be created - which is desirable in real-time applications as it doesn't cause unforeseen
//! pauses in execution.
//!
//! # Why Lisp? #
//! Here's my favorite excerpt that words it perfectly, from
//! [Let Over Lambda](https://letoverlambda.com/index.cl/guest/chap1.html#sec_1), ch. 1:
//!
//! ```text
//! Macros are the single greatest advantage that lisp has as a programming language and the single
//! greatest advantage of any programming language. With them you can do things that you simply
//! cannot do in other languages. Because macros can be used to transform lisp into other
//! programming languages and back, programmers who gain experience with them discover that all
//! other languages are just skins on top of lisp. This is the big deal. Lisp is special because
//! programming with it is actually programing at a higher level. Where most languages invent and
//! enforce syntactic and semantic rules, lisp is general and malleable.
//! With lisp, you make the rules.
//! ```
//!
//! # Examples Code #
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
//!                       (if (= n 1)
//!                           accum
//!                           (factorial (- n 1) (* n accum)))))
//! (factorial 5 1)
//! ```
//! # Usage #
//! Example: using this library to interpret Teko:
//!
//! ```
//! extern crate teko;
//! extern crate num_traits;
//! use num_traits::cast::ToPrimitive;
//! fn main() {
//! 	let program = teko::parse::parse_string("
//! 	(define factorial (fn (n accum)
//! 	                      (if (= n 1)
//! 	                          accum
//! 	                          (factorial (- n 1) (* n accum)))))
//! 	(write (factorial 5 1))").ok().unwrap();
//! 	let env = teko::interpret::interpret(program);
//!
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
// ✓ Implementor's checklist: (✓ = Implemented | ✗ = rejected | empty = unimplemented
//
// ✓ Core expansion, parameterizations, and preparation
// ✓ Builtin Function calls
// ✓ Builtin Macro calls
// ✓ Tail call optimization
// ✓ If branching
// ✓ Integer parsing
// ✓ head/tail/pair
// ✓ wind/unwind
// ✓ ' quote
// ✓ " strings
// ✓ Add the error creation function
// ✓ Make Source data optional
// ✓ Macroize the initial environment (to clean up code)
// ✓ Replace panics with unwinds in 'fn eval'
// ✓ Change transfer functions, do we need top?
// ✓ Use booleans for If
// ✓ Easily add constants (pi, e, true, false)
// ✓ Sort the builtins.rs file by function names
// ✓ Improve error unwinding (do we need to pop params?), add formal errors
// ✓ transfer -> Option<String> for consistent error handling
// ✗ ` quasiquote                            - Can be built from primitives
// ✗ Test different TCO strategies (HashSet, sorted Vec,..)  - Not important
// ✗ Implement powers for numbers                            - Implemented using primitives
// ✗ Rational parsing + promotion            - Not minimal
// ✗ Complex parsing + promotion             - Not minimal
// ✓ <, >, =, <=, >=, != number comparison   - Only < and == builtin, others derived
// ✓ Boolean not, and, or
// ✗ Create a builtin error registry         - Not minimal, keep errors short
// ✓ quote ✓ symbol?  ✓ same?  ✓ pair?  ✓ head ✓ tail ✓ pair ✓ if ✓ fn ✓ mo
// ✗ Create FFI for C                        - Not minimal
// ✗ Functional map/set/trie/fingertree      - Not very minimal
// ✗ Multithreading                          - Not part of the idealized language
// ✗ Channels                                - As above
// ✗ Make Userdata easily editable           - Is only a reference impl, no need
// ✓ Replace all panics with unwinds
// ✓ Sort imports and uses where possible
// ✓ Implement a proper fmt::Display for Sourcedata
// ✓ Actually make error handling consistent + stacktrace
// ✓ Clippify everything
// ✓ Write tests (not complete, but the structure is there)
// ✓ Document everything
// ✓ Make readme and build instructions
//   Create extension interface (not sure if feasible atm)
//
// //////////////////////////////////////////////////////////

#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]
#![feature(slice_patterns)]

extern crate num;

#[macro_use]
mod macros;

pub mod builtins;
pub mod data_structures;
pub mod interpret;
pub mod parse;
pub mod utilities;

// Preallocate buffers for each Vec
const VEC_CAPACITY: usize = 100;
