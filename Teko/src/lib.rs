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
// //////////////////////////////////////////////////////////

#![feature(slice_patterns)]
extern crate num;

mod builtins;
mod data_structures;
mod interpret;
mod parse;
mod utilities;

const VEC_CAPACITY: usize = 10;
