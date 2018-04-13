# Rationale #

The lambda calculus is a beautiful foundation of functional programming, but it has its flaws:

* No macro facility - This makes ergonomic programming hard
* Currying is implicit - Making variadic arguments quite un-ergonomic

## Example of Unergonomic Lambda Calculus ##

Not having a macro facility makes it impossible to create syntax-aware transformations:
`log (add 1 2)` can transform into the string `"log (add 1 2) = 3"` only using macros.
Being forced to instead write `log "(add 1 2)" (add 1 2)` ought to be criminal.

Implicit currying makes it undecidable when the function is to compute or take further arguments:
Does `add 1 2 4` result in `(add 1 2) 4 => 3 4` or `7`?

Suppose we want to add a variable amount of arguments.
In the lambda calculus we'd need to construct a list, and the
function would take the entire list as an argument.

It'd look like this:
```lisp
add (pair 5 (pair 9 (pair 3 0)))
```

But what we really want to express is:
```lisp
add 5 9 3
```

To remedy this I propose a new calculus called the Tau Calculus.

## Tau Calculus ##

Differences between the lambda and tau calculi:

* Application is explicit in the tau calculus | `(add 1 2)` instead of `add 1 2`
* The tau calculus admits macros | τ functions
* There is no implicit currying in the tau calculus | `(add 1)` does not return a function

## Examples ##


### Explicit Application ###
Application is explicit, which means that terms `A B C` in the lambda calculus are written `(A B C)` in the tau calculus.

Conventional lambda calculus:
```lisp
2 = (λ (f x) f (f x))           # Number 2
3 = (λ (f x) f (f (f x)))       # Number 3
add = (λ (m n f x) m f (n f x)) # Addition function

add 2 3 -> (λ (f x) f (f (f (f (f x)))))
```

Tau calculus:
```lisp
2 = (λ (f x) (f (f x)))                 # Number 2
3 = (λ (f x) (f (f (f x))))             # Number 3
add = (λ (m n) (λ (f x) (m f (n f x)))) # Addition function

(add 2 3) -> (λ (f x) (f (f (f (f (f x))))))
```

### Macros ###

Implementing `log` as exemplified earlier is easy using macros:
```lisp
log = (τ (x) (string-append "log " (write (first (first x))) " = " (, (first (first x)))))

(log (add 1 2)) -> "log (add 1 2) = 3"
```

Inside `log` the argument `(add 1 2)` takes the form
```lisp
(' ((add 13 5) (3 6)
      (add (3 7))
      (13 (3 9))
      (5 (3 11))))
```
making each symbol in the expression a syntax object. (`'` is the quoting symbol, this prevents evaluation, `,` unquotes a datum, thus evaluating it.)

### No Implicit Currying ###

In lambda calculus the expression `add 3` reduces to `(λ (n f x) (λ (f x) f (f (f x))) f (n f x))`.

In tau calculus the expression `(add 3)` is an error.
