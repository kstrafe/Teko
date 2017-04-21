#! /usr/bin/env racket
#lang racket

(require lens threading)

(define prepend cons)

(struct/lens state (token stack program) #:prefab)
(define empty-state (state "" #f empty))

(define (append-character-to-token character state)
  (lens-transform state-token-lens state (curryr string-append (string character))))

(define (move-stack-to-program state)
  (~>
    (lens-transform state-program-lens state (lambda (x) (cons (first (state-stack state)) x)))
    (lens-set state-stack-lens _ #f)))

(define (interpret-left-parenthesis state)
  (let ([state* (move-token-to-stack state)])
    (cond
      ([false? (state-stack state*)] (lens-set       state-stack-lens state* '(())))
      ([list? (state-stack state*)]  (lens-transform state-stack-lens state* (curry cons empty)))
      (else "Something is seriously wrong, state-stack is neither false nor a stack"))))

(define (interpret-right-parenthesis state)
  (let ([state* (move-token-to-stack state)])
    (cond
      ([= (length (state-stack state*)) 1] (move-stack-to-program state*))
      (else (lens-transform state-stack-lens state* (lambda (x) (cons (cons (first x) (second x)) (rest (rest x)))))))))

(define (clear-token state) (lens-set state-token-lens state ""))
(define (clear-stack  state) (lens-set state-stack-lens  state empty))

(define (move-token-to-stack state)
  (if (non-empty-string? (state-token state))
    (if (false? (state-stack state))
      (~>
        (lens-transform (lens-compose state-program-lens) state (curry prepend (state-token state)))
        clear-token)
      (~>
        (lens-transform (lens-compose first-lens state-stack-lens) state (curry prepend (state-token state)))
        clear-token))
    state))

(let ([string (string->list (file->string "input"))])
  (define (ll1 in state)
    (writeln state)
    (cond
      ([char-whitespace? in] (move-token-to-stack state))
      ([char=? in #\(] (interpret-left-parenthesis state))
      ([char=? in #\)] (interpret-right-parenthesis state))
      (else (append-character-to-token in state))))
  (foldl ll1 empty-state string))

