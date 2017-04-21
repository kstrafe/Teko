#! /usr/bin/env racket
#lang racket

(require lens threading)

(define prepend cons)

(struct/lens state (tree list token) #:prefab)
(define empty-state (state empty #f ""))

(define (append-character-to-token character state)
  (lens-transform state-token-lens state (curryr string-append (string character))))

(define (move-list-to-tree state)
  (~>
    (lens-transform state-tree-lens state (lambda (x) (cons (state-list state) x)))
    (lens-set state-list-lens _ #f)))

(define (interpret-left-parenthesis state)
  (cond
    ([false? (state-list state)] (lens-set       state-list-lens state '(())))
    ([list?  (state-list state)] (lens-transform state-list-lens state (curry cons empty)))
    (else "Something is seriously wrong, state-list is neither false nor a list")))
(define (interpret-right-parenthesis state)
  (let ([state* (move-token-to-list state)])
    (cond
      ([= (length (state-list state*)) 1] (move-list-to-tree state*))
      (else (lens-transform state-list-lens state* (lambda (x) (cons (cons (first x) (second x)) (rest (rest x)))))))))

(define (clear-token state) (lens-set state-token-lens state ""))
(define (clear-list  state) (lens-set state-list-lens  state empty))

(define (move-token-to-list state)
  (if (non-empty-string? (state-token state))
    (~>
      (lens-transform (lens-compose first-lens state-list-lens) state (curry prepend (state-token state)))
      clear-token)
    state))

(let ([string (string->list (file->string "input"))])
  (define (ll1 in state)
    (displayln state)
    (cond
      ([char-whitespace? in] (move-token-to-list state))
      ([char=? in #\(] (interpret-left-parenthesis state))
      ([char=? in #\)] (interpret-right-parenthesis state))
      (else (append-character-to-token in state))))
  (foldl ll1 empty-state string))

