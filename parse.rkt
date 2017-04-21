#lang racket

(provide state empty-state parse)

(require lens threading)

(define prepend cons)

(struct/lens state (token stack program error) #:prefab)

(define empty-state (state "" #f empty #f))

(define (parse character [state empty-state])
  (cond
    ([char-whitespace? character] (whitespace          state))
    ([char=? character #\(]       (left-parenthesis    state))
    ([char=? character #\)]       (right-parenthesis   state))
    (else                         (otherwise character state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (whitespace state)
  (if (non-empty-string? (state-token state))
      (~>
        (lens-transform (if (state-stack state)
                          (lens-compose first-lens state-stack-lens)
                          (lens-compose state-program-lens))
                        state (curry prepend (state-token state)))
        clear-token)
      state))

(define (left-parenthesis state)
  (let* ([state* (whitespace state)]
         [stack  (state-stack state*)])
        (cond
          ([false? stack] (lens-set       state-stack-lens state* '(())))
          ([list?  stack] (lens-transform state-stack-lens state* (curry cons empty)))
          (else                           (set-error state* "Unable to process opening parenthesis")))))

(define (right-parenthesis state)
  (let* ([state* (whitespace state)]
         [stack  (state-stack state*)])
        (writeln state*)
        (cond
          ([not (list? stack)]  (set-error state* "Unmatched closing parenthesis"))
          ([= (length stack) 1] (move-stack-to-program state*))
          ([> (length stack) 1] (lens-transform state-stack-lens state* (lambda (x) (cons (cons (first x) (second x)) (rest (rest x))))))
          (else                 (set-error state* "What the...")))))

(define (otherwise character state)
  (lens-transform state-token-lens state (curryr string-append (string character))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (clear-token state) (lens-set state-token-lens state ""))

(define (move-stack-to-program state)
  (~>
    (lens-transform state-program-lens state (curry cons (first (state-stack state))))
    (lens-set state-stack-lens _ #f)))

(define (set-error state error) (lens-set state-error-lens state error))
