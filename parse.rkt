#lang racket

(provide state empty-state parse parse-file parse-string)

(require "skeltal.rkt" lens threading)

(define prepend cons)

(skeltals (state (line column token stack program error)))

(define empty-state (state 1 1 "" #f empty #f))

(define (parse-file filename)
  (parse-string (file->string filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-string string [state empty-state])
  ; (foldl parse state (string->list string)))
  (for/fold ([state empty-state])
            ([character (string->list string)])
            #:break (state-error state)
            (parse character state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse character [state empty-state])
  (let/ec escape
          (~>> (parse-internal             escape character state)
               (count-characters-and-lines character _))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (count-characters-and-lines character state)
  (cond
    ([char=? character #\newline] (~> (lens-transform state-line-lens state add1)
                                      (lens-set state-column-lens     _     1)))
    (else                         (lens-transform state-column-lens   state add1))))

(define (parse-internal escape character state)
  (cond
    ([char-whitespace? character] (whitespace          state))
    ([char=? character #\(]       (left-parenthesis    state escape))
    ([char=? character #\)]       (right-parenthesis   state escape))
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

(define (left-parenthesis state escape)
  (let* ([state* (whitespace state)]
         [stack  (state-stack state*)])
        (cond
          ([false? stack] (lens-set       state-stack-lens state* '(())))
          ([list?  stack] (lens-transform state-stack-lens state* (curry cons empty)))
          (else                           (escape (set-error state* "Unable to process opening parenthesis"))))))

(define (right-parenthesis state escape)
  (let* ([state* (whitespace state)]
         [stack  (state-stack state*)])
        (cond
          ([not (list? stack)]  (escape (set-error state* "Unmatched closing parenthesis")))
          ([= (length stack) 1] (move-stack-to-program state*))
          ([> (length stack) 1] (lens-transform state-stack-lens state* (lambda (x) (cons (cons (first x) (second x)) (rest (rest x))))))
          (else                 (escape (set-error state* "Unable to process closing parenthesis"))))))

(define (otherwise character state)
  (lens-transform state-token-lens state (curryr string-append (string character))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (clear-token state) (lens-set state-token-lens state ""))

(define (move-stack-to-program state)
  (~>
    (lens-transform state-program-lens state (curry cons (first (state-stack state))))
    (lens-set state-stack-lens _ #f)))

(define (set-error state error)
  (~>
    empty-state
    (lens-set state-error-lens _ error)
    (lens-set state-line-lens _ (state-line state))
    (lens-set state-column-lens _ (state-column state))))
