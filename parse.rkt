#lang racket

(provide finish-parsing-characters parse-character parse-file parse-string)

(require "skeltal.rkt"
         lens threading)

(skeltals
  (position (line column))
  (state    (position unmatched-opening-parentheses token stack program error)))

(define empty-state (state (position 1 1) empty "" #f empty #f))

(define (parse-file filename)
  (parse-string (file->string filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-string string)
  (let ([state* (for/fold ([state empty-state])
                          ([character (string->list string)])
                          #:break (state-error state)
                          (parse-character character state))])
       (finish-parsing-characters state*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (finish-parsing-characters state)
  (define (deep-reverse lst) (if (list? lst) (reverse (map deep-reverse lst)) lst))
  (let ([state* (whitespace state)])
    (if (state-stack state*)
        (set-error state* "Unmatched opening parenthesis")
        (deep-reverse (state-program state*)))))

(define (parse-character character [state empty-state])
  (let/ec escape
          (~>> (parse-internal             escape character state)
               (count-characters-and-lines character _))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (count-characters-and-lines character state)
  (lens-transform state-position-lens
                  state
                  (lambda (position)
                          (cond ([char=? character #\newline] (~> (lens-transform position-line-lens position add1)
                                                                  (lens-set position-column-lens     _        1)))
                                (else                         (lens-transform position-column-lens   position add1))))))

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
                        state (curry cons (state-token state)))
        clear-token)
      state))

(define (left-parenthesis state escape)
  (let* ([state* (~> (whitespace state) copy-location-to-last-open-location)]
         [stack  (state-stack state*)])
        (cond
          ([false? stack] (lens-set       state-stack-lens state* '(())))
          ([list?  stack] (lens-transform state-stack-lens state* (curry cons empty)))
          (else                           (escape (set-error state* "Unable to process opening parenthesis"))))))

(define (right-parenthesis state escape)
  (let* ([state* (~> (whitespace state) (pop-previous-opening-parenthesis escape))]
         [stack  (state-stack state*)])
        (cond
          ([not (list? stack)]  (escape (set-error state* "Unmatched closing parenthesis")))
          ([= (length stack) 1] (move-stack-to-program state*))
          ([> (length stack) 1] (lens-transform state-stack-lens
                                                state*
                                                (lambda (x) (cons (cons (first x) (second x))
                                                                  (rest (rest x))))))
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
    (lens-set state-position-lens _ (state-position state))
    (lens-set state-unmatched-opening-parentheses-lens _ (state-unmatched-opening-parentheses state))))

(define (copy-location-to-last-open-location state)
  (lens-transform state-unmatched-opening-parentheses-lens
                  state
                  (lambda (x)
                          (cons (state-position state) x))))

(define (pop-previous-opening-parenthesis state escape)
  (lens-transform state-unmatched-opening-parentheses-lens
                  state
                  (lambda (x) (if (empty? x)
                                  (escape (set-error state "Unmatched closing parenthesis"))
                                  (rest x)))))
