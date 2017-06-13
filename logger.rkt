#lang racket

;;;; Provides loggers to standard error ports. The loggers automatically write the expressions and their results.
;;;; This means executing `(trce (+ 1 2 3))` writes `(trce time (source line column) (+ 1 2 3) = 6)`.
;;;; Appending a star to the logger name prevents writing the expression: `(trce* (+ 1 2 3))` writes
;;;; `(trce time (source line column) _ = 6)`.
;;;; The output can be read using `read`. It is printed using `pretty-write`. The time format is set to
;;;; iso-8601 using date-display-format, so it may interfere with your date tools.

(provide trce  dbug  info  warn  erro  crit  ftal
         trce* dbug* info* warn* erro* crit* ftal)

(require racket/date racket/syntax (for-syntax racket/syntax syntax/parse))

(define (get-time)
  (string->symbol (date->string (current-date) #t)))

(define (get-source stx)
  (match (syntax-source stx)
    ('readline-input 'terminal)
    (#f 'unknown)  ; #f in the case of running racket -e
    (final (if (path? final) (path->string final) 'unknown))))

(define-syntax (base stx)
  (syntax-parse stx
    [(_ name:id)
      #'(begin
        (pretty-write `(name ,(get-time) (,(get-source #'name) ,(syntax-line #'name) ,(syntax-column #'name))) (current-error-port)))]
    [(_ name:id expr:expr ...+)
      #'(begin
        (pretty-write `(name #|,(get-time) (,(get-source #'expr) ,(syntax-line #'expr) ,(syntax-column #'expr))|# expr = ,expr) (current-error-port)) ...)]))

(define-syntax (base-no-print stx)
  (syntax-parse stx
    [(_ name:id expr:expr)
      #'(begin
        (let ([result expr])
          (pretty-write `(name ,(get-time) (,(get-source #'expr) ,(syntax-line #'expr) ,(syntax-column #'expr)) _ = ,result) (current-error-port)) result) )]))

(define-syntax (make-loggers stx)
  (syntax-parse stx
    [(_ name:id ...+)
      (let ([loggers (syntax-e #'(name ...))])
        (with-syntax* ([(rename ...) (for/list ([name-e (syntax-e #'(name ...))]) (format-id stx "~a*" name-e))])
          (syntax-protect
            #'(begin
              (date-display-format 'iso-8601)
              (begin
                (define-syntax (name (... stx))
                  (syntax-parse (... stx)
                    (... [(call:id whatever:expr ...)
                      #'(base call whatever ...)])))
                (define-syntax (rename (... stx))
                  (syntax-parse (... stx)
                    (... [(call:id whatever:expr ...)
                      (with-syntax ([caller-id (datum->syntax #'call 'name #'call)])
                        #'(base-no-print caller-id whatever ...))])))) ...))))]))

(make-loggers trce dbug info warn erro crit ftal)
