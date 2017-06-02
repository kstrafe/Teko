#! /usr/bin/env racket
#lang racket

(provide interpret)

(require "logger.rkt" "skeltal.rkt"
         lens threading syntax/parse/define)

(skeltals (fn (parameters code))
          (mo (parameter code)))

; Euler's number
(define e 2.7182818284590452353602874713527)

; This regex can be derived from grammar 2 in the Teko Language Specification
(define number-regex #px"((\\+|\\-)?((([0-9][0-9]*)/([0-9][0-9]*)|([0-9][0-9]*)\\.([0-9][0-9]*)|([0-9][0-9]*)\\.|\\.([0-9][0-9]*)|([0-9][0-9]*))(e(([0-9][0-9]*)))?)((\\+|\\-)(((([0-9][0-9]*)/([0-9][0-9]*)|([0-9][0-9]*)\\.([0-9][0-9]*)|([0-9][0-9]*)\\.|\\.([0-9][0-9]*)|([0-9][0-9]*))(e(([0-9][0-9]*)))?))i)?)")

; Re-entrant interpret function as given by equation (1)
(define (interpret program [env (make-hash)])
  ; Allocate the parameter, return value, and call stack in the environment
  (hash-set! env '@params empty)
  (hash-set! env '@return empty)
  (hash-set! env '@calls empty)

  ; Set up the environment by binding functions and constants
  (define-simple-macro (racket-hash-sets! hash symbol ...)
    (begin (hash-set! hash (symbol->string 'symbol) (list symbol)) ...))
  (define-simple-macro (racket-hash-sets-apetail! hash symbol ...)
    (begin (hash-set! hash (string-append "@" (symbol->string 'symbol)) (list symbol)) ...))
  (racket-hash-sets! env #| Numeric Operations |#
                         * + - / < <= = >= >= cos exact->inexact exp expt inexact->exact modulo real->double-flonum
                         real->single-flonum round sin sqrt
                         #| Number Classes |#
                         complex? double-flonum? even? exact-integer? exact-nonnegative-integer? exact-positive-integer?
                         exact? fixnum? flonum? inexact-real? inexact? integer? negative? number? odd? positive?
                         single-flonum? rational? real?  zero?
                         #| Constants |#
                         e pi
                         #| List Functions |#
                         andmap append drop empty? filter first foldl foldr last list? map ormap take rest
                         #| String Function |#
                         string-append
                         write read max min read-line read-string  equal? bitwise-and bitwise-xor bitwise-ior
                         #| Miscellaneous |#
                         exit)
  (racket-hash-sets-apetail! env sleep)

  (eval program env)
  (hash-ref env '@return))

(define (hash-ref2 hash key)
  (if (hash-has-key? hash key)
      (hash-ref hash key)
      (void)))

(define (eval program env)
  (if (empty? program)
    (void)
    (let ([x  (first program)]
          [xs (rest program)])
      (trce program (hash-ref2 env "x") (hash-ref2 env "y"))
      (match x
        ([list "\"" expr ...]                 (let-values ([(sum _)
                                                (for/fold ([sum ""]
                                                           [prev-expr? #f])
                                                          ([expr_i expr])
                                                  (match expr_i
                                                    ([list C] (values (string-append sum (string (integer->char (string->number C)))) #t))
                                                    (word (values (string-append sum (if (and (not prev-expr?) (non-empty-string? sum)) " " "") word) #f))))])
                                                (hash-set! env '@return sum))
                                              (eval xs env))

        ([list "mo" atom expr ...]            (hash-set! env '@return (mo atom expr))
                                              (eval xs env))

        ([list "fn" (list atom ...) expr ...] (hash-set! env '@return (fn atom expr))
                                              (eval xs env))

        ([list "define" atom value]           (eval (cons value
                                                          (cons `(define ,atom @return)
                                                                xs))
                                              env))

        ([list 'define atom '@return]         (hash-set! env atom (list (hash-ref env '@return)))
                                              (eval xs env))

        ([list "if" test then otherwise]      (eval (append (list test `(if @return ,then ,otherwise)) xs) env))

        ([list 'if '@return then otherwise]   (if (false? (hash-ref env '@return))
                                                  (eval (append (list otherwise) xs) env)
                                                  (eval (append (list then) xs) env)))

        ([list 'set! atom '@return]           (hash-set! env atom (cons (hash-ref env '@return) (rest (hash-ref env atom))))
                                              (eval xs env))

        ([list "set!" atom value]             (eval (cons value
                                                          (cons `(set! ,atom @return)
                                                                xs))
                                              env))

        ([list '@return=>@params]             (let ([args (hash-ref env '@params)])
                                                   (hash-set! env '@params
                                                              (cons
                                                                (cons (hash-ref env '@return) (first args))
                                                                (rest args))))
                                              (eval xs env))

        ([list '@pop-params arg ...]          (for ([i arg])
                                                   (let ([h (hash-ref env i)])
                                                        (hash-set! env i (rest h))))
                                              (eval xs env))

        ([list '@return=>@calls]              (let ([tmp (cons (hash-ref env '@return)
                                                               (hash-ref env '@calls))]
                                                    [args (hash-ref env '@params)])
                                                   (hash-set! env '@calls tmp)
                                                   (eval xs env)))

        ([list '@do-call]                     (let* ([@calls (hash-ref env '@calls)]
                                                     [call  (first @calls)]
                                                     [args  (hash-ref env '@params)])
                                                    (hash-set! env '@calls (rest @calls))
                                                    (cond
                                                      ([procedure? call] ; builtin?
                                                        (hash-set! env '@return (apply call (first args)))
                                                        (hash-set! env '@params (rest args))
                                                        (eval xs env))
                                                      ([mo? call] ; Macro call?
                                                        (exit)
                                                      )
                                                      ([fn? call] ; User-defined?
                                                        (if (= (length (fn-parameters call))
                                                               (length (first args)))
                                                          (begin
                                                            (let ([args (hash-ref env '@params)])
                                                              (hash-set! env '@params (rest args)))
                                                            (if (or (empty? (fn-parameters call))
                                                                    (and (not (empty? xs))
                                                                         (list? (first xs))
                                                                         (not (empty? (first xs)))
                                                                         (symbol? (first (first xs)))
                                                                         (symbol=? (first (first xs)) '@pop-params)
                                                                         (andmap string=? (fn-parameters call) (rest (first xs)))))
                                                              (begin
                                                                (for ([f (fn-parameters call)]
                                                                      [i (first args)])
                                                                  (if (hash-has-key? env f)
                                                                    (let ([old (hash-ref env f)])
                                                                      (hash-set! env f (cons i (rest old))))
                                                                    (hash-set! env f (cons i empty))))
                                                                (eval (append (fn-code call) xs) env))
                                                              (begin
                                                                (for ([f (fn-parameters call)]
                                                                      [i (first args)])
                                                                  (if (hash-has-key? env f)
                                                                    (let ([old (hash-ref env f)])
                                                                      (hash-set! env f (cons i old)))
                                                                    (hash-set! env f (cons i empty))))
                                                                (eval (append (fn-code call) `((@pop-params ,@(fn-parameters call))) xs) env))))
                                                          (hash-set! env '@return "Incorrect number of arguments to function")
                                                        )))))

        ([list '@call arg ...]               (let ([call (first (hash-ref env '@calls))])
                                                  (cond
                                                    ([or (procedure? call) (fn? call)] ; builtin?
                                                      (let ([args (hash-ref env '@params)])
                                                        (hash-set! env '@params (cons empty args)))
                                                      (~>
                                                        (for/fold ([xn (cons '(@do-call) xs)])
                                                                  ([a arg])
                                                          (cons a (append (list '(@return=>@params)) xn)))
                                                        (eval env))))))

        ([list expr arg ...]                  (eval (append (list expr '(@return=>@calls) `(@call ,@arg)) xs) env))

        (atom                                 (cond
                                                ([all-digits? atom] (hash-set! env '@return (string->number atom))
                                                                    (eval xs env))
                                                ([and (string? atom) (regexp-match-exact? number-regex atom)]
                                                                    (hash-set! env '@return (string->number atom))
                                                                    (eval xs env))
                                                (else (hash-set! env '@return (first (hash-ref env atom)))
                                                      (eval xs env))
                                                ))))))

(define (all-digits? string)
  (if (string? string)
      (andmap isdigit? (string->list string))
      #f))

(define (isdigit? character)
  (<= 48(char->integer character) 57))
