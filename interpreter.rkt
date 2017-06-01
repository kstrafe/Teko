#! /usr/bin/env racket
#lang racket

(provide (all-defined-out))
(require "parse.rkt" "logger.rkt" "skeltal.rkt" lens threading syntax/parse/define)

(skeltals (fn (parameters code))
          (mo (parameter code)))

(define (interpret program [env (make-hash)])
  (hash-set! env 'arguments empty)
  (hash-set! env 'calls empty)
  (eval program env)
  (trce env))

(define (eval program env)
  (if (empty? program)
    (void)
    (let ([x  (first program)]
          [xs (rest program)])
      (trce x xs env)
      (match x
        ("sleep" (hash-set! env 'return sleep)
             (eval xs env))
        ("+" (hash-set! env 'return +)
             (eval xs env))
        ("*" (hash-set! env 'return *)
             (eval xs env))
        ("/" (hash-set! env 'return /)
             (eval xs env))
        ("-" (hash-set! env 'return -)
             (eval xs env))
        ("<" (hash-set! env 'return <)
             (eval xs env))
        (">" (hash-set! env 'return >)
             (eval xs env))
        ("=" (hash-set! env 'return =)
             (eval xs env))
        ([list "mo" atom expr ...]     (hash-set! env 'return (mo atom expr))
                                       (eval xs env))
        ([list "fn" (list atom ...) expr ...]
                                       (hash-set! env 'return (fn atom expr))
                                       (eval xs env))
        ([list "define" atom "return"] (hash-set! env atom (list (hash-ref env 'return)))
                                       (eval xs env))
        ([list "if" "return" then otherwise] (if (zero? (hash-ref env 'return))
                                               (eval (append (list otherwise) xs) env)
                                               (eval (append (list then) xs) env)))
        ([list "if" test then otherwise] (eval (append (list test `("if" "return" ,then ,otherwise)) xs) env))
        ([list "set!" atom "return"]   (hash-set! env atom (cons (hash-ref env 'return) (rest (hash-ref env atom))))
                                       (eval xs env))
        ([list "set!" atom value]      (eval (cons value
                                                   (cons `("set!" ,atom "return")
                                                         xs))
                                             env))
        ([list "define" atom value]    (eval (cons value
                                                   (cons `("define" ,atom "return")
                                                         xs))
                                             env))
        ([list 'return=>arguments] (let ([args (hash-ref env 'arguments)])
                                     (hash-set! env 'arguments
                                                (cons
                                                  (cons (hash-ref env 'return) (first args))
                                                  (rest args))))
                                   (eval xs env))
        ([list 'pop-arguments function arg ...] (for ([i arg])
                                         (let ([h (hash-ref env i)])
                                           (hash-set! env i (rest h))))
                                       (eval xs env))
        ([list 'return=>calls]     (let ([tmp (cons (hash-ref env 'return)
                                                    (hash-ref env 'calls))]
                                         [args (hash-ref env 'arguments)])
                                     (hash-set! env 'calls tmp)
                                   (eval xs env)))
        ([list 'call]         (let* ([calls (hash-ref env 'calls)]
                                     [call  (first calls)]
                                     [args  (hash-ref env 'arguments)])
                                    (hash-set! env 'calls (rest calls))
                                    (cond
                                      ([procedure? call] ; builtin?
                                        (hash-set! env 'return (apply call (first args)))
                                        (hash-set! env 'arguments (rest args))
                                        (eval xs env))
                                      ([fn? call] ; User-defined?
                                        (if (= (length (fn-parameters call))
                                               (length (first args)))
                                          (begin
                                            (let ([args (hash-ref env 'arguments)])
                                              (hash-set! env 'arguments (rest args)))
                                            (if (and (not (empty? xs))
                                                     (list? (first xs))
                                                     (not (empty? (first xs)))
                                                     (symbol? (first (first xs)))
                                                     (symbol=? (first (first xs)) 'pop-arguments)
                                                     (equal? call (second (first xs))))
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
                                                (eval (append (fn-code call) `((pop-arguments ,call ,@(fn-parameters call))) xs) env))))
                                          (exit)
                                        )))))
        ([list ''call arg ...] (let ([call (first (hash-ref env 'calls))])
                                  (cond
                                    ([or (procedure? call) (fn? call)] ; builtin?
                                      (let ([args (hash-ref env 'arguments)])
                                        (hash-set! env 'arguments (cons empty args)))
                                      (~>
                                        (for/fold ([xn (cons '(call) xs)])
                                                  ([a arg])
                                          (cons a (append (list '(return=>arguments)) xn)))
                                        (eval env))))))
        ([list expr arg ...] (eval (append (list expr '(return=>calls) `('call ,@arg))
                                           xs)
                                   env))
        (atom (cond
                ([all-digits? atom] (hash-set! env 'return (string->number atom))
                                    (eval xs env))
                (else (hash-set! env 'return (first (hash-ref env atom)))
                      (eval xs env))
                ))
        ))))

(define (all-digits? string)
  (if (string? string)
      (andmap isdigit? (string->list string))
      #f))

(define (isdigit? character)
  (<= 48(char->integer character) 57))
