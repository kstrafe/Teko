#! /usr/bin/env racket
#lang racket

(provide (all-defined-out))
(require "parse.rkt" "logger.rkt" "skeltal.rkt" lens threading syntax/parse/define)

(define (interpret program [env (make-hash)])
  (hash-set! env 'arguments empty)
  (hash-set! env 'calls empty)
  (eval program env)
  (trce env))

(define (eval program env)
  (trce program env)
  (if (empty? program)
    (void)
    (let ([x  (first program)]
          [xs (rest program)])
      (dbug x)
      (match x
        ("+" (hash-set! env 'return +)
             (eval xs env))
        ("*" (hash-set! env 'return *)
             (eval xs env))
        ([list "define" atom "return"] (hash-set! env atom (hash-ref env 'return))
                                       (eval xs env))
        ([list "define" atom value]    (eval (cons value (cons `("define" ,atom "return") xs)) env))
        ([list 'return=>arguments] (hash-set! env 'arguments
                                              (cons (hash-ref env 'return)
                                                    (hash-ref env 'arguments)))
                                   (eval xs env))
        ([list 'return=>calls]     (let ([tmp (cons (hash-ref env 'return)
                                                    (hash-ref env 'calls))])
                                     (hash-set! env 'calls tmp))
                                   (eval xs env))
        ([list 'call]         (let* ([calls (hash-ref env 'calls)]
                                     [call  (first calls)]
                                     [args  (hash-ref env 'arguments)])
                                    (hash-set! env 'calls (rest calls))
                                    (when (procedure? call) ; builtin?
                                      (hash-set! env 'return (apply call args))
                                      (hash-set! env 'arguments empty)
                                      (eval xs env))))
        ([list ''call arg ...] (let ([call (first (hash-ref env 'calls))])
                                   (when (procedure? call) ; builtin?
                                     (~>
                                       (for/fold ([xn (cons '(call) xs)])
                                                 ([a arg])
                                         (cons a (append (list '(return=>arguments)) xn)))
                                       (eval env)))))
        ([list expr arg ...] (eval (append (list expr '(return=>calls) `('call ,@arg))
                                           xs)
                                   env))
        (atom (cond
                ([all-digits? atom] (hash-set! env 'return (string->number atom))
                                    (eval xs env))
                (else (hash-set! env 'return (hash-ref env atom))
                      (eval xs env))
                ))
        ))))

(define (all-digits? string)
  (if (string? string)
      (andmap isdigit? (string->list string))
      #f))

(define (isdigit? character)
  (<= 48(char->integer character) 57))
