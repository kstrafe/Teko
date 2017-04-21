#! /usr/bin/env racket
#lang racket

(require lens threading "parse.rkt")

(define in (string->list (file->string "input")))

(define (parse-string string [state empty-state])
  (foldl parse state string))

(parse-string in)
