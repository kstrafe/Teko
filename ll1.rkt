#! /usr/bin/env racket
#lang racket

(require "logger.rkt" "parse.rkt" "remove-comments.rkt" "interpreter.rkt" lens threading)

(~>
  (parse-file "input")
  remove-comments
  interpret
  trce*)
