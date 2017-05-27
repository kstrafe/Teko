#lang racket

(provide (all-defined-out))

(require lens syntax/parse/define)

(define-simple-macro (skeltals (name:id (attributes:id ...)) ...)
  (begin (struct/lens name (attributes ...) #:prefab) ...))
