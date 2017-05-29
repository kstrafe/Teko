#lang racket

(provide (all-defined-out))

(require lens syntax/parse/define)

(define-simple-macro (skeltals (name:id (attributes:id ...)) ...)
  (begin (struct/lens name (attributes ...) #:prefab) ...))

(define-simple-macro (skeltal-default (name:id (attributes:id default:expr) ...))
  (begin
    (struct/lens name (attributes ...) #:prefab)
    (define ((format-id #'name "~a-default" name))
      (name default ...))))
