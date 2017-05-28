#lang racket

(require "data-structures.rkt" "logger.rkt" lens opengl)

(provide simulate-game)

(define (simulate-game state)
  (let ([size (events-window-size (game-events state))])
    (if (not (equal? size
                       (game-old-window-size state)))
      (begin
        (trce "Resizing viewport to" size)
        (glViewport 0 0 (first size) (second size))
        (lens-set game-old-window-size-lens state size))
      state)))
