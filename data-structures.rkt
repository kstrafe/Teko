#lang racket

(require "logger.rkt"
         "rkt-glfw/glfw.rkt"
         "utilities.rkt"
         lens
         opengl
         threading)

(provide (all-defined-out))

(generate-collect-events events collect-events default-events-state
                         (MOUSE_BUTTON_LEFT)
                         (KEY_W KEY_A KEY_S KEY_D))

(struct/lens game (events old-window-size) #:prefab)
(define (initial-game-state)
  (game (default-events-state) empty))
