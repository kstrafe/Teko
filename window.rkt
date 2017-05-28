#! /usr/bin/env racket

#lang racket

(require "data-structures.rkt"
         "logger.rkt"
         "game.rkt"
         "rkt-glfw/glfw.rkt"
         "utilities.rkt"
         lens
         opengl
         threading)

(define (main)
  (define window (initialize-graphics))
  (let loop ([state (initial-game-state)])
    (when (not (events-close-pressed? (game-events state)))
      (~>
        (lens-set game-events-lens state
                  (display-clear-sleep-collect window collect-events))
        simulate-game
        trce*
        loop)))
  (deinitialize-graphics window))

(main)
