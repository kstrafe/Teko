#lang racket

(require "rkt-glfw/glfw.rkt"
         (for-syntax racket/syntax)
         (for-syntax racket/list)
         opengl
         lens
         syntax/parse/define)

(provide (all-defined-out))

(define (initialize-graphics)
  (glfwInit)
  (define window (glfwCreateWindow 800 600 "Example Window" #f #f))
  (glViewport 0 0 800 600)
  (glDisable GL_DEPTH_TEST)
  (glClearColor 0.3 0.3 0.3 0.5)
  (glfwSetInputMode window GLFW_STICKY_KEYS GL_TRUE)
  window)

(define (deinitialize-graphics window)
  (glfwDestroyWindow window)
  (glfwTerminate))

(define (display-clear-sleep-collect window collection-method)
  (glfwSwapBuffers window)  ; Automatically sleeps according to vsync
  (displayln (glGetError))
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT))
  (collection-method window))


(define-simple-macro (values->list expression:expr)
  (call-with-values (lambda () expression) list))

(define-syntax (prepend-glfw-and-upper identifier)
  (format-id (second (syntax->list identifier))
             "GLFW_~a"
             (second (syntax->datum identifier))))

(define (glfw-to-scheme-boolean in)
  (= GLFW_TRUE in))

(define-syntax (atfalse stx)
  #'#f)

(define-simple-macro (generate-collect-events name:id collector-name:id default-name:id (mouse:id ...) (key:id ...))
  (begin
    (struct/lens name (mouse ... key ... cursor-position window-size close-pressed? time) #:prefab)
    (define (default-name)
      (name (atfalse mouse) ...
            (atfalse key) ...
            '(0 0)
            '(0 0)
            #f
            0))
    (define (collector-name window)
      (define g glfw-to-scheme-boolean)
      (glfwPollEvents)
      (name (g (glfwGetMouseButton window (prepend-glfw-and-upper mouse))) ...
            (g (glfwGetKey window (prepend-glfw-and-upper key))) ...
            (values->list (glfwGetCursorPos window))
            (values->list (glfwGetWindowSize window))
            (g (glfwWindowShouldClose window))
            (current-inexact-milliseconds)))))
