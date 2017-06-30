#lang racket
(require "utils.rkt"
         "env-closure.rkt"
         "env-box.rkt"
         "env-ast.rkt"
         "env-box-eval.rkt")

(provide (all-defined-out))

; missing test for exception
; Support exception handling
(define (try f (ans 'error))
  (with-handlers ((exn?
                   (lambda (exn) ans))) (f)));;



(define env-box-eval-tests
  (lambda ()
    (display "env-box-eval-tests:\t")
    (let ((ge the-global-env))
      (run-tests
 (test (env-box-eval (parseL4 '((lambda ((a lazy)) 1) (/ 1 0))) ge) => '1)
       (test (env-box-eval (parseL4 '((lambda (y) (y (/ 1 0))) (lambda ((x lazy)) 1))) ge) => '1)
       (test (env-box-eval (parseL4 '((lambda ((x lazy) y) (y x)) (/ 1 0) (lambda ((x lazy)) 1))) ge) => '1)
       (test (env-box-eval (parseL4 '((lambda (x) (x (/ 1 0))) (lambda ((x lazy)) ((lambda ((x lazy)) 1) (/ 1 0))))) ge) => '1)
       (test (env-box-eval (parseL4 '(((lambda ()(lambda ((x lazy)) 1))) (/ 1 0))) ge) => '1)
       
       (test (env-box-eval (parseL4 '((lambda ((x lazy)) 1) (/ 1 1))) ge) => '1)
       (test (env-box-eval (parseL4 '((lambda ((x lazy) (b lazy) c) c) (/ 1 0) (/ 1 0) 1)) ge) => '1)
       (test (env-box-eval (parseL4 '((lambda ((x lazy) y) (if ( = y 0) y x)) (/ 1 0) 0)) ge) => '0)
       (test (env-box-eval (parseL4 '((lambda ((x lazy)) ((lambda ((x lazy)) 0) (/ 1 0))) (/ 1 0))) ge) => '0)
       (test (env-box-eval (parseL4 '(((lambda ()(lambda ((x lazy)) 1))) (/ 1 0))) ge) => '1)
       (test (env-box-eval (parseL4 '((lambda ((x lazy)) 1) (/ 1 0))) ge) => '1) ; basic test
       (test (env-box-eval-program (parseL4 '(L4 (define factorial (lambda (fact) ; check that we didn't break existing code
       (if ( = 0 fact) 1 (* fact (factorial (- fact 1)))))) (factorial 5)))) => '120)
       (test (env-box-eval (parseL4 '((lambda ((x lazy)) x) 5)) ge) => '5) ; check that we evaluate var-refs
       (test (env-box-eval (parseL4 '((lambda ((x lazy)) ((lambda ((y lazy)) 5) x)) (/ 1 0))) ge) => '5) ; make sure successive lazy invocations work



       (test (env-box-eval (parseL4 '((lambda ((x lazy)) ((lambda ((y lazy)) 5) x)) (/ 1 0))) ge) => '5) ; make sure successive lazy invocations work

       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(env-box-eval-tests)

;

