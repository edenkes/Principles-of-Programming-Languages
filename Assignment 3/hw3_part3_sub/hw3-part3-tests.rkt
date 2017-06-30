#lang racket
(require "utils.rkt"
         "env.rkt"
         "L1-ast.rkt"
         "L2-ast.rkt"
         "L3-ast.rkt"
         "L2-eval.rkt"
         "L3-eval.rkt")


(define tests
  (lambda ()
    (display "tests:\t")
    (let ((ge (make-empty-env)))
      (run-tests
       (test (L3-applicative-eval (parseL3 '((lambda ((a lazy)) 1) (/ 1 0))) ge) => '1)
       (test (L3-applicative-eval (parseL3 '((lambda (y) (y (/ 1 0))) (lambda ((x lazy)) 1))) ge) => '1)
       (test (L3-applicative-eval (parseL3 '((lambda ((x lazy) y) (y x)) (/ 1 0) (lambda ((x lazy)) 1))) ge) => '1)
       (test (L3-applicative-eval (parseL3 '((lambda (x) (x (/ 1 0))) (lambda ((x lazy)) ((lambda ((x lazy)) 1) (/ 1 0))))) ge) => '1)
       (test (L3-applicative-eval (parseL3 '(((lambda ()(lambda ((x lazy)) 1))) (/ 1 0))) ge) => '1)
       
       (test (L3-applicative-eval (parseL3 '((lambda ((x lazy)) 1) (/ 1 1))) ge) => '1)
       (test (L3-applicative-eval (parseL3 '((lambda ((x lazy) (b lazy) c) c) (/ 1 0) (/ 1 0) 1)) ge) => '1)
       (test (L3-applicative-eval (parseL3 '((lambda ((x lazy) y) (if ( = y 0) y x)) (/ 1 0) 0)) ge) => '0)
       (test (L3-applicative-eval (parseL3 '((lambda ((x lazy)) ((lambda ((x lazy)) 0) (/ 1 0))) (/ 1 0))) ge) => '0)
       (test (L3-applicative-eval (parseL3 '(((lambda ()(lambda ((x lazy)) 1))) (/ 1 0))) ge) => '1)
       (test (L3-applicative-eval (parseL3 '((lambda ((x lazy)) 1) (/ 1 0))) ge) => '1) ; basic test
       (test (eval-L3-program (parseL3 '(L3 (define factorial (lambda (fact) ; check that we didn't break existing code
       (if ( = 0 fact) 1 (* fact (factorial (- fact 1)))))) (factorial 5)))) => '120)
       (test (L3-applicative-eval (parseL3 '((lambda ((x lazy)) x) 5)) ge) => '5) ; check that we evaluate var-refs
       (test (L3-applicative-eval (parseL3 '((lambda ((x lazy)) ((lambda ((y lazy)) 5) x)) (/ 1 0))) ge) => '5) ; make sure successive lazy invocations work

       ))))

(tests)

 ; (index-of '(1 2 3) 2)
