#lang racket
(require "utils.rkt"
         "env.rkt"
         "L1-ast.rkt"
         "L1-eval.rkt")

(provide (all-defined-out))

;; Support exception handling
(define (try f (ans 'error))
  (with-handlers ((exn?
                   (lambda (exn) ans))) (f)))

(define parseL1-tests
  (lambda ()
    (display "parseL1-tests:\t")
    (run-tests
     (test (parseL1 '1) => '(num-exp 1))
     (test (parseL1 '#t) => '(bool-exp #t))
     (test (parseL1 '+) => '(prim-op +))
     (test (parseL1 'x) => '(var-ref x))
     (test (parseL1 '(+ 1 x)) => '(app-exp (prim-op +) ((num-exp 1) (var-ref x))))
     (test (parseL1 '(* (+ 1 x) 2)) => '(app-exp (prim-op *) ((app-exp (prim-op +) ((num-exp 1) (var-ref x))) (num-exp 2))))
     (test (parseL1 '(L1 (define x (+ 1 2)) (* x x))) =>
           '(program ((def-exp (var-decl x) (app-exp (prim-op +) ((num-exp 1) (num-exp 2))))
                      (app-exp (prim-op *) ((var-ref x) (var-ref x))))))
     )))


(define env-tests
  (lambda ()
    (display "env-tests:\t")
    (run-tests
     (test (try (lambda () (apply-env (make-empty-env) 'x))) => 'error)
     (test (apply-env (extend-env (make-empty-env) '(x) '(1)) 'x) => 1)
     (test (try (lambda () (apply-env (extend-env (make-empty-env) '(y) '(1)) 'x))) => 'error)
     (test (apply-env (extend-env (extend-env (make-empty-env) '(x) '(1)) '(y) '(2)) 'x) => 1)
     (test (apply-env (extend-env (extend-env (make-empty-env) '(x) '(1)) '(x) '(2)) 'x) => 2)
     )))

            
(define eval-L1-tests
  (lambda ()
    (display "eval-L1-tests:\t")
    (let ((ge (make-empty-env)))
      (run-tests
       (test (L1-applicative-eval (parseL1 '1) ge) => 1)
       (test (L1-applicative-eval (parseL1 '#t) ge) => #t)
       (test (L1-applicative-eval (parseL1 'x) (extend-env ge '(x) '(1))) => 1)
       (test (L1-applicative-eval (parseL1 '+) ge) => '(prim-op +))
       (test (L1-applicative-eval (parseL1 '(+ 1 2)) ge) => 3)
       (test (L1-applicative-eval (parseL1 '(> 2 1)) ge) => #t)
       (test (L1-applicative-eval (parseL1 '(not (> 2 1 0))) ge) => #f)
       (test (L1-applicative-eval (parseL1 '(+ (* 2 2) 3)) ge) => 7)
     ))))

(define eval-program-L1-tests
  (lambda ()
    (display "eval-program-L1-tests:\t")
    (run-tests
     ;(test (eval-L1-program (parseL1 '(L1 (define x (+ 3 2))
      ;                                 (* x x)))) => 25)
     ;(test (eval-L1-program (parseL1 '(L1 (define x 1)))) => (void))
     (test (eval-L1-program (parseL1 '(L1 (define x 3) (* x x) (+ x x)))) => 6)
     ;(test (eval-L1-program (parseL1 '(L1 (define x 3) (not (> x 2))))) => #f)
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(parseL1-tests)
(env-tests)
(eval-L1-tests)
(eval-program-L1-tests)