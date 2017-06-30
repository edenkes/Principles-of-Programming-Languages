#lang racket

(require "utils.rkt"
         "L5-ast.rkt"
         "L5-substitution-adt.rkt"
         "L5-type-equations.rkt")


;; (require srfi/1)
(provide (all-defined-out))

;; Support exception handling
(define (try f (ans 'error))
  (with-handlers ((exn?
                   (lambda (exn) ans))) (f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Signature: verify-te-of-expr(expr te)
;; Purpose:   (1) Map type-vars to sub expressions in expr (expr-tvars-list).
;;            (2) Let tvar-of-expr be the type-var associated with expr.
;;            (3) Generate type equations based on the expr-tvar map.
;;            (4) Solve the equations.
;;            (5) Find the type-expression associated with tvar-of-expr.
;;            (6) See if it is equivalent to the given expression, expr.
;;
(define verify-te-of-expr
  (lambda (exp tec)
    (let* ((expr (parseL5 exp))
           (te (parse-texp tec))
           (pool (L5-exp->pool expr))
           (equations  (pool->equations pool))
           (solve-sub  (solve-equations equations))
           (tvar-of-expr (second (assoc expr pool)))
           (type-of-expr (sub-apply solve-sub tvar-of-expr)))
      (equivalent-tes? te type-of-expr))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define solve-tests
  (lambda ()
    (display "solve-tests:\t")
    (run-tests

     ;; Equations: T1 = T3, Substitution: Empty
     (test (solve (list (make-equation (make-tvar 'T_1) (make-tvar 'T_3)))
                  (make-empty-sub))
           => (make-sub (map parse-texp '(T_1))
                        (map parse-texp '(T_3))))

     (test (verify-te-of-expr 3 'number)
           => #t)
     (test (verify-te-of-expr '(+ 1 2) 'number))
     (test (verify-te-of-expr '(+ (+ 1 2) 3) 'number))
     (test (verify-te-of-expr '+ '[number * number -> number]))
     (test (verify-te-of-expr '> '[number * number -> boolean]))
     (test (verify-te-of-expr '(> 1 2) 'boolean))
     (test (verify-te-of-expr '(> 1 (+ 1 2)) 'boolean))

     ;; Type of '(lambda (x) (+ x 1)) is '(number -> number)
     (test (verify-te-of-expr
            '(lambda (x) (+ x 1))
            '[number -> number])
           => #t)

     ;; Type of '((lambda (x) (+ x 1)) 3) is 'number
     (test (verify-te-of-expr
            '((lambda (x) (+ x 1)) 3)
            'number)
           => #t)

     ;; Verify that the type of '((lambda (x) (+ x 1)) 3) is equivalent
     ;; to `(-> (* (-> (* number) T)) T), where T is some type variable
     (test (verify-te-of-expr
            '(lambda (x) (x 11))
            '[(number -> T) -> T])
           => #t)

     ;; g: [T1->T2]
     ;; f: [T2->T3]
     ;; ==> (lambda(n) (f (g n)))               : [T1->T3]
     ;; ==> (lambda(f g) (lambda(n) (f (g n)))) : [[T2-T3]*[T1->T2]->[T1->T3]]
     (test (verify-te-of-expr
            '(lambda (f g) (lambda (n) (f (g n))))
            '[(T2 -> T3) * (T1 -> T2) -> (T1 -> T3)])
           => #t)

     ;; f: [N->N]
     ;; ==> (lambda(x) (- (f 3) (f x)))             : [N->N]
     ;; ==> (lambda(f) (lambda(x) (- (f 3) (f x)))) : [[N->N]->[N->N]]
     (test (verify-te-of-expr
            '(lambda (f) (lambda (x) (- (f 3) (f x))))
            '[(number -> number) -> (number -> number)])
           => #t)

     (test (verify-te-of-expr
            '(lambda (x) (+ (+ x 1) (+ x 1)))
            '[number -> number])
           => #t)

     (test (verify-te-of-expr
            '(lambda () (lambda (x) (+ (+ x 1) (+ x 1))))
            '[Empty -> (number -> number)])
           => #t)

     (test (verify-te-of-expr
            '((lambda (x) (x 11 3)) +)
            'number)
           => #t)

     ;; (lambda (y) y):      [T1 -> T1]
     ;; (lambda (x) (x 11)): [N  -> T2]
     (test (verify-te-of-expr
            '((lambda (x) (x 11)) (lambda (y) y))
            'number)
           => #t)

     ;; Circular types cannot be inferred 
     (test (try
            (lambda ()
              (verify-te-of-expr
               '(lambda (x) (x x))
               'Circular)))
           => 'error)

     ;; A free variable cannot have type inferred
     (test (verify-te-of-expr 'x 'T)
           => #t)

     ;; A free variable whose type is inferred from context
     (test (verify-te-of-expr '(+ x 1) 'number)
           => #t)

     ;; Not enough info in context to infer type of f
     (test (verify-te-of-expr '(f 1) 'T)
           => #t)

     ;; Primitive provides sufficient info
     (test (verify-te-of-expr '(> (f 1) 0) 'boolean)
           => #t)

     ;; Parameters that are not used
     (test (verify-te-of-expr '(lambda (x) 1) '(T -> number))
           => #t)
     (test (verify-te-of-expr '(lambda (x y) x) '(T1 * T2 -> T1))
           => #t)
     (test (verify-te-of-expr '((lambda (x) 1) 2) 'number)
           => #t)

     ;; Bad number of parameters
     ;; Extra param
     (test
      (try
       (lambda ()
         (verify-te-of-expr '((lambda () 1) 2) 'bad-arity)))
       => 'error)
     ;; Missing param
     (test
      (try
       (lambda ()
         (verify-te-of-expr '((lambda (x) 1))) 'bad-arity))
      => 'error)
     
     )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(solve-tests)
