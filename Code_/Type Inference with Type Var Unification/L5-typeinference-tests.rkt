#lang racket
(require "utils.rkt"
         "tenv.rkt"
         "L5-ast.rkt"
         "L5-typeinference.rkt")

(provide (all-defined-out))

;; Support exception handling
(define (try f (ans 'error))
  (with-handlers ((exn?
                   (lambda (exn) ans))) (f)))

(define type-inference-tests-annotated
  (lambda ()
    (letrec ((do (lambda (exp) (unparse-texp (typeof-exp (parseL5 exp) (make-empty-tenv))))))
      (display "type-inference-tests-annotated:\t")
      (run-tests
       (test (do 5) => 'number)
       (test (do #t) => 'boolean)
       
       (test (do '+) => '(number * number -> number))
       (test (do '-) => '(number * number -> number))
       (test (do '*) => '(number * number -> number))
       (test (do '/) => '(number * number -> number))
       (test (do '=) => '(number * number -> boolean))
       (test (do '<) => '(number * number -> boolean))
       (test (do '>) => '(number * number -> boolean))
       (test (do 'not) => '(boolean -> boolean))

       (test (typeof-exp (parseL5 'x) (extend-tenv (make-empty-tenv) '(x) '(num-te))) => 'num-te)

       (test (do '(if (> 1 2) 1 2)) => 'number)
       (test (do '(if (= 1 2) #t #f)) => 'boolean)

       (test (do '(lambda ([x : number]) : number x)) => '(number -> number))
       (test (do '(lambda ([x : number]) : boolean (> x 1))) => '(number -> boolean))

       (test (do '(lambda ([x : number]) : (number -> number) (lambda ([y : number]) : number (* y x))))
             =>
             '(number -> (number -> number)))

       (test (do '(lambda ([f : (number -> number)]) : number (f 2)))
             =>
             '((number -> number) -> number))

       (test (do '(let (([x : number] 1)) (* x 2))) => 'number)
       
       (test (do '(let (([x : number] 1)
                        ([y : number] 2))
                    (lambda ([a : number]) : number (+ (* x a) y))))
             =>
             '(number -> number))

       (test (do '(lambda ([x : number]) : number
                    (let (([y : number] x)) (+ x y))))
             =>
             '(number -> number))

       (test (do '(letrec (([p1 : (number -> number)] (lambda ([x : number]) : number (* x x))))
                    p1))
             => '(number -> number))

       (test (do '(letrec (([p1 : (number -> number)] (lambda ([x : number]) : number (* x x))))
                    (p1 2)))
             => 'number)

       (test (do '(letrec (([odd? : (number -> boolean)] (lambda ([n : number]) : boolean
                                                           (if (= n 0) #f (even? (- n 1)))))
                           ([even? : (number -> boolean)] (lambda ([n : number]) : boolean
                                                            (if (= n 0) #t (odd? (- n 1))))))
                    (odd? 12)))
             => 'boolean)

       (test (try (lambda ()
                    (do '(letrec ([[odd? : (boolean -> number)] (lambda ([n : number]) : boolean
                                                                  (if (= n 0) #f (even? (- n 1))))]
                                  [[even? : (boolean -> number)] (lambda ([n : number]) : boolean
                                                                   (if (= n 0) #t (odd? (- n 1))))])
                           (odd? 12)))))
             => 'error)
       
       ))))
       
(define typeof-exp-tests-polymorphic
  (lambda ()
    (letrec ((do (lambda (exp) (unparse-texp (typeof-exp (parseL5 exp) (make-empty-tenv))))))
      (display "typeof-exp-tests-polymorphic:\t")
      (run-tests
       (test (do '(lambda ([x : T1]) : T1 x)) => '(T1 -> T1))

       (test (do '(let (([x : number] 1))
                    (lambda ([y : T] [z : T]) : T (if (> x 2) y z))))
             =>
             '(T * T -> T))

       (test (do '(lambda () : number 1)) => '(Empty -> number))
       
       ))))



(define type-inference-tests-unannotated
  (lambda ()
    (letrec ((do (lambda (exp) (unparse-texp (typeof-exp (parseL5 exp) (make-empty-tenv))))))
      (display "type-inference-tests-unannotated:\t")
      (run-tests
       ;; Infer return type
       (test (do '(lambda ([x : number]) x)) => '(number -> number))
       ;; Infer param type
       (test (do '(lambda (x) : number x)) => '(number -> number))
       ;; Infer both
       (test (do '(lambda (x) (> x 1))) => '(number -> boolean))

       (test (do '(lambda (x) (lambda (y) (* y x))))
             =>
             '(number -> (number -> number)))

       (test (do '(let ((x 1)) (* x 2))) => 'number)
       
       (test (do '(let ((x 1)
                        (y 2))
                    (lambda (a) (+ (* x a) y))))
             =>
             '(number -> number))

       (test (do '(lambda (x) 
                    (let ((y x)) (+ x y))))
             =>
             '(number -> number))

       (test (do '(letrec ((p1 (lambda (x) (* x x))))
                    p1))
             => '(number -> number))

       (test (do '(letrec ((p1 (lambda (x) (* x x))))
                    (p1 2)))
             => 'number)

       (test (do '(lambda () 1)) => '(Empty -> number))
       
       (test (do '(letrec ((odd? (lambda (n)
                                   (if (= n 0) #f (even? (- n 1)))))
                           (even? (lambda (n)
                                    (if (= n 0) #t (odd? (- n 1))))))
                    (odd? 12)))
             => 'boolean)
       ))))
       
(define typeof-exp-tests-polymorphic-unannotated
  (lambda ()
    (letrec ((check (lambda (exp expected-te)
                      (equivalent-tes?
                       (typeof-exp (parseL5 exp) (make-empty-tenv))
                       (parse-texp expected-te)))))
      (display "typeof-exp-tests-polymorphic-unannotated:\t")
      (run-tests
       (test (check '(lambda (x) x)
                    '(T1 -> T1))
             => #t)

       (test (check '(lambda (f) (f 2))
                    '((number -> T) -> T))
             => #t)

       (test (check '(let ((x 1))
                       (lambda (y z) (if (> x 2) y z)))
                    '(T * T -> T))
             => #t)

       ;; this fails because our treatment of generics is limited
       (test (try (lambda () (check '(letrec ((id (lambda (x) x)))
                                       (if (id #t) (id 1) (id 2)))
                                    'bad-generic)))
             => 'error)
       ))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(type-inference-tests-annotated)
(typeof-exp-tests-polymorphic)
(type-inference-tests-unannotated)
(typeof-exp-tests-polymorphic-unannotated)