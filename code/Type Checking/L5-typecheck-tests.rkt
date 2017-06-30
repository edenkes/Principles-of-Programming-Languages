#lang racket
(require "utils.rkt"
         "tenv.rkt"
         "L5-ast.rkt"
         "L5-typecheck.rkt")

(provide (all-defined-out))

;; Support exception handling
(define (try f (ans 'error))
  (with-handlers ((exn?
                   (lambda (exn) ans))) (f)))

(define parse-texp-tests
  (lambda ()
    (display "parse-texp-tests:\t")
    (run-tests
     (test (parse-texp 'number) => 'num-te)
     (test (parse-texp 'boolean) => 'bool-te)
     (test (parse-texp 'T1) => '(tvar T1 #&#f))
     (test (parse-texp '(T * T -> boolean)) => '(proc-te ((tvar T #&#f) (tvar T #&#f)) bool-te))
     (test (parse-texp '(number -> (number -> number))) => '(proc-te (num-te) (proc-te (num-te) num-te)))
     (test (parse-texp 'void) => 'void-te)
     (test (parse-texp '(Empty -> void)) => '(proc-te () void-te))
     )))

(define unparse-texp-tests
  (lambda ()
    (display "unparse-texp-tests:\t")
    (run-tests
     (test (unparse-texp 'num-te) => 'number)
     (test (unparse-texp 'bool-te) => 'boolean)
     (test (unparse-texp '(tvar T1 #&#f)) => 'T1)
     (test (unparse-texp '(proc-te ((tvar T #&#f) (tvar T #&#f)) bool-te)) => '(T * T -> boolean))
     (test (unparse-texp '(proc-te (num-te) (proc-te (num-te) num-te))) => '(number -> (number -> number)))
     )))

(define parseL5-tests
  (lambda ()
    (display "parseL5-tests:\t")
    (run-tests
     (test (parseL5 '(define [x : number] 1)) => '(def-exp (var-decl x num-te) (num-exp 1)))
     (test (parseL5 '(lambda ([x : number]) : number x))
           =>
           '(proc-exp ((var-decl x num-te)) ((var-ref x)) num-te))
     )))

(define typeof-exp-tests
  (lambda ()
    (letrec ((do (lambda (exp) (unparse-texp (typeof-exp (parseL5 exp) (make-empty-tenv))))))
      (display "typeof-exp-tests:\t")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(parse-texp-tests)
(unparse-texp-tests)
(parseL5-tests)
(typeof-exp-tests)
(typeof-exp-tests-polymorphic)