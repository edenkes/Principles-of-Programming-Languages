#lang racket
(require "utils.rkt"
         "env.rkt"
         "L1-ast.rkt"
         "L2-ast.rkt"
         "L3-ast.rkt"
         "L2-eval.rkt"
         "L3-eval.rkt"
         "L3-normal.rkt")

(provide (all-defined-out))

;; Support exception handling
(define (try f (ans 'error))
  (with-handlers ((exn?
                   (lambda (exn) ans))) (f)))

(define normal-L3-tests
  (lambda ()
    (display "normal-L3-tests:\t")
    (let ((ge (make-empty-env)))
      (run-tests
       (test (L3-normal-eval (parseL3 '1) ge) => 1)
       (test (L3-normal-eval (parseL3 '#t) ge) => #t)
       (test (L3-normal-eval (parseL3 'x) (extend-env ge '(x) '(1))) => 1)
       (test (L3-normal-eval (parseL3 '+) ge) => '(prim-op +))
       (test (L3-normal-eval (parseL3 '(+ 1 2)) ge) => 3)
       (test (L3-normal-eval (parseL3 '(> 2 1)) ge) => #t)
       (test (L3-normal-eval (parseL3 '(not (> 2 1))) ge) => #f)
       (test (L3-normal-eval (parseL3 '(+ (* 2 2) 3)) ge) => 7)

       ;; L2 syntactic forms
       (test (L3-normal-eval (parseL3 '(if (> 2 1) 3 -3)) ge) => 3)
       (test (L3-normal-eval (parseL3 '(lambda (x) x)) ge) => '(closure ((var-decl x)) ((var-ref x))))

       ;; L3 syntactic forms
       (test (L3-normal-eval (parseL3 '(cons 1 '())) ge) => '(1))
       (test (L3-normal-eval (parseL3 '(car '(1 . 2))) ge) => 1)
       (test (L3-normal-eval (parseL3 '(cdr '(1 2))) ge) => '(2))
       (test (L3-normal-eval (parseL3 '(number? 'x)) ge) => #f)
       (test (L3-normal-eval (parseL3 '(symbol? 'x)) ge) => #t)
       (test (L3-normal-eval (parseL3 '(list? (cons 1 2))) ge) => #f)
       (test (L3-normal-eval (parseL3 '(pair? (cons 1 2))) ge) => #t)
       (test (L3-normal-eval (parseL3 '(boolean? #t)) ge) => #t)
       (test (L3-normal-eval (parseL3 '(eq? 'x 'x)) ge) => #t)

       ))))

(define normal-program-L3-tests
  (lambda ()
    (display "normal-program-L3-tests:\t")
    (run-tests
     (test (normal-L3-program (parseL3 '(L3 (define x (+ 3 2))
                                            (* x x)))) => 25)
     (test (normal-L3-program (parseL3 '(L3 (define x 1)))) => (void))
     (test (normal-L3-program (parseL3 '(L3 (define x 3) (* x x) (+ x x)))) => 6)
     (test (normal-L3-program (parseL3 '(L3 (define x 3) (not (> x 2))))) => #f)

     ;; Procedure application
     (test (normal-L3-program (parseL3 '(L3 (define f (lambda (x) (* x x))) (f 3)))) => 9)
     (test (normal-L3-program (parseL3 '(L3 (define f (lambda (x) (if (> x 0) x (- 0 x)))) (f -3)))) => 3)

     ;; Recursive procedure
     (test (normal-L3-program (parseL3 '(L3 (define f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))) (f 3)))) => 6)

     ;; Preserve bound variables in subst
     (test (normal-L3-program
            (parseL3
             '(L3 (define nf
                    (lambda (f n)
                      (if (= n 0)
                          (lambda (x) x)
                          (if (= n 1)
                              f
                              (lambda (x) (f ((nf f (- n 1)) x)))))))
                  ((nf (lambda (x) (* x x)) 2) 3)))) => 81)

     ;; Accidental capture of the z variable if no renaming
     (test (normal-L3-program
            (parseL3
             '(L3
               (define z (lambda (x) (* x x)))
               (((lambda (x) (lambda (z) (x z)))
                 (lambda (w) (z w)))
                2))))
           =>
           4)

     ;; Y-combinator
     (test (normal-L3-program
            (parseL3
             '(L3 (((lambda (f) (f f))
                    (lambda (fact)
                      (lambda (n)
                        (if (= n 0)
                            1
                            (* n ((fact fact) (- n 1)))))))
                   6))))
           =>
           720)

     ;; L3 higher order functions
     (test (normal-L3-program
            (parseL3
             '(L3 (define map
                    (lambda (f l)
                      (if (eq? l '())
                          l
                          (cons (f (car l)) (map f (cdr l))))))
                  (map (lambda (x) (* x x))
                       '(1 2 3)))))
           =>
           '(1 4 9))

     (test (normal-L3-program
            (parseL3
             '(L3 (define empty? (lambda (x) (eq? x '())))
                  (define filter
                    (lambda (pred l)
                      (if (empty? l)
                          l
                          (if (pred (car l))
                              (cons (car l) (filter pred (cdr l)))
                              (filter pred (cdr l))))))
                  (filter (lambda (x) (not (= x 2)))
                          '(1 2 3 2)))))
           =>
           '(1 3))

     (test (normal-L3-program
            (parseL3
             '(L3 (define compose (lambda (f g) (lambda (x) (f (g x)))))
                  ((compose not number?) 2))))
           =>
           #f)

     )))

(define normal-only-L3-tests
  (lambda ()
    (display "normal-only-L3-tests:\t")
    (run-tests
     (test (normal-L3-program
            (parseL3
             ;; This program loops in eval-order - but completes in normal order
             '(L3 (define loop (lambda () (loop)))
                  (define f (lambda (x y z) (if (= x 1) y z)))
                  (f 1 2 (loop)))))
           =>
           2)

     (test (normal-L3-program
            (parseL3
             '(L3
               (define loop (lambda (x) (loop x)))
               (define g (lambda (x) 5))
               (g (loop 0)))))
           =>
           5)

     (test (normal-L3-program
            (parseL3
             '(L3 
               (define try 
                 (lambda (a b) 
                   (if (= a 0)
                       1
                       b)))
               (try 0 (/ 1 0)))))
           =>
           1)

     (test (normal-L3-program
            (parseL3
             '(L3
               (define f (lambda (x) (display x) (newline) (+ x 1)))
               (define g (lambda (x) 5))
               (g (f 0)))))
           =>
           5)
     )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(normal-L3-tests)
(normal-program-L3-tests)
(normal-only-L3-tests)
  