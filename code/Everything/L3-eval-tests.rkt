#lang racket
(require "utils.rkt"
         "env.rkt"
         "L1-ast.rkt"
         "L2-ast.rkt"
         "L3-ast.rkt"
         "L2-eval.rkt"
         "L3-eval.rkt")

(provide (all-defined-out))

;; Support exception handling
(define (try f (ans 'error))
  (with-handlers ((exn?
                   (lambda (exn) ans))) (f)))

(define parseL3-tests
  (lambda ()
    (display "parseL3-tests:\t")
    (run-tests
     ;; L1 syntactic forms
     (test (parseL3 '1) => '(num-exp 1))
     (test (parseL3 '#t) => '(bool-exp #t))
     (test (parseL3 '+) => '(prim-op +))
     (test (parseL3 'x) => '(var-ref x))
     (test (parseL3 '(+ 1 x)) => '(app-exp (prim-op +) ((num-exp 1) (var-ref x))))
     (test (parseL3 '(* (+ 1 x) 2)) => '(app-exp (prim-op *) ((app-exp (prim-op +) ((num-exp 1) (var-ref x))) (num-exp 2))))
     (test (parseL3 '(L3 (define x (+ 1 2)) (* x x))) =>
           '(program ((def-exp (var-decl x) (app-exp (prim-op +) ((num-exp 1) (num-exp 2))))
                      (app-exp (prim-op *) ((var-ref x) (var-ref x))))))

     ;; L2 syntactic forms
     (test (parseL3 '(if #t 1 2)) => '(if-exp (bool-exp #t) (num-exp 1) (num-exp 2)))
     (test (parseL3 '(lambda (x) x)) => '(proc-exp ((var-decl x)) ((var-ref x))))

     ;; L3 syntactic forms
     (test (parseL3 '(cons 1 '())) => '(app-exp (prim-op cons) ((num-exp 1) (lit-exp ()))))
     (test (parseL3 '(car '(1 . 2))) => '(app-exp (prim-op car) ((lit-exp (1 . 2)))))
     (test (parseL3 '(cdr '(1 2))) => '(app-exp (prim-op cdr) ((lit-exp (1 2)))))
     (test (parseL3 '(number? 'x)) => '(app-exp (prim-op number?) ((lit-exp x))))
     
     )))


(define substitute-tests
  (lambda ()
    (display "substitute-tests:\t")
    (run-tests
     (test (substitute (map parseL2 '(((lambda (x) (* x x)) x)))
                       '(x)
                       '((num-exp 3)))
           =>
           (map parseL2 '(((lambda (x) (* x x)) 3))))

     (test (substitute (map parseL3 '((if (= n 0)
                                          (lambda (x) x)
                                          (if (= n 1)
                                              f
                                              (lambda (x) (f ((nf f (- n 1)) x)))))))
                       '(f n)
                       (list (parseL3 '(lambda (x) (* x x)))
                             (parseL3 2)))
           =>
           (map parseL3 '((if (= 2 0)
                              (lambda (x) x)
                              (if (= 2 1)
                                  (lambda (x) (* x x))
                                  (lambda (x) ((lambda (x) (* x x))
                                               ((nf (lambda (x) (* x x)) (- 2 1)) x))))))))

     ;; Note how z becomes bound in the result of the substitution
     ;; To avoid such accidental captures - we must use rename-vars.
     (test (substitute (map parseL3 '((lambda (z) (x z))))
                       '(x)
                       (map parseL3 '((lambda (w) (z w)))))
           =>
           (map parseL3 '((lambda (z) ((lambda (w) (z w)) z)))))
     )))

(define rename-tests
  (lambda ()
    (run-tests
     (display "rename-tests:\t")
     (test (rename-exps (list (parseL3 '(lambda (x) x)))) => (list (parseL2 '(lambda (x1) x1))))
     (test (rename-exps (map parseL3
                             '((((lambda (x) (lambda (z) (x z)))
                                 (lambda (w) (z w)))
                                2))))
           =>
           (map parseL3 '((((lambda (x1) (lambda (z2) (x1 z2)))
                            (lambda (w3) (z w3)))
                           2))))
     )))

(define eval-L3-tests
  (lambda ()
    (display "eval-L3-tests:\t")
    (let ((ge (make-empty-env)))
      (run-tests
       (test (L3-applicative-eval (parseL3 '1) ge) => 1)
       (test (L3-applicative-eval (parseL3 '#t) ge) => #t)
       (test (L3-applicative-eval (parseL3 'x) (extend-env ge '(x) '(1))) => 1)
       (test (L3-applicative-eval (parseL3 '+) ge) => '(prim-op +))
       (test (L3-applicative-eval (parseL3 '(+ 1 2)) ge) => 3)
       (test (L3-applicative-eval (parseL3 '(> 2 1)) ge) => #t)
       (test (L3-applicative-eval (parseL3 '(not (> 2 1))) ge) => #f)
       (test (L3-applicative-eval (parseL3 '(+ (* 2 2) 3)) ge) => 7)

       ;; L2 syntactic forms
       (test (L3-applicative-eval (parseL3 '(if (> 2 1) 3 -3)) ge) => 3)
       (test (L3-applicative-eval (parseL3 '(lambda (x) x)) ge) => '(closure ((var-decl x)) ((var-ref x))))

       ;; L3 syntactic forms
       (test (L3-applicative-eval (parseL3 '(cons 1 '())) ge) => '(1))
       (test (L3-applicative-eval (parseL3 '(car '(1 . 2))) ge) => 1)
       (test (L3-applicative-eval (parseL3 '(cdr '(1 2))) ge) => '(2))
       (test (L3-applicative-eval (parseL3 '(number? 'x)) ge) => #f)
       (test (L3-applicative-eval (parseL3 '(symbol? 'x)) ge) => #t)
       (test (L3-applicative-eval (parseL3 '(list? (cons 1 2))) ge) => #f)
       (test (L3-applicative-eval (parseL3 '(pair? (cons 1 2))) ge) => #t)
       (test (L3-applicative-eval (parseL3 '(boolean? #t)) ge) => #t)
       (test (L3-applicative-eval (parseL3 '(eq? 'x 'x)) ge) => #t)

       ))))

(define eval-program-L3-tests
  (lambda ()
    (display "eval-program-L3-tests:\t")
    (run-tests
     (test (eval-L3-program (parseL3 '(L3 (define x (+ 3 2))
                                       (* x x)))) => 25)
     (test (eval-L3-program (parseL3 '(L3 (define x 1)))) => (void))
     (test (eval-L3-program (parseL3 '(L3 (define x 3) (* x x) (+ x x)))) => 6)
     (test (eval-L3-program (parseL3 '(L3 (define x 3) (not (> x 2))))) => #f)

     ;; Procedure application
     (test (eval-L3-program (parseL3 '(L3 (define f (lambda (x) (* x x))) (f 3)))) => 9)
     (test (eval-L3-program (parseL3 '(L3 (define f (lambda (x) (if (> x 0) x (- 0 x)))) (f -3)))) => 3)

     ;; Recursive procedure
     (test (eval-L3-program (parseL3 '(L3 (define f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))) (f 3)))) => 6)

     ;; Preserve bound variables in subst
     (test (eval-L3-program
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
     (test (eval-L3-program
            (parseL3
             '(L3
               (define z (lambda (x) (* x x)))
               (((lambda (x) (lambda (z) (x z)))
                 (lambda (w) (z w)))
                2))))
           =>
           4)

     ;; Y-combinator
     (test (eval-L3-program
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
     (test (eval-L3-program
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

     (test (eval-L3-program
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

     (test (eval-L3-program
            (parseL3
             '(L3 (define compose (lambda (f g) (lambda (x) (f (g x)))))
                  ((compose not number?) 2))))
           =>
           #f)

     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(parseL3-tests)
(substitute-tests)
(rename-tests)
(eval-L3-tests)
(eval-program-L3-tests)