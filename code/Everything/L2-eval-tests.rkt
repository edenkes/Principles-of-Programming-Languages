#lang racket
(require "utils.rkt"
         "env.rkt"
         "L1-ast.rkt"
         "L2-ast.rkt"
         "L2-eval.rkt")

(provide (all-defined-out))

;; Support exception handling
(define (try f (ans 'error))
  (with-handlers ((exn?
                   (lambda (exn) ans))) (f)))

(define parseL2-tests
  (lambda ()
    (display "parseL2-tests:\t")
    (run-tests
     (test (parseL2 '1) => '(num-exp 1))
     (test (parseL2 '#t) => '(bool-exp #t))
     (test (parseL2 '+) => '(prim-op +))
     (test (parseL2 'x) => '(var-ref x))
     (test (parseL2 '(+ 1 x)) => '(app-exp (prim-op +) ((num-exp 1) (var-ref x))))
     (test (parseL2 '(* (+ 1 x) 2)) => '(app-exp (prim-op *) ((app-exp (prim-op +) ((num-exp 1) (var-ref x))) (num-exp 2))))
     (test (parseL2 '(L2 (define x (+ 1 2)) (* x x))) =>
           '(program ((def-exp (var-decl x) (app-exp (prim-op +) ((num-exp 1) (num-exp 2))))
                      (app-exp (prim-op *) ((var-ref x) (var-ref x))))))

     ;; New L2 syntactic forms
     (test (parseL2 '(if #t 1 2)) => '(if-exp (bool-exp #t) (num-exp 1) (num-exp 2)))
     (test (parseL2 '(lambda (x) x)) => '(proc-exp ((var-decl x)) ((var-ref x))))
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

     (test (substitute (map parseL2 '((if (= n 0)
                                          (lambda (x) x)
                                          (if (= n 1)
                                              f
                                              (lambda (x) (f ((nf f (- n 1)) x)))))))
                       '(f n)
                       (list (parseL2 '(lambda (x) (* x x)))
                             (parseL2 2)))
           =>
           (map parseL2 '((if (= 2 0)
                              (lambda (x) x)
                              (if (= 2 1)
                                  (lambda (x) (* x x))
                                  (lambda (x) ((lambda (x) (* x x))
                                               ((nf (lambda (x) (* x x)) (- 2 1)) x))))))))

     ;; Note how z becomes bound in the result of the substitution
     ;; To avoid such accidental captures - we must use rename-vars.
     (test (substitute (map parseL2 '((lambda (z) (x z))))
                       '(x)
                       (map parseL2 '((lambda (w) (z w)))))
           =>
           (map parseL2 '((lambda (z) ((lambda (w) (z w)) z)))))
     )))

(define rename-tests
  (lambda ()
    (run-tests
     (display "rename-tests:\t")
     (test (rename-exps (list (parseL2 '(lambda (x) x)))) => (list (parseL2 '(lambda (x__1) x__1))))
     (test (rename-exps (map parseL2
                             '((((lambda (x) (lambda (z) (x z)))
                                 (lambda (w) (z w)))
                                2))))
           =>
           (map parseL2 '((((lambda (x__1) (lambda (z__2) (x__1 z__2)))
                            (lambda (w__3) (z w__3)))
                           2))))
     )))

(define eval-L2-tests
  (lambda ()
    (display "eval-L2-tests:\t")
    (let ((ge (make-empty-env)))
      (run-tests
       (test (L2-applicative-eval (parseL2 '1) ge) => 1)
       (test (L2-applicative-eval (parseL2 '#t) ge) => #t)
       (test (L2-applicative-eval (parseL2 'x) (extend-env ge '(x) '(1))) => 1)
       (test (L2-applicative-eval (parseL2 '+) ge) => '(prim-op +))
       (test (L2-applicative-eval (parseL2 '(+ 1 2)) ge) => 3)
       (test (L2-applicative-eval (parseL2 '(> 2 1)) ge) => #t)
       (test (L2-applicative-eval (parseL2 '(not (> 2 1))) ge) => #f)
       (test (L2-applicative-eval (parseL2 '(+ (* 2 2) 3)) ge) => 7)

       ;; New L2 syntactic forms
       (test (L2-applicative-eval (parseL2 '(if (> 2 1) 3 -3)) ge) => 3)
       (test (L2-applicative-eval (parseL2 '(lambda (x) x)) ge) => '(closure ((var-decl x)) ((var-ref x))))
     ))))

(define eval-program-L2-tests
  (lambda ()
    (display "eval-program-L2-tests:\t")
    (run-tests
     (test (eval-L2-program (parseL2 '(L2 (define x (+ 3 2))
                                       (* x x)))) => 25)
     (test (eval-L2-program (parseL2 '(L2 (define x 1)))) => (void))
     (test (eval-L2-program (parseL2 '(L2 (define x 3) (* x x) (+ x x)))) => 6)
     (test (eval-L2-program (parseL2 '(L2 (define x 3) (not (> x 2))))) => #f)

     ;; Procedure application
     (test (eval-L2-program (parseL2 '(L2 (define f (lambda (x) (* x x))) (f 3)))) => 9)
     (test (eval-L2-program (parseL2 '(L2 (define f (lambda (x) (if (> x 0) x (- 0 x)))) (f -3)))) => 3)

     ;; Recursive procedure
     (test (eval-L2-program (parseL2 '(L2 (define f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))) (f 3)))) => 6)

     ;; Preserve bound variables in subst
     (test (eval-L2-program
            (parseL2
             '(L2 (define nf
                    (lambda (f n)
                      (if (= n 0)
                          (lambda (x) x)
                          (if (= n 1)
                              f
                              (lambda (x) (f ((nf f (- n 1)) x)))))))
                  ((nf (lambda (x) (* x x)) 2) 3)))) => 81)

     ;; Accidental capture of the z variable if no renaming
     (test (eval-L2-program
            (parseL2
             '(L2
               (define z (lambda (x) (* x x)))
               (((lambda (x) (lambda (z) (x z)))
                 (lambda (w) (z w)))
                2))))
           =>
           4)

     ;; Y-combinator
     (test (eval-L2-program
            (parseL2
             '(L2 (((lambda (f) (f f))
                    (lambda (fact)
                      (lambda (n)
                        (if (= n 0)
                            1
                            (* n ((fact fact) (- n 1)))))))
                   6))))
           =>
           720)
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(parseL2-tests)
(substitute-tests)
(rename-tests)
(eval-L2-tests)
(eval-program-L2-tests)