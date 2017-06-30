#lang racket
(require "utils.rkt"
         "parser.rkt"
         "free-vars.rkt"
         "rewrite.rkt")

(provide (all-defined-out))

;; Support exception handling
(define (try f (ans 'error))
  (with-handlers ((exn?
                   (lambda (exn) ans))) (f)))


(define free-tests
  (lambda ()
    (display "free-tests:\t")
    (run-tests
     (test (occurs-free? 'x (parse 1)) => #f)
     (test (occurs-free? 'x (parse #t)) => #f)
     (test (occurs-free? 'x (parse "x")) => #f)
     (test (occurs-free? 'x (parse '(quote x))) => #f)
     (test (occurs-free? 'x (parse 'x)) => #t)
     (test (occurs-free? 'x (parse 'y)) => #f)

     (test (occurs-free? 'x (parse '(lambda () x))) => #t)
     (test (occurs-free? 'x (parse '(lambda (x) x))) => #f)
     (test (occurs-free? 'x (parse '(lambda (y) x))) => #t)
     (test (occurs-free? 'x (parse '(lambda (y) (lambda (z) x)))) => #t)
     (test (occurs-free? 'x (parse '(lambda (x) (lambda (z) x)))) => #f)
     (test (occurs-free? 'x (parse '(lambda (y x) x))) => #f)

     (test (occurs-free? 'x (parse '(let () x))) => #t)
     (test (occurs-free? 'x (parse '(let ((x 1)) x))) => #f)
     (test (occurs-free? 'x (parse '(let ((y 1)) x))) => #t)
     (test (occurs-free? 'x (parse '(let ((y 1)) (lambda (z) x)))) => #t)
     (test (occurs-free? 'x (parse '(let ((x 1)) (lambda (z) x)))) => #f)
     (test (occurs-free? 'x (parse '(let ((y 1) (x 2)) x))) => #f)
     (test (occurs-free? 'x (parse '(let ((y x) (x 2)) x))) => #t)
     (test (occurs-free? 'x (parse '(let ((y x) (z 2)) z))) => #t)

     (test (occurs-free? 'x (parse '(if x 1 2))) => #t)
     (test (occurs-free? 'x (parse '(if #t x 2))) => #t)
     (test (occurs-free? 'x (parse '(if #t 1 x))) => #t)
     (test (occurs-free? 'x (parse '(if #t 1 2))) => #f)

     (test (occurs-free? 'x (parse '(+ 1 x))) => #t)
     (test (occurs-free? 'x (parse '(+ 1 2))) => #f)

     )))
     
     
(define rewrite-tests
  (lambda ()
    (display "rewrite-tests:\t")
    (run-tests
     (test (unparse (rewrite-let (parse '(let ((x 1)) (+ x x))))) => '((lambda (x) (+ x x)) 1))
     
     (test (unparse (rewrite-all-let (parse '(lambda (x) (let ((y (+ x 2))) (* y y)))))) => '(lambda (x) ((lambda (y) (* y y)) (+ x 2))))
     (test (unparse (rewrite-all-let (parse '(let ((x 1)) (let ((y (+ x 2))) (* y y)))))) =>
           '((lambda (x) ((lambda (y) (* y y)) (+ x 2))) 1))

     )))
     


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(free-tests)
(rewrite-tests)
