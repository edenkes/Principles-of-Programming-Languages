#lang racket
(require "utils.rkt"
         "parser.rkt"
         "lexical-address.rkt")

(provide (all-defined-out))

(define parseLA-tests
  (lambda ()
    (display "parseLA-tests:\t")
    (run-tests
     (test (parseLA '1) => '(num-exp 1))
     (test (parseLA '(if #t (+ 1 2) 'ok)) => '(if-expLA (bool-exp #t) (app-expLA (var-ref +) ((num-exp 1) (num-exp 2))) (literal-exp ok)))
     (test (parseLA '(lambda (x) x)) => '(proc-expLA ((var-decl x)) ((var-ref x))))
     )))

(define unparseLA-tests
  (lambda ()
    (display "unparseLA-tests:\t")
    (run-tests
     (test (unparseLA (parseLA '1)) => 1)
     (test (unparseLA (parseLA '(if #t (+ 1 2) 'ok))) => '(if #t (+ 1 2) 'ok))
     (test (unparseLA (parseLA '(lambda (x) x))) => '(lambda (x) x))
     )))

(define test-get-lexical-address
  (lambda ()
    (display "test-get-lexical-address:\t")
    (run-tests
     (test (get-lexical-address '(var-ref b) '([lex-addr a 0 0] [lex-addr b 0 1])) => '[lex-addr b 0 1])
     (test (get-lexical-address '(var-ref c) '([lex-addr a 0 0] [lex-addr b 0 1])) => '[free-var c])
     (test (get-lexical-address '(var-ref a) '([lex-addr a 0 0] [lex-addr b 0 1] [lex-add a 1 1])) => '[lex-addr a 0 0])
     )))

(define test-index-of-var
  (lambda ()
    (display "test-index-of-var:\t")
    (run-tests
     (test (index-of-var '(var-decl b) '([var-decl a] [var-decl b])) => 1)
     (test (index-of-var '(var-decl c) '([var-decl a] [var-decl b])) => 'free)
     )))

(define test-cross-contour
  (lambda ()
    (display "test-cross-contour:\t")
    (run-tests
     (test (cross-contour '([var-decl a] [var-decl b]) '([lex-addr a 0 0] [lex-addr c 0 1])) =>
           '([lex-addr a 0 0] [lex-addr b 0 1] [lex-addr a 1 0] [lex-addr c 1 1])))))

(define test-lexical-address
  (lambda ()
    (display "test-lexical-address:\t")
    (run-tests
     (test (unparseLA (lexical-address (parseLA '(lambda (x) x)))) => '(lambda (x) [x : 0 0]))
     (test (unparseLA (lexical-address (parseLA '(lambda (x) (lambda (y) (+ x y)))))) =>
           '(lambda (x) (lambda (y) ([+ free] [x : 1 0] [y : 0 0]))))
     (test (unparseLA (lexical-address (parseLA '((lambda (x) (* x x)) ((lambda (x) (+ x x)) 2))))) =>
           '((lambda (x) ([* free] [x : 0 0] [x : 0 0])) ((lambda (x) ([+ free] [x : 0 0] [x : 0 0])) 2)))
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(parseLA-tests)
(unparseLA-tests)
(test-get-lexical-address)
(test-index-of-var)
(test-cross-contour)
(test-lexical-address)
