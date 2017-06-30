#lang racket
(require "utils.rkt"
         "stream.rkt"
         "scanner.rkt"
         "reader.rkt"
         "parser.rkt")

(provide (all-defined-out))

;; Support exception handling
(define (try f (ans 'error))
  (with-handlers ((exn?
                   (lambda (exn) ans))) (f)))


(define stack-tests
  (lambda ()
    (display "stack-tests:\t")
    (run-tests
     (test (push '(a b) 'c) => '(a b c))
     (test (push '() 'a) => '(a))
     (test (push '(a b) '(c)) => '(a b (c))))))


(define stream-tests
  (lambda ()
    (display "stream-tests:\t")
    (run-tests

     ;; take-n
     (test (take-n '() 2) => '(()))
     (test (take-n '(1 2 3) 2) => '((1 2) 3))
     (test (take-n '(1 2 3) 0) => '(() 1 2 3))
     (test (take-n '(1 2 3) 4) => '((1 2 3)))

     ;; take-until
     (test (take-until even? '()) => '(()))
     (test (take-until even? '(1 2 3 4)) => '((1) 2 3 4))
     (test (take-until even? '(2 3 4)) => '(() 2 3 4))
     (test (take-until odd? '(2 4 6)) => '((2 4 6)))

     ;;
     (test (chars-to-groups '(" " "a" "b" " " "c" " ")) => '(("a" "b") ("c")))
     (test (chars-to-groups '("a" "b" " " "c" " ")) => '(("a" "b") ("c")))

     ;;
     (test (make-aggregate-stream space? '(" " "a" "b" " " "c" " ")) => '(("a" "b") ("c")))
     (test (make-aggregate-stream even? '(1 2 3 4 5)) => '((1) (3) (5)))
     (test (make-aggregate-stream odd? '(1 2 3 4 5)) => '((2) (4)))
     (test (make-aggregate-stream odd? '()) => '())
     (test (make-aggregate-stream odd? '(1 3 5)) => '())

     )))

(define scanner-tests
  (lambda ()
    (display "scanner-tests:\t")
    (run-tests
     (test (classify-token "12.3") => 'number)
     (test (classify-token "#f") => 'boolean)
     (test (classify-token "#t") => 'boolean)
     (test (classify-token "; this is a comment") => 'comment)
     (test (classify-token "12.3a") => 'identifier)
     (test (classify-token "a12.3") => 'identifier)
     (test (classify-token "\"12\"") => 'string)
     (test (classify-token "\"12\"3\"") => 'string)
     (test (classify-token "'") => 'quote)
     (test (classify-token "(") => 'lparen)
     (test (classify-token ")") => 'rparen)
     (test (classify-token "ab?") => 'identifier)

     ;; scan-string - split into tokens according to Scheme lexical rules
     (test (scan-string "") => '())

     (test (scan-string "a") => '("a"))
     (test (scan-string " a") => '("a"))    ;; skip spaces
     (test (scan-string " a   ") => '("a")) ;; skip spaces
     
     (test (scan-string "1") => '("1"))
     (test (scan-string " 1") => '("1"))    ;; skip spaces
     (test (scan-string " 1   ") => '("1")) ;; skip spaces

     (test (scan-string "#t") => '("#t"))
     (test (scan-string " #t") => '("#t"))    ;; skip spaces
     (test (scan-string " #t   ") => '("#t")) ;; skip spaces

     (test (scan-string "\"a\"") => '("\"a\""))
     (test (scan-string " \"a\"") => '("\"a\""))
     (test (scan-string " \"a\"  ") => '("\"a\""))
     (test (scan-string "\"a b\"") => '("\"a b\"")) ;; preserve space inside strings
     
     (test (scan-string "a b") => '("a" "b"))
     (test (scan-string " a 1 ") => '("a" "1")) 
     (test (scan-string " a1   ") => '("a1"))
     
     (test (scan-string "a #t 1") => '("a" "#t" "1"))
     (test (scan-string " a? 1 1a") => '("a?" "1" "1a"))
     
     (test (scan-string "(a b ())") => '("(" "a" "b" "(" ")" ")"))
     (test (scan-string "()") => '("(" ")"))
     (test (scan-string "'()") => '("'" "(" ")"))

     (test (scan-string "() ;; comment ()") => '("(" ")" ";; comment ()"))
     
     )))


(define reader-tests
  (lambda ()
    (display "reader-tests:\t")
    (run-tests
     (test (tokens->sexp '()) => '())
     
     (test (tokens->sexp (scan-string "1")) => '(1))
     (test (tokens->sexp (scan-string "#t")) => '(#t))
     (test (tokens->sexp (scan-string "#f")) => '(#f))
     (test (tokens->sexp (scan-string "\"a b\"")) => '("a b"))
     (test (tokens->sexp (scan-string "a")) => '(a))
     (test (tokens->sexp (scan-string ";; comment is skipped")) => '())

     ;; two atomic sexps in sequence - extracts the first token only
     (test (tokens->sexp (scan-string "1 2")) => '(1 "2"))
     (test (tokens->sexp (scan-string "1 #t")) => '(1 "#t"))
     (test (tokens->sexp (scan-string "1 a")) => '(1 "a"))

     ;; compound sexp
     (test (tokens->sexp (scan-string "(1 2)")) => '((1 2)))     
     (test (tokens->sexp (scan-string "((1) 2)")) => '(((1) 2)))

     ;; literal sexp
     (test (tokens->sexp (scan-string "'a")) =>  '('a))
     (test (tokens->sexp (scan-string "'()")) => '('()))
     (test (tokens->sexp (scan-string "'1")) => '('1))
     (test (tokens->sexp (scan-string "'(a)")) => '('(a)))

     ;; read-string-as-sexps
     (test (read-string-as-sexps "") => '())
     
     (test (read-string-as-sexps "1") => '(1))
     (test (read-string-as-sexps "#t") => '(#t))
     (test (read-string-as-sexps "#f") => '(#f))
     (test (read-string-as-sexps "\"a b\"") => '("a b"))
     (test (read-string-as-sexps "a") => '(a))
     (test (read-string-as-sexps ";; comment is skipped") => '())

     ;; two atomic sexps in sequence - extracts the first token only
     (test (read-string-as-sexps "1 2") => '(1 2))
     (test (read-string-as-sexps "1 #t") => '(1 #t))
     (test (read-string-as-sexps "1 a") => '(1 a))

     ;; compound sexp
     (test (read-string-as-sexps "(1 2)") => '((1 2)))     
     (test (read-string-as-sexps "((1) 2)") => '(((1) 2)))

     ;; literal sexp
     (test (read-string-as-sexps "'a") =>  '('a))
     (test (read-string-as-sexps "'()") => '('()))
     (test (read-string-as-sexps "'1") => '('1))
     (test (read-string-as-sexps "'(a)") => '('(a)))
     
     )))


(define parser-tests
  (lambda ()
    (display "parser-tests:\t")
    (run-tests
     (test (try (lambda () (parse '()))) => 'error)
     ;; atomic expressions
     (test (parse '1) => '(num-exp 1))
     (test (parse '#t) => '(bool-exp #t))
     (test (parse 'a) => '(var-exp a))
     (test (parse "a") => '(str-exp "a"))

     ;; literal expressions
     (test (parse '(quote a)) => '(literal-exp a))
     (test (parse '(quote (a b))) => '(literal-exp (a b)))

     ;; define
     (test (parse '(define v 1)) => '(def-exp (var-exp v) (num-exp 1)))

     ;; proc-exp
     (test (parse '(lambda () #t)) => '(proc-exp () ((bool-exp #t))))
     (test (parse '(lambda (x) x)) => '(proc-exp ((var-exp x)) ((var-exp x))))
     (test (parse '(lambda (x) x #t)) => '(proc-exp ((var-exp x)) ((var-exp x) (bool-exp #t))))
     
     ;; if-exp
     (test (parse '(if #t (+ 1 2) 'ok)) => '(if-exp (bool-exp #t) (app-exp (var-exp +) ((num-exp 1) (num-exp 2))) (literal-exp ok)))

     ;; let-exp
     (test (parse '(let () 1)) => '(let-exp () ((num-exp 1))))
     (test (parse '(let ((a 1)) a)) => '(let-exp ((binding (var-exp a) (num-exp 1))) ((var-exp a))))
     (test (parse '(let ((a 1)) a #t)) => '(let-exp ((binding (var-exp a) (num-exp 1))) ((var-exp a) (bool-exp #t))))
     
     ;; app-exp
     (test (parse '(+ 1 2)) => '(app-exp (var-exp +) ((num-exp 1) (num-exp 2))))
     (test (parse '(f)) => '(app-exp (var-exp f) ()))
     (test (parse '((lambda () 1))) => '(app-exp (proc-exp () ((num-exp 1))) ()))
     
     )))

(define unparse-tests
  (lambda ()
    (display "unparse-tests:\t")
    (run-tests
     (test (try (lambda () (unparse '()))) => 'error)
     ;; atomic expressions
     (test (unparse (parse '1)) => '1)
     (test (unparse (parse '#t)) => '#t)
     (test (unparse (parse 'a)) => 'a)
     (test (unparse (parse "a")) => '"a")

     ;; literal expressions
     (test (unparse (parse '(quote a))) => '(quote a))
     (test (unparse (parse '(quote (a b)))) => '(quote (a b)))

     ;; define
     (test (unparse (parse '(define v 1))) => '(define v 1))

     ;; proc-exp
     (test (unparse (parse '(lambda () #t))) => '(lambda () #t))
     (test (unparse (parse '(lambda (x) x))) => '(lambda (x) x))
     (test (unparse (parse '(lambda (x) x #t))) => '(lambda (x) x #t))
     
     ;; if-exp
     (test (unparse (parse '(if #t (+ 1 2) 'ok))) => '(if #t (+ 1 2) 'ok))

     ;; let-exp
     (test (unparse (parse '(let () 1))) => '(let () 1))
     (test (unparse (parse '(let ((a 1)) a))) => '(let ((a 1)) a))
     (test (unparse (parse '(let ((a 1)) a #t))) => '(let ((a 1)) a #t))
     
     ;; app-exp
     (test (unparse (parse '(+ 1 2))) => '(+ 1 2))
     (test (unparse (parse '(f))) => '(f))
     (test (unparse (parse '((lambda () 1)))) => '((lambda () 1)))
     
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(stack-tests)
(stream-tests)
(scanner-tests)
(reader-tests)
(parser-tests)
(unparse-tests)