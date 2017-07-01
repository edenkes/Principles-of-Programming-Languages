#lang racket
(provide (all-defined-out))

;; L2 AST extends that of L1
(require "L1-ast.rkt")

;; L2 AST
;; ======

;; <program> ::= (L2 <exp>+) // program(exps:List(exp))
;; <exp> ::= <define-exp> | <cexp>
;; <define-exp> ::= (define <var-decl> <cexp>) // def-exp(var:var-decl, val:cexp)
;; <cexp> ::= <num-exp> // num-exp(val:Number)
;;        | <bool-exp>  // bool-exp(val:Boolean)
;;        | <prim-op>   // prim-op(op:Symbol)
;;        | <var-ref>   // var-ref(var:Symbol)
;;        | <var-decl>  // var-decl(var:Symbol)
;;        | (if <exp> <exp> <exp>) // if-exp(test,then,else)                                   #####
;;        | (lambda (<var-decl>*) <cexp>+) // proc-exp(params:List(var-decl), body:List(cexp)) #####
;;        | (<cexp> <cexp>*) // app-exp(rator:cexp, rands:List(cexp))
;; <prim-op> ::= + | - | * | / | < | > | = | not
;; <num-exp> ::= a number token
;; <bool-exp> ::= #t | #f
;; <var-ref> ::= an identifier token
;; <var-decl> ::= an identifier token

;; Add cases for if-exp and proc-exp
(define cexp2?
  (lambda (x) (or (num-exp? x)
                  (bool-exp? x)
                  (prim-op? x)
                  (var-ref? x)
                  (var-decl? x)
                  (if-exp? x)
                  (proc-exp? x)
                  (app-exp? x))))


;; AST support for if-exp and proc-exp

;; Purpose: if-exp value constructor
;; Signature: make-if-exp(test,then,else)
;; Type: [Cexp * Cexp * Cexp -> If-exp]
(define make-if-exp
  (lambda (test then else)
    (list 'if-exp test then else)))

(define if-exp?
  (lambda (x)
    (and (list? x) (= (length x) 4) (eq? (car x) 'if-exp))))

(define if-exp->test
  (lambda (if-exp) (second if-exp)))
(define if-exp->then
  (lambda (if-exp) (third if-exp)))
(define if-exp->else
  (lambda (if-exp) (fourth if-exp)))

;; Purpose: proc-exp value constructor
;; Signature: make-proc-exp(params, body)
;; Type: [List(var-decl) * List(cexp) -> proc-exp]
(define make-proc-exp
  (lambda (params body)
    (list 'proc-exp params body)))

(define proc-exp?
  (lambda (x) (and (list? x) (= (length x) 3) (eq? (car x) 'proc-exp))))

(define proc-exp->params
  (lambda (proc-exp) (second proc-exp)))
(define proc-exp->body
  (lambda (proc-exp) (third proc-exp)))

;; Parser
;; ======

;; Purpose: Parse concrete syntax of L2 in sexp into L2-AST
;; Signature: parseL2(x)
;; Type: [SExp -> Program | Exp]
;; Examples:
;; (parseL2 '(L2 (define f (lambda (x) (+ x x))) (f 2))) =>
;;   '(program ((def-exp (var-decl f) (proc-exp ((var-decl x)) ((app-exp (prim-op +) ((var-ref x) (var-ref x)))))
;;              (app-exp (var-ref f) ((num-exp 2)))))
(define parseL2
  (lambda (x)
    (cond ((empty? x) (error "Unexpected empty"))
          ((list? x)
           (cond ((eq? (car x) 'L2)
                  (make-program (map parseL2 (cdr x))))
                 ((eq? (car x) 'define)
                  (make-def-exp (make-var-decl (second x))
                                (parseL2 (third x))))
                 ((eq? (car x) 'if)
                  (make-if-exp (parseL2 (second x))
                               (parseL2 (third x))
                               (parseL2 (fourth x))))
                 ((eq? (car x) 'lambda)
                  (make-proc-exp (map make-var-decl (second x))
                                 (map parseL2 (cddr x))))
                 (else (make-app-exp (parseL2 (car x))
                                     (map parseL2 (cdr x))))))
          ((number? x) (make-num-exp x))
          ((boolean? x) (make-bool-exp x))
          ((primitive-op? x) (make-prim-op x))
          ((symbol? x) (make-var-ref x))
          (else (error "Unexpected type " x)))))

