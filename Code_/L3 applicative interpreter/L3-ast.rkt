#lang racket
(provide (all-defined-out))

;; L3 AST extends that of L1 and L2
(require "L1-ast.rkt"
         "L2-ast.rkt")

;; L3 AST
;; ======

;; L3 extends L2 with support for:
;; - Pair and List datatypes
;; - Compound literal expressions denoted with quote
;; - Primitives: cons, car, cdr, pair?, list?
;; - The empty-list literal expression

;; <program> ::= (L3 <exp>+) // program(exps:List(exp))
;; <exp> ::= <define-exp> | <cexp>
;; <define-exp> ::= (define <var-decl> <cexp>) // def-exp(var:var-decl, val:cexp)
;; <cexp> ::= <num-exp> // num-exp(val:Number)
;;        | <bool-exp>  // bool-exp(val:Boolean)
;;        | <prim-op>   // prim-op(op:Symbol)
;;        | <var-ref>   // var-ref(var:Symbol)
;;        | <var-decl>  // var-decl(var:Symbol)
;;        | (if <exp> <exp> <exp>) // if-exp(test,then,else)                                   ##### L2
;;        | (lambda (<var-decl>*) <cexp>+) // proc-exp(params:List(var-decl), body:List(cexp)) ##### L2
;;        | (quote <sexp>) // lit-exp(val:Sexp)                                                ##### L3
;;        | (<cexp> <cexp>*) // app-exp(rator:cexp, rands:List(cexp))
;; <prim-op> ::= + | - | * | / | < | > | = | not |  eq?
;;        | cons | car | cdr | pair? | list? | number? | boolean? | symbol?                    ##### L3
;; <num-exp> ::= a number token
;; <bool-exp> ::= #t | #f
;; <var-ref> ::= an identifier token
;; <var-decl> ::= an identifier token
;; <sexp> ::= a symbol token | ( <sexp>* )                                                     ##### L3

;; Add cases for lit-exp
(define cexp3?
  (lambda (x) (or (num-exp? x)
                  (bool-exp? x)
                  (prim-op? x)
                  (var-ref? x)
                  (var-decl? x)
                  (if-exp? x)
                  (proc-exp? x)
                  (lit-exp? x)
                  (app-exp? x))))


;; AST support for lit-exp

;; Purpose: lit-exp value constructor
;; Signature: make-lit-exp(val)
;; Type: [Sexp -> lit-exp]
(define make-lit-exp
  (lambda (val)
    (list 'lit-exp val)))

(define lit-exp?
  (lambda (x)
    (and (list? x) (= (length x) 2) (eq? (car x) 'lit-exp))))

(define lit-exp->val
  (lambda (lit-exp) (second lit-exp)))

;; Parser
;; ======

;; Purpose: Recognize primitive operators as part of the syntax
;; Signature: primitive-op?(x)
;; Type: [Any -> Boolean]
(define primitive-op3?
  (lambda (x) (or (eq? x '+)
                  (eq? x '-)
                  (eq? x '*)
                  (eq? x '/)
                  (eq? x '<)
                  (eq? x '>)
                  (eq? x '=)
                  (eq? x 'not)
                  (eq? x 'cons)
                  (eq? x 'car)
                  (eq? x 'cdr)
                  (eq? x 'pair?)
                  (eq? x 'list?)
                  (eq? x 'number?)
                  (eq? x 'symbol?)
                  (eq? x 'boolean?)
                  (eq? x 'eq?)
                  (eq? x 'display)
                  (eq? x 'newline))))

;; Purpose: Parse concrete syntax of L3 in sexp into L3-AST
;; Signature: parseL3(x)
;; Type: [SExp -> Program | Exp]
;; Examples:
;; (parseL3 '(L3 (define f (lambda (x) (+ x x))) (f 2))) =>
;;   '(program ((def-exp (var-decl f) (proc-exp ((var-decl x)) ((app-exp (prim-op +) ((var-ref x) (var-ref x)))))
;;              (app-exp (var-ref f) ((num-exp 2)))))
(define parseL3
  (lambda (x)
    (cond ((empty? x) (make-lit-exp x)) ; ##### L3
          ((list? x)
           (cond ((eq? (car x) 'L3)
                  (make-program (map parseL3 (cdr x))))
                 ((eq? (car x) 'define)
                  (make-def-exp (make-var-decl (second x))
                                (parseL3 (third x))))
                 ((eq? (car x) 'if)
                  (make-if-exp (parseL3 (second x))
                               (parseL3 (third x))
                               (parseL3 (fourth x))))
                 ((eq? (car x) 'lambda)
                  (make-proc-exp (map make-var-decl (second x))
                                 (map parseL3 (cddr x))))
                 ((eq? (car x) 'quote)
                  (make-lit-exp (second x)))
                 (else (make-app-exp (parseL3 (car x))
                                     (map parseL3 (cdr x))))))
          ((number? x) (make-num-exp x))
          ((boolean? x) (make-bool-exp x))
          ((primitive-op3? x) (make-prim-op x))
          ((symbol? x) (make-var-ref x))
          (else (error "Unexpected type " x)))))

