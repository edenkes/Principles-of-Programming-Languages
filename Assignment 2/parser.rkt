#lang racket
(provide (all-defined-out))

;; =============================================================================
;; Scheme Parser
;;
;; Parse a sexp into a scheme expression according to the following CFG:
;; <exp> ::= <define> | <cexp>              / def-exp | cexp
;; <define> ::= ( define <var> <cexp> )     / def-exp(var:var-exp, val:cexp)
;; <var> ::= <identifier>                   / var-exp(var:symbol)
;; <binding> ::= ( <var> <cexp> )           / binding(var:var-exp, val:cexp)
;; <cexp> ::= <number>                      / num-exp(val:number)
;;         |  <boolean>                     / bool-exp(val:boolean)
;;         |  <string>                      / str-exp(val:string)
;;         |  ( lambda ( <var>* ) <cexp>+ ) / proc-exp(params:List(var-exp), body:List(cexp))
;;         |  ( if <cexp> <cexp> <cexp> )   / if-exp(test: cexp, then: cexp, else: cexp)
;;         |  ( let ( binding* ) <cexp>+ )  / let-exp(bindings:List(binding), body:List(cexp))
;;         |  ( <cexp> <cexp>* )            / app-exp(operator:cexp, operands:List(cexp))
;;         |  ( quote <sexp> )              / literal-exp(val:sexp)

;; Define the types
;; All the expression types are disjoint union types.
;; ================
;; For each one: constructor, type-predicate and getters
;; 
;; def-exp: [var:symbol, val:cexp]
(define make-def-exp (lambda (var val) (list 'def-exp var val)))
(define def-exp? (lambda (x) (and (list? x) (eq? (car x) 'def-exp))))
(define def-exp->var (lambda (exp) (second exp)))
(define def-exp->val (lambda (exp) (third exp)))

;; var: [var:symbol]
(define make-var-exp (lambda (symbol) (list 'var-exp symbol)))
(define var-exp? (lambda (x) (and (list? x) (eq? (car x) 'var-exp))))
(define var-exp->var (lambda (exp) (second exp)))

;; binding: [var:var, val:cexp]
(define make-binding (lambda (var cexp) (list 'binding var cexp)))
(define binding? (lambda (x) (and (list? x) (eq? (car x) 'binding))))
(define binding->var (lambda (x) (second x)))
(define binding->val (lambda (x) (third x)))

;; cexp is a disjoint union type
(define cexp? (lambda (x)
                (or (num-exp? x)
                    (bool-exp? x)
                    (str-exp? x)
                    (var-exp? x)
                    (literal-exp? x)
                    (proc-exp? x)
                    (if-exp? x)
                    (let-exp? x)
                    (let*-exp? x)
                    (app-exp? x))))

;; num-exp: [val:number]
(define make-num-exp (lambda (val) (list 'num-exp val)))
(define num-exp? (lambda (x) (and (list? x) (eq? (car x) 'num-exp))))
(define num-exp->val (lambda (x) (second x)))

;; bool-exp: [val:boolean]
(define make-bool-exp (lambda (val) (list 'bool-exp val)))
(define bool-exp? (lambda (x) (and (list? x) (eq? (car x) 'bool-exp))))
(define bool-exp->val (lambda (x) (second x)))

;; str-exp: [val:string]
(define make-str-exp (lambda (val) (list 'str-exp val)))
(define str-exp? (lambda (x) (and (list? x) (eq? (car x) 'str-exp))))
(define str-exp->val (lambda (x) (second x)))

;; literal-exp: [val:sexp]
(define make-literal-exp (lambda (val) (list 'literal-exp val)))
(define literal-exp? (lambda (x) (and (list? x) (eq? (car x) 'literal-exp))))
(define literal-exp->val (lambda (x) (second x)))

;; proc-exp: [params:List(var-exp), body:List(cexp))]
(define make-proc-exp (lambda (params body) (list 'proc-exp params body)))
(define proc-exp? (lambda (x) (and (list? x) (eq? (car x) 'proc-exp))))
(define proc-exp->params (lambda (x) (second x)))
(define proc-exp->body (lambda (x) (third x)))

;; if-exp: [test:cexp, then:cexp, else:cexp]
(define make-if-exp (lambda (test then else) (list 'if-exp test then else)))
(define if-exp? (lambda (x) (and (list? x) (eq? (car x) 'if-exp))))
(define if-exp->test (lambda (x) (second x)))
(define if-exp->then (lambda (x) (third x)))
(define if-exp->else (lambda (x) (fourth x)))

;; let-exp: [bindings:List(binding), body:List(cexp)]
(define make-let-exp (lambda (bindings body) (list 'let-exp bindings body)))
(define let-exp? (lambda (x) (and (list? x) (eq? (car x) 'let-exp))))
(define let-exp->bindings (lambda (x) (second x)))
(define let-exp->body (lambda (x) (third x)))

;; app-exp: [rator:cexp, rands:List(cexp)]
(define make-app-exp (lambda (rator rands) (list 'app-exp rator rands)))
(define app-exp? (lambda (x) (and (list? x) (eq? (car x) 'app-exp))))
(define app-exp->rator (lambda (x) (second x)))
(define app-exp->rands (lambda (x) (third x)))


; Purpose: let* value constructor
; Signature: make-let*-exp(bindings,body)
; Type: *Write the type here*
(define make-let*-exp (lambda (bindings body) (list 'let*-exp bindings body)))

; Purpose: let* type predicate
; Signature: let*-exp?(x)
; Type: *Write the type here*
(define let*-exp? (lambda (x) (and (list? x) (eq? (car x) 'let*-exp))))

; Purpose: An accessor for the bindings of a let* expression
; Signature: let*-exp->bindings(x)
; Type: *Write the type here*
(define let*-exp->bindings (lambda (x) (second x)))

; Purpose: An accessor for the body of a let* expression
; Signature: let*-exp->body(x)
; Type: *Write the type here*
(define let*-exp->body (lambda (x) (third x)))


;; Purpose: parse a sexp into a Scheme abstract syntax tree (AST) expression.
;; Type: [Sexp -> Exp]
;; Signature: parse(sexp)
;; Examples:
;; (parse '1) -> '(num-exp 1)
;; (parse '(define v 1)) -> '(def-exp (var-exp v) (num-exp 1))
;; (parse '(if #t (+ 1 2) 'ok)) -> '(if-exp (bool-exp #t) (app-exp (var-exp +) ((num-exp 1) (num-exp 2))) (literal-exp ok))
(define parse
  (lambda (sexp)
    (cond ((empty? sexp) (error "unexpected " sexp))
          ((and (list? sexp)
                (eq? (first sexp) 'define))
           (if (and (= (length sexp) 3)
                    (symbol? (second sexp)))
               (make-def-exp (make-var-exp (second sexp))
                             (parse-cexp (third sexp)))
               (error "bad define expression " exp)))
          (else (parse-cexp sexp)))))

;; Type: [Sexp -> Cexp]
;; Signature: parse-cexp(sexp)
;; Purpose: parse a sexp into a cexp (an expression which is not define)
(define parse-cexp
  (lambda (sexp)
    (cond ((number? sexp) (make-num-exp sexp))
          ((boolean? sexp) (make-bool-exp sexp))
          ((string? sexp) (make-str-exp sexp))
          ((symbol? sexp) (make-var-exp sexp))
          ((empty? sexp) (error "unexpected empty"))
          (else (let ((first (car sexp)))
                  (cond ((eq? first 'quote)
                         (make-literal-exp (second sexp)))
                        ((eq? first 'lambda)
                         (make-proc-exp (map make-var-exp (second sexp))
                                        (map parse-cexp (cddr sexp))))
                        ((eq? first 'if)
                         (make-if-exp (parse-cexp (second sexp))
                                      (parse-cexp (third sexp))
                                      (parse-cexp (fourth sexp))))
                        ((eq? first 'let)
                         (make-let-exp (map (lambda (pair)
                                              (make-binding (make-var-exp (car pair))
                                                            (parse-cexp (second pair))))
                                            (second sexp))
                                       (map parse-cexp (cddr sexp))))
                        ((eq? first 'let*)
                         (make-let*-exp (map (lambda (pair)
                                              (make-binding (make-var-exp (car pair))
                                                            (parse-cexp (second pair))))
                                            (second sexp))
                                       (map parse-cexp (cddr sexp))))
                        (else ;; app (rator . rands)
                         (make-app-exp (parse-cexp (car sexp))
                                       (map parse-cexp (cdr sexp))))
                         ))))
            ))


;; =============================================================================
;; Unparse

;; Purpose: Map an abstract syntax tree to a concrete syntax sexp.
;; Signature: unparse(exp)
;; Type: [Exp -> Sexp]
;; Example: (unparse (parse '(lambda (x) x))) => '(lambda (x) x)
(define unparse
  (lambda (exp)
    (cond ((def-exp? exp) (list 'define
                                (unparse (def-exp->var exp))
                                (unparse (def-exp->val exp))))
          ((cexp? exp)
           (cond ((num-exp? exp)  (num-exp->val exp))
                 ((bool-exp? exp) (bool-exp->val exp))
                 ((str-exp? exp)  (str-exp->val exp))
                 ((var-exp? exp)  (var-exp->var exp))
                 ((literal-exp? exp) (list 'quote (literal-exp->val exp)))
                 ((proc-exp? exp) (cons 'lambda
                                        (cons (map unparse (proc-exp->params exp))
                                              (map unparse (proc-exp->body exp)))))
                 ((if-exp? exp) (list 'if
                                      (unparse (if-exp->test exp))
                                      (unparse (if-exp->then exp))
                                      (unparse (if-exp->else exp))))
                 ((let-exp? exp) (cons 'let
                                       (cons (map (lambda (b) (list (unparse (binding->var b))
                                                                    (unparse (binding->val b))))
                                                  (let-exp->bindings exp))
                                             (map unparse (let-exp->body exp)))))
                 ((let*-exp? exp) (cons 'let*
                                       (cons (map (lambda (b) (list (unparse (binding->var b))
                                                                    (unparse (binding->val b))))
                                                  (let*-exp->bindings exp))
                                             (map unparse (let*-exp->body exp)))))
                 ((app-exp? exp) (cons (unparse (app-exp->rator exp))
                                       (map unparse (app-exp->rands exp))))
                 (else (error "Unknown exp type: " exp))))
          (else (error "Unknown exp type: " exp)))))
