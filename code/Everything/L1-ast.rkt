#lang racket
(provide (all-defined-out))

;; L1 AST
;; ======

;; <program> ::= (L1 <exp>+) // program(exps:List(exp))
;; <exp> ::= <define-exp> | <cexp>
;; <define-exp> ::= (define <var-decl> <cexp>) // def-exp(var:var-decl, val:cexp)
;; <cexp> ::= <num-exp> // num-exp(val:Number)
;;        | <bool-exp>  // bool-exp(val:Boolean)
;;        | <prim-op>   // prim-op(op:Symbol)
;;        | <var-ref>   // var-ref(var:Symbol)
;;        | <var-decl>  // var-decl(var:Symbol)
;;        | (<cexp> <cexp>*) // app-exp(rator:cexp, rands:List(cexp))
;; <prim-op> ::= + | - | * | / | < | > | = | not
;; <num-exp> ::= a number token
;; <bool-exp> ::= #t | #f
;; <var-ref> ::= an identifier token
;; <var-decl> ::= an identifier token

;; Type: [List(exp) -> Program]
(define make-program
  (lambda (exps) (list 'program exps)))
(define program?
  (lambda (x) (and (list? x) (= (length x) 2) (eq? (car x) 'program))))
(define program->exps
  (lambda (program-exp) (second program-exp)))

(define exp?
  (lambda (x) (or (def-exp? x) (cexp? x))))

(define cexp?
  (lambda (x) (or (num-exp? x)
                  (bool-exp? x)
                  (prim-op? x)
                  (var-ref? x)
                  (var-decl? x)
                  (app-exp? x))))

;; Type: [var-decl * cexp -> def-exp]
(define make-def-exp
  (lambda (var val) (list 'def-exp var val)))
(define def-exp?
  (lambda (x) (and (list? x) (= (length x) 3) (eq? (car x) 'def-exp))))
(define def-exp->var
  (lambda (def-exp) (second def-exp)))
(define def-exp->val
  (lambda (def-exp) (third def-exp)))

;; Type: [Number -> num-exp]
(define make-num-exp
  (lambda (val) (list 'num-exp val)))
(define num-exp?
  (lambda (x) (and (list? x)(= (length x) 2) (eq? (car x) 'num-exp))))
(define num-exp->val
  (lambda (num-exp) (second num-exp)))

;; Type: [Boolean -> bool-exp]
(define make-bool-exp
  (lambda (val) (list 'bool-exp val)))
(define bool-exp?
  (lambda (x) (and (list? x)(= (length x) 2) (eq? (car x) 'bool-exp))))
(define bool-exp->val
  (lambda (bool-exp) (second bool-exp)))

;; Type: [Symbol -> prim-op]
(define make-prim-op
  (lambda (op) (list 'prim-op op)))
(define prim-op?
  (lambda (x) (and (list? x) (= (length x) 2) (eq? (car x) 'prim-op))))
(define prim-op->op
  (lambda (prim-op) (second prim-op)))

;; Type: [Symbol -> var-ref]
(define make-var-ref
  (lambda (var) (list 'var-ref var)))
(define var-ref?
  (lambda (x) (and (list? x) (= (length x) 2) (eq? (car x) 'var-ref))))
(define var-ref->var
  (lambda (var-ref) (second var-ref)))

;; Type: [Symbol -> var-decl]
(define make-var-decl
  (lambda (var) (list 'var-decl var)))
(define var-decl?
  (lambda (x) (and (list? x) (= (length x) 2) (eq? (car x) 'var-decl))))
(define var-decl->var
  (lambda (var-decl) (second var-decl)))

;; Type: [cexp * List(cexp) -> app-exp]
(define make-app-exp
  (lambda (rator rands) (list 'app-exp rator rands)))
(define app-exp?
  (lambda (x) (and (list? x) (= (length x) 3) (eq? (car x) 'app-exp))))
(define app-exp->rator
  (lambda (app-exp) (second app-exp)))
(define app-exp->rands
  (lambda (app-exp) (third app-exp)))


;; Parser
;; ======

;; Purpose: Recognize primitive operators as part of the syntax
;; Signature: primitive-op?(x)
;; Type: [Any -> Boolean]
(define primitive-op?
  (lambda (x) (or (eq? x '+)
                  (eq? x '-)
                  (eq? x '*)
                  (eq? x '/)
                  (eq? x '<)
                  (eq? x '>)
                  (eq? x '=)
                  (eq? x 'not))))

;; Purpose: Parse concrete syntax of L1 in sexp into L1-AST
;; Signature: parseL1(x)
;; Type: [SExp -> Program | Exp]
;; Examples:
;; (parseL1 '(L1 (define x 1) (+ x x))) =>
;;   '(program ((def-exp (var-decl x) (num-exp 1))
;;              (app-exp (prim-op +) ((var-ref x) (var-ref x)))))
(define parseL1
  (lambda (x)
    (cond ((empty? x) (error "Unexpected empty"))
          ((list? x)
           (cond ((eq? (car x) 'L1)
                  (make-program (map parseL1 (cdr x))))
                 ((eq? (car x) 'define)
                  (make-def-exp (make-var-decl (second x))
                                (parseL1 (third x))))
                 (else (make-app-exp (parseL1 (car x))
                                     (map parseL1 (cdr x))))))
          ((number? x) (make-num-exp x))
          ((boolean? x) (make-bool-exp x))
          ((primitive-op? x) (make-prim-op x))
          ((symbol? x) (make-var-ref x))
          (else (error "Unexpected type " x)))))

