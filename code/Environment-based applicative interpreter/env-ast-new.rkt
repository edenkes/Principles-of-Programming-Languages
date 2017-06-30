#lang racket
(provide (all-defined-out))

;; L4 AST
;; ======

;; <program> ::= (L4 <exp>+) // program(exps:List(exp))
;; <exp> ::= <define-exp> | <cexp>
;; <define-exp> ::= (define <var-decl> <cexp>) // def-exp(var:var-decl, val:cexp)
;; <cexp> ::= <num-exp> // num-exp(val:Number)
;;        | <bool-exp>  // bool-exp(val:Boolean)
;;        | <prim-op>   // prim-op(op:Symbol)
;;        | <var-ref>   // var-ref(var:Symbol)
;;        | <var-decl>  // var-decl(var:Symbol)
;;        | (if <exp> <exp> <exp>) // if-exp(test,then,else)
;;        | (lambda (<var-decl>*) <cexp>+) // proc-exp(params:List(var-decl), body:List(cexp))
;;        | (quote <sexp>) // lit-exp(val:Sexp)
;;        | (let (<binding>*) <cexp>+) // let-exp(bindings:List(binding), body:List(cexp)) 
;;        | (letrec (<binding>*) <cexp>+) // letrec-exp(bindings:List(binding), body:List(cexp)) 
;;        | (<cexp> <cexp>*) // app-exp(rator:cexp, rands:List(cexp))
;; <prim-op> ::= + | - | * | / | < | > | = | not |  eq?
;;        | cons | car | cdr | pair? | list? | number? | boolean? | symbol? | display | newline
;; <num-exp> ::= a number token
;; <bool-exp> ::= #t | #f
;; <var-ref> ::= an identifier token
;; <var-decl> ::= an identifier token
;; <sexp> ::= a symbol token | ( <sexp>* )
;; <binding> ::= ( <var-decl> <proc-exp> ) // Binding(var:var-decl, val:proc-exp)


;; Type: [List(exp) -> Program]
(define make-program
  (lambda (exps) (list 'program exps)))
(define program?
  (lambda (x) (and (list? x) (= (length x) 2) (eq? (car x) 'program))))
(define program->exps
  (lambda (program-exp) (second program-exp)))

(define exp?
  (lambda (x) (or (def-exp? x) (cexp? x))))

;; Type: [Any -> Boolean]
(define cexp?
  (lambda (x) (or (num-exp? x)
                  (bool-exp? x)
                  (prim-op? x)
                  (var-ref? x)
                  (var-decl? x)
                  (if-exp? x)
                  (proc-exp? x)
                  (lit-exp? x)
                  (let-exp? x)
                  (letrec-exp? x)
                  (app-exp? x)
                  (set!-exp? x))))

;; def-exp
;; Type: [var-decl * cexp -> def-exp]
(define make-def-exp
  (lambda (var val) (list 'def-exp var val)))
(define def-exp?
  (lambda (x) (and (list? x) (= (length x) 3) (eq? (car x) 'def-exp))))
(define def-exp->var
  (lambda (def-exp) (second def-exp)))
(define def-exp->val
  (lambda (def-exp) (third def-exp)))

;; num-exp
;; Type: [Number -> num-exp]
(define make-num-exp
  (lambda (val) (list 'num-exp val)))
(define num-exp?
  (lambda (x) (and (list? x)(= (length x) 2) (eq? (car x) 'num-exp))))
(define num-exp->val
  (lambda (num-exp) (second num-exp)))

;; bool-exp
;; Type: [Boolean -> bool-exp]
(define make-bool-exp
  (lambda (val) (list 'bool-exp val)))
(define bool-exp?
  (lambda (x) (and (list? x)(= (length x) 2) (eq? (car x) 'bool-exp))))
(define bool-exp->val
  (lambda (bool-exp) (second bool-exp)))

;; prim-op
;; Type: [Symbol -> prim-op]
(define make-prim-op
  (lambda (op) (list 'prim-op op)))
(define prim-op?
  (lambda (x) (and (list? x) (= (length x) 2) (eq? (car x) 'prim-op))))
(define prim-op->op
  (lambda (prim-op) (second prim-op)))

;; var-ref
;; Type: [Symbol -> var-ref]
(define make-var-ref
  (lambda (var) (list 'var-ref var)))
(define var-ref?
  (lambda (x) (and (list? x) (= (length x) 2) (eq? (car x) 'var-ref))))
(define var-ref->var
  (lambda (var-ref) (second var-ref)))

;; var-decl
;; Type: [Symbol -> var-decl]
(define make-var-decl
  (lambda (var) (list 'var-decl var)))
(define var-decl?
  (lambda (x) (and (list? x) (= (length x) 2) (eq? (car x) 'var-decl))))
(define var-decl->var
  (lambda (var-decl) (second var-decl)))

;; app-exp
;; Type: [cexp * List(cexp) -> app-exp]
(define make-app-exp
  (lambda (rator rands) (list 'app-exp rator rands)))
(define app-exp?
  (lambda (x) (and (list? x) (= (length x) 3) (eq? (car x) 'app-exp))))
(define app-exp->rator
  (lambda (app-exp) (second app-exp)))
(define app-exp->rands
  (lambda (app-exp) (third app-exp)))

;; if-exp
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

;; proc-exp
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

;; lit-exp
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

;; let-exp
;; Type: [List(binding) * List(cexp) -> letrec-exp]
(define make-let
  (lambda (bindings body)
    (list 'let-exp bindings body)))
(define let-exp?
  (lambda (x)
    (and (list? x) (= (length x) 3) (eq? (car x) 'let-exp))))
(define let-exp->bindings
  (lambda (x) (second x)))
(define let-exp->body
  (lambda (x) (third x)))

;; letrec-exp
;; Type: [List(binding) * List(cexp) -> letrec-exp]
(define make-letrec
  (lambda (bindings body)
    (list 'letrec-exp bindings body)))
(define letrec-exp?
  (lambda (x)
    (and (list? x) (= (length x) 3) (eq? (car x) 'letrec-exp))))
(define letrec-exp->bindings
  (lambda (x) (second x)))
(define letrec-exp->body
  (lambda (x) (third x)))

;; binding
;; Type: [Var-decl * proc-exp -> Binding]
(define make-binding
  (lambda (var val)
    (list 'binding var val)))
(define binding?
  (lambda (x)
    (and (list? x) (= (length x) 3) (eq? (car x) 'binding))))
(define binding->var
  (lambda (x) (second x)))
(define binding->val
  (lambda (x) (third x)))

;;set!
;;Type: [var-ref * cexp -> set!-exp]
(define make-set!-exp
  (lambda (var val)
    (list 'set! var val)))
(define set!-exp?
  (lambda (x)
    (and (list? x) (= (length x) 3) (eq? (car x) 'set!))))
(define set!->var
  (lambda (x) (second x)))
(define set!->val
  (lambda (x) (third x)))

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

;; Purpose: Parse concrete syntax of L4 in sexp into L4-AST
;; Signature: parseL4(x)
;; Type: [SExp -> Program | Exp]
;; Examples:
;; (parseL4 '(L4 (define f (lambda (x) (+ x x))) (f 2))) =>
;;   '(program ((def-exp (var-decl f) (proc-exp ((var-decl x)) ((app-exp (prim-op +) ((var-ref x) (var-ref x)))))
;;              (app-exp (var-ref f) ((num-exp 2)))))
(define parseL4
  (lambda (x)
    (cond ((empty? x) (make-lit-exp x)) ; ##### L3
          ((list? x)
           (cond ((eq? (car x) 'L4)
                  (make-program (map parseL4 (cdr x))))
                 ((eq? (car x) 'define)
                  (make-def-exp (make-var-decl (second x))
                                (parseL4 (third x))))
                 ((eq? (car x) 'if)
                  (make-if-exp (parseL4 (second x))
                               (parseL4 (third x))
                               (parseL4 (fourth x))))
                 ((eq? (car x) 'lambda)
                  (make-proc-exp (map make-var-decl (second x))
                                 (map parseL4 (cddr x))))
                 ((eq? (car x) 'let)
                  (make-let (map make-binding
                                 (map (lambda (b) (make-var-decl (car b))) (second x))
                                 (map (lambda (b) (parseL4 (second b))) (second x)))
                            (map parseL4 (cddr x))))
                 ((eq? (car x) 'letrec)
                  (make-letrec (map make-binding
                                    (map (lambda (b) (make-var-decl (car b))) (second x))
                                    (map (lambda (b) (parseL4 (second b))) (second x)))
                               (map parseL4 (cddr x))))
                 ((eq? (car x) 'quote)
                  (make-lit-exp (second x)))
                 ((eq? (car x) 'set!)
                  (make-set!-exp (make-var-ref (second x))
                                 (parseL4 (third x))))
                 (else (make-app-exp (parseL4 (car x))
                                     (map parseL4 (cdr x))))))
          ((number? x) (make-num-exp x))
          ((boolean? x) (make-bool-exp x))
          ((primitive-op? x) (make-prim-op x))
          ((symbol? x) (make-var-ref x))
          (else (error "Unexpected type " x)))))

