#lang racket
(provide (all-defined-out))
(require "env.rkt"
         "L1-ast.rkt")

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; L1 applicative eval


;; Purpose: Evaluate an L1 program with applicative-eval algorithm
;; Signature: applicative-eval(exp,env)
;; Type: CExp * Env --> Value
;; Define Value Type: [Number | Boolean | Prim-op]
;; Example:
;; (applicative-eval '(app-exp (prim-op +) ((num-exp 1) (num-exp 2))) (make-empty-env)) => 3
;; (applicative-eval '(app-exp (prim-op +) ((num-exp 1) (var-ref x)))
;;                   (extend-env (make-empty-env) '(x) '(2))) => 3
(define L1-applicative-eval 
  (lambda (exp env)
    (cond ((num-exp? exp) (num-exp->val exp))
          ((bool-exp? exp) (bool-exp->val exp))
          ((prim-op? exp) exp)
          ((var-ref? exp) (apply-env env (var-ref->var exp)))
          ((app-exp? exp)
           (L1-apply-procedure (app-exp->rator exp)
                               (map (lambda (rand) (L1-applicative-eval rand env))
                                    (app-exp->rands exp))))
          (else (error "Bad L1 AST" exp)))))

;; Purpose: Apply a procedure to evaluated arguments.
;; Signature: apply-procedure(proc, args)
;; Type: [Value * List(Value) -> Value]
;; Pre-conditions: proc must be a prim-op value
;; Examples:
;; (apply-procedure '(prim-op +) '(1 2))  => 3
(define L1-apply-procedure
  (lambda (proc args)
    (cond ((prim-op? proc)
           (apply-primitive proc args))
          (else (error "Bad procedure" proc)))))

;; Purpose: Apply a primitive procedure to evaluated arguments.
;; Signature: apply-primitive(proc, args)
;; Type: [Prim-op * List(Value) -> Value]
;; Examples:
;; (apply-primitive '(prim-op +) '(1 2))  => 3
;; Note that +,-,*,/ are variadic - can take any number of args
;; which is implemented using foldr (aka reduce).
(define apply-primitive
  (lambda (prim-op args)
    (let ((op (prim-op->op prim-op)))
      (cond ((eq? op '+) (foldr + 0 args))
            ((eq? op '-) (foldr - 0 args))
            ((eq? op '*) (foldr * 1 args))
            ((eq? op '/) (foldr / 1 args))
            ((eq? op '>) (> (car args) (second args)))
            ((eq? op '<) (< (car args) (second args)))            
            ((eq? op '=) (= (car args) (second args)))
            ((eq? op 'not) (not (car args)))
            (else (error "Bad primitive op" op))))))

;; Purpose: evaluate a program made up of a sequence of expressions.
;; When def-exp expressions are executed, thread an updated env to the continuation.
;; For other expressions (that have no side-effect), execute the expressions sequentially.
;; Signature: eval-program(program)
;; Type: [Program -> Value]
;; Examples:
;; (eval-program (parseL1 '(L1 (define x 2) (* x x)))) => 4
;; (eval-program (parseL1 '(L1 (define x 2)))) => (void)
;; (eval-program (parseL1 '(L1 (define x 3) (* x x) (+ x x)))) => 6
(define eval-L1-program
  (lambda (program)
    (let ((env (make-empty-env))
          (exps (program->exps program)))
      (letrec
          ((loop (lambda (exps env)
                   (if (empty? exps)
                       (void)
                       (let ((exp (car exps)))
                         (cond ((def-exp? exp)
                                (let ((var (def-exp->var exp))
                                      (val (def-exp->val exp)))
                                  (loop (cdr exps)
                                        (extend-env env
                                                    (list (var-decl->var var))
                                                    (list (L1-applicative-eval val env))))))
                               ((empty? (cdr exps))
                                (L1-applicative-eval exp env))
                               (else
                                (let ((val (L1-applicative-eval exp env))) ;; Question: when is this needed?
                                  (loop (cdr exps) env)))))))))
        (loop exps env)))))

