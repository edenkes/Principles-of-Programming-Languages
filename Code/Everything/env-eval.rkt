#lang racket
(provide (all-defined-out))
(require "env-env.rkt"
         "env-ast-new.rkt"
         "env-closure.rkt") ;; 


;; L4 AST
;; ======

;; L4 extends L3 with support for letrec

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
;;        | (quote <sexp>)   // lit-exp(val:Sexp)
;;        | (<cexp> <cexp>*) // app-exp(rator:cexp, rands:List(cexp))
;;        | (letrec (<binding>*) <cexp>+) // letrec-exp(bindings:List(binding), body:List(cexp)) 
;; <prim-op> ::= + | - | * | / | < | > | = | not |  eq?
;;        | cons | car | cdr | pair? | list? | number? | boolean? | symbol? | display | newline
;; ##### Note that we restrict letrec to bind procedures
;; <binding> ::= (<var-decl> <proc-exp>) // binding(var:var-decl, val:proc-exp)

;; L4 Values Type - same as L4
;; Value = Number | Boolean | Prim-op | Void | Closure | Sexp 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; L4 applicative eval with environment model

;; Purpose: Evaluate an L4 expression with applicative-eval algorithm and environment model
;; Signature: env-eval(exp,env)
;; Type: CExp * Env -> Value
;; Define Value Type: [Number | Boolean | Prim-op | Closure | Void | Sexp]
;; Example:
;; (env-eval '(app-exp (prim-op +) ((num-exp 1) (num-exp 2))) (make-empty-env)) => 3
;; (env-eval '(app-exp (prim-op +) ((num-exp 1) (var-ref x)))
;;           (extend-env (make-empty-env) '(x) '(2))) => 3
(define env-eval 
  (lambda (exp env)
    (cond ((num-exp? exp) (num-exp->val exp))
          ((bool-exp? exp) (bool-exp->val exp))
          ((prim-op? exp) exp)
          ((var-ref? exp) (apply-env env (var-ref->var exp)))
          ((lit-exp? exp) (lit-exp->val exp))
          ((if-exp? exp)
           (if (true-value? (env-eval (if-exp->test exp) env))
               (env-eval (if-exp->then exp) env)
               (env-eval (if-exp->else exp) env)))
          ((proc-exp? exp)                         ; ##### ENV: record the current env in closures
           (make-closure (proc-exp->params exp)
                         (proc-exp->body exp)
                         env))
          ;; #### LET: Direct evaluation rule without syntax expansion
          ;; compute the values, extend the env, eval the body.
          ((let-exp? exp)
           (let ((bindings (let-exp->bindings exp)))
             (let ((vars (map (lambda (b) (var-decl->var (binding->var b))) bindings))
                   (vals (map (lambda (b) (env-eval (binding->val b) env)) bindings)))
               (env-eval-sequence
                (let-exp->body exp)
                (extend-env env vars vals)))))
          ;; ##### LETREC: Support recursion with extend-rec-env
          ((letrec-exp? exp)
           (let ((bindings (letrec-exp->bindings exp)))
             (let ((vars (map (lambda (b) (var-decl->var (binding->var b))) bindings))
                   (paramss (map (lambda (b) (proc-exp->params (binding->val b))) bindings))
                   (bodies (map (lambda (b) (proc-exp->body (binding->val b))) bindings)))
               (env-eval-sequence
                (letrec-exp->body exp)
                (extend-rec-env env vars paramss bodies)))))
          ;; #### APP-EXP: apply-proc does not take the current env as param
          ((app-exp? exp)
           (env-apply-procedure (env-eval (app-exp->rator exp) env)  
                                (map (lambda (rand) (env-eval rand env))
                                     (app-exp->rands exp))))
          (else (error "Bad L4 AST" exp)))))


;; Purpose: Define what is considered a true value in an if-exp
;; Signature: true-value?(x)
;; Type: [Value -> Boolean]
(define true-value? 
  (lambda (x)
    (not (eq? x #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application handling

;; Purpose: Apply a procedure to evaluated arguments.
;; Signature: env-apply-procedure(proc, args)
;; Type: [Value * List(Value) -> Value]
;; Pre-conditions: proc must be a prim-op or a closure value
;; Examples:
;; (env-apply-procedure '(prim-op +) '(1 2))  => 3
(define env-apply-procedure
  (lambda (proc args)
    (cond ((prim-op? proc)
           (apply-primitive proc args))
          ((closure? proc)
           (let ((closure-env (closure->env proc))
                 (body (closure->body proc))
                 (params (closure->params proc)))
             ;; ##### Evaluate body in new environment
             ;; ##### extend the closure-env and not the current env.
             (env-eval-sequence body
                                (extend-env closure-env
                                            (map var-decl->var params)
                                            args))))
          (else (error "Bad procedure" proc)))))


;; Purpose: Evaluate a sequence of expressions
;; Signature: env-eval-sequence(exps, env)
;; Type: [List(CExp) * Env -> Value]
;; Pre-conditions: exps is not empty
(define env-eval-sequence
  (lambda (exps env)
    (cond ((empty? (cdr exps))
           (env-eval (car exps) env))
          (else (env-eval (car exps) env)
                (env-eval-sequence (cdr exps) env)))))

;; Purpose: Apply a primitive procedure to evaluated arguments.
;; Signature: apply-primitive(proc, args)
;; Type: [Prim-op * List(Value) -> Value]
(define apply-primitive
  (lambda (prim-op args)
    (let ((op (prim-op->op prim-op)))
      (cond ((eq? op '+) (reduce-ix + 0 args))
            ((eq? op '*) (reduce-ix * 1 args))
            ;; - and / must get at least one arg
            ((eq? op '-) (reduce-ix - 0 args))
            ((eq? op '/) (reduce-ix / 1 args))
            ((eq? op '>) (> (car args) (second args)))
            ((eq? op '<) (< (car args) (second args)))            
            ((eq? op '=) (= (car args) (second args)))
            ((eq? op 'not) (not (car args)))

            ;; ##### L3
            ((eq? op 'eq?) (eq? (car args) (second args)))
            ((eq? op 'cons) (cons (car args) (second args)))
            ((eq? op 'car) (car (car args)))
            ((eq? op 'cdr) (cdr (car args)))
            ((eq? op 'pair?) (pair? (car args)))
            ((eq? op 'list?) (list? (car args)))
            ((eq? op 'symbol?) (symbol? (car args)))
            ((eq? op 'number?) (number? (car args)))
            ((eq? op 'boolean?) (boolean? (car args)))

            ;; ##### L3 side effects
            ((eq? op 'display) (map display args) (void))
            ((eq? op 'newline) (newline))
            
            (else (error "Bad primitive op" op))))))

;; Reduce a list of values with a unary or binary operator f
;; in a way that reflects the behavior of Scheme primitives - and /
;; and also compatible with * and +
(define reduce-ix
  (lambda (f init xs)
    (letrec ((loop (lambda (current xs)
                     (if (empty? xs)
                         current
                         (loop (f current (car xs)) (cdr xs))))))
      (cond ((empty? xs) init)
            ((empty? (cdr xs)) (f (car xs)))
            (else (loop (car xs) (cdr xs)))))))

;; Purpose: evaluate a define expression and return a new env in which the new binding holds.
;; Signature: eval-define(def-exp, env)
;; Type: [Def-exp * Env -> Env]
(define eval-define
  (lambda (def-exp env)
    (let ((var (var-decl->var (def-exp->var def-exp)))
          (val (def-exp->val def-exp)))
      (if (proc-exp? val)
          (extend-rec-env env
                          (list var)
                          (list (proc-exp->params val))
                          (list (proc-exp->body val)))
          (let ((evaluated-val (env-eval val env)))
            (extend-env env (list var) (list evaluated-val)))))))

;; Purpose: evaluate a program made up of a sequence of expressions. (Same as in L1)
;; When def-exp expressions are executed, thread an updated env to the continuation.
;; For other expressions (that have no side-effect), execute the expressions sequentially.
;; Signature: env-eval-program(program)
;; Type: [Program -> Value]
(define env-eval-program
  (lambda (program)
    (let ((env (make-empty-env))
          (exps (program->exps program)))
      (letrec
          ((loop (lambda (exps env)
                   (if (empty? exps)
                       (void)
                       (let ((exp (car exps)))
                         (cond ((def-exp? exp)
                                (loop (cdr exps)
                                      (eval-define exp env)))
                               ((empty? (cdr exps))
                                (env-eval exp env))
                               (else
                                (let ((val (env-eval exp env)))
                                  (loop (cdr exps) env)))))))))
        (loop exps env)))))
