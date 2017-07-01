#lang racket
(provide (all-defined-out))
(require "env-closure.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment
;; ===========
;; An environment represents a partial function from symbols (variable names) to values.
;; It supports the operation: apply-env(env,var)
;; which either returns the value of var in the environment, or else throws an error.
;; 
;; Env is defined inductively by the following cases:
;; * <env> ::= <empty-env> | <extended-env> | <rec-env>
;; * <empty-env> ::= (empty-env) // empty-env()
;; * <extended-env> ::= (env (symbol+) (value+) enclosing-env) // env(vars:List(Symbol), vals:List(Value), enclosing-env: Env)
;; * <rec-ext-env> ::= (rec-env (symbol+) (params+) (bodies+) enclosing-env)
;;       // rec-env(vars:List(Symbol), paramss:List(List(var-decl)), bodies:List(List(cexp)), enclosing-env: Env)
;;
;; The key operation on env is apply-env(var) which returns the value associated to var in env
;; or throw an error if var is not defined in env.

;; empty-env value constructor
(define make-empty-env
  (lambda ()
    (list 'empty-env)))

(define empty-env?
  (lambda (x)
    (and (list? x) (= (length x) 1) (eq? (car x) 'empty-env))))

;; extend-env 
(define extend-env
  (lambda (enclosing-env vars vals)
    (list 'env vars vals enclosing-env)))
(define extend-env?
  (lambda (x)
    (and (list? x) (= (length x) 4) (eq? (car x) 'env))))
(define env->vars
  (lambda (env) (second env)))
(define env->vals
  (lambda (env) (third env)))
(define env->enclosing-env
  (lambda (env) (fourth env)))

;; rec-env
(define extend-rec-env
  (lambda (enclosing-env vars paramss bodies)
    (list 'rec-env vars paramss bodies enclosing-env)))
(define rec-env?
  (lambda (x)
    (and (list? x) (= (length x) 5) (eq? (car x) 'rec-env))))
(define rec-env->vars
  (lambda (rec-env) (second rec-env)))
(define rec-env->paramss
  (lambda (rec-env) (third rec-env)))
(define rec-env->bodies
  (lambda (rec-env) (fourth rec-env)))
(define rec-env->enclosing-env
  (lambda (rec-env) (fifth rec-env)))

;; Purpose: lookup the value of var in env.
;; Signature: apply-env(env, var)
;; Type: [Env * Symbol -> Value]
(define apply-env
  (lambda (env var)
    (cond ((empty-env? env) (error "Var not found" var))
          ((extend-env? env)
           (let ((vars (env->vars env))
                 (vals (env->vals env))
                 (enclosing-env (env->enclosing-env env)))
             ;; If var not found in first frame, recurse on enclosing-env
             (let ((match (index-of vars var)))
               (if (number? match)
                   (list-ref vals match)
                   (apply-env enclosing-env var)))))
          ;; Recursive env - created by letrec and for global recursive procs.
          ;; The outer frame maps names of procedures to closures <params, body, env>
          ;; where the env is the environment itself.
          ;; To avoid the loop - we do not store the closures inside the env - but only
          ;; params and body - when apply-env finds the procedure given its name, we construct
          ;; the closure with a reference to the env itself.
          ((rec-env? env)
           (let ((vars (rec-env->vars env)))
             (let ((match (index-of vars var)))
               (if (number? match)
                   (let ((params (list-ref (rec-env->paramss env) match))
                         (body (list-ref (rec-env->bodies env) match)))
                     (make-closure params body env))
                   (apply-env (rec-env->enclosing-env env) var))))))))
