#lang racket
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment
;; ===========
;; An environment represents a partial function from symbols (variable names) to values.
;; It supports the operation: apply-env(env,var)
;; which either returns the value of var in the environment, or else throws an error.
;; 
;; Env is defined inductively by the following recurrence relations:
;; * <env> ::= <empty-env> } <extended-env>
;; * <empty-env> ::= (empty-env) // empty-env()
;; * <extended-env> ::= (env (symbol+) (value+) env) // env(vars:List(Symbol), vals:List(Value), env: Env)
;;
;; The key operation on env is apply-env(var) which returns the value associated to var in env
;; or throw an error if var is not defined in env.

(define make-empty-env
  (lambda ()
    (list 'empty-env)))

(define empty-env?
  (lambda (x)
    (and (list? x) (= (length x) 1) (eq? (car x) 'empty-env))))

(define extend-env
  (lambda (env vars vals)
    (list 'env vars vals env)))

(define env?
  (lambda (x)
    (and (list? x) (= (length x) 4) (eq? (car x) 'env))))
(define env->vars
  (lambda (env) (second env)))
(define env->vals
  (lambda (env) (third env)))
(define env->env
  (lambda (env) (fourth env)))

(define apply-env
  (lambda (env var)
    (cond ((empty-env? env) (error "Var not found" var))
          ((env? env)
           (let ((vars (env->vars env))
                 (vals (env->vals env))
                 (next-env (env->env env)))
             (let ((match (index-of vars var)))
               (if (number? match)
                   (list-ref vals match)
                   (apply-env next-env var))))))))
