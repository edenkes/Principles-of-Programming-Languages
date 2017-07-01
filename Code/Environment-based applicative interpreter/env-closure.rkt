#lang racket
(provide (all-defined-out))

;; ##### Key change in the environment model: closures include an env field.
;; Purpose: Closure value constructor
;; Signature: make-closure(params, body)
;; Type: [List(Var-decl) * List(Cexp) * Env -> Closure]
(define make-closure
  (lambda (params body env)
    (list 'closure params body env)))

(define closure?
  (lambda (x)
    (and (list? x) (= (length x) 4) (eq? (car x) 'closure))))
(define closure->params
  (lambda (closure)
    (second closure)))
(define closure->body
  (lambda (closure)
    (third closure)))
(define closure->env
  (lambda (closure)
    (fourth closure)))
