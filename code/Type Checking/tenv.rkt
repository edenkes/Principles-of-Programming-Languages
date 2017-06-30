#lang racket
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Environment
;; ================
;; An environment represents a partial function from symbols (variable names) to type expressions.
;; It supports the operation: apply-tenv(tenv,var)
;; which either returns the type of var in the type-environment, or else throws an error.
;; 
;; TEnv is defined exactly as Env - except that we map vars to type-expressions instead of values.
;; * <tenv> ::= <empty-tenv> | <extended-tenv>
;; * <empty-tenv> ::= empty-tenv()
;; * <extended-tenv> ::= (tenv (symbol+) (type-exp+) enclosing-tenv) // env(vars:List(Symbol), tes:List(Type-exp), enclosing-tenv: TEnv)
;;
;; The key operation on tenv is apply-tenv(var) which returns the type expression associated to var in tenv
;; or throw an error if var is not defined in tenv.

;; empty-env value constructor
(define make-empty-tenv
  (lambda ()
    (list 'empty-tenv)))

(define empty-tenv?
  (lambda (x)
    (and (list? x) (= (length x) 1) (eq? (car x) 'empty-tenv))))

;; extend-tenv 
(define extend-tenv
  (lambda (enclosing-tenv vars tes)
    (list 'tenv vars tes enclosing-tenv)))
(define extend-tenv?
  (lambda (x)
    (and (list? x) (= (length x) 4) (eq? (car x) 'tenv))))
(define tenv->vars
  (lambda (tenv) (second tenv)))
(define tenv->tes
  (lambda (tenv) (third tenv)))
(define tenv->enclosing-tenv
  (lambda (tenv) (fourth tenv)))


;; Purpose: lookup the type of var in tenv.
;; Signature: apply-tenv(tenv, var)
;; Type: [TEnv * Symbol -> Type-expr]
(define apply-tenv
  (lambda (tenv var)
    (cond ((empty-tenv? tenv) (error "Var not found" var))
          ((extend-tenv? tenv)
           (let ((vars (tenv->vars tenv))
                 (tes (tenv->tes tenv))
                 (enclosing-tenv (tenv->enclosing-tenv tenv)))
             ;; If var not found in first frame, recurse on enclosing-tenv
             (let ((match (index-of vars var)))
               (if (number? match)
                   (list-ref tes match)
                   (apply-tenv enclosing-tenv var)))))
          (else (error "Unknown tenv type" tenv)))))