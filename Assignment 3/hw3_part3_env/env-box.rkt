#lang racket
(provide (all-defined-out))
(require "env-closure.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Box Environment
;; ===============
;; Represent an environment as a mapping from var to boxes containing values.
;; The global environment is the root of all extended environment.
;; It contains a frame that is initialized with primitive bindings
;; and can be extended with the define operator.
;;
;; Box-Env is defined inductively by the following cases:
;; * <box-env> ::= <global-env> | <extended-box-env>
;; * <global-env> ::= (global-env frame) // global-env(frame:Box(Frame))
;; * <extended-box-env> ::= (extended-box-env frame enclosing-env)
;;      // extended-box-env(vars:List(Symbol), frame: Frame)
;;
;; Frame:
;; * <fbinding> ::= (var val) // binding(var:Symbol, val:Box(Value))
;; * <frame> ::= (frame (var val)*) // frame(bindings:List(fbinding))
;; (apply-frame frame var) -> val
;;
;; The key operation on env is apply-env(var) which returns the value associated to var in env
;; or throw an error if var is not defined in env.


;; Frame Binding
;; =============
;; Purpose: Frame binding value constructor
;; Signature: make-fbinding(var,val)
;; Type: [Symbol * Value -> Fbinding]
(define make-fbinding
  (lambda (var val)
    (list var (box val))))
(define fbinding->var
  (lambda (fbinding)
    (first fbinding)))
(define fbinding->val
  (lambda (fbinding)
    (unbox (second fbinding))))
(define fbinding-set!
  (lambda (fbinding val)
    (set-box! (second fbinding) val)))

;; Frame
;; =====
;; Purpose: Value constructor for frame
;; Signature: make-frame(vars vals)
;; Type: [List(Symbol) * List(Value) -> Frame]
(define make-frame
  (lambda (vars vals)
    (list 'frame (map make-fbinding vars vals))))
;; Purpose: Value constructor for frame
;; Signature: extend-frame(frame,var,val)
;; Type: [Frame * Symbol * Value -> Frame]
(define extend-frame
  (lambda (frame var val)
    (list 'frame
          (cons (make-fbinding var val)
                (frame->fbindings frame)))))
(define frame?
  (lambda (x)
    (and (list? x) (= (length x) 2) (eq? (car x) 'frame))))
(define frame->fbindings
  (lambda (frame)
    (second frame)))
(define frame->vars
  (lambda (frame)
    (map fbinding->var (frame->fbindings frame))))
(define frame->vals
  (lambda (frame)
    (map fbinding->val (frame->fbindings frame))))

;; Purpose: lookup a variable in a frame - return #f if not found.
;; Signature: apply-frame(frame, var)
;; Type: [Frame * Symbol -> Fbinding | #f]
(define apply-frame
  (lambda (frame var)
    (assoc var (frame->fbindings frame))))

;; Purpose: Update the binding of a var to a new-value
;; Signature: frame-set-var!(frame, var, new-val)
;; Type: [Frame * Symbol * Value -> Void | #f]
(define frame-set-var!
  (lambda (frame var new-val)
    (let ((binding (apply-frame frame var)))
      (if binding
          (fbinding-set! binding new-val)
          #f))))

;; Environment
;; ===========

;; global-env - has a mutable frame - so that we can add bindings at any time.

;; Purpose: global-env value constructor
;; Signature: make-global-env()
;; Type: [Void -> Global-env]
(define make-global-env
  (lambda ()
    (list 'global-env
          (box (make-frame '() '())))))
(define global-env?
  (lambda (x)
    (and (list x) (= (length x) 2) (eq? (car x) 'global-env))))
;; Type: [Global-env -> Frame]
(define global-env->frame
  (lambda (ge)
    (unbox (second ge))))
;; Type: [Global-env * Frame -> Void]
(define global-env-set-frame!
  (lambda (ge new-frame)
    (set-box! (second ge) new-frame)))
     
;; There is a single mutable value in the type Global-env
(define the-global-env (make-global-env))

;; Purpose: Add a binding to the global env - possibly hide existing binding for the same value.
;; Signature: global-env-add-binding!(var,val)
;; Type: [Symbol * Value -> Void]
(define global-env-add-binding!
  (lambda (var val)
    (global-env-set-frame!
     the-global-env
     (extend-frame (global-env->frame the-global-env) var val))))

;; extend-env-box
(define extend-env-box
  (lambda (enclosing-env vars vals)
    (list 'env-box (make-frame vars vals) enclosing-env)))
(define env-box?
  (lambda (x)
    (and (list? x) (= (length x) 3) (eq? (car x) 'env-box))))
(define env-box->frame
  (lambda (env) (second env)))
(define env-box->vars
  (lambda (env) (map fbinding->var (env-box->frame env))))
(define env-box->vals
  (lambda (env) (map fbinding->val (env-box->frame env))))
(define env-box->enclosing-env
  (lambda (env) (third env)))

;; Purpose: lookup the value of var in env.
;; Signature: apply-env(env, var)
;; Type: [Env * Symbol -> Value]
(define apply-env
  (lambda (env var)
    (cond ((global-env? env)
           (let ((bdg (apply-frame (global-env->frame env) var)))
             (if bdg
                 (fbinding->val bdg)
                 (error "Var not found in GE: " var))))
          ((env-box? env)
           (let ((bdg (apply-frame (env-box->frame env) var))
                 (enclosing-env (env-box->enclosing-env env)))
             ;; If var not found in first frame, recurse on enclosing-env
             (if bdg
                 (fbinding->val bdg)
                 (apply-env enclosing-env var)))))))
