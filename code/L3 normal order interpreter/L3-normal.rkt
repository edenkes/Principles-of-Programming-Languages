#lang racket
(provide (all-defined-out))
(require "env.rkt"
         "L1-ast.rkt"
         "L2-ast.rkt"
         "L3-ast.rkt"
         "L2-eval.rkt"
         "L3-eval.rkt")	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; L3 normal eval

;; Purpose: Evaluate an L3 expression with normal-eval algorithm
;; Signature: L3-normal-eval(exp,env)
;; Type: CExp * Env -> Value
;; Define Value Type: [Number | Boolean | Prim-op | Closure | Sexp]
;; Example:
;; (L3-normal-eval '(app-exp (prim-op +) ((num-exp 1) (num-exp 2))) (make-empty-env)) => 3
;; (L3-normal-eval '(app-exp (prim-op +) ((num-exp 1) (var-ref x)))
;;                      (extend-env (make-empty-env) '(x) '(2))) => 3
(define L3-normal-eval 
  (lambda (exp env)
    (cond ((num-exp? exp) (num-exp->val exp))
          ((bool-exp? exp) (bool-exp->val exp))
          ((prim-op? exp) exp)
          ((var-ref? exp) (apply-env env (var-ref->var exp)))
          ((lit-exp? exp) (lit-exp->val exp))
          ((if-exp? exp)
           (if (true-value? (L3-normal-eval (if-exp->test exp) env))
               (L3-normal-eval (if-exp->then exp) env)
               (L3-normal-eval (if-exp->else exp) env)))
          ((lit-exp? exp) (lit-exp->val exp)) ;; ##### L3
          ((proc-exp? exp)
           (make-closure (proc-exp->params exp)
                         (proc-exp->body exp)))
          ;; This is the difference between applicative-eval and normal-eval
          ;; Substitute the arguments into the body without evaluating them first.
          ((app-exp? exp)
           (L3-normal-apply-procedure (L3-normal-eval (app-exp->rator exp) env)
                                      (app-exp->rands exp)
                                      env))
          (else (error "Bad L3 AST" exp)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Normal Order Application handling

;; Purpose: Apply a procedure to NON evaluated arguments.
;; Signature: L3-normal-apply-procedure(proc, args)
;; Type: [Value * List(CExp) -> Value]
;; Pre-conditions: proc must be a prim-op or a closure value
(define L3-normal-apply-procedure
  (lambda (proc args env)
    (cond ((prim-op? proc)
           (let ((arg-vals (map (lambda (arg) (L3-normal-eval arg env)) args)))
             (L3-apply-primitive proc arg-vals)))
          ((closure? proc)
           (let ((vars (map var-decl->var (closure->params proc)))
                 (body (L3-rename-exps (closure->body proc))))
             ;; Substitute non-evaluated args into the body
             (L3-normal-eval-sequence
              (L3-substitute body vars args) env)))
          (else (error "Bad procedure" proc)))))


;; Purpose: Evaluate a sequence of expressions
;; Signature: L3-normal-eval-sequence(exps, env)
;; Type: [List(CExp) * Env -> Value]
;; Pre-conditions: exps is not empty
(define L3-normal-eval-sequence
  (lambda (exps env)
    (cond ((empty? (cdr exps))
           (L3-normal-eval (car exps) env))
          (else (L3-normal-eval (car exps) env)
                (L3-normal-eval-sequence (cdr exps) env)))))

;; Purpose: evaluate a program made up of a sequence of expressions. (Same as in L1)
;; When def-exp expressions are executed, thread an updated env to the continuation.
;; For other expressions (that have no side-effect), execute the expressions sequentially.
;; Signature: normal-L3-program(program)
;; Type: [Program -> Value]
(define normal-L3-program
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
                                                    (list (L3-normal-eval val env))))))
                               ((empty? (cdr exps))
                                (L3-normal-eval exp env))
                               (else
                                (let ((val (L3-normal-eval exp env)))
                                  (loop (cdr exps) env)))))))))
        (loop exps env)))))




