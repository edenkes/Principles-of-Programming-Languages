#lang racket
(provide (all-defined-out))
(require "env-box.rkt"
         "env-ast.rkt"
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
;; <binding> ::= (<var-decl> <exp>) // binding(var:var-decl, val:cexp)

;; L4 Values Type - same as L3
;; Value = Number | Boolean | Prim-op | Void | Closure | Sexp 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; L4 applicative eval with box environment model

;; Purpose: Evaluate an L4 expression with applicative-eval algorithm and box environment model
;; Signature: env-box-eval(exp,env)
;; Type: CExp * Env -> Value
;; Define Value Type: [Number | Boolean | Prim-op | Closure | Void | Sexp]
(define env-box-eval 
  (lambda (exp env)
    (cond ((num-exp? exp) (num-exp->val exp))
          ((bool-exp? exp) (bool-exp->val exp))
          ((prim-op? exp) exp)
          ((var-ref? exp) (apply-env env (var-ref->var exp)))
          ((lit-exp? exp) (lit-exp->val exp))
          ((set!-exp? exp)
           (cond ((global-env? env)
                  (let ((bdg (apply-frame (global-env->frame env) (var-ref->var (set!->var exp))))
                        (new-val (env-box-eval (set!->val exp) env)))
                    (if bdg
                        (fbinding-set! bdg new-val)
                        #f)))
                 ((env-box? env)
                  (let ((bdg (apply-frame (env-box->frame env) (var-ref->var (set!->var exp))))
                        (enclosing-env (env-box->enclosing-env env))
                        (new-val (env-box-eval (set!->val exp) env)))
                    (if bdg
                        (fbinding-set! bdg new-val)
                        (env-box-eval exp enclosing-env))))))
          ((if-exp? exp)
           (if (true-value? (env-box-eval (if-exp->test exp) env))
               (env-box-eval (if-exp->then exp) env)
               (env-box-eval (if-exp->else exp) env)))
          ((proc-exp? exp)
           (make-closure (proc-exp->params exp)
                         (proc-exp->body exp)
                         env))
          ;; LET: Direct evaluation rule without syntax expansion
          ;; compute the values, extend the env, eval the body.
          ((let-exp? exp)
           (let ((bindings (let-exp->bindings exp)))
             (let ((vars (map (lambda (b) (var-decl->var (binding->var b))) bindings))
                   (vals (map (lambda (b) (env-box-eval (binding->val b) env)) bindings)))
               (env-box-eval-sequence
                (let-exp->body exp)
                (extend-env-box env vars vals)))))
          ;; LETREC: Support recursion
          ;; 1. extend the env with vars initialized to void (temporary value)
          ;; 2. compute the vals in the new extended env
          ;; 3. update the bindings of the vars to the computed vals
          ;; 4. compute body in extended env
          ((letrec-exp? exp)
           (let ((bindings (letrec-exp->bindings exp)))
             (let ((vars (map (lambda (b) (var-decl->var (binding->var b))) bindings))
                   (vals (map binding->val bindings)))
               (let ((extended-env (extend-env-box env vars (map void vars))))
                 (let ((cvals (map (lambda (val) (env-box-eval val extended-env)) vals))
                       (frame (env-box->frame extended-env)))
                   ;; Update the bindings of the current frame
                   (for-each (lambda (var val) (frame-set-var! frame var val))
                             vars cvals)
                   (env-box-eval-sequence (letrec-exp->body exp) extended-env))))))
          ;; APP-EXP
          ((app-exp? exp)
           (env-box-apply-procedure (env-box-eval (app-exp->rator exp) env)  
                                    (map (lambda (rand) (env-box-eval rand env))
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
(define env-box-apply-procedure
  (lambda (proc args)
    (cond ((prim-op? proc)
           (apply-primitive proc args))
          ((closure? proc)
           (let ((closure-env (closure->env proc))
                 (body (closure->body proc))
                 (params (closure->params proc)))
             ;; Evaluate body in new environment
             ;; extend the closure-env and not the current env (as would be in dynamic scoping)
             (env-box-eval-sequence
              body
              (extend-env-box closure-env
                              (map var-decl->var params)
                              args))))
          (else (error "Bad procedure" proc)))))


;; Purpose: Evaluate a sequence of expressions
;; Signature: env-eval-sequence(exps, env)
;; Type: [List(CExp) * Env -> Value]
;; Pre-conditions: exps is not empty
(define env-box-eval-sequence
  (lambda (exps env)
    (cond ((empty? (cdr exps))
           (env-box-eval (car exps) env))
          (else (env-box-eval (car exps) env)
                (env-box-eval-sequence (cdr exps) env)))))

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

;; Purpose: evaluate a define expression and mutate the-global-env
;; Signature: eval-box-define(def-exp)
;; Type: [Def-exp -> Void]
(define eval-box-define
  (lambda (def-exp)
    (let ((var (var-decl->var (def-exp->var def-exp)))
          (val (def-exp->val def-exp)))
      (let ((cval (env-box-eval val the-global-env)))
        (global-env-add-binding! var cval)))))

;; Purpose: evaluate a program made up of a sequence of expressions.
;; Signature: env-box-eval-program(program)
;; Type: [Program -> Value]
(define env-box-eval-program
  (lambda (program)
    ;; Reinitialize the-global-env
    (global-env-set-frame! the-global-env (make-frame '() '()))
    (let ((exps (program->exps program)))
      (letrec
          ((eval (lambda (exp)
                   (if (def-exp? exp)
                       (eval-box-define exp)
                       (env-box-eval exp the-global-env))))
           (loop (lambda (exps)
                   (cond ((empty? exps) (void))
                         ((empty? (cdr exps))
                          (eval (car exps)))
                         (else (eval (car exps))
                               (loop (cdr exps)))))))
        (loop exps)))))
