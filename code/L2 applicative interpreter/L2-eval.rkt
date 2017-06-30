#lang racket
(provide (all-defined-out))
(require "env.rkt"
         "L1-ast.rkt"
         "L2-ast.rkt")

;; L2 AST
;; ======

;; <program> ::= (L1 <exp>+) // program(exps:List(exp))
;; <exp> ::= <define-exp> | <cexp>
;; <define-exp> ::= (define <var-decl> <cexp>) // def-exp(var:var-decl, val:cexp)
;; <cexp> ::= <num-exp> // num-exp(val:Number)
;;        | <bool-exp>  // bool-exp(val:Boolean)
;;        | <prim-op>   // prim-op(op:Symbol)
;;        | <var-ref>   // var-ref(var:Symbol)
;;        | <var-decl>  // var-decl(var:Symbol)
;;        | (if <exp> <exp> <exp>) // if-exp(test,then,else)                                   #####
;;        | (lambda (<var-decl>*) <cexp>+) // proc-exp(params:List(var-decl), body:List(cexp)) #####
;;        | (<cexp> <cexp>*) // app-exp(rator:cexp, rands:List(cexp))
;; <prim-op> ::= + | - | * | / | < | > | = | not
;; <num-exp> ::= a number token
;; <bool-exp> ::= #t | #f
;; <var-ref> ::= an identifier token
;; <var-decl> ::= an identifier token

;; L2 Values Type
;; Value = Number | Boolean | Prim-op | Void | Closure

;; Purpose: Closure value constructor
;; Signature: make-closure(params, body)
;; Type: [List(Var-decl) * List(Cexp) -> Closure]
(define make-closure
  (lambda (params body)
    (list 'closure params body)))

(define closure?
  (lambda (x)
    (and (list? x) (= (length x) 3) (eq? (car x) 'closure))))
(define closure->params
  (lambda (closure)
    (second closure)))
(define closure->body
  (lambda (closure)
    (third closure)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; L2 applicative eval


;; Purpose: Evaluate an L2 program with applicative-eval algorithm
;; Signature: applicative-eval(exp,env)
;; Type: CExp * Env --> Value
;; Define Value Type: [Number | Boolean | Prim-op]
;; Example:
;; (applicative-eval '(app-exp (prim-op +) ((num-exp 1) (num-exp 2))) (make-empty-env)) => 3
;; (applicative-eval '(app-exp (prim-op +) ((num-exp 1) (var-ref x)))
;;                   (extend-env (make-empty-env) '(x) '(2))) => 3
(define L2-applicative-eval 
  (lambda (exp env)
    (cond ((num-exp? exp) (num-exp->val exp))
          ((bool-exp? exp) (bool-exp->val exp))
          ((prim-op? exp) exp)
          ((var-ref? exp) (apply-env env (var-ref->var exp)))
          ((if-exp? exp)
           (if (true-value? (L2-applicative-eval (if-exp->test exp) env))
               (L2-applicative-eval (if-exp->then exp) env)
               (L2-applicative-eval (if-exp->else exp) env)))
          ((proc-exp? exp)
           (make-closure (proc-exp->params exp)
                         (proc-exp->body exp)))
          ((app-exp? exp)
           (L2-apply-procedure (L2-applicative-eval (app-exp->rator exp) env)
                               (map (lambda (rand) (L2-applicative-eval rand env))
                                    (app-exp->rands exp))
                               env))
          (else (error "Bad L2 AST" exp)))))

;; Purpose: Define what is considered a true value in an if-exp
;; Signature: true-value?(x)
;; Type: [Value -> Boolean]
(define true-value? 
  (lambda (x)
    (not (eq? x #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application handling

;; Purpose: Apply a procedure to evaluated arguments.
;; Signature: apply-procedure(proc, args)
;; Type: [Value * List(Value) -> Value]
;; Pre-conditions: proc must be a prim-op or a closure value
;; Examples:
;; (apply-procedure '(prim-op +) '(1 2))  => 3
(define L2-apply-procedure
  (lambda (proc args env)
    (cond ((prim-op? proc)
           (apply-primitive proc args))
          ((closure? proc)
           (let ((vars (map var-decl->var (closure->params proc)))
                 (body (rename-exps (closure->body proc))))
             (let ((lit-args (map value->lit-exp args)))
               (eval-sequence (substitute body vars lit-args) env))))
          (else (error "Bad procedure" proc)))))


;; Purpose: Evaluate a sequence of expressions
;; Signature: eval-sequence(exps, env)
;; Type: [List(CExp) * Env -> Value]
;; Pre-conditions: exps is not empty
(define eval-sequence
  (lambda (exps env)
    (cond ((empty? (cdr exps))
           (L2-applicative-eval (car exps) env))
          (else (L2-applicative-eval (car exps) env)
                (eval-sequence (cdr exps) env)))))

;; Purpose: Transform a value into a literal expression denoting this value
;; Signature: Value->Lit-exp(val)
;; Type: [Value -> Num-Exp | Bool-exp | Prim-op | Proc-exp]
;; Pre-conditions: val is not void
(define value->lit-exp
  (lambda (val)
    (cond ((number? val) (make-num-exp val))
          ((boolean? val) (make-bool-exp val))
          ((prim-op? val) val)
          ((closure? val) (make-proc-exp (closure->params val)
                                         (closure->body val)))
          (else (error "Bad arg value" val)))))

;; Purpose: Substitute all free occurrences of var-ref in vars in body with corresponding exp
;; Signature: substitute(body, vars, exps)
;; Type: [List(Cexp) * List(Symbol) * List(Cexp) -> List(Cexp)]
;; Pre-conditions: vars and exps have the same length
(define substitute
  (lambda (body vars exps)
    (letrec ((sub (lambda (exp)
                    (cond ((num-exp? exp) exp)
                          ((bool-exp? exp) exp)
                          ((prim-op? exp) exp)
                          ((var-ref? exp)
                           (let ((pos (index-of vars (var-ref->var exp))))
                             (if (number? pos)
                                 (list-ref exps pos)
                                 exp)))
                          ((if-exp? exp)
                           (make-if-exp (sub (if-exp->test exp))
                                        (sub (if-exp->then exp))
                                        (sub (if-exp->else exp))))
                          ((proc-exp? exp)
                           ;; Do not substitute bound variables
                           (let ((param-names (map var-decl->var (proc-exp->params exp)))
                                 (body  (proc-exp->body exp))
                                 (subst (map list vars exps)))
                               (let ((free-subst (filter (lambda (v-e) (not (index-of param-names (car v-e)))) subst)))
                                 (make-proc-exp (proc-exp->params exp)
                                                (substitute body (map car free-subst) (map second free-subst))))))
                          ((app-exp? exp)
                           (make-app-exp (sub (app-exp->rator exp))
                                         (map sub (app-exp->rands exp))))
                          (else (error "Bad exp" exp))))))
      (map sub body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rename

;; Purpose: generate a new symbol of the form varn
;; with n incremented at each call.
;; Reset the counter by passing #f
;; Signature: gen-var(var) or gen-var(#f)
;; Type: [Symbol -> Symbol] | [#f -> void]
;; Example:
;; (gen-var #f) -> (void)
;; (gen-var 'x) -> 'x1
(define gen-var
  (let ((count 0))
    (lambda (var)
      (cond ((symbol? var)
             (set! count (+ count 1))
             (string->symbol (~a var "__" count)))
            (else (set! count 0))))))


;; Signature: rename-exps(exp)
;; Purpose: Consistently rename bound variables in 'exps' to fresh names.
;;          Start numbering at 1 for all new var names.
;; Type: [List(CExp) -> CExp]
;; Example:
;; (rename-exps (list (parseL2 '(lambda (x) x)))) => (list (parseL2 '(lambda (x1) x1)))
(define rename-exps
  (letrec ((replace
            (lambda (exp)
              (cond ((num-exp? exp) exp)
                    ((bool-exp? exp) exp)
                    ((prim-op? exp) exp)
                    ((var-ref? exp) exp)
                    ((if-exp? exp)
                     (make-if-exp (replace (if-exp->test exp))
                                  (replace (if-exp->then exp))
                                  (replace (if-exp->else exp))))
                    ((app-exp? exp)
                     (make-app-exp (replace (app-exp->rator exp))
                                   (map replace (app-exp->rands exp))))
                    ;; Rename the params and substitute old params with renamed ones.
                    ;; First recursively rename all proc-exps inside the body.
                    ((proc-exp? exp)
                     (let ((old-params (map var-decl->var (proc-exp->params exp)))
                           (body (proc-exp->body exp)))
                       (let ((new-param-names (map gen-var old-params))
                             ;; Recurse on body
                             (renamed-body (map replace body))) 
                         (make-proc-exp (map make-var-decl new-param-names)
                                        (substitute renamed-body
                                                    old-params
                                                    (map make-var-ref new-param-names))))))))))
    (lambda (exps)
      ;; Reset the gen-var counter to 0 - so that the proc can be tested
      (gen-var #f)
      (map replace exps))))


;; Purpose: Apply a primitive procedure to evaluated arguments. Same as in L1.
;; Signature: apply-primitive(proc, args)
;; Type: [Prim-op * List(Value) -> Value]
(define apply-primitive
  (lambda (prim-op args)
    (let ((op (prim-op->op prim-op)))
      (cond ((eq? op '+) (reduce-ix + 0 args))
            ((eq? op '*) (reduce-ix * 1 args))
            ;; - and / must get at least one arg
            ((eq? op '-) (reduce-ix - (car args) (cdr args)))
            ((eq? op '/) (reduce-ix / (car args) (cdr args)))
            ((eq? op '>) (> (car args) (second args)))
            ((eq? op '<) (< (car args) (second args)))            
            ((eq? op '=) (= (car args) (second args)))
            ((eq? op 'not) (not (car args)))
            (else (error "Bad primitive op" op))))))

(define reduce-ix
  (lambda (f init xs)
    (if (empty? xs)
        init
        (reduce-ix f (f init (car xs)) (cdr xs)))))

;; Purpose: evaluate a program made up of a sequence of expressions. (Same as in L1)
;; When def-exp expressions are executed, thread an updated env to the continuation.
;; For other expressions (that have no side-effect), execute the expressions sequentially.
;; Signature: eval-program(program)
;; Type: [Program -> Value]
(define eval-L2-program
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
                                                    (list (L2-applicative-eval val env))))))
                               ((empty? (cdr exps))
                                (L2-applicative-eval exp env))
                               (else
                                (let ((val (L2-applicative-eval exp env)))
                                  (loop (cdr exps) env)))))))))
        (loop exps env)))))

