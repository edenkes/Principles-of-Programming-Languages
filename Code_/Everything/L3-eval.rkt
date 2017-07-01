#lang racket
(provide (all-defined-out))
(require "env.rkt"
         "L1-ast.rkt"
         "L2-ast.rkt"
         "L3-ast.rkt"
         "L2-eval.rkt") ;; closure, substitute, rename-exps

;; L3 AST
;; ======

;; L3 extends L2 with support for:
;; - Pair and List datatypes
;; - Compound literal expressions denoted with quote
;; - Primitives: cons, car, cdr, pair?, list?, eq?, number?, symbol?, boolean?
;; - The empty-list literal expression

;; <program> ::= (L3 <exp>+) // program(exps:List(exp))
;; <exp> ::= <define-exp> | <cexp>
;; <define-exp> ::= (define <var-decl> <cexp>) // def-exp(var:var-decl, val:cexp)
;; <cexp> ::= <num-exp> // num-exp(val:Number)
;;        | <bool-exp>  // bool-exp(val:Boolean)
;;        | <prim-op>   // prim-op(op:Symbol)
;;        | <var-ref>   // var-ref(var:Symbol)
;;        | <var-decl>  // var-decl(var:Symbol)
;;        | (if <exp> <exp> <exp>) // if-exp(test,then,else)                                   ##### L2
;;        | (lambda (<var-decl>*) <cexp>+) // proc-exp(params:List(var-decl), body:List(cexp)) ##### L2
;;        | (quote <sexp>) // lit-exp(val:Sexp)                                                ##### L3
;;        | (<cexp> <cexp>*) // app-exp(rator:cexp, rands:List(cexp))
;; <prim-op> ::= + | - | * | / | < | > | = | not |  eq?
;;        | cons | car | cdr | pair? | list? | number? | boolean? | symbol?                    ##### L3
;;        | display | newline

;; L3 Values Type
;; Value = Number | Boolean | Prim-op | Void | Closure | Sexp 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; L3 applicative eval

;; Purpose: Evaluate an L3 expression with applicative-eval algorithm
;; Signature: L3-applicative-eval(exp,env)
;; Type: CExp * Env -> Value
;; Define Value Type: [Number | Boolean | Prim-op | Closure | Sexp]
;; Example:
;; (L3-applicative-eval '(app-exp (prim-op +) ((num-exp 1) (num-exp 2))) (make-empty-env)) => 3
;; (L3-applicative-eval '(app-exp (prim-op +) ((num-exp 1) (var-ref x)))
;;                      (extend-env (make-empty-env) '(x) '(2))) => 3
(define L3-applicative-eval 
  (lambda (exp env)
    (cond ((num-exp? exp) (num-exp->val exp))
          ((bool-exp? exp) (bool-exp->val exp))
          ((prim-op? exp) exp)
          ((var-ref? exp) (apply-env env (var-ref->var exp)))
          ((lit-exp? exp) (lit-exp->val exp))
          ((if-exp? exp)
           (if (true-value? (L3-applicative-eval (if-exp->test exp) env))
               (L3-applicative-eval (if-exp->then exp) env)
               (L3-applicative-eval (if-exp->else exp) env)))
          ((lit-exp? exp) (lit-exp->val exp)) ;; ##### L3
          ((proc-exp? exp)
           (make-closure (proc-exp->params exp)
                         (proc-exp->body exp)))
          ((app-exp? exp)
           (L3-apply-procedure (L3-applicative-eval (app-exp->rator exp) env)
                               (map (lambda (rand) (L3-applicative-eval rand env))
                                    (app-exp->rands exp))
                               env))
          (else (error "Bad L3 AST" exp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application handling

;; Purpose: Apply a procedure to evaluated arguments.
;; Signature: L3-apply-procedure(proc, args)
;; Type: [Value * List(Value) -> Value]
;; Pre-conditions: proc must be a prim-op or a closure value
;; Examples:
;; (L3-apply-procedure '(prim-op +) '(1 2))  => 3
(define L3-apply-procedure
  (lambda (proc args env)
    (cond ((prim-op? proc)
           (L3-apply-primitive proc args))
          ((closure? proc)
           (let ((vars (map var-decl->var (closure->params proc)))
                 (body (L3-rename-exps (closure->body proc))))
             (let ((lit-args (map L3-value->lit-exp args)))
               (L3-eval-sequence (L3-substitute body vars lit-args) env))))
          (else (error "Bad procedure" proc)))))


;; Purpose: Evaluate a sequence of expressions
;; Signature: L3-eval-sequence(exps, env)
;; Type: [List(CExp) * Env -> Value]
;; Pre-conditions: exps is not empty
(define L3-eval-sequence
  (lambda (exps env)
    (cond ((empty? (cdr exps))
           (L3-applicative-eval (car exps) env))
          (else (L3-applicative-eval (car exps) env)
                (L3-eval-sequence (cdr exps) env)))))

;; Purpose: Transform a value into a literal expression denoting this value
;; Signature: L3-Value->Lit-exp(val)
;; Type: [Value -> Num-Exp | Bool-exp | Prim-op | Proc-exp | Lit-exp]
;; Pre-conditions: val is not void
(define L3-value->lit-exp
  (lambda (val)
    (cond ((number? val) (make-num-exp val))
          ((boolean? val) (make-bool-exp val))
          ((prim-op? val) val)
          ((closure? val) (make-proc-exp (closure->params val)
                                         (closure->body val)))
          (else (make-lit-exp val)))))  ; ##### L3

;; Purpose: Substitute all free occurrences of var-ref in vars in body with corresponding exp
;; Signature: L3-substitute(body, vars, exps)
;; Type: [List(Cexp) * List(Symbol) * List(Cexp) -> List(Cexp)]
;; Pre-conditions: vars and exps have the same length
;;                 and all bound variables have been renamed in body.
(define L3-substitute
  (lambda (body vars exps)
    (letrec ((sub (lambda (exp)
                    (cond ((num-exp? exp) exp)
                          ((bool-exp? exp) exp)
                          ((prim-op? exp) exp)
                          ((lit-exp? exp) exp)  ; ##### L3
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
                                                (L3-substitute body (map car free-subst) (map second free-subst))))))
                          ((app-exp? exp)
                           (make-app-exp (sub (app-exp->rator exp))
                                         (map sub (app-exp->rands exp))))
                          (else (error "Bad exp" exp))))))
      (map sub body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Renaming

;; Signature: L3-rename-exps(exp)
;; Purpose: Consistently rename bound variables in 'exps' to fresh names.
;;          Start numbering at 1 for all new var names.
;; Type: [List(CExp) -> CExp]
;; Example:
;; (L3-rename-exps (list (parseL3 '(lambda (x) x)))) => (list (parseL3 '(lambda (x1) x1)))
(define L3-rename-exps
  (letrec ((replace
            (lambda (exp)
              (cond ((num-exp? exp) exp)
                    ((bool-exp? exp) exp)
                    ((prim-op? exp) exp)
                    ((var-ref? exp) exp)
                    ((lit-exp? exp) exp)   ; ##### L3
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
                                        (L3-substitute renamed-body            ; ##### L3
                                                       old-params
                                                       (map make-var-ref new-param-names))))))))))
    (lambda (exps)
      ;; Reset the gen-var counter to 0 - so that the proc can be tested
      (gen-var #f)
      (map replace exps))))


;; Purpose: Apply a primitive procedure to evaluated arguments.
;; Signature: L3-apply-primitive(proc, args)
;; Type: [Prim-op * List(Value) -> Value]
(define L3-apply-primitive
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

;; Purpose: evaluate a program made up of a sequence of expressions. (Same as in L1)
;; When def-exp expressions are executed, thread an updated env to the continuation.
;; For other expressions (that have no side-effect), execute the expressions sequentially.
;; Signature: eval-program(program)
;; Type: [Program -> Value]
(define eval-L3-program
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
                                                    (list (L3-applicative-eval val env))))))
                               ((empty? (cdr exps))
                                (L3-applicative-eval exp env))
                               (else
                                (let ((val (L3-applicative-eval exp env)))
                                  (loop (cdr exps) env)))))))))
        (loop exps env)))))




