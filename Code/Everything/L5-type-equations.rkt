#lang racket

(require "L5-ast.rkt"
         "L5-substitution-adt.rkt")
(provide (all-defined-out))

;; ============================================================n
;; Pool ADT
;; A pool represents a map from L5-exp to Type-expressions.
;; It is implemented as a list of pairs (L5-exp TExp).
;; When a new L5-exp is added to a pool, a fresh Tvar
;; is allocated for it.

;; Type: [Empty -> Pool]
(define make-empty-pool
  (lambda () '()))

(define empty-pool? empty?)

;; Purpose: construct a pool with one additional pair
;;          (L5-exp fresh-tvar)
;; Type: [L5-exp * Pool -> Pool]
;; Precondition: L5-exp is not already in pool.
(define extend-pool
  (lambda (L5-exp pool)
    (cons (list L5-exp (make-fresh-tvar)) pool)))

;; Purpose: construct a pool with one additional pair
;;          ((var-ref var) texp)
;;          from a (var-decl var texp) declaration.
;; Type: [Var-decl * Pool -> Pool]
;; Precondition: var is not already in pool - which means
;; that all bound variables have been renamed with distinct names.
(define extend-pool-var-decl
  (lambda (var-decl pool)
    (cons (list (make-var-ref (var-decl->var var-decl))
                (var-decl->texp var-decl))
          pool)))

;; Type: [L5-exp * Pool -> Boolean]
(define element-of-pool?
  (lambda(exp pool)
    (assoc exp pool)))
(define pool->exp first)
(define pool->tvar second)

;; Map a function over a list of expressions to accumulate
;; matching sub-expressions into a pool.
;; Signature: map-pool(fun, exp-list, result)
;; Type: [(Scheme-exp * Pool -> Pool) * List(Scheme-exp) * Pool -> Pool]
;; fun should construct a new pool given a new expression from exp-list
;; that has not yet been seen before.
(define map-pool
  (lambda (fun exp-list result)
    (if (null? exp-list)
        result
        (map-pool fun
                  (cdr exp-list)
                  (if (element-of-pool? (car exp-list) result)
                      result
                      (fun (car exp-list) result))))))


;; Type: [L5-exp -> Pool]
;; Purpose: Traverse the abstract syntax tree L5-exp
;;          and collect all sub-expressions into a Pool of fresh type variables.
;; Example:
;; (L5-exp->pool (parseL5 '(+ x 1))) =>
;; '(((app-exp (prim-op +) ((var-ref x) (num-exp 1))) (tvar T252722 #&#f))
;;  ((num-exp 1) (tvar T252721 #&#f))
;;  ((var-ref x) (tvar T252720 #&#f))
;;  ((prim-op +) (tvar T252719 #&#f)))
(define L5-exp->pool
  (lambda (exp)
    (letrec
        ((findVarList
          (lambda (exp var-pool)
            (cond ((null? exp) (make-empty-pool))
                  ((var-decl? exp) (extend-pool-var-decl exp var-pool))
                  ((cexp-atomic? exp)
                   (extend-pool exp var-pool))
                  ((proc-exp? exp)
                   (extend-pool
                    exp
                    (map-pool findVarList
                              (append (proc-exp->params exp)
                                      (proc-exp->body exp))
                              var-pool)))
                  ((cexp-composite? exp)
                   (extend-pool
                    exp
                    (map-pool findVarList
                              (cexp-composite->components exp)
                              var-pool)))
                  (else (error 'findVarList "Bad L5 exp ~s" exp))))))
      (findVarList exp (make-empty-pool)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constructor for equations for a Scheme expression:
;;; this constructor implements the second step of the type-inference-equations
;;; algorithm -- derive equations for all composite sub expressions of a
;;; given L5 expression. Its input is a pool of pairs (L5-exp Tvar).
;;; A Scheme expression is mapped to a pool with L5-exp->pool

;; Signature: pool->equations(pool)
;; Purpose: Return a set of equations for a given L5-exp
;; and a list of pairs: (Sub-expression Tvar)
;; Type: [Pool -> List(Equation)]
;; Example: (pool->equations
;;            (L5-exp->pool (parseL5 '(+ x 1))))
;;          ==> '(((app-exp (prim-op +) ((var-ref x) (num-exp 1))) (tvar T275460 #&#f))
;;                ((num-exp 1) (tvar T275459 #&#f))
;;                ((var-ref x) (tvar T275458 #&#f))
;;                ((prim-op +) (tvar T275457 #&#f)))
;; Pre-condition: pool is the result of (L5-exp->pool exp)
(define pool->equations
  (lambda (pool)
    (let ((pool-without-vars
           ;; var-refs generate no equations beyond that of var-decl - remove them
           (filter (lambda (exp-tvar)
                     (not (var-ref? (pool->exp exp-tvar))))
                   pool)))
      (map (lambda (exp) (make-equation-from-exp exp pool))
           (map pool->exp pool-without-vars)))))

;; Signature: make-equation-from-exp(exp, pool)
;; Purpose: Return a single equation
;; Type: [Cexp * Pool -> Equation]
;; Pre-condition: exp is a member of pool
(define make-equation-from-exp
  (lambda (exp pool)
    (letrec ((get-tvar-of-exp (lambda (exp) (second (assoc exp pool)))))
      (cond
        ;; The type of procedure is (T1 * ... * Tn -> Te)
        ;; where Te is the type of the last exp in the body of the proc.
        ;; and   Ti is the type of each of the parameters.
        ;; No need to traverse the other body expressions - they will be
        ;; traversed by the overall loop of pool->equations
        ((proc-exp? exp)
         (let ((left (get-tvar-of-exp exp))
               (right (make-proc-te
                       (map var-decl->texp (proc-exp->params exp))
                       (get-tvar-of-exp (last (proc-exp->body exp))))))
           (make-equation left right)))
        ;; An application must respect the type of its operator
        ;; Type(Operator) = [T1 * .. * Tn -> Te]
        ;; Type(Application) = Te
        ((app-exp? exp)
         (let ((left (get-tvar-of-exp (app-exp->rator exp)))
               (right (make-proc-te
                       (map get-tvar-of-exp (app-exp->rands exp))
                       (get-tvar-of-exp exp))))
           (make-equation left right)))
        ;; The type of a number is Number
        ((num-exp? exp)
         (let ((left (get-tvar-of-exp exp))
               (right (typeof-num-exp exp)))
           (make-equation left right)))
        ;; The type of a boolean is Boolean
        ((bool-exp? exp)
         (let ((left (get-tvar-of-exp exp))
               (right (typeof-bool-exp exp)))
           (make-equation left right)))
        ;; The type of a primitive procedure is given by the primitive.
        ((prim-op? exp)
         (let ((left (get-tvar-of-exp exp))
               (right (typeof-prim-op exp)))
           (make-equation left right)))
        ;; let-exp?
        ;; letrec-exp?
        (else (error 'make-equation "Bad expression" exp))))))


;; a number literal has type num-te
;; Type: [Num-exp -> Num-te]
(define typeof-num-exp
  (lambda (num) (make-num-te)))

;; a boolean literal has type bool-te
;; Type: [Bool-exp -> Bool-te]
(define typeof-bool-exp
  (lambda (bool) (make-bool-te)))

;; primitive ops have known proc-te types
;; Type: [Prim-op -> Proc-te]
(define typeof-prim-op
  (lambda (x)
    (let ((num-op-te      (parse-texp '(number * number -> number)))
          (num-comp-op-te (parse-texp '(number * number -> boolean)))
          (bool-op-te     (parse-texp '(T -> boolean)))
          (x (prim-op->op x)))
      (cond ((eq? x '+) num-op-te)
            ((eq? x '-) num-op-te)
            ((eq? x '*) num-op-te)
            ((eq? x '/) num-op-te)
            ((eq? x '<) num-comp-op-te)
            ((eq? x '>) num-comp-op-te)
            ((eq? x '=) num-comp-op-te)
            ((eq? x 'not) bool-op-te)
          ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equations ADT

;; Equation is a Pair(Texp, Texp)
(define make-equation cons)
(define equation->left car)
(define equation->right cdr)

;; Equations pool is an ordered list of Equations
;; Signature: get-first-equation(equations)
;; Example: (get-first-equation '((T_1 T_2) (T_2 T_3)))  ==> '(T_1 T_2)
(define get-first-equation car)

;; Signature: get-rest-equations(equations)
;; Example: (get-rest-equations '((T_1 T_2) (T_2 T_3)))  ==> '(T_2 T_3)
(define get-rest-equations cdr)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signature: infer-type(exp)
;; Purpose: Infer the type of a L5 expression using the equations method
;; Type: [Exp -> Texp]
;; Example: (unparse-texp (infer-type (parseL5 '(lambda (f x) (f (f x))))))
;;          ==> '((T_1 -> T_1) * T_1 -> T_1)
(define infer-type
  (lambda (exp)
    (let* ((pool (L5-exp->pool exp))
           (equations (pool->equations pool))
           (sub (solve-equations equations))
           (texp (second (assoc exp pool))))
      ;; get the type expression of the variable that represents exp,
      ;; in the substitution returned by solve-equations.
      (if (member texp (sub->variables sub))
          (sub->exp-of-var sub texp)
          ;; not enough info to infer a type - return a variable
          texp))))

;; Type: [Concrete-Exp -> Concrete-Tex[]
(define infer
  (lambda (exp)
    (unparse-texp (infer-type (parseL5 exp)))))

;; type equation solving

;; Signature: solve-equations(equation-list)
;; Purpose: Solve the type equations and return the resulting substitution
;;          or error, if not solvable
;; Type: [List(Equation) -> Sub]
;; Example: (solve-equations
;;           (pool->equations
;;             (scheme-exp->pool
;;               (scheme-parse '((lambda (x) (x 11)) (lambda (y) y))))))
;;           ==> '((T_59 T_63 T_57 T_61 T_60 T_62 T_58)
;;           ==> '(sub (T_7 T_9 T_11 T_6
;;                      T_5 T_10 T_8)
;;                     (Number Number Number Number
;;                      (-> (* Number) Number)
;;                      (-> (* Number) Number)
;;                      (-> (* (-> (* Number) Number)) Number)))
(define solve-equations
  (lambda (equations)
    (solve equations (make-empty-sub))))


;; Signature: solve(equations, substitution)
;; Purpose: Solve the equations, starting from a given substitution.
;;          Returns the resulting substitution, or error, if not solvable
;; Type: [List(Equation)*Substitution -> Substitution]
;; Example: (solve (pool->equations
;;                  (scheme-exp->pool
;;                    (scheme-parse '((lambda (x) (x 11)) (lambda (y) y)))))
;;                 (make-empty-sub)) ==>
;;    '(sub (T_59 T_63 T_57 T_61 T_60 T_62 T_58)
;;          (Number Number Number Number
;;           (-> (* Number) Number)
;;           (-> (* Number) Number)
;;           (-> (* (-> (* Number) Number)) Number)))
(define solve
  (lambda (equations sub)
    (if (empty? equations)
        sub
        (let ((eq (make-equation
                   (sub-apply sub (equation->left (car equations)))
                   (sub-apply sub (equation->right (car equations))))))
          (letrec
              ((solve-var-eq  ; If one side of eq is a variable
                (lambda (var-part other-part)
                  (solve (cdr equations)
                         (sub-combine
                          sub
                          (make-sub (list var-part)
                                    (list other-part))))))
               (both-sides-atomic?
                (lambda (eq)
                  (and (atomic-te? (equation->left eq))
                       (atomic-te? (equation->right eq)))))
               (handle-both-sides-atomic
                (lambda (eq)
                  (if (equal-atomic-te? (equation->left eq)
                                        (equation->right eq))
                      (solve (cdr equations) sub)
                      (error
                       'solve
                       "equation contains unequal atomic types: ~e" eq)))))
            (cond
             ((tvar? (equation->left eq))
              (solve-var-eq (equation->left eq) (equation->right eq)))
             ((tvar? (equation->right eq))
              (solve-var-eq (equation->right eq) (equation->left eq)))
             ((both-sides-atomic? eq)
              (handle-both-sides-atomic eq))
             ((and (composite-te? (equation->left eq))
                   (composite-te? (equation->right eq))
                   (unifyable-structure eq))
              (solve (append (cdr equations) (split-equation eq)) sub))
             (else (error
                    'solve
                    "equation contains incompatible type expression: ~s" eq))))))))


;; Signature: unifyable-structure(equation)
;; Purpose: Compars the structure of the type expressions of the equation
;; Type: [Equation -> Boolean]
(define unifyable-structure
  (lambda (eq)
   (let ((left (equation->left eq))
         (right (equation->right eq)))
     (and (proc-te? left) (proc-te? right)
          (= (length (proc-te->param-tes left))
             (length (proc-te->param-tes right)))))))

;; Signature: split-equation(equation)
;; Purpose: For an equation with unifyable type expressions,
;;          create equations for corresponding components.
;; Type: [Equation -> List(Equation)]
;; Example: (split-equation 
;;           (make-equation (parse-texp '(T_1 -> T2))
;;                          (parse-texp '(T_3 -> (T_4 -> T_4)))) =>
;;            ((equation T_2 (T_4 -> T_4))
;;             (equation T_3 T_1))
;; Pre-condition: (and (composite? (equation->left eq))
;;                     (composite? (equation->right eq))
;;                     (unifyable-structure eq))
(define split-equation
  (lambda (eq)
    (letrec ((make-equations-from-components
              ;;create equations from corresponding type expressions
              (lambda (l1 l2) (map make-equation l1 l2))))
      (let ((left (equation->left eq))
            (right (equation->right eq)))
        (make-equations-from-components
         (cons (proc-te->return-te left)
               (proc-te->param-tes left))
         (cons (proc-te->return-te right)
               (proc-te->param-tes right)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
