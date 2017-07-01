#lang racket
(provide (all-defined-out))

;; L5 AST
;; ======
;; Type checking language
;; Syntax with optional type annotations for var declarations and function return types.

;; Type language
;; <texp>         ::= <atomic-te> | <composite-te> | <tvar> 
;; <atomic-te>    ::= <num-te> | <bool-te> | <void-te>
;; <num-te>       ::= number   // num-te()
;; <bool-te>      ::= boolean  // bool-te()
;; <void-te>      ::= void     // void-te()
;; <composite-te> ::= <proc-te> | <tuple-te>
;; <non-tuple-te> ::= <atomic-te> | <proc-te> | <tvar>
;; <proc-te>      ::= [ <tuple-te> -> <non-tuple-te> ] // proc-te(param-tes: list(te), return-te: te)
;; <tuple-te>     ::= <non-empty-tuple-te> | <empty-te>
;; <non-empty-tuple-te> ::= ( <non-tuple-te> *)* <non-tuple-te> // tuple-te(tes: list(te))
;; <empty-te>     ::= Empty
;; <tvar>         ::= a symbol starting with T // tvar(id: Symbol)

;; Examples of type expressions
;; number
;; boolean
;; void
;; [number -> boolean]
;; [number * number -> boolean]
;; [number -> [number -> boolean]]
;; [Empty -> number]
;; [Empty -> void]

;; Type Language AST
(define texp?
  (lambda (x) (or (num-te? x)
                  (bool-te? x)
                  (void-te? x)
                  (proc-te? x)
                  (tvar? x))))

(define atomic-te?
  (lambda (x) (or (num-te? x)
                  (bool-te? x)
                  (void-te? x))))

(define composite-te?
  (lambda (x) (proc-te? x)))

;; Purpose: uniform access to atomic types
;; Signature: atomic-te->name(te)
;; Type: [Atomic-te -> Symbol]
(define atomic-te->name
  (lambda (te)
    (cond ((num-te? te) 'num-te)
          ((bool-te? te) 'bool-te)
          ((void-te? te) 'void-te)
          (else (error "Not an atomic type" te)))))

(define equal-atomic-te?
  (lambda (te1 te2)
    (eq? (atomic-te->name te1)
         (atomic-te->name te2))))

;; Num-te
(define make-num-te
  (lambda () 'num-te))
(define num-te?
  (lambda (x) (eq? x 'num-te)))

;; Bool-te
(define make-bool-te
  (lambda () 'bool-te))
(define bool-te?
  (lambda (x) (eq? x 'bool-te)))

;; void-te
(define make-void-te
  (lambda () 'void-te))
(define void-te?
  (lambda (x) (eq? x 'void-te)))

;; Purpose: Proc-te value constructor
;; Type: [List(texp) * texp -> proc-te]
(define make-proc-te
  (lambda (param-tes return-te)
    (list 'proc-te param-tes return-te)))
(define proc-te?
  (lambda (x) (and (list? x) (= (length x) 3) (eq? (car x) 'proc-te))))
(define proc-te->param-tes
  (lambda (proc-te) (second proc-te)))
(define proc-te->return-te
  (lambda (proc-te) (third proc-te)))
;; Purpose: collect all the components in a composite te
;; Signature: proc-te->components(proc-te)
;; Type: [Proc-te -> List(Texp)]
(define proc-te->components
  (lambda (proc-te)
    (cons (proc-te->return-te proc-te)
          (proc-te->param-tes proc-te))))

;; Purpose: tvar value constructor
;; Type: [Symbol -> tvar]
(define make-tvar
  (lambda (var) (list 'tvar var (box #f))))
(define make-fresh-tvar
  (lambda () (list 'tvar (gensym 'T) (box #f))))
(define tvar?
  (lambda (x) (and (list? x) (= (length x) 3) (eq? (car x) 'tvar))))
(define tvar->var
  (lambda (tvar) (second tvar)))
(define tvar->contents
  (lambda (tvar) (unbox (third tvar))))
(define tvar-set-contents!
  (lambda (tvar val) (set-box! (third tvar) val)))
(define tvar-non-empty?
  (lambda (tvar) (not (eq? (tvar->contents tvar) #f))))

;; Purpose: find the reference of a tvar by following the chain of substitutioons.
;; Signature: tvar-deref(tvar)
;; Type: [TExp -> Texp]
(define tvar-deref
  (lambda (tvar)
    (cond ((not (tvar? tvar)) tvar)
          ((tvar-non-empty? tvar) (tvar-deref (tvar->contents tvar)))
          (else tvar))))

(define tvar-eq?
  (lambda (tvar1 tvar2)
    (eq? (tvar->var tvar1) (tvar->var tvar2))))

;; Examples of type annotated programs
;; (define [x : number] 5)
;; (define [f : [number -> number]] (lambda ([x : number]) : number (* x x))
;; (define [f : [number * number -> number]] (lambda ([x : number] [y : number]) : number (* x x))
;; (define f (lambda ([x : number]) (* x x)))  ;; no type annotation on f and on return value of lambda
;; (let (([a : number] 1)
;;       ([b : boolean] #t))
;;   (if b a (+ a 1)))
;; (define [id : (T1 -> T1)] (lambda ([x : T1]) : T1 x))


;; The only changes in the syntax of L5 are optional type annotations in var-decl and proc-exp
;; <program> ::= (L5 <exp>+) // program(exps:List(exp))
;; <exp> ::= <define-exp> | <cexp>
;; <define-exp> ::= (define <var-decl> <cexp>) // def-exp(var:var-decl, val:cexp)
;; <cexp> ::= <num-exp> // num-exp(val:Number)
;;        | <bool-exp>  // bool-exp(val:Boolean)
;;        | <prim-op>   // prim-op(op:Symbol)
;;        | <var-ref>   // var-ref(var:Symbol)
;;        | (if <exp> <exp> <exp>) // if-exp(test,then,else)
;;        | (lambda (<var-decl>*) [: <texp>]? <cexp>+) // proc-exp(params:List(var-decl), body:List(cexp), return-te: Texp) ##### L5
;;        | (quote <sexp>) // lit-exp(val:Sexp)
;;        | (let (<binding>*) <cexp>+) // let-exp(bindings:List(binding), body:List(cexp)) 
;;        | (letrec (<binding>*) <cexp>+) // letrec-exp(bindings:List(binding), body:List(cexp)) 
;;        | (<cexp> <cexp>*) // app-exp(rator:cexp, rands:List(cexp))
;; <prim-op> ::= + | - | * | / | < | > | = | not |  eq?
;;        | cons | car | cdr | pair? | list? | number? | boolean? | symbol? | display | newline
;; <num-exp> ::= a number token
;; <bool-exp> ::= #t | #f
;; <var-ref> ::= an identifier token
;; <var-decl>   ::= <symbol> | [<symbol> : <texp>] // var-decl(var:Symbol, type:Texp)  ##### L5
;; <sexp> ::= a symbol token | ( <sexp>* )
;; <binding> ::= ( <var-decl> <cexp> ) // Binding(var:var-decl, val:cexp)


;; Type: [List(exp) -> Program]
(define make-program
  (lambda (exps) (list 'program exps)))
(define program?
  (lambda (x) (and (list? x) (= (length x) 2) (eq? (car x) 'program))))
(define program->exps
  (lambda (program-exp) (second program-exp)))

(define exp?
  (lambda (x) (or (def-exp? x) (cexp? x))))

;; Type: [Any -> Boolean]
(define cexp?
  (lambda (x) (or (num-exp? x)
                  (bool-exp? x)
                  (prim-op? x)
                  (var-ref? x)
                  (var-decl? x)
                  (if-exp? x)
                  (proc-exp? x)
                  (lit-exp? x)
                  (let-exp? x)
                  (letrec-exp? x)
                  (app-exp? x))))

;; Generic methods for cexp
;; ------------------------
(define cexp-atomic?
  (lambda (x) (or (num-exp? x)
                  (bool-exp? x)
                  (prim-op? x)
                  (var-ref? x))))

(define cexp-composite?
  (lambda (x) (or (if-exp? x)
                  (proc-exp? x)
                  (let-exp? x)
                  (letrec-exp? x)
                  (app-exp? x))))

;; Purpose: retrieve a list of components
;; from a composite expression.
;; Type: [Cexp-composite -> List(cexp)]
(define cexp-composite->components
  (lambda (x)
    (cond ((if-exp? x) (list (if-exp->test x) (if-exp->then x) (if-exp->else x)))
          ((proc-exp? x) (proc-exp->body x))
          ((let-exp? x) (append (map binding->val (let-exp->bindings x))
                                (let-exp->body x)))
          ((letrec-exp? x) (append (map binding->val (letrec-exp->bindings x))
                                   (letrec-exp->body x)))
          ((app-exp? x) (cons (app-exp->rator x)
                              (app-exp->rands x)))
          (else (error "Bad composite exp" x)))))
  
;; def-exp
;; Type: [var-decl * cexp -> def-exp]
(define make-def-exp
  (lambda (var val) (list 'def-exp var val)))
(define def-exp?
  (lambda (x) (and (list? x) (= (length x) 3) (eq? (car x) 'def-exp))))
(define def-exp->var
  (lambda (def-exp) (second def-exp)))
(define def-exp->val
  (lambda (def-exp) (third def-exp)))

;; num-exp
;; Type: [Number -> num-exp]
(define make-num-exp
  (lambda (val) (list 'num-exp val)))
(define num-exp?
  (lambda (x) (and (list? x)(= (length x) 2) (eq? (car x) 'num-exp))))
(define num-exp->val
  (lambda (num-exp) (second num-exp)))

;; bool-exp
;; Type: [Boolean -> bool-exp]
(define make-bool-exp
  (lambda (val) (list 'bool-exp val)))
(define bool-exp?
  (lambda (x) (and (list? x)(= (length x) 2) (eq? (car x) 'bool-exp))))
(define bool-exp->val
  (lambda (bool-exp) (second bool-exp)))

;; prim-op
;; Type: [Symbol -> prim-op]
(define make-prim-op
  (lambda (op) (list 'prim-op op)))
(define prim-op?
  (lambda (x) (and (list? x) (= (length x) 2) (eq? (car x) 'prim-op))))
(define prim-op->op
  (lambda (prim-op) (second prim-op)))

;; var-ref
;; Type: [Symbol -> var-ref]
(define make-var-ref
  (lambda (var) (list 'var-ref var)))
(define var-ref?
  (lambda (x) (and (list? x) (= (length x) 2) (eq? (car x) 'var-ref))))
(define var-ref->var
  (lambda (var-ref) (second var-ref)))

;; var-decl #####L5
;; Type: [Symbol Texp -> var-decl]
(define make-var-decl
  (lambda (var texp) (list 'var-decl var texp)))
(define var-decl?
  (lambda (x) (and (list? x) (= (length x) 3) (eq? (car x) 'var-decl))))
(define var-decl->var
  (lambda (var-decl) (second var-decl)))
(define var-decl->texp
  (lambda (var-decl) (third var-decl)))

;; app-exp
;; Type: [cexp * List(cexp) -> app-exp]
(define make-app-exp
  (lambda (rator rands) (list 'app-exp rator rands)))
(define app-exp?
  (lambda (x) (and (list? x) (= (length x) 3) (eq? (car x) 'app-exp))))
(define app-exp->rator
  (lambda (app-exp) (second app-exp)))
(define app-exp->rands
  (lambda (app-exp) (third app-exp)))

;; if-exp
;; Purpose: if-exp value constructor
;; Signature: make-if-exp(test,then,else)
;; Type: [Cexp * Cexp * Cexp -> If-exp]
(define make-if-exp
  (lambda (test then else)
    (list 'if-exp test then else)))
(define if-exp?
  (lambda (x)
    (and (list? x) (= (length x) 4) (eq? (car x) 'if-exp))))
(define if-exp->test
  (lambda (if-exp) (second if-exp)))
(define if-exp->then
  (lambda (if-exp) (third if-exp)))
(define if-exp->else
  (lambda (if-exp) (fourth if-exp)))

;; proc-exp ##### L5
;; Purpose: proc-exp value constructor
;; Signature: make-proc-exp(params, body, return-te)
;; Type: [List(var-decl) * List(cexp) * Texp -> proc-exp]
(define make-proc-exp
  (lambda (params body return-te)
    (list 'proc-exp params body return-te)))
(define proc-exp?
  (lambda (x) (and (list? x) (= (length x) 4) (eq? (car x) 'proc-exp))))
(define proc-exp->params
  (lambda (proc-exp) (second proc-exp)))
(define proc-exp->body
  (lambda (proc-exp) (third proc-exp)))
(define proc-exp->return-te
  (lambda (proc-exp) (fourth proc-exp)))

;; lit-exp
;; Purpose: lit-exp value constructor
;; Signature: make-lit-exp(val)
;; Type: [Sexp -> lit-exp]
(define make-lit-exp
  (lambda (val)
    (list 'lit-exp val)))
(define lit-exp?
  (lambda (x)
    (and (list? x) (= (length x) 2) (eq? (car x) 'lit-exp))))
(define lit-exp->val
  (lambda (lit-exp) (second lit-exp)))

;; let-exp
;; Type: [List(binding) * List(cexp) -> letrec-exp]
(define make-let
  (lambda (bindings body)
    (list 'let-exp bindings body)))
(define let-exp?
  (lambda (x)
    (and (list? x) (= (length x) 3) (eq? (car x) 'let-exp))))
(define let-exp->bindings
  (lambda (x) (second x)))
(define let-exp->body
  (lambda (x) (third x)))

;; letrec-exp
;; Type: [List(binding) * List(cexp) -> letrec-exp]
(define make-letrec
  (lambda (bindings body)
    (list 'letrec-exp bindings body)))
(define letrec-exp?
  (lambda (x)
    (and (list? x) (= (length x) 3) (eq? (car x) 'letrec-exp))))
(define letrec-exp->bindings
  (lambda (x) (second x)))
(define letrec-exp->body
  (lambda (x) (third x)))

;; binding
;; Type: [Var-decl * cexp -> Binding]
(define make-binding
  (lambda (var val)
    (list 'binding var val)))
(define binding?
  (lambda (x)
    (and (list? x) (= (length x) 3) (eq? (car x) 'binding))))
(define binding->var
  (lambda (x) (second x)))
(define binding->val
  (lambda (x) (third x)))

;; Type Language Parser
;; ====================

;; Purpose: split a list into 2 values - items in even positions and odd positions
;; Signature: split-even-odd-items(l)
;; Type: [List(T) -> [Values List(T) List(T)]]
;; Example:
;; (split-even-odd-items '(0 1 2 3)) => (values '(0 2) '(1 3))
;; (split-even-odd-items '()) => (values '() '())
;; NOTE: a function in Scheme can return multiple values - using the values special form.
;; When invoking such a function, use (let-values (x1 x2) (f ...) body)
;; which binds the returned values to the local vars x1, x2.
(define split-even-odd-items
  (lambda (l)
    (cond ((null? l) (values '() '()))
          ((empty? (cdr l)) (values l '()))
          (else (let-values ([(even odd) (split-even-odd-items (cdr l))])
                  (values (cons (car l) odd) even))))))

;; Purpose: check that pred holds on all items in L
;; Signature: every(pred, l)
;; Type: [(T -> Boolean] * List(T) -> Boolean]
;; Example:
;; (every even? '(2 4)) => #t
(define every
  (lambda (pred l)
    (cond ((empty? l) #t)
          (else (and (pred (car l))
                     (every pred (cdr l)))))))

;; Purpose: Parse a type expression
;; Signature: parse-texp(texp)
;; Type: [Sexp -> Te]
;; Example:
;; (parse-texp 'number) => 'num-te
;; (parse-texp 'boolean) => 'bool-te
;; (parse-texp 'T1) => '(tvar T1)
;; (parse-texp '(T * T -> boolean)) => '(proc-te ((tvar T) (tvar T)) bool-te)
;; (parse-texp '(number -> (number -> number)) => '(proc-te (num-te) (proc-te (num-te) num-te))
(define parse-texp
  (lambda (texp)
    (cond ((eq? texp 'number) (make-num-te))
          ((eq? texp 'Number) (make-num-te))
          ((eq? texp 'boolean) (make-bool-te))
          ((eq? texp 'Boolean) (make-bool-te))
          ((eq? texp 'void) (make-void-te))
          ((eq? texp 'Void) (make-void-te))
          ((symbol? texp) (make-tvar texp))
          ((list? texp)
           ;; expected structure: (<params> -> <returnte>)
           ;; expected exactly one -> in the list
           ;; We do not accept (a -> b -> c) - must parenthesize
           (let-values (([params arrow-return-te]
                         (splitf-at texp (lambda (x) (not (eq? x '->))))))
             (cond ((empty? arrow-return-te) (error "Procedure type expression without ->" texp))
                   ((empty? (cdr arrow-return-te)) (error "No return type in proc texp" texp))
                   ((empty? params) (error "No param types in proc texp" texp))
                   ((member '-> (cdr arrow-return-te)) (error "Only one -> in toplevel allowed" texp))
                   (else (let ((return-te (parse-texp (second arrow-return-te))))
                           ;; params must be of form [te1 * te2 * ... * ten] or Empty
                           (if (equal? params (list 'Empty))
                               (make-proc-te '() return-te)
                               ;; Should check that Empty does not appear in params.
                               (let-values (([tes stars] (split-even-odd-items params)))
                                 (if (every (lambda (x) (eq? x '*)) stars)
                                     (make-proc-te (map parse-texp tes) return-te)
                                     (error "Bad parameter list - missing *" texp)))))))))
          (else (error "Wrong texp" texp)))))

;; Purpose: Unparse a type expression Texp into its concrete form
;; Signature: unparse-texp(te)
;; Type: [Te -> Sexp]
(define unparse-texp
  (lambda (te)
    (letrec ((unparse-tuple (lambda (param-tes)
                              (cond ((empty? param-tes) (list 'Empty))
                                    (else (cons (unparse-texp (car param-tes))
                                                (foldr (lambda (te acc)
                                                         (append (list '* (unparse-texp te)) acc))
                                                       '() (cdr param-tes))))))))
      (cond ((eq? te 'num-te) 'number)
            ((eq? te 'bool-te) 'boolean)
            ((eq? te 'void-te) 'void)
            ((tvar? te)
             (if (tvar-non-empty? te)
                 (unparse-texp (tvar->contents te))
                 (tvar->var te)))
            ((proc-te? te)
             (let ((tuple (unparse-tuple (proc-te->param-tes te)))
                   (return (unparse-texp (proc-te->return-te te))))
               (append tuple '(->) (list return))))
            (else (error "Unexpected te"))))))
  
             
;; Scheme Parser
;; =============

;; Purpose: Recognize primitive operators as part of the syntax
;; Signature: primitive-op?(x)
;; Type: [Any -> Boolean]
(define primitive-op?
  (lambda (x) (or (eq? x '+)
                  (eq? x '-)
                  (eq? x '*)
                  (eq? x '/)
                  (eq? x '<)
                  (eq? x '>)
                  (eq? x '=)
                  (eq? x 'not)
                  (eq? x 'cons)
                  (eq? x 'car)
                  (eq? x 'cdr)
                  (eq? x 'pair?)
                  (eq? x 'list?)
                  (eq? x 'number?)
                  (eq? x 'symbol?)
                  (eq? x 'boolean?)
                  (eq? x 'eq?)
                  (eq? x 'display)
                  (eq? x 'newline))))

;; Purpose: Parse concrete syntax of L5 in sexp into L5-AST
;; Signature: parseL5(x)
;; Type: [SExp -> Program | Exp]
;; Examples:
;; (parseL5 '(L5 (define [f : [number -> number]] (lambda ([x : number]) : number (+ x x))) (f 2))) =>
;;   '(program ((def-exp (var-decl f [-> (number) number]) (proc-exp ((var-decl x number)) ((app-exp (prim-op +) ((var-ref x) (var-ref x)))) number))
;;              (app-exp (var-ref f) ((num-exp 2)))))
;; Key change relative to parseL4:
;; - Parse optional type expression in all var-decl occurrences and after formals within proc-exp
;; - When a type annotation is not find, generate a fresh new type variable.
(define parseL5
  (lambda (x)
    (letrec
        ;; Parse optional variable decl type annotation
        ((parse-var-decl (lambda (var-decl)
                           (cond ((symbol? var-decl)
                                  (make-var-decl var-decl (make-fresh-tvar)))
                                 ((list? var-decl)
                                  (and (= (length var-decl) 3)
                                       (symbol? (car var-decl))
                                       (eq? (second var-decl) ':)
                                       (texp? (third var-decl)))
                                  (make-var-decl (car var-decl) (parse-texp (third var-decl)))))))
         (parse (lambda (x)
                  (cond ((empty? x) (make-lit-exp x))
                        ((list? x)
                         (cond ((eq? (car x) 'L5)
                                (make-program (map parse (cdr x))))
                               ((eq? (car x) 'define)
                                (make-def-exp (parse-var-decl (second x))
                                              (parse (third x))))
                               ((eq? (car x) 'if)
                                (make-if-exp (parse (second x))
                                             (parse (third x))
                                             (parse (fourth x))))
                               ((eq? (car x) 'lambda)
                                ;; Check whether return-type is present
                                ;; (lambda (params) : texp body) or
                                ;; (lambda (params) body)
                                (let ((after-params (cddr x))
                                      (params (map parse-var-decl (second x))))
                                  (if (eq? (car after-params) ':)
                                      (let ((return-type (parse-texp (second after-params)))
                                            (body (cddr after-params)))
                                        (make-proc-exp params (map parse body) return-type))
                                      (let ((return-type (make-fresh-tvar)))
                                        (make-proc-exp params (map parse after-params) return-type)))))
                               ((eq? (car x) 'let)
                                (make-let (map make-binding
                                               (map (lambda (b) (parse-var-decl (car b))) (second x))
                                               (map (lambda (b) (parseL5 (second b))) (second x)))
                                          (map parse (cddr x))))
                               ((eq? (car x) 'letrec)
                                (make-letrec (map make-binding
                                                  (map (lambda (b) (parse-var-decl (car b))) (second x))
                                                  (map (lambda (b) (parse (second b))) (second x)))
                                             (map parse (cddr x))))
                               ((eq? (car x) 'quote)
                                (make-lit-exp (second x)))
                               (else (make-app-exp (parse (car x))
                                                   (map parse (cdr x))))))
                        ((number? x) (make-num-exp x))
                        ((boolean? x) (make-bool-exp x))
                        ((primitive-op? x) (make-prim-op x))
                        ((symbol? x) (make-var-ref x))
                        (else (error "Unexpected type " x))))))
      (parse x))))


(define unparse
  (lambda (exp)
    (cond ((num-exp? exp) (num-exp->val exp))
          ((bool-exp? exp) (bool-exp->val exp))
          ((var-ref? exp) (var-ref->var exp))
          ((prim-op? exp) (prim-op->op exp))
          ((var-decl? exp) (list (var-decl->var exp) ': (unparse-texp (var-decl->texp exp))))
          ((if-exp? exp) (list 'if (unparse (if-exp->test exp))
                               (unparse (if-exp->then exp))
                               (unparse (if-exp->else exp))))
          ((proc-exp? exp)
           (list 'lambda (map unparse (proc-exp->params exp))
                 ': (unparse-texp (proc-exp->return-te exp))
                 (map unparse (proc-exp->body exp))))
          ((app-exp? exp)
           (cons (unparse (app-exp->rator exp))
                 (map unparse (app-exp->rands exp))))
          ((let-exp? exp)
           (list 'let (map (lambda (b) (list (unparse (binding->var b))
                                             (unparse (binding->val b))))
                           (let-exp->bindings exp))
                 (map unparse (let-exp->body exp))))
          ((letrec-exp? exp)
           (list 'letrec (map (lambda (b) (list (unparse (binding->var b))
                                                (unparse (binding->val b))))
                              (letrec-exp->bindings exp))
                 (map unparse (letrec-exp->body exp))))
          ((def-exp? exp)
           (list 'define (unparse (def-exp->var exp))
                 (unparse (def-exp->val exp))))
          (else (error "Unknown exp type" exp)))))


;; ============================================================
;; equivalent-tes: 2 TEs are equivalent up to variable renaming.
;; For example:
;; (equivalent-tes? (parse-texp '(T1 -> T2)) (parse-texp '(T3 -> T4)))


;; Signature: match-tvars-in-te$(te1 te2 succ fail)
;; Type: [Texp * Texp * [List(Pair(Tvar, Tvar)) -> T1] * [Empty -> T2]] |
;;       [List(Texp) * List(Texp) * ...]
;; Purpose:   Receives two type expressions or list(texps) plus continuation procedures
;;            and, in case they are equivalent, pass a mapping between
;;            type variable they include to succ. Otherwise, invoke fail.
;; Examples:
;; (match-tvars-in-te$ (parse-texp '(Number * T1 -> T1))
;;                     (parse-texp '(Number * T7 -> T5))
;;                     (lambda (x) x)
;;                     (lambda () #f)) ==> '((T1.T7) (T1.T5))
;; (match-tvars-in-te$ (parse-texp '(Boolean * T1 -> T1))
;;                     (parse-texp '(Number * T7 -> T5))
;;                     (lambda (x) x) (lambda () #f)) ==> #f
(define match-tvars-in-te$
  (lambda (te1 te2 succ fail)
    (cond ((or (empty? te1) (empty? te2))
           (if (and (empty? te2) (empty? te2))
               (succ (list))
               (fail)))
          ((or (tvar? te1) (tvar? te2))
           (let ((tval1 (tvar-deref te1))
                 (tval2 (tvar-deref te2)))
             (cond ((and (tvar? tval1) (tvar? tval2))
                    (if (tvar-eq? tval1 tval2)
                        (succ (list))
                        (succ (list (cons tval1 tval2)))))
                   ((or (tvar? tval1) (tvar? tval2))
                    (fail))
                   (else                     
                    (match-tvars-in-te$ tval1 tval2 succ fail)))))
          ((or (atomic-te? te1) (atomic-te? te2))
           (if (and (atomic-te? te1) (atomic-te? te2)
                    (equal-atomic-te? te1 te2))
               (succ (list))
               (fail)))
          ((or (proc-te? te1) (proc-te? te2))
           (if (and (proc-te? te1) (proc-te? te2))
               (let ((components1 (proc-te->components te1))
                     (components2 (proc-te->components te2)))
                 (match-tvars-in-te$ components1 components2 succ fail))
               (fail)))
          (else ;; 2 parallel lists of texps - match them one by one and accumulate
           (match-tvars-in-te$
            (car te1) (car te2)
            (lambda (sub-car)
              (match-tvars-in-te$ (cdr te1) (cdr te2)
                                  (lambda (sub-cdr)
                                    (succ (append sub-car sub-cdr)))
                                  fail))
            fail)))))
           

;; Signature: equivalent-tes?(te1, te2)
;; Type:      [TE * TE -> Boolean]
;; Purpose:   Check whether 2 type expressions are equivalent up to
;;            type variable renaming.
;; Examples:  (equivalent-tes? (parse-texp '(T1 * (Number -> T2) -> T3))
;;                             (parse-texp '(T4 * (Number -> T5) -> T6))) => #t
(define equivalent-tes?
  (lambda (te1 te2)
    (let ((tvars-pairs (match-tvars-in-te$
                        te1 te2 (lambda (x) x) (lambda () #f))))
      (if tvars-pairs
          (let* ((tvars-unique-pairs (set->list (list->set tvars-pairs)))
                 (tvars-pairs-car    (map car tvars-unique-pairs))
                 (tvars-pairs-cdr    (map cdr tvars-unique-pairs)))
            (and (eq? (set-count (list->set tvars-pairs-car))
                      (length tvars-pairs-car))
                 (eq? (set-count (list->set tvars-pairs-cdr))
                      (length tvars-pairs-cdr))))
          #f))))
