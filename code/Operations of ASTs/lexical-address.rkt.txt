#lang racket
(require "parser.rkt")

(provide (all-defined-out))

;; =============================================================================
;; lexical-address

;; Extend exp AST to distinguish variable declarations and variable references.
;; Variable references are replaced by lexical addresses which indicate unambiguously
;; to which declaration each variable reference is bound.

;; We skip let-exp because they can be rewritten to lambda-app.

;; <cexpLA> ::= <number>                           / num-exp(val:number)
;;         |  <boolean>                            / bool-exp(val:boolean)
;;         |  <string>                             / str-exp(val:string)
;;         |  <var-decl>                           / var-decl(var:symbol)
;;         |  <var-ref>                            / var-ref(var:symbol)
;;         |  ( lambda ( <var-decl>* ) <cexpLA>+ ) / proc-expLA(params:List(var-decl), body:List(cexp))
;;         |  ( if <cexpLA> <cexpLA> <cexpLA> )    / if-expLA(test: cexpLA, then: cexpLA, else: cexpLA)
;;         |  ( <cexpLA> <cexpLA>* )               / app-expLA(rator:cexpLA, rands:List(cexpLA))
;;         |  ( quote <sexp> )                     / literal-exp(val:sexp)

;; var-decl: [var:symbol]
(define make-var-decl (lambda (symbol) (list 'var-decl symbol)))
(define var-decl? (lambda (x) (and (list? x) (eq? (car x) 'var-decl))))
(define var-decl->var (lambda (exp) (second exp)))

;; var-ref: [var:symbol]
(define make-var-ref (lambda (symbol) (list 'var-ref symbol)))
(define var-ref? (lambda (x) (and (list? x) (eq? (car x) 'var-ref))))
(define var-ref->var (lambda (exp) (second exp)))

;; cexpLA is a disjoint union type
(define cexpLA? (lambda (x)
                  (or (num-exp? x)
                      (bool-exp? x)
                      (str-exp? x)
                      (literal-exp? x)
                      (var-decl? x)
                      (var-ref? x)
                      (address? x)
                      (proc-expLA? x)
                      (if-expLA? x)
                      (app-expLA? x))))

;; proc-expLA: [params:List(var-decl), body:List(cexpLA))]
(define make-proc-expLA (lambda (params body) (list 'proc-expLA params body)))
(define proc-expLA? (lambda (x) (and (list? x) (eq? (car x) 'proc-expLA))))
(define proc-expLA->params (lambda (x) (second x)))
(define proc-expLA->body (lambda (x) (third x)))

;; if-expLA: [test:cexpLA, then:cexpLA, else:cexpLA]
(define make-if-expLA (lambda (test then else) (list 'if-expLA test then else)))
(define if-expLA? (lambda (x) (and (list? x) (eq? (car x) 'if-expLA))))
(define if-expLA->test (lambda (x) (second x)))
(define if-expLA->then (lambda (x) (third x)))
(define if-expLA->else (lambda (x) (fourth x)))

;; app-expLA: [rator:cexpLA, rands:List(cexpLA)]
(define make-app-expLA (lambda (rator rands) (list 'app-expLA rator rands)))
(define app-expLA? (lambda (x) (and (list? x) (eq? (car x) 'app-expLA))))
(define app-expLA->rator (lambda (x) (second x)))
(define app-expLA->rands (lambda (x) (third x)))


;; AST extension for lexical-address annotations
;; <address> ::= <free-var> | <lexical-address>
;; <free-var> ::= [<identifier> free]                       / free-var(var)
;; <lexical-address> ::= [<identifier> : <number> <number>] / lexical-address(var:Symbol, depth:Number, pos:Number]

(define address? (lambda (x) (or (free-var? x) (lexical-address? x))))

;; Purpose: value constructor for free-var
;; Signature: make-free-var(var)
;; Type: [Symbol -> Free-var]
(define make-free-var
  (lambda (var)
    (list 'free-var var)))

;; Purpose: type predicate for free-var
;; Signature: free-var?(var)
;; Type: [Any -> Boolean]
(define free-var?
  (lambda (v)
    (and (list? v)
         (= (length v) 2)
         (eq? (car v) 'free-var)
         (symbol? (second v)))))

;; Purpose: accessor for free-var
;; Signature: free-var->var(fv)
;; Type: [Free-var -> Symbol]
(define free-var->var
  (lambda (fv) (second fv)))

;; Purpose: value constructor for lexical address
;; Signature: make-lexical-address(v,d,p)
;; Type: [Symbol * Number * Number -> Lexical-address]
(define make-lexical-address
  (lambda (var depth pos)
    (list 'lex-addr var depth pos)))

;; Purpose: value constructor for lexical address
;; Signature: make-deeper-lexical-address(address)
;; Type: [Lexical-address -> Lexical-address]
;; Example:
;; (make-deeper-lexical-address (make-lexical-address 'a 0 0)) => '(lex-addr a 1 0)
(define make-deeper-lexical-address
  (lambda (address)
    (make-lexical-address (lexical-address->var address)
                          (+ 1 (lexical-address->depth address))
                          (lexical-address->pos address))))

;; Purpose: type predicate for lexical-address
;; Signature: lexical-address?(v)
;; Type: [Any -> Boolean]
(define lexical-address?
  (lambda (v)
    (and (list? v)
         (= (length v) 4)
         (eq? (car v) 'lex-addr)
         (symbol? (second v))
         (number? (third v))
         (number? (fourth v)))))

;; Purpose: accessor for lexical-address
;; Signature: lexical-address->var(address)
;; Type: [Lexical-address -> Symbol]
(define lexical-address->var
  (lambda (address)
    (second address)))

;; Purpose: accessor for lexical-address
;; Signature: lexical-address->depth(address)
;; Type: [Lexical-address -> Number]
(define lexical-address->depth
  (lambda (address)
    (third address)))

;; Purpose: accessor for lexical-address
;; Signature: lexical-address->pos(address)
;; Type: [Lexical-address -> Number]
(define lexical-address->pos
  (lambda (address)
    (fourth address)))



;; Purpose: parse a sexp into a ExpLA AST value.
;; Type: [Sexp -> ExpLA]
;; Signature: parseLA(sexp)
;; Examples:
;; (parseLA '1) -> '(num-exp 1)
;; (parseLA '(if #t (+ 1 2) 'ok)) -> '(if-expLA (bool-exp #t) (app-expLA (var-ref +) ((num-exp 1) (num-exp 2))) (literal-exp ok))
(define parseLA
  (lambda (sexp)
    (cond ((number? sexp) (make-num-exp sexp))
          ((boolean? sexp) (make-bool-exp sexp))
          ((string? sexp) (make-str-exp sexp))
          ((symbol? sexp) (make-var-ref sexp))
          ((empty? sexp) (error "unexpected empty"))
          (else (let ((first (car sexp)))
                  (cond ((eq? first 'quote)
                         (make-literal-exp (second sexp)))
                        ((eq? first 'lambda)
                         (make-proc-expLA (map make-var-decl (second sexp))
                                          (map parseLA (cddr sexp))))
                        ((eq? first 'if)
                         (make-if-expLA (parseLA (second sexp))
                                        (parseLA (third sexp))
                                        (parseLA (fourth sexp))))
                        (else ;; app (rator . rands)
                         (make-app-expLA (parseLA (car sexp))
                                         (map parseLA (cdr sexp))))
                         ))))
            ))


;; Purpose: Map a Lexical Address AST to a concrete syntax sexp.
;; Signature: unparseLA(exp)
;; Type: [ExpLA -> Sexp]
;; Example: (unparseLA (parseLA '(lambda (x) x))) => '(lambda (x) x)
(define unparseLA
  (lambda (exp)
    (cond ((num-exp? exp)  (num-exp->val exp))
          ((bool-exp? exp) (bool-exp->val exp))
          ((str-exp? exp)  (str-exp->val exp))
          ((var-ref? exp)  (var-ref->var exp))
          ((var-decl? exp) (var-decl->var exp))
          ((free-var? exp) (list (free-var->var exp) 'free))
          ((lexical-address? exp) (list (lexical-address->var exp) ':
                                        (lexical-address->depth exp)
                                        (lexical-address->pos exp)))
          ((literal-exp? exp) (list 'quote (literal-exp->val exp)))
          ((proc-expLA? exp) (cons 'lambda
                                   (cons (map unparseLA (proc-expLA->params exp))
                                         (map unparseLA (proc-expLA->body exp)))))
          ((if-expLA? exp) (list 'if
                                 (unparseLA (if-expLA->test exp))
                                 (unparseLA (if-expLA->then exp))
                                 (unparseLA (if-expLA->else exp))))
          ((app-expLA? exp) (cons (unparseLA (app-expLA->rator exp))
                                  (map unparseLA (app-expLA->rands exp))))
          (else (error "Unknown exp type: " exp)))))


;; Annotate an exp AST so that all variable references are marked with their lexical address.
;; The lexical address links a variable reference to its corresponding variable declaration.
;; It can be of two forms:
;; - If the variable is free - it is noted [var free]
;; - else [var : depth var-index]
;;   where depth is the 0-based distance ot the enclosing lambda declaration
;;         var-index is the 0-based index of the variable in the lambda declaration
;; Example:
;; (unparseLA (lexical-address (parse-LA
;;   '(lambda (a b c)
;;      (if (eq? b c)
;;          ((lambda (c)
;;             (cons a c))
;;           a)
;;          b)))
;; =>
;; (lambda (a b c)
;;  (if ([eq? free] [b : 0 1] [c : 0 2])
;;    ((lambda (c) ([cons free] [a : 1 0] [c : 0 0]))
;;     [a : 0 0])
;;    [b : 0 1]))


;; Purpose: get the closest enclosing lexical address given a variable name.
;; Signature: get-lexical-address(var, lexical-addresses)
;; Type: [Var-ref * List(Lexical-address) -> Lexical-address]
;; Pre-conditions: Lexical-addresses are sorted by depth
;; Examples:
;; (get-lexical-address '(var-ref b) '([lex-addr a 0 0] [lex-addr b 0 1]))
;; => '[lex-addr b 0 1]
;; (get-lexical-address '(var-ref c) '([lex-addr a 0 0] [lex-addr b 0 1]))
;; => '[free-var c]
;; (get-lexical-address '(var-ref a) '([lex-addr a 0 0] [lex-addr b 0 1] [lex-add a 1 1]))
;; => '[lex-addr a 0 0]
(define get-lexical-address
  (lambda (var lexical-addresses)
    (letrec ((loop (lambda (lst)
                     (cond ((empty? lst) (make-free-var (var-ref->var var)))
                           ((eq? (var-ref->var var) (lexical-address->var (car lst))) (car lst))
                           (else (get-lexical-address var (cdr lst)))))))
      (loop lexical-addresses))))


;; Purpose: get the pos of a variable in a declaration list (parameters from a lambda-exp)
;; Signature: index-of-var(var, parameters)
;; Type: [var-decl * List(var-decl) -> Number | 'free]
;; Examples:
;; (index-of-var '(var-decl b) '([var-decl a] [var-decl b])) => 1
;; (index-of-var '(var-decl c) '([var-decl a] [var-decl b])) => 'free
(define index-of-var
  (lambda (v declarations)
    (letrec ((loop (lambda (lst index)
                     (cond ((empty? lst) 'free)
                           ((equal? (car lst) v) index)
                           (else (loop (cdr lst) (+ index 1)))))))
      (loop declarations 0))))

;; Purpose: create a new view of the accessible variables when a declaration
;;          contour is crossed - that is, when we enter a (lambda (declarations) ...)
;;          variables in declarations are now visible at depth 0
;;          variables previously visible are now a depth + 1
;;          the new variables appear first in the new addresses
;; Signature: cross-contour(declarations, addresses)
;; Type: [List(Var-decl) * List(Lexical-address) -> List(Lexical-address)]
;; Example:
;; (cross-contour '([var-decl a] [var-decl b])
;;                '([lex-addr a 0 0] [lex-addr c 0 1])) =>
;; '([lex-addr a 0 0] [lex-addr b 0 1] [lex-addr a 1 0] [lex-addr c 1 1])
;; This corresponds to the visible variables from the body of the inner lambda in:
;; '(lambda (a c) (lambda (a b) <here>))
(define cross-contour
  (lambda (declarations addresses)
    (let ((bound (make-bound-addresses declarations))
          (deeper (map make-deeper-lexical-address addresses)))
      (append bound deeper))))

;; Signature: make-bound-addresses(declarations)
;; Type: [List(Var-decl) -> List(Lexical-address)]
;; Example:
;; (make-bound-addresses '([var-decl a] [var-decl b])) => '([lex-addr a 0 0] [lex-addr c 0 1])
(define make-bound-addresses
  (lambda (declarations)
    (map (lambda (decl)
           (make-lexical-address (var-decl->var decl)
                                 0
                                 (index-of-var decl declarations)))
         declarations)))


;; Purpose: map all variable reference expressions to their lexical address inside exp.
;; Signature: lexical-address(exp)
;; Type: [ExpLA -> ExpLA]
;; Example:
;; (unparseLA (lexical-address
;;   (parseLA '(lambda (a b c)
;;               (if (eq? b c)
;                         ((lambda (c)
;                            (cons a c))
;                          a)
;                         b)))
;
;(lambda (a b c)
;  (if ((eq? free) (b : 0 1) (c : 0 2))
;    ((lambda (c) ((cons free) (a : 1 0) (c : 0 0)))
;     (a : 0 0))
;    (b : 0 1)))
(define lexical-address
  (lambda (exp)
    (letrec ((visit (lambda (exp addresses)
                      (cond ((num-exp? exp)  exp)
                            ((bool-exp? exp) exp)
                            ((str-exp? exp)  exp)
                            ((var-ref? exp)  (get-lexical-address exp addresses))
                            ((var-decl? exp) exp)
                            ((free-var? exp) (error "unexpected free-var" exp addresses))
                            ((lexical-address? exp) (error "unexpected address" exp addresses))
                            ((proc-expLA? exp)
                             (let ((declarations (proc-expLA->params exp))
                                   (body (proc-expLA->body exp)))
                               (let ((new-addresses (cross-contour declarations addresses)))
                                 (make-proc-expLA declarations
                                                  (map (lambda (e) (visit e new-addresses))
                                                       body)))))
                            ((if-expLA? exp)
                             (make-if-expLA (visit (if-expLA->test exp) addresses)
                                            (visit (if-expLA->then exp) addresses)
                                            (visit (if-expLA->else exp) addresses)))
                            ((app-expLA? exp)
                             (make-app-expLA (visit (app-expLA->rator exp) addresses)
                                             (map (lambda (e) (visit e addresses))
                                                  (app-expLA->rands exp))))
                            (else (error "Unknown exp type: " exp))))))
      (visit exp '()))))
