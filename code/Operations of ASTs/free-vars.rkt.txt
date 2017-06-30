#lang racket
(require "parser.rkt")

(provide (all-defined-out))

;; <cexp> ::= <number>                      / num-exp(val:number)
;;         |  <boolean>                     / bool-exp(val:boolean)
;;         |  <string>                      / str-exp(val:string)
;;         |  <variable>                    / var-exp(var:symbol)
;;         |  ( lambda ( <var>* ) <cexp>+ ) / proc-exp(params:List(var-exp), body:List(cexp))
;;         |  ( if <cexp> <cexp> <cexp> )   / if-exp(test: cexp, then: cexp, else: cexp)
;;         |  ( let ( binding* ) <cexp>+ )  / let-exp(bindings:List(binding), body:List(cexp))
;;         |  ( <cexp> <cexp>* )            / app-exp(rator:cexp, rands:List(cexp))
;;         |  ( quote <sexp> )              / literal-exp(val:sexp)

;; Purpose: determine whether var occurs free within cexp.
;; Signature: occurs-free?(var, cexp)
;; Type: [Symbol * Cexp -> Boolean]
;; Example:
;; (occurs-free? 'x (parse '(lambda (x) x))) => #f
;; (occurs-free? 'y (parse '(lambda (x) y))) => #t
;; (occurs-free? 'y (parse '(lambda (x) x))) => #f
(define occurs-free?
  (lambda (var cexp)
    (cond ((num-exp? cexp) #f)
          ((bool-exp? cexp) #f)
          ((str-exp? cexp) #f)
          ((literal-exp? cexp) #f)
          ((var-exp? cexp) (eq? (var-exp->var cexp) var))
          ((proc-exp? cexp) (and (not (member var (map var-exp->var (proc-exp->params cexp))))
                                 (any (lambda (e) (occurs-free? var e)) (proc-exp->body cexp))))
          ((let-exp? cexp)
           (let ((vars (map (lambda (b) (var-exp->var (binding->var b)))
                            (let-exp->bindings cexp)))
                 (vals (map binding->val (let-exp->bindings cexp)))
                 (body (let-exp->body cexp)))
             (or (any (lambda (e) (occurs-free? var e)) vals)
                 (and (not (member var vars))
                      (any (lambda (e) (occurs-free? var e)) body)))))
          ((if-exp? cexp) (or (occurs-free? var (if-exp->test cexp))
                              (occurs-free? var (if-exp->then cexp))
                              (occurs-free? var (if-exp->else cexp))))
          ((app-exp? cexp) (or (occurs-free? var (app-exp->rator cexp))
                               (any (lambda (e) (occurs-free? var e)) (app-exp->rands cexp))))
          (else (error "unexpected type " cexp)))))

 
;; Purpose: test that pred holds for at least one item in l
;; Signature: any(pred, l)
;; Type: [[T->Boolean]*List(T)->Boolean]
;; Example: (any even? '(1 2 3)) => #t
;;          (any even? '()) => #f
(define any
  (lambda (pred l)
    (cond ((empty? l) #f)
          ((pred (car l)) #t)
          (else (any pred (cdr l))))))


;; Purpose: union of two sets
;; Signature: union(s1,s2)
;; Type: [Set(T)*Set(T)=>Set(T)]
;; Examples:
;; (union '(1 2) '(2 3)) => '(1 2 3)
;; (union '(1) '(1)) => '(1)
(define union
  (lambda (s1 s2)
    (cond ((empty? s1) s2)
          ((member (car s1) s2) (union (cdr s1) s2))
          (else (cons (car s1) (union (cdr s1) s2))))))

;; Purpose: collect set of variables that occur as references in an expression
;; Signature: referenced-vars(cexp)
;; Type: [Cexp->List(var-exp)]
;; Example: (referenced-vars (parse '(lambda (x) (+ y z)))) => '((var-ref +) (var-ref y) (var-ref z))
(define referenced-vars
  (lambda (cexp)
    (cond ((num-exp? cexp) '())
          ((bool-exp? cexp) '())
          ((str-exp? cexp) '())
          ((literal-exp? cexp) '())
          ((var-exp? cexp) (list cexp))
          ((proc-exp? cexp) (foldr union '() (map referenced-vars (proc-exp->body cexp))))
          ((let-exp? cexp)
           (let ((vals (map binding->val (let-exp->bindings cexp))))
             (foldr union '() (map referenced-vars
                                   (append vals (let-exp->body cexp))))))
          ((if-exp? cexp) (union (referenced-vars  (if-exp->test cexp))
                                 (union (referenced-vars  (if-exp->then cexp))
                                        (referenced-vars  (if-exp->else cexp)))))
          ((app-exp? cexp) (union (referenced-vars (app-exp->rator cexp))
                                  (foldr union '() (map referenced-vars (app-exp->rands cexp)))))
          (else (error "unexpected type " cexp)))))

;; declared-vars

;; free-vars

;; bound-vars

