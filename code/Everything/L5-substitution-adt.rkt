#lang racket

(require "L5-ast.rkt")

(provide (all-defined-out))

;;;;;;;;;;; Implementation of the Substitution ADT ;;;;;;;;;;;;;;;;;;
;; A substitution is represented as a 2 element list of equal length
;; lists of variables and type expression.
;; The empty substitution is the list (() ()).

;; Constructors:
;; Signature: make-sub(variables, tes)
;; Purpose: Create a substitution in which the i-th element of 'variables'
;;          is mapped to the i-th element of 'tes'.
;; Type: [LIST(Tvar)*LIST(Texp) -> Substitution]
;; Example: (make-sub
;;             (map parse-texp '(x y z))
;;             (map parse-texp '(Number Boolean (Number -> Number))))
;;          => ((x y z) (num-te bool-te (proc-te (Number) Number)))
;;          (make-sub (map parse-texp '(x y z))
;;                    (map parse-texp '(Number Boolean (z -> Number))))
;;          => error make-sub: circular substitution:
;; Pre-condition: (length variables) = (length tes)
;;                variables has no repetitions (set)
(define make-sub
  (lambda (variables tes)
    ;; if any variable substituting expression is circular
    (for-each check-no-occurrence!  variables tes)
    (list 'sub variables tes)))

(define make-empty-sub
  (lambda ()
    (make-sub '() '())))

;; Purpose: when attempting to bind tvar to te in a sub - check whether tvar occurs in te.
;; Throws error if a circular reference is found.
;; Signature: check-no-occurrence!(tvar, te)
;; Type: [Tvar * Texp -> Symbol]
;; Pre-conditions: Tvar is not bound
(define check-no-occurrence!
  (lambda (tvar te)
    (letrec ((loop (lambda (te1)
                     (cond ((atomic-te? te1) #t)
                           ((proc-te? te1)
                            (for-each loop (proc-te->param-tes te1))
                            (loop (proc-te->return-te te1))
                            'proc-te-ok)
                           ((tvar? te1)
                            (if (eq? (tvar->var te1) (tvar->var tvar))
                                (error "Occurrence check error - circular unification ~s ~s in ~s" tvar te exp)
                                'tvar-ok))
                           (else (error "Bad type expression ~s ~s" te exp))))))
      (loop te))))

;; Signature: sub?(sub)
;; Type: [T -> Boolean]
(define sub?
  (lambda (x)
    (and (list? x) (= (length x) 3) (eq? (car x) 'sub))))

;; Signature: empty-sub?(sub)
;; Type: [T -> Boolean]
(define empty-sub?
  (lambda (sub)
    (and (sub? sub)
         (empty? (sub->variables sub))
         (empty? (sub->tes sub)))))

;; Signature: non-empty-sub?(sub)
;; Type: [T -> Boolean]
(define non-empty-sub?
  (lambda (sub) (and (sub? sub) (not (empty-sub? sub)))))

;; Signature: sub->variables(sub)
;; Type: [Sub -> List(TVar)]
(define sub->variables
  (lambda (sub) (second sub)))

;; Signature: sub->tes(sub)
;; Type: [Sub -> List(TExp)]
(define sub->tes
  (lambda (sub) (third sub)))

;; Signature: sub->exp-of-var(sub,var)
;; Type: Client view: [Sub * TVar -> TExp]
;; Pre-condition: sub is a non empty substitution that includes var.
(define sub->exp-of-var
  (lambda (sub var)
    (letrec
        ((lookup
          (lambda (vars tes)
            (cond ((or (empty-sub? sub) (not (member var vars)))
                   (error 'sub->exp-of-var
                     "var is not a variable of sub ~s ~s" sub var))
                  ((tvar-eq? var (car vars)) (car tes))
                  (else (lookup (cdr vars) (cdr tes)))))))
      (lookup (sub->variables sub) (sub->tes sub)))))


;; Signature: extend-sub(sub,var,te)
;; Type: [Sub * TVar * TExp -> Sub]
;; Calls to make-sub do the occur-check
(define extend-sub
  (lambda (sub var te)
    (if (empty-sub? sub)
        (make-sub (list var) (list te))
        (let ((vars (sub->variables sub))
              (tes (sub->tes sub))
              (new-sub (make-sub (list var) (list te))))
          (let ((updated-tes (map (lambda (sub-te)
                                    (sub-apply new-sub sub-te))
                                  tes)))
            (if (member var vars)
                (make-sub vars updated-tes)
                (make-sub
                 (cons var vars)
                 (cons te updated-tes))))))))



;; ============================================================
;; Signature: sub-apply(sub,te)
;; Type: [Sub * TExp -> TExp]
;; Example:
;; (unparse-texp (sub-apply (make-sub (map parse-texp '(t1 t2)) (map parse-texp '(number boolean)))
;;                          (parse-texp '(t1 * t2 -> t1))) =>
;; '(number * boolean -> number)
(define sub-apply
  (lambda (sub te)
    (cond ((empty-sub? sub) te)
          ((atomic-te? te) te)
          (else
           (let ((vars (sub->variables sub))
                 (tes (sub->tes sub))
                 (sub-apply-this (lambda (texp) (sub-apply sub texp))))
             (cond ((tvar? te)
                    (if (member te vars)
                        (sub->exp-of-var sub te)
                        te))
                   ((proc-te? te)
                    (make-proc-te
                     (map sub-apply-this (proc-te->param-tes te))
                     (sub-apply-this (proc-te->return-te te))))
                   (else te)))))))


;; ============================================================
;; Signature: sub-combine(sub1,sub2)
;; Purpose: Returns the composition of substitutions s.t.:
;;  (sub-apply result te) === (sub-apply sub2 (sub-apply sub1 te))
;;
;; Type: [Sub * Sub -> Sub]
;; Example:
;; (sub-combine
;;   (make-sub (map parse-texp '(T1 T2 T3))
;;             (map parse-texp '(S1 (S2 * Number) Boolean)))
;;   (make-sub (map parse-texp '(S1 S2))
;;             (map te-parse '((T21 * (Number * T11 -> T11)) T3)))) =>
;;  (make-sub (map parse-texp '(S2 S1 T1 T2 T3))
;;            (map parse-texp '(T3
;;                             (T21 * (Number * T11 -> T11) T11))
;;                             (T21 * (Number * T11 -> T11))
;;                             (T3 * Number)
;;                             Boolean))
(define sub-combine
  (lambda (sub1 sub2)
    (cond ((empty-sub? sub1) sub2)
          ((empty-sub? sub2) sub1)
          (else (letrec ((combine
                          (lambda (sub vars tes)
                            (if (empty? vars)
                                sub
                                (combine
                                 (extend-sub sub (car vars) (car tes))
                                 (cdr vars) (cdr tes))))))
                  (combine sub1 (sub->variables sub2) (sub->tes sub2)))))))
