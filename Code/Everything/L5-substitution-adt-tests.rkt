#lang racket

(require "utils.rkt"
         "L5-ast.rkt"
         "L5-substitution-adt.rkt")

(provide (all-defined-out))

;; Support exception handling
(define (try f (ans 'error))
  (with-handlers ((exn?
                   (lambda (exn) ans))) (f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The empy substitution
(define sub0 (make-empty-sub))

;; {T1:number, T2:[T4 -> number], T3:T9}
(define sub1
  (make-sub (map parse-texp '(T1 T2 T3))
            (map parse-texp
                 '(number [T4 -> number] T9))))

;; {T4:[T1->number], T5:boolean, T6:T7}
(define sub2
  (make-sub (map parse-texp '(T4 T5 T6))
            (map parse-texp
                 '([T1 -> number]
                   boolean
                   T7))))

;; {T7:number, T8:[T5 * number -> T3], T9:boolean}
(define sub3
  (make-sub (map parse-texp '(T7 T8 T9))
            (map parse-texp
                 '(number [T5 * number -> T3] boolean))))

(define sub4
  (make-sub (map parse-texp '(T1 T2 T3))
            (map parse-texp
                 '(boolean [number -> T10] [T5 * boolean -> number]))))

;; {T1:boolean, T2:(T5*boolean), T3:number}
(define sub5
  (make-sub (map parse-texp '(T1 T2 T3))
            (map parse-texp
                 '(boolean [T5 -> boolean] number))))

(define sub-to-vars-te-list
  (lambda (sub)
    (let ((vars (map unparse-texp (sub->variables sub)))
          (tes (map unparse-texp (sub->tes sub))))
      (map list vars tes))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sub-combination-tests
  (lambda ()
    (display "sub-combination-tests:\t")
    (run-tests

     (test
      (let ((vars-tes
             (sub-to-vars-te-list
              (sub-combine
               (make-sub (map parse-texp '(T1 T2))
                         (map parse-texp
                              '([number -> S1] [number -> S4])))
               (make-sub (map parse-texp '(T3))
                         (map parse-texp
                              '([number -> S2])))))))
        (set=? (list->set vars-tes)
               (list->set '((T3 (number -> S2))
                            (T1 (number -> S1))
                            (T2 (number -> S4))))))
      => #t)

     (test
      (let
          ((vars-tes
            (sub-to-vars-te-list
             (sub-combine
              (make-sub (map parse-texp '(T1 T2))
                        (map parse-texp
                             '([number -> S1]
                               [number -> T5])))
              (make-sub (map parse-texp '(T3 T4 T5))
                        (map parse-texp
                             '([number -> S2]
                               [number -> S1]
                               boolean)))))))
        (set=? (list->set vars-tes)
               (list->set '((T3 (number -> S2))
                            (T1 (number -> S1))
                            (T2 (number -> boolean))
                            (T4 (number -> S1))
                            (T5 boolean)))))
           => #t)

     (test
      (let
          ((vars-tes
            (sub-to-vars-te-list
             (sub-combine
              (make-sub (map parse-texp '(T1 T2))
                        (map parse-texp
                             '([number -> S1] [T5 -> T4])))
              (make-sub (map parse-texp '(S1 T3 T5 T4))
                        (map parse-texp
                             '(boolean
                               [number -> S2]
                               [number -> S1]
                               boolean)))))))
        (set=? (list->set vars-tes)
               (list->set '((T3 (number -> S2))
                            (T1 (number -> boolean))
                            (T2 ((number -> S1) -> boolean))
                            (T5 (number -> S1))
                            (T4 boolean)
                            (S1 boolean)))))
           => #t)

     (test
      (let ((vars-tes
             (sub-to-vars-te-list
              (sub-combine
               (make-sub (map parse-texp '(T1 T2 T3))
                         (map parse-texp '(S1 [S2 -> number] boolean)))
               (make-sub (map parse-texp '(S1 S2))
                         (map parse-texp
                              '([T5 -> (number * T2 -> T2)] T3)))))))
        (set=? (list->set vars-tes)
               (list->set '((T1 (T5 -> (number * T2 -> T2)))
                            (T2 (T3 -> number))
                            (T3 boolean)
                            (S1 (T5 -> (number * T2 -> T2)))
                            (S2 T3)))))
           => #t)

     (test
      (let ((vars-tes
             (sub-to-vars-te-list
              (sub-combine
               (make-sub (map parse-texp '(T1 T2 T3))
                         (map parse-texp '(number [T4 -> number] T9)))
               (make-sub (map parse-texp '(T4 T5 T6))
                         (map parse-texp '([T1 -> number] boolean T7)))))))
        (set=? (list->set vars-tes)
               (list->set '((T6 T7)
                            (T5 boolean)
                            (T4 (T1 -> number))
                            (T1 number)
                            (T2 ((T1 -> number) -> number))
                            (T3 T9)))))
      => #t)

     ;; Assert error on cicular substitution
     (test
      (try
       (lambda()
         (sub-combine
          (make-sub (map parse-texp '(T3 S1 T4 T5))
                    (map parse-texp
                         '(boolean [number -> T2] [number -> S1] boolean)))
          (make-sub (map parse-texp '(T1 T2))
                    (map parse-texp
                         '([number -> S1] [T3 -> S1]))))))
      => 'error)

     ;; Assert T8 is removed after applying sub2 to sub1
     (test
      (let
          ((vars-tes
            (sub-to-vars-te-list
             (sub-combine
              (make-sub (map parse-texp '(T7 T8))
                        (map parse-texp
                             '(number [T5 * number -> T3])))
              (make-sub (map parse-texp '(T5 T8))
                        (map parse-texp
                             '(T7 boolean)))))))
        (set=? (list->set vars-tes)
               (list->set '((T5 T7)
                            (T7 number)
                            (T8 (T7 * number -> T3))))))
      => #t)

     )))


(define extend-sub-tests
  (lambda ()
    (display "extend-sub-tests:\t")
    (run-tests

     (test
      (let ((vars-tes
             (sub-to-vars-te-list
              (extend-sub
               (make-sub (map parse-texp '(T1 T2 T3))
                         (map parse-texp '(S1 [S2 -> number] boolean)))
               (parse-texp 'S1)
               (parse-texp '[T21 -> (number * T23 -> T22)])))))
        (set=? (list->set vars-tes)
               (list->set '((S1 (T21 -> (number * T23 -> T22)))
                            (T1 (T21 -> (number * T23 -> T22)))
                            (T2 (S2 -> number))
                            (T3 boolean)))))
           => #t)

     ;; Assert error due to circular substitution
     (test
      (try
       (lambda()
         (extend-sub
          (make-sub (map parse-texp '(T1 T2 T3))
                    (map parse-texp '(S1 [S2 -> number] boolean))
                    (parse-texp 'S1)
                    (parse-texp '[T1 -> (number * T23 -> T22)])))))
      => 'error)

     ;; Assert error due to circular substitution
     (test
      (try
       (lambda()
         (extend-sub
          (make-sub (map parse-texp '(T1 T2 T3))
                    (map parse-texp '(S1 [S2 -> number] boolean)))
          (parse-texp 'S1)
          (parse-texp '[S1 -> (number * T23 -> T22)]))))
      => 'error)

     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoking tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sub-combination-tests)
(extend-sub-tests)
