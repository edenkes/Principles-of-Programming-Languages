#lang racket

(provide (all-defined-out))

;; STACK
;; =============================================================================
;; Purpose: Functional stack - represented as a list of elements - pushed at end of list
;; Signature: push(stack, item)
;; Type: [Stack(T) * T -> Stack(T)]
;; Example:
;; (push '(a b) 'c) -> '(a b c)
(define push
  (lambda (stack item)
    (append stack (list item))))

;; STREAM
;; =============================================================================
;; When tokenizing, we operate over streams of data: 
;; We get a stream of chars, turn it into a stream of tokens, and then into a
;; stream of higher level expressions (each expression built from tokens).
;; To operate over a stream in a functional way - we need a data type that allows
;; us to consume data from the beginning of stream, and then continue where 
;; we stopped.
;; The basic operations over a Stream(T) are:
;; empty-stream?(Stream(T)) -> Boolean
;; make-stream(T, Stream(T)) -> Stream(T)
;; take(Stream(T)) -> Pair(T,stream(T)) - return the first T and the stream without T.
;; take-n(Stream(T)) -> Pair(List(T), Stream(T)) 
;; take-until(pred, Stream(T)) -> Pair(List(T), Stream(T))

;; For example, let us write a simple tokenizer - given a stream of chars
;; we return a stream of lists of chars where we split the groups whereever 
;; we detect spaces.
(define char-stream '("a" "b" " " "c" "d" " " "e"))

;; Stream implementation
;; ---------------------
;; Stream(T) implemented as a List(T)

;; Type: [Stream(T)->Boolean]
(define empty-stream? empty?)

;; Purpose: Construct a stream starting with item
;; Type: [T * Stream(T) -> Stream(T)]
(define make-stream (lambda (item stream) (cons item stream)))

;; Purpose: Take the first item of the stream
;;          return it with the new stream.
;; Type: [Stream(T)->Pair(T, Stream(T))
(define take (lambda (stream) stream))

;; Purpose: take up to n elements from stream and return the stream suffix.
;; Type: [Stream(T) * Number -> Pair(List(T),Stream(T))]
;; Signature: take-n(stream, n)
;; Examples:
;; (take-n '(1 2 3) 2) -> '((1 2) 3)
;; (take-n '(1 2 3) 0) -> '(() 1 2 3)
;; (take-n '(1 2 3) 4) -> '((1 2 3))
(define take-n
  (lambda (stream n)
    (letrec ((loop (lambda (stream n prefix)
                     (cond ((empty? stream) (cons prefix stream))
                           ((= n 0) (cons prefix stream))
                           (else (loop (cdr stream) (- n 1) (push prefix (car stream))))))))
      (loop stream n '()))))

;; Purpose: take the prefix of the stream until a pred is met
;; Signature: take-until(stream)
;; Type: [[T->Boolean] * Stream(T)->Pair(List(T),Stream(T))]
;; Example:
;; (take-until even? '(1 2 3 4)) -> '((1) 2 3 4)
;; (take-until even? '(2 3 4)) -> '(() 2 3 4)
;; (take-until odd? '(2 4 6)) -> '((2 4 6))
(define take-until
  (lambda (pred stream)
    (letrec ((loop (lambda (prefix stream)
                     (cond ((empty? stream) (cons prefix stream))
                           ((pred (car stream)) (cons prefix stream))
                           (else (loop (push prefix (car stream)) (cdr stream)))))))
      (loop '() stream))))

;; Example predicates
(define space? (lambda (c) (string=? c " ")))
(define non-space? (lambda (c) (not (space? c))))

;; Purpose: Turn a stream of T into a stream of List(T)
;;          where all elements in Group(T) satisfy pred
;;          and all elements in stream that do not satisfy pred
;;          are filtered out.
;; Signature: chars-to-groups(stream)
;; Type: [Stream(char)->Stream(List(char))]
;; Examples:
;; (chars-to-groups '(" " "a" "b" " " "c" " "))
;;   --> '(("a" "b") ("c"))
(define chars-to-groups
  (lambda (stream)
    (let ((ns-stream (cdr (take-until non-space? stream))))
      (if (empty? ns-stream)
          '()
          (let ((group-stream (take-until space? ns-stream)))
            (make-stream (car group-stream)
                         (chars-to-groups (cdr group-stream))))))))


;; Let us generalize to generic streams with abstract predicate

;; Purpose: Turn a stream of T into a stream of List(T)
;;          where all elements in Group(T) satisfy pred
;;          and all elements in stream that do not satisfy pred
;;          are filtered out.
;; Signature: make-aggregate-stream(delim?, stream)
;; Type: [[T->Boolean]*Stream(T)->Stream(List(T))]
;; Examples:
;; (make-aggregate-stream space? '(" " "a" "b" " " "c" " "))
;;   --> '(("a" "b") ("c"))
(define make-aggregate-stream
  (lambda (delim? stream)
    (let ((skip-stream (cdr (take-until (lambda (x) (not (delim? x))) stream))))
      (if (empty? skip-stream)
          '()
          (let ((group-stream (take-until delim? skip-stream)))
            (make-stream (car group-stream)
                         (make-aggregate-stream delim? (cdr group-stream))))))))


