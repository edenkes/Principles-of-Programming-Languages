#lang racket
(require "stream.rkt" "scanner.rkt")

(provide (all-defined-out))

;; READER - Parse a list of tokens into a SExp
;; =============================================================================

;; SExp are lists of Sexp or atomic expressions.
;; They are used to encode literal expressions in Scheme.
;; They are defined as a Context Free Grammar (CFG)
;; because a regular language is not expressive enough to represent recursive nesting.

;; We map atomic tokens (number, boolean, string, identifier) to corresponding
;; Scheme value types (number, boolean, string, symbol).

;; sexp ::= number |
;;          boolean |
;;          string |
;;          identifier |
;;          lparen sexp* rparen |
;;          quote sexp

;; Purpose: Parse one sexp from the beginning of a stream of tokens
;;          This transforms a stream of tokens into a stream of Sexp.
;;          Return the first sexp in the stream and the rest of the stream as a pair.
;;          Skip the comments
;; Signature: tokens->sexp(tokens)
;; Type: [List(String) -> Pair(SExp, List(String))]
;; Example:
;; (tokens->sexp '( "(" "1" ")" "2" )) -> '( (1) "2" )
(define tokens->sexp
  (lambda (tokens)
    (if (empty? tokens)
        '()
        (let ((first (car tokens))
              (rest (cdr tokens)))
          (let ((first-class (classify-token first)))
            (cond 
              ;; Construct a number value from its string representation
              ((eq? first-class 'number)
               (cons (string->number first) rest))
              ;; Construct a boolean value
              ((eq? first-class 'boolean)
               (cons (string=? first "#t") rest))
              ;; Remove the "" delimiters of strings
              ((eq? first-class 'string)
               (cons (substring first 1 (- (string-length first) 1)) rest))
              ;; Construct a symbol value
              ((eq? first-class 'identifier)
               (cons (string->symbol first) rest))
              ;; Recursively parse a list of sexp til the closing paren
              ((eq? first-class 'lparen)
               (letrec ((loop (lambda (tokens rest)
                                (let ((next (tokens->sexp rest)))
                                  (let ((next-token (car next))
                                        (rest (cdr next)))
                                    (if (eq? next-token 'rparen)
                                        (cons tokens rest)
                                        (loop (push tokens next-token) rest)))))))
                 (loop '() rest)))
              ((eq? first-class 'rparen)
               (cons 'rparen rest))
              ;; Construct a literal expression from the next sexp
              ((eq? first-class 'quote)
               (let ((next (tokens->sexp rest)))
                 (cons (list 'quote (car next)) (cdr next))))
              ;; Skip comments
              ((eq? first-class 'comment)
               (tokens->sexp rest))
              (else (error "bad token" first))))))))


;; Purpose: Parse a stream as a list of sexp
;;          Perform tokenization then sexp parsing.
;; Signature: read-stream-as-sexps(in)
;; Type: [Stream -> List(Sexp)]
(define read-stream-as-sexps
  (lambda (in)
    (let ((tokens (scan-stream in)))
      (letrec ((loop (lambda (tokens sexps)
                       (if (empty? tokens)
                           sexps
                           (let ((next-sexp (tokens->sexp tokens)))
                             (if (empty? next-sexp)
                                 sexps
                                 (loop (cdr next-sexp) (push sexps (car next-sexp)))))))))
        (loop tokens '())))))

;; Purpose: Parse a whole file as a list of sexp
;; Signature: read-file-as-sexps(filename)
;; Type: [String -> List(Sexp)]
(define read-file-as-sexps
  (lambda (filename)
    (call-with-input-file filename read-stream-as-sexps)))
    

;; Purpose: Parse a whole file as a list of sexp
;; Signature: read-string-as-sexps(filename)
;; Type: [String -> List(Sexp)]
(define read-string-as-sexps
  (lambda (s)
    (let ((in (open-input-string s)))
      (read-stream-as-sexps in))))
