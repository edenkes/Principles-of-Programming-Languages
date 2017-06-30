#lang racket

(require racket/string)
(require "stream.rkt")
(provide (all-defined-out))

;; SCANNER (TOKENIZER)
;; =============================================================================
;; Define the lexical structure of the language:
;; What are the different types of tokens and how they are recognized from sequences of chars.
;; We use regular expressions to represent this level of structure - which is defined as a regular language of token types.
;; In Scheme:
;; * Parentheses and the quote symbol
(define parens-regexp "[(')]")
;; * Strings are marked by "...." delimiters
;;   An " char inside a string is marked with an escape \\"
(define string-regexp "\"([\\].|[^\\\"])*\"")
;; * Comments start with a ; and go til the end of the line
(define comment-regexp ";[^\n]*")
;; * Any other sequence of character which does not contain a delimiter or a space is a token
(define other-regexp "[^ \t\n('\";)]+")
;; * Between tokens, there can be any number of repeated space chars (space, tab, newlines).
(define inter-token-regexp "\\s*")

;; The alternation of the different token types gives us a way to split a sequence of chars into a sequence of tokens.
;; regexp is the Regexp constructor - it compiles a regular expression noted as a string to an efficient internal representation
;; which is based on a finite automaton.
(define scheme-tokenizer (regexp (string-append inter-token-regexp "(" parens-regexp "|" string-regexp "|" comment-regexp "|" other-regexp ")")))

;; We could have written the whole regexp as one long string instead
;; (define scheme-tokenizer (regexp "\\s*([(')]|\"([\\].|[^\\\"])*\"|;.*|[^ \t\n('\";)]+)"))


;; Purpose: determine the class of a Scheme token based on the characters it contains.
;; Signature: classify-token(token)
;; Type: token-type = 'lparen | 'rparen | 'quote | 'boolean | 'string | 'number | 'comment | 'identifier
;;       [String -> Token-type]
;; Example:
;; (classify-token "12.3") -> 'number
;; (classify-token "#f") -> 'boolean
;; (classify-token "; this is a comment") -> 'comment
(define classify-token
  (lambda (token)
    (cond ((string=? token "'") 'quote)
          ((string=? token "(") 'lparen)
          ((string=? token ")") 'rparen)
          ((string=? token "#t") 'boolean)
          ((string=? token "#f") 'boolean)
          ((string-prefix? token ";") 'comment)
          ((string-prefix? token "\"") 'string)
          ((regexp-match "^[+-]?[0-9]+(\\.[0-9]*)?$" token) 'number)
          (else 'identifier))))


;; Purpose: tokenize a stream of bytes or chars into a stream of Scheme tokens
;;          read one token from a stream of char and advance the stream to the position after the token.
;;          Return #f when invoked after the eof of the stream.
;; Signature: next-token(in)
;; Type: [Input-Port -> String | #f]
;; Note: (regexp-match re in) is a Racket primitive which returns either:
;; - #f if no match is found in the input stream
;; - A list (<matched sequence> ...) with the string that is matched in the input stream
;;   as a side effect the stream is advanced to the position past the match.
;;   The return value can include additional info depending on the structure of the regexp.
;;   The return value can be a string or a sequence of bytes corresponding to a UTF-8 encoding.
(define next-token
  (lambda (in)
    (let ((match (regexp-match scheme-tokenizer in)))
      (cond ((not match) #f)
            (else (let ((matched (car match)))
                     (if (string? matched)
                         matched
                         (bytes->string/utf-8 matched))))))))


;; Purpose: Scan a whole stream of Scheme text and return a list of Scheme tokens.
;; Signature: scan-stream(in)
;; Type: [Stream -> List(String)]
(define scan-stream
  (lambda (in)
    (letrec ((loop (lambda (l)
                     (let ((token (next-token in)))
                       (if (not token)
                           l
                           (loop (push l token)))))))
      (loop '()))))


;; Purpose: Scan a whole file of Scheme text and return a list of Scheme tokens.
;; Signature: scan-file(filename)
;; Type: [String -> List(String)]
(define scan-file
  (lambda (filename)
    (call-with-input-file filename scan-stream)))

;; Purpose: Scan a string and return a list of Scheme tokens.
;; Signature: scan-string(s)
;; Type: [String -> List(String)]
(define scan-string
  (lambda (s)
    (let ((in (open-input-string s)))
      (scan-stream in))))
