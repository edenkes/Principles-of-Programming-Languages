#lang racket
(require "parser.rkt")

; Purpose: Produce a list of lists of length window each with the elements slided by factor window 
; Signature: sliding-window(list,window)
; Type: [List * Number -> List]
(define sliding-window (lambda (lst len)
                         (if (< (length lst) len) '() 
                             (append (list (take lst len))
                                     (sliding-window (rest lst) len))))) 

; Purpose: Compute the greatest node data value in the tree
; Signature: greatest-node(tree)
; Type: [List(Number) -> Number]
(define greatest-node
  (lambda (tree)
    (cond
      ((empty? tree) -1)
      ((list? tree)
       (max (greatest-node (first tree))
            (greatest-node (second tree))
            (greatest-node (third tree))))
      (else tree))))

; Purpose: Compute the number of nodes whose data value is equal to x
; Signature: count-node(tree,x)
; Type: [List * T -> Number]
(define count-node (lambda (tree x)
                     (cond
                       ((empty? tree) 0)
                       ((list? tree)
                        (+ (count-node (first tree) x)
                           (count-node (second tree) x)
                           (count-node (third tree) x) ))
                       (else (if (eq? tree x)  1 0)))))

; Purpose: Compute the mirrored tree
; Signature: mirror-tree(tree)
; Type: [List -> List]
 (define mirror-tree (lambda (tree)
                       (cond
                         ((empty? tree) '())
                         ((list? tree)
                          (list (mirror-tree (first tree))
                                (mirror-tree (third tree))
                                (mirror-tree (second tree))))
                         (else tree))))


 ; Purpose: unparse a Scheme AST into Javascript syntax without considering infix notation
 ; Signature: unparse->js(ast,output-port)
 ; Type: [Exp * Output-Port -> Void]
 (define unparse->js
   (lambda (ast output-port)
     (cond ((def-exp? ast) (fprintf output-port "const ")
                            (unparse->js (def-exp->var ast) output-port)
                            (fprintf output-port " = ")
                            (unparse->js (def-exp->val ast) output-port)
                            (fprintf output-port ";"))
          ((cexp? ast)
           (cond ((num-exp? ast) (fprintf output-port "~a" (num-exp->val ast)))
                 ((bool-exp? ast) (fprintf output-port "~s"(if (eq? (bool-exp->val ast) #t) 'true 'false)))
                 ((str-exp? ast)  (fprintf output-port "~s" (str-exp->val ast)))
                 ((var-exp? ast)  (fprintf output-port "~s" (if(eq? (var-exp->var ast) '=) '== (var-exp->var ast))))
                 ((literal-exp? ast) (list 'quote (literal-exp->val ast)))
                 ((proc-exp? ast) (fprintf output-port "(")
                                  (map (lambda(x) (unparse->js x output-port)
                                            (fprintf output-port ","))
                                            (drop-right(proc-exp->params ast) 1 ))
                                       (unparse->js (last (proc-exp->params ast)) output-port)
                                   (fprintf output-port ") => { ")
                                   (map (lambda(x) (unparse->js x output-port)
                                            (fprintf output-port "; "))
                                            (drop-right(proc-exp->body ast) 1 ))
                                       (unparse->js (last (proc-exp->body ast)) output-port)
                                       (fprintf output-port " }"))
                 ((if-exp? ast) (unparse->js (if-exp->test ast)  output-port)
                                (fprintf output-port " ? ")
                                (unparse->js (if-exp->then ast)  output-port)
                                (fprintf output-port " : ")
                                 (unparse->js (if-exp->else ast)  output-port))
                 ((let-exp? ast) (fprintf output-port "let ")
                                 (map (lambda (b) (unparse->js (binding->var b)  output-port)
                                                   (fprintf output-port " = ")
                                                   (unparse->js (binding->val b)  output-port)
                                                   (fprintf output-port ", "))
                                                 (drop-right(let-exp->bindings ast) 1 ))
                                 (unparse->js (binding->var (last (let-exp->bindings ast))) output-port)
                                 (fprintf output-port " = ")
                                 (unparse->js (binding->val (last (let-exp->bindings ast))) output-port)
                                 (fprintf output-port "; ")
                                 (map (lambda (b) (unparse->js b output-port)
                                                   (fprintf output-port "; "))
                                      (drop-right(let-exp->body ast) 1 ))
                                 (unparse->js (last (let-exp->body ast)) output-port)
                                 (fprintf output-port ";"))
                 ((app-exp? ast) (unparse->js (app-exp->rator ast) output-port)
                                   (fprintf output-port "(")
                                       (map (lambda(x) (unparse->js x output-port)
                                            (fprintf output-port ","))
                                            (drop-right(app-exp->rands ast) 1 ))
                                       (unparse->js (last (app-exp->rands ast)) output-port)
                                   (fprintf output-port ")"))
                 (else (error "Unknown exp type: " ast))))
          (else (error "Unknown exp type: " ast)))))

 ; Purpose: unparse a Scheme AST into Javascript syntax while considering infix notation
 ; Signature: unparse->js-infix(ast,output-port)
 ; Type: [Exp * Output-Port -> Void]
 (define unparse->js-infix
   (lambda (ast output-port)
     (cond ((def-exp? ast) (fprintf output-port "const ")
                            (unparse->js-infix (def-exp->var ast) output-port)
                            (fprintf output-port " = ")
                            (unparse->js-infix (def-exp->val ast) output-port)
                            (fprintf output-port ";"))
          ((cexp? ast)
           (cond ((num-exp? ast) (fprintf output-port "~a" (num-exp->val ast)))
                 ((bool-exp? ast) (fprintf output-port "~s"(if (eq? (bool-exp->val ast) #t) 'true 'false)))
                 ((str-exp? ast)  (fprintf output-port "~s" (str-exp->val ast)))
                 ((var-exp? ast)  (fprintf output-port "~s" (if(eq? (var-exp->var ast) '=) '== (var-exp->var ast))))
                 ((literal-exp? ast) (list 'quote (literal-exp->val ast)))
                 ((proc-exp? ast) (fprintf output-port "(")
                                  (map (lambda(x) (unparse->js-infix x output-port)
                                            (fprintf output-port ","))
                                            (drop-right(proc-exp->params ast) 1 ))
                                       (unparse->js-infix (last (proc-exp->params ast)) output-port)
                                   (fprintf output-port ") => { ")
                                   (map (lambda(x) (unparse->js-infix x output-port)
                                            (fprintf output-port "; "))
                                            (drop-right(proc-exp->body ast) 1 ))
                                       (unparse->js-infix (last (proc-exp->body ast)) output-port)
                                       (fprintf output-port " }"))
                 ((if-exp? ast) (unparse->js-infix (if-exp->test ast)  output-port)
                                (fprintf output-port " ? ")
                                (unparse->js-infix (if-exp->then ast)  output-port)
                                (fprintf output-port " : ")
                                 (unparse->js-infix (if-exp->else ast)  output-port))
                 ((let-exp? ast) (fprintf output-port "let ")
                                 (map (lambda (b) (unparse->js-infix (binding->var b)  output-port)
                                                   (fprintf output-port " = ")
                                                   (unparse->js-infix (binding->val b)  output-port)
                                                   (fprintf output-port ", "))
                                                 (drop-right(let-exp->bindings ast) 1 ))
                                 (unparse->js-infix (binding->var (last (let-exp->bindings ast))) output-port)
                                 (fprintf output-port " = ")
                                 (unparse->js-infix (binding->val (last (let-exp->bindings ast))) output-port)
                                 (fprintf output-port "; ")
                                 (map (lambda (b) (unparse->js-infix b output-port)
                                                   (fprintf output-port "; "))
                                      (drop-right(let-exp->body ast) 1 ))
                                 (unparse->js-infix (last (let-exp->body ast)) output-port)
                                 (fprintf output-port ";"))
                 ((app-exp? ast) 
                  (cond ( (or(eq? (var-exp->var(app-exp->rator ast)) '+)
                         (eq? (var-exp->var(app-exp->rator ast)) '-)
                         (eq? (var-exp->var(app-exp->rator ast)) '*)
                         (eq? (var-exp->var(app-exp->rator ast)) '/)
                         (eq? (var-exp->var(app-exp->rator ast)) '=))
                     (fprintf output-port "(")
                     (map (lambda(x) (unparse->js-infix x output-port)
                                     (fprintf output-port " ")
                                     (unparse->js-infix (app-exp->rator ast) output-port)
                                     (fprintf output-port " "))
                          (drop-right(app-exp->rands ast) 1 ))
                     (unparse->js-infix (last (app-exp->rands ast)) output-port)
                     (fprintf output-port ")"))
                    (else
                     (unparse->js-infix (app-exp->rator ast) output-port)
                     (fprintf output-port "(")
                     (map (lambda(x) (unparse->js-infix x output-port)
                                     (fprintf output-port ","))
                          (drop-right(app-exp->rands ast) 1 ))
                     (unparse->js-infix (last (app-exp->rands ast)) output-port)
                     (fprintf output-port ")"))))
                 (else (error "Unknown exp type: " ast))))
          (else (error "Unknown exp type: " ast)))))