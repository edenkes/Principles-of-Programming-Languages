#lang racket
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Q3a ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define append$
 (lambda (x y cont)
  (if (empty? x)
        (cont y)


        (append$ (cdr x) y (lambda(res-acc)
                 (cont (cons (car x) res-acc)))))
  )
)


(define make-tree list)
(define add-subtree cons)
(define make-leaf (lambda (d) d))
(define empty-tree empty)
(define first-subtree car)
(define rest-subtrees cdr)
(define leaf-data (lambda (x) x))
(define composite-tree? pair?)
(define leaf?
(lambda (t) (not (list? t))))
(define empty-tree? empty?)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Q3b ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define equal-trees$ 
 (lambda (tree1 tree2 succ fail)
   ( cond ((and ( empty-tree? tree1 ) ( empty-tree? tree2 )) (succ '()))
             (( empty-tree? tree1 ) ( fail (list tree1 (car tree2)) ))
             (( empty-tree? tree2 ) ( fail (list (car tree1)) ))
             ((and ( leaf? tree1 ) ( leaf? tree2 ) )
              ( succ (cons tree1 tree2)))
             ((or ( leaf? tree1 ) ( leaf? tree2 ) )
              ( fail (cons tree1 tree2)))
             ( else ( equal-trees$
                      (first-subtree tree1 )
                      (first-subtree tree2 )
                      ( lambda ( first-res )
                         ( equal-trees$
                           ( rest-subtrees tree1 )
                           ( rest-subtrees tree2 )
                           ( lambda ( rest-res )
                              ( succ  ( add-subtree first-res rest-res )))
                           ( lambda ( rest-res )
                              ( fail  rest-res))))
                      ( lambda ( first-res )
                         ( fail first-res)))))
 )
)