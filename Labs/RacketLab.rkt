#lang  racket
( provide  lookup )
( provide  Leaf )
( provide  Node )
( provide  average )

(struct Leaf (num))
(struct Node (left right))

#| Problem 1 |#
(define (lookup dict k)
	(if (null? dict) "no key"
		(let ([x (first dict)][y (rest dict)])
		(if (eq? (car x) k)(cdr x)
		(lookup y k)))))
		
#| Problem 2 |#		
( define (sumt t )
     (cond
       [( Leaf? t )( Leaf-int t )]
       [( Binary? t )
        (+ (sumt ( Binary-tree1 t ))(sumt ( Binary-tree2 t )))]))
(define (cl t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else (+ (cl (car t)) 
        (cl (cdr t))))))
(define (average t)
    (/ (sumt t)(cl t)))