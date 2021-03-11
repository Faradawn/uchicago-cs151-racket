#lang typed/racket
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require typed/test-engine/racket-tests)

(define-struct Node
  ([root : Real]
   [l : Tree]
   [r : Tree]))

(define-type Tree (U' 'Empty Node))

(: insert : Integer Tree -> Tree)
(define (insert a n)
  (match n
    ['Empty (Node a 'Empty 'Empty)]
    [(Node root l r) (cond
                       [(< a root) (Node root (insert a l) r)]
                       [(> a root) (Node root l (insert a r))]
                       [else n])]))

; (string<? ) for String tree

(insert 1 (Node 2 'Empty 'Empty)