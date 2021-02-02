#lang typed/racket

(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

(define-type Nat
  (U 'Z S))

(define-struct S
  ([n : Nat]))

(define-struct Point ;; 'Point' is a 'struct' and a 'type'
  ([x : Integer]
   [y : Integer])) ;; Integer is 'type' and Nat is 'type' ?


;; (: ) function takes in type (: a Type)
;; Integer is type
;; Nat is type
;;
;; Point is type and struct
;;    Every struct is a type?
;; 
;; S is struct (type?)
;;    How to define something to be S?
;;    (: c S)
;;    (define c (Nat c))


(define-type ArithTree 
  (U Leaf PlusTree TimesTree PowerTree))

(define-struct Leaf 
  ([n : Integer]))

(define-struct PlusTree 
  ([lsub : ArithTree] 
   [rsub : ArithTree]))

(define-struct TimesTree 
  ([lsub : ArithTree] 
   [rsub : ArithTree]))

(define-struct PowerTree 
  ([lsub : ArithTree] 
   [rsub : ArithTree]))

(define-type StringTree 
  (U 'Empty StringNode))

(define-struct StringNode 
  ([root : String] 
   [lsub : StringTree] 
   [rsub : StringTree]))

(: same-tree? : StringTree StringTree -> Boolean)
(define (same-tree? tree1 tree2)
  (match* (tree1 tree2)
    [('Empty 'Empty) #t]
    [('Empty _) #f]
    [(_ 'Empty) #f]
    [((StringNode root1 l1 r1)
      (StringNode root2 l2 r2))
     (cond
       [(string=? root1 root2) (and (same-tree? l1 l2) (same-tree? r1 r2))]
       [else #f])]))


(: subset? : StringTree StringTree -> Boolean)
(define (subset? tree1 tree2)
  (match tree2
    ['Empty #f]
    [(StringNode root2 l2 r2)
     (cond
       [(or (same-tree? tree1 l2) (same-tree? tree1 r2) #t)]
       [else
        (match tree1
          ['Empty #f]
          [(StringNode root1 l1 r1)
           (or
            (subset? l1 l2)
            (subset? l1 r2)
            (subset? r1 l2)
            (subset? r1 r2)) #t]
          )])]))