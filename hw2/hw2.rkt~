
;; Faradawn Yang
;; Feb 1, 2021
;; CS 15100 Homework 2 - Recursive Tree 

#lang typed/racket

(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

(define-type Nat ;; Define Nat for Natural Numbers
  (U 'Z S)) 
(define-struct S ; Define S for Successor
  ([n : Nat]))


;; Part 1 - Natural Number Operation
;;    (nat-equals?)
;;        Takes in two Nat and check if reach 'Z simultaneously.
;;    (nat-greater-than?)
;;        Takes in two Nat;
;;        Whoever reaches 'Z first will be the small one.
;;    (nat-to-string)
;;        Takes in a Nat and append "S" after each iteration.
;;        Appends "Z" at the final iteration.
;;
(: nat-equals? : Nat Nat -> Boolean)
(define (nat-equals? a b)
  (match* (a b)
    [('Z 'Z) #t]
    [('Z _) #f]
    [(_ 'Z) #f]
    [((S a) (S b)) (nat-equals? a b)]))

(: nat-greater-than? : Nat Nat -> Boolean)
(define (nat-greater-than? a b)
  (match* (a b)
    [('Z 'Z) #f]
    [(_ 'Z) #t]
    [('Z _) #f]
    [((S a) (S b)) (nat-greater-than? a b)]))

(: nat-to-string : Nat -> String)
(define (nat-to-string a)
  (match a
    ['Z "Z"]
    [(S a) (string-append "S" (nat-to-string a))]))

;; Tests:
;;     a is 1
;;     b is 2
;;     c is 2
;;
(define a 'Z)
(define b (S (S 'Z)))
(define c (S (S 'Z)))

(check-expect(nat-equals? a b) #f)
(check-expect(nat-equals? b c) #t)
(check-expect(nat-greater-than? b c) #f)
(check-expect(nat-greater-than? c a) #t)
(check-expect(nat-to-string c) "SSZ")



;; Part 2 - Arithmetic Tree

;;;;;;;;;
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
;;;;;;;;;

;; 2-1: (eval)
;;      Takes in two leaves and computer one of the three operations: + - ^
;;      Power computation only allows positive powers. Else, throws an error.
;;
(: eval : ArithTree -> Integer)
(define (eval a)
  (match a
    [(Leaf a) a]
    [(PlusTree l r) (+ (eval l) (eval r))]
    [(TimesTree l r) (* (eval l) (eval r))]
    [(PowerTree l r) (cond
                       [(>= (eval r) 0)
                        (exact-ceiling (expt (eval l) (eval r)))]
                       [else (error "power is negative")])]))
    
(check-expect (eval (PlusTree (Leaf 1) (Leaf 2))) 3)
(check-expect (eval (TimesTree (Leaf 1) (Leaf 0))) 0)
(check-error (eval (PowerTree (Leaf 2) (Leaf -1))) "power is negative")
(check-expect (eval (PowerTree (Leaf 2) (PlusTree (Leaf 1) (Leaf 2)))) 8)

;; 2-2: (arith-tree-to-string)
;;      Takes in an ArithTree.
;;      Use 'string->number' to convert each leaf into a string.
;;      Append, e.g., "(+ " and ")" to the two ends.
;;
(: arith-tree-to-string : ArithTree -> String)
(define (arith-tree-to-string a)
  (match a
    [(Leaf a) (number->string a)]
    [(PlusTree l r) (string-append "(+ "
                                   (arith-tree-to-string l) " "
                                   (arith-tree-to-string r) ")")]
    [(TimesTree l r) (string-append "(* "
                                   (arith-tree-to-string l) " "
                                   (arith-tree-to-string r) ")")]
    [(PowerTree l r) (string-append "(expt "
                                   (arith-tree-to-string l) " "
                                   (arith-tree-to-string r) ")")]))

(check-expect (arith-tree-to-string (PowerTree
                                     (Leaf 2)
                                     (PlusTree (Leaf 1) (Leaf 2))))
                                    "(expt 2 (+ 1 2))")
;; 2-3 (num-internal-nodes)
;;     Takes in an ArithTree and counts how many operations were went through.
;;
(: num-internal-nodes : ArithTree -> Integer)
(define (num-internal-nodes a)
  (match a
    [(Leaf a) 0]
    [(PlusTree l r) (+ 1 (num-internal-nodes l) (num-internal-nodes r))]
    [(TimesTree l r) (+ 1 (num-internal-nodes l) (num-internal-nodes r))]
    [(PowerTree l r) (+ 1 (num-internal-nodes l) (num-internal-nodes r))]))

(check-expect (num-internal-nodes (PowerTree
                                   (Leaf 2)
                                   (PlusTree (Leaf 1) (Leaf 2)))) 2)
(check-expect (num-internal-nodes (PowerTree
                                   (PlusTree (Leaf 1) (Leaf 2))
                                   (PlusTree
                                    (TimesTree (Leaf 1) (Leaf 2)) (Leaf 2)))) 4)

;; 2-4 (num-leaves)
;;     Takes in an ArithTree and counts how many Leaf
;;     Leaf number should be node + 1.
;;
(: num-leaves : ArithTree -> Integer)
(define (num-leaves a)
  (+ 1 (num-internal-nodes a)))

(check-expect (num-leaves (PowerTree
                           (PlusTree (Leaf 1) (Leaf 2))
                           (PlusTree
                            (TimesTree (Leaf 1) (Leaf 2)) (Leaf 2)))) 5)

;; 2-5 (internal-nodes-to-leaves)
;;     Takes in an ArithTree and counts
(: internal-nodes-to-leaves : ArithTree -> Exact-Rational)
(define (internal-nodes-to-leaves a)
  (/ (num-internal-nodes a) (num-leaves a)))

(check-expect (internal-nodes-to-leaves (PowerTree
                                   (PlusTree (Leaf 1) (Leaf 2))
                                   (PlusTree
                                    (TimesTree (Leaf 1) (Leaf 2)) (Leaf 2))))
              4/5)


;; Part 3 - String Tree

;;;;;;
(define-type StringTree 
  (U 'Empty StringNode))

(define-struct StringNode 
  ([root : String] 
   [lsub : StringTree] 
   [rsub : StringTree]))
;;;;;;

;; 3-1 (contains?)
;;     Takes in a String and StringTree.
;;     Checks whether the target String and match any 'root' String.
;;     
(: contains? : String StringTree -> Boolean)
(define (contains? s tree)
  (match tree
    ['Empty #f]
    [(StringNode root l r) (cond
                             [(string=? root s) #t]
                             [else (or (contains? s l) (contains? s r))])]))

(define tree1 (StringNode "root1"
                          (StringNode "root2" 'Empty 'Empty)
                          (StringNode "root3" 'Empty
                                      (StringNode "root4" 'Empty 'Empty))))
(define tree2 (StringNode "root"
                          'Empty
                          (StringNode "root1" 'Empty
                                      (StringNode "root" 'Empty 'Empty))))
(define tree3 (StringNode "root1"
                          (StringNode "root2" 'Empty 'Empty)
                          (StringNode "root3" 'Empty
                                      (StringNode "root4" 'Empty 'Empty))))
(define tree4 (StringNode "root1" 'Empty' Empty))
                                      

(check-expect (contains? "root2" tree1) #t)
(check-expect (contains? "root" tree2) #t)
(check-expect (contains? "nope" tree1) #f)

;; 3-2 (count)
;;     Takes in a String and StringTree.
;;     +1 and continues when finds a match;
;;     Continues without adding when didn't find a match.
;;
(: count : String StringTree -> Integer)
(define (count s tree)
  (match tree
    ['Empty 0]
    [(StringNode root l r) (cond
                             [(string=? root s) (+ 1 (count s l) (count s r))]
                             [else (+ (count s l) (count s r))])]))

(check-expect (count "root" tree1) 0)
(check-expect (count "root" tree2) 2)

;; 3-3 (total-length)
;;     Takes in a StringTree.
;;     Get every String's leagth and add together.
;; 
(: total-length : StringTree -> Integer)
(define (total-length tree)
  (match tree
    ['Empty 0]
    [(StringNode root l r) (+ (string-length root)
                              (total-length l)
                              (total-length r))]))

(check-expect (total-length tree1) 20)

;; 3-4 (same-tree?)
;;     Takes in two StringTrees.
;;     Check if they are same on each iteration.
;; 
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

(check-expect (same-tree? tree1 tree2) #f)
(check-expect (same-tree? tree1 tree3) #t)

;; 3-5 (subset?)
;;     Takes in two StringTrees.
;;     Uses (same-tree?) to compare tree1 to every level of tree2.
;;     Uses match 'Empty twice to create stop condition.
;;
(: subset? : StringTree StringTree -> Boolean)
(define (subset? tree1 tree2)
  (match tree2
    ['Empty #f]
    [(StringNode root2 l2 r2)
     (cond
       [(same-tree? tree1 tree2) #t]
       [(or (same-tree? tree1 l2) (same-tree? tree1 r2)) #t]
       [else
        (match tree1
          [`Empty #f]
          [(StringNode roo1 l1 r1)
           (cond
             [(same-tree? tree1 l2) #t]
             [(same-tree? tree1 r2) #t]
             [else #f])])])]))
        

(check-expect (subset? tree2 tree1) #f)
(check-expect (subset? tree1 tree1) #t)
(test)

;; For test (check-expect (subset? tree4 tree1) #t)
;;      tree4 is countained in tree1, but didn't work
;;      Tried many times but failed
;;      Sorry that I could not figure out the last problem
;;      
;; This is the end of Homework 2
;;      Thank you for your patient grading
;;      Wish you a wonderful day!
;;      -- Faradawn
;;

