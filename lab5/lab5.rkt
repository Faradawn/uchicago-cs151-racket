#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

;; lab5 collaborators: completed individually

(define-type (Optional A)
  (U 'None (Some A)))

(define-struct (Some A)
  ([value : A]))

(define-type BST 
  (U 'Empty Node))

(define-struct Node
  ([shop : String]
   [ratings : (Listof Integer)]
   [lsub : BST]
   [rsub : BST]))

(define-type Directions   (Listof (U 'left 'right)))

;; example BST
(: tree0 BST)
(define tree0 'Empty)
(: tree1 BST)
(define tree1 (Node "alpha" '() 'Empty
                    (Node "beta" '(1 2 3) 'Empty 'Empty)))
(: tree2 BST)
(define tree2 (Node "charlie" '(5 5 2 1)
                    (Node "alpha" '(1 0) 'Empty 'Empty)
                    (Node "epsilon" '(-1 -2)
                          (Node "delta" '(0) 'Empty 'Empty)
                          'Empty)))
;; 0/7 helper functions
;;     insert-string takes in a string and puts it in a sorted list
(: insert-string : String (Listof String) -> (Listof String))
(define (insert-string s ls)
  (match ls
    ['() (list s)]
    [(cons head tail) (if (string<? s head) (cons s ls)
                          (cons head (insert-string s tail)))]))
(check-expect (insert-string "c" '("a" "b" "d")) '("a" "b" "c" "d"))
;;     insert-sort sorts a list of string in ascending order
(: insert-sort : (Listof String) -> (Listof String))
(define (insert-sort ls)
  (foldr insert-string '() ls))
(check-expect (insert-sort '("b" "d" "c" "a")) '("a" "b" "c" "d"))
;;     avg computes the average rating. Should not take in empty list.
(: avg : (Listof Exact-Rational) -> Exact-Rational)
(define (avg ls)
  (/ (foldr + 0 ls) (length ls)))
(check-expect (avg '(1/2 1/2 3 4 2)) 2)
;;     contain? checks whether the target shop is in the tree
(: contain? : String BST -> Boolean)
(define (contain? name tree)
  (match tree
    ['Empty #f]
    [(Node root _ l r) [cond
                         [(string<? name root) (contain? name l)]
                         [(string>? name root) (contain? name r)]
                         [else #t]]]))
(check-expect (contain? "delta" tree1) #f)
(check-expect (contain? "delta" tree2) #t)
(check-expect (contain? "charlie" tree2) #t)

                         
     



;; 1/7 add-rating 
;;     Adds a rating for a shop. If the shop is not yet in
;;     the BST, add it; otherwise, add to the *front* of the
;;     existing rating list.
(: add-rating : String Integer BST -> BST)
(define (add-rating name n tree)
  (match tree
    ['Empty (Node name (list n) 'Empty 'Empty)]
    [(Node root ratings l r)
     (cond
       [(string<? name root) (Node root ratings (add-rating name n l) r)]
       [(string>? name root) (Node root ratings l (add-rating name n r))]
                               [else (Node name (cons n ratings) l r)])]))

(check-expect (add-rating "chi" 1 'Empty) (Node "chi" '(1) 'Empty 'Empty))
(check-expect (add-rating "beta" 2 tree1)
              (Node "alpha" '() 'Empty
                    (Node "beta" '(2 1 2 3) 'Empty 'Empty)))
(check-expect (add-rating "zeta" 3 tree2)
              (Node "charlie" '(5 5 2 1)
                    (Node "alpha" '(1 0) 'Empty 'Empty)
                    (Node "epsilon" '(-1 -2)
                          (Node "delta" '(0) 'Empty 'Empty)
                          (Node "zeta" '(3) 'Empty 'Empty))))

;; 2/7 shop-names
;;     Produces a sorted list of the shop names in the BST.
;;     First extracts the names from BST.
;;     Then uses helper insert-sort to sort the String list.
(: shop-names : BST -> (Listof String))
(define (shop-names tree)
  (local
       {(: extract-names : BST -> (Listof String))
        (define (extract-names t)
          (match t
            ['Empty '()]
            [(Node root _ l r)
             (cons root
                   (append (extract-names l)(extract-names r)))]))}
    (insert-sort (extract-names tree))))

(check-expect (shop-names tree1) '("alpha" "beta"))
(check-expect (shop-names tree2) '("alpha" "charlie" "delta" "epsilon"))
     

;; 3/7 highly-rated
;;     Produces an alphabetical list of shop names for shops
;;     in the BST that  have a rating greater than or equal
;;     to the rational number threshold. Shops with no 
;;     ratings should not be included in the output.
(: highly-rated : BST Exact-Rational -> (Listof String))
(define (highly-rated t bar)
  (match t
    ['Empty '()]
    [(Node root ratings l r)
     (if (and (not (empty? ratings)) (>= (avg ratings) bar))
         (cons root (append (highly-rated l bar)(highly-rated r bar)))
         (append (highly-rated l bar) (highly-rated r bar)))]))

(check-expect (highly-rated tree1 1/2) '("beta"))
(check-expect (highly-rated tree2 0) '("charlie" "alpha" "delta"))
(check-expect (highly-rated tree2 -5) '("charlie" "alpha" "epsilon" "delta"))


;; 4/7 prune-alpha
;;     Takes in a BST and two strings, start and end, where
;;     start is alphabetically before (or equal to) end.
;;     Returns a BST that is identical to the input BST,
;;     except keeping only the nodes where the shop is
;;     alphabetically between start and end (inclusive).
(: prune-alpha : BST String String -> BST)
(define (prune-alpha tree a b)
  'Empty)

;; 5/7 most-recent-rating
;;     Returns the most recent rating that the given shop has
;;     received, if any, and otherwise returns 'None.
;;     Note that the shop name input may or may not appear in
;;     the BST.
(: most-recent-rating : String BST -> (Optional Integer))
(define (most-recent-rating name tree)
  (match tree
    ['Empty 'None]
    [(Node root ratings l r)
     (cond 
       [(string<? name root) (most-recent-rating name l)]
       [(string>? name root) (most-recent-rating name r)]
       [else (Some (first ratings))])]))

; (check-expect (most-recent-rating "alpha" tree1) 'None) ;; todo
(check-expect (most-recent-rating "alpha" tree2) (Some 1))

;; 6/7 directions-to   
;;     Gives directions in the BST from the root to the
;;     node with that shop name.
;;     If that shop is not in the BST, gives an error.;; (example below)
(: directions-to : String BST -> Directions)
(define (directions-to name tree)
  (match tree
    ['Empty '()]
    [(Node root ratings l r)
     (if (contain? name tree) 
         (cond
           [(string>? name root) (cons 'right (directions-to name r))]
           [(string<? name root) (cons 'left (directions-to name l))]
           [else '()])
         (error "shop unfound"))]))
(check-expect (directions-to "alpha" tree1) '())
(check-expect (directions-to "delta" tree2) (list 'right 'left))
(check-error (directions-to "phi" tree2) "shop unfound") 

;; 7/7 follow-directions
;;     Given a BST and a list of directions, follows the
;;     directions and returns the shop name at that location.
;;     If there is not shop at that location, gives an error.
(: follow-directions : BST Directions -> String)
(define (follow-directions tree dir)
  (match tree
    ['Empty (error "no shop at place")]
    [_ (match (foldl (λ ([x : (U 'left 'right)] [y : BST])
                     (match x
                       ['left (match y [(Node _ _ l _) l] [_ 'Empty])]
                       ['right (match y [(Node _ _ _ r) r] [_ 'Empty])]
                       [_ y])) tree dir)
         [(Node root _ _ _) root]
         ['Empty (error "no shop at place")])]))

(check-expect (follow-directions tree2 (list 'right 'left)) "delta")
(check-expect (follow-directions tree1 '()) "alpha")
(check-error (follow-directions tree2 (list 'left 'left 'left))
             "no shop at place")
(check-error (follow-directions 'Empty '()) "no shop at place")


(test)

          