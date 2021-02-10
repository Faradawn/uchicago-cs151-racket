#lang typed/racket

;; Lab 3 - Travel Guide
;; Faradawn Yang
;; Collaborators: You Li
;; Feb 4, 2021

(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

(define-struct MenuItem
  ([kind  : (U 'appetizer 'entree 'dessert)]
   [name  : String]
   [price : Integer] ;; <-- integer num of pennies*
   [veg   : (U 'nonveg 'vegetarian 'vegan)]))

;; *inexact numbers aren't good for representing money

(define-type Menu
  (Listof MenuItem))

;; Define a few example Menus for tests
(define menu1 (list
               [MenuItem 'appetizer "salad1" 10 'vegan]
               [MenuItem 'entree "entree1" 10 'nonveg]
               [MenuItem 'appetizer "salad2" 10 'vegetarian]
               [MenuItem 'entree "entree2" 10 'nonveg]
               [MenuItem 'dessert "dessert1" 490 'nonveg]
               [MenuItem 'dessert "dessert2" 500 'nonveg]))
(define menu2 (list
               [MenuItem 'appetizer "salad1" 10 'vegan]
               [MenuItem 'entree "entree1" 10 'nonveg]
               [MenuItem 'appetizer "salad2" 10 'vegetarian]
               [MenuItem 'entree "entree2" 10 'nonveg]
               [MenuItem 'dessert "dessert1" 510 'nonveg]
               [MenuItem 'dessert "dessert2" 500 'nonveg]))
(define menu1_vegan (list
                     [MenuItem 'appetizer "salad1" 10 'vegan]))
(define menu2_desserts (list
                     [MenuItem 'dessert "dessert2" 490 'nonveg]
                     [MenuItem 'dessert "dessert2" 510 'nonveg]
                     [MenuItem 'dessert "dessert2" 520 'nonveg]))
(define menu_list1 (list
                   menu1
                   menu2))
(define menu_list2 (list
                   menu1
                   menu2
                   menu2_desserts))
(define menu_list3 (list
                   menu1
                   ))

;; Part 1 - Evaluating Menus
;; 1-1 (num-appetizers)
;;      Takes in a Menu
;;      Counts the number of appetizers
;;      Returns an Integer of the count
;;
(: num-appetizers : Menu -> Integer)
(define (num-appetizers menu)
  (match menu
    ['() 0]
    [(cons (MenuItem kind _ _ _) sub)
     (+ (if (symbol=? kind 'appetizer) 1 0)
        (num-appetizers sub))]))

(check-expect (num-appetizers menu1) 2)

;; 1-2 (names-of-entrees)
;;      Takes in a Menu
;;      If kind is 'entree, extract the name
;;      Returns an list of names
;;
(: names-of-entrees : Menu -> (Listof String))
(define (names-of-entrees a)
  (match a
    ['() '()]
    [(cons (MenuItem kind name _ _) sub)
     (if (symbol=? kind 'entree)
         (cons name (names-of-entrees sub))
         (names-of-entrees sub))]))

(check-expect (names-of-entrees menu1) (list "entree1" "entree2"))

;; 1-3 (vegetarian-friendly?)
;;      Takes in a Menu
;;      If veg is 'veg or 'vegetarian, return True
;;
(: vegetarian-friendly? : Menu -> Boolean)
(define (vegetarian-friendly? a)
  (match a
    ['() #f]
    [(cons (MenuItem _ _ _ veg) sub)
           (cond
             [(or (symbol=? veg 'vegan) (symbol=? veg 'vegetarian)) #t]
             [else (vegetarian-friendly? sub)])]))

(check-expect (vegetarian-friendly? menu1) #t)

;; 1-4 (num-vegetarian-items)
;;      Takes in a Menu
;;      If veg, counter adds one
;;      Returns the total number
;;
(: num-vegetarian-items : Menu -> Integer)
(define (num-vegetarian-items a)
  (match a
    ['() 0]
    [(cons (MenuItem _ _ _ veg) sub)
     (cond
       [(or (symbol=? veg 'vegan) (symbol=? veg 'vegetarian))
        (+ 1 (num-vegetarian-items sub))]
       [else (+ (num-vegetarian-items sub))])]))

(check-expect (num-vegetarian-items menu1) 2)

;; 1-5 (vegan-items)
;;      Takes in a Menu
;;      If veg, add MenuItem to list
;;      Returns a Menu list
;;
(: vegan-items : Menu -> Menu)
(define (vegan-items a)
  (match a
    ['() '()]
    [(cons (MenuItem a b c veg) sub)
     (cond
       [(symbol=? veg 'vegan)
        (cons (MenuItem a b c veg) (vegan-items sub))]
       [else (vegan-items sub)])]))

(check-expect (vegan-items menu1) menu1_vegan)

;; 1-6 (vegan-ratio)
;;      Takes in a Menu
;;      Use length to compute the vegan-items
;;      Divide it by the length of original list
;;      Returns an Exact-Rational
;;
(: vegan-ratio : Menu -> Exact-Rational)
(define (vegan-ratio a)
  (/ (length (vegan-items a)) (length a)))

(check-expect (vegan-ratio menu1) 1/6)

;; 1-7 (average-dessert-price)
;;      Combines function 1 (total number) and function 2 (total price)
;;      Performs a division to compute the average price
;;      (How to put three pieces into one function?)

;; function 1 - number of desserts
;;
(: num-desserts : Menu -> Integer)
(define (num-desserts a)
  (match a
    ['() 0]
    [(cons (MenuItem kind _ _ _) sub)
     (cond
       [(symbol=? kind 'dessert)
        (+ 1 (num-desserts sub))]
       [else (+ (num-desserts sub))])]))

(check-expect (num-desserts menu1) 2)

;; function 2 - total price of dessets
;;
(: total-price-desserts : Menu -> Integer)
(define (total-price-desserts a)
  (match a
    ['() 0]
    [(cons (MenuItem kind _ price _) sub)
     (if (symbol=? kind 'dessert) (+ price (total-price-desserts sub))
         (+ (total-price-desserts sub)))]))

(check-expect (total-price-desserts menu1) 990)

;; function 3 - average-dessert-price
;;
(: average-dessert-price : Menu -> Exact-Rational)
(define (average-dessert-price a)
  (/ (total-price-desserts a) (num-desserts a)))
  
(check-expect (average-dessert-price menu1) 495)




;; 1-8 (menus-with-cheaper-desserts)
;;      Takes in a Menu
;;      If average dessert price < 500, add Menu to (Listof Menu)
;;      Returns a Menu list
;;
(: menus-with-cheaper-desserts : (Listof Menu) -> (Listof Menu))
(define (menus-with-cheaper-desserts a)
  (match a
    ['() '()]
    [(cons menu1 sub2)
     (if (< (average-dessert-price menu1) 500)
         (cons menu1 (menus-with-cheaper-desserts sub2))
         (menus-with-cheaper-desserts sub2))]))

(check-expect(menus-with-cheaper-desserts menu_list2) menu_list3)
     
       
;; 1-9 (most-expensive)
;;      Takes in a Menu
;;      Finds the highest price item
;;      Returns the menu item
;;
(: most-expensive : Menu -> MenuItem)
(define (most-expensive a)
  (match a
    ['() (error "none")]
    [(cons a '()) a]
    [(cons item1 sub)
     (local
       {(define max (most-expensive sub))}  ;; important
       (if (>= (MenuItem-price item1) (MenuItem-price max)) item1 max))]))

(check-expect (most-expensive menu1) [MenuItem 'dessert "dessert2" 500 'nonveg])
(check-expect (most-expensive menu2) [MenuItem 'dessert "dessert1" 510 'nonveg])

(test)

;; This is the end of Lab 3
;;      Thank you for grading
;;      Wish you a splendid day!
;;      -- Faradawn
;;



