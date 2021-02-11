#lang typed/racket
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require typed/test-engine/racket-tests)

;; list your collaborators in a comment
;; Collaborators: Haichuan Wang and Henry Herzog

;; Create test instances of menu
(define-struct MenuItem
  ([kind  : (U 'appetizer 'entree 'dessert)]
   [name  : String]
   [price : Integer] ;; <-- integer num of pennies*
   [veg   : (U 'nonveg 'vegetarian 'vegan)]))
             ;; <--- vegan is a stricter form of vetegarian

(define-type Menu
  (Listof MenuItem))

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
(define menu_nonvegan (list                       
                       [MenuItem 'entree "entree1" 10 'nonveg]
                       [MenuItem 'entree "entree2" 10 'nonveg]
                       [MenuItem 'dessert "dessert1" 510 'nonveg]
                       [MenuItem 'dessert "dessert2" 500 'nonveg]))
(define menu1_vegan (list
                     [MenuItem 'appetizer "salad1" 10 'vegan]))
(define menu2_desserts (list
                     [MenuItem 'dessert "dessert2" 490 'nonveg]
                     [MenuItem 'dessert "dessert2" 510 'nonveg]
                     [MenuItem 'dessert "dessert2" 530 'nonveg]))
(define menu_list1 (list
                   menu1
                   menu2))
(define menu_list2 (list
                   menu1
                   menu2
                   menu2_desserts))
(define menu_list3 (list
                    menu2
                    menu2_desserts
                   ))

;; Part 1 - Upgrading the Guide

;; 1 - 1
;; Takes in a Menu
;; Returns a list for only the items of vegan
(: vegan-items : Menu -> Menu)
(define (vegan-items ls)
  (local
    {(: is-vegan (-> MenuItem Boolean))
     (define (is-vegan item) (symbol=? (MenuItem-veg item) 'vegan))}
    (filter is-vegan ls)))

(check-expect (vegan-items menu1) menu1_vegan)

;; 1 - 2
;; Takes in a Menu
;; Returns a list of price
(: prices : Menu -> (Listof Integer))
(define (prices ls)
  (local
    {(: create-list (-> MenuItem Integer))
     (define (create-list item)
       (MenuItem-price item))}
    (map create-list ls)))

(check-expect (prices menu1) (list 10 10 10 10 490 500))
        
                        
; 1 - 3 (needs a little fix)
; Takes in a menu
; Returns the most expensive item on the Menu
(: most-expensive : Menu -> MenuItem)
(define (most-expensive ls)
  (local
    {(: compare (-> MenuItem MenuItem))
     (define (compare item)
       (if (> (MenuItem-price item)(MenuItem-price max))
           item max))
     (: max MenuItem)
     (define max (map compare ls))}
    max))


;; 1 - 4
;; Takes in a menu
;; Checks whether it contains a 'vegan or 'vegetarian item
(: vegetarian-friendly? : Menu -> Boolean)
(define (vegetarian-friendly? ls)
  (local
    {(: is-veg (-> MenuItem Boolean))
     (define (is-veg item)
       (or (symbol=? (MenuItem-veg item) 'vegetarian))
           (symbol=? (MenuItem-veg item) 'vegan))}
    (ormap is-veg ls)))

(check-expect (vegetarian-friendly? menu1) #t)
(check-expect (vegetarian-friendly? menu_nonvegan) #f)
        


;; 1 - 5 (failed method)
;; Takes in a menu 
;; Returns a list of entree names 
;(: names-of-entrees : Menu -> (Listof String))
;(define (names-of-entrees ls)
;  (local
;    {(: is-entree (-> MenuItem Boolean))
;     (define (is-entree item)
;       (symbol=? (MenuItem-kind item) 'entree))
;     (: extract-name (-> MenuItem String))
;     (define (extract-name item)
;       (MenuItem-name item))
;     }
;    (map (extract-name (filter is-entree ls)))))


;; 1 - 5 (working method)
;; Helper Function 1
;; Returns a list with only items that are of entree kind
(: filter-entree (-> Menu Menu))
(define (filter-entree ls)
  (local
    {(: is-entree (-> MenuItem Boolean))
     (define (is-entree item)
       (symbol=? (MenuItem-kind item) 'entree))}
    (filter is-entree ls)))

(check-expect (filter-entree menu1) (list
                                     [MenuItem 'entree "entree1" 10 'nonveg]
                                     [MenuItem 'entree "entree2" 10 'nonveg]))
;; Main function 1 - 5 
;; Takes in a menu
;; Returns a list of entree names
(: names-of-entrees (-> Menu (Listof String)))
(define (names-of-entrees ls)
  (local
    {(: extract-name (-> MenuItem String))
     (define (extract-name item)
       (MenuItem-name item))}
    (map extract-name (filter-entree ls))))

(check-expect (names-of-entrees menu1) (list "entree1" "entree2"))


;; Part 2 -- Count

;; 2 - 1
;; Takes in a condition and a list
;; Returns the count of the items that statisfy the condition
(: count-if : All (A) (A -> Boolean) (Listof A) -> Integer)
(define (count-if fx ls)
  (length (filter fx ls)))

(check-expect
 (local
   {(: is-one (-> Integer Boolean))
    (define (is-one n)
      (= n 1))}
   (count-if is-one (list 1 0 1 0))) 2)
                 

;; 2 - 2
;; Counts the number of appetizers
(: num-appetizers : Menu -> Integer)
(define (num-appetizers ls)
  (local
    {(: is-appetizer (-> MenuItem Boolean))
     (define (is-appetizer item)
       (symbol=? (MenuItem-kind item) 'appetizer))}
    (count-if is-appetizer ls)))

(check-expect (num-appetizers menu1) 2)
(check-expect (num-appetizers menu2_desserts) 0)

;; 2 - 3
;; First counts the vegan items
;; Then computes the fraction of vegan items
(: vegan-ratio : Menu -> Exact-Rational)
(define (vegan-ratio ls)
  (local
    {(: is-vegan (-> MenuItem Boolean))
     (define (is-vegan item) (symbol=? (MenuItem-veg item) 'vegan))}
    (/ (count-if is-vegan ls) (length ls))))

(check-expect (vegan-ratio menu1) 1/6)
(check-expect (vegan-ratio menu2_desserts) 0)

;; 2 - 4

;; Helper Function 1
;; Counts the number of desserts in a menu
(: num-desserts (-> Menu Integer))
(define (num-desserts ls)
  (local
    {(: is-dessert (-> MenuItem Boolean))
     (define (is-dessert item) (symbol=? (MenuItem-kind item) 'dessert))}
    (count-if is-dessert ls)))

(check-expect (num-desserts menu1) 2)

;; Helper Function 2
;; Sums the total price of desserts
(: total-price-desserts : Menu -> Integer)
(define (total-price-desserts ls)
  (local
    {(: add-price (-> MenuItem Integer Integer))
     (define (add-price item z)
       (if (symbol=? (MenuItem-kind item) 'dessert)
           (+ (MenuItem-price item) z) z))}
    (foldr add-price 0 ls)))
       
(check-expect (total-price-desserts menu1) 990)

;; Main Function 2 - 4
;; Computes total price divided by number of desserts
(: average-dessert-price : Menu -> Exact-Rational)
(define (average-dessert-price ls)
  (/ (total-price-desserts ls) (num-desserts ls)))

(check-expect (average-dessert-price menu1) 495)


;; 2 - 5
;; Takes in a list of Menu
;; Counts how many have lower-than-threshold dessert price
(: num-menus-with-cheaper-desserts : (Listof Menu) Integer -> Integer)
(define (num-menus-with-cheaper-desserts ls bar)
  (local
    {(: below-bar (-> Menu Boolean))
     (define (below-bar menu)
       (< (average-dessert-price menu) bar))}
    (count-if below-bar ls)))

;; menu_list2 has $495, $505, and $510
(check-expect (num-menus-with-cheaper-desserts menu_list2 500) 1)
;; menu_list2 has $505 and $510
(check-expect (num-menus-with-cheaper-desserts menu_list3 500) 0)


(test)
