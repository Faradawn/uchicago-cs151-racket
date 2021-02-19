#lang typed/racket

;; Homework 3 -- Image Universe
;; Faradawn Yang
;; Feb. 18, 2021
;;

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
  
(require typed/test-engine/racket-tests)

;; === data definitions

(define-type (Optional A)
  (U 'None (Some A)))

(define-struct (Some A)
  ([value : A]))

(define-struct Click
  ([x : Integer]
   [y : Integer]))

(define-struct CircleWorld
  ([background-color : Image-Color]
   [initial-color    : Image-Color]
   [change-to-color  : Image-Color]
   [num-circles : Integer]
   [radius  : Integer]
   [padding : Integer]
   [clicked-on? : (Listof Boolean)]))

;; == sample world, useful for testing

(define world0
  (CircleWorld 'lightblue
               'gray
               'dodgerblue
               12
               20
               8
               (make-list 12 #f)))
(define world1
  (CircleWorld 'gray
               'lightblue
               'dodgerblue
               12
               20
               8
               (make-list 12 #f)))
(define world2
  (CircleWorld 'gray
               'lightblue
               'dodgerblue
               12
               20
               8
               (list #f #t #f #f #f #f #f #f #f #f #f #f)))
;; === operations and calculations

(: replace-at : All (A) Integer A (Listof A) -> (Listof A))
;; replace the item at the given position
;; position counting starts at 0
;; ex: (replace-at 0 'Z '(a b c)) -> '(Z b c)
;; ex: (replace-at 1 'Z '(a b c)) -> '(a Z c)
(define (replace-at i x xs)
  (match xs
    ['() '()]
    [(cons head tail)
     (if (= i 0) (cons x (replace-at (- i 1) x tail))
         (cons head (replace-at (- i 1) x tail)))]))

(check-expect (replace-at 1 'Z '(a b c))'(a Z c))
(check-expect (replace-at 2 0 '(1 1 1 1 1)) '(1 1 0 1 1))
(check-expect (replace-at 6 #t (make-list 12 #f))
                          '(#f #f #f #f #f #f #t #f #f #f #f #f))
        

(: clicked-within : Click CircleWorld -> (Optional Integer))
;; assume each circle has an index from 0 to (n-1), counting from the left
;; if the click is within circle i, this function returns (Some i)
;; otherwise 'None
 (define (clicked-within click world)
   (match click
     [(Click x y)
      (match world
        [(CircleWorld _ _ _ num r pad _)
         (local
           {(: n Integer)
            (define n (exact-ceiling (/ x (+ (* 2 r) pad))))
            (: sqr : Integer -> Integer)
            (define (sqr x) (* x x))}     
           (if (<= (+ (sqr (- x (* (- (* n 2) 1) r) (* n pad)))
                      (sqr (- y r pad))) (sqr r))
               (Some (- n 1)) 'None))])]))

(check-expect (clicked-within (Click 7 28) world0) 'None)
(check-expect (clicked-within (Click 8 28) world0) (Some 0))
(check-expect (clicked-within (Click 92 40) world0) (Some 1))
(check-expect (clicked-within (Click 93 40) world0) 'None)
(check-expect (clicked-within (Click 96 28) world0) (Some 1))
(check-expect (clicked-within (Click 97 28) world0) 'None)
(check-expect (clicked-within (Click 76 48) world0) (Some 1))
(check-expect (clicked-within (Click 76 49) world0) 'None)
(check-expect (clicked-within (Click 104 27) world0) 'None)
(check-expect (clicked-within (Click 104 28) world0) (Some 2))
(check-expect (clicked-within (Click 576 28) world0) (Some 11))
(check-expect (clicked-within (Click 577 28) world0) 'None)
  

;; === universe support

(: draw : CircleWorld -> Image)
;; draw the CircleWorld, taking care that padding is correct
(define (draw world)
  (match world
    [(CircleWorld bg c1 c2 num r pad click-ls)
     (local
       {(: stack : Boolean Image -> Image)
        (define (stack true img) 
          (beside
           img
           (if true (circle r 'solid c2) (circle r 'solid c1))
           (square pad 'solid bg)))}
       (overlay/align
        "right" "center"
        (foldl stack empty-image click-ls)
        (rectangle (+ (* 2 r num) (* pad (+ num 1))) (+ (* 2 r) (* 2 pad))
                   'solid bg)))]))
;; Eye-ball tests
; (draw world0)
; (draw world1)
(check-expect (image-width (draw world0)) 584)
(check-expect (image-height (draw world0)) 56)
(draw (CircleWorld 'gray 'lightblue 'dodgerblue 12 20 8
                   '(#t #f #t #f #f #f #f #f #f #t #f #f)))


       

(: react-to-mouse : CircleWorld Integer Integer Mouse-Event -> CircleWorld)
;; if the user clicks on a circle, change the color to the "change-to-color"
;; if the user clicks outside any circle, reset all circles to the initial color
(define (react-to-mouse world x y e)
  (match world
    [(CircleWorld bg c1 c2 num r pad ls)
     (match e
       ["button-down" 
        (match (clicked-within (Click x y) world)
          ['None 
           (CircleWorld bg c1 c2 num r pad (make-list num #f))]
          [(Some n) 
           (CircleWorld bg c1 c2 num r pad (replace-at n #t ls))])]
       [_ world])]))

;; world1 has 12 #f
;; world2 has '(#f #t #f #f #f #f #f #f #f #f #f #f)
(check-expect (react-to-mouse world1 92 40 "button-down") world2)
(check-expect (react-to-mouse world1 0 0 "button-down") world1) 


            

(: run : Image-Color Image-Color Image-Color Integer Integer Integer 
         -> CircleWorld)
;; run the world given setup details
(define (run bg init ch n r p)
  (big-bang (CircleWorld bg init ch n r p (make-list n #f)) : CircleWorld
    [to-draw draw]
    [on-mouse react-to-mouse]))

; eyeball tests
;(run 'gray 'lightblue 'dodgerblue 12 20 8)
   

(test)
