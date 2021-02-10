#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require typed/test-engine/racket-tests)


;(above
; (triangle 80 'solid 'red)
; (beside
;  (square 30 'solid 'blue)
;  (square 15 'solid 'green)
;  (square 30 'solid 'blue)))

;(overlay
; (circle 20 'outline 'red)
; (square 40 'solid 'white)
; (circle 40 'solid 'blue))

;(above
; (overlay
;  (circle 20 'solid 'green)
;  (rectangle 100 50 'solid 'black))
; (rectangle 100 50 'solid 'cyan))

;; The function forms a strings that writes out the number of apples and bananas.
;(: fruit (-> Integer Integer String))
;(define (fruit num-a num-b)
;  (cond
;    [(and (> num-a 0) (> num-b 0))
;     (cond
;       [(and (= num-a 1) (= num-b 1))
;        (string-append (number->string num-a) " apple and " (number->string num-b) " banana")]
;       [(and (> num-a 1) (= num-b 1))
;        (string-append (number->string num-a) " apples and " (number->string num-b) " banana")]
;       [(and (= num-a 1) (> num-b 1))
;        (string-append (number->string num-a) " apple and " (number->string num-b) " bananas")]
;       [(and (= num-a 1) (> num-b 1))
;        (string-append (number->string num-a) " apple and " (number->string num-b) " bananas")]
;       [else
;        (string-append (number->string num-a) " apples and " (number->string num-b) " bananas")])]
;     [(and (= num-a 0) (> num-b 0))
;      (if (= num-b 1)
;          (string-append (number->string num-b) " banana")  
;          (string-append (number->string num-b) " bananas"))]
;     [(and (> num-a 0) (= num-b 0))
;      (if (= num-a 1)
;          (string-append (number->string num-a) " apple")  
;          (string-append (number->string num-a) " apples"))]
;     [else
;      "no fruit"]))


;; ==== Q6
;
;(define-type Nat
;  (U 'Z S))
;(define-struct S
;  ([n : Nat]))
;
;(define-type (UnaryTree A)
;  (U 'Nil (Cons A)))
;
;(define-struct (Cons A)
;  ([first : A]
;   [rest  : (UnaryTree A)]))
;   
;(: nth-item : All (A) (UnaryTree A) Nat -> A)
;(define (nth-item ls index)
;  (match ls
;    ['() (error "ERROR")]
;    [(Cons first rest)
;     (local
;       {(: counter Integer)
;        (define counter (+ 1 (nth-item rest)))
;        (: int (-> Nat Integer))
;        (define (int n)
;          (match n
;            ['Z 0]
;            [(S 'Z) (+ 1 (int S))]))}
;        (if (= counter int) (Cons first (nth-item rest)) (nth-item rest)))]))
        

;(: create : Integer -> Image)
;(define (create n)
;  (beside (if (even? n)
;              (square n 'solid 'blue)
;              (square n 'solid 'green))
;          (if (> n 1) (create (- n 1)) empty-image)))
;
;(create 50)

(define-struct Point
  ([c : Real]))

;; (LongInterval a b) represents the closed interval [a, b]
;; consisting of all numbers between a and b inclusive.
;; It is required that a < b (note that the inequality is strict).
(define-struct LongInterval
  ([a : Real]
   [b : Real]))

;; An Interval is either a Point, a LongInterval, 
;; or the empty set, denoted by 'Empty.
(define-type Interval (U LongInterval Point 'Empty))
(: smart-interval : Real Real -> Interval)
(define (smart-interval x1 x2)
  (cond
    [(= x1 x2) (if (< x1 x2) (Point x1) (Point x2))]
    [else (LongInterval x1 x2)]))

(: interval-length : Interval -> Real)
(define (interval-length n)
  (match n
    ['Empty 0]
    [(Point a) 0]
    [(LongInterval x1 x2) (- x2 x1)]))
             


(: intersect : Interval Interval -> Interval)
(define (intersect a b)
  (match* (a b)
    [('Empty 'Empty) 'Empty]
    [((Point a) (LongInterval b1 b2)) (if (or (= a b1) (= a b2)) (Point a) 'Empty)]
    [((LongInterval a1 a2) (Point b)) (if (or (= b a1) (= b a2)) (Point b) 'Empty)]
    [((LongInterval a1 a2) (LongInterval b1 b2))
     (cond
       [(and (< a2 b1) ((LongInterval (

     
     