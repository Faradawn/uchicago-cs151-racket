#lang typed/racket
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require typed/test-engine/racket-tests)

(: take : All(A) (Listof A) Integer -> (Listof A))
(define (take ls pos)
  (match ls
    ['() '()]
    [(cons head tail) (if (> pos 1)
                          (cons head (take tail (- pos 1)))
                          (list head))]))
(check-expect (take '(1 2 3) 2) '(1 2))
;; (drop) takes in a list and keep the sub-list after index
(: drop : All(A) (Listof A) Integer -> (Listof A))
(define (drop ls pos)
  (match ls
    ['() '()]
    [(cons head tail) (if (> pos 1)
                          (drop tail (- pos 1))
                          tail)]))
(check-expect (drop '(1 2 3) 1) '(2 3))

(: get : All(A) (Listof A) Integer -> A)
(define (get ls pos)
  (match ls
    ['() (error "out of bound")]
    [(cons head tail) (if (<= pos 0)
                          head
                          (list-ref tail (- pos 1)))]))
(check-expect (get '(1 2 3) 1) 2)

(define str "2:23pm")
(define ls0 (string->list str))
(define ls1 (if (or (char=? (get ls0 1) #\:) (char=? (get ls0 2) #\:))
              (filter char-numeric? (take ls0 2)) '()))
(define ls2 (filter char-numeric? (take (drop ls0 (+ (length ls1) 1)) 2)))

(: list->int : (Listof Char) -> Integer)
(define (list->int ls)
  (cast (string->number (list->string ls)) Integer))
(check-expect (list->int '(#\1 #\2)) 12) 


(test)