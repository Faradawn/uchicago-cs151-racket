#lang typed/racket

(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

;; computer distance
;;

(define-struct P
  ([x : Real]
   [y : Real]))

(: midpoint (-> P P Real))
(define (midpoint m n)
  (match* (m n) [((P mx my) (P nx ny))
    (* 2 (+ mx nx))]))
    




(check-expect (midpoint (P 1 1) (P 1 0)) 4)
(test)
    