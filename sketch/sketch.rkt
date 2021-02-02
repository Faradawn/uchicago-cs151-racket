#lang typed/racket

(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

(define-type Union
  (U 'ev Integer))

(define-struct Car
  ([quality : Symbol]
   [union : Union]
   [price : Integer]))

(: isEV (-> Car String))
(define (isEV n)
  (match n
    [(Car quality union price)
     (cond
       [(symbol=? union 'ev) "yes"]
       [else "no"])]))


(define a (Car 'good 'ev 100))

(isEV a)