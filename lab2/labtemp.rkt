#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

(: a (-> Integer Integer))
(define (a n)
  (cond
    [(= n 0) 10]
    [(