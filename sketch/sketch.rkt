#lang typed/racket
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require typed/test-engine/racket-tests)

(define-struct Time
  ([hour : Integer] ;; from 0 to 23
   [minute : Integer]
   [second : Integer]))

(: time<? : Time Time -> Boolean)
(define (time<? t1 t2)
  (match* (t1 t2)
    [((Time h1 m1 s1)(Time h2 m2 s2))
     (if (< h1 h2) #t
         (if (= h1 h2) (if (< m1 m2) #t
                           (if (= m1 m2)
                               (if (< s1 s2) #t #f)
                               #f))
                           #f))]))

(check-expect (time<? (Time 10 23 00) (Time 10 23 01)) #t)
(check-expect (time<? (Time 10 23 00) (Time 10 24 00)) #t)
(check-expect (time<? (Time 12 23 00) (Time 13 24 01)) #t)
(check-expect (time<? (Time 12 23 00) (Time 12 23 00)) #f)
(check-expect (time<? (Time 1 23 00) (Time 20 24 00)) #t)
(check-expect (time<? (Time 12 23 00) (Time 20 24 01)) #t)

(test)