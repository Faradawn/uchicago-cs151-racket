#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

;; lab 6 : Collaborator： Yolanda Li
;; Mar. 6, 2021

(define-type Group (U 'A 'B' C))

(define-struct Passenger
  ([name : String]
   [group : Group]
   [position : Integer]
   [family? : Boolean]
   [upgraded? : Boolean]))


(: passenger<? : Passenger Passenger -> Boolean)
(define (passenger<? a b)
  (match* (a b)
    [((Passenger name gp pos fam up) (Passenger name2 gp2 pos2 fam2 up2))
     ;-- a upgrade
     (if up (if (not up2) #t
                 (if (symbol<? gp gp2) #t
                     (if (symbol<? gp2 gp) #f
                         (if (< pos pos2) #t #f))))
         ;-- a not updrage 
         (if up2 #f
             ;-- a b not upggrade
             (if (symbol<? gp gp2)
                 ;-- gp1 < pg2
                 (if (symbol=? gp 'A) #t
                     (if (not fam2) #t
                         (if fam
                             (if (string<? name name2) #t #f)
                             #f)))
                                 
                 (if (symbol=? gp gp2)
                     ;-- gp1 = gp2
                     (if (symbol=? gp 'A)
                         (if (< pos pos2) #t #f)
                         (if fam
                             (if (not fam2) #t
                                 (if (string<? name name2) #t #f))
                             (if (not fam2)
                                 (if (< pos pos2) #t #f)
                                 #f)))
                     ;; gp1 > gp2            
                     (if (symbol=? gp2 'A) #f
                         (if fam
                             (if (not fam2) #t
                                 (if (string<? name name2) #t #f))
                             (if fam2 #f
                                 #f)))))))]))
                                 

;; my tests
(define p11 (Passenger "11" 'A 2 #t #t))
(define p12 (Passenger "12" 'B 0 #f #t))
(define p13 (Passenger "13" 'A 1 #t #f))
(define p14 (Passenger "14" 'B 9 #t #f))
(define p15 (Passenger "15" 'B 10 #t #f))
(define p16 (Passenger "16" 'B 5 #f #f))
(define p17 (Passenger "17" 'B 10 #f #f))
(define p18 (Passenger "18" 'B 15 #f #f))

(check-expect (passenger<?  p11 p12) #t)
(check-expect (passenger<?  p12 p13) #t)
(check-expect (passenger<?  p13 p14) #t)
(check-expect (passenger<?  p14 p15) #t)
(check-expect (passenger<?  p15 p16) #t)
(check-expect (passenger<?  p16 p17) #t)
(check-expect (passenger<?  p17 p18) #t)

;; offical tests
(define p1 (Passenger "P1" 'C 60 #f #f))
(define p2 (Passenger "P2" 'C 20 #f #f))
(define p3 (Passenger "P3" 'B 30 #f #f))
(define p4 (Passenger "P4" 'A 40 #f #f))
(define p5 (Passenger "P5" 'C 40 #t #f))
(define p6 (Passenger "P6" 'C 30 #t #f))
(define p7 (Passenger "P7" 'B 50 #t #f))
(define p8 (Passenger "P8" 'C 55 #t #t))
(define p9 (Passenger "P9" 'B 45 #f #t))
(define p10 (Passenger "P10" 'C -1000 #f #t))
(check-expect (passenger<? p9 p10) #t)
(check-expect (passenger<? p8 p4) #t)
(check-expect (passenger<? p5 p6) #t)
(check-expect (passenger<? p7 p3) #t)
(check-expect (passenger<? p2 p1) #t)
(check-expect (passenger<? p8 p10) #f)
(check-expect (passenger<? p5 p4) #f)
(check-expect (passenger<? p7 p6) #f) 
(check-expect (passenger<? p2 p3) #f)
(check-expect (passenger<? p9 p4) #t)
(check-expect (passenger<? p5 p7) #t) 
(check-expect (passenger<? p1 p9) #f)

;------- Algo 1 - Insertion Sort - Expoential -------

;(: insert : Passenger (Listof Passenger) -> (Listof Passenger))
;(define (insert n ls) ; ascending list
;  (match ls
;    ['() (list n)]
;    [(cons head tail) (if (passenger<? n head)
;                          (cons n ls)
;                          (cons head (insert n tail)))]))
;(: sort-passengers : (Listof Passenger) -> (Listof Passenger))
;(define (sort-passengers ls)
;  (match ls
;    ['() '()]
;    [(cons head tail) (insert head (sort-passengers tail))]))
     


;------ Algo 2: Quick Sort - Linear ------



(: sort-passengers : (Listof Passenger) -> (Listof Passenger))
(define (sort-passengers ls)
  (match ls
    ['() '()]
    [(cons pivot tail)
     (append
      (sort-passengers (filter (λ ([ x : Passenger])
                                 (passenger<? x pivot)) tail))
      (list pivot)
      (sort-passengers (filter (λ ([ x : Passenger])
                                 (not (passenger<? x pivot))) tail)))]))

;(check-expect (sort-passengers (list p12 p11 p13 p14))
;              (list p11 p12 p13 p14))


;------ Algo 3: Merge Sort - Exponential ------



      
;; Part 2 - Analyzing Three Sorting Speed
;;
(: random-passenger : -> Passenger)
;; Generates a random passenger
(define (random-passenger)
  (Passenger (number->string (random 0 2000000000))
             (match (random 0 3) [0 'A] [1 'B] [2 'C])
             (random -2000000000 2000000000)
             (= (random 0 4) 0)
             (= (random 0 4) 0)))

(: random-passenger-list : Integer -> (Listof Passenger))
;; Generates a list of n random passengers
(define (random-passenger-list n)
  (build-list n (lambda ([k : Integer]) (random-passenger))))

(: eat : All (A) A -> 'yum)
;; Takes in any value and returns 'yum.
(define (eat a) 'yum)



;; generate passenger lists

(define pl1 (random-passenger-list 1000))
(define pl2 (random-passenger-list 2000))
(define pl3 (random-passenger-list 4000))
(define pl4 (random-passenger-list 8000))
(define pl5 (random-passenger-list 16000))
(define pl6 (random-passenger-list 25000))

(begin (display "10k: ") (time (eat (sort-passengers pl1))))
(begin (display "20k: ") (time (eat (sort-passengers pl2))))
(begin (display "40k: ") (time (eat (sort-passengers pl3))))
(begin (display "80k: ") (time (eat (sort-passengers pl4))))
(begin (display "160k: ") (time (eat (sort-passengers pl5))))
(begin (display "250k: ") (time (eat (sort-passengers pl6))))


(test)


