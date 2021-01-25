#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

;; Hi. Faradawn's Homework 1.
;; There are 7 parts and 14 tests in total.
;; Thanks in advance!
;;

;; [Preparation]
;; Define the TaxReturn Struct
;;
(define-struct TaxReturn
  ([income : Integer]
   [charity : Integer]
   [num-adults : Integer]
   [num-children : Integer]
   [purchased-EV? : Boolean]
   [covid-costs : Integer]
   [already-paid : Integer]))

;; [Preparation]
;; Define 3 intances of TaxReturn for testing
;; 'a' should go through all checkings and have no refund.
;; 'b' (no ev) should skip 'ev-credit' and have a refund.
;; 'c' (0 adults) should return false at Plausible checking
;;     and should have no refund. 
;;
(: a TaxReturn)
(: b TaxReturn)
(: c TaxReturn)
(define a (TaxReturn 90000 1000 1 2 #t 1000 0))
(define b (TaxReturn 10000 1000 1 2 #f 1000 30000))
(define c (TaxReturn 10000 5000 0 0 #f 6000 0))


;; Part 1: Plausible?
;; Check wether all 6 values are nonnegative (and num-adults strictly positive).
;; Check-expect 'c' should be #f because of the negative 'already-paid' amount.
;;
(: plausible? : TaxReturn -> Boolean)
(define (plausible? n) (cond
                         [(and
                          (>= (TaxReturn-income n) 0)
                          (>= (TaxReturn-charity n) 0)
                          (> (TaxReturn-num-adults n) 0)
                          (>= (TaxReturn-num-children n) 0)
                          (>= (TaxReturn-covid-costs n) 0)
                          (>= (TaxReturn-already-paid n) 0)
                          #t)]
                         [else #f]))

(check-expect (plausible? c) #f)


;; Part 2: Adjusted-income takes in parameter 'n' and returns an Integer.
;; 1. Let 'm' be the calculated adjusted income.
;; 2. If adjusted income m >= 0, return m. Else return 0.
;; 3. Apply exact-ceiling to round the result.
;; Check-expect a: 90000 - 1000 - 1000 = 88000.
;; Check-expect b: 10000 - 6000 - 5000 = -1000 -> 0
;; 
(: adjusted-income : TaxReturn -> Integer)
(define (adjusted-income n)
  (let
      ([m (- (TaxReturn-income n)
             (+ (TaxReturn-charity n) (TaxReturn-covid-costs n)))])
    (if (>= m 0) m 0)))

(check-expect (adjusted-income a) 88000)
(check-expect (adjusted-income b) 8000)


;; Part 3: Income-tax takes in parameter 'n' and returns an Integer.
;; 1. Let 'm' be the adjusted income. 
;; 2. Divide the condition into 5 cases.
;; 3. Adjusted income 'm' will enter one of cases
;;    and go through that calculation.
;; 4. Apply exact-ceiling to the final output.
;; Check-expect a: 0.5*(88000-80000) + 0.4*40000 + 0.3*20000 + 0.2*10000 = 28000
;; Check-expect b: 0, becasue income less than 10000
;;
(: income-tax : TaxReturn -> Integer)
(define (income-tax n)
  (let ([m (adjusted-income n)])
    (exact-ceiling
     (cond
       [(and (> m 10000) (<= m 20000))
        (* 0.2 (- m 10000))]
       [(and (> m 20000) (<= m 40000))
        (+ (* 0.2 10000) (* 0.3 (- m 20000)))] 
       [(and (> m 40000) (<= m 80000))
        (+ (+ (* 0.2 10000) (* 0.3 20000)) (* 0.4 (- m 40000)))]
       [(> m 80000)
        (+ (+ ( + (* 0.2 10000) (* 0.3 20000)) (* 0.4 40000))
           (* 0.5 (- m 80000)))]
       [else 0]))))

(check-expect (income-tax a) 28000)
(check-expect (income-tax b) 0)


;; Part 4: Child-credit takes in parameter 'n' and returns an Integer.
;; 1. Child-credit is the sum of 1200 for each child.
;; Check-expect a (two children): 2 * 1200 = 2400
;;
(: child-credit : TaxReturn -> Integer)
(define (child-credit n)
  (* 1200 (TaxReturn-num-children n)))

(check-expect (child-credit a) 2400)

;; Part 5: EV-credit takes in parameter 'n' and returns an Integer.
;; If purchased an EV, get 500 per person. Else, gets 0 credit.
;; Check-expect a (2 adults and 1 child): 3 * 500 = 1500
;; Check-expect a  (no EV): 0
;; 
(: ev-credit : TaxReturn -> Integer)
(define (ev-credit n)
  (if (TaxReturn-purchased-EV? n)
      (* 500 (+ (TaxReturn-num-adults n) (TaxReturn-num-children n)))
      0))

(check-expect (ev-credit a) 1500)
(check-expect (ev-credit b) 0)


;; Part 6: Balance takes in parameter 'n' and returns an Integer.
;; Balance = Income Tax - Child Credit - EV Credit - Already Paid.
;; Check-expect a (none already-paid):
;;                24100 tax after credit - 0 paid = 24100
;; Check-expect b (negative tax and huge already-pard):
;;                 -2400 tax after credit - 30000 paid = - 32400?
;; Check-expect b (huge already-pard):
;;                0 tax after credit - 0 paid = 0
;;
(: balance : TaxReturn -> Integer)
(define (balance n)
  (- (income-tax n) (+ (+ (child-credit n) (ev-credit n))
                       (TaxReturn-already-paid n))))

(check-expect (balance a) 24100)
(check-expect (balance b) -32400)
(check-expect (balance c) 0)

;; Part 7: Refund takes in parameter 'n' and returns a Boolean.
;; If Balance < 0, shound return refund (#t). Else, no refund.
;; Check-expect a: no refund.
;; Check-expect b: yes refund.
;; Check-expect c: no refund.
;;
(: refund? : TaxReturn -> Boolean)
(define (refund? n)
  (if (< (balance n) 0) #t #f))

(check-expect (refund? a) #f)
(check-expect (refund? b) #t)
(check-expect (refund? c) #f)


(test)

;; This is the end of Homework 1.
;; Thank you for grading!
;; Wish you a pleasant day!
;;                               -- Faradawn