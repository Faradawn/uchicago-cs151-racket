
;; Faradawn Yang
;; January 28, 2021
;; CS 15100 Lab 2 - Doomsday Calculator
;; Collaborators: Henry Herzog and Sabine Salnave

#lang typed/racket

(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

(define-struct Date
  ([m : Integer]
   [d : Integer]
   [y : Integer]))

;; note: for the month, 1 means January, 2 means February, etc.

(define-struct Day-Of-Week
  ([num : Integer]))

;; note: 0 means Sunday, 1 means Monday, ..., 6 means Saturday

;; Part 1: Leap Year
;; Checks whether or not a year is divisible by 4 and not 100,
;; or divisible by 400.
;; 
(: leap? (-> Integer Boolean))
(define (leap? year)
  (cond
    [(= (remainder year 400) 0) #t]
    [(= (remainder year 100) 0) #f]
    [(= (remainder year 4) 0) #t]
    [else #f]))

(check-expect (leap? 2021) #f)
(check-expect (leap? 1000) #f)
(check-expect (leap? 0) #t)

;; Part 2: Days in Month
;; Takes in momth and year.
;; Returns the days in month, depending on leap year.
;; (assuming month is from 1 to 12)
;; 
(: days-in-month (-> Integer Integer Integer))
(define (days-in-month m y)
  (match m
    [1 31]
    [2 (if (leap? y) 29 28)]
    [3 31]
    [5 31]
    [7 31]
    [8 31]
    [10 31]
    [12 31]
    [_ 30]
    ))

(check-expect (days-in-month 2 2021) 28)
(check-expect (days-in-month 2 2020) 29)
(check-expect (days-in-month 3 2021) 31)
(check-expect (days-in-month 4 2021) 30)

;; Part 3: Smart Constructor
;; Takes in month, day, and year.
;; Filters out dates before 1900 or 'bad' dates.
;; Returns a Data instance.
;;
(: smart-date (-> Integer Integer Integer Date))
(define (smart-date m d y)
  (cond
    [(or (< m 1) (> m 12)) (error "month out of range")]
    [(< y 1900) (error "year out of range")]
    [(or (< d 0) (> d (days-in-month m y))) (error "day out of range")]
    [else (Date m d y)]))

(check-error (smart-date 100 17 2021) "month out of range")
(check-error (smart-date 1 17 1899) "year out of range")
(check-error (smart-date 1 100 2021) "day out of range")
(check-expect (smart-date 1 17 2021) (Date 1 17 2021))

;; Part 4: Comparing Dates
;; 'date=?' takes in two Dates and compares their three values.
;;
(: date=? (-> Date Date Boolean))
(define (date=? date1 date2)
  (match* (date1 date2)
    [((Date d1 m1 y1) (Date d2 m2 y2))
     (if (and (and (= d1 d2) (= m1 m2)) (= y1 y2)) #t #f)]))

(check-expect (date=? (Date 1 27 2021) (Date 1 27 2021)) #t)
(check-expect (date=? (Date 1 27 2021) (Date 1 28 2021)) #f)

;; 'date>?' takes in two Dates and decides whether the first comes first.
;; Compares years first. If equal, compares months. If equal, compares day.
;;
(: date<? (-> Date Date Boolean))
(define (date<? date1 date2)
  (match* (date1 date2)
    [((Date m1 d1 y1) (Date m2 d2 y2))
     (cond
       [(> y1 y2) #f]
       [(< y1 y2) #t]
       [else (cond
               [(> m1 m2) #f]
               [(< m1 m2) #t]
               [else (cond
                       [(< d1 d2) #t]
                       [else #f])])])]))
                             
(check-expect (date<? (Date 1 26 2021) (Date 1 27 2021)) #t)
(check-expect (date<? (Date 2 28 2020) (Date 1 27 2021)) #t)
(check-expect (date<? (Date 7 16 2019) (Date 5 13 2020)) #t)
(check-expect (date<? (Date 1 26 2022) (Date 1 27 2021)) #f)

;; Part 5 - 1: Days After
;; Takes in Day-of-Week and number of days to add.
;; Returns a Day-of-Week.
;; The number of days to add can be negative, i.e. the days before.
;; For exmaple, how to get 4 days before Tuesday is Firday? 
;;              Use Tuesday plus 35 days and divide by 7.
;;              The remainder yields 5, which is Friday.
;;              (number 35 (5x7), is guarentee if 5 weeks in a month.)
;;
(: days-after (-> Day-Of-Week Integer Day-Of-Week))
(define (days-after day n)
  (match day
    [(Day-Of-Week d)
     (local
       {(define d1 (remainder (+ (+ d n) 35) 7))}
       (Day-Of-Week d1))]))

(define Tues (Day-Of-Week 2))
(define Fri (Day-Of-Week 5))
(check-expect (days-after Tues -4) Fri)



;; Part 5 - 2: Doomsday in Month
;; Takes in month and year.
;; Returns Doomsday in month.
;;
(: doomsday-in-month (-> Integer Integer Integer))
(define (doomsday-in-month m y)
  (match m
    [1 (if (leap? y) 32 31)]
    [2 (if (leap? y) 29 28)]
    [3 0]
    [4 4]
    [5 9]
    [6 6]
    [7 11]
    [8 8]
    [9 5]
    [10 10]
    [11 7]
    [12 12]))

(check-expect (doomsday-in-month 1 2021) 31)
(check-expect (doomsday-in-month 1 2020) 32)
(check-expect (doomsday-in-month 10 2020) 10)

;; Part 5 - 3: Find Doomsday
;; Takes in Date.
;; Returns Day-Of-Week.
;; Here are some local variables:
;; 'd-day-century': takes first two digit of year, e.g. 20 for 2020;
;;                  divide by 4 to see if remainder = 0  -> Tue 2
;;                                        remainder = 1 -> Sun 0
;;                                        remainder = 2 -> Fri 5
;;                                        remainder = 3 -> Wed 3
;;                   'd-day-century' for 2020 is Tuesday.
;; 'd-day-week': use 2020 as exmaple:
;;               (Tuesday + 20 + 5) = Saturday 
;;
(: find-day-of-week (-> Date Day-Of-Week))
(define (find-day-of-week date)
  (match date
    [(Date m d y)
     (local
       {(define d-day-century
               (cond
                 [(= (remainder (quotient y 100) 4) 0) 2]
                 [(= (remainder (quotient y 100) 4) 1) 0]
                 [(= (remainder (quotient y 100) 4) 2) 5]
                 [(= (remainder (quotient y 100) 4) 3) 3]
                 [else -100]))
        (define d-day-week (+ d-day-century
                             (+ (remainder y 100)
                                (quotient (remainder y 100) 4))))
        (define d-day-month (doomsday-in-month m y))
        (define day-difference (- d d-day-month))}
       (days-after (Day-Of-Week d-day-week) day-difference))]))


;; [Leap Year Test, of the 2000 Century]
;; Doom_Day_2020 is Saturday.
;; Test My_Day1 (2 25 2020), which is Tuesday
;; Test My_Day2 (1 28 2020), which is Tuesday
;; Test My_Day3 (3 1 2020), which is Sunday
;;
(define My_Day1 (Date 2 25 2020))
(define My_Day2 (Date 1 28 2020))
(define My_Day3 (Date 3 1 2020))

(check-expect (find-day-of-week My_Day1) (Day-Of-Week 2))
(check-expect (find-day-of-week My_Day2) (Day-Of-Week 2))
(check-expect (find-day-of-week My_Day3) (Day-Of-Week 0))

;; [Common Year Test, of the 1900 Century]
;; Doom_Day_1901 is Wednesday.
;; Test My_Day4 (1 25 1901), which is Friday
;; Test My_Day5 (2 1 1901), which is Friday
;; Test My_Day6 (12 31 1900), which is Tuesday
;;
(define My_Day4 (Date 1 25 1901))
(define My_Day5 (Date 2 1 1901))
(define My_Day6 (Date 12 31 1901))

(check-expect (find-day-of-week My_Day4) (Day-Of-Week 5))
(check-expect (find-day-of-week My_Day5) (Day-Of-Week 5))
(check-expect (find-day-of-week My_Day6) (Day-Of-Week 2))

(test)

;; End of Lab 2 :)
;; Thank you for grading and wish you a wonderful day!
;; -- Faradawn
;;

