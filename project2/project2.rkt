#lang typed/racket
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require typed/racket/date)
(require typed/test-engine/racket-tests)

;; project 2

(define-struct Date
  ([m : Integer]
   [d : Integer]
   [y : Integer]))

(define-struct Time
  ([hour : Integer] ;; from 0 to 23
   [minute : Integer]
   [second : Integer]))

(define-struct Day-Of-Week
  ([num : Integer]))

(: leap? (-> Integer Boolean))
(define (leap? year)
  (cond
    [(= (remainder year 400) 0) #t]
    [(= (remainder year 100) 0) #f]
    [(= (remainder year 4) 0) #t]
    [else #f]))

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

(: days-after (-> Day-Of-Week Integer Day-Of-Week))
(define (days-after day n)
  (match day
    [(Day-Of-Week d)
     (local
       {(define d1 (remainder (+ (+ d n) 35) 7))}
       (Day-Of-Week d1))]))


(: day-of-week (-> Date Integer))
(define (day-of-week date)
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
       (Day-Of-Week-num
        (days-after (Day-Of-Week d-day-week) day-difference)))]))


(check-expect (day-of-week (Date 5 1 2021)) 6)

;; Defines CalFormat, CalWorld, and a test CalFormat 'fmt0'
(define-struct CalFormat
  ([cell-size : Integer]
   [title-bar-bg : Image-Color]
   [title-bar-font : Image-Color]
   [title-bar-height : Integer]
   [day-label-bg : Image-Color]
   [day-label-font : Image-Color]
   [day-label-height : Integer]
   [cell-bg : Image-Color]
   [cell-font : Image-Color]))

(define-struct CalWorld2
  ([mode : (U 'calendar 'help)]
   [format : CalFormat]
   [calendar-current-date : Date]
   [now-date : Date]
   [now-date-string : String]
   [now-time : Time]))

(: fmt0 CalFormat)
(define fmt0
  (CalFormat 40
             'dodgerblue 'lightyellow 60
             'silver 'blue 30
             'lightyellow 'black))

;; print-month
;; takes in an Integer and returns the month in String
(: print-month : Integer -> String)
(define (print-month n)
  (match n
    [1 "January"]
    [2 "February"]
    [3 "March"]
    [4 "April"]
    [5 "May"]
    [6 "June"]
    [7 "July"]
    [8 "August"]
    [9 "September"]
    [10 "October"]
    [11 "November"]
    [12 "December"]
    [_ (error "wrong month number")]))
;; takes in an Integer and returns that day of week in String
(: print-day : Integer -> String)
(define (print-day n)
  (match n
    [0 "Sun"]
    [1 "Mon"]
    [2 "Tue"]
    [3 "Wed"]
    [4 "Thu"]
    [5 "Fri"]
    [6 "Sat"]
    [_ (error "wrong week day")]))

;; print-date takes in a date and returns a string of the date
(: print-date : Date -> String)
(define (print-date date)
  (match date
    [(Date m d y)
     (string-append (print-day (day-of-week date)) ", "
                    (print-month m) " "
                    (number->string d) ", "
                    (number->string y))]))
(check-expect (print-date (Date 3 1 2021)) "Mon, March 1, 2021")
(check-expect (print-date (Date 2 28 2021)) "Sun, February 28, 2021")
(check-expect (print-date (Date 3 31 2021)) "Wed, March 31, 2021")

;; Part 1: draw-month
;;         this part includes 6 functions
;;         the last one (6/6) is the final 'draw-month'

;; 1/6: title-bar
;;         stacks the 'month title' on top of the 'day titles'
;;         todo: local define encapsulate draw cell
(: title-bar : Integer Integer Integer Image-Color Image-Color Integer
   Image-Color Image-Color Integer -> Image)
(define (title-bar month year cell bg1 font1 height1 bg2 font2 height2)
  (local
    {(: draw-day-cell : Integer Image -> Image)
     (define (draw-day-cell n img)
       (beside
        img
        (overlay/align
         "center" "center"
         (scale 0.3 (text (print-day n) (cast cell Byte) font2))
         (rectangle cell height2 'outline 'black)
         (rectangle cell height2 'solid bg2))))}
    (above
     (overlay/align
      "center" "center"
      (scale 0.5 (text (string-append (print-month month) " "
                                      (number->string year))
                       (cast cell Byte) font1))
      (rectangle (* 7 cell) height1 'outline 'black)
      (rectangle (* 7 cell) height1 'solid bg1))
     (foldl draw-day-cell empty-image '(0 1 2 3 4 5 6)))))
; eye-ball test
;(display " --- The Title Bar --- \n")
;(title-bar 5 2021 40
;             'dodgerblue 'lightyellow 60
;             'silver 'blue 30)

;; 2/6: draw-cell
;;         Draws one cell at a time, prepares for 'foldl'
;;         Conditionally overlays text (day number) on empty cell
;;         If intake Integer n == 0, draws empty cell
;;         Otherwise draws cell with day number (n)
(: draw-cell : Integer Integer (U Image-Color String) (U Image-Color String)
   Image -> Image)
(define (draw-cell n cell bg-cell font-cell img)
  (beside
   img
   (overlay/align
    "center" "center"
    (if (< n 1) empty-image
        (scale 0.3 (text (number->string n) (cast cell Byte) font-cell)))
    (if (= n -1) empty-image
        (square cell 'outline 'black))
    (square cell 'solid bg-cell))))

;; 3/6: first-line
;;         Draws the first week of the calendar
;;         First makes a Integer list for the first 7 days, e.g.'(0 0 0 0 0 0 1)
;;         Then puts into 'foldl' and use draw-cell. ( 0 is empty cell )
;;         todo: local define encapsulate draw cell
(: first-line : Integer Integer Image-Color Image-Color -> Image)
(define (first-line day cell bg-cell font-cell)
  (foldl (λ ([n : Integer] [img : Image])
           (draw-cell n cell bg-cell font-cell img)) ;; λ to localize draw-cell
         empty-image
         (append
          (make-list day 0)
          (build-list (- 7 day)
                      (λ ([x : Integer]) (+ x 1))))))
;(display " --- The First Line --- \n")
;(first-line 6 40 'lightyellow 'black) ;; eye-ball test


;; 4/6: middle-lines
;;         Draws the middle part of the month
;;         'loop-list' is for later 'foldl' to loop through the 3 (or 4) weeks
;;         'line-list' is for each week, e.g. '(2 3 4 5 6 7 8)
;;         Finally stacks the 3 (or 4) lines together using 'foldl'
(: middle-lines : Integer Integer Integer Image-Color Image-Color -> Image)
(define (middle-lines day total-days cell bg-cell font-cell)
  (local
    {;; builds a list loops through the weeks, e.g. '(1 2 3 4)  
     (: loop-list (Listof Integer))
     (define loop-list (build-list (- (exact-floor (/ (+ total-days day) 7)) 1)
                         (λ ([x : Integer]) (+ x 1))))
     ;; creates a list for each line
     (: line-list : Integer -> (Listof Integer))
     (define (line-list n)
       (build-list 7 (λ ([x : Integer]) (- (+ 7 x) day))))
     ;; draw a line of 7 days
     (: draw-line : Integer Image -> Image)
     (define (draw-line n img)
       (above
        img
        (foldl (λ ([n1 : Integer] [img1 : Image]) ;λ to localize draw-cell
                 (draw-cell n1 cell bg-cell font-cell img1)) 
               empty-image
               (build-list 7 (λ ([x : Integer]) (- (+ x (* 7 n)) (- day 1)))))
        ))}
    (foldl draw-line empty-image loop-list)))
;(display " --- The Middle Lines --- \n")
;(middle-lines 6 31 40 'lightyellow 'black) ;; eye-ball test, middle lines


;; 4/6 last-line
;;         Similar to 'first-line'
;;         Draws the last line with empty cells in the end
(: last-line : Integer Integer Integer Image-Color Image-Color -> Image)
(define (last-line day total-days cell bg-cell font-cell)
  (local
    {(: num1 Integer) ;; first day of last line
     (define num1 (- (* (exact-floor (/ (+ total-days day) 7)) 7) (- day 1))) 
     (: num2 Integer) ;; how many days in last line 
     (define num2 (+ (- total-days num1) 1))}
    (foldl (λ ([n : Integer] [img : Image])
             (draw-cell n cell bg-cell font-cell img)) 
           empty-image
           (append
            (build-list num2 
                      (λ ([x : Integer]) (+ x num1)))
            (make-list (- 7 num2) 0)))))
;(display " --- The Last Line --- \n")
;(last-line 6 31 40 'lightyellow 'black) ;; eye-ball test

; draw-month draws the base calendar
(: draw-month : CalFormat Integer Integer Date Date String Time -> Image) 
(define (draw-month format month year cal-date now-date now-date-str now-time)
  (match format
      [(CalFormat cell bg1 font1 height1 bg2 font2 height2 bg-cell font-cell)
       ;-- define place-target
       (local
         {(define day (day-of-week (Date month 1 year)))
          (define total-days (days-in-month month year))
          (define place-target (overlay/xy
             (square cell 'solid (color 0 0 0 0))
             ;-- xy math
             (* cell (+ (remainder (+ (Date-d now-date) day -1) 7)))
             (+ (* cell (exact-floor (/ (+ (Date-d now-date) day -1) 7)))
                (+ height1 height2))
             ;-- target circle
             (overlay/align "center" "center"
                            (circle (/ cell 4) 'solid (color 255 192 203 90))
                            (square cell 'solid (color 255 192 100 50)))))}
         ;-- start overlay
         (overlay/align "left" "top" place-target (beside/align "top"
         ;-- left calendar
         (above
          (title-bar month year cell bg1 font1 height1 bg2 font2 height2)
          (first-line day cell bg-cell font-cell)
          (middle-lines day total-days cell bg-cell font-cell)
          (last-line day total-days cell bg-cell font-cell)
          (circle (/ cell 4) 'solid 'white)
          (scale 0.3 (text now-date-str (cast cell Byte)'black)))
          ;-- right calendar
         (scale 0.3 
                (above/align
                 "right"
                 (circle (/ cell ) 'solid 'white)
                 (text (string-append
                        "   "
                        (print-date cal-date))
                       (cast cell Byte) 'black) 
                 (text (string-append
                        "   Time: "
                        (number->string (Time-hour now-time)) ":"
                        (number->string (Time-minute now-time)) ":"
                        (number->string (Time-second now-time)))
                       (cast cell Byte)'black)
                 (text "   Press '?' for Help"(cast cell Byte)'black))))
          ;-- bg rectangle
        (rectangle (* 10 cell) (+ height1 height2 (* cell 7)) 'solid 'white)))]
      [_ (error "wrong Calformat")]))

; draw-menu draws the help-menu
(: draw-menu : CalFormat -> Image)
(define (draw-menu format)
  (match format
    [(CalFormat cell bg1 font1 height1 bg2 font2 height2 bg-cell font-cell)
     (overlay/align
      "left" "top"
      (scale 0.3
             (above
              (text "+ : move right one day" (cast cell Byte) 'black)
              (text "- : move left one day" (cast cell Byte)'black)))
      (rectangle (* 10 cell) (+ height1 height2 (* cell 7)) 'solid 'pink))]))

;; draw-final is the final function that used in run 
(: draw-final : CalWorld2 -> Image)
(define (draw-final world)
  (match world
    [(CalWorld2 mode format cal-date now-date now-str now-time)
     (match mode
       ['help (draw-menu format)]
       ['calendar
        (match now-date
          [(Date m d y)
           (draw-month format m y cal-date now-date now-str now-time)])])]))
; eye-ball tests tit
(define CalWorld-1 (CalWorld2 'help fmt0 (Date 3 1 2021) (Date 3 6 2021)
                              "Wed, March 3, 2021" (Time 07 29 59)))
(draw-final CalWorld-1)
(define CalWorld-2 (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 3 7 2021)
                              "Wed, March 3, 2021" (Time 07 29 59)))
(draw-final CalWorld-2)
(define CalWorld-3 (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 3 31 2021)
                              "Wed, March 3, 2021" (Time 07 29 59)))
(draw-final CalWorld-3)

;; yesterday takes in a date and returns the preivous day
;            keeping in mind the special cases of first day of month / year
(: yesterday : Date -> Date)
(define (yesterday date)
    (match date
    [(Date m d y)
       (if (= d 1) (if (= m 1)
              (Date 12 (days-in-month 12 (- y 1)) (- y 1)) ; Jan minus
              (Date (- m 1) (days-in-month (- m 1) y) y)) ; 2 28 2021
           (Date m (- d 1) y))]))
(check-expect (yesterday (Date 3 1 2021)) (Date 2 28 2021))
(check-expect (yesterday (Date 1 1 2021)) (Date 12 31 2020))
(check-expect (yesterday (Date 3 10 2021)) (Date 3 9 2021))
;; tomorrow takes in a date and returns the next day
;;          keeping in mind last day of month / year
(: tomorrow : Date -> Date)
(define (tomorrow date)
  (match date
    [(Date m d y)
     (if (= d (days-in-month m y))
         (if (= m 12) (Date 1 1 (+ y 1)) (Date (+ m 1) 1 y))
         (Date m (+ d 1) y))]))

(check-expect (tomorrow (Date 3 31 2021)) (Date 4 1 2021))
(check-expect (tomorrow (Date 12 31 2021)) (Date 1 1 2022))
(check-expect (tomorrow (Date 3 10 2021)) (Date 3 11 2021))
  
;; react-to-key handles the 8 types inputs of keyboard
(: react-to-key : CalWorld2 String -> CalWorld2)
(define (react-to-key world key)
  (match world
    [(CalWorld2 mode format cal-date now-date now-str now-time)
     (match now-date
       [(Date m d y)
        ;-- define plus minus functions
        (local
          {(define next-month
             (cond [(= m 12) (Date 1 d (+ y 1))] ; test 12 2021
                   [(> d (days-in-month (+ m 1) y)) ; test 2 2021 -> 3 2021
                    (Date (+ m 1) (days-in-month (+ m 1) y) y)]
                   [else (Date (+ m 1) d y)]))
           (define prev-month
             (cond [(= m 1) (Date 12 d (- y 1))]
                   [(> d (days-in-month (- m 1) y)) ; test 2 2021 -> 3 2021
                    (Date (- m 1) (days-in-month (- m 1) y) y)]
                   [else (Date (- m 1) d y)]))
           (define next-year
             (cond [(and (= m 2) (= d 29)) (Date 3 1 (+ y 1))]
                   [else (Date m d (+ y 1))]))
           (define prev-year
             (cond [(and (= m 2) (= d 29)) (Date 3 1 (- y 1))]
                   [else (Date m d (- y 1))]))}
           
        ;-- match key
        (match key
          ["+" (CalWorld2 mode format cal-date
                          (tomorrow now-date)
                          (print-date (tomorrow now-date)) now-time)]
          ["-" (CalWorld2 mode format cal-date
                          (yesterday now-date)
                          (print-date (yesterday now-date)) now-time)]
          ["right" (CalWorld2 mode format cal-date
                              next-month (print-date next-month) now-time)]
          ["left" (CalWorld2 mode format cal-date
                              prev-month (print-date prev-month) now-time)]
          ["up" (CalWorld2 mode format cal-date
                              next-year (print-date next-year) now-time)]
          ["down" (CalWorld2 mode format cal-date
                              prev-year (print-date prev-year) now-time)]
          ["T" (CalWorld2 mode format cal-date
                              cal-date (print-date cal-date) now-time)]
          ["?" (CalWorld2 'help format cal-date
                              now-date (print-date now-date) now-time)]
          ["escape" (CalWorld2 'calendar format cal-date
                              now-date (print-date now-date) now-time)]
          [_ world]))])]))
;; tests of right and left; (tests of - + are under yesterday and tomorrow)
(define world1 (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 12 31 2021)
                              " " (Time 07 29 59)))
(check-expect (react-to-key world1 "right")
              (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 1 31 2022)
                         "Mon, January 31, 2022" (Time 7 29 59)))
(define world2 (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 1 31 2021)
                              " " (Time 07 29 59)))
(check-expect (react-to-key world2 "right")
              (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 2 28 2021)
                         "Sun, February 28, 2021" (Time 7 29 59)))
(define world3 (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 1 5 2021)
                              " " (Time 07 29 59)))
(check-expect (react-to-key world3 "right")
              (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 2 5 2021)
                         "Fri, February 5, 2021" (Time 7 29 59)))
(define world4 (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 1 31 2021)
                              " " (Time 07 29 59)))
(check-expect (react-to-key world4 "left")
              (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 12 31 2020)
                         "Thu, December 31, 2020" (Time 7 29 59)))
(define world5(CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 3 31 2021)
                              " " (Time 07 29 59)))
(check-expect (react-to-key world5 "left")
              (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 2 28 2021)
                         "Sun, February 28, 2021" (Time 7 29 59)))
(define world6 (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 3 5 2021)
                              " " (Time 07 29 59)))
(check-expect (react-to-key world6 "left")
              (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 2 5 2021)
                         "Fri, February 5, 2021" (Time 7 29 59)))
;; tests for up and down
(define worlda (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 2 29 2020)
                              " " (Time 07 29 59)))
(check-expect (react-to-key worlda "up")
              (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 3 1 2021)
                         "Mon, March 1, 2021" (Time 7 29 59)))
(define worldb (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 2 28 2020)
                              " " (Time 07 29 59)))
(check-expect (react-to-key worldb "up")
              (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 2 28 2021)
                         "Sun, February 28, 2021" (Time 7 29 59)))
(check-expect (react-to-key worlda "down")
              (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 3 1 2019)
                         "Fri, March 1, 2019" (Time 7 29 59)))
(check-expect (react-to-key worldb "down")
              (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 2 28 2019)
                         "Thu, February 28, 2019" (Time 7 29 59)))
;; test for T and ?
(check-expect (react-to-key world1 "T")
              (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 3 1 2021)
                         "Mon, March 1, 2021" (Time 7 29 59)))
(check-expect (react-to-key world1 "?")
              (CalWorld2 'help fmt0 (Date 3 1 2021) (Date 12 31 2021)
                         "Fri, December 31, 2021" (Time 7 29 59)))
(check-expect (react-to-key world1 "esccape")
              (CalWorld2 'calendar fmt0 (Date 3 1 2021) (Date 12 31 2021)
                         " " (Time 7 29 59)))

(: read-date-now : -> Date)
(define read-date-now (λ () (Date (date-month (current-date))
                            (date-day (current-date))
                            (date-year (current-date)))))

(: read-time-now : -> Time)
(define read-time-now (λ () (Time (date-hour (current-date))
                            (date-minute (current-date))
                            (date-second (current-date)))))

;; tick takes in a CalWorld and returns an updated one every second
;; tick updates the time continously
;  and changes the current date if enters new day
(: tick : CalWorld2 -> CalWorld2)
(define (tick world)
  (match world
    [(CalWorld2 mode format cal-date now-date now-str now-time)
     (match* ((read-date-now) (read-time-now))
       [((Date month day year) (Time hr min sec))
        (local
          {(define new-now-date
             (if (and (= hr 23) (= min 59) (= sec 59))
                 (match now-date
                   [(Date m d y)
                    (if (= d (days-in-month m y))
                        (if (= m 12) (Date 1 1 (+ y 1)) (Date (+ m 1) 1 y))
                        (Date m (+ d 1) y))])
                 now-date))}
          (CalWorld2 mode format
                     (Date month day year)
                     new-now-date
                     (print-date new-now-date)
                     (Time hr min sec)))])]))


;; run, takes in a CalFormat, month, and year to initialize
;; a CalWorld2. Tick will set Time to the current time. 
(: run : CalFormat Integer Integer -> CalWorld2)
(define (run format month year)  
  (big-bang (CalWorld2 'calendar format
                       (read-date-now) ; cal-date
                       (Date month 1 year) ; now-date
                       (print-date (Date month 1 year)) ; now-date-str
                       (read-time-now)) : CalWorld2
    [to-draw draw-final]
    [on-key react-to-key]
    [on-tick tick 1]))

(test)
(run fmt0 12 2021)                 


