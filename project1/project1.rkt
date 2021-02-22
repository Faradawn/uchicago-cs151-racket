#lang typed/racket
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require typed/test-engine/racket-tests)

;; Table of Content
;;         Part 0 -- Doomsday Calendar and Defining CalWorld
;;         Part 1 -- Draw-Month
;;         Part 2 -- Run

;; Part 0: Import Doomsday Calendar from Lab2
;;         Only uses 'day-of-week' function,
;;         which returns an Integer that represents Sun, Mon, ... 
;;         Lab2 collaborators: Henry Herzog, Sabine Salnave
(define-struct Date
  ([m : Integer]
   [d : Integer]
   [y : Integer]))

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

(define-struct CalWorld
  ([format : CalFormat]
   [current-month : Integer]
   [current-year : Integer]))

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
    

;; Part 1: draw-month
;;         this part includes 6 functions
;;         the last one (6/6) is the final 'draw-month'

;; 1/6: title-bar
;;         stacks the 'month title' on top of the 'day titles'
;;         todo: local define encapsulate draw cell
(: title-bar : Integer Integer Integer Image-Color Image-Color Integer
   Image-Color Image-Color Integer -> Image)
(define (title-bar month year cell bg1 font1 height1 bg2 font2 height2)
  (above
   (overlay/align
    "center" "center"
    (scale 0.5 (text (string-append (print-month month) " "
                                    (number->string year))
          (cast cell Byte) font1))
    (rectangle (* 7 cell) height1 'outline 'black)
    (rectangle (* 7 cell) height1 'solid bg1))
   (beside
    (overlay/align
     "center" "center"
     (scale 0.3 (text "Sun" (cast cell Byte) font2))
     (rectangle cell height2 'outline 'black)
     (rectangle cell height2 'solid bg2))
    (overlay/align
     "center" "center"
     (scale 0.3 (text "Mon" (cast cell Byte) font2))
     (rectangle cell height2 'outline 'black)
     (rectangle cell height2 'solid bg2))
    (overlay/align
     "center" "center"
     (scale 0.3 (text "Tue" (cast cell Byte) font2))
     (rectangle cell height2 'outline 'black)
     (rectangle cell height2 'solid bg2))    
    (overlay/align
     "center" "center"
     (scale 0.3 (text "Wed" (cast cell Byte) font2))
     (rectangle cell height2 'outline 'black)
     (rectangle cell height2 'solid bg2))    
    (overlay/align
     "center" "center"
     (scale 0.3 (text "Thu" (cast cell Byte) font2))
     (rectangle cell height2 'outline 'black)
     (rectangle cell height2 'solid bg2))    
    (overlay/align
     "center" "center"
     (scale 0.3 (text "Fri" (cast cell Byte) font2))
     (rectangle cell height2 'outline 'black)
     (rectangle cell height2 'solid bg2))    
    (overlay/align
     "center" "center"
     (scale 0.3 (text "Sat" (cast cell Byte) font2))
     (rectangle cell height2 'outline 'black)
     (rectangle cell height2 'solid bg2)))))

; eye-ball test
(title-bar 5 2021 40
             'dodgerblue 'lightyellow 60
             'silver 'blue 30)

;; 2 - Draw First Line

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

(first-line 6 40 'lightyellow 'black) ;; eye-ball test


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

(middle-lines 6 31 40 'lightyellow 'black) ;; eye-ball test, middle lines


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

(last-line 6 31 40 'lightyellow 'black) ;; eye-ball test


;; 5/6: place-holiday
;;        Uses overlay/xy to deploy the holiday to the right position
(: place-holiday :
   Integer Integer Integer Image-Color Image-Color Integer Integer -> Image)
(define (place-holiday day month cell bg-cell font-cell height1 height2)
  (match month
    [5
     (overlay/xy
      (draw-cell -1 cell (color 255 255 255 0) font-cell empty-image);transpar
      (* cell 1) (if (<= day 4) (+ (* cell 4) height1 height2)
                     (+ (* cell 5) height1 height2))
      (draw-cell 0 cell (color 255 192 203 90) font-cell empty-image))];pink    
    [9
     (overlay/xy
      (draw-cell -1 cell (color 255 255 255 0) font-cell empty-image)
      (* cell 1) (if (< day 2) (+ (* cell 0) height1 height2)
                     (+ (* cell 1) height1 height2))
      (draw-cell 0 cell (color 255 192 203 90) font-cell empty-image))]
    [11
     (overlay/xy
      (draw-cell -1 cell (color 255 255 255 0) font-cell empty-image)
      (* cell 1) (if (< day 2) (+ (* cell 0) height1 height2)
                     (+ (* cell 1) height1 height2))
      (draw-cell 0 cell (color 255 192 203 90) font-cell empty-image))]
    [_ empty-image]))
;; tests are under 6/6


; 6/6: draw-month
;;        Stacks the 'first-line', 'middle-lines', and 'last-line' with 'above'
;;        If the month contains special day, deploy 'place-holiday',
;;        which is a single cell that replaces the target day
(: draw-month : CalFormat Integer Integer -> Image)
(define (draw-month format month year)
  (local
    {(: day Integer)
     (define day (day-of-week (Date month 1 year)))
     (: total-days Integer)
     (define total-days (days-in-month month year))}
    (match format
      [(CalFormat cell bg1 font1 height1 bg2 font2 height2 bg-cell font-cell)
       (overlay/align
        "left" "top"
        (place-holiday day month cell bg-cell font-cell height1 height2)
        (above
         (title-bar month year cell bg1 font1 height1 bg2 font2 height2)
         (first-line day cell bg-cell font-cell)
         (middle-lines day total-days cell bg-cell font-cell)
         (last-line day total-days cell bg-cell font-cell))
       (rectangle (* 7 cell) (+ height1 height2 (* cell 6)) 'solid 'white))]
      [_ (error "wrong Calformat")])))

;(draw-month fmt0 2 2021) ;; eye-ball tests
(draw-month fmt0 5 2021)
(draw-month fmt0 5 2018)


;; Part 2: Run

;; 1/2 react-to-key
(: react-to-key : CalWorld String -> CalWorld)
(define (react-to-key world key)
  (match world
    [(CalWorld format month year)
     (match key
       ["right"
        (if (= month 12) (CalWorld format 1 (+ year 1))
            (CalWorld format (+ month 1) year))]
       ["left"
        (if (= month 1) (CalWorld format 12 (- year 1))
            (CalWorld format (- month 1) year))]
       ["up" (CalWorld format month (+ year 1))]
       ["down" (CalWorld format month (- year 1))]
       [_ world])]))
       

;; 2/2 run
(: run : CalFormat Integer Integer -> CalWorld)
(define (run format month year)  
  (big-bang (CalWorld format month year) : CalWorld
    [to-draw (λ ([a : CalWorld]) (draw-month
                                 (CalWorld-format a)
                                 (CalWorld-current-month a)
                                 (CalWorld-current-year a)))]
    [on-key react-to-key]))
;; run 
;; (run fmt0 2 2021)


(test)
