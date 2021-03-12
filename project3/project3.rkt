#lang typed/racket
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require typed/racket/date)
(require typed/test-engine/racket-tests)

;; Project 3
;; Basic codes borrowed from project1 and project2
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

;; New project 3 defines Optional,
;;     CalFormat, CalWorld, and a test CalFormat 'fmt0'
(define-type (Optional A)
  (U 'None (Some A)))

(define-struct (Some A)
  ([value : A]))

(define-struct Span
  ([start : Time]
   [end : Time]))

(define-struct Event
  ([date : Date]
   [time : (U 'all-day Time Span)]
   [description : String]))

(define-type EventTree
  (U 'Empty EventNode))

(define-struct EventNode
  ([date : Date]
   [events : (Listof Event)] ;; maintain this list in ascending order
   [lsub : EventTree]
   [rsub : EventTree]))

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

(define-struct CalWorld3
  ([mode : (U 'calendar 'help 'entry)]
   [entry-mode : (U 'start 'end 'description)]
   [format : CalFormat]
   [calendar-current-date : Date]
   [now-date : Date]
   [now-date-string : String]
   [now-time : Time]
   [notepad : String]
   [opt-start : (Optional Time)]
   [opt-end : (Optional Time)]
   [events : EventTree]))

(define-struct CalWorld2
  ([mode : (U 'calendar 'help)]
   [format : CalFormat]
   [calendar-current-date : Date]
   [now-date : Date]
   [now-date-string : String]
   [now-time : Time]))

(: fmt0 CalFormat)
(define fmt0
  (CalFormat 50
             'black 'white 60
             'silver 'black 20
             'white 'black))
(define fmt1
  (CalFormat 30
             'black 'white 60
             'silver 'black 20
             'white 'black))

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

;; print-time takes in a time and returns a string of the time
(: print-time : Time -> String)
(define (print-time time)
  (match time
    [(Time hr min sec)
     (string-append
      (number->string
       (if (> hr 12) (- hr 12) hr)) ":"
           (number->string min) (if (> hr 12) "pm" "am"))]))
(check-expect (print-time (Time 13 50 00)) "1:50pm")

;; 1/6: title-bar, stacks the 'month title' on top of the 'day titles'
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

;; 2/6: draw-cell, Draws one cell at a time, prepares for 'foldl'
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

;; 3/6: first-line, Draws the first week of the calendar
(: first-line : Integer Integer Image-Color Image-Color -> Image)
(define (first-line day cell bg-cell font-cell)
  (foldl (λ ([n : Integer] [img : Image])
           (draw-cell n cell bg-cell font-cell img)) ;; λ to localize draw-cell
         empty-image
         (append
          (make-list day 0)
          (build-list (- 7 day)
                      (λ ([x : Integer]) (+ x 1))))))

;; 4/6: middle-lines, Draws the middle part of the month
(: middle-lines : Integer Integer Integer Image-Color Image-Color -> Image)
(define (middle-lines day total-days cell bg-cell font-cell)
  (local
    {(define loop-list (build-list (- (exact-floor (/ (+ total-days day) 7)) 1)
                         (λ ([x : Integer]) (+ x 1))))
     (define (line-list n)
       (build-list 7 (λ ([x : Integer]) (- (+ 7 x) day))))
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

;; 4/6 last-line, Draws the last line with empty cells in the end
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


;; The main part of project3 starts here

;; (take) takes in a list and keep the sub-list before index
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
;; (get) takes in a list and returns the element at index
(: get : All(A) (Listof A) Integer -> A)
(define (get ls pos)
  (match ls
    ['() (error "out of bound")]
    [(cons head tail) (if (<= pos 0)
                          head
                          (list-ref tail (- pos 1)))]))
(define char-ls (string->list "12:30pm"))
(check-expect (char=? (get char-ls 2) #\:) #t)
;; (list->int) takes ina list of numeric char and returns an integer
(: list->int : (Listof Char) -> Integer)
(define (list->int ls)
  (cast (string->number (list->string ls)) Integer))
(check-expect (list->int '(#\1 #\2)) 12) 

;; (string->time) takes the string from notepad and returns a Optional Time
(: string->time : String -> (Optional Time))
(define (string->time str)
  (if (< (string-length str) 3) 'None
  (local
    {(define ls0 (string->list str))
     (define ls1 (if (or (char=? (get ls0 1) #\:) (char=? (get ls0 2) #\:))
              (filter char-numeric? (take ls0 2)) '()))
     (define ls2 (filter char-numeric? (take (drop ls0 (+ (length ls1) 1)) 2)))
     (define a (length ls1))
     (define b (length ls2))
     (define am (list->string (take (drop ls0 (+ a b 1)) 2)))}
    (if (and (> a 0) (> b 0) (<= (length ls0) (+ a b 3)))
        (match am
          ["am" (Some (Time (list->int ls1) (list->int ls2) 0))]
          ["pm" (Some (Time (+ (list->int ls1) 12) (list->int ls2) 0))]
          [_ 'None])
          'None))))
(check-expect (string->time "10:00am") (Some (Time 10 0 0)))
(check-expect (string->time "4:30pm") (Some (Time 16 30 0)))
(check-expect (string->time "1:30am") (Some (Time 1 30 0)))
(check-expect (string->time ":30pm") 'None)
(check-expect (string->time "12:30pmm") 'None)
(check-expect (string->time "1230pm") 'None)

;; (time<?) takes in two Time and compare which one is eariler
(: time<? : Time Time -> Boolean)
(define (time<? t1 t2)
  (match* (t1 t2)
    [((Time h1 m1 s1)(Time h2 m2 s2))
     (if (< h1 h2) #t
         (if (= h1 h2) (if (< m1 m2) #t
                           (if (= m1 m2) (< s1 s2) #f)) #f))]))
(check-expect (time<? (Time 10 23 00) (Time 10 23 01)) #t)
(check-expect (time<? (Time 10 23 00) (Time 10 24 00)) #t)
(check-expect (time<? (Time 12 23 00) (Time 13 24 01)) #t)
(check-expect (time<? (Time 12 23 00) (Time 12 23 00)) #f)
(check-expect (time<? (Time 1 23 00) (Time 20 24 00)) #t)
(check-expect (time<? (Time 12 23 00) (Time 20 24 01)) #t)
;; (time=?) takes in two Time and compare if they are equal
(: time=? : Time Time -> Boolean)
(define (time=? t1 t2)
  (match* (t1 t2)
    [((Time h1 m1 s1)(Time h2 m2 s2))
     (and (= h1 h2) (= m1 m2) (= s1 s2))]))
(check-expect (time=? (Time 10 23 00) (Time 10 23 00)) #t)
(check-expect (time=? (Time 10 23 00) (Time 10 24 00)) #f)

;; (event<?) determines if the first event is "less than"
;;          the second according to event order
(: event<? : Event Event -> Boolean)
(define (event<? event1 event2)
  (match* (event1 event2)
    [((Event date1 time1 des1) (Event date2 time2 des2))
     (match* (time1 time2)
       [('all-day 'all-day) (string<? des1 des2)]
       [('all-day _) #t]
       [(_ 'all-day) #f]
       [((Time _ _ _)(Time _ _ _))
        (if (time<? time1 time2) #t
            (if (time=? time1 time2) (string<? des1 des2) #f))]
       [((Time _ _ _)(Span start end)) (if (not (time<? time1 start)) #f #t)]
       [((Span start end) (Time _ _ _)) (if (time<? start time2) #t #f)]
       [((Span start1 end1)(Span start2 end2))
        (if (time<? start1 start2) #t
            (if (time=? start1 start2)
                (if (time<? end1 end2) #t
                    (if (time=? end1 end2)
                        (string<? des1 des2) #f)) #f))])]))

(define date1 (Date 5 12 2019))
(define date2 (Date 2 11 2021))
(define date3 (Date 2 20 2021))
(define date4 (Date 4 9 2021))
(define time1 (Time 0 50 00))
(define time2 (Time 11 22 01))
(define time3 (Time 22 09 00))
(define time4 (Time 23 00 00))
(check-expect
 (event<? (Event date1 'all-day "a") (Event date1 'all-day "b")) #t)
(check-expect
 (event<? (Event date1 'all-day "a") (Event date1 time1 "b")) #t)
(check-expect
 (event<? (Event date1 time1 "c") (Event date1 time2 "a")) #t)
(check-expect
 (event<? (Event date1 time1 "a") (Event date1 time1 "b")) #t)
(check-expect
 (event<? (Event date1 time1 "a") (Event date1 (Span time1 time2) "a")) #f)
(check-expect
 (event<? (Event date1 time2 "a") (Event date1 (Span time1 time2) "a")) #f)
(check-expect
 (event<? (Event date1 (Span time1 time2) "b")
          (Event date1 (Span time1 time3) "a")) #t)
(check-expect
 (event<? (Event date1 (Span time2 time3) "b")
          (Event date1 (Span time1 time4) "a")) #f)
       
;; (date<?) compares two date and determine which is smaller
(: date<? : Date Date -> Boolean)
(define (date<? date1 date2)
  (match* (date1 date2)
    [((Date m1 d1 y1)(Date m2 d2 y2))
     (if (< y1 y2) #t
         (if (= y1 y2) ;;
             (if (< m1 m2) #t
                 (if (= m1 m2) (< d1 d2)  #f)) #f))]))
(check-expect (date<? date1 date1) #f)
(check-expect (date<? date1 date2) #t)
(check-expect (date<? date2 date3) #t)
(check-expect (date<? date4 date1) #f)
;; (date=? compares if two dates are equal
(: date=? : Date Date -> Boolean)
(define (date=? date1 date2)
  (match* (date1 date2)
    [((Date m1 d1 y1)(Date m2 d2 y2))
     (and (= m1 m2) (= d1 d2) (= y1 y2))]))
(check-expect (date=? date1 date1) #t)
(check-expect (date=? date1 date2) #f)


; define events
(define event1 (Event date1 'all-day "Asa's Birthday Party"))
(define event2 (Event date1 'all-day "Belle's Birthday Party"))
(define event3 (Event date1 time1 "Cindy's Birthday Party"))
(define event4 (Event date2 time1 "Daisy's Birthday Party"))
(define event5 (Event date2 (Span time2 time3) "Evelyn's Birthday Party"))
(define event6 (Event date2 (Span time3 time4) "Freya's Birthday Party"))

;; (insert) adds an event to an list in ascending order
(: insert : Event (Listof Event) -> (Listof Event))
(define (insert s ls)
  (match ls
    ['() (list s)]
    [(cons head tail) (if (event<? s head) (cons s ls)
                          (cons head (insert s tail)))]))
(check-expect (insert event2 (list event1 event3)) (list event1 event2 event3))
(check-expect (insert event3 (list event1 event2)) (list event1 event2 event3))
(check-expect (insert event4 '()) (list event4))

;; (insert-event-tree) inserts event ascendingly into list, or creates node
(: insert-event-tree : Event EventTree -> EventTree)
(define (insert-event-tree event tree)
  (match event
    [(Event date time des)
     (match tree
       ['Empty (EventNode date (list event) 'Empty 'Empty)]
       [(EventNode date1 ls lsub rsub)
        (cond
          [(date<? date date1) (EventNode date1 ls
                                          (insert-event-tree event lsub) rsub)]
          [(date=? date date1) (EventNode date (insert event ls) lsub rsub)]
          [else (EventNode date1 ls lsub (insert-event-tree event rsub))])])]))

(define tree1 (EventNode date2 (list event4 event5)
                         (EventNode date1 (list event1 event2) 'Empty 'Empty)
                         'Empty))

(check-expect (insert-event-tree event6 tree1)
              (EventNode date2 (list event4 event5 event6)
                         (EventNode date1 (list event1 event2) 'Empty 'Empty)
                         'Empty))
(check-expect (insert-event-tree event3 tree1)
              (EventNode date2 (list event4 event5)
                         (EventNode date1 (list event1 event2 event3)
                                    'Empty 'Empty) 'Empty))
(check-expect (insert-event-tree event1 'Empty)
              (EventNode date1 (list event1) 'Empty 'Empty))


;; (insert-event-world) insert an event into the event tree in a cal world
(: insert-event-world : Event CalWorld3 -> CalWorld3)
(define (insert-event-world event world)
  (match world
    [(CalWorld3 mode entry-mode format cal-date now-date now-str now-time
                notepad opt-start opt-end tree)
     (CalWorld3 mode entry-mode format cal-date now-date now-str now-time
                notepad opt-start opt-end (insert-event-tree event tree))]))

(define world0 (CalWorld3 'calendar 'start fmt0 date1 date1 "str" time1
                         "10:00am" 'None 'None tree1))
(define world1 (CalWorld3 'entry 'start fmt1 date1 date1 "str" time1
                         "10:00am" 'None 'None tree1))
(check-expect (insert-event-world event6 world0)
              (CalWorld3 'calendar 'start fmt0 date1 date1 "str" time1
                         "10:00am" 'None 'None
                         (EventNode date2 (list event4 event5 event6)
                         (EventNode date1 (list event1 event2) 'Empty 'Empty)
                         'Empty)))
    
;; (retrieve-event) fetches the list of events for the given date or '()
(: retrieve-events : Date EventTree -> (Listof Event))
(define (retrieve-events date tree)
  (match tree
    ['Empty '()]
    [(EventNode date1 ls lsub rsub)
        (cond
          [(date<? date date1) (retrieve-events date lsub)]
          [(date=? date date1) ls]
          [else (retrieve-events date rsub)])]))

(check-expect (retrieve-events date1 tree1) (list event1 event2))
(check-expect (retrieve-events date2 tree1) (list event4 event5))
(check-expect (retrieve-events date3 tree1) '())

; draw-event-bar - new
(: draw-event-bar : Date EventTree Byte -> Image)
(define (draw-event-bar date tree size)
    (foldr (λ ([x : Event][acc : Image])
           (above
            (circle 20 'solid 'white)
            (rectangle size 5 'solid 'darkcyan)
            (circle 10 'solid 'white)
            (text (print-date date) size 'black)
            (text (match x
                    ['() "You Are Free Today!"]
                    [(Event date time des)
                     (string-append
                      (match time
                        ['all-day "(all-day)"]
                        [(Time _ _ _)(print-time time)]
                        [(Span a b)(string-append
                                    (print-time a) "-"
                                    (print-time b))])
                      "\n" des)]) size 'black) acc))
           empty-image (retrieve-events date tree)))










; draw-month draws the base calendar
(: draw-month :
   CalFormat Integer Integer Date Date String Time EventTree -> Image) 
(define
  (draw-month format month year cal-date now-date now-date-str now-time tree)
  (match format
      [(CalFormat cell bg1 font1 height1 bg2 font2 height2 bg-cell font-cell)
       ;-- define place-target
       (local
         {(define font-size (cast cell Byte))
          (define hr (Time-hour now-time))
          (define min (Time-minute now-time))
          (define sec (Time-second now-time))
          (define day (day-of-week (Date month 1 year)))
          (define total-days (days-in-month month year))
          (define place-target (overlay/xy
             (square cell 'solid (color 0 0 0 0))
             ;-- xy math
             (* cell (+ (remainder (+ (Date-d now-date) day -1) 7)))
             (+ (* cell (exact-floor (/ (+ (Date-d now-date) day -1) 7)))
                (+ height1 height2))
             ;-- target circle
             (overlay/align "center" "center"
                            (circle (/ cell 7) 'solid (color 0 0 0 20))
                            (circle (/ cell 4) 'solid (color 0 0 0 15))
                            (square cell 'solid (color 0 0 0 10)))))}
         ;-- start overlay
         (overlay/align "left" "top" place-target (beside/align "top"
         ;-- left calendar
         (above
          (title-bar month year cell bg1 font1 height1 bg2 font2 height2)
          (first-line day cell bg-cell font-cell)
          (middle-lines day total-days cell bg-cell font-cell)
          (last-line day total-days cell bg-cell font-cell)
          (circle (/ cell 4) 'solid 'white)
          (scale 0.4 (text/font now-date-str (cast cell Byte)
                      "black" "Gill Sans" 'decorative 'normal 'light #f)))
          ;-- right sidebar
         (scale 0.27 
                (above/align
                 "right"
                 (circle (/ cell ) 'solid 'white)
                 (text/font (string-append "   " (print-date cal-date) )
                            (cast cell Byte) "black" "Gill Sans"
                            'modern 'normal 'bold #f)
                 (text (string-append
                        "   Time:  "
                        (number->string
                         (if (> hr 12) (- hr 12) hr)) ":"
                        (number->string min) ":"
                        (number->string sec) " "
                        (if (> hr 12) "pm\n" "am\n")) (cast cell Byte)'black)
                 (text/font "   Press '?' for Help\n[return] to enter event\n"
                            (cast cell Byte)
                            "black" "Gill Sans" 'script 'normal 'light #f)
                 (draw-event-bar now-date tree font-size))))
          ;-- background size 7 : 5
        (rectangle (* 10 cell) (+ height1 height2 (* cell 7)) 'solid 'white)))]
      [_ (error "wrong Calformat")]))








; draw-enter-event draws the input event page
(: draw-entry : CalFormat String (Optional Time) (Optional Time)
   (U 'start 'end 'description) String -> Image)
(define (draw-entry format notepad opt-start opt-end entry-mode now-str)
  (match format
    [(CalFormat cell bg1 font1 height1 bg2 font2 height2 bg-cell font-cell)
     (local
       {(: draw-textbox : String -> Image)
        (define (draw-textbox str)
          (above
           (overlay/align
            "left" "center"
            (text str (cast cell Byte) 'black)
            (rectangle (if (> (string-length str)20)
                           (+ (* cell 10) (* 10 (- (string-length str) 20)))
                           (* cell 10)) cell 'outline 'black)
            (rectangle (if (> (string-length str) 20)
                           (+ (* cell 10) (* 10 (- (string-length str) 20)))
                           (* cell 10)) cell 'solid
                                       (match str
                                         ["" 'white]
                                         [_ 'lavender])))
           (circle (quotient cell 2) 'solid 'white)))}
       (overlay/align/offset
        "left" "top"
        (scale 0.3
         (above/align "left"           
          (text/font "Entry Page\n" (cast cell Byte)
                     "black" "Gill Sans" 'script 'normal 'bold #t)
          (match entry-mode
            ['start ;-- 1
             (above/align
              "left"
              (text "Enter start time (esc to canel, ` to clear\n"
                    (cast cell Byte) 'black)
              (draw-textbox now-str)
              (draw-textbox notepad))]
            ['end ;-- 2
             (above/align
              "left"
              (text "Enter end time (esc to canel, ` to clear\n"
                    (cast cell Byte) 'black)
                        (draw-textbox now-str)
              (draw-textbox (match opt-start
                              ['None " "]
                              [(Some t) (print-time t)]))
              (draw-textbox notepad))]
            ['description ;-- 3
             (above/align
              "left"
              (text "Enter description (esc to canel, ` to clear\n"
                    (cast cell Byte) 'black)
              (draw-textbox now-str)
              (draw-textbox (match opt-start
                              ['None " "]
                              [(Some t) (print-time t)]))
              (draw-textbox (match opt-end
                              ['None " "]
                              [(Some t) (print-time t)]))
              (draw-textbox notepad))] )))
        -20 -15
        ;-- background size
        (rectangle (* 10 cell) (+ height1 height2 (* cell 7))
                   'solid 'white)))]))
















; draw-menu draws the help-menu
(: draw-menu : CalFormat -> Image)
(define (draw-menu format)
  (match format
    [(CalFormat cell bg1 font1 height1 bg2 font2 height2 bg-cell font-cell)
     (overlay/align/offset
      "left" "top"
      (scale 0.3
             (above/align "left"
              (text/font "Help\n" (cast cell Byte)
                         "black" "Gill Sans" 'script 'normal 'bold #t)
              (text "+ : Day Forward \n" (cast cell Byte) 'black)
              (text "- : Day Backward \n" (cast cell Byte)'black)
              (text "[right] : Month Forward \n" (cast cell Byte)'black)
              (text "[left] : Month Backward \n" (cast cell Byte)'black)
              (text "[up] : Year Forward \n" (cast cell Byte)'black)
              (text "[down] : Year Backward \n" (cast cell Byte)'black)
              (text "T : Reset to Current Date \n" (cast cell Byte)'black)
              (text "? : Help" (cast cell Byte)'black)))
      -20 -15
      ;-- background size
      (rectangle (* 10 cell) (+ height1 height2 (* cell 7)) 'solid 'white))]))

;; draw-final is the final function that used in run 
(: draw-final : CalWorld3 -> Image)
(define (draw-final world)
  (match world
    [(CalWorld3 mode entry-mode format cal-date now-date now-str now-time
                notepad opt-start opt-end tree)
     (match mode
       ['entry (draw-entry format notepad opt-start opt-end entry-mode now-str)]
       ['help (draw-menu format)]
       ['calendar
        (match now-date
          [(Date m d y)
           (draw-month format m y cal-date now-date now-str now-time tree)])]
       ['entry empty-image])]))

(define world2 (CalWorld3 'entry 'description fmt1 date1 date1
                          "Mon, March 1, 2021" time1
                          "" 'None 'None tree1))

(draw-final world2)




        
                       
                   
   
    



;; react-to-key handles the 8 types inputs of keyboard
(: react-to-key : CalWorld3 String -> CalWorld3)
(define (react-to-key world key)
  (match world
    [(CalWorld3 mode entry-mode format cal-date now-date now-str now-time
                notepad opt-start opt-end events)
     (match mode
       ['entry
        (match key
          ["\r"
           (match entry-mode
             ['start (CalWorld3 mode (match notepad
                                       ["" 'description]
                                       [_ 'end])
                                format cal-date now-date
                                now-str now-time ""
                                (string->time notepad) opt-end events)]
             ['end (CalWorld3 mode 'description format cal-date now-date
                                now-str now-time ""
                                opt-start (string->time notepad) events)]
             ['description (CalWorld3 'calendar 'start format cal-date now-date
                                now-str now-time ""
                                opt-start opt-end
                                (insert-event-tree
                                 (match opt-start
                                   ['None (Event now-date 'all-day notepad)]
                                   [(Some n)
                                    (match opt-end
                                      ['None (Event now-date n notepad)]
                                      [(Some m)(Event now-date (Span n m)
                                                      notepad)])]) events))])]
          ["escape" (CalWorld3 'calendar 'start
                               format cal-date now-date
                               now-str now-time ""
                               'None 'None events)]
          ["`" (CalWorld3 'entry 'start
                               format cal-date now-date
                               now-str now-time ""
                               'None 'None events)]
          ["shift" (CalWorld3 mode entry-mode
                              format cal-date now-date
                              now-str now-time notepad
                              opt-start opt-end events)]
          [n (CalWorld3 mode entry-mode
                               format cal-date now-date
                               now-str now-time (string-append notepad n)
                               opt-start opt-end events)]
          )]
       [_
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
          ["\r" (if (symbol=? mode 'calendar)
                       (CalWorld3 'entry 'start
                               format cal-date now-date
                               now-str now-time ""
                               'None 'None events)
                       (CalWorld3 'calendar 'start
                               format cal-date now-date
                               now-str now-time ""
                               'None 'None events))]
          ["+" (CalWorld3 mode entry-mode format cal-date
                          (tomorrow now-date)
                          (print-date (tomorrow now-date)) now-time
                          notepad opt-start opt-end events)]
          ["-" (CalWorld3 mode entry-mode format cal-date
                          (yesterday now-date)
                          (print-date (yesterday now-date)) now-time
                          notepad opt-start opt-end events)]
          ["right" (CalWorld3 mode entry-mode format cal-date
                              next-month (print-date next-month) now-time
                              notepad opt-start opt-end events)]
          ["left" (CalWorld3 mode entry-mode format cal-date
                              prev-month (print-date prev-month) now-time
                              notepad opt-start opt-end events)]
          ["up" (CalWorld3 mode entry-mode format cal-date
                              next-year (print-date next-year) now-time
                              notepad opt-start opt-end events)]
          ["down" (CalWorld3 mode entry-mode format cal-date
                              prev-year (print-date prev-year) now-time
                              notepad opt-start opt-end events)]
          ["T" (CalWorld3 mode entry-mode format cal-date
                              cal-date (print-date cal-date) now-time
                              notepad opt-start opt-end events)]
          ["?" (CalWorld3 'help entry-mode format cal-date
                              now-date (print-date now-date) now-time
                              notepad opt-start opt-end events)]
          ["escape" (CalWorld3 'calendar entry-mode format cal-date
                              now-date (print-date now-date) now-time
                              notepad opt-start opt-end events)]
          [_ world]))])])]))
;; read-date-now and read-time-now reads the current date and time
;;      and return a Date and Time sturct
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
(: tick : CalWorld3 -> CalWorld3)
(define (tick world)
  (match world
    [(CalWorld3 mode entry-mode format cal-date now-date now-str now-time
                notepad opt-start opt-end events)
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
          (CalWorld3 mode entry-mode format
                     (Date month day year)
                     new-now-date
                     (print-date new-now-date)
                     (Time hr min sec)
                     notepad opt-start opt-end events))])]))

;; run, takes in a CalFormat, month, and year to initialize
;; a CalWorld. Tick will set Time to the current time. 
(: run : CalFormat Integer Integer -> CalWorld3)
(define (run format month year)  
  (big-bang (CalWorld3 'calendar 'start format
                       (read-date-now) ; cal-date
                       (Date month 1 year) ; now-date
                       (print-date (Date month 1 year)) ; now-date-str
                       (read-time-now)
                       ""
                       'None
                       'None
                       'Empty) : CalWorld3
    [name "CS151 Project3 Winter 2021"]
    [to-draw draw-final]
    [on-key react-to-key]
    [on-tick tick 1]))





(run fmt0 3 2021)


(test)
