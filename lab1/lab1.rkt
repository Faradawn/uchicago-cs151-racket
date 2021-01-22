#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require typed/test-engine/racket-tests)


;; Part 1 - Stop Sign
;; (: stop Image) renderst the final composition
;;
(: stop Image)
(define stop
  [overlay/align "center" "center"
                 (text "STOP" 60 "white")
                 (regular-polygon 75 8 "outline" "white")
                 (regular-polygon 80 8 "solid" "red")
                 ]) 


;; Part 2 - Do Not Enter Sign
;; (: container Image) renders the outside box, circle, and white bar.
;; (: do-not-enter Image) renders the final composition
;;
(: container Image)
(define container
  [overlay/align
   "center" "center"
   (rectangle 160 20 "solid" "white")
   (circle 100 "solid" "red")
   (rectangle 200 200 "outline" "black")])

(: do-not-enter Image)
(define do-not-enter
  [overlay/align/offset
   "center" "center"
   [overlay/align/offset
    "center" "center"
    (text "DO NOT" 30 "white")
    0 55
    (text "ENTER" 30 "white")]
   0 -3
  container])


;; Part 3 - Do Not Yield
;; (: triangles Image) renders 3 outside triangles
;; (: yield Image) renders the final composition
;; 
(: triangles Image)
(define triangles
  (rotate 180
  [overlay/align/offset
   "center" "center"
   [overlay/align/offset
    "center" "center"
    (triangle 100 "solid" "white")
    0 -15
    (triangle 180 "outline" "white")]   
   0 -4
   (triangle 200 "solid" "red")
   ]))
   
(: yield Image)
(define yield
  [overlay/align/offset
   "center" "center"
   (text "YIELD" 25 "red")
   0 45
   triangles])

;; Part 4 - First Warning Sign
;; (: yellowBox Image) renders the yellow container
;; (: signal-ahead Image) renders the final composition.
;;
(: yellowBox Image)
(define yellowBox
  (rotate 45
  [overlay/align
   "center" "center"
   (rectangle 125 125 "outline" "black")
   (rectangle 130 130 "solid" "gold")]))
   
(: signal-ahead Image)
(define signal-ahead
  [overlay/align
   "center" "center"
   [above/align
    "center"
    (circle 15 "solid" "red")
    (circle 4 "solid" "black")
    (circle 15 "solid" "gold")
    (circle 4 "solid" "black")
    (circle 15 "solid" "seagreen")]
   (rectangle 45 130 "solid" "black")
   yellowBox])


;; Part 5 - Second Warning Sign
;; (: arrow Image) renders the top black arrow
;; (: stop-ahead Image) renders the final composition
;
(: arrow Image)
(define arrow
  [above/align
   "center"
   (isosceles-triangle 40 90 "solid" "black")
   (rectangle 20 20 "solid" "black")])

(: stop-ahead Image)
(define stop-ahead
  [overlay/align/offset
   "center" "center"
   [above
    arrow
    (circle 5 "solid" "gold")
    (scale 0.38 [overlay (rectangle 160 74 "solid" "red") stop])]
   0 10
   yellowBox])



;; Part 6 - Third Warning Sign
;; (: yield-ahead Image) renders the final composition

(: yield-ahead Image)
(define yield-ahead
  [overlay/align/offset
   "center" "center"
   [above
    arrow
    (circle 5 "solid" "gold")
    (scale 0.45 triangles)]
   0 3
   yellowBox])
   
;; Thank you for grading! -- Faradawn