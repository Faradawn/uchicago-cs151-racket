#lang typed/racket

(require "../../include/cs151-core.rkt")
(require "../../include/cs151-image.rkt")

(: first-name String)
(define first-name "Faradawn")

(: last-name String)
(define last-name "Yang")

(: platform String)
(define platform "Mac")

(: time-zone String)
(define time-zone "Shanghai, Asia, +8:00 CST")

(: blue-star Image)
(define blue-star (star 50 "solid" "blue"))