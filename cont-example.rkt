#lang racket

(require racket/control)

; k -> (lambda (x) x)
(reset (shift k 1)) ; output: 1
(reset (shift k "1")) ; output: "1"

(reset
 ; k -> (lambda (x) (+ 1 x))
 (+ 1 (shift k (* 5 2)))
 ) ; output: 10

; k -> (lambda (x) x)
(- (reset (shift k 3)) 1) ; output: 2
;;; (- (reset (shift k "3")) 1) ; error

; k -> (lambda (x) x)
((reset (shift k k)) 30) ; output: 30

; k -> (lambda (x) x)
(reset (shift k (k 2))) ; output: 2

(reset
 ; k -> (lambda (x) x)
 (shift k (+ 3 (k 2)))
 ) ; output: 5

((lambda (x)
   ; k -> (lambda (x) x)
   (+ (reset (shift k (k x))) 10)
   ) 2) ; output: 12

; k -> (lambda (x) (+ 1 x))
((reset (+ 1 (shift k k))) 20) ; output: 21

(reset 99) ; output: 99
(reset (reset 88)) ; output: 88

(shift k 1) ; output: 1

(((((reset 
     ; k -> (lambda (x) (shift k k) (shift k k) (shift k k))
     (shift k k)
     (shift k k)
     (shift k k)
     (shift k k)
     ) 1) 2) 3) 4) ; output: 4