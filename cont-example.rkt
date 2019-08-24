#lang racket

(require racket/control)

(reset (shift k 1)) ; 1
(reset (shift k "1")) ; "1"

(- (reset (shift k 3)) 1) ; 2
;;; (- (reset (shift k "3")) 1) ; error

((reset (shift k k)) 3) ; 3

((lambda (x)
   (+ (reset (shift k (k x))) 1)
   ) 2) ; 3

(((((reset 
     (shift k k)
     (shift k k)
     (shift k k)
     (shift k k)
     ) 1) 2) 3) 4) ; 4