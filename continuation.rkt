#lang racket

; 「shift/reset プログラミング入門」を Racket で書いてみたもの
; http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-j.pdf

(require racket/control)
(require racket/trace)

; 2.5 ----------------------------------------------

;;; (reset (- (+ 3 (shift k (* 5 2))) 1)) ; output: 10

;;; (reset (- (+ 3 (shift k "hello")) 1)) ; output: "hello"

;;; (- (reset (+ 3 (shift k (* 5 2)))) 1) ; output: 9

(define (sum int-list)
  (if (empty? int-list)
      0
      (+ (first int-list) (sum (rest int-list)))))

(define (times num-list)
  (match num-list
    ['() 1]
    [(list x y ...) (* x (times y))]))

(define (times2 num-list)
  (reset
   (match num-list
     ['() 1]
     [(list 0 y ...) (shift k 0)]
     [(list x y ...) (* x (times2 y))])))

;(trace times2)
;(times2 '(1 2 3 4 5 6 7))
;(times2 '(1 2 3 0 4 5 6 7))

; 2.5 ----------------------------------------------

(define (f x)
  ((reset
    (- (+ 3 (shift k k)) 1)
    ) x))

;(displayln (f 10)) ;output: 12

(define (5-1 x)
  ((reset
    (* 5 (+ (shift k k) (* 3 4)))
    ) x))

;(displayln (5-1 2)) ;output: 70

(define (5-2 x)
  ((reset
    (string-append
     (if (shift k k)
         "hello"
         "hi"
         )
     " world"
     )
    ) x))

;(displayln (5-2 #t)) ;output: "hello world"
;(displayln (5-2 #f)) ;output: "hi world"

(define (6-1 lst)
  (match lst
    ['() '()]
    [(list x y ...)
     (cons x (6-1 y))
     ]))

(define (6-2 lst)
  (match lst
    ['() (shift k k)]
    [(list x y ...)
     (cons x (6-2 y))
     ]))

;((reset (6-2 '(1 2 3))) '("hoge" "fuga")) ;output: '(1 2 3 "hoge" "fuga")

; 2.7 ----------------------------------------------

(struct empty () #:transparent)
(struct node (t1 n t2) #:transparent)

(define (tree1)
  (node (node (empty) 1 (empty)) 2 (node (empty) 3 (empty))))

(define (walk1 tree)
  (match tree
    [(empty) (void)]
    [(node t1 n t2)
     (begin
       (walk t1)
       (display n)
       (walk t2)
       )]))

;;; (trace walk1)
;;; (walk1 (tree1))


(struct done () #:transparent)
(struct next (n cont) #:transparent)

(define (yield n)
  (shift k (next n k)))


(define (walk tree)
  (match tree
    [(empty) (void)]
    [(node t1 n t2)
     (begin
       (walk t1)
       (yield n)
       (walk t2)
       )]))

(define (start tree)
  (reset 
   (walk tree)
   (done)))

(define (print-nodes tree)
  (define (loop r)
    (match r
      [(done) (void)]
      [(next n k)
       (begin
         (display n)
         (loop (k))
         )]))
  (loop (start tree))
  )

;;; (start (tree1))
;;; (print-nodes (tree1))

(define (add-tree tree)
  (define (loop r)
    (match r 
      [(done) 0]
      [(next n k) (+ n (loop (k)))]))
  (loop (start tree)))

;;; (add-tree (tree1)) ; output: 6

(define (7-1-same-fringe tree1 tree2)
  (define (loop r1 r2)
    (match* (r1 r2)
      [((done) (done)) #t]
      [((done) (next _ _)) #f]
      [((next _ _) (done)) #f]
      [((next n1 k1) (next n2 k2)) 
       (if 
        (equal? n1 n2)
        (loop (k1) (k2))
        #f)]))
  (let
      ([t1 (start tree1)]
       [t2 (start tree2)])
    (loop t1 t2)))

;;; (7-1-same-fringe
;;;  (node (node (empty) 1 (empty)) 2 (node (empty) 3 (empty)))
;;;  (node (empty) 1 (node (empty) 2 (node (empty) 3 (empty)))) 
;;;  ) ; output: #t

;;; (7-1-same-fringe
;;;  (node (node (empty) 1 (empty)) 2 (node (empty) 3 (empty)))
;;;  (node (empty) 1 (node (empty) 2 (node (empty) 4 (empty)))) 
;;;  ) ; output: #f

;;; (7-1-same-fringe
;;;  (node (empty) 1 (empty))
;;;  (node (empty) 2 (empty)) 
;;;  ) ; output: #f

; 2.8 ----------------------------------------------

((lambda () 
   (define (f x)
     ((reset 
       (string-append
        (shift k (lambda (_) (k "hello")))
        " world")
       ) x))

   ;;;  (f '()) ; output: "hello world"


   (define (8-1 str)
     ((reset (string-append (string-append "hello " (shift k k)) "!")) str))

   ;;;  (displayln (8-1 "world")) ; output: "hello world!"

   (define (8-2 num)
     ((reset (string-append (string-append "hello " (number->string (shift k k))) "!")) num))

   ;;;  (displayln (8-2 123)) ; output: "hello 123!"

   (void)
   ))

; 2.10 ----------------------------------------------

((lambda ()
   (define (get)
     (shift k (lambda (state) ((k state) state))))
    
   ; TODO
   ;;;  (displayln 
   ;;;     (lambda () 
   ;;;       ((reset (get)) 0)
   ;;;       )
   ;;;     )

   (define (tick)
     (shift k (lambda (state) ((k) (+ state 1)))))

   (define (run-state thunk)
     ((reset 
       (let
           ([result (thunk)])
         (lambda (state) result))
       ) 0))

   ;;;  (displayln
   ;;;   (run-state
   ;;;    (lambda ()
   ;;;      (tick)
   ;;;      (tick)
   ;;;      (let ([a (get)])
   ;;;        (tick)
   ;;;        (- (get) a)
   ;;;        )))) ; output: 1

   (void)
   ))

; 2.12 ----------------------------------------------

((lambda ()
   (define (either a b)
     (shift
      k
      (k a)
      (k b)))

   ;;;  (reset
   ;;;   (let
   ;;;       ([x (either 0 1)])
   ;;;     (display x)
   ;;;     (displayln "")
   ;;;     )
   ;;;   ) ; output: 0 1

   (define (12-1 lis)
     (shift 
      k 
      (for ([i lis])
        (k i))))

   ;;;  (reset
   ;;;   (let
   ;;;       ([x (12-1 '(1 "2" "hello"))])
   ;;;     (display x)
   ;;;     (displayln ""))) ; output: 1 2 hello

   ;;;  (reset 
   ;;;   (let 
   ;;;       ([p (either true false)]
   ;;;        [q (either true false)])
   ;;;     (if (and (and (or p q) (or p (not q))) (or (not p) (not q)))
   ;;;         (begin
   ;;;           (display p)
   ;;;           (display ", ")
   ;;;           (display q)
   ;;;           (displayln ""))
   ;;;         (void)))) ; output: #t, #f

   (void)
   ))