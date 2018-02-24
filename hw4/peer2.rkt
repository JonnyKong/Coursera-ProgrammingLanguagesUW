
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (cond [(< low high)
         (cons low (sequence (+ low stride) high stride))]
        [(= low high)
         (cons high null)]
        [#t null]))
;;; 5 pts


(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))
;;; 5 pts


(define (list-nth-mod xs n)
  (cond [(> 0 n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))
;;; 5 pts


(define (stream-for-n-steps s n)
  (let ([x (s)])
    (cond [(< 0 n) (cons (car x) (stream-for-n-steps (cdr x) (- n 1)))]
          [#t null])))
;;; 5 pts


(define funny-number-stream
  (letrec ([f (lambda (x) (cond [(= 0 (remainder x 5)) (cons (- x) (lambda () (f (+ x 1))))]
                                [#t (cons x (lambda () (f (+ x 1))))]))])
    (lambda () (f 1))))
;;; 5 pts


(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    dan))
;;; 5 pts


(define (stream-add-zero s)
  (letrec ([f (lambda (st)
                (cons (cons 0 (car st)) (lambda () (f ((cdr st))))))])
    (lambda ()(f (s)))))
;;; 5 pts


(define (cycle-lists xs ys)
  (letrec ([f (lambda (xs ys n)
                (cons
                 (cons (list-nth-mod xs n) (list-nth-mod ys n))
                 (lambda () (f xs ys (+ n 1)))))])
    (lambda () (f xs ys 0))))
;;; 5 pts


;---------------9------------------------
;Write a function vector-assoc that takes a value v and a vector vec. It should behave like Racket's
;assoc library function except (1) it processes a vector (Racket's name for an array) instead of a list,
;(2) it allows vector elements not to be pairs in which case it skips them, and (3) it always takes exactly
;two arguments. Process the vector elements in order starting from 0. You must use library functions
;vector-length, vector-ref, and equal?. Return #f if no vector element is a pair with a car field
;equal to v, else return the first pair with an equal car field. Sample solution is 9 lines, using one local
;recursive helper function.

(define (vector-assoc v vec)
  (letrec ([va
            (lambda (n)
              (cond [(<= (vector-length vec) n) #f]
                    [(and (pair? (vector-ref vec n))
                          (equal? v (car (vector-ref vec n))))
                     (vector-ref vec n)]
                    [#t (va (+ n 1))]))])
    (va 0)))
;;; 5 pts


;
;Write a function cached-assoc that takes a list xs and a number n and returns a function that takes
;one argument v and returns the same thing that (assoc v xs) would return. However, you should
;use an n-element cache of recent results to possibly make this function faster than just calling assoc
;(if xs is long and a few elements are returned often). The cache must be a Racket vector of length n
;that is created by the call to cached-assoc (use Racket library function vector or make-vector) and
;used-and-possibly-mutated each time the function returned by cached-assoc is called. Assume n is
;positive.
;The cache starts empty (all elements #f). When the function returned by cached-assoc is called, it
;rst checks the cache for the answer. If it is not there, it uses assoc and xs to get the answer and if
;the result is not #f (i.e., xs has a pair that matches), it adds the pair to the cache before returning
;(using vector-set!). The cache slots are used in a round-robin fashion: the rst time a pair is added
;to the cache it is put in position 0, the next pair is put in position 1, etc. up to position n 􀀀 1 and
;then back to position 0 (replacing the pair already there), then position 1, etc.
;Hints:
; In addition to a variable for holding the vector whose contents you mutate with vector-set!,
;use a second variable to keep track of which cache slot will be replaced next. After modifying the
;cache, increment this variable (with set!) or set it back to 0.
; To test your cache, it can be useful to add print expressions so you know when you are using the
;cache and when you are not. But remove these print expressions before submitting your code.
; Sample solution is 15 lines.

(define (cached-assoc xs n)
  (letrec ([rez (make-vector n #f)]
           [index -1]
           [get-next-index (lambda () (if (= index n)
                                          (begin (set! index 0) index)
                                          (begin (set! index (+ index 1)) index)))]
           [get-stored-rez (lambda(v c)
                             (cond
                               [(= c n) #f]
                               [(equal? (vector-take rez c) v) c]
                               [#t (get-stored-rez v (+ c 1))]))]           
           [f (lambda (v)
                (let ([stored-rez (get-stored-rez v 0)])
                  (if stored-rez
                      stored-rez
                      (let ([calc-rez (assoc v xs)])
                        (if calc-rez
                          (begin
                            (vector-set! rez (get-next-index) calc-rez)
                            calc-rez)
                          calc-rez)))))])
    f))
;;; 5 pts