
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Problem 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


;; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))


;; Problem 3
(define (list-nth-mod xs n)
  (define get-nth (lambda (xs n) (if (= 0 n) (car xs) (get-nth (cdr xs) (- n 1)))))
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (get-nth xs (remainder n (length xs)))]))


;; Problem 4
(define (stream-for-n-steps s n)
  (if (= n 0)
    null
    (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))


;; Problem 5
(define funny-number-stream
  (letrec ([f (lambda(x) (cons (if (= (remainder x 5) 0) (- 0 x) x)
    (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))


;; Problem 6
(define dan-then-dog
  (letrec ([next_dan (lambda () (cons "dan.jpg" (lambda () (next_dog))))]
           [next_dog (lambda () (cons "dog.jpg" (lambda () (next_dan))))])
  (lambda () (next_dan))))


;; Problem 7
(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s))) (lambda () ((stream-add-zero (cdr (s))))))))


;; Problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) 
            (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))


;; Problem 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n) 
            (cond [(= n (vector-length vec)) #f]
                  [(pair? (vector-ref vec n)) 
                    (if (equal? v (car (vector-ref vec n)))
                      (vector-ref vec n)
                      (f (+ n 1)))]
                  [#t (f (+ n 1))]))])
    (f 0)))


;; Problem 10
(define (cached-assoc xs n)
  (letrec ([pos 0]
          [cache (make-vector n #f)])
    (lambda (v) 
      (letrec ([search_cache (vector-assoc v cache)])
        (if search_cache search_cache ; in cache
          (letrec ([search_list (assoc v xs)])  ; otherwise
            (if search_list 
              (begin  ; not in cache but in list
                (vector-set! cache pos search_list)
                (if (= pos (- n 1)) (set! pos 0) (set! pos (+ pos 1)))
                search_list)  
              #f)))))))  ; not in list