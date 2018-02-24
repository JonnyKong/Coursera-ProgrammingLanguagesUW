#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
  '()
  (cons low (sequence (+ low stride) high stride))))
;;; 5pts


(define (string-append-map str-list suffix)
  (map (lambda (x) (string-append x suffix)) str-list))
;;; 5pts


(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (empty? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))
;;; 5pts


(define (stream-for-n-steps s n)
  (if (= n 0)
      '()
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))
;;; 5pts


(define funny-number-stream
  (letrec ([f (lambda (x) (if (= 0 (remainder x 5))
                              (cons (- x) (lambda () (f (+ x 1))))
                              (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1) )))
;;; 5pts


(define dan-then-dog
  (letrec ([f (lambda (x) (if (= 0 (remainder x 2))
                              (cons "dan.jpg" (lambda () (f (+ x 1))))
                              (cons "dog.jpg" (lambda () (f (+ x 1))))))])
    (lambda () (f 0) )))
;;; 5pts


(define (stream-add-zero s)
  (letrec ([f (lambda (x) (cons (cons 0 (car (x)))
                                (stream-add-zero (cdr (x)))))])
  (lambda () (f s))))
;;; 5pts


(define (cycle-lists xs ys)
  (letrec ([f (lambda (xs ys n)
                (cons (cons (list-nth-mod xs n)
                            (list-nth-mod ys n))
                      (lambda () (f xs ys (+ n 1)))))])
    (lambda () (f xs ys 0))))
;;; 5pts


(define (vector-assoc v vec)
  (letrec([f (lambda (v vec n)
               (if (> n (vector-length vec))
                   #f
                   (letrec ([value (vector-ref vec n)])
                   (if (pair? value)
                       (if (equal? v (car value))
                           value
                           (f v vec (+ n 1))                           )
                       (f v vec (+ n 1))))))])
    (f v vec 0)))
;;; 5pts



(define (cached-assoc xs n)
  (letrec
      ([cached-vector (make-vector n #f)]
       [index 0]
       [f (lambda (av ac)
            (cond
              [(= ac n) #f]
              [(letrec ([element-in-vector (vector-ref cached-vector ac)])
                 (and (pair? element-in-vector) (= (car element-in-vector) av) element-in-vector))]
              [else (f av (+ ac 1))]))])
    (lambda (v)
      (letrec ([value-in-vector (f v 0)])
        (cond [(not value-in-vector)
               (letrec ([value-in-list (assoc v xs)])
                 (begin (vector-set! cached-vector index value-in-list)
                        (set! index (remainder (+ 1 index) n)) value-in-list))] 
              [else value-in-vector])))))
;;; 5pts



(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([v1 e1]
              [loop (lambda()
                      (if (>= e2 v1)
                          #t
                          (loop)))])
              (loop))]))
  