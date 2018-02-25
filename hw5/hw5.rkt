;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
;; CHANGE (put your solutions here)
(define (racketlist->mupllist racketlist)   
  (cond [(null? racketlist) (aunit)]
        [(list? (cdr racketlist)) 
          (apair (car racketlist) (racketlist->mupllist (cdr racketlist)))]
        [#t (error "racketlist->mupllist applied to non-list")]))

(define (mupllist->racketlist mupllist)
  (cond [(aunit? mupllist) '()]
        [(apair? mupllist) 
          (cons (apair-e1 mupllist) (mupllist->racketlist (apair-e2 mupllist)))]
        [#t (error "mupllist->racketlist applied to non-list")]))



;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
          (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e)
          e]
        [(ifgreater? e)
          (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
            (if (and (int? v1) (int? v2))
              (if (> (int-num v1) (int-num v2)) 
                (eval-under-env (ifgreater-e3 e) env)
                (eval-under-env (ifgreater-e4 e) env))
              (error "ifgreater applied to non-number")))]
        [(fun? e)
          (closure env e)]
        [(call? e)
          (let ([clr (eval-under-env (call-funexp e) env)]
                [arg (eval-under-env (call-actual e) env)])
            (if (closure? clr) 
              ; Evaluate closure's fun body in closure's env
              (eval-under-env 
                (fun-body (closure-fun clr))
                (if (fun-nameopt (closure-fun clr))
                  (cons (cons (fun-nameopt (closure-fun clr)) clr)
                    (cons (cons (fun-formal (closure-fun clr)) arg) (closure-env clr)))
                  (cons (cons (fun-formal (closure-fun clr)) arg) (closure-env clr))))
              (error "MUPL call applied to non-function")))]
        [(mlet? e)
          (let ([v (eval-under-env (mlet-e e) env)])
            (eval-under-env (mlet-body e) 
              (cons (cons (mlet-var e) v) env)))]
        [(apair? e)
          (apair (eval-under-env (apair-e1 e) env) 
                 (eval-under-env (apair-e2 e) env))]
        [(fst? e)
          (let ([v (eval-under-env (fst-e e) env)]) 
            (if (apair? v) 
              (apair-e1 v) 
              (error "MUPL fst applied to non-apair")))]
        [(snd? e)
          (let ([v (eval-under-env (snd-e e) env)]) 
            (if (apair? v)
              (apair-e2 v) 
              (error "MUPL snd applied to non-apair")))]
        [(aunit? e)
          e]
        [(isaunit? e)
          (let ([v (eval-under-env (isaunit-e e) env)]) 
            (if (aunit? v) (int 1) (int 0)))]
        [(closure? e)
          e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3
(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
    e2
    (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1 (mlet "_y" e2 
    (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3)))))

;; Problem 4

(define mupl-map 
  (fun #f "f"
    (fun "g" "list"
      (ifaunit (var "list") 
        (var "list")
        (apair (call (var "f") (fst (var "list"))) (call (var "g") (snd (var "list"))))))))


(define mupl-mapAddN
  (mlet "map" mupl-map
    (fun #f "i"
      (fun "f" "list"
        (call (call (var "map") (fun #f "x" (add (var "x") (var "i"))))
          (var "list"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))