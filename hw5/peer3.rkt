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

;; Problem 1 - convert between racket and muple lists
(define (racketlist->mupllist lst)
  (if (null? lst) (aunit)
      (apair (car lst) (racketlist->mupllist (cdr lst)))))

(define (mupllist->racketlist lst)
  (if (aunit? lst) null
      (cons (apair-e1 lst) (mupllist->racketlist (apair-e2 lst)))))
;;; 5 pts



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
        [(aunit? e) e]
        [(int? e) e]
        [(closure? e) e]
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (eval-under-env (mlet-body e)
                         (cons (cons (mlet-var e)
                                     (eval-under-env (mlet-e e) env))
                               env))]
        [(call? e)
         (let ([clr (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? clr)
               (let* ([func (closure-fun clr)]
                      [func-name (fun-nameopt func)]
                      [func-body (fun-body func)]
                      [clr-env (append (closure-env clr) env)]
                      [func-arg-env (cons (cons (fun-formal func)
                                                (eval-under-env arg env))
                                          clr-env)]
                      [func-env (if (equal? func-name "#f") func-arg-env
                                    (cons (cons func-name clr)
                                          func-arg-env))])
                 (eval-under-env func-body func-env))
               (error "MUPL call applied to non closure")))]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env)
                (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([p (eval-under-env (fst-e e) env)])
           (if (apair? p) (apair-e1 p)
               (error "MUPL fst applied to non apair")))]
        [(snd? e)
         (let ([p (eval-under-env (snd-e e) env)])
           (if (apair? p) (apair-e2 p)
               (error "MUPL snd applied to non apair")))]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))
;;; 5 pts



;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (letrec ([add-var (lambda (var env)
                      (cons (cons (car var) (eval-under-env (cdr var) env))
                            env))]
           [build-env (lambda (lst env)
                        (if (null? lst) env
                            (build-env (cdr lst) (add-var (car lst) env))))])
    (eval-under-env e2 (build-env lstlst null))))

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y") e4
                         (ifgreater (var "_y") (var "_x") e4
                                    e3)))))
;;; 5 pts


;; Problem 4

(define mupl-map
  (fun #f "map-func"
       (fun "apply-func" "xs"
            (ifgreater (isaunit (var "xs")) (int 0) (aunit)
                       (apair (call (var "map-func") (fst (var "xs")))
                              (call (var "apply-func") (snd (var "xs"))))))))

(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun #f "n" (call (var "map") (fun #f "x" (add (var "x") (var "n")))))))
;;; 5 pts

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; Needed to add this line for racket to understand set functions
(require racket/set)

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (letrec
      ([build-expr
        (lambda (local-vars e)
          (cond [(var? e) (if (set-member? local-vars (var-string e))
                              (cons e (set))
                              (cons e (set (var-string e))))]
                [(int? e) (cons e (set))]
                [(add? e)
                 (let ([e1 (build-expr local-vars (add-e1 e))]
                       [e2 (build-expr local-vars (add-e2 e))])
                   (cons (add (car e1) (car e2))
                         (set-union (cdr e1) (cdr e2))))]
                [(ifgreater? e)
                 (let ([e1 (build-expr local-vars (ifgreater-e1 e))]
                       [e2 (build-expr local-vars (ifgreater-e2 e))]
                       [e3 (build-expr local-vars (ifgreater-e3 e))]
                       [e4 (build-expr local-vars (ifgreater-e4 e))])
                   (cons (ifgreater (car e1) (car e2) (car e3) (car e4))
                         (set-union (cdr e1) (cdr e2) (cdr e3) (cdr e4))))]
                [(fun? e)
                 (let* ([nameopt (fun-nameopt e)]
                        [formal (fun-formal e)]
                        [body (fun-body e)]
                        [local-vars-fname (if (eq? nameopt "#f") local-vars
                                              (set-add local-vars nameopt))]
                        [local-vars-arg (set-add local-vars-fname formal)]
                        [subexpr (build-expr local-vars-arg body)]
                        [newbody (car subexpr)]
                        [free-vars (cdr subexpr)])
                   (cons (fun-challenge nameopt formal newbody free-vars) (set)))]
                [(call? e)
                 (let ([func-expr (build-expr local-vars (call-funexp e))]
                       [arg-expr (build-expr local-vars (call-actual e))])
                   (cons (call (car func-expr) (car arg-expr))
                         (set-union (cdr func-expr) (cdr arg-expr))))]
                [(mlet? e)
                 (let ([expr (build-expr local-vars (mlet-e e))]
                       [body (build-expr (set-add local-vars (mlet-var e))
                                         (mlet-body e))])
                   (cons (mlet (mlet-var e) (car expr) (car body))
                         (set-union (cdr expr) (cdr body))))]
                [(apair? e)
                 (let ([e1 (build-expr local-vars (apair-e1 e))]
                       [e2 (build-expr local-vars (apair-e2 e))])
                   (cons (apair (car e1) (car e2))
                         (set-union (cdr e1) (cdr e2))))]
                [(fst? e)
                 (let ([subexp (build-expr local-vars (fst-e e))])
                   (cons (fst (car subexp)) (cdr subexp)))]
                [(snd? e)
                 (let ([subexp (build-expr local-vars (snd-e e))])
                   (cons (snd (car subexp)) (cdr subexp)))]
                [(aunit? e)
                 (cons (aunit) (set))]
                [(isaunit? e)
                 (let ([subexp (build-expr local-vars (isaunit-e e))])
                   (cons (isaunit (car subexp)) (cdr subexp)))]
                [#t (error (format "bad MUPL expression: ~v" e))]))])
    (car (build-expr (set) e))))

;; Takes a set of variable names and returns their values in the env
(define (envlookup-vars env var-names)
  (letrec
      ([lookup (lambda (var-names var-vals)
                 (if (null? var-names) var-vals
                     (lookup (cons (envlookup env (car var-names)) var-vals)
                             (cdr var-names))))])
    (lookup (set->list var-names) null)))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(aunit? e) e]
        [(int? e) e]
        [(closure? e) e]
        [(fun-challenge? e)
         (closure (envlookup-vars env (fun-challenge-freevars e)) e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (eval-under-env-c (mlet-body e)
                         (cons (cons (mlet-var e)
                                     (eval-under-env-c (mlet-e e) env))
                               env))]
        [(call? e)
         (let ([clr (eval-under-env-c (call-funexp e) env)]
               [arg (eval-under-env-c (call-actual e) env)])
           (if (closure? clr)
               (let* ([func (closure-fun clr)]
                      [func-name (fun-challenge-nameopt func)]
                      [func-body (fun-challenge-body func)]
                      [clr-env (append (closure-env clr) env)]
                      [func-arg-env (cons (cons (fun-challenge-formal func)
                                                (eval-under-env-c arg env))
                                          clr-env)]
                      [func-env (if (equal? func-name "#f") func-arg-env
                                    (cons (cons func-name clr)
                                          func-arg-env))])
                 (eval-under-env-c func-body func-env))
               (error "MUPL call applied to non closure")))]
        [(apair? e)
         (apair (eval-under-env-c (apair-e1 e) env)
                (eval-under-env-c (apair-e2 e) env))]
        [(fst? e)
         (let ([p (eval-under-env-c (fst-e e) env)])
           (if (apair? p) (apair-e1 p)
               (error "MUPL fst applied to non apair")))]
        [(snd? e)
         (let ([p (eval-under-env-c (snd-e e) env)])
           (if (apair? p) (apair-e2 p)
               (error "MUPL snd applied to non apair")))]
        [(isaunit? e)
         (if (aunit? (eval-under-env-c (isaunit-e e) env)) (int 1) (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
