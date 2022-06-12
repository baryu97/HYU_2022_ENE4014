#lang racket
(provide (all-defined-out)) ;; exports the defined variables in this file.

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

;; Definitions for extra requirements should be here.


;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Extra requirement

(struct glet (var e body) #:transparent) ;; a global binding that overrides any local binding (similar to the following ML code: let var = e in body)

(struct num-array  (size) #:transparent)  ;; a number array  (initialized to zeroes), e.g., (num-array-var 10)
                                                     ;; e.g. (num-array 4)

(struct num-array-at   (e1 e2) #:transparent) ;; e1 evaluates to num-array and e2 evaluates to racket int (index of the value to access) index starts from 0
                                              ;; (num-array-at (num-array 4) 3)
                                              ;; (num-array-at (num-array 4) 4) ;  this should give a nice error message (like "array access out of bound")
                                              ;; (num-array-at (num-array 4) -1) ;  this should give a nice error message (like "array access out of bound")

(struct num-array-set  (e1 e2 e3) #:transparent) ;; e1 evaluates to a num-array value, e2 evaluates to racket int (index of the value to access), and e3 evaluates to a MUPL int
                                              ;; (num-array-set (num-array 4) 0 (int 42))
                                              ;; (num-array-set (num-array 4) 5 (int 42)) ; this should give a nice error message (like "array access out of bound")
                                              ;; (num-array-set (num-array 4) -1 (int 42)) ; this should give a nice error message (like "array access out of bound")

(define (num-array-object? v) ;; hackish implementation for num-array object testing. We assume that if a value is mpair, it is a num-array object.
  (mpair? v))

(define (array-length array)
  (if (eq? (mcdr array) null)
      1
      (+ 1 (array-length (mcdr array)))))

(define (make-array-object length)  
    (if (= length 0)
        null
        (mcons (int 0) (make-array-object (- length 1)))))

(define (set-array-val array index val)
  (if (= index 0)
      (set-mcar! array val)
      (set-array-val (mcdr array) (- index 1) val)))


;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist xs)
  ;(if (null? xs)
   ;   (aunit)
    ;  (apair (first xs) (racketlist->mupllist (list-tail xs 1)))))
  (cond [(null? xs) (aunit)]
        [(pair? xs) (apair (car xs) (racketlist->mupllist (cdr xs)))]))
  
(define (mupllist->racketlist ml)  
  (cond [(aunit? ml) '()]
        [(apair? ml) (cons (apair-e1 ml) (mupllist->racketlist (apair-e2 ml)))]))

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
        [(int? e)
         e]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
	       [v2 (eval-under-env (call-actual e) env)])
	   (if (closure? v1) 
	       (eval-under-env (fun-body (closure-fun v1))
			       (append (closure-env v1)
				       (list (cons (fun-formal (closure-fun v1)) v2)
                                             (if (fun-nameopt (closure-fun v1))
                                                 (cons (fun-nameopt (closure-fun v1)) v1)
                                                 '()))))
	       (error "MUPL call error")))]
        [(fun? e)
         (closure env e)
         ]
        [(mlet? e)
         (let ([v1 (mlet-var e)][v2 (eval-under-env (mlet-e e) env)][v3 (mlet-body e)])
         (eval-under-env v3 (append (list (cons v1 (eval-under-env v2 env))) env)))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))
         ]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (cond [(apair? v) (apair-e1 v)]
                 [#t (error "MUPL fst error")]))
         ]
        [(snd? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (cond [(apair? v) (apair-e2 v)]
                 [#t (error "MUPL snd error")]))
         ]
        [(aunit? e)
         e
         ]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))
         ]
        [(closure? e) e]

        ;; Extra requirements
        [(glet? e)
         (let ([v1 (glet-var e)][v2 (eval-under-env (glet-e e) env)][v3 (glet-body e)])
          (local [(define (glet-help env1 var val)
                    (cond
                      [(null? env1) (list (cons v1 val))]
                      [(equal? var (car (car env1))) (cons (cons (car (car env1)) val) (cdr env1))]
                      [(closure? (cdr (car env1)))
                       (cons (cons (car (car env1))
                                   (closure (glet-help (closure-env (cdr (car env1))) var val) (closure-fun (cdr (car env1)))))
                             (glet-help (cdr env1) var val))]
                      [#t (cons (car env1) (glet-help (cdr env1) var val))]))]
            (eval-under-env v3 (glet-help env v1 v2))))]
        [(num-array? e)
         (if (>(num-array-size)0)
             (make-array-object (num-array-size e))
             (error "array access out of bound"))]
        [(num-array-at? e) (let ((v1 (eval-under-env (num-array-at-e1) env))(v2 (num-array-at-e2)))
                             (cond
                               [(not (num-array-object? v1)) (error "not a num-array")]
                               [(or (< v2 0) (>= v2 (array-length v1))) (error "array access out of bound")]
                               [(= v2 0) (mcar v1)]
                               [#t (eval-under-env (num-array-at (mcdr v1) (- v2 1)) env)]))]                               
        [(num-array-set? e) (let ((v1 (eval-under-env (num-array-set-e1) env))(v2 (num-array-set-e2))(v3 (eval-under-env (num-array-set-e3) env)))
                              (cond
                                 [(not (num-array-object? v1)) (error "not a num-array")]
                                 [(or (< v2 0) (>= v2 (array-length v1))) (error "array access out of bound")]
                                 [#t (set-array-val v1 v2 v3) (eval-under-env (num-array-at v1 v2) env)]))]
        
        [#t (error (format "bad MUPL expression: ~v" e))]))

(define exp1 (add (int 3) (int 4)))
(eval-under-env exp1 null) ; this should return (int 7)
(define exp2 (add (var "x") (var "y")))
(eval-under-env exp2 (list (cons "x" (int 3)) (cons "y" (int 4)))) ; this should return (int 7)


;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0)
             e2
             e3))

(define (mlet* lstlst e2)
  (if (null? lstlst) e2
      (let ([v (car lstlst)])
        (mlet (car v) (cdr v) (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y")
                    e4
                    (ifgreater (var "_y") (var "_x")
                               e4
                               e3))))


;; Problem 4

(define mupl-map
  (fun "mupl-map-f" "f"
       (fun "mupl-map-lst" "lst" 
            (ifeq (aunit? (var "lst")) (int 1)
                  (aunit)
                  (apair (call (var "f") (fst (var "lst")))
                         (call (var "mupl-map-lst")(snd (var "lst"))))))))

(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun "mapAddI" "i"
             (fun "mapAddLst" "args"
                  (call (call (var "map") (fun "mapAdd" "elm" (add (var "elm") (var "i")))) (var "args"))))))

