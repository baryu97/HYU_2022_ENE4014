#lang racket
(provide(all-defined-out))

(define (check_bst x)
  (if (null? x)
      #t
      (let ((right (car (cdr (cdr x)))) (left (car (cdr x))))
      (if (null? right)
          (if (null? left)
              #t
              (if (< (car left) (car x))
                  (and (check_bst left) (check_bst right))
                       #f))
          (if (null? left)
              (if (< (car x) (car right))
                  (and (check_bst left) (check_bst right))
                       #f)
              (if (and (< (car x) (car right)) (< (car left) (car x)))
                  (and (check_bst left) (check_bst right))
                  #f))))))

(define (apply f x)
  (if (null? x)
      '()
      (let ((right (car (cdr (cdr x)))) (left (car (cdr x))))
        (cons (f (car x)) (cons (apply f left) (cons (apply f right) null))))))

(define (append xs ys)
(if (null? xs)
    ys
    (cons (car xs) (append (cdr xs) ys))))

(define (inorder x)
  (if (null? x)
      '()
      (append (inorder (car(cdr (cdr x)))) (cons (car x) (inorder (car (cdr x)))) )))


(define (same x y)
  (if (null? x)
      (if (null? y)
          #t
          #f)
      (if (= (car x) (car y))
          (same (cdr x) (cdr y))
          #f)))

(define (equals x y)
  (same (inorder x) (inorder y)))