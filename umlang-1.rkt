#lang racket
(require rackunit)

;; A Program is an Expression.

;; An Exp denotes an expression in the "arithmetic" language in the
;; Evaluation Arithmetic chapter of PLAI. It is one of:

;; - a (num Number), stands for a literal number
;; - a (plus Exp Exp), denotes addition of the results of two expressions

(struct num (n) #:transparent) ;; transparent is for
(struct plus (l r) #:transparent)

;; EXAMPLES

;; (num 0)
;; (num -5.6)
;; (plus (num 3) (num 4))
;; (plus (num 1) (plus (num 2) (num 3)))


;; TEMPLATE for Exp

#; (define (F exp)
     (match exp
       [(num n) ... n ...]
       [(plus left right) ... (F left) ... (F right) ...]
      )
    )


;; calc : Exp -> Number
;; Computes the result of the given 'exp'.
(define (calc exp)
  (match exp
    [(num n) n]
    [(plus left right) (+ (calc left) (calc right))]
  )
)

(module+ test
  (check-equal? (calc (num 0)) 0)
  (check-equal? (calc (num -5.6)) -5.6)
  (check-equal? (calc (plus (num 3) (num 4))) 7)
  (check-equal? (calc (plus (num 1) (plus (num 2) (num 3)))) 6))