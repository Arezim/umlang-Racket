#lang racket
(require rackunit)

;; A Program is an Expression.

;; An Exp denotes an expression in the "arithmetic" language in the
;; Evaluation Arithmetic chapter of PLAI. It is one of:

;; - a (num Number), stands for a literal number
;; - a (bool Boolean), stands for a literal boolean
;; - a (plus Exp Exp), denotes addition of the results of two expressions
;; - a (conditional Exp Exp Exp), an if/then/else expression

(struct num (n) #:transparent)
(struct bool (b) #:transparent)
(struct plus (l r) #:transparent)
(struct conditional (test if-true if-false))


;; EXAMPLES

;; (num 0)
;; (num -5.6)
;; (bool #t)
;; (plus (num 3) (num 4))
;; (plus (num 1) (plus (num 2) (num 3)))
;; (conditional (bool #t) (num 1) (num 2))

;; TEMPLATE for Exp

#; (define (F exp)
     (match exp
       [(num n) ... n ...]
       [(plus left right) ... (F left) ... (F right) ...]
      )
    )


;; A Value denotes the result of evaluation of an Exp.
;; It is one of:
;; - a (v-num Number), a numeric result
;; - a (v-bool Boolean), a boolean result

(struct v-num (n) #:transparent)
(struct v-bool (b) #:transparent)


;; calc : Exp -> Value
;; Computes the result of the given 'exp'.
(define (calc exp)
  (match exp
    [(num n) (v-num n)]
    [(bool b) (v-bool b)]
    [(plus left right) (v-num (+ (calc left) (calc right)))]    ;; !!!contract violation - (v-num ...) expected a number, and got another v-num, or - what's even worse - a v-bool! 
                                                                ;; Thtat's why we need a different way to add numerals
    [(conditional test if-true if-false) (if (calc test) (calc if-true) (calc if-false))] 
  )
)

(module+ test
  (check-equal? (calc (num 0)) 0)
  (check-equal? (calc (num -5.6)) -5.6)
  (check-equal? (calc (plus (num 3) (num 4))) 7)
  (check-equal? (calc (plus (num 1) (plus (num 2) (num 3)))) 6)
)

;; parse-exp : S-Expression -> Exp
;; Parse the concrete S-Expression to an instance of Exp, if possible.
;; PARTIAL FUNCTION. Will signal an error on invalid inputs
(define (parse-exp s)
  (match s
    [(list '+ L R) (plus (parse-exp L) (parse-exp R))]
    [(list 'if Test If-True If-False) (conditional (parse-exp Test) (parse-exp If-True) (parse-exp If-False))]
    [(? boolean? B) (bool B)]                                   
    [(? number? N) (num N)]
    [_ (error 'parse-exp "Parse error: ~v" s)]
  )
)


;; Parser S-expression syntax examples:

;; `{+ 1 2} -> (plus 1 2)
;; `{+ 1 {+ 2 3}} -> (plus 1 (plus 2 3))
;; `{if #t 1 2} -> (conditional (bool #t) (num 1) (num 2))
;; #t -> (bool #t)
;; 1 -> (num 1)


(module+ test
  (check-equal? (parse-exp 0) (num 0))
  (check-equal? (parse-exp -5.6) (num -5.6))
  (check-equal? (parse-exp `{+ 3 4}) (plus (num 3) (num 4)))
  (check-equal? (parse-exp `{+ 1 {+ 2 3}}) (plus (num 1) (plus (num 2) (num 3))))
  (check-exn #px"Parse error" (lambda () (parse-exp `{1 + 2})))
)
