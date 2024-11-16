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
    [(plus left right) (primitive-add (calc left) (calc right))]
    [(conditional test if-true if-false) (if (truthy? (calc test)) (calc if-true) (calc if-false))] 
  )
)

;; truthy?: Value -> Boolean
;; PARTIAL: error is signalled unless 'v' is a 'v-bool'
;; Answer #t if 'v' denotes a "true" value, and #f if 'v' denotes a "false" value
(define (truthy? v)
  (match v
    [(v-bool b) b]
    [(v-num _) (error 'truthy? "Expected boolean: ~v" v)]
  )
)

;; primitive-add: Value Value -> Value
;; PARTIAL: error is signalled on non-numeric inputs
;; Add 'a' and 'b' together
(define (primitive-add a b)
  (v-num (+ (ensure-number a) (ensure-number b)))  
)

;; ensure number: Value -> Number
;; PARTIAL: error is signalled on non-'v-num'
;; Extract numeric value from general value
(define (ensure-number x)
  (match x
    [(v-num n) n]
    [(v-bool _) (error 'ensure-number "Expected number: ~v" x)]
  )
)


(module+ test
  (check-equal? (calc (num 0)) (v-num 0))
  (check-equal? (calc (num -5.6)) (v-num -5.6))
  (check-equal? (calc (plus (num 3) (num 4))) (v-num 7))
  (check-equal? (calc (plus (num 1) (plus (num 2) (num 3)))) (v-num 6))
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


;; run: S-Expression -> Value
;; Parses and then calculates.
;; Partil: Parsing may fail; interpretation may fail.
(define (run s)
  (calc (parse-exp s))
)

(module+ test
  (check-equal? (run 0) (v-num 0))
  (check-equal? (run #t) (v-bool #t))
  (check-equal? (run -5.6) (v-num -5.6))
  (check-equal? (run `{+ 3 4}) (v-num 7))
  (check-equal? (run `{+ 1 {+ 2 3}}) (v-num 6))
  (check-equal? (run `{if #t 1 2}) (v-num 1))
  (check-equal? (run `{if #f 1 2}) (v-num 2))
  ;; Let's try some negative tests: valid S-expressions, not parseable into `Exp`s.
  (check-exn #px"Parse error" (lambda () (run `{1 + 2})))
  (check-exn #px"Parse error" (lambda () (run `{if #t then 1 else 2})))
)