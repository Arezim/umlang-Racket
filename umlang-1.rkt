#lang racket
(require rackunit)

;; A Program is an Expression.

;; An Exp denotes an expression in the "arithmetic" language in the
;; Evaluation Arithmetic chapter of PLAI. It is one of:

;; - a (ref Symbol), stands for a variable reference
;; - a (let1 Symbol Exp Exp), makes 'name' available in 'body'
;; - a (num Number), stands for a literal number
;; - a (bool Boolean), stands for a literal boolean
;; - a (plus Exp Exp), denotes addition of the results of two expressions
;; - a (conditional Exp Exp Exp), an if/then/else expression
;; - a (fn Symbol Exp), makes a one-argument function
;; - a (call Exp Exp), makes a call to 'f' with argument 'arg'

(struct ref (name) #:transparent)
(struct let1 (name init body) #:transparent)
(struct num (n) #:transparent)
(struct bool (b) #:transparent)
(struct plus (l r) #:transparent)
(struct conditional (test if-true if-false) #:transparent)
(struct fn (formal-arg body) #:transparent)
(struct call (f arg) #:transparent)


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
;; - a (v-fn Symbol Exp), a function value

(struct v-num (n) #:transparent)
(struct v-bool (b) #:transparent)
(struct v-fn (formal body) #:transparent)

;; An Env denotes an *Environment*, mapping names (Symbols) to Values.
;; It is a ListOF<pair Symbol Value>
(struct pair (name Value) #:transparent) 

;; empty-env : Env
(define empty-env '())

;; extend-env: Symbol Value Env -> Env
(define (extended-env name value env)
  (cons (pair name value) env)
)

;; lookup: Env Symbol -> Value
(define (lookup env name)
  (match env
    ['() (error 'lookup "Unbound variable: ~a" name)]
    [(cons (pair n v) rest) 
      (if (equal? name n)
        v
        (lookup rest name)
      )
    ]
  )
)

;; calc : Exp Env -> Value
;; Computes the result of the given 'exp'.
(define (calc exp env)
  (match exp
    [(ref name) (lookup env name)]
    [(let1 name init body) (calc body (extended-env name (calc init env) env))]
    [(num n) (v-num n)]
    [(bool b) (v-bool b)]
    [(plus left right) (primitive-add (calc left env) (calc right env))]
    [(conditional test if-true if-false) (if (truthy? (calc test env)) (calc if-true env) (calc if-false env))]
    [(fn formal body) (v-fn formal body)]
    [(call f arg) (let ([function-value (calc f env)]
                        [argument-value (calc arg env)])
                    (match function-value
                      [(v-fn formal body) (calc body (extended-env formal argument-value env))]
                      [_ (error 'calc "Expected function: ~v" function-value)]
                    )
                  )
    ]
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


;; ~read : String -> S-Expression
;; ~parse : S-Expression -> AST

;; parse-exp : S-Expression -> Exp
;; Parse the concrete S-Expression to an instance of Exp, if possible.
;; PARTIAL FUNCTION. Will signal an error on invalid inputs
(define (parse-exp s)
  (match s
    [(list '+ L R) (plus (parse-exp L) (parse-exp R))]
    [(list 'if Test If-True If-False) (conditional (parse-exp Test) (parse-exp If-True) (parse-exp If-False))]
    [(list 'let1 (list Name Init) Body) (let1 Name (parse-exp Init) (parse-exp Body))]
    [(list 'fn (list FORMAL) BODY) (fn FORMAL (parse-exp BODY))]
    [(list F ARG) (call (parse-exp F) (parse-exp ARG))]
    [(? boolean? B) (bool B)]                                   
    [(? number? N) (num N)]
    [(? symbol? S) (ref S)]
    [_ (error 'parse-exp "Parse error: ~v" s)]
  )
)


(module+ test
  (check-equal? (parse-exp `x) (ref 'x))
  (check-equal? (parse-exp 0) (num 0))
  (check-equal? (parse-exp #t) (bool #t))
  (check-equal? (parse-exp -5.6) (num -5.6))
  (check-equal? (parse-exp `{+ 3 4}) (plus (num 3) (num 4)))
  (check-equal? (parse-exp `{+ 1 {+ 2 3}}) (plus (num 1) (plus (num 2) (num 3))))
  (check-equal? (parse-exp `{if #t 1 2}) (conditional (bool #t) (num 1) (num 2)))
  (check-equal? (parse-exp `{let1 {x 123} x}) (let1 'x (num 123) (ref 'x)))
  (check-equal? (parse-exp `{fn {x} {+ x 1}}) (fn 'x (plus (ref 'x) (num 1))))
  (check-equal? (parse-exp `{1 2}) (call (num 1) (num 2))))


;; Parser S-expression syntax examples:

;; {fn {x} {+ x 1}} -> (fn 'x (plus (ref 'x) (num 1)))
;; {f 123} -> (call (ref 'f) (num 123))
;; x -> (ref 'x)
;; {let1 {x 123} x} -> (let1 'x (num 123) (ref 'x))
;; `{+ 1 2} -> (plus 1 2)
;; `{+ 1 {+ 2 3}} -> (plus 1 (plus 2 3))
;; `{if #t 1 2} -> (conditional (bool #t) (num 1) (num 2))
;; #t -> (bool #t)
;; 1 -> (num 1)


;; run: S-Expression -> Value
;; Parses and then calculates.
;; Partil: Parsing may fail; interpretation may fail.
(define (run s)
  (calc (parse-exp s) empty-env)
)


(module+ test
  (check-equal? (run 0) (v-num 0))
  (check-equal? (run #t) (v-bool #t))
  (check-equal? (run -5.6) (v-num -5.6))
  (check-equal? (run `{+ 3 4}) (v-num 7))
  (check-equal? (run `{+ 1 {+ 2 3}}) (v-num 6))
  (check-equal? (run `{if #t 1 2}) (v-num 1))
  (check-equal? (run `{if #f 1 2}) (v-num 2))
  (check-equal? (run `{let1 {x 123} x}) (v-num 123))
  (check-equal? (run `{fn {x} {+ x 1}}) (v-fn 'x (plus (ref 'x) (num 1))))
  (check-equal? (run `{{fn {x} {+ x 1}} 123}) (v-num 124))

  ;; Let's try some negative tests: valid S-expressions, not parseable into `Exp`s.
  (check-exn #px"Parse error" (lambda () (run `{1 + 2})))
  (check-exn #px"Parse error" (lambda () (run `{if #t then 1 else 2})))
  (check-exn #px"Parse error" (lambda () (run `{let1 {x 123 234} x})))
  (check-exn #px"Parse error" (lambda () (run `{fn {x y} x}))))