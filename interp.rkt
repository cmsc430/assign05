#lang racket
(provide (all-defined-out))

(require "ast.rkt")

;; type REnv = (Listof (List Variable Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr REnv -> Answer
(define (interp-env e r)
  (match e
    [(? value? v) (get-val v)]
    [(nil-e) '()]
    [(prim-e (? prim? p) es)
     (let ((as (interp-env* es r)))
       (interp-prim p as))]    
    [(if-e e0 e1 e2)
     (match (interp-env e0 r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(var-e x)
     (lookup r x)]
    [(let-e bs body)
     (match (interp-env* (get-defs bs) r)
       ['err 'err]
       [vs
        (interp-env body (append (zip (get-vars bs) vs) r))])]
    [(cond-e cs el)
     (interp-cond-env cs el r)]))

;; (Listof Expr) REnv -> (Listof Value) | 'err
(define (interp-env* es r)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r)
       ['err 'err]
       [v (cons v (interp-env* es r))])]))

;; (Listof (List Expr Expr)) Expr REnv -> Answer
(define (interp-cond-env cs en r)
  (match cs
    ['() (interp-env en r)]
    [(cons (clause eq ea) cs)
     (match (interp-env eq r)
       ['err 'err]
       [v
        (if v
            (interp-env ea r)
            (interp-cond-env cs en r))])]))

;; Any -> Boolean
(define (value? x)
  (or (int-e? x)
      (bool-e? x)
      (char-e? x)
      (string-e? x)))

;; Prim (Listof Answer) -> Answer
(define (interp-prim p as)
  (match (cons p as)
    [(list '- (? integer? i0)) (- i0)]
    [(list 'abs (? integer? i0)) (abs i0)]
    [(list 'add1 (? integer? i0)) (+ i0 1)]
    [(list 'sub1 (? integer? i0)) (- i0 1)]
    [(list 'zero? (? integer? i0)) (zero? i0)]
    [(list 'char? v0) (char? v0)]
    [(list 'integer? v0) (integer? v0)]
    [(list 'boolean? v0) (boolean? v0)]
    [(list 'integer->char (? codepoint? i0)) (integer->char i0)]
    [(list 'char->integer (? char? c)) (char->integer c)]
    [(list '+ (? integer? i0) (? integer? i1)) (+ i0 i1)]
    [(list 'cons v0 v1) (cons v0 v1)]
    [(list 'car (? cons? v0)) (car v0)]
    [(list 'cdr (? cons? v0)) (cdr v0)]
    [(list 'string? v0) (string? v0)]
    [(list 'box? v0) (box? v0)]
    [(list 'empty? v0) (empty? v0)]
    [(list 'cons? v0) (cons? v0)]
    [(list 'box v0) (box v0)]
    [(list 'unbox (? box? v0)) (unbox v0)]
    [(list 'string-length (? string? v0)) (string-length v0)]
    [(list 'make-string (? char? v0) (? natural? v1)) (make-string v0 v1)]
    [(list 'string-ref (? string? v0) (? natural? v1))
     (if (< v1 (string-length v0))
         (string-ref v0 v1)
         'err)]
    [(list '= (? integer? v0) (? integer? v1)) (= v0 v1)]
    [(list '< (? integer? v0) (? integer? v1)) (< v0 v1)]
    [(list '<= (? integer? v0) (? integer? v1)) (<= v0 v1)]
    [(list 'char=? (? char? v0) (? char? v1)) (char=? v0 v1)]
    [(list 'boolean=? (? boolean? v0) (? boolean? v1)) (boolean=? v0 v1)]    
    [_ 'err]))

;; REnv Variable -> Answer
(define (lookup env x)
  (match env
    ['() 'err]
    [(cons (list y v) env)
     (match (symbol=? x y)
       [#t v]
       [#f (lookup env x)])]))

;; REnv Variable Value -> Value
(define (ext r x v)
  (cons (list x v) r))

;; Any -> Boolean
(define (codepoint? x)
  (and (integer? x)
       (<= 0 x #x10FFFF)
       (not (<= #xD800 x #xDFFF))))

;; (Listof A) (Listof B) -> (Listof (List A B))
(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y) (zip xs ys))]))
