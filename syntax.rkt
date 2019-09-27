#lang racket
(provide (all-defined-out))

;; Any -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [(? integer? i) #t]
    [(? boolean? b) #t]
    [(? char? c) #t]
    [(? string? s) #t]
    [`(if ,x ,y ,z)
     (and (expr? x)
          (expr? y)
          (expr? z))]
    [`(,(? prim1?) ,x) (expr? x)]
    [`(,(? prim2?) ,x ,y) (and (expr? x) (expr? y))]
    [(list 'cond `(,xs ,ys) ... `(else ,z))
     (and (andmap expr? xs)
          (andmap expr? ys)
          (expr? z))]
    ;; TODO
    ;; ...
    [_ #f]))

;; Expr -> Boolean
;; Is e a closed expression?
(define (closed? e)
  ;; TODO
  #f)


;; Any -> Boolean
;; Is x a unary primitive?
(define (prim1? x)
  (and (symbol? x)
       (memq x '(add1 sub1 abs - integer->char char->integer
                      car cdr length                      
                      char? integer? boolean? zero?))))

;; Any -> Boolean
;; Is x a binary primitive?
(define (prim2? x)
  (and (symbol? x)
       (memq x '(+
                 cons
                 -))))




