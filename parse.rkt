#lang racket
(provide parse)

; type Token =
; | Integer
; | Char
; | Boolean
; | String
; | `(variable ,Variable)
; | `(keyword ,Keyword)
; | `(prim1 ,Prim1)
; | `(prim2 ,Prim2)
; | 'lparen    ;; (
; | 'rparen    ;; )
; | 'lsquare   ;; [
; | 'rsquare   ;; ]
; | 'eof       ;; end of file

; type Variable = Symbol (other than 'let, 'cond, etc.)

; type Keyword =
; | 'let
; | 'cond
; | 'else
; | 'if

; type Prim1 =
; | 'add1
; | 'sub1
; | 'zero?
; | 'abs
; | '-
; | 'integer->char
; | 'char->integer
; | 'char?
; | 'boolean?
; | 'integer?
; | 'string?
; | 'box?
; | 'empty?
; | 'cons?
; | 'box
; | 'unbox
; | 'car
; | 'cdr
; | 'string-length

; type Prim2 =
; | 'make-string
; | 'string-ref
; | '=
; | '<
; | '<=
; | 'char=?
; | 'boolean=?
; | '+
; | '-

;; (Listof Token) -> Expr
(define (parse lot)
  0
  ;; Select whichever parser you want to start from
  #;
  (parse-imperative lot)
  #;
  (parse-functional lot))

;; Any -> Boolean
(define (prim1? p)
  (match p
    [`(prim1 ,_) #t]
    [_ #f]))

;; Any -> Boolean
(define (prim2? p)
  (match p
    [`(prim2 ,_) #t]
    [_ #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imperative approach

(define *input* (box '()))

(define (parse-imperative lot)
  (set-box! *input* lot)
  (let ((e (parse-expr!))
        (_ (match-tok! 'eof)))
    e))

;; -> Token
(define (look-ahead)
  (match (unbox *input*)
    ['() (error "no look-ahead")]
    [(cons x _) x]))

;; -> Token
(define (look-ahead2)
  (match (unbox *input*)
    ['() (error "no look-ahead")]
    [(cons _ '()) (error "no look-ahead 2")]
    [(cons _ (cons x _)) x]))

;; Token -> Token
(define (match-tok! t)
  (match (unbox *input*)
    ['() (error "no token")]
    [(cons x xs)
     (if (equal? t x)
         (begin (set-box! *input* xs) x)
         (error "parse error"))]))

;; -> Expr
(define (parse-expr!)
  (match (look-ahead)
    [(? integer? i) (match-tok! i)]
    ['lparen
     (let ((lp (match-tok! 'lparen))
           (e  (parse-compound!))
           (rp (match-tok! 'rparen)))
       e)]
    ['lsquare
     (let ((lp (match-tok! 'lsquare))
           (e  (parse-compound!))
           (rp (match-tok! 'rsquare)))
       e)]))

;; -> Expr
(define (parse-compound!)
  (match (look-ahead)
    [(? prim1? p)
     (let ((p (match-tok! p))
           (e (parse-expr!)))
       (match p
         [`(prim1 ,p) `(,p ,e)]))]    
    [(? prim2? p)
     (let ((p (match-tok! p))
           (e0 (parse-expr!))
           (e1 (parse-expr!)))
       (match p
         [`(prim2 ,p) `(,p ,e0 ,e1)]))]
    ['(keyword if)
     (let ((if (match-tok! '(keyword if)))
           (q (parse-question!))
           (e1 (parse-expr!))
           (e2 (parse-expr!)))
       `(if ,q ,e1 ,e2))]
    ['(keyword cond)
     (let ((c (match-tok! '(keyword cond)))
           (cs (parse-clauses!))
           (el (parse-else!)))
       `(cond ,@cs ,el))]))

;; -> (Listof (List (List 'zero? Expr) Expr))
(define (parse-clauses!)
  (match (look-ahead)
    [(or 'lparen 'lsquare)
     (match (look-ahead2)
       ['(keyword else) '()]
       [_
        (let ((c (parse-clause!))
              (cs (parse-clauses!)))
          (cons c cs))])]
    [_ '()]))

;; -> (List (List 'zero? Expr) Expr)
(define (parse-clause!)
  (match (look-ahead)
    ['lparen
     (let ((lp (match-tok! 'lparen))
           (q (parse-question!))
           (a (parse-expr!))
           (rp (match-tok! 'rparen)))
       (list q a))]
    ['lsquare
     (let ((lp (match-tok! 'lsquare))
           (q (parse-question!))
           (a (parse-expr!))
           (rp (match-tok! 'rsquare)))
       (list q a))]))

;; -> (Listof (List 'else Expr))
(define (parse-else!)
  (match (look-ahead)
    ['lparen
     (let ((lp (match-tok! 'lparen))
           (el (match-tok! '(keyword else)))
           (e  (parse-expr!))
           (rp (match-tok! 'rparen)))
       `(else ,e))]
    ['lsquare
     (let ((lp (match-tok! 'lsquare))
           (el (match-tok! '(keyword else)))
           (e  (parse-expr!))
           (rp (match-tok! 'rsquare)))
       `(else ,e))]))

;; -> (List 'zero? Expr)
(define (parse-question!)
  (match (look-ahead)
    ['lparen
     (let ((lp (match-tok! 'lparen))
           (z (match-tok! '(prim zero?)))
           (e (parse-expr!))
           (rp (match-tok! 'rparen)))
       `(zero? ,e))]
    ['lsquare
     (let ((lp (match-tok! 'lsquare))
           (z (match-tok! '(prim zero?)))
           (e (parse-expr!))
           (rp (match-tok! 'rsquare)))
       `(zero? ,e))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functional approach

;; (Listof Token) -> Expr
(define (parse-functional lot)
  (match (parse-expr lot)
    [(cons '(eof) e) e]
    [_ (error "parse error")]))

;; (Listof Token) -> (Pairof (Listof Token) Expr)
(define (parse-expr lot)
  (match lot
    [(cons (? integer? i) lot)
     (cons lot i)]
    [(cons 'lparen lot)
     (match (parse-compound lot)
       [(cons (cons 'rparen lot) e)
        (cons lot e)])]
    [(cons 'lsquare lot)
     (match (parse-compound lot)
       [(cons (cons 'rsquare lot) e)
        (cons lot e)])]))

;; (Listof Token) -> (Pairof (Listof Token) Expr)
(define (parse-compound lot)
  (match lot
    [(cons (? prim1? p) lot)
     (match (parse-expr lot)
       [(cons lot e)
        (match p
          [`(prim1 ,p)
           (cons lot (list p e))])])]
    [(cons (? prim2? p) lot)
     (match (parse-expr lot)
       [(cons lot e0)
        (match (parse-expr lot)
          [(cons lot e1)
           (match p
             [`(prim1 ,p)
              (cons lot (list p e0 e1))])])])]    
    [(cons '(keyword if) lot)
     (match (parse-question lot)
       [(cons lot q)
        (match (parse-expr lot)
          [(cons lot e1)
           (match (parse-expr lot)
             [(cons lot e2)
              (cons lot (list 'if q e1 e2))])])])]
    [(cons '(keyword cond) lot)
     (match (parse-clauses lot)
       [(cons lot cs)
        (match (parse-else lot)
          [(cons lot el)
           (cons lot `(cond ,@cs ,el))])])]))

;; (Listof Token) -> (Pairof (Listof Token) (Listof (List (List 'zero? Expr) Expr)))
;; requires look-ahead of 2
(define (parse-clauses lot)
  (match lot
    [(cons (or 'lparen 'lsquare) (cons '(keyword else) _))
     (cons lot '())]
    [(cons (or 'lparen 'lsquare) _)
     (match (parse-clause lot)
       [(cons lot c)
        (match (parse-clauses lot)
          [(cons lot cs)
           (cons lot (cons c cs))])])]
    [_
     (cons lot '())]))

;; (Listof Token) -> (Pairof (Listof Token) (List (List 'zero? Expr) Expr))
(define (parse-clause lot)
  (match lot
    [(cons 'lparen lot)
     (match (parse-question lot)
       [(cons lot q)
        (match (parse-expr lot)
          [(cons (cons 'rparen lot) e)
           (cons lot (list q e))])])]
    [(cons 'lsquare lot)
     (match (parse-question lot)
       [(cons lot q)
        (match (parse-expr lot)
          [(cons (cons 'rsquare lot) e)
           (cons lot (list q e))])])]))

;; (Listof Token) -> (Pairof (Listof Token) (List 'zero? Expr))
(define (parse-question lot)
  (match lot
    [(cons 'lparen (cons '(prim zero?) lot))
     (match (parse-expr lot)
       [(cons (cons 'rparen lot) e)
        (cons lot (list 'zero? e))])]
    [(cons 'lsquare (cons '(prim zero?) lot))
     (match (parse-expr lot)
       [(cons (cons 'rsquare lot) e)
        (cons lot (list 'zero? e))])]))

;; (Listof Token) -> (Pairof (Listof Token) (List 'else Expr)
(define (parse-else lot)
  (match lot
    [(cons 'lparen (cons '(keyword else) lot))
     (match (parse-expr lot)
       [(cons (cons 'rparen lot) e)
        (cons lot (list 'else e))])]
    [(cons 'lsquare (cons '(keyword else) lot))
     (match (parse-expr lot)
       [(cons (cons 'rsquare lot) e)
        (cons lot (list 'else e))])]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(module+ test
  (require rackunit)
  (require "lex.rkt")
  ;; String -> Expr
  (define (p s)
    (parse (lex-string (string-append "#lang racket " s))))

  (check-equal? (p "7") 7)
  (check-equal? (p "(add1 7)") '(add1 7))
  (check-equal? (p "(sub1 7)") '(sub1 7))
  (check-equal? (p "[add1 7]") '(add1 7))
  (check-equal? (p "[sub1 7]") '(sub1 7))
  (check-equal? (p "(abs 7)") '(abs 7))
  (check-equal? (p "[abs 7]") '(abs 7))  (check-equal? (p "(- 7)") '(- 7))
  (check-equal? (p "[- 7]") '(- 7))
  (check-equal? (p "(cond [else 1])") '(cond [else 1]))
  (check-equal? (p "(cond [(zero? 0) 2] [else 1])")
                '(cond [(zero? 0) 2] [else 1]))
  (check-equal? (p "(cond [(zero? 0) 2] [(zero? 1) 3] [else 1])")
                '(cond [(zero? 0) 2] [(zero? 1) 3] [else 1]))
  (check-equal? (p "(cond [(zero? 0) 2] [(zero? 1) 3] (else 1))")
                '(cond [(zero? 0) 2] [(zero? 1) 3] [else 1]))
  (check-equal? (p "(if (zero? 9) 1 2)")
                '(if (zero? 9) 1 2))
  (check-equal? (p "(+ 1 2)") '(+ 1 2))
  (check-equal? (p "(char=? #\\a #\\b)") '(char=? #\a #\b))
  ;; TODO: add more tests
  #;...)
