#lang racket
(provide (all-defined-out))

(require "ast.rkt")

; type Token =
; | Integer
; | Char
; | Boolean
; | String
; | '()
; | `(variable ,Variable)
; | `(keyword ,Keyword)
; | `(prim ,Prim)
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

; type Prim = Prim1 | Prim2 | '-

; type Prim1 =
; | 'add1
; | 'sub1
; | 'zero?
; | 'abs
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
; | 'cons
; | 'make-string
; | 'string-ref
; | '=
; | '<
; | '<=
; | 'char=?
; | 'boolean=?
; | '+

;; (Listof Token) -> Expr
(define (parse lot)
  (match (parse-expr lot)
    [(cons '(eof) e) e]
    [_ (error "parse error")]))

;; Any -> Boolean
(define (prim1? p)
  (match p
    [`(prim ,n)
     (and (memq n '(add1 sub1 zero? abs integer->char char->integer char?
                         boolean? integer? string? box? empty? cons?
                         box unbox car cdr string-length))
          #t)]

    [_ #f]))

;; Any -> Boolean
(define (prim2? p)
  (match p
    [`(prim ,n)
     (and (memq n '(make-string string-ref = < <= char=? boolean=? + cons))
          #t)]
    [_ #f]))

;; (Listof Token) -> (Pairof (Listof Token) Expr)
(define (parse-expr lot)
  (match lot
    [(cons '() lot)
     (cons lot (nil-e))]
    [(cons (? integer? i) lot)
     (cons lot (int-e i))]
    [(cons (? char? c) lot)
     (cons lot (char-e c))]
    [(cons (? boolean? b) lot)
     (cons lot (bool-e b))]
    [(cons (? string? s) lot)
     (cons lot (string-e s))]
    [(cons `(variable ,x) lot)
     (cons lot (var-e x))]
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
          [`(prim ,p)
           (cons lot (prim-e p (list e)))])])]
    [(cons (? prim2? p) lot)
     (match (parse-expr lot)
       [(cons lot e0)
        (match (parse-expr lot)
          [(cons lot e1)
           (match p
             [`(prim ,p)
              (cons lot (prim-e p (list e0 e1)))])])])]
    [(cons '(prim -) lot)
     (match (parse-expr lot)
       [(cons lot e0)
        (match (parse-maybe-expr lot)
          [(cons lot #f)
           (cons lot (prim-e '- (list e0)))]
          [(cons lot e1)
           (cons lot (prim-e '- (list e0 e1)))])])]
    [(cons '(keyword if) lot)
     (match (parse-expr lot)
       [(cons lot q)
        (match (parse-expr lot)
          [(cons lot e1)
           (match (parse-expr lot)
             [(cons lot e2)
              (cons lot (if-e q e1 e2))])])])]
    [(cons '(keyword cond) lot)
     (match (parse-clauses lot)
       [(cons lot cs)
        (match (parse-else lot)
          [(cons lot el)
           (cons lot (cond-e cs el))])])]
    [(cons '(keyword let) lot)
     (match (parse-bindings lot)
       [(cons lot bs)
        (match (parse-expr lot)
          [(cons lot e)
           (cons lot (let-e bs e))])])]))

(define (parse-maybe-expr lot)
  (match lot
    ['() (cons '() #f)]
    [(cons (or 'rparen 'rsquare) _)
     (cons lot #f)]
    [_ (parse-expr lot)]))

(define (parse-bindings lot)
  (match lot
    [(cons 'lparen lot)
     (match (parse-binding-list lot)
       [(cons (cons 'rparen lot) bs) (cons lot bs)])]
    [(cons 'lsquare lot)
     (match (parse-binding-list lot)
       [(cons (cons 'rsquare lot) bs) (cons lot bs)])]))

(define (parse-binding-list lot)
  (match lot
    [(cons (or 'lparen 'lsquare) _)
     (match (parse-binding lot)
       [(cons lot b)
        (match (parse-binding-list lot)
          [(cons lot bs)
           (cons lot (cons b bs))])])]
    [_ (cons lot '())]))

(define (parse-binding lot)
  (match lot
    [(cons 'lparen (cons (? variable? x) lot))
     (match (parse-expr lot)
       [(cons (cons 'rparen lot) e)
        (match x
          [`(variable ,x)
           (cons lot (binding x e))])])]
    [(cons 'lsquare (cons (? variable? x) lot))
     (match (parse-expr lot)
       [(cons (cons 'rsquare lot) e)
        (match x
          [`(variable ,x)
           (cons lot (binding x e))])])]))

;; (Listof Token) -> (Pairof (Listof Token) (Listof (List Expr Expr)))
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

;; (Listof Token) -> (Pairof (Listof Token) (List Expr Expr))
(define (parse-clause lot)
  (match lot
    [(cons 'lparen lot)
     (match (parse-expr lot)
       [(cons lot q)
        (match (parse-expr lot)
          [(cons (cons 'rparen lot) e)
           (cons lot (clause q e))])])]
    [(cons 'lsquare lot)
     (match (parse-expr lot)
       [(cons lot q)
        (match (parse-expr lot)
          [(cons (cons 'rsquare lot) e)
           (cons lot (clause q e))])])]))

;; (Listof Token) -> (Pairof (Listof Token) (List 'else Expr)
(define (parse-else lot)
  (match lot
    [(cons 'lparen (cons '(keyword else) lot))
     (match (parse-expr lot)
       [(cons (cons 'rparen lot) e)
        (cons lot e)])]
    [(cons 'lsquare (cons '(keyword else) lot))
     (match (parse-expr lot)
       [(cons (cons 'rsquare lot) e)
        (cons lot e)])]))

(define (variable? x)
  (match x
    [`(variable ,_) #t]
    [_ #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(module+ test
  (require rackunit)
  (require "lex.rkt")
  (require "syntax.rkt")
  ;; String -> Expr
  (define (p s)
    (parse (lex-string (string-append "#lang racket " s))))

  (check-equal? (p "7")               (sexpr->ast 7))
  (check-equal? (p "(add1 7)")        (sexpr->ast '(add1 7)))
  (check-equal? (p "(sub1 7)")        (sexpr->ast '(sub1 7)))
  (check-equal? (p "[add1 7]")        (sexpr->ast '(add1 7)))
  (check-equal? (p "[sub1 7]")        (sexpr->ast '(sub1 7)))
  (check-equal? (p "(abs 7)")         (sexpr->ast '(abs 7)))
  (check-equal? (p "[abs 7]")         (sexpr->ast '(abs 7)))
  (check-equal? (p "(- 7)")           (sexpr->ast '(- 7)))
  (check-equal? (p "[- 7]")           (sexpr->ast '(- 7)))
  (check-equal? (p "(cond [else 1])") (sexpr->ast '(cond [else 1])))
  (check-equal? (p "(cond [(zero? 0) 2] [else 1])")
                (sexpr->ast '(cond [(zero? 0) 2] [else 1])))
  (check-equal? (p "(cond [(zero? 0) 2] [(zero? 1) 3] [else 1])")
                (sexpr->ast '(cond [(zero? 0) 2] [(zero? 1) 3] [else 1])))
  (check-equal? (p "(cond [(zero? 0) 2] [(zero? 1) 3] (else 1))")
                (sexpr->ast '(cond [(zero? 0) 2] [(zero? 1) 3] [else 1])))
  (check-equal? (p "(if (zero? 9) 1 2)")
                (sexpr->ast '(if (zero? 9) 1 2)))
  (check-equal? (p "(+ 1 2)") (sexpr->ast '(+ 1 2)))
  (check-equal? (p "(- 1 2)") (sexpr->ast '(- 1 2)))
  (check-equal? (p "(char=? #\\a #\\b)") (sexpr->ast '(char=? #\a #\b)))
  ;; TODO: add more tests
  #;...)
