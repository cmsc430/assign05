#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?)                  (Int s)]
    [(? boolean?)                  (Bool s)]
    [(? char?)                     (Char s)]
    ;; Provided for you:
    [(? string?)                   (String s)]
    ['eof                          (Eof)]
    [(? symbol?)                   (Var s)]
    [(list 'quote (list))          (Empty)]
    [(list (? (op? op0) p0))       (Prim0 p0)]           
    [(list (? (op? op1) p1) e)     (Prim1 p1 (parse e))]
    [(list (? (op? op2) p2) e1 e2) (Prim2 p2 (parse e1) (parse e2))]
    [(list 'begin e1 e2)
     (Begin (parse e1) (parse e2))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse e1) (parse e2))]
    [_ (error "Parse error" s)]))

(define op0
  '(read-byte peek-byte void))

(define op1
  '(add1 sub1 zero? char? write-byte eof-object?
         integer->char char->integer box unbox empty? car cdr
         ;; Provided for you:
         string? string-length
         ))
(define op2
  '(+ - eq? cons
      ;; Provided for you:
      string-ref make-string
      ))

(define (op? ops)
  (λ (x)
    (and (symbol? x)
         (memq x ops))))
