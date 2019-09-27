#lang racket
;; type Expr =
;; | Integer
;; | Boolean
;; | Character
;; | Variable
;; | `(,Prim ,Expr)
;; | `(if ,Expr ,Expr ,Expr)
;; | `(cond ,@Clauses [else ,Expr])
;; | `(let ,Bindings ,Expr)

;; type Prim =
;; | 'add1 | 'sub1 | 'zero? | 'abs | '- | 'char? | 'integer? | 'boolean?
;; | 'integer->char | 'char->integer

;; type Clauses = (Listof `(,Expr ,Expr))
;; type Bindings = (Listof `(,Variable ,Expr))

;; type Variable = Symbol (except 'let, 'add1, etc.)
