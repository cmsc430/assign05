#lang racket
(provide (all-defined-out))

;; type Expr = (Eof)
;;           | (Empty)
;;           | (Int Integer)
;;           | (Bool Boolean)
;;           | (Char Character)
;;           | (String String)
;;           | (Prim0 Op0)
;;           | (Prim1 Op1 Expr)
;;           | (Prim2 Op2 Expr Expr)
;;           | (If Expr Expr Expr)
;;           | (Begin Expr Expr)
;;           | (Let Id Expr Expr)
;;           | (Var Id)
;; type Id   = Symbol
;; type Op0  = 'read-byte
;; type Op1  = 'add1 | 'sub1 | 'zero?
;;           | 'char? | 'integer->char | 'char->integer
;;           | 'string? | 'string-length 
;;           | 'write-byte | 'eof-object?
;;           | 'box | 'car | 'cdr | 'unbox
;;           | 'empty?
;;           | 'box? | 'cons? | 
;; type Op2  = '+ | '- | 'eq?
;;           | 'string-ref | 'make-string
;;           | 'cons
;;           | '= | '< | '<= | 'char=? | 'boolean=?
(struct Eof    ()           #:prefab)
(struct Empty  ()           #:prefab)
(struct Int    (i)          #:prefab)
(struct Bool   (b)          #:prefab)
(struct Char   (c)          #:prefab)
;; Added in HUSTLE+
(struct String (s)          #:prefab)
(struct Prim0  (p)          #:prefab)
(struct Prim1  (p e)        #:prefab)
(struct Prim2  (p e1 e2)    #:prefab)
(struct If     (e1 e2 e3)   #:prefab)
(struct Begin  (e1 e2)      #:prefab)
(struct Let    (x e1 e2)    #:prefab)
(struct Var    (x)          #:prefab)
