#lang racket
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; The grammar of our AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; type Expr =
;; | Integer
;; | Boolean
;; | Character
;; | Variable
;; | String
;; | Prim1 Expr
;; | Prim2 Expr Expr
;; | If Expr Expr Expr
;; | cond (Clause list) Expr
;; | Let (Binding list) Expr
;; | Nil

;; type Prim1 =
;; | 'add1 | 'sub1 | 'zero? | box | unbox | car | cdr
;; | 'integer? | 'boolean? | 'char? | 'integer->char | 'char->integer

;; type Prim2 = '+ | '- | cons

;; type Binding = Variable Expr

;; type Variable = Symbol (except 'add1 'sub1 'if, etc.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; The AST data structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The AST can be viewed as having 'kinds' of nodes.
;;
;; * The nodes that represnt an expression themselves
;;
;; * The nodes that are part of an expression, but no an expression themselves

;; The below are the former:

(struct int-e  (i)     #:transparent)
(struct bool-e (b)     #:transparent)
(struct char-e (c)     #:transparent)
(struct string-e (s)   #:transparent)
(struct var-e  (v)     #:transparent)
(struct prim-e (p es)  #:transparent)
(struct if-e   (e t f) #:transparent)
(struct cond-e (cs el) #:transparent)
(struct let-e  (bs b)  #:transparent)
(struct nil-e  ()      #:transparent)

;; The next two are the latter:

;; a clause now takes an _arbitrary_ expression and a body.
;; This is different than assignment 3! If you want to understand
;; why, look at the lecture notes for Dupe.
(struct clause  (e body) #:transparent)

;; A binding holds a symbol representing the bound variable and
;; Expr that represents the value that will be bound to that variable
(struct binding (v e) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; AST utility functions (predicates)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Any -> Boolean
;; Is x a unary primitive?
(define (prim1? x)
  (and (symbol? x)
       (memq x '(add1 sub1 abs - integer->char char->integer
                 car cdr length box?  string?  cons?  empty?
                 box unbox string-length
                 char? integer? boolean? zero?))))

;; Any -> Boolean
;; Is x a binary primitive?
(define (prim2? x)
  (and (symbol? x)
       (memq x '(+ cons string-ref make-string char=?
                 = <= < char=? boolean=? -))))

(define (prim? x)
  (or (prim1? x)
      (prim2? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; AST utility functions (getters)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Expr -> Value
(define (get-val v)
  (match v
    [(int-e x) x]
    [(bool-e x) x]
    [(char-e x) x]
    [(string-e x) x]
    [(nil-e) '()]))

;; It will sometimes be useful to get the list of all the variables that are
;; introduced by a `let`
;; [Binding] -> [Symbol]
(define (get-vars bs)
  (match bs
    ['() '()]
    [(cons (binding v _) bs) (cons v (get-vars bs))]))

;; Get all of the _definitions_ from a list of bindings
;; [Binding] -> [Expr]
(define (get-defs bs)
  (match bs
    ['() '()]
    [(cons (binding _ def) bs) (cons def (get-defs bs))]))

;; Get all of the predicate expressions from a list of clauses
;; [Clause] -> [Expr]
(define (get-preds cs)
  (match cs
    ['() '()]
    [(cons (clause p _) cs) (cons p (get-preds cs))]))

;; Get all of the bodies expressions from a list of clauses
;; [Clause] -> [Expr]
(define (get-bods cs)
  (match cs
    ['() '()]
    [(cons (clause _ b) cs) (cons b (get-preds cs))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; AST utility functions (printers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We have switched to using `#:transparent` above, so this should only be
;; necessary if you're desperate when debugging :'(

;; Given an AST, construct an sexpr that has the same shape
(define (ast-debug a)
  (match a
    [(int-e i)     `(int-e ,i)]
    [(bool-e b)    `(bool-e ,b)]
    [(char-e b)    `(char-e ,b)]
    [(string-e s)  `(string-e ,s)]
    [(var-e v)     `(var-e ,v)]
    [(nil-e)       ''()]
    [(prim-e p es) `(prim-e ,p ,@(map ast-debug es))]
    [(if-e e t f)  `(if-e ,(ast-debug e)
                          ,(ast-debug t)
                          ,(ast-debug f))]
    [(cond-e cs f) `(cond-e ,(clauses-debug cs) ,(ast-debug f))]
    [(let-e bs b)  `(let-e ,(binding-debug bs) ,(ast-debug b))]))

(define (binding-debug bnds)
  (match bnds
    ['() '()]
    [(cons (binding v e) bnds) `((,v ,(ast-debug e)) ,@(binding-debug bnds))]))

(define (clauses-debug cs)
  (match cs
    ['() '()]
    [(cons (clause e b) cs) `((,(ast-debug e) ,(ast-debug b)) ,@(clauses-debug cs))]))
