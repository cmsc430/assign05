#lang racket
(require "../interp.rkt" rackunit)

(define (run e)
  (interp e))

;; Abscond examples
(check-equal? (run 7) 7)
(check-equal? (run -8) -8)

;; Blackmail examples
(check-equal? (run '(add1 (add1 7))) 9)
(check-equal? (run '(add1 (sub1 7))) 7)

;; Con examples
(check-equal? (run '(if (zero? 0) 1 2)) 1)
(check-equal? (run '(if (zero? 1) 1 2)) 2)
(check-equal? (run '(if (zero? -7) 1 2)) 2)
(check-equal? (run '(if (zero? 0)
                        (if (zero? 1) 1 2)
                        7))
              2)
(check-equal? (run '(if (zero? (if (zero? 0) 1 0))
                        (if (zero? 1) 1 2)
                        7))
              7)

;; Con+ examples
(check-equal? (run '(abs 10)) 10)
(check-equal? (run '(abs -10)) 10)
(check-equal? (run '(- 10)) -10)
(check-equal? (run '(- -10)) 10)
(check-equal? (run '(- (- 10))) 10)
(check-equal? (run '(cond [else 5])) 5)
(check-equal? (run '(cond [(zero? 1) 2] [else 3])) 3)
(check-equal? (run '(cond [(zero? 0) 2] [else 3])) 2)
(check-equal? (run '(cond [(zero? 1) 2] [(zero? (sub1 1)) 4] [else 3])) 4)

;; Dupe examples
(check-equal? (run '(if #t 3 4)) 3)
(check-equal? (run '(if #f 3 4)) 4)
(check-equal? (run '(if  0 3 4)) 3)
(check-equal? (run '(zero? 4)) #f)
(check-equal? (run '(zero? 0)) #t)

;; Dupe+ examples
(check-equal? (run '(cond [#t 2] [else 3])) 2)
(check-equal? (run '(cond [#f 2] [else 3])) 3)
(check-equal? (run '(cond [1 2] [else 3])) 2)
(check-equal? (run '(cond [#f 2] [#t 4] [else 3])) 4)
(check-equal? (run '(cond [#t 2] [#f 4] [else 3])) 2)
(check-equal? (run '(cond [#t 2] [#f (add1 #f)] [else 3])) 2)

;; Extort examples
(check-equal? (run '(zero? #t)) 'err)
(check-equal? (run '(zero? #f)) 'err)
(check-equal? (run '(add1 #f)) 'err)
(check-equal? (run '(if (add1 #f) 1 2)) 'err)

;; Extort+ examples
(check-equal? (run '(abs #f)) 'err)
(check-equal? (run '(cond [(add1 #f) 1] [else 2])) 'err)

;; Fraud examples
(check-equal? (run '(let ((x 7)) x)) 7)
(check-equal? (run '(let ((x 7)) 2)) 2)
(check-equal? (run '(let ((x 7)) (add1 x))) 8)
(check-equal? (run '(let ((x (add1 7))) x)) 8)
(check-equal? (run '(let ((x 7)) (let ((y 2)) x))) 7)
(check-equal? (run '(let ((x 7)) (let ((x 2)) x))) 2)
(check-equal? (run '(let ((x 7)) (let ((x (add1 x))) x))) 8)
(check-equal? (run '(let ((x (add1 #f))) 0)) 'err)
               
;; Fraud+ examples
(check-equal? (run '(let () 7)) 7)
(check-equal? (run '(let ((x 7) (y 8)) 2)) 2)
(check-equal? (run '(let ((x 7) (y 8)) (add1 x))) 8)
(check-equal? (run '(let ((x 7) (y 8)) (add1 y))) 9)
(check-equal? (run '(let ((x (add1 7)) (y 0)) y)) 0)
(check-equal? (run '(let ((x 7) (z 9)) (let ((y 2)) x))) 7)
(check-equal? (run '(let ((x 7) (z 9)) (let ((x 2)) x))) 2)
(check-equal? (run '(let ((x 7) (z 9)) (let ((x (add1 x)) (z z)) x))) 8)
(check-equal? (run '(let ((x (add1 #f)) (z 9)) x)) 'err)
(check-equal? (run '(char? #\a)) #t)
(check-equal? (run '(integer? #\a)) #f)
(check-equal? (run '(boolean? #\a)) #f)
(check-equal? (run '(char? 4)) #f)
(check-equal? (run '(integer? 4)) #t)
(check-equal? (run '(boolean? 4)) #f)
(check-equal? (run '(char? #f)) #f)
(check-equal? (run '(integer? #f)) #f)
(check-equal? (run '(boolean? #t)) #t)
(check-equal? (run '(char->integer #\a)) 97)
(check-equal? (run '(integer->char 97)) #\a)
(check-equal? (run '(integer->char #\a)) 'err)
