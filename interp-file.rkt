#lang racket
(provide (all-defined-out))
(require (only-in "interp.rkt" interp) "lex.rkt" "syntax.rkt" "parse.rkt")

;; String -> Void
;; Parse and interpret contents of given filename,
;; print result on stdout
(define (main fn)
  (with-input-from-file fn
    (λ ()
      (let ((p (read-program)))
        (unless (and (expr? p) (closed? p))
          (error "syntax error" p))
        (writeln (interp p))))))

(define (read-program)
  (parse (lex-port (current-input-port))))
