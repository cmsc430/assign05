#lang racket
(provide (all-defined-out))
(require "compile.rkt" "syntax.rkt" "asm/printer.rkt" "lex.rkt" "parse.rkt")

;; String -> Void
;; Compile contents of given file name,
;; emit asm code on stdout
(define (main fn)
  (with-input-from-file fn
    (λ ()
      (let ((p (read-program)))
        (unless (and (expr? p) (closed? p))
          (error "syntax error"))          
        (asm-display (compile p))))))

(define (read-program)
  (parse (lex-port (current-input-port))))
