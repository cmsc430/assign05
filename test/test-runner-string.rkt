#lang racket
(provide test-runner test-runner-io)
(require rackunit)

(define (test-runner run) 
  ;; Public string test
  (check-equal?
   (run
    '(let ((x (make-string 3 #\f)))
      (if (string? x)
          (string-ref x
                      (- (string-length x) 1))
          42)))
   #\f)

  ;; TODO: Add your own tests here
  )

(define (test-runner-io run)
  (check-equal?
   (run
    '(let ((x (make-string 3 (integer->char (read-byte)))))
      (if (string? x)
          (string-ref x
                      (- (string-length x) 1))
          42))
    "L")
   (cons #\L ""))

  ;; TODO: Add your own tests here
)
