#lang racket
(provide unload/free unload-value)
(require "types.rkt"
         ffi/unsafe)

;; Answer* -> Answer
(define (unload/free a)
  (match a
    ['err 'err]
    [(cons h v) (begin0 (unload-value v)
                        (free h))]))

;; Value* -> Value
(define (unload-value v)
  (match v
    [(? imm-bits?) (bits->imm v)]
    [(? box-bits? i)
     (box (unload-value (heap-ref i)))]
    [(? cons-bits? i)
     (cons (unload-value (heap-ref (+ i (arithmetic-shift 1 imm-shift))))
           (unload-value (heap-ref i)))]
    [(? string-bits? i)
     (let* ((n (unload-value (heap-ref i)))
            (cs (map (lambda (j) (unload-value (heap-ref (+ i (* 8 j) 8)))) (build-list n values))))
       (apply string cs))]
    ))

(define (untag i)
  (arithmetic-shift (arithmetic-shift i (- (integer-length ptr-mask)))
                    (integer-length ptr-mask)))

(define (heap-ref i)
  (ptr-ref (cast (untag i) _int64 _pointer) _uint64))
