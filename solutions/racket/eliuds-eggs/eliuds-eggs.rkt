#lang racket

(provide number->eggs)

(define (number->eggs n)
  (if (zero? n) 0
    (add1 (number->eggs 
      (bitwise-and n (sub1 n))))))
