(import (rnrs))

(define (leap-year? year)
(apply (lambda (a b c) (and a (or (not b) c)))
       (map (lambda (n) (zero? (remainder year n)))
            '(4 100 400))))

