(import (rnrs))

(define (leap-year? year)
    (define (divisible-by? n) (zero? (remainder year n)))
        (and (divisible-by? 4)
            (or (not (divisible-by? 100))
                (divisible-by? 400))))

