#lang racket

(provide balanced?)

(define (balanced? str)
  ; Checks that all the brackets and braces in the string are matched correctly, 
  ; and nested correctly
  (define (is-balanced? lst closers)
    (if (empty? lst)
        (empty? closers)
        (let ([first (car lst)] [rest (cdr lst)])
          (match first
            ; opening bracket was found, add matching closing value to the stack
            [#\( (is-balanced? rest (cons #\) closers))]
            [#\{ (is-balanced? rest (cons #\} closers))]
            [#\[ (is-balanced? rest (cons #\] closers))]
            ; closing bracket was found, is it the next expected value on stack?
            [(or #\) #\} #\]) (and (not (empty? closers)) (eq? first(car closers)) 
              (is-balanced? rest (cdr closers)))]
            [_ (is-balanced? rest closers)]))))
                        
  (is-balanced? (string->list str) '()))
