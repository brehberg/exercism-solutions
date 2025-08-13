(import (rnrs))

(define (dna-to-rna nucleotide)
  (case nucleotide
    ((#\G) #\C)
    ((#\C) #\G)
    ((#\T) #\A)
    ((#\A) #\U)))

(define (dna->rna dna)
  (list->string
    (map dna-to-rna
        (string->list dna))))
