(defpackage :protein-translation
  (:use :cl)
  (:export :proteins
           :invalid-protein))

(in-package :protein-translation)

(define-condition invalid-protein () ())

(defun from-codon (codon)
  (cond
    ((string= codon "AUG") "Methionine")
    ((string= codon "UGG") "Tryptophan")
    ((member codon '("UGU" "UGC") :test #'string=) "Cysteine")
    ((member codon '("UUA" "UUG") :test #'string=) "Leucine")
    ((member codon '("UUU" "UUC") :test #'string=) "Phenylalanine")
    ((member codon '("UAU" "UAC") :test #'string=) "Tyrosine")    
    ((member codon '("UCU" "UCC" "UCA" "UCG") :test #'string=) "Serine")   
    ((member codon '("UAA" "UAG" "UGA") :test #'string=) :STOP)
    (t (error 'invalid-protein))))

(defun proteins (strand)
  (when (plusp (length strand))
    (loop for rest = strand then (subseq rest 3)
          for size = (min 3 (length rest))
          for codon = (subseq rest 0 size)
          for amino-acid = (from-codon codon)
          until (string= amino-acid :STOP)
          collect amino-acid
          until (= 3 (length rest)))))
