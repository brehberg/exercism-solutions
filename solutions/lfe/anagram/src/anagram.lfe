(defmodule anagram
  (export (find 2)))

(defun find (target candidates)
  (let ((base (string:to_upper target)))
    (lists:filter
      (lambda (candidate) (is-anagram base candidate)) candidates)))

(defun is-anagram (base candidate)
  (let ((word (string:to_upper candidate)))
    (and (/= base word)
         (== (lists:sort base) (lists:sort word)))))


