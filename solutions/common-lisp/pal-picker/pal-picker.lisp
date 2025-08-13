(defpackage :pal-picker
  (:use :cl)
  (:export :pal-picker :habitat-fitter :feeding-time-p
           :pet :play-fetch))

(in-package :pal-picker)

;; Picking a Pal - takes some personality trait (a keyword) 
;; and returns the type of pet (a string) with the given trait
(defun pal-picker (personality)
  (case personality
    (:lazy "Cat")
    (:energetic "Dog")
    (:quiet "Fish")
    (:hungry "Rabbit")
    (:talkative "Bird")
    (otherwise "I don't know... A dragon?"))
)

;; In Their Natural Habitat - selecting the proper habitat 
;; size (a keyword) from pet's weight in kilograms (a integer).
(defun habitat-fitter (weight)
  (cond ((>= weight 40) :massive)
        ((>= weight 20) :large)
        ((>= weight 10) :medium)
        ((>= weight 1) :small)
        (t :just-your-imagination))
)

;; And Now, We Feast - take a percent fullness (an integer) 
;; and return a message in the form of a string.
(defun feeding-time-p (fullness)
  (if (> fullness 20) "All is well." "It's feeding time!")
)

;; A Code of Conduct - a pair of functions – pet and play-fetch
;; that take the type of pet (as a string) and return either
;; nothing or a message if the action is unfitting.
(defun pet (pet)
  (when (string= pet "Fish") "Maybe not with this pet...")
)
(defun play-fetch (pet)
  (unless (string=  pet "Dog") "Maybe not with this pet...")
)
