(defpackage :lillys-lasagna
  (:use :cl)
  (:export :expected-time-in-oven
           :remaining-minutes-in-oven
           :preparation-time-in-minutes
           :elapsed-time-in-minutes))

(in-package :lillys-lasagna)

(defun expected-time-in-oven () 
  "Define the expected oven time in minutes."
  337
)

(defun remaining-minutes-in-oven (elapsed-bake-time)
  "Calculate the bake time remaining."
  (- (expected-time-in-oven) elapsed-bake-time)
)

(defun preparation-time-in-minutes (number-of-layers)
  "Preparation time in minutes."
  (let ((preparation-time-per-layer 19))
    (* preparation-time-per-layer number-of-layers))
)

(defun elapsed-time-in-minutes (number-of-layers elapsed-bake-time)
  "Return elapsed cooking time."
  (+ (preparation-time-in-minutes number-of-layers) elapsed-bake-time)
)
