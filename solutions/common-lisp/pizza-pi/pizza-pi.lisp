(defpackage :pizza-pi
  (:use :cl)
  (:export :dough-calculator :pizzas-per-cube
           :size-from-sauce :fair-share-p))

(in-package :pizza-pi)

;; Takes the number of pizzas (n), their diameter (d)
;; And returns the dough needed to the nearest gram (g).
;;   g = n * (((45 * pi * d) / 20) + 200)
(defun dough-calculator (pizzas diameter)
  (round (* pizzas (+ (/ (* 45 pi diameter) 20) 200))) ; => A Dough Ratio
)

;; calculates the pizza diameter (d) from every milliliter of sauce applied (s).
;;   d = square-root of ((40 * s) / (3 * pi))
(defun size-from-sauce (sauce)
  (sqrt (/ (* 40 sauce) (* 3 pi))) ; => A Splash of Sauce 
)

;; Takes a side-length of a cheese cube (l) and the pizzas' diameter (d),
;; And returns the number of pizzas (n) made while rounding down.
;;    n = (2 * (l^3)) / (3* pi * (d^2))
(defun pizzas-per-cube (cube-size diameter)
  (floor (/ 
    (* 2 (expt cube-size 3)) 
    (* 3 pi (expt diameter 2)))) ; => Some Cheese, Please
)

;; Takes a number of pizzas and number of friends
;; and returns T if the pizzas will evenly distribute and NIL otherwise.
(defun fair-share-p (pizzas friends)
  (zerop (mod (* 8 pizzas) friends)) ; => A Fair Share
)
