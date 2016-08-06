(defpackage :src/generator
  (:use :common-lisp
        :cl-geometry
        :src/drawer
	:src/simple-state
        :src/printer))

(in-package :src/generator)

(defun generator-test (file)
  (let* ((fold-specs `((:a -1 :b 1 :c ,(- (/ 1 4)) :x 1 :y 0)
		       (:a 1 :b 1 :c ,(- (/ 1 4)) :x 0 :y 2)
		       (:a 1 :b 1 :c ,(- (/ 1 3)) :x 0 :y 2)
		       (:a 1 :b 0 :c ,(- (/ 2 3)) :x 0 :y 2)
		       (:a 0 :b 1 :c ,(- (/ 2 3)) :x 0 :y 0)))
	 (new-specs)
	 (specs-size 10))
    (loop for n from 1 to specs-size do
	 (push (list :a (+ (random 5) 1/100) :b (+ (random 3) 1/100) :c (- (/ (random 2) (1+ (random 5)))) :x (random 2) :y (random 2)) new-specs))
    (print-solution
     (src/simple-state::fold-quad-and-show file new-specs :animate t))))
