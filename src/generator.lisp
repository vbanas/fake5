(defpackage :src/generator
  (:use :common-lisp
        :cl-geometry
        :src/drawer
	:src/simple-state))

(in-package :src/generator)

(defun generator-test (file)
  (let* ((fold-specs `((:a -1 :b 1 :c ,(- (/ 1 4)) :x 1 :y 0)
		       (:a 1 :b 1 :c ,(- (/ 1 4)) :x 0 :y 2)
		       (:a 1 :b 1 :c ,(- (/ 1 3)) :x 0 :y 2)
		       (:a 1 :b 0 :c ,(- (/ 2 3)) :x 0 :y 2)
		       (:a 0 :b 1 :c ,(- (/ 2 3)) :x 0 :y 0))))
    (src/simple-state::fold-quad-and-show file fold-specs :animate t)))
