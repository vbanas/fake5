(defpackage :src/printer 
  (:use :common-lisp :src/utils :src/types :cl-geometry)
  (:export #:print-problem 
           ))

(in-package :src/printer)

(defun print-point (p)
  (format nil "~A,~A" (x p) (y p)))

(defun print-line-segment (l)
  (format nil "~A ~A"
          (print-point (start l))
          (print-point (end l))))

(defun print-polygon (p)
  (let ((points (point-list p)))
    (format nil "~A~%~{~A~%~}"
            (length points)
            (mapcar #'print-point points))))

(defun print-silhouette (s)
  (format nil "~A~%~{~A~}"
          (length s)
          (mapcar #'print-polygon s)))

(defun print-skeleton (s)
  (format nil "~A~%~{~A~%~}"
          (length s)
          (mapcar #'print-line-segment s)))

(defun print-problem (p)
  (format nil "~A~A"
          (print-silhouette (silhouette p))
          (print-skeleton (skeleton p))))
