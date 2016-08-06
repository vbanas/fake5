(defpackage :src/printer 
  (:use :common-lisp :src/utils :src/types :cl-geometry
        :src/utils)
  (:import-from :alexandria)
  (:export #:print-problem 
           #:print-solution))

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

(defun print-solution (field)
  (let ((origins (make-hash-table :test #'equalp))
        (orig-ind -1))
    (labels ((%add-origin (point)
               (unless (gethash (list (x (orig-point point))
                                      (y (orig-point point)))
                                origins)
                 (setf (gethash (list (x (orig-point point))
                                      (y (orig-point point)))
                                origins)
                       (cons (incf orig-ind)
                             point))))
             (%get-origin (point)
               (car (gethash (list (x (orig-point point))
                                   (y (orig-point point)))
                             origins)))
             (%sorted-origins (key)
               (let ((lst (alexandria:hash-table-alist origins)))
                 (mapcar key
                         (sort (copy-list lst)
                               #'<
                               :key (alexandria:compose #'car #'cdr))))))
      (loop for poly in field do
           (loop for point in (point-list poly) do
                (%add-origin point)))
      ;; Print origins
      (let ((lst (%sorted-origins #'car)))
        (format t "~A~%" (length lst))
        (loop for (x y) in lst do
             (format t "~A,~A~%" x y)))
      ;; Print polygons
      (format t "~A~%" (length field))
      (loop for poly in field do
           (let ((points (point-list poly)))
             (format t "~A ~{~A~^ ~}" (length points)
                     (mapcar #'%get-origin points)))
           (format t "~%"))
      ;; Print destintations
      ;; TODO: matrix operations
      (let ((lst (%sorted-origins (alexandria:compose #'cdr #'cdr))))
        (loop for point in lst do
             (format t "~A,~A~%" (x point) (y point)))))))
