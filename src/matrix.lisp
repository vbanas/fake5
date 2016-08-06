(defpackage :src/matrix
  (:use :common-lisp 
        :cl-geometry) 
  (:import-from :cl-geometry
                :point-equal-p)
  (:export #:inverse-tr-matrix
           #:transpose-tr-matrix
           #:mult-tr-matrix
           #:mult-point-matrix
           #:identity-tr-matrix
           #:translate-matrix
           #:mult-polygon-matrix))

(in-package :src/matrix)

(defun identity-tr-matrix ()
  '((1 0 0) (0 1 0) (0 0 1)))

(defun translate-matrix (dx dy)
  (list (list 1 0 dx)
        (list 0 1 dy)
        (list 0 0 1)))

(defun inverse-tr-matrix (matr)
  (destructuring-bind ((m00 m01 m02) (m10 m11 m12) (m20 m21 m22)) matr
    (let ((det (- (+ (* m00 m11 m22)
                     (* m01 m12 m20)
                     (* m02 m10 m21))
                  (* m00 m12 m21)
                  (* m01 m12 m22)
                  (* m02 m11 m20))))
      (mapcar (lambda (row)
                (mapcar (lambda (v)
                          (/ v det))
                        row))
              (list (list (- (* m11 m22) (* m12 m21))
                          (- (* m02 m21) (* m01 m22))
                          (- (* m01 m12) (* m02 m11)))
                    (list (- (* m12 m20) (* m10 m22))
                          (- (* m00 m22) (* m02 m20))
                          (- (* m02 m10) (* m00 m12)))
                    (list (- (* m10 m21) (* m11 m20))
                          (- (* m01 m20) (* m00 m21))
                          (- (* m00 m11) (* m01 m10))))))))

(defun transpose-tr-matrix (matrix)
  (when (notevery #'null matrix)
    (cons (mapcar #'first matrix)
          (transpose-tr-matrix (mapcar #'cdr matrix)))))

(defun dot-product (v1 v2)
  (reduce #'+ (mapcar #'* v1 v2)))

(defun mult-tr-matrix (matr1 matr2)
  (let ((matr2 (transpose-tr-matrix matr2)))
    (loop for a in matr1 collect
         (loop for b in matr2 collect
              (dot-product a b)))))

(defun mult-polygon-matrix (polygon matr)
  (make-polygon-from-point-list
   (loop for point in (point-list polygon) collect
        (mult-point-matrix point matr))))

(defun mult-point-matrix (point matr)
  (let* ((point-matr (list (list (x point))
                           (list (y point))
                           '(1)))
         (res-matr (mult-tr-matrix matr point-matr)))
    (make-instance 'point
                   :x (car (first res-matr))
                   :y (car (second res-matr)))))
