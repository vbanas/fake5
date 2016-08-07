(defpackage :src/matrix
  (:use :common-lisp 
        :cl-geometry
        :src/utils) 
  (:import-from :cl-geometry
                :point-equal-p
                :make-point)
  (:export #:inverse-tr-matrix
           #:transpose-tr-matrix
           #:mult-tr-matrix
           #:mult-point-matrix
           #:identity-tr-matrix
           #:translate-matrix
           #:mult-polygon-matrix
           #:mult-polygons-matrix
           #:rotate-polygon
           #:rotate-point
           #:rotate-edge-to-x-matrix
           ))

(in-package :src/matrix)

(defun identity-tr-matrix ()
  '((1 0 0) (0 1 0) (0 0 1)))

(defun translate-matrix (dx dy)
  (list (list 1 0 dx)
        (list 0 1 dy)
        (list 0 0 1)))

(defun inverse-tr-matrix (matr)
  (destructuring-bind ((m00 m10 m20) (m01 m11 m21) (m02 m12 m22)) matr
    (let ((det (- (+ (* m00 m11 m22)
                     (* m01 m12 m20)
                     (* m02 m10 m21))
                  (* m00 m12 m21)
                  (* m01 m10 m22)
                  (* m02 m11 m20))))
      (mapcar (lambda (row)
                (mapcar (lambda (v)
                          (/ v det))
                        row))
              (list (list (- (* m11 m22) (* m12 m21))
                          (- (* m12 m20) (* m10 m22))
                          (- (* m10 m21) (* m11 m20)))
                    (list (- (* m02 m21) (* m01 m22))
                          (- (* m00 m22) (* m02 m20))
                          (- (* m01 m20) (* m00 m21)))
                    (list (- (* m01 m12) (* m02 m11))
                          (- (* m02 m10) (* m00 m12))
                          (- (* m00 m11) (* m01 m10))))))))

(defun inverse-tr-matrix-test ()
  (labels ((%test (matr)
             (assert (equal (identity-tr-matrix)
                            (mult-tr-matrix matr (inverse-tr-matrix matr))))))
    (%test '((1 0 0) (0 1 0) (0 0 1)))
    (%test '((0 -1 0) (1 0 0) (0 0 1)))))

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

(defun mult-polygons-matrix (polygons matr)
  (loop for poly in polygons collect
       (mult-polygon-matrix poly matr)))

(defun mult-edge-matrix (edge matr)
  (make-instance 'line-segment
                 :start (mult-point-matrix (start edge) matr)
                 :end (mult-point-matrix (end edge) matr)))

(defun mult-point-matrix (point matr)
  (let* ((point-matr (list (list (x point))
                           (list (y point))
                           '(1)))
         (res-matr (mult-tr-matrix matr point-matr)))
    (make-instance 'point
                   :x (car (first res-matr))
                   :y (car (second res-matr)))))

(defun move-polygon (polygon dx dy)
  (let ((m (translate-matrix dx dy)))
    (mult-polygon-matrix polygon m)))

(defun move-point (point dx dy)
  (let ((m (translate-matrix dx dy)))
    (mult-point-matrix point m)))

(defun rotation-matrix (m n)
  (labels ((%sin (m n) (/ (- (* n n) (* m m))
                          (+ (* n n) (* m m))))
           (%cos (m n) (/ (* 2 n m)
                          (+ (* n n) (* m m)))))
    (list (list (%cos m n) (- (%sin m n)) 0)
          (list (%sin m n) (%cos m n)     0)
          (list 0          0              1))))

(defun rotate-polygon (p m n &key around)
  (let ((matrix (rotation-matrix m n)))
    (if around
        (move-polygon
         (mult-polygon-matrix
          (move-polygon p
                        (- (cl-geometry:x around))
                        (- (cl-geometry:y around)))
          matrix)
         (cl-geometry:x around)
         (cl-geometry:y around))
        (mult-polygon-matrix p matrix))))

(defun rotate-point (p m n &key around)
  (let ((matrix (rotation-matrix m n)))
    (if around
        (move-point
         (mult-point-matrix
          (move-point p
                      (- (cl-geometry:x around))
                      (- (cl-geometry:y around)))
          matrix)
         (cl-geometry:x around)
         (cl-geometry:y around))
        (mult-point-matrix p matrix))))

(defun make-rotation-matrix (sin cos)
  (list (list cos (- sin) 0)
        (list sin cos     0)
        (list 0   0       1)))

(defun edge-length (edge)
  "Returns length if it is a rational number, returns NIL otherwise"
  (let* ((x (- (x (start edge)) (x (end edge))))
         (y (- (y (start edge)) (y (end edge))))
         (s (+ (* x x) (* y y)))
         ;; SBCL's isqrt returns big integer,
         ;; not sure about precision though
         (s-root (/ (isqrt (numerator s))
                    (isqrt (denominator s))))
         (diff (- s (* s-root s-root))))
    (when (= 0 diff)
      s-root)))

(defun rotate-edge-to-x-matrix (edge)
  "Returns matrix that rotates 'edge' so that it becomes parallel
   to X axis, returns NIL if length of 'edge' is not a rational number"
  (let ((len (edge-length edge)))
    (when len
      (let* ((dx (- (x (end edge)) (x (start edge))))
             (dy (- (y (end edge)) (y (start edge))))
             (sin (/ dy len))
             (cos (/ dx len)))
        ;; (format t "dx = ~A dy = ~A len = ~A sin = ~A cos = ~A~%"
        ;;         dx dy len sin cos)
        (make-rotation-matrix (- sin) cos)))))

(defun test-rotate-edge-to-x-matrix ()
  (labels ((%test (&key coords expect)
             (destructuring-bind (x1 y1 x2 y2) coords
               (let* ((e (make-instance 'line-segment
                                        :start (make-point x1 y1)
                                        :end (make-point x2 y2)))
                      (m (rotate-edge-to-x-matrix e))
                      (e1 (mult-edge-matrix e m)))
                 (assert1 (edge->list e1)
                          expect)
                 ))))
    ;; len of this edge should be 5 (3^2 + 4^2 = 5^2)
    (%test :coords '(0 0 4 3)
           :expect '(0 0 5 0))
    ;; len of this edge should be 13 (12^2 + 5^2 = 13^2)
    (%test :coords '(0 0 12 5)
           :expect '(0 0 13 0))
    ;; negative coords
    (%test :coords '(0 0 4 -3)
           :expect '(0 0 5 0))
    (%test :coords '(0 0 -4 3)
           :expect '(0 0 5 0))
    (%test :coords '(0 0 -4 -3)
           :expect '(0 0 5 0))
    t))
