(defpackage :src/simple-state
  (:use :common-lisp
<<<<<<< HEAD
        :2d-geometry
        :src/drawer
        :src/polygons)
  (:import-from :2d-geometry
=======
        :cl-geometry
        :src/utils
        :src/drawer
        :src/mcts)
  (:import-from :cl-geometry
>>>>>>> 53a7be8d2fa22af2f705588edb35131bbe244f9a
                :point-equal-p))

(in-package :src/simple-state)

(defun mirror-point (point line)
  (let* ((c (- (* (B line) (x point)) (* (A line) (y point))))
         (pline (make-instance 'line :a (- (B line)) :b (A line) :c c))
         (ipoint (lines-intersection-point line pline))
         (mirror-point (make-instance 'point
                                      :x (- (* 2 (x ipoint)) (x point))
                                      :y (- (* 2 (y ipoint)) (y point)))))
    mirror-point))

(defun test-mirror-point ()
  (labels ((%test (&key a b (c 0)  x y mx my)
             (let* ((l (make-instance 'line :a a :b b :c c))
                    (p (make-instance 'point :x x :y y))
                    (m (mirror-point p l)))
               (assert (= (x m) mx))
               (assert (= (y m) my)))))
    (%test :a 1 :b -1 :x 0 :y 1 :mx 1 :my 0)
    (%test :a 1 :b 0 :c -1 :x 0 :y 1 :mx 2 :my 1)
    (%test :a 0 :b 1 :c -1 :x 1 :y 0 :mx 1 :my 2)
    t))

(defun cl-geometry::between-p (a b c)
  "Is c colinear with a->b and lies between them?"
  (when (cl-geometry::colinear-p a b c)
    (if (= (x a)(x b))
        (or (and (>= (y c)(y a))
                 (<= (y c)(y b)))
            (and (>= (y c)(y b))
                 (<= (y c)(y a))))
        (or (and (>= (x c)(x a))
                 (<= (x c)(x b)))
            (and (>= (x c)(x b))
                 (<= (x c)(x a)))))))

(defun really-between-p (a b c)
  (and (cl-geometry::between-p a b c)
       (not (cl-geometry::point-equal-p a c))
       (not (cl-geometry::point-equal-p b c))))

(defun split-polygon (polygon line)
  (let ((points-1 nil)
        (points-2 nil)
        (points-on-line 0))
    (labels ((%push (pt)
               (let ((sign (line-equation-res line pt)))
                 (cond
                   ((> sign 0) (push pt points-1))
                   ((< sign 0) (push pt points-2))
                   (t
                    (incf points-on-line)
                    (push pt points-1)
                    (push pt points-2))))))
      (loop for edge in (edge-list polygon) do
           (let* ((edge-line (line-from-segment edge))
                  (ipoint (lines-intersection-point line edge-line)))
             (if (and ipoint
                      (cl-geometry::between-p (start edge)
                                              (end edge)
                                              ipoint))
                 (progn
                   (%push (start edge))
                   (unless (or (point-equal-p ipoint (start edge))
                               (point-equal-p ipoint (end edge)))
                     (%push ipoint)))
                 (%push (start edge))))))
    (if (or (and points-2
                 (not (= (length points-2) points-on-line))
                 points-1
                 (not (= (length points-1) points-on-line))))
        (list (make-polygon-from-point-list (reverse points-1))
              (make-polygon-from-point-list (reverse points-2)))
        (list polygon))))

(defun point->list (point)
  (list (x point) (y point)))

(defun polygon->list (polygon)
  (alexandria:mappend
   #'point->list
   (point-list polygon)))

(defun assert1 (result expected)
  (unless (equalp result expected)
    (error
     (with-output-to-string (*standard-output*)
       (format t "No match, expected:~%~A~%" expected)
       (format t "Result:~%~A~%" result)))))

(defun test-split-polygon ()
  (labels ((%test (&key a b c coords expected)
             (let* ((p (apply #'make-polygon-from-coords coords))
                    (l (make-instance 'line :a a :b b :c c))
                    (res (split-polygon p l)))
               (assert1 (mapcar #'polygon->list res) expected))))
    (%test :a 0 :b 1 :c -1
           :coords '(0 0 0 2 2 2 2 0)
           :expected '((0 1 0 2 2 2 2 1)
                       (0 0 0 1 2 1 2 0)))
    (%test :a 0 :b 1 :c -2
           :coords '(0 0 0 2 2 2 2 0)
           :expected '((0 0 0 2 2 2 2 0)))
    (%test :a -1 :b 1 :c (- (/ 1 4))
           :coords '(0 0 0 1 1 1 1 0)
           :expected '((0 1/4 0 1 3/4 1)
                       (0 0 0 1/4 3/4 1 1 1 1 0)))))

(defun mirror-polygon (polygon line)
  (make-polygon-from-point-list
   (mapcar
    (lambda (pt) (mirror-point pt line))
    (reverse
     (point-list polygon)))))

(defun line-equation-res (line point)
  (+
   (* (A line) (x point))
   (* (B line) (y point))
   (C line)))

(defun fold-polygon (polygon line point)
  (let ((split-list (split-polygon polygon line))
        (point-sign (line-equation-res line point)))
    (mapcar
     (lambda (polygon)
       (let ((sign
              (nth-value 1 (find-non-collinear-point line polygon))))
         (if (>= (* sign point-sign) 0)
             polygon
             (mirror-polygon polygon line))))
     split-list)))

(defun fold-polygon-test ()
  (labels ((%test (&key a b c x y coords expected)
             (let* ((p (apply #'make-polygon-from-coords coords))
                    (l (make-instance 'line :a a :b b :c c))
                    (res (fold-polygon
                          p l
                          (make-instance 'point :x x :y y))))
               (assert1 (mapcar #'polygon->list res) expected))))
    (%test :a 0 :b 1 :c -1
           :x 0 :y 0
           :coords '(0 0 0 2 2 2 2 0)
           :expected '((2 1 2 0 0 0 0 1)
                       (0 0 0 1 2 1 2 0)))
    (%test :a 0 :b 1 :c -1
           :x 2 :y 2
           :coords '(0 0 0 2 2 2 2 0)
           :expected '((0 1 0 2 2 2 2 1)
                       (2 2 2 1 0 1 0 2)))
    (%test :a 0 :b 1 :c -2
           :x 0 :y 0
           :coords '(0 0 0 2 2 2 2 0)
           :expected '((0 0 0 2 2 2 2 0)))
    (%test :a 0 :b 1 :c -2
           :x 3 :y 3
           :coords '(0 0 0 2 2 2 2 0)
           :expected '((2 4 2 2 0 2 0 4)))
    (%test :a -1 :b 1 :c (- (/ 1 4))
           :x 1 :y 0
           :coords '(0 0 0 1 1 1 1 0)
           :expected '((3/4 1 3/4 1/4 0 1/4)
                       (0 0 0 1/4 3/4 1 1 1 1 0)))
    (assert1
     (mapcar
      #'polygon->list
      (fold-quad `((:a -1 :b 1 :c ,(- (/ 1 4)) :x 1 :y 0)
                   (:a 1 :b 1 :c ,(- (/ 1 4)) :x 0 :y 2)
                   (:a 1 :b 1 :c ,(- (/ 1 3)) :x 0 :y 2)
                   (:a 1 :b 0 :c ,(- (/ 2 3)) :x 0 :y 2)
                   (:a 0 :b 1 :c ,(- (/ 2 3)) :x 0 :y 0))))
     '((7/12 1/3 7/12 2/3 2/3 2/3 2/3 5/12)
       (2/3 2/3 2/3 1/4 7/12 1/4 7/12 2/3)
       (2/3 2/3 2/3 5/12 5/12 2/3)
       (2/3 1/4 1/12 1/4 1/24 7/24 5/12 2/3 2/3 2/3)
       (1/24 7/24 1/12 1/3 1/12 1/4)
       (2/3 2/3 2/3 5/12 7/12 1/3 1/3 1/3 1/3 2/3)
       (2/3 0 1/3 0 1/3 2/3 2/3 2/3)
       (2/3 2/3 2/3 5/12 5/12 2/3)
       (1/24 7/24 5/12 2/3 2/3 2/3 2/3 0 1/3 0)
       (1/3 1/12 1/3 0 1/24 7/24 1/12 1/3)
       (1/12 1/4 1/4 1/4 1/4 1/12)
       (1/4 1/12 1/12 1/4 1/12 1/3 1/3 1/12)))
    ))

(defun fold-polygon-list (polygon-list line point)
  (alexandria:mappend
   (lambda (polygon)
     (fold-polygon polygon line point))
   polygon-list))

(defun fold-quad (fold-specs)
  (let* ((quad (make-polygon-from-coords 0 0 0 1 1 1 1 0))
         (result (reduce (lambda (q fold-spec)
                           (destructuring-bind (&key a b (c 0) x y)
                               fold-spec
                             (fold-polygon-list
                              q
                              (make-instance 'line :a a :b b :c c)
                              (make-instance 'point :x x :y y))))
                         fold-specs
                         :initial-value (list quad))))
    result))

(defun fold-quad-and-show (file fold-specs &key animate)
  (labels ((%once (file fold-specs)
             (let* ((result (fold-quad fold-specs)))
               ;; (format t "result: ~A~%"
               ;;         (mapcar #'polygon->list result))
               (draw-polygons-to-svg
                result :filename file))))
    (let ((file-pn (pathname file)))
      (if animate
          (loop for i from 0 to (length fold-specs) do
               (%once (make-pathname
                       :defaults file-pn
                       :name (format nil "~A~A"
                                     (pathname-name file-pn) i))
                      (subseq fold-specs 0 i)))
          (%once file fold-specs)))))

(defun area-simple-polygon (polygon)
  "Calculate an area of a simple polygon."
  (* 1/2
     (polygon-orientation polygon)
     (reduce #'+ (maplist #'(lambda (list)
                              (let ((v1 (car list))
                                    (v2 (if (cdr list)
                                            (cadr list)
                                            (car list))))
                                (- (* (x v1)(y v2))(* (x v2)(y v1)))))
                          (point-list polygon)))))

(defun single? (lst)
  (and lst (null (cdr lst))))

(defun intersect-and-reduce (polygons)
  (reduce
   (lambda (ps p2)
     (loop for p1 in ps append
          (reduce-triangles (polygon-intersection p1 p2))))
   (cdr polygons)
   :initial-value (list (car polygons))))

(defun union-and-reduce (polygons)
  (reduce
   (lambda (ps p2)
     (loop for p1 in ps append
          (reduce-triangles (polygon-union p1 p2))))
   (cdr polygons)
   :initial-value (list (car polygons))))

(defun score (polygons target-polygons)
  (let* ((polygons (union-and-reduce polygons))
         (intersected-area-ps (intersect-and-reduce (append polygons target-polygons)))
         (united-area-ps (union-and-reduce (append polygons target-polygons))))
    (/ (reduce #'+ (mapcar #'abs (mapcar #'area-simple-polygon intersected-area-ps)))
       ;; (reduce #'+ (mapcar #'abs (mapcar #'area-simple-polygon united-area-ps)))
       1.0)))

(defclass game-state ()
  ((field :type list
          :documentation "All polygons on the field"
          :accessor field
          :initarg :field)
   (adjustment-matrix :type list
                      :documentation "list of lists of values, describes translation of original problem"
                      :accessor adjustment-matrix
                      :initarg :adjustment-matrix)
   (target-field :type list
                 :documentation "List of polygons describing desired final state"
                 :accessor target-field
                 :initarg :target-field)))

(defmethod clone-state (_ (st game-state))
  st)

(defclass action ()
  ((folding-line :accessor folding-line
                 :type 'line
                 :initarg :folding-line)
   (folding-side :accessor folding-side
                 :type 'point
                 :initarg :folding-side)))

(defmethod next-state (_ (st game-state) action)
  (copy-instance
   st
   :field (fold-polygon-list (field st)
                             (folding-line action)
                             (folding-side action))))

(defun find-non-collinear-point (line polygon)
  (dolist (pt (point-list polygon))
    (let ((pt-sign (line-equation-res line pt)))
      (unless (= pt-sign 0)
        (return-from find-non-collinear-point
          (values pt pt-sign)))))
  nil)

(defmethod possible-actions (_ (st game-state))
  (loop for polygon in (target-field st) append
       (loop for edge in (edge-list polygon) append
            (let* ((line (line-from-segment edge))
                   (direction-point
                    (find-non-collinear-point line polygon)))
              (when direction-point
                (list
                 (make-instance
                  'action
                  :folding-line line
                  :folding-side direction-point)))))))

