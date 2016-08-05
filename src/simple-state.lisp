(defpackage :src/simple-state
  (:use :common-lisp
        :2d-geometry))

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

(defun really-between-p (a b c)
  (and (2d-geometry::between-p a b c)
       (not (2d-geometry::point-equal-p a c))
       (not (2d-geometry::point-equal-p b c))))

(defun split-polygon (polygon line)
  (let ((points-1 nil)
        (points-2 nil)
        (push-to-1 t))
    (labels ((%push (pt)
               (if push-to-1
                   (push pt points-1)
                   (push pt points-2)))
             (%swap ()
               (setf push-to-1 (not push-to-1))))
      (loop for edge in (edge-list polygon) do
           (let* ((edge-line (line-from-segment edge))
                  (ipoint (lines-intersection-point line edge-line)))
             (if (and ipoint
                      (really-between-p (start edge)
                                        (end edge)
                                        ipoint))
                 (progn
                   (%push (start edge))
                   (%push ipoint)
                   (%swap)
                   (%push ipoint))
                 (%push (start edge))))))
    (if points-2
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
           :expected '((0 0 0 1 2 1 2 0)
                       (0 1 0 2 2 2 2 1)))
    (%test :a 0 :b 1 :c -2
           :coords '(0 0 0 2 2 2 2 0)
           :expected '((0 0 0 2 2 2 2 0)))))

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
              (some (lambda (pt)
                      (let ((pt-sign (line-equation-res line pt)))
                        (unless (= pt-sign 0)
                          pt-sign)))
                    (point-list polygon))))
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
           :expected '((0 0 0 1 2 1 2 0)
                       (2 1 2 0 0 0 0 1)))
    (%test :a 0 :b 1 :c -1
           :x 2 :y 2
           :coords '(0 0 0 2 2 2 2 0)
           :expected '((2 2 2 1 0 1 0 2)
                       (0 1 0 2 2 2 2 1)))
    (%test :a 0 :b 1 :c -2
           :x 0 :y 0
           :coords '(0 0 0 2 2 2 2 0)
           :expected '((0 0 0 2 2 2 2 0)))
    (%test :a 0 :b 1 :c -2
           :x 3 :y 3
           :coords '(0 0 0 2 2 2 2 0)
           :expected '((2 4 2 2 0 2 0 4)))
    ))
