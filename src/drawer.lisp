(defpackage :src/drawer
  (:nicknames :drawer)
  (:use :common-lisp :src/field :src/utils :cl-svg :cl-geometry)
  ;;(:import-from :mcts)
  (:import-from :spatial-trees)
  (:import-from :rectangles)
  (:import-from :src/types)
  (:import-from :src/parser))

(in-package :src/drawer)

(defparameter *scale* 500)
(defparameter *border* (round (/ *scale* 100)))
(defparameter *steps* 5)
(defparameter *x-shift* 0)
(defparameter *y-shift* 0)

(defun get-x (x)
  (round (+ *border* (* (+ *x-shift* x) *scale*))))

(defun get-y (y)
  (round (- *scale* (- (* (+ *y-shift* y) *scale*) *border*))))

(defun draw-field (scene)
  (let ((a1 *border*)
        (a2 (+ *border* *scale*))
        (a3 (round (/ *border* 2)))
        (step (round (/ *scale* *steps*)))
        (group
         (make-group scene (:stroke "black"
                                    ;;:fill color :opacity alpha
                                    :stroke-width 1
                                    ;;:fill-opacity (* alpha 0.6)
                                    ;;:stroke-linecap "round"
                                    )))
        (group2
         (make-group scene (:stroke "gray"
                                    ;;:fill color :opacity alpha
                                    :stroke-width 1
                                    ;;:fill-opacity (* alpha 0.6)
                                    ;;:stroke-linecap "round"
                                    :stroke-dasharray "5,5"))))
    (draw group (:line :x1 a1 :y1 a1 :x2 a2 :y2 a1))
    (draw group (:line :x1 a1 :y1 a1 :x2 a1 :y2 a2))
    (draw group (:line :x1 a2 :y1 a2 :x2 a1 :y2 a2))
    (draw group (:line :x1 a2 :y1 a2 :x2 a2 :y2 a1))
    (loop for i from 1 to (1- *steps*)
       do (let ((cur-a (+ *border* (* i step))))
            (draw group2 (:line :x1 cur-a :y1 a3 :x2 cur-a :y2 (+ *scale* a3)))
            (draw group2 (:line :x1 a3 :y1 cur-a :x2 (+ *scale* a3) :y2 cur-a))
            (draw group (:line :x1 cur-a :y1 (- *border* a3) :x2 cur-a :y2 (+ *border* a3)))
            (draw group (:line :x1 (- *border* a3) :y1 cur-a :x2 (+ *border* a3) :y2 cur-a))
            (draw group (:line :x1 cur-a :y1 (+ *scale* (- *border* a3)) :x2 cur-a :y2 (+ *scale* (+ *border* a3))))
            (draw group (:line :x1 (+ *scale* (- *border* a3)) :y1 cur-a :x2 (+ *scale* (+ *border* a3)) :y2 cur-a))
            ))))

(defun draw-text (scene text point)
  (text scene (:x (get-x (x point)) :y (get-y (y point))) text))

(defun draw-point (scene point)
  (draw scene (:circle :cx (get-x (x point)) :cy (get-y (y point)) :r 2)))

(defun draw-polygon (scene polygon)
  (let* ((points (point-list polygon))
         (string-points (format nil "~{~A~^ ~}" (mapcar (lambda (point)
                                                          (format nil "~A,~A"
                                                                  (get-x (x point))
                                                                  (get-y (y point))))
                                                        points))))
    (draw scene (:polygon :points string-points :fill "orange"))))

(defun draw-line-segment (scene line-segment)
  (let ((group
         (make-group scene (:stroke "darkorange"
                                    ;;:fill color :opacity alpha
                                    :stroke-width 1
                                    ;;:fill-opacity (* alpha 0.6)
                                    ;;:stroke-linecap "round"
                                    ))))
    (draw group (:line :x1 (get-x (x (start line-segment)))
                       :y1 (get-y (y (start line-segment)))
                       :x2 (get-x (x (end line-segment)))
                       :y2 (get-y (y (end line-segment)))))))

(defun bounding-box-union (box1 box2)
  (when (cl-geometry::bounding-boxes-intersect-p box1 box2)
    (make-instance 'bounding-box
                   :x-min (min (cl-geometry::x-min box1)
                               (cl-geometry::x-min box2))
                   :x-max (max (cl-geometry::x-max box1)
                               (cl-geometry::x-max box2))
                   :y-min (min (cl-geometry::y-min box1)
                               (cl-geometry::y-min box2))
                   :y-max (max (cl-geometry::y-max box1)
                               (cl-geometry::y-max box2)))))

(defun draw-problem (problem &key (filename "~/field.svg"))
  (let* ((polygons (src/types:silhouette problem))
         (lines (src/types:skeleton problem))
         (bounding-box (reduce #'bounding-box-union (mapcar #'cl-geometry::construct-bounding-box polygons)))
         (*x-shift* (/ (1+ (+ (cl-geometry::x-min bounding-box) (cl-geometry::x-max bounding-box))) 2))
         (*y-shift* (/ (1+ (+ (cl-geometry::y-min bounding-box) (cl-geometry::y-max bounding-box))) 2))
         (max-size (max (- (cl-geometry::x-max bounding-box) (cl-geometry::x-min bounding-box))
                        (- (cl-geometry::y-max bounding-box) (cl-geometry::y-min bounding-box))))
         (*border* (if (> max-size 1)
                       (round (/ (* *scale* (- max-size 1)) 2))
                       *border*))
         (size (+ *scale* (* 2 *border*))))
    (with-svg-to-file
        (scene 'svg-1.1-toplevel :height size :width size)
        (filename :if-exists :supersede)
      (mapc (lambda (polygon) (draw-polygon scene polygon)) polygons)
      (mapc (lambda (line) (draw-line-segment scene line)) lines)
      (draw-field scene))))

(defun dump-problems (problem-folder)
  (mapc (lambda (problem-file)
          (handler-case
              (let ((problem (src/parser:parse-problem problem-file))
                    (svg-filename (format nil "~A.svg" problem-file)))
                (draw-problem problem :filename svg-filename))
            (error () (format t "~A~%" problem-file))))
        (directory problem-folder)))

