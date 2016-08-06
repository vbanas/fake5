(defpackage :src/cairo
  (:use :common-lisp :src/utils :cl-cairo2 :src/polygons :src/drawer :cl-geometry))

(in-package :src/cairo)

(defparameter *background-color* (list 1 1 1)) ;; white
(defparameter *polygon-color* (list 1 0 0)) ;; red
(defparameter *clockwise-polygon-color* (list 1 1 1)) ;; white
(defparameter *counterclockwise-polygon-color* (list 0 1 0)) ;; green

(defun set-source-color (color)
  (set-source-rgb (first color) (second color) (third color)))

(defparameter *scale* 100)

(defparameter *x-min* nil)
(defparameter *x-max* nil)
(defparameter *y-min* nil)
(defparameter *y-max* nil)
(defparameter *buffer-width* 100)
(defparameter *buffer-height* nil)


(defun draw-coordinates (point)
  (let ((res (cons (* *scale* (/ (- (cl-geometry:x point) *x-min*)
                                 (- *x-max* *x-min*)))
                   (* *scale* (/ (- (cl-geometry:y point) *y-min*)
                                 (- *y-max* *y-min*))))))
    (format t "~A~%" res)
    res))

(defgeneric draw-polygon-to-surface (polygon))

(defmethod draw-polygon-to-surface :before (polygon)
  (assert (and *x-min* *x-max* *y-min* *y-max*
               *buffer-width* *buffer-height*)))

(defmethod draw-polygon-to-surface ((polygon cl-geometry:polygon))
  (set-source-color *polygon-color*)
  (set-line-width 1)
  (let ((points (mapcar #'draw-coordinates (cl-geometry:point-list polygon))))
    (move-to (car (car points)) (cdr (car points)))
    (loop for p in (cdr points) do
         (line-to (car p) (cdr p)))
    (close-path)
    (stroke-preserve)
    (fill-path)))

(defun draw-polygons-to-surface (polygons) 
  (loop for p in polygons do
       (draw-polygon-to-surface p)))

(defun draw-solution-to-surfaces (solution-polygons silhouette-polygons
                                  &key (dump-to-png (list "/home/ihors/tmp/s.png"
                                                          "/home/ihors/tmp/p.png")))
  (let* ((bounding-box
          (reduce #'src/drawer::bounding-box-union
                  (mapcar #'cl-geometry::construct-bounding-box
                          (append solution-polygons silhouette-polygons))))
         (*x-min* (cl-geometry::x-min bounding-box))
         (*x-max* (cl-geometry::x-max bounding-box))
         (*y-min* (cl-geometry::y-min bounding-box))
         (*y-max* (cl-geometry::y-max bounding-box))
         (*buffer-height* (* *buffer-width*
                             (/ (- *y-max* *y-min*)
                                (- *x-max* *x-min*))))
         )
    (format t "Xmin = ~A~%Xmax = ~A~%Ymin = ~A~%Ymax = ~A~%H = ~A~%"
            *x-min* *x-max* *y-min* *y-max* *buffer-height*)
    ;; solution
    (let* ((surface (create-image-surface :rgb24 *buffer-width* *buffer-height*))
           (cl-cairo2::*context* (create-context surface)))
      (progn (set-source-color *background-color*)
             (paint)
             (draw-polygons-to-surface solution-polygons))
      (when dump-to-png
        (surface-write-to-png surface (car dump-to-png))))
    ))

