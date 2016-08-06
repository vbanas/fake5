(defpackage :src/cairo
  (:use :common-lisp :src/utils :cl-cairo2 :src/polygons :src/drawer :cl-geometry)
  (:export #:compute-score-for-polygons))

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
  (let ((res (cons (round (* *scale* (/ (- (cl-geometry:x point) *x-min*)
                                        (- *x-max* *x-min*))))
                   (round (* *scale* (/ (- (cl-geometry:y point) *y-min*)
                                        (- *y-max* *y-min*)))))))
     ;; (format t "~A ~A -> ~A~%" (cl-geometry:x point) (cl-geometry:y point) res)
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
                                  &key (dump-to-png nil))
  (let* ((bounding-box
          (reduce #'src/drawer::bounding-box-union
                  (mapcar #'cl-geometry::construct-bounding-box
                          (append solution-polygons silhouette-polygons))))
         (*x-min* (cl-geometry::x-min bounding-box))
         (*x-max* (cl-geometry::x-max bounding-box))
         (*y-min* (cl-geometry::y-min bounding-box))
         (*y-max* (cl-geometry::y-max bounding-box))
         (*buffer-height* (round (* *buffer-width*
                                    (/ (- *y-max* *y-min*)
                                       (- *x-max* *x-min*)))))
         )
    ;; (format t "Xmin = ~A~%Xmax = ~A~%Ymin = ~A~%Ymax = ~A~%H = ~A~%"
    ;;         *x-min* *x-max* *y-min* *y-max* *buffer-height*)
    (values
     ;; solution
     (let* ((surface (create-image-surface :rgb24 *buffer-width* *buffer-height*))
            (cl-cairo2::*context* (create-context surface)))
       (progn (cairo:set-antialias :none)
              (set-source-color *background-color*)
              (paint)
              (draw-polygons-to-surface solution-polygons))
       (when dump-to-png
         (surface-write-to-png surface (first dump-to-png)))
       surface)
     
     ;; silhouette
     (let* ((surface (create-image-surface :rgb24 *buffer-width* *buffer-height*))
            (cl-cairo2::*context* (create-context surface))
            ;; (*polygon-color* (list 0 1 0))
            ;; green
            )
       (progn (cairo:set-antialias :none)
              (set-source-color *background-color*)
              (paint)
              (draw-polygons-to-surface silhouette-polygons))
       (when dump-to-png
         (surface-write-to-png surface (second dump-to-png)))
       surface))))

(defun compute-score-for-buffers (buffer1 buffer2)
  (let ((c1 nil) (c2 nil)
        (count-and 0)
        (count-or 0)
        (white (list 255 255 255 255)))
    (loop for x across buffer1
       for y across buffer2 do
       ;; (format t "~A ~A~%" c1 c2)
         (push x c1) (push y c2)
         (when (and (= (length c1) 4)
                    (= (length c2) 4)) 
           ;; (format t "c1 = ~A; c2 = ~A;~%" c1 c2)
           (cond ((and (equal c1 c2)
                       (not (equal c1 white)))
                  (incf count-and)
                  (incf count-or))
                 ((or (and (equal c1 white)
                           (not (equal c2 white)))
                      (and (equal c2 white)
                           (not (equal c1 white))))
                  (incf count-or))
                 (t nil))
           (setf c1 nil) (setf c2 nil)))
    (values count-and count-or)))

(defun compute-score-for-polygons (ps1 ps2)
  (multiple-value-bind (s1 s2)
      (draw-solution-to-surfaces ps1 ps2)
    (multiple-value-bind (count-and count-or)
        (compute-score-for-buffers
         (image-surface-get-data s1)
         (image-surface-get-data s2))
      (/ count-and count-or))))
