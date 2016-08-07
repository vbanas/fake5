(defpackage :src/cairo
  (:use :common-lisp :src/utils :cl-cairo2 :src/polygons :src/drawer :cl-geometry)
  (:export #:compute-score-for-polygons
           #:with-scale))

(in-package :src/cairo)

(defparameter *background-color* (list 1 1 1)) ;; white
(defparameter *polygon-color* (list 0 0 0)) ;; red
(defparameter *clockwise-polygon-color* *background-color*) 
(defparameter *counterclockwise-polygon-color* *polygon-color*) 

(defun set-source-color (color)
  (set-source-rgb (first color) (second color) (third color)))

(defparameter *scale* 64)

(defparameter *x-min* nil)
(defparameter *x-max* nil)
(defparameter *y-min* nil)
(defparameter *y-max* nil)
(defparameter *buffer-width* *scale*)
(defparameter *buffer-height* nil)

(defmacro with-scale ((scale) &body body)
  `(let* ((*scale* ,scale)
          (*buffer-width* *scale*))
     ,@body))

(defun draw-coordinates (point)
  (let ((res (cons (round (* *buffer-width* (/ (- (cl-geometry:x point) *x-min*)
                                               (- *x-max* *x-min*))))
                   (round (- *buffer-height*
                           (* *buffer-height*
                              (/ (- (cl-geometry:y point) *y-min*)
                                 (- *y-max* *y-min*))))))))
    ;; (format t "~A ~A -> ~A~%" (cl-geometry:x point) (cl-geometry:y point) res)
    res))

(defgeneric draw-polygon-to-surface (polygon))

(defmethod draw-polygon-to-surface :before (polygon)
  (assert (and *x-min* *x-max* *y-min* *y-max*
               *buffer-width* *buffer-height*)))

(defmethod draw-polygon-to-surface :around ((polygon src/types:clockwise-polygon))
  (let ((*polygon-color* *clockwise-polygon-color*))
    (call-next-method)))

(defmethod draw-polygon-to-surface :around ((polygon src/types:counterclockwise-polygon))
  (let ((*polygon-color* *counterclockwise-polygon-color*))
    (call-next-method)))

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

(defun make-image ()
  (create-image-surface :rgb24 *buffer-width* *buffer-height*))

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
     (get-filled-surface solution-polygons (first dump-to-png))
     
     ;; silhouette
     (get-filled-surface silhouette-polygons (second dump-to-png)))))

(defun get-filled-surface (polygons dump-to-png)
  (let* ((surface (make-image))
         (cl-cairo2::*context* (create-context surface)))
    (unwind-protect
         (progn (cairo:set-antialias :none)
                (set-source-color *background-color*)
                (paint)
                (draw-polygons-to-surface polygons))
      (destroy cl-cairo2::*context*))
    (when dump-to-png
      (surface-write-to-png surface dump-to-png))
    surface))

(defun compute-score-for-buffers (buffer1 buffer2)
  (cl-cairo2::with-cairo-object (buffer1 pointer1)
    (cl-cairo2::with-cairo-object (buffer2 pointer2) 
      (let* ((count-and 0)
             (count-or 0)
             (white 255)
             (data-pointer1 (cl-cairo2::cairo_image_surface_get_data pointer1))
             (data-pointer2 (cl-cairo2::cairo_image_surface_get_data pointer2))
             (width (image-surface-get-width buffer1))
             (height (image-surface-get-height buffer1))
             (bytes-per-pixel (cl-cairo2::get-bytes-per-pixel (cl-cairo2::image-surface-get-format buffer1))))
        (loop for i from 0 below (* width height bytes-per-pixel) by 4 do
             (let ((c1 (cffi:mem-ref data-pointer1 :uint8 i))
                   (c2 (cffi:mem-ref data-pointer2 :uint8 i)))
               (cond ((and (= c1 c2)
                           (not (= c1 white)))
                      (incf count-and)
                      (incf count-or))
                     ((or (and (= c1 white)
                               (not (= c2 white)))
                          (and (= c2 white)
                               (not (= c1 white))))
                      (incf count-or))
                     (t nil))))
        (values count-and count-or)))))

(defun compute-score-for-polygons (ps1 ps2)
  (multiple-value-bind (s1 s2)
      (draw-solution-to-surfaces ps1 ps2)
    (unwind-protect
         (multiple-value-bind (count-and count-or)
             (compute-score-for-buffers s1 s2)
           (if (zerop count-or)
               0
               (/ count-and count-or)))
      (destroy s1)
      (destroy s2))))
