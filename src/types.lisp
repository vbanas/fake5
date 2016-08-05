(defpackage :src/types
  (:use :common-lisp :src/utils :cl-geometry)
  (:export #:problem
           #:silhouette
           #:skeleton
           #:clockwise-polygon
           #:counterclockwise-polygon)
  )

(in-package :src/types)

(defclass problem ()
  ((silhouette :accessor silhouette
               :initarg :silhouette
               :initform nil) ;; list of polygons
   (skeleton :accessor skeleton
             :initarg :skeleton
             :initform nil)) ;; list of line-segments
  )

(defclass clockwise-polygon (polygon) ())
(defclass counterclockwise-polygon (polygon) ())


