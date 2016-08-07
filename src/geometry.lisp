(defpackage :src/geometry
  (:use :common-lisp 
        :cl-geometry
        :src/utils
        :src/polygons
        :src/matrix
        :src/drawer
        :src/parser
        :src/types
        :anaphora)
  (:import-from :cl-geometry
                :point-equal-p)
  (:import-from :cl-containers)
  (:export #:classify-polygon-edges
           #:direction-from-edge))

(in-package :src/geometry)

(defun point-earlier (p1 p2)
  (if (= (x p1) (x p2))
      (< (y p1) (y p2))
      (< (x p1) (x p2))))

(defun make-initial-heap (polygon)
  (let ((heap (make-instance 'cl-heap:priority-queue
                             :sort-fun #'point-earlier))
        (edge-list (edge-list polygon)))
    (loop for (edge1 edge2) on edge-list do
         (let ((edge2 (or edge2
                          (first edge-list)))
               (point (end edge1)))
           (cl-heap:enqueue
            heap
            (list point
                  (remove-if-not
                   (lambda (edge)
                     (eq (left-point edge)
                         point))
                   (list edge1 edge2)))
            point)))
    heap))

(defun left-point (edge)
  (with-slots (start end) edge
    (cond ((< (x start)
              (x end))
           start)
          ((> (x start)
              (x end))
           end)
          (t (if (< (y start)
                    (y end))
                 start
                 end)))))

(defun right-point (edge)
  (with-slots (start end) edge
    (let ((left (left-point edge)))
      (if (eq left start)
          end
          start))))

(defun edge-is-below (edge1 edge2)
  (if (eq (left-point edge1)
          (left-point edge2))
      (let ((min-x (min (x (right-point edge1))
                        (x (right-point edge2))))
            (line1 (line-from-segment edge1))
            (line2 (line-from-segment edge2)))
        (if (or (= 0 (b line1))
                (= 0 (b line2)))
            (< (y (right-point edge1))
               (y (right-point edge2)))
            (< (line-y-at-x line1 min-x)
               (line-y-at-x line2 min-x))))
      (< (y (left-point edge1))
         (y (left-point edge2)))))

(defun add-edge-sorted (edge list)
  (cond
    ((null list) (list edge))
    ((edge-is-below edge (car list))
     (cons edge list))
    (t (cons (car list)
             (add-edge-sorted edge (cdr list))))))

(defun other-class (class)
  (case class
    (:entry :exit)
    (:exit :entry)))

(defun classify-edge (edge list class)
  (if (eq edge (car list))
      class
      (classify-edge edge (cdr list) (other-class class))))

(defun do-scan (heap)
  (let ((res-tab (make-hash-table :test #'eq))
        scan-line)
    (loop while (not (= (cl-heap:queue-size heap) 0)) do
         ;; 'edges' contains only edges that start from 'point'
         (destructuring-bind (point edges)
             (cl-heap:dequeue heap)
           (setf scan-line
                 (remove-if (lambda (edge)
                              (eq (right-point edge)
                                  point))
                            scan-line))
           (loop for edge in edges do
                (setf scan-line (add-edge-sorted edge scan-line)))
           (loop for edge in edges do
                (let ((class (classify-edge edge scan-line :entry)))
                  (setf (gethash edge res-tab)
                        class)))))
    res-tab))

(defun classify-polygon-edges (polygon)
  (do-scan (make-initial-heap polygon)))

(defun classify-problem (filename svg-filename)
  (let* ((poly (car (silhouette (parse-problem filename))))
         (tab (classify-polygon-edges poly)))
    (draw-classified-polygon poly tab svg-filename)))

(defun direction-from-edge (edge class)
  (with-slots (start end) edge
    (ecase class
      (:entry (if (= (x start)
                     (x end))
                  (copy-instance
                   start
                   :x (- (x start) 1))
                  (copy-instance
                   start
                   :y (+ (y start) 1))))
      (:exit (if (= (x start)
                    (x end))
                 (copy-instance
                  start
                  :x (+ (x start) 1))
                 (copy-instance
                  start
                  :y (- (y start) 1)))))))
