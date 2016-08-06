(defpackage :src/utils
  (:use :common-lisp :cl-geometry :src/types :cl-containers :anaphora)
  (:export #:copy-instance)
  (:export #:update-skeleton-with-intersection)
  (:export #:polygons->problem
           #:split-list-at
           #:point-with-origin
           #:orig-point))

(in-package :src/utils)
;; Taken from
;; http://stackoverflow.com/questions/11067899/is-there-a-generic-method-for-cloning-clos-objects
;; By blambert
(defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
  (:documentation "Makes and returns a shallow copy of OBJECT.

  An uninitialized object of the same class as OBJECT is allocated by
  calling ALLOCATE-INSTANCE.  For all slots returned by
  CLASS-SLOTS, the returned object has the
  same slot values and slot-unbound status as OBJECT.

  REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
  (:method ((object standard-object) &rest initargs &key &allow-other-keys)
    (let* ((class (class-of object))
           (copy (allocate-instance class)))
      (dolist (slot-name (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class)))
    (when (slot-boundp object slot-name)
      (setf (slot-value copy slot-name)
        (slot-value object slot-name))))
      (apply #'reinitialize-instance copy initargs))))

(defun polygons->problem (polygons)
  (let ((lines))
    (mapc (lambda (polygon)
            (mapc (lambda (segment) (push segment lines)) (edge-list polygon)))
          polygons)
    (make-instance 'problem
                   :silhouette polygons
                   :skeleton lines)))

(defun update-skeleton-with-intersection (problem)
  (let ((lines)
        (queue (make-instance 'basic-queue)))
    (mapc (lambda (edge) (insert-item queue edge)) (skeleton problem))
    (loop while (not (empty-p queue))
       do (let ((first (first-element queue)))
            (delete-first queue)
            (block iteration
              (iterate-nodes
               queue
               (lambda (edge)
                 (awhen (line-segments-intersection-point first edge :exclude-endpoints t)
                   (insert-item queue (make-instance 'line-segment :start (start first) :end it))
                   (insert-item queue (make-instance 'line-segment :start it :end (end first)))
                   (insert-item queue (make-instance 'line-segment :start (start edge) :end it))
                   (insert-item queue (make-instance 'line-segment :start it :end (end edge)))
                   (delete-item queue edge)
                   (return-from iteration))))
              (push first lines))))
    (make-instance 'problem
                   :silhouette (silhouette problem)
                   :skeleton lines)))

(defun get-point-to-edge-ht (edges)
  (let ((ht (make-hash-table :test #'equal)))
    (mapc (lambda (edge)
            (push (gethash (cons (x (start edge)) (y (start edge))) ht) edge)
            (push (gethash (cons (x (end edge)) (y (end edge))) ht) edge))
          edges)
    ht))

;; (defun update-silhouette-with-intersection (problem)
;;   (let ((queue (make-instance 'basic-queue))
;;         (point-to-edge-ht (get-point-to-edge-ht (skeleton problem))))
;;     (mapc (lambda (edge) (insert-item queue edge)) (skeleton problem))
;;     (loop while (not (empty-p queue))
;;        do (let ((first (first-element)))
;;             (delete-first queue)
;;             ()))))

(defun split-list-at (x lst &key (test #'equal) acc)
  (when lst
    (let ((head (car lst)))
      (if (funcall test x head)
          (values (reverse (cons head acc)) (cdr lst))
          (split-list-at x (cdr lst) :acc (cons head acc))))))

;; (defun get-region-color (region)
;;   (or (gethash region *region-color-ht*)
;;       (let* ((rs (make-random-state t))
;;              (color (format nil "#~x~x~x" (+ 127 (random 128 rs)) (+ 127 (random 128 rs)) (+ 127 (random 128 rs)))))
;;         (setf (gethash region *region-color-ht*) color)
;;         color)))

(defclass point-with-origin (cl-geometry::point)
  ((orig-point :initarg :orig-point
               :accessor orig-point)))
