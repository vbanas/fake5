(defpackage :src/utils
  (:use :common-lisp :cl-geometry :src/types :cl-containers :anaphora)
  (:export #:copy-instance)
  (:export #:polygons->problem)
  (:export #:update-skeleton-with-intersection))

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
                   (push (make-instance 'line-segment :start (start first) :end it) lines)
                   (push (make-instance 'line-segment :start (end first) :end it) lines)
                   (push (make-instance 'line-segment :start (start edge) :end it) lines)
                   (push (make-instance 'line-segment :start (end edge) :end it) lines)
                   (delete-item queue edge)
                   (return-from iteration))))
              (push first lines))))
    (make-instance 'problem
                   :silhouette (silhouette problem)
                   :skeleton lines)))
