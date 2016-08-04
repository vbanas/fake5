(defpackage :src/utils
  (:use :common-lisp)
  (:export #:copy-instance))

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
