(defpackage :src/normalization
  (:nicknames :src/norm)
  (:use :common-lisp :src/utils :src/types :cl-geometry
        :src/polygons :src/simple-state :anaphora)
  (:export #:normalized-solution?)
  )

(in-package :src/norm)

(defun normalized-solution? (solution)
  (let ((polygon-pairs
         (mapcar (lambda (p)
                   (cons (make-polygon-from-point-list
                          (mapcar #'src/simple-state::orig-point (point-list p)))
                         p))
                 solution)))
    (labels ((%sign (line p)
               (multiple-value-bind (_ sign)
                   (src/simple-state::find-non-collinear-point line p)
                 (declare (ignore _)) 
                 sign)))
      (loop for (source-p . destination-p) in polygon-pairs do
           (let* ((adjacent-at-source
                   (remove nil
                           (loop for (sp . dp) in polygon-pairs collect
                                (awhen (src/polygons::adjacent source-p sp)
                                       (cons dp
                                             (line-from-segment
                                              (make-instance 'cl-geometry:line-segment
                                                             :start (first it)
                                                             :end (second it)))))))))
             (loop for (p . line) in adjacent-at-source 
                unless (eq p destination-p)
                do (when (< (* (%sign line p)
                               (%sign line destination-p))
                            0)
                     ;; (format t "p1 = ~{~A ~}~%p2 = ~{~A ~}~%line = ~A~%edge = ~A~%"
                     ;;         (point-list p)
                     ;;         (point-list destination-p)
                     ;;         line
                     ;;         (src/polygons::adjacent p destination-p))
                     (return-from normalized-solution?
                       (values nil p destination-p (src/polygons::adjacent p destination-p))))))))
    t))

(defun normalize-solution (solution)
  (multiple-value-bind (normalized? p1 p2 start-end)
      (normalized-solution? solution)
    (if normalized?
        solution
        (progn
          (format t "NORMALIZING!~%")
          (normalize-solution
           (cons (src/polygons::merge-adjacent p1 p2 start-end)
                 (remove p2 (remove p1 solution :test #'eq) :test #'eq)))))))

