(defpackage :src/polygons
  (:use :common-lisp :src/utils :src/types :cl-geometry :anaphora)
  (:export #:reduce-triangles)
  )

(in-package :src/polygons)

(defclass origami ()
  ((polygons :initarg :polygons
             :initform nil)
   (holes :initarg :holes
          :initform nil)))

(defun triangles-intersect (ts1 ts2)
  (loop for t1 in ts1 append
       (loop for t2 in ts2 append
            (polygon-intersection t1 t2))))

(defun triangles-union (ts1 ts2)
  (loop for t1 in ts1 append
       (loop for t2 in ts2 append
            (polygon-union t1 t2))))

(defun line-segment-equal (l1 l2)
  (or (and (point-equal-p (start l1) (start l2))
           (point-equal-p (end l1) (end l2)))
      (and (point-equal-p (start l1) (end l2))
           (point-equal-p (end l1) (start l2)))))

(defun adjacent (p1 p2)
  (loop for l1 in (edge-list p1) do
       (awhen (find-if (lambda (l2) (line-segment-equal l1 l2)) (edge-list p2))
              (return-from adjacent (list (start it) (end it))))))

(defun shift-left (lst)
  (append (cdr lst) (list (car lst))))

(defun shift-right (lst)
  (format t "~A~%" lst)
  (append (last lst) (butlast lst)))

(defun merge-adjacent (p1 p2 ls) 
  (let (points)
    (labels ((%line-left? (lst)
               (or (and (point-equal-p (first lst) (first ls))
                        (point-equal-p (second lst) (second ls)))
                   (and (point-equal-p (first lst) (second ls))
                        (point-equal-p (second lst) (first ls))))) 
             (%shl-until (lst f &key (n 0))
               (when (>= n (length lst)) (error "!!!"))
               (if (funcall f lst)
                   lst
                   (%shl-until (shift-left lst) f :n (1+ n)))))
      (setf points (append (cdr (%shl-until (point-list p1) #'%line-left?))
                           (cdr (%shl-until (point-list p2) #'%line-left?))))
      (make-polygon-from-point-list points))))

(defun reduce-triangles (ts)
  (let* ((rest nil)
         (merged (reduce (lambda (p1 p2)
                           (aif (adjacent p1 p2)
                                (merge-adjacent p1 p2 it)
                                (progn (push p2 rest) p1)))
                         ts)))
    (if (= (length ts) (1+ (length rest)))
        (cons merged rest)
        (if rest
            (reduce-triangles (cons merged rest))
            (list merged)))))


