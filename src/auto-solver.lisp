(defpackage :src/auto-solver
  (:use :common-lisp 
        :cl-geometry
        :src/drawer
        :src/utils
        :src/drawer
        :src/polygons
        :src/matrix
        :src/mcts
        :src/cairo 
        :src/parser
        :src/printer
        :src/types
        :src/simple-state)
  (:import-from :cl-geometry
                :point-equal-p)
  (:import-from :trivial-timeout)
  (:export :auto-solve))

(in-package :src/auto-solver)

(defun auto-solve (problem-folder solution-folder)
  (mapcar (lambda (problem-file)
            
            (let* ((name (pathname-name problem-file))
                   (directory (make-pathname :directory
                                             (pathname-directory problem-file)))
                   (dest (format nil "~A~A.txt" solution-folder name)))
              (unless (or (probe-file (format nil "~A../solved-problems/~A.txt" directory name))
                          (probe-file (format nil "~A../our-problems/~A.txt" directory name)))
                (format t "~%-> ~A~%" (pathname-name problem-file))
                (cons name (src/simple-state::solve problem-file dest :timeout 5)))))
          (directory problem-folder)))

;; (defun dump-res-to-file (res res-file)
;;   (with-open-file (stream res-file)))
