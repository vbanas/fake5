(defpackage :src/test/solve
  (:use :common-lisp :src/utils :src/simple-state
        :lisp-unit :cl-quickcheck))

(in-package :src/test/solve)


(defun solve-folder-with-stats (folder solution-folder stats-file &key (timeout 4))
  (with-open-file
      (fs stats-file
          :direction :output
          :if-exists :supersede
          :if-does-not-exist :create)
    (loop for filename in (cl:directory folder) do
         (let* ((problem (src/parser:parse-problem filename))
                (target-polygons (src/types:silhouette problem))
                )
           (multiple-value-bind (solution matrix)
               (src/simple-state::solve filename
                                        (format nil "~A/~A.txt"
                                                solution-folder
                                                (pathname-name filename))
                                        :timeout timeout)
             (let ((str (with-output-to-string (str) 
                          (src/printer:print-solution solution :matrix matrix :stream str))))
               (format fs "Task: ~A; Solution size: ~A; Resemblance: ~,3F; Score: ~,3F~%"
                       (pathname-name filename)
                       (length str)
                       (+ 0.0 (src/cairo:compute-score-for-polygons
                               solution target-polygons)) 
                       (+ 0.0 (src/simple-state::get-field-score solution target-polygons))))
             ))
         )))

