(defpackage :src/simple-state
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
        :anaphora)
  (:import-from :cl-geometry
                :point-equal-p)
  (:import-from :cl-containers)
  (:export :orig-point
           :field
           :adjustment-matrix
           :target-field))

(in-package :src/simple-state)

(defparameter *use-partial-folds* nil)

(defun save-origin (point)
  (make-instance 'point-with-origin
                 :x (x point)
                 :y (y point)
                 :orig-point point))

(defun mirror-point (point line)
  (let* ((c (- (* (B line) (x point)) (* (A line) (y point))))
         (pline (make-instance 'line :a (- (B line)) :b (A line) :c c))
         (ipoint (lines-intersection-point line pline))
         (mirror-point (copy-instance point
                        :x (- (* 2 (x ipoint)) (x point))
                        :y (- (* 2 (y ipoint)) (y point)))))
    mirror-point))

(defun test-mirror-point ()
  (labels ((%test (&key a b (c 0)  x y mx my)
             (let* ((l (make-instance 'line :a a :b b :c c))
                    (p (make-instance 'point-with-origin
                                      :x x :y y
                                      :orig-point (make-instance 'point
                                                                 :x x
                                                                 :y y)))
                    (m (mirror-point p l)))
               (assert (= (x m) mx))
               (assert (= (y m) my)))))
    (%test :a 1 :b -1 :x 0 :y 1 :mx 1 :my 0)
    (%test :a 1 :b 0 :c -1 :x 0 :y 1 :mx 2 :my 1)
    (%test :a 0 :b 1 :c -1 :x 1 :y 0 :mx 1 :my 2)
    t))

(defun cl-geometry::between-p (a b c)
  "Is c colinear with a->b and lies between them?"
  (when (cl-geometry::colinear-p a b c)
    (if (= (x a)(x b))
        (or (and (>= (y c)(y a))
                 (<= (y c)(y b)))
            (and (>= (y c)(y b))
                 (<= (y c)(y a))))
        (or (and (>= (x c)(x a))
                 (<= (x c)(x b)))
            (and (>= (x c)(x b))
                 (<= (x c)(x a)))))))

(defun really-between-p (a b c)
  (and (cl-geometry::between-p a b c)
       (not (cl-geometry::point-equal-p a c))
       (not (cl-geometry::point-equal-p b c))))

(defun interpolate-coord-helper (start end orig-start orig-end coord)
  (if (= orig-start orig-end)
      orig-end
      (+ orig-start
         (* (- orig-end orig-start)
            (/ (- coord start)
               (- end start))))))

(defun interpolate-coord (start-point end-point orig-start orig-end point)
  (if (= (x start-point) (x end-point))
      (interpolate-coord-helper (y start-point) (y end-point) orig-start orig-end (y point))
      (interpolate-coord-helper (x start-point) (x end-point) orig-start orig-end (x point))))

(defun interpolate-origin (start end point)
  (if (and (typep start 'point-with-origin)
           (typep end 'point-with-origin))
      (let* ((orig-start (orig-point start))
             (orig-end (orig-point end))
             (orig-x (interpolate-coord start end (x orig-start) (x orig-end) point))
             (orig-y (interpolate-coord start end (y orig-start) (y orig-end) point)))
        (make-instance 'point-with-origin
                       :x (x point)
                       :y (y point)
                       :orig-point (make-instance 'point :x orig-x :y orig-y)))
      point))

(defun make-polygon-from-coords-with-origins (&rest coord-list)
  (make-polygon-from-point-list (mapcar #'save-origin (apply #'coords-to-points coord-list))))

(defparameter *split-polygon-cache* (make-hash-table :test #'equal))

(defun split-polygon (polygon line)
  (let ((key (list polygon (a line) (b line) (c line))))
    (or (gethash key *split-polygon-cache*)
        (let ((result (split-polygon-aux polygon line)))
          (setf (gethash key *split-polygon-cache*) result)
          result))))

(defun split-polygon-aux (polygon line)
  (let ((points-1 nil)
        (points-2 nil)
        (points-on-line 0))
    (labels ((%push (pt)
               (let ((sign (line-equation-res line pt)))
                 (cond
                   ((> sign 0) (push pt points-1))
                   ((< sign 0) (push pt points-2))
                   (t
                    (incf points-on-line)
                    (push pt points-1)
                    (push pt points-2))))))
      (loop for edge in (edge-list polygon) do
           (let* ((edge-line (line-from-segment edge))
                  (ipoint (lines-intersection-point line edge-line)))
             (if (and ipoint
                      (cl-geometry::between-p (start edge)
                                              (end edge)
                                              ipoint))
                 (progn
                   (%push (start edge))
                   (unless (or (point-equal-p ipoint (start edge))
                               (point-equal-p ipoint (end edge)))
                     (%push (interpolate-origin (start edge) (end edge) ipoint))))
                 (%push (start edge))))))
    (if (or (and points-2
                 (not (= (length points-2) points-on-line))
                 points-1
                 (not (= (length points-1) points-on-line))))
        (list (make-polygon-from-point-list (reverse points-1))
              (make-polygon-from-point-list (reverse points-2)))
        (list polygon))))

(defun test-split-polygon ()
  (labels ((%test (&key a b c coords expected)
             (let* ((p (apply #'make-polygon-from-coords-with-origins coords))
                    (l (make-instance 'line :a a :b b :c c))
                    (res (split-polygon p l)))
               (assert1 (mapcar #'polygon->list res) expected))))
    (%test :a 0 :b 1 :c -1
           :coords '(0 0 0 2 2 2 2 0)
           :expected '((0 1 0 2 2 2 2 1)
                       (0 0 0 1 2 1 2 0)))
    (%test :a 0 :b 1 :c -2
           :coords '(0 0 0 2 2 2 2 0)
           :expected '((0 0 0 2 2 2 2 0)))
    (%test :a -1 :b 1 :c (- (/ 1 4))
           :coords '(0 0 0 1 1 1 1 0)
           :expected '((0 1/4 0 1 3/4 1)
                       (0 0 0 1/4 3/4 1 1 1 1 0)))))

(defun mirror-polygon (polygon line)
  (make-polygon-from-point-list
   (mapcar
    (lambda (pt) (mirror-point pt line))
    (reverse
     (point-list polygon)))))

(defun line-equation-res (line point)
  (+
   (* (A line) (x point))
   (* (B line) (y point))
   (C line)))

(defun fold-polygon (polygon line point)
  (let ((split-list (split-polygon polygon line))
        (point-sign (line-equation-res line point)))
    (mapcar
     (lambda (polygon)
       (let ((sign
              (nth-value 1 (find-non-collinear-point line polygon))))
         (if (>= (* sign point-sign) 0)
             polygon
             (mirror-polygon polygon line))))
     split-list)))

(defun fold-polygon-test ()
  (labels ((%test (&key a b c x y coords expected)
             (let* ((p (apply #'make-polygon-from-coords-with-origins coords))
                    (l (make-instance 'line :a a :b b :c c))
                    (res (fold-polygon
                          p l
                          (make-instance 'point :x x :y y))))
               (assert1 (mapcar #'polygon->list res) expected))))
    (%test :a 0 :b 1 :c -1
           :x 0 :y 0
           :coords '(0 0 0 2 2 2 2 0)
           :expected '((2 1 2 0 0 0 0 1)
                       (0 0 0 1 2 1 2 0)))
    (%test :a 0 :b 1 :c -1
           :x 2 :y 2
           :coords '(0 0 0 2 2 2 2 0)
           :expected '((0 1 0 2 2 2 2 1)
                       (2 2 2 1 0 1 0 2)))
    (%test :a 0 :b 1 :c -2
           :x 0 :y 0
           :coords '(0 0 0 2 2 2 2 0)
           :expected '((0 0 0 2 2 2 2 0)))
    (%test :a 0 :b 1 :c -2
           :x 3 :y 3
           :coords '(0 0 0 2 2 2 2 0)
           :expected '((2 4 2 2 0 2 0 4)))
    (%test :a -1 :b 1 :c (- (/ 1 4))
           :x 1 :y 0
           :coords '(0 0 0 1 1 1 1 0)
           :expected '((3/4 1 3/4 1/4 0 1/4)
                       (0 0 0 1/4 3/4 1 1 1 1 0)))
    (assert1
     (mapcar
      #'polygon->list
      (fold-quad `((:a -1 :b 1 :c ,(- (/ 1 4)) :x 1 :y 0)
                   (:a 1 :b 1 :c ,(- (/ 1 4)) :x 0 :y 2)
                   (:a 1 :b 1 :c ,(- (/ 1 3)) :x 0 :y 2)
                   (:a 1 :b 0 :c ,(- (/ 2 3)) :x 0 :y 2)
                   (:a 0 :b 1 :c ,(- (/ 2 3)) :x 0 :y 0))))
     '((7/12 1/3 7/12 2/3 2/3 2/3 2/3 5/12)
       (2/3 2/3 2/3 1/4 7/12 1/4 7/12 2/3)
       (2/3 2/3 2/3 5/12 5/12 2/3)
       (2/3 1/4 1/12 1/4 1/24 7/24 5/12 2/3 2/3 2/3)
       (1/24 7/24 1/12 1/3 1/12 1/4)
       (2/3 2/3 2/3 5/12 7/12 1/3 1/3 1/3 1/3 2/3)
       (2/3 0 1/3 0 1/3 2/3 2/3 2/3)
       (2/3 2/3 2/3 5/12 5/12 2/3)
       (1/24 7/24 5/12 2/3 2/3 2/3 2/3 0 1/3 0)
       (1/3 1/12 1/3 0 1/24 7/24 1/12 1/3)
       (1/12 1/4 1/4 1/4 1/4 1/12)
       (1/4 1/12 1/12 1/4 1/12 1/3 1/3 1/12)))
    ))

(defun fold-polygon-list (polygon-list line point)
  (alexandria:mappend
   (lambda (polygon)
     (fold-polygon polygon line point))
   polygon-list))

(defun fold-some-polygons-in-list (polygon-list line point to-fold-table)
  (alexandria:mappend
   (lambda (polygon)
     (if (gethash polygon to-fold-table)
         (fold-polygon polygon line point)
         (list polygon)))
   polygon-list))

(defun fold-quad (fold-specs)
  (let* ((quad (make-polygon-from-coords-with-origins 0 0 0 1 1 1 1 0))
         (result (reduce (lambda (q fold-spec)
                           (destructuring-bind (&key a b (c 0) x y)
                               fold-spec
                             (fold-polygon-list
                              q
                              (make-instance 'line :a a :b b :c c)
                              (make-instance 'point :x x :y y))))
                         fold-specs
                         :initial-value (list quad))))
    result))

(defun fold-quad-and-show (file fold-specs &key animate)
  (labels ((%once (file fold-specs)
             (let* ((result (fold-quad fold-specs)))
               ;; (format t "result: ~A~%"
               ;;         (mapcar #'polygon->list result))
               (draw-polygons-to-svg
                result :filename file)
               result)))
    (let ((file-pn (pathname file)))
      (if animate
          (progn
            (loop for i from 0 to (length fold-specs) do
                 (%once (make-pathname
                         :defaults file-pn
                         :name (format nil "~A~A"
                                       (pathname-name file-pn) i))
                        (subseq fold-specs 0 i)))
            (fold-quad fold-specs))
          (%once file fold-specs)))))

(defun area-simple-polygon (polygon)
  "Calculate an area of a simple polygon."
  (* 1/2
     (polygon-orientation polygon)
     (reduce #'+ (maplist #'(lambda (list)
                              (let ((v1 (car list))
                                    (v2 (if (cdr list)
                                            (cadr list)
                                            (car list))))
                                (- (* (x v1)(y v2))(* (x v2)(y v1)))))
                          (point-list polygon)))))

(defun single? (lst)
  (and lst (null (cdr lst))))

(defun intersect-and-reduce (polygons)
  (reduce
   (lambda (ps p2)
     (loop for p1 in ps append
          (reduce-triangles (polygon-intersection p1 p2))))
   (cdr polygons)
   :initial-value (list (car polygons))))

(defun union-and-reduce (polygons)
  (reduce
   (lambda (ps p2)
     (loop for p1 in ps append
          (reduce-triangles (polygon-union p1 p2))))
   (cdr polygons)
   :initial-value (list (car polygons))))

;; (defun score (polygons target-polygons)
;;   (let* ((polygons (union-and-reduce polygons))
;;          (intersected-area-ps (intersect-and-reduce (append polygons target-polygons)))
;;          (united-area-ps (union-and-reduce (append polygons target-polygons))))
;;     (/ (reduce #'+ (mapcar #'abs (mapcar #'area-simple-polygon intersected-area-ps)))
;;        ;; (reduce #'+ (mapcar #'abs (mapcar #'area-simple-polygon united-area-ps)))
;;        1.0)))

(defclass game-state ()
  ((field :type list
          :documentation "All polygons on the field"
          :accessor field
          :initarg :field)
   (field-score :type number
                :documentation "Score for mcts"
                :accessor field-score
                :initarg :field-score)
   (resemblance :type number
                :documentation "Score by area"
                :accessor resemblance
                :initarg :resemblance)
   (adjustment-matrix :type list
                      :documentation "list of lists of values, describes translation of original problem"
                      :accessor adjustment-matrix
                      :initarg :adjustment-matrix)
   (target-field :type list
                 :documentation "List of polygons describing desired final state"
                 :accessor target-field
                 :initarg :target-field)
   (adjusted :type t
             :accessor adjusted
             :initarg :adjusted
             :initform nil)
   (possible-adjustment-matrices :type list
                                 :documentation "List of all matrices to consider at first move"
                                 :accessor possible-adjustment-matrices
                                 :initarg :possible-adjustment-matrices)))

(defun read-task-state (filename)
  (let* ((problem (parse-problem filename))
         (start (make-polygon-from-coords-with-origins 0 0 0 1 1 1 1 0))
         (bbox (reduce #'src/drawer::bounding-box-union
                       (mapcar #'cl-geometry::construct-bounding-box
                               (silhouette problem))))
         (bb-matrix (translate-matrix (- (cl-geometry::x-min bbox))
                                      (- (cl-geometry::y-min bbox))))
         (silhouette (mult-polygons-matrix (silhouette problem) bb-matrix))
         (possible-adj-matrs (or (polygons-right-matrices silhouette)
                                 (list (identity-tr-matrix)))
           ))
    (make-instance 'game-state
                   :field (list start)
                   :adjustment-matrix bb-matrix
                   :target-field silhouette
                   :field-score 0
                   :resemblance 0
                   :possible-adjustment-matrices possible-adj-matrs)))

(defclass problem-spec ()
  ((root-state :initarg :root-state
               :accessor root-state)
   (iters-count :initarg :iters-count
                :accessor iters-count)
   (iters-per-move :initarg :iters-per-move
                   :accessor iters-per-move)))

(defun solve-once-0 (problem-spec)
  (clrhash *split-polygon-cache*)
  (let* ((state (root-state problem-spec))
         (best-state state)
         (iteration 0))
    (loop while (and (< (field-score state) 1)
                     (< iteration (iters-count problem-spec)))
       do
         (incf iteration)
         (let ((move (get-best-move
                      (select-next-move
                       (make-node-for-state state) state
                       (iters-per-move problem-spec)))))
           (unless move
             (return))
           (setf state (next-state state move))
           (when (> (field-score state)
                    (field-score best-state))
             (setf best-state state))))
    best-state))

(defun solve-once-1 (problem-spec)
  (clrhash *split-polygon-cache*)
  (let* ((state (root-state problem-spec))
         (mcts-root (make-node-for-state state))
         (best-state state)
         (iteration 0))
    (loop while (and (< (field-score state) 1)
                     (< iteration (iters-count problem-spec)))
       do
         (incf iteration)
         (setf mcts-root (get-best-child
                          (select-next-move
                           mcts-root state
                           (iters-per-move problem-spec))))
         (when (null (src/mcts::action mcts-root))
           (return))
         (setf state (next-state
                      state (src/mcts::action mcts-root)))
         (when (> (field-score state)
                  (field-score best-state))
           (setf best-state state)))
    best-state))

(defun solve-once-2 (problem-spec)
  (clrhash *split-polygon-cache*)
  (let* ((root-state (root-state problem-spec))
         (mcts-root (select-next-move
                     (make-node-for-state root-state)
                     root-state (iters-per-move problem-spec)))
         (actions (get-best-moves-chain mcts-root)))
    (reduce #'next-state actions
            :initial-value root-state)))

(defun collect-solution-scores (dir &key iters-count iters-per-move)
  (labels ((%file (file)
             (when (pathname-name file)
               (let* ((spec (make-instance 'problem-spec
                                           :root-state (read-task-state file)
                                           :iters-count iters-count
                                           :iters-per-move iters-per-move))
                      (spec-2 (make-instance 'problem-spec
                                             :root-state (read-task-state file)
                                             :iters-count iters-count
                                             :iters-per-move (* iters-per-move
                                                                iters-count))))
                 (format t "---- ~A ----~%" (pathname-name file))
                 (let ((score-0 (field-score (solve-once-0 spec))))
                   (format t "0 -> ~,3F~%" score-0))
                 (let ((score-1 (field-score (solve-once-1 spec))))
                   (format t "1 -> ~,3F~%" score-1))
                 (let ((score-2 (field-score (solve-once-2 spec-2))))
                   (format t "2 -> ~,3F~%" score-2))
                 ))))
    (if (cl-fad:directory-pathname-p dir)
        (cl-fad:walk-directory dir #'%file)
        (%file dir))))

(defun solve (problem-file solution-file
              &key
                (iters-count 100)
                (iters-per-move 50)
                timeout
                log-dir)
  (clrhash *split-polygon-cache*)
  (let* ((root-state (read-task-state problem-file))
         (state root-state)
         (best-state state)
         (iteration 0)
         (stop-time (when timeout
                      (+ (get-internal-run-time)
                         (* timeout
                            internal-time-units-per-second)))))
    (loop while (and (< (resemblance state) 1)
                     (< iteration iters-count)
                     (if stop-time
                         (< (get-internal-run-time) stop-time)
                         t))
       do
         (incf iteration)
         (let* ((action (get-best-move
                         (select-next-move
                          (make-node-for-state state) state
                          iters-per-move
                          :timeout-in-seconds (when timeout (/ timeout iters-count))))))
           (when (null action)
             (format t "No more actions found")
             (return))
           (setf state (next-state state action))
           (when (> (field-score state)
                    (field-score best-state))
             (setf best-state state))
           (format t "Iteration ~A resemblance ~,3F~%"
                   iteration (resemblance state))
           (when log-dir
             (let ((file (make-pathname
                          :name (format nil "~A~A" iteration
                                        (if (= iteration 1)
                                            "-adjusted"
                                            ""))
                          :type "svg"
                          :defaults log-dir)))
               (draw-polygons-to-svg (if (= iteration 1)
                                         (target-field state)
                                         (field state)) :filename file)))))
    (format t "Selected state with resemblance ~,3F~%" (resemblance best-state))
    (let* ((path (pathname solution-file))
           (name (pathname-name path))
           (type (pathname-type path))
           (dir (make-pathname :directory (pathname-directory path)))
           (filename (if (= (resemblance best-state) 1)
                         (format nil "~A/../maybe_good_solutions/~A.~A" dir name type)
                         solution-file)))
      (with-open-file
          (*standard-output* filename
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
        (print-solution (field best-state) :matrix (inverse-tr-matrix
                                                    (adjustment-matrix state)))
        (resemblance best-state))
      (values (field best-state)
              (inverse-tr-matrix
               (adjustment-matrix state))))))

(defmethod clone-state ((st game-state))
  st)

(defclass action ()
  ((folding-line :accessor folding-line
                 :type line
                 :initarg :folding-line)
   (folding-side :accessor folding-side
                 :type point
                 :initarg :folding-side)
   (polygons-to-fold :accessor polygons-to-fold
                     :type t
                     :initarg :polygons-to-fold)))

(defclass adjustment-action ()
  ((adjustment-matrix :accessor adjustment-matrix
                      :type list
                      :initarg :adjustment-matrix)))

(defmethod print-object ((a action) stream)
  (format t "(action: line ~A side ~A)"
          (folding-line a) (folding-side a)))

(defmethod next-state ((st game-state) (action action))
  (let ((new-field (if (polygons-to-fold action)
                       (fold-some-polygons-in-list
                        (field st)
                        (folding-line action)
                        (folding-side action)
                        (polygons-to-fold action))
                       (fold-polygon-list
                        (field st)
                        (folding-line action)
                        (folding-side action)))))
    (multiple-value-bind (score resemblance)
        (get-field-score new-field (target-field st))
      (copy-instance
       st
       :field new-field
       :field-score score
       :resemblance resemblance))))

(defmethod next-state ((st game-state) (action adjustment-action))
  (let* ((matrix (adjustment-matrix action))
         (new-silhouette (mult-polygons-matrix (target-field st)
                                               matrix)))
    (assert (equal (identity-tr-matrix)
                   (mult-tr-matrix matrix (inverse-tr-matrix matrix))))
    (multiple-value-bind (score resemblance)
        (get-field-score (field st) new-silhouette)
      (copy-instance
       st
       :target-field new-silhouette
       :adjusted t
       :adjustment-matrix (mult-tr-matrix matrix (adjustment-matrix st))
       :field-score score
       :resemblance resemblance))))

(defun find-non-collinear-point (line polygon)
  (dolist (pt (point-list polygon))
    (let ((pt-sign (line-equation-res line pt)))
      (unless (= pt-sign 0)
        (return-from find-non-collinear-point
          (values pt pt-sign)))))
  nil)

(defun find-all-non-collinear-points (line polygon)
  (let (positive negative)
    (dolist (pt (point-list polygon))
      (let ((pt-sign (line-equation-res line pt)))
        (when (< pt-sign 0)
          (setf negative pt))
        (when (> pt-sign 0)
          (setf positive pt))
        (when (and positive negative)
          (return-from find-all-non-collinear-points
            (list positive negative)))))
    (remove nil (list positive negative))))

(defun valid-move? (field line)
  (loop for polygon in field do
        (when (cdr (split-polygon polygon line))
          (return-from valid-move? t)))
  nil)

(defun partial-folds (polygons line direction-point)
  (let ((direction-sign (line-equation-res line direction-point))
        (union-find (make-instance 'cl-containers:union-find-container
                                   :test #'eq))
        (polygon-map (make-hash-table :test #'eq)))
    (labels ((%add-point (p poly)
               (unless (cl-containers:find-item union-find (orig-point p))
                 (cl-containers:insert-item union-find (orig-point p)))
               (push poly (gethash (orig-point p) polygon-map)))
             (%union-points (p1 p2)
               (cl-containers:graft-nodes
                (cl-containers:representative-node union-find (orig-point p1))
                (cl-containers:representative-node union-find (orig-point p2)))))
      (loop for polygon in polygons do
           (let ((first-point nil))
             (loop for point in (point-list polygon) do
                  (let ((sign (line-equation-res line point)))
                    (when (< (* sign direction-sign) 0)
                      (%add-point point polygon)
                      (if first-point
                          (%union-points point first-point)
                          (setf first-point point)))))))
      (let ((res-tab (make-hash-table :test #'eq)))
        (maphash (lambda (orig-point polys)
                   (loop for poly in polys do
                        (let ((repr (cl-containers:representative union-find orig-point)))
                          (aif (gethash repr res-tab)
                            (setf (gethash poly it) t)
                            (let ((tab (make-hash-table :test #'eq)))
                              (setf (gethash poly tab) t)
                              (setf (gethash repr res-tab) tab))))))
                 polygon-map)
        (alexandria:hash-table-values res-tab)))))

(defun partial-folds-test ()
  (labels ((%test (specs &key a b c x y expected)
             (let* ((folded (fold-quad specs))
                    (l (make-instance 'line :a a :b b :c c))
                    (res (partial-folds
                          folded l
                          (make-instance 'point :x x :y y))))
               (assert1 (mapcar (lambda (polys)
                                  (mapcar #'polygon->list (alexandria:hash-table-keys
                                                           polys)))
                                res)
                        expected))))
    (%test
     '((:a 0 :b 1 :c -1/2 :x 0 :y 1))
     :a 1 :b 1 :c -3/2
     :x 1/2 :y 1/2
     :expected '(((1 1 1 1/2 0 1/2 0 1))
                 ((0 1/2 0 1 1 1 1 1/2))))
    (%test
     '((:a 0 :b 1 :c -1/2 :x 0 :y 0))
     :a 1 :b 1 :c -1
     :x 0 :y 0
     :expected '(((1 1/2 1 0 0 0 0 1/2)
                  (0 0 0 1/2 1 1/2 1 0))))))

(defun possible-folds-actions (st)
  (let ((l
         (loop for polygon in (target-field st) append
              (loop for edge in (edge-list polygon) append
                   (let* ((line (line-from-segment edge))
                          (direction-points
                           (find-all-non-collinear-points line polygon)))
                     (loop for direction-point in direction-points append
                          (let ((folds (if *use-partial-folds*
                                           (partial-folds (field st) line direction-point)
                                           (if (valid-move? (field st) line)
                                               (list nil)
                                               nil))))
                            (loop for fold in folds collect
                                 (make-instance
                                  'action
                                  :folding-line line
                                  :folding-side direction-point
                                  :polygons-to-fold fold)))))))))
    ;;(format t ">> possible-actions: ~A~%" l)
    l))

(defun possible-adjustment-actions (st)
  (loop for matr in (possible-adjustment-matrices st) collect
       (make-instance 'adjustment-action :adjustment-matrix matr)))

(defmethod possible-actions ((st game-state))
  (if (adjusted st)
      (possible-folds-actions st)
      (possible-adjustment-actions st)))

(defun get-field-score (field target-field)
  (let ((size (length (with-output-to-string (s)
                        (print-solution field :stream s))))
        (resemblance (compute-score-for-polygons target-field field)))
    ;; (format t "field size = ~A; " size) 
    ;; (format t "resemblance = ~A~%" (+ 0.0 resemblance))
    (values
     (if (> size 5000)
         0
         (- resemblance
            (* 1/13 (/ size 5000)))) 
     resemblance)))

(defmethod estimate-state-reward ((st game-state))
  ;; (labels ((%once (st)
  ;;            (let* ((actions (possible-actions st))
  ;;                   (actions-num (length actions))
  ;;                   (best-st st))
  ;;              (when actions
  ;;                (loop for i below 10 do
  ;;                     (let* ((action (nth (random actions-num) actions)))
  ;;                       (setf st (next-state st action))
  ;;                       (if (< (field-score best-st)
  ;;                              (field-score st))
  ;;                           (setf best-st st)))))
  ;;              (field-score best-st)))))
  ;; (with-scale (32) 
  ;;   (apply #'max 
  ;;          (loop for i below 10 collect (%once st))))
  ;; (loop for i below 10 do
  ;;       (setf st (%once st)))
  ;; st)
  (labels ((%once (st)
             (let* ((actions (possible-actions st))
                    (actions-num (length actions)))
               (loop for i below actions-num do
                    (let* ((action (nth (random actions-num) actions))
                           (next-st (next-state st action)))
                      (when (< (field-score st)
                               (field-score next-st))
                        (return-from %once next-st))))
               st)))
    (loop for i below 10 do
         (setf st (%once st)))
    ;; (format t "estimate-state-reward:~%")
    ;; (format t "field: ~A~%" (mapcar #'polygon->list (field st)))
    ;; (format t "score: ~A~%" (field-score st))
    (field-score st)))

(defun detect-right-angles (polygon-list)
  (let ((edge-pairs nil))
    (dolist (p polygon-list)
      (let ((first-edge (car (edge-list p))))
        (loop for (e1 e2) on (edge-list p) do
             (let ((e2 (or e2 first-edge)))
               (let ((v1 (vec-from-edge e1))
                     (v2 (vec-from-edge e2)))
                 (when (= 0 (vec-product v1 v2))
                   (push (cons e1 e2) edge-pairs)))))))
    edge-pairs))

(defun vec-from-edge (edge)
  (make-vec (start edge) (end edge)))

(defun make-vec (a b)
  (make-instance 'point
                 :x (- (x b) (x a))
                 :y (- (y b) (y a))))

(defun vec-product (a b)
  (+ (* (x a) (x b))
     (* (y a) (y b))))

(defun test-detect-right-angles ()
  (labels ((%test (&key coords expected)
             (let* ((pl (mapcar
                         (lambda (x)
                           (apply #'make-polygon-from-coords x))
                         coords))
                    (result (detect-right-angles pl))
                    (result-1 (mapcar
                               (lambda (pair)
                                 (list (edge->list (car pair))
                                       (edge->list (cdr pair))))
                               result)))
               (assert1 result-1 expected))))
    (%test :coords '((0 0 0 1 1 0))
           :expected '(((1 0 0 0) (0 0 0 1))))
    (%test :coords '((0 0 0 1 1 0)
                     (0 1 2 1 1 0))
           :expected '(((2 1 1 0) (1 0 0 1))
                       ((1 0 0 0) (0 0 0 1))))
    t))

(defun edge-pair-tr-matrix (pair)
  (let* ((target-edge (cdr pair))
         (move-matr (translate-matrix (- (x (start target-edge)))
                                      (- (y (start target-edge)))))
         (rot-matr (rotate-edge-to-x-matrix target-edge)))
    (when rot-matr
      (mult-tr-matrix rot-matr move-matr))))

(defun test-edge-pair-tr-matrix ()
  (labels ((%test (&key coords expected)
             (let* ((pair (destructuring-bind (x0 y0 x1 y1 x2 y2)
                              coords
                            (cons (make-instance 'line-segment
                                                 :start (make-instance 'point :x x0 :y y0)
                                                 :end (make-instance 'point :x x1 :y y1))
                                  (make-instance 'line-segment
                                                 :start (make-instance 'point :x x1 :y y1)
                                                 :end (make-instance 'point :x x2 :y y2)))))
                    (result (edge-pair-tr-matrix pair)))
               (assert1 result expected))))
    (%test :coords '(0 0 1 0 1 1)
           :expected '((0 1 0) (-1 0 1) (0 0 1)))
    t))

(defun polygons-right-matrices (polygons)
  (let ((edge-pairs (detect-right-angles polygons)))
    (remove nil (mapcar #'edge-pair-tr-matrix edge-pairs))))

(defun polygons-right-adjusted (polygons)
  (let ((matrs (polygons-right-matrices polygons)))
    (loop for matr in matrs collect
         (loop for poly in polygons collect
              (mult-polygon-matrix poly matr)))))
