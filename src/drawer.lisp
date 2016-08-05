(defpackage :src/drawer
  (:nicknames :drawer)
  (:use :common-lisp :src/field :src/utils :cl-svg)
  ;;(:import-from :mcts)
  (:import-from :spatial-trees)
  (:import-from :rectangles))

(in-package :src/drawer)

(defvar *scale* 500)

(defun draw-field (scene)
  (draw scene (:line :x1 0 :y1 0 :x2 *scale* :y2 0))
  (draw scene (:line :x1 0 :y1 0 :x2 0 :y2 *scale*))
  (draw scene (:line :x1 0 :y1 *scale* :x2 *scale* :y2 *scale*))
  (draw scene (:line :x1 *scale* :y1 0 :x2 *scale* :y2 *scale*)))

(defun draw-state (field &key (filename #p"field.svg"))
  (declare (ignore field))
  (with-svg-to-file
      (scene 'svg-1.1-toplevel :height *scale* :width *scale*)
      (filename :if-exists :supersede)
    (draw-field scene)))
