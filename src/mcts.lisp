;; As described in:
;;   http://www.cameronius.com/cv/mcts-survey-master.pdf

(defpackage :src/mcts
  (:use :common-lisp
        :src/utils)
  (:export :select-next-move
           :possible-actions
           :next-state
           :clone-state
           :estimate-state-reward
           :make-node-for-state
           :get-best-child
           :get-best-move
           :get-best-moves-chain))

(in-package :src/mcts)

(defclass node ()
  ((parent :accessor parent
           :initarg :parent)
   (action :accessor action
           :initarg :action)
   ;; Q
   (simulation-reward :type number
                      :initform 0
                      :accessor simulation-reward
                      :initarg :simulation-reward)
   ;; N
   (visit-count :type integer
                :initform 0
                :accessor visit-count
                :initarg :visit-count)
   (unexplored-actions :type list
                       :accessor unexplored-actions
                       :initarg :unexplored-actions)
   (children :type list
             :initform nil
             :accessor children
             :initarg :children)
   ))

(defgeneric possible-actions (state))
(defgeneric next-state (state action &key &allow-other-keys))
(defgeneric clone-state (state))
(defgeneric estimate-state-reward (state))

(defvar *exploration-coefficient*)

(defun select-next-move (root-node root-state max-iters
                         &key
                           timeout-in-seconds
                           (exploration-coefficient 1))
  (let* ((stop-time (when timeout-in-seconds
                      (+ (get-internal-run-time)
                         (* timeout-in-seconds
                            internal-time-units-per-second))))
         (*exploration-coefficient* exploration-coefficient)
         (root root-node)
         (i 0))
    (loop while (and (< i max-iters)
                     (if stop-time
                         (< (get-internal-run-time) stop-time)
                         t))
       do 
         (multiple-value-bind (node state)
             (find-best-nested-child
              root (clone-state root-state))
           (incf i)
           (backup node (estimate-state-reward state))))
    root))

(defun get-best-child (root)
  (let* ((*exploration-coefficient* 0)
         (node (select-best-child root)))
    (copy-instance node :parent nil)))

(defun get-best-move (root)
  (let* ((*exploration-coefficient* 0)
         (node (select-best-child root)))
    (action node)))

(defun get-best-moves-chain (root)
  (let* ((*exploration-coefficient* 0)
         (node (select-best-child root))
         (root-action (action root)))
    (if (eq node root)
        (when root-action (list root-action))
        (let ((chain (get-best-moves-chain node)))
          (if root-action
              (cons root-action chain)
              chain)))))

(defun find-best-nested-child (node state)
  (with-slots (children unexplored-actions) node
    (cond
      ((and (null children)
            (null unexplored-actions))
       (values node state))
      (unexplored-actions
       (expand-node node state))
      (children
       (let* ((best-child (select-best-child node))
              (best-child-state (next-state
                                 state (action best-child))))
         (find-best-nested-child best-child best-child-state)))
      (t (error "Should never reach this state")))))

(defun expand-node (node state)
  (let* ((action (pop (unexplored-actions node)))
         (child-state (next-state state action))
         (child-node (make-node-for-state child-state
                                          :parent node
                                          :action action)))
    (push child-node (children node))
    (values child-node child-state)))

(defun select-best-child (node)
  (if (null (children node))
      node
      (let ((2-ln-node-visits (log (visit-count node))))
        (labels ((%ucb1 (child-node)
                   (+ (/ (simulation-reward child-node)
                         (visit-count child-node))
                      (* *exploration-coefficient*
                         (sqrt (/ 2-ln-node-visits
                                  (visit-count child-node)))))))
          (let* ((best-child (first (children node)))
                 (best-child-score (%ucb1 best-child)))
            (dolist (child (rest (children node)))
              (let ((child-score (%ucb1 child)))
                (when (> child-score best-child-score)
                  (setf best-child child
                        best-child-score child-score))))
            best-child)))))

(defun backup (nested-node reward)
  (loop
     for node = nested-node then (parent node)
     while node
     do
       (incf (visit-count node))
       (incf (simulation-reward node) reward)))

(defun make-node-for-state (state &key parent action)
  (make-instance
   'node
   :unexplored-actions (possible-actions state)
   :parent parent
   :action action))

