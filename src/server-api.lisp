(defpackage :src/server-api
  (:use :common-lisp :anaphora))

(in-package :src/server-api)

(defun get-team-key ()
  (aif (sb-ext::posix-getenv "TEAM_KEY")
       it
       (error "TEAM_KEY environment variable is not set.")))

(defun get-latest-snapshot ()
  (sleep 1)
  (asdf::run-shell-command 
   (format nil "curl --compressed -L -H Expect: -H 'X-API-Key: ~A' 'http://2016sv.icfpcontest.org/api/snapshot/list' > /tmp/snapshot"
	   (get-team-key)))
  (with-open-file (foo "/tmp/snapshot")
    (let ((table (yason::parse foo)))
      (when (gethash "ok" table)
	(let* ((sn-s (gethash "snapshots" table))
	       (res (car sn-s)))
	  (mapc (lambda (sn)
		  (let ((tm (gethash "snapshot_time" sn)))
		    (when (> tm (gethash "snapshot_time" res))
		      (setf res sn))))
		(cdr sn-s))
	  res)))))

(defun blob-lookup (blob-hash blob-type &optional problem-id)
  (sleep 1)
  (asdf::run-shell-command 
   (format nil "curl --compressed -L -H Expect: -H 'X-API-Key: ~A' 'http://2016sv.icfpcontest.org/api/blob/~A' > /tmp/blob"
	   (get-team-key) blob-hash))
  (with-open-file (foo "/tmp/blob")
    (case blob-type
      (:snapshot (yason::parse foo))
      (:problem 
       (format t "problem:~A~%" problem-id)
       (asdf::run-shell-command (format nil "cp /tmp/blob /tmp/problems/~A" problem-id)))
      (:solution
       ;;copy to solutions_from_server
       ))))

(defun get-problems ()
  (let* ((snapshot (get-latest-snapshot))
	 (snapshot-blob (blob-lookup (gethash "snapshot_hash" snapshot) :snapshot))
	 (problems (gethash "problems" snapshot-blob)))
    (format t "~%===============~%Problems count:~A~%" (length problems))
    (mapc (lambda (x)
	    (let ((problem-hash (gethash "problem_spec_hash" x))
		  (problem-id (gethash "problem_id" x)))
	      (unless (probe-file (format nil "/tmp/problems/~A" problem-id))
		(blob-lookup problem-hash :problem problem-id))))
    	  problems)
    snapshot-blob))

(defun submit-solution (problem-id path-to-sol-file)
  (sleep 1)
  (asdf::run-shell-command 
   (format nil "curl --compressed -L -H Expect: -H 'X-API-Key: ~A' -F 'problem_id=~A' -F 'solution_spec=@~A' 'http://2016sv.icfpcontest.org/api/solution/submit'"
	   (get-team-key) problem-id path-to-sol-file)))


