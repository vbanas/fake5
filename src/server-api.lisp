(defpackage :src/server-api
  (:use :common-lisp :anaphora :cl-fad))

(in-package :src/server-api)

(defun get-team-key ()
  (aif (sb-ext::posix-getenv "TEAM_KEY")
       it
       (error "TEAM_KEY environment variable is not set.")))

(defun get-latest-snapshot ()
  (sleep 1)
  (uiop::run-program 
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
  (uiop::run-program 
   (format nil "curl --compressed -L -H Expect: -H 'X-API-Key: ~A' 'http://2016sv.icfpcontest.org/api/blob/~A' > /tmp/blob"
	   (get-team-key) blob-hash))
  (with-open-file (foo "/tmp/blob")
    (case blob-type
      (:snapshot (yason::parse foo))
      (:problem 
       (format t "problem:~A~%" problem-id)
       (uiop::run-program (format nil "cp /tmp/blob /tmp/problems/~A" problem-id))
       (uiop::run-program (format nil "cp /tmp/blob /tmp/newproblems/~A" problem-id))
       )
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
  (uiop::run-program 
   (format nil "curl --compressed -L -H Expect: -H 'X-API-Key: ~A' -F 'problem_id=~A' -F 'solution_spec=@~A' 'http://2016sv.icfpcontest.org/api/solution/submit' > /tmp/solution"
	   (get-team-key) problem-id path-to-sol-file))
  (with-open-file (foo "/tmp/solution")
    (handler-case
	(let ((table (yason::parse foo)))
	  (if (gethash "ok" table)
	      (progn
		(format t "solution for:~A resemblence:~A~%" problem-id (gethash "resemblance"table))
		(if (= 1 (gethash "resemblance" table))
		    :solved
		    :partial))
	      (progn
		(format t "FAILED solution for:~A~%~A~%" problem-id (gethash "error" table))
		:failed)))
      (error (e) (format t "Connection error:~A.~%" e)))))

(defun submit-all-solutions (folder good-folder problem-folder solved-problem-folder)
  (mapc (lambda (f) 
	  (let ((res (submit-solution (pathname-name f) (namestring f))))
	    (case res
	      (:solved ;;move to good_solutions
	       (uiop::run-program
		(format nil "mv ~A ~A/~A.txt" (namestring f) good-folder (pathname-name f)))
	       (uiop::run-program
		(format nil "cp ~A/~A ~A/~A.txt" problem-folder (pathname-name f)
			solved-problem-folder (pathname-name f))))
	      (:partial
	       ;;nothing to do
	       )
	      (:failed))))
	;; (cl-fad:list-directory folder)
	(directory folder)
	))

;;1470481200 - 11:00 UTC 6_08_2016
;;1470484800 - 12:00 UTC

(defun submit-problem (path-to-prob-file timestamp)
  (sleep 1)
  (uiop::run-program 
   (format nil "curl --compressed -L -H Expect: -H 'X-API-Key: ~A' -F 'solution_spec=@~A' -F 'publish_time=~A' 'http://2016sv.icfpcontest.org/api/problem/submit' > /tmp/problem"
	   (get-team-key) path-to-prob-file timestamp))
  (with-open-file (foo "/tmp/problem")
    (let ((table (yason::parse foo)))
      (if (gethash "ok" table)
      	  (format t "problem id:~A hash:~A~%" 
		  (gethash "problem_id" table)
		  (gethash "problem_spec_hash" table))
      	  (format t "FAILED Problem ~A submit~%~A~%" path-to-prob-file (gethash "error" table))))))
