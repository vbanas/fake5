(defpackage :src/main
  (:nicknames :main)
  (:use :common-lisp :src/field :src/utils :src/state))

(in-package :src/main)

(defun main ()
  (print "Hello World!!!"))

;; (defun main ()
;;   (when sb-ext:*posix-argv*
;;     (let* ((parsed-args (apply-argv:parse-argv* ;;'("./test" "-f" "problems/problem_1.json")))
;; 			 sb-ext:*posix-argv*))
;; 	   (files) (phrases) (time) (memory) (proc-count))
;;       ;;(format t "~A~%~A~%" parsed-args (alexandria:plist-alist (cdr parsed-args)))
;;       (mapcar (lambda (p) 
;; 		(let ((o (string (car p)))
;; 		      (v (cdr p)))
;; 		  (cond
;; 		    ((string= "-f" o) (push v files))
;; 		    ((string= "-p" o) (push v phrases))
;; 		    ((string= "-c" o) (setq proc-count v))
;; 		    ((string= "-m" o) (setq memory v))
;; 		    ((string= "-t" o) (setq time (parse-integer v :junk-allowed t))))))
;; 	      (alexandria:plist-alist (cdr parsed-args)))
;;       (when time
;;         (set-timeout time))
;;       (setq *magic-words* phrases)
;;       (setq *magic-words-cst* (make-command-seq-matching-tree phrases))
;;       ;;(format t "~A~%" files)
;;       (let ((result-list nil))
;; 	(dolist (f (reverse files))			
;; 	  (when (probe-file f)
;; 	    ;;(format t "~A~%~%" (alexandria:read-file-into-string f))
;;             (format *error-output* "Processing file ~A~%" f)
;; 	    (setf result-list 
;; 		  (append result-list (let ((*standard-output* *error-output*))
;;                                         (simple-wave-from-task 
;;                                          (decode-task (alexandria:read-file-into-string f)))))))) 
;; 	(yason:encode result-list)))))
