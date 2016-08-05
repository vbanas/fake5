(defpackage :src/test/parser
  (:use :common-lisp :src/utils :src/parser
        :lisp-unit :cl-quickcheck))

(in-package :src/test/parser)

(defun test-problems (&key (path "/home/ihors/repo/fake5/problems/*"))
  (loop for p in (cl::directory path) do
       (parse-problem p)))

