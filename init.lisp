(load #P"~/quicklisp/setup.lisp")

(ql:quickload 'asdf)

(proclaim '(optimize (debug 3) (safety 3)))
(require 'asdf)

(ql:quickload 'fset)
(ql:quickload 'lisp-unit)
(ql:quickload 'cl-quickcheck)
(ql:quickload 'alexandria)
(ql:quickload 'cl-graph)
(ql:quickload 'cl-heap)
(ql:quickload 'spatial-trees)
(ql:quickload 'spatial-trees.nns)
(ql:quickload 'apply-argv)
;;(ql:quickload 'ironclad)
;;(ql:quickload 'babel)
(ql:quickload 'cl-svg)

(ql:quickload 'cl-geometry)

(in-package :cl-user)
(asdf:initialize-source-registry '(:source-registry
                                   :inherit-configuration
                                   (:directory :here)
                                   (:directory (:here "src/"))))

(asdf:compile-system :src)
(asdf:load-system :src)

(print "Welcome.")






