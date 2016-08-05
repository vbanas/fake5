(defpackage :src/parser 
  (:use :common-lisp :src/utils :cl-geometry :smug)
  (:export #:problem
           #:parse-problem))

(in-package :src/parser)

(defclass problem ()
  ((silhouette :accessor silhouette
               :initarg :silhouette
               :initform nil) ;; list of polygons
   (skeleton :accessor skeleton
             :initarg :skeleton
             :initform nil)) ;; list of line-segments
  )

(defun .newline () (.char= #\NewLine))
(defun .space () (.char= #\Space))

(defun .digits ()
  (.plus (.let* ((x (smug::.digit-char-p))
               (xs (.digits)))
         (.identity (cons x xs)))
       (.identity nil)))

(defun .zero-or-more (parser)
  (.plus (.let* ((x parser)
                 (xs (.zero-or-more parser)))
           (.identity (cons x xs)))
         (.identity ())))

(defun .one-or-more (parser)
  (.let* ((x parser)
	  (y (.zero-or-more parser)))
    (.identity (cons x y))))

(defun .rational ()
  (.concatenate 'string (.digits) (smug::.make-list 1 :initial-element (.char= #\/)) (.digits)))

(defun .pint ()
  (.concatenate 'string (.digits)))

(defun .number ()
  (.first (.or (.rational) (.pint))))

(defun .point ()
  (.let* ((x (.number))
          (y (.and (.char= #\,) (.number))))
    (.identity (make-instance 'point
                              :x (read-from-string x)
                              :y (read-from-string y)))))

(defun .vertex ()
  (.let* ((p (.point))
          (_ (.newline)))
    (.identity p)))

(defun .line-segment ()
  (.let* ((p1 (.point))
          (_ (.char= #\Space))
          (p2 (.point))
          (_ (.or (.newline) (.space)))) 
    (.identity (make-instance 'line-segment
                              :start p1
                              :end p2))))

(defun .polygon ()
  (.let* ((_ (.and (.pint) (.newline)))  ; number of vertexes, probably is odd
          (vertexes (.first (.one-or-more (.vertex)))))
    (.identity (make-polygon-from-point-list vertexes))))

(defun .silhouette ()
  (.let* ((_ (.and (.pint) (.newline)))  ; number of polygons, probably is odd
          (polygons (.first (.one-or-more (.polygon)))))
    (.identity polygons)))

(defun .skeleton ()
  (.let* ((_ (.and (.pint) (.char= #\Newline))) ; number of line-segments, probably is odd
          (polygons (.first (.one-or-more (.line-segment)))))
    (.identity polygons)))

(defun .problem ()
  (.let* ((silhouette (.silhouette))   ; number of line-segments, probably is odd
          (skeleton (.skeleton)))
    (.identity (make-instance 'problem
                              :silhouette silhouette
                              :skeleton skeleton))))

(defun read-file-as-string (filename)
  (let (lines)
    (with-open-file (stream filename)
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (push line lines))) (format nil "~{~A~%~}"(reverse lines))))

(defun parse-problem (filename)
  (caar (run (.problem) (read-file-as-string filename))))


