;;;; Maxima interface for simplifying Lisp math expressions

;;; Converts Lisp math expressions to Maxima equivalents
;;; Simplifies in Maxima's Lisp level

;;; TODO
;;; Fix Jacobi method for finding eigenvalues & eigenvectors of matrices
;;; Access other Maxima features for enhanced mathematics in Lisp

(defpackage :bld-maxima
  (:use :common-lisp :cl-ppcre)
  (:import-from :bld-utils :maptree)
  (:export :*delay*
	   :simp
	   :simp-exprs
	   :trigreduce
	   :trigexpand
	   :trigsimp
	   :trigrat
	   :jacobi
	   :atan2
	   :delay
	   :%pi
	   :%e))

;; Reset from change made by EMBEDDABLE-MAXIMA
(setq cl:*read-default-float-format* 'single-float)

(in-package :bld-maxima)

;; Define %PI variable to be Maxima %PI for simplifying trig relationships
(defparameter %pi 'maxima::$%pi)

(defparameter %e 'maxima::$%e)

(defvar *delay* nil "Check whether to defer evaluation")
(defparameter *maxima-lisp-table*
  '((mplus +)
    (mminus -)
    (mtimes *)
    (mexpt expt)
    (mlist list)
    (mabs abs)
    (mquotient /)
    (%sin sin)
    (%cos cos)
    (%tan tan)
    (%exp exp)
    (%log log)
    (%sqrt sqrt)
    (%sinh sinh)
    (%cosh cosh)
    (%tanh tanh)
    (%atan atan)
    ($atan2 atan2)
    (%asin asin)
    (%acos acos)
    (%signum signum)
    ($max max)
    ($min min)
    (rat /))
  "lookup table of maxima > lisp expressions")

(defmethod atan2 ((n1 number)(n2 number))
  "Tangent of 2 args given rise & run. Used for conversion between maxima & lisp."
  (atan n1 n2))

(defparameter *maxima-lisp-table-intern*
  (loop for (m l) in *maxima-lisp-table*
     collect (list (intern (format nil "~a" m) :maxima) l))
  "*maxima-lisp-table* interned in :MAXIMA so symbols are recognized")

(defun array-to-matrix (a)
  "Convert a lisp 2D array to Maxima matrix form"
  (loop with string = "(mfuncall '\\$matrix"
     with (n m) = (array-dimensions a)
     for i below n
     do (setq string (concatenate 'string string " '((mlist)"))
       (loop for j below m
	  do (setq string (concatenate 'string string (format nil " ~a" (aref a i j))))
	  finally (setq string (concatenate 'string string ")")))
     finally (setq string (concatenate 'string string ")"))
       (return string)))

(defun matrix-to-array (mm)
  "Convert maxima matrix as list expression to lisp 2D array"
  (let ((rows (1- (length mm)))
	(cols (1- (length (second mm)))))
    (make-array (list rows cols)
		:initial-contents
		(mapcar #'rest (rest mm)))))

(defun mlist-to-array (ml)
  (make-array (1- (length ml)) :initial-contents (rest ml)))

(defun jacobi (a)
  "Find eigenvalues of an array using Maxima's eigens_by_jacobi function"
  (let ((*maxima-init-expressions* (list "load(\"linearalgebra\")$"))
	(*read-default-float-format* 'double-float))
    (let ((result
	   (read-from-string
	    (run-maxima-lisp (format nil "(mfuncall '\\$eigens_by_jacobi ~a)" (array-to-matrix a))))))
      (values
       (mlist-to-array (second result)) ; eigenvalues
       (matrix-to-array (third result)))))) ; eigenvectors

(defun match-lisp-funs (string)
  "Regular expression match of (function args). Doesn't match ((maximafun) args). Returns list of matches."
  (remove-duplicates (all-matches-as-strings "\\([^\\(^\\)^\\s]+ [^\\(^\\)]+\\)" string) :test #'equal))

(defun rename-lisp-funs-embedded (mexpr lfuns ren-funs)
  (loop with out-expr = (copy-tree mexpr)
     for lfun in lfuns
     for ren-fun in ren-funs
     do (setq out-expr (nsubst (read-from-string ren-fun) (read-from-string lfun) out-expr :test #'equal))
     finally (return out-expr)))

(defun re-rename-lisp-funs-embedded (mexpr lfuns rfuns)
  (loop with outexpr = (copy-tree mexpr)
     for rfun in rfuns
     for lfun in lfuns
     do (setq outexpr (nsubst (read-from-string lfun) (read-from-string rfun) outexpr))
     finally (return outexpr)))

(defun keywords-to-strings (lexpr)
  "Turn keywords into strings"
  (maptree #'(lambda (lf)
	       (if (keywordp lf)
		   (format nil ":~a" lf)
		   lf))
	   lexpr))

(defun lisp-to-maxima (lexpr)
  "Convert Lisp to Maxima expression"
  (loop for (m l) in *maxima-lisp-table-intern*
     for mexpr = (subst (list m) l lexpr)
     then (nsubst (list m) l mexpr)
     finally (return mexpr)))

(defun maxima-to-lisp (mexpr)
  (let ((lexpr (copy-tree mexpr)))
    (loop for (maxima lisp) in *maxima-lisp-table-intern*
       do (setq lexpr (nsubst lisp (list maxima 'maxima::simp) lexpr :test #'equal))
       do (setq lexpr (nsubst lisp (list maxima 'maxima::simp 'maxima::ratsimp) lexpr :test #'equal))
       do (setq lexpr (nsubst lisp (list maxima) lexpr :test #'equal)))
    lexpr))

(defun simplify-lisp-expr (lexpr &optional (simpfun 'maxima::$ev))
  "Simplify an algebraic lisp expression"
  (let* ((mexpr (lisp-to-maxima lexpr))
	 (mstring (format nil "~a" (keywords-to-strings mexpr)))
	 (lfuns (match-lisp-funs mstring))
	 (rfuns (loop for lfun in lfuns collect (format nil "~a" (gensym)))))
    (maxima-to-lisp
     (re-rename-lisp-funs-embedded
      (maxima::mfuncall simpfun
       (rename-lisp-funs-embedded mexpr lfuns rfuns))
      lfuns
      rfuns))))

(defmacro delay (&body body)
  "Delay simplification of a block of code"
  `(let ((bld-maxima::*delay* t))
     ,@body))

(defun simp (lexpr &optional (simpfun 'maxima::$ev))
  (if *delay*
      lexpr
      (simplify-lisp-expr lexpr simpfun)))

(defun simp-exprs (&rest exprs)
  (if *delay*
      exprs
      (mapcar #'simplify-lisp-expr exprs)))

(defun trigreduce (lexpr)
  (simp lexpr 'maxima::$trigreduce))

(defun trigexpand (lexpr)
  (simp lexpr 'maxima::$trigexpand))

(defun trigsimp (lexpr)
  (simp lexpr 'maxima::$trigsimp))

(defun trigrat (lexpr)
  (simp lexpr 'maxima::$trigrat))
