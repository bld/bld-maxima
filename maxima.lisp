;; Maxima interface
;; Converts CL math expressions to Maxima equivalent and evaluates them in Maxima's Lisp level to simplify them
;; Results are returned and extracted from the Maxima output

(defpackage :bld-maxima
  (:use :common-lisp :cl-ppcre)
  (:import-from :kmrcl :command-output)
  (:import-from :split-sequence :split-sequence)
  (:export :*maxima-binary*
	   :*maxima-batch-options*
	   :simplify-lisp-expr))

(in-package :bld-maxima)

(defparameter *maxima-binary* "/usr/bin/maxima")
(defparameter *maxima-batch-options* "-q --batch-string \"display2d : false$ ")
(defvar *maxima-init-expressions* nil)
(defparameter *maxima-lisp-table*
  '((mplus +)
    (mminus -)
    (mtimes *)
    (rat /)
    (mexpt expt)
    (mlist list)
    (mequal =)
    (mgreaterp >)
    (mabs abs)
    (%sin sin)
    (%cos cos)
    (%tan tan)
    (%exp exp)
    (%log log)
    (%sqrt sqrt)
    (%sinh sinh)
    (%cosh cosh)
    (%atan atan)
    (%signum signum)
    (%max max))
  "lookup table of maxima > lisp expressions")

(defparameter *maxima-lisp-table-string*
  (loop for (m l) in *maxima-lisp-table*
     collect (list (format nil "~a" m) (format nil "~a" l)))
  "*maxima-lisp-table* converted to list of strings")

(defparameter *regex-symbols*
  (list "(" ")"
	"[" "]"
	"^"
	"$"
	"."
	"|"
	"?"
	"*"
	"+")
  "regular expression special characters")
#|
(defun array-to-matrix (a)
  "Convert a lisp 2D array to Maxima matrix form"
  (regexify-special
   (format nil "~a"
	   `(mfuncall '$matrix
		      ,@(loop with (n m) = (array-dimensions a)
			   for i below n
			   collect `'((mlist)
				      ,@(loop for j below m
					   collect (aref a i j))))))
   "$"))
|#

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

(defun run-maxima-command (string)
  "evaluate maxima expression string"
  (command-output 
   "~a ~a ~a;\"" 
   *maxima-binary* 
   (apply #'concatenate 'string *maxima-batch-options* *maxima-init-expressions*)
   string))

(defun run-maxima-lisp (string)
  "execute a string using maxima's internal lisp, returning last expression as a string"
  (scan-to-strings "\\n[\\s\\S]*"
		   (nth (1+ (length *maxima-init-expressions*))
			(split "\\(%i[\\d]+\\)"
			       (run-maxima-command 
				(format nil ":lisp ~a" string))))))

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
  (let* ((*maxima-init-expressions* (list "load(\"linearalgebra\")$"))
	 (result
	  (read-from-string
	   (run-maxima-lisp (format nil "(mfuncall '\\$eigens_by_jacobi ~a)" (array-to-matrix a))))))
    (values
     (mlist-to-array (second result)) ; eigenvalues
     (matrix-to-array (third result))))) ; eigenvectors

(defun match-lisp-funs (string)
  "Regular expression match of (function args). Doesn't match ((maximafun) args). Returns list of matches."
  (remove-duplicates (all-matches-as-strings "\\([^\\(^\\)^\\s]+ [^\\(^\\)]+\\)" string) :test #'equal))

(defun match-re-lisp-funs (string)
  "Match renamed lisp functions"
  (all-matches-as-strings "#:\\|\\([^\\(^\\)^\\s]+ [^\\(^\\)]+\\)\\|" string))

(defun regexify-parens (string)
  "Add \\ to parentheses in a string"
  (regex-replace-all "\\(" (regex-replace-all "\\)" string "\\\\)")  "\\\\("))

(defun regexify-special (string char)
  "Add \\ to regular expression special character in a string to make it a literal"
  (regex-replace-all (concatenate 'string "\\" char) string (concatenate 'string "\\\\" char)))

(defun regexify-specials (string)
  "Add \\ to all regular expression special characters in a string to make them literal"
  (loop for char in *regex-symbols*
     for new-string = (regexify-special string char)
     then (regexify-special new-string char)
     finally (return new-string)))

(defun rename-lisp-funs (maxima-string lisp-funs ren-funs)
  "Rename lisp functions (non-math) in a Maxima expression to something that evaluates as a symbol in Maxima's lisp mode"
  (loop with out-string = maxima-string
     for lisp-fun in lisp-funs
     for ren-fun in ren-funs
     do (setq out-string (regex-replace-all (regexify-parens lisp-fun) out-string ren-fun))
     finally (return out-string)))

(defun re-rename-lisp-funs (maxima-string lisp-funs ren-funs)
  "Replace renamed lisp functions with original expressions"
  (loop with out-string = maxima-string
     for ren-fun in ren-funs
     for lisp-fun in lisp-funs
     do (setq out-string (regex-replace-all ren-fun out-string lisp-fun))
     finally (return out-string)))

(defun lisp-to-maxima-string (lisp-string)
  "convert lisp string to maxima string"
  (let ((maxima-string lisp-string))
    (loop for (maxima lisp) in *maxima-lisp-table-string*
       do (setq maxima-string
		(regex-replace-all (regexify-specials lisp) maxima-string (format nil "(~a)" maxima))))
    maxima-string))

(defun maxima-to-lisp-string (maxima-string)
  "convert simplified maxima output string to lisp string"
  (let ((lisp-string maxima-string))
    (loop for (maxima lisp) in *maxima-lisp-table-string*
       do (setq lisp-string
		(regex-replace-all (format nil "\\(~a SIMP\\)" maxima) lisp-string lisp)))
    lisp-string))

(defun simplify-lisp-string (lisp-string)
  "Convert a lisp expression as a string to maxima, and run in maxima to simplify"
  (let* ((maxima-string (lisp-to-maxima-string lisp-string))
	 (lisp-funs (match-lisp-funs maxima-string))
	 (ren-funs (loop for lisp-fun in lisp-funs collect (format nil "~a" (gensym)))))
    (maxima-to-lisp-string
     (re-rename-lisp-funs
      (run-maxima-lisp
       (format nil "(simplify '~a)"
	       (rename-lisp-funs maxima-string lisp-funs ren-funs)))
      lisp-funs
      ren-funs))))

(defun simplify-lisp-expr (lisp-expr)
  "Simplify a mathematical lisp expression"
  (read
   (make-string-input-stream
    (simplify-lisp-string (format nil "~a" lisp-expr)))))
