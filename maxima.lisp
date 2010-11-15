;; Maxima interface
;; Converts CL math expressions to Maxima equivalent and evaluates them in Maxima's Lisp level to simplify them
;; Results are returned and extracted from the Maxima output

(defpackage :bld-maxima
  (:use :common-lisp :cl-ppcre :usocket)
  (:import-from :kmrcl :command-output :run-shell-command)
  (:export :*delay*
	   :*maxima-binary*
	   :*maxima-batch-options*
	   :*maxima-init-expressions*
	   :*maxima-lisp-table*
	   :simp
	   :simp-exprs
	   :run-maxima-lisp
	   :jacobi
	   :run-maxima-command
	   :run-maxima-lisp
	   :*maxima-port*
	   :*maxima-socket-options*
	   :*maxima-socket-init-forms*
	   :*maxima-host*
	   :*maxima-socket-passive*
	   :*maxima-socket*
	   :*maxima-pid*
	   :maxima-start
	   :maxima-shutdown
	   :maxima-read
	   :maxima-send
	   :maxima-send-lisp
	   :simp-socket
	   :jacobi-socket
	   :atan2
	   :delay))

(in-package :bld-maxima)

(defvar *delay* nil "Check whether to defer evaluation")
(defparameter *maxima-binary* "/usr/bin/maxima")
(defparameter *maxima-batch-options* "-q --batch-string \"display2d : false$ ")
(defvar *maxima-init-expressions* nil)
(defparameter *maxima-lisp-table*
  '((mplus +)
    (mminus -)
    (mtimes *)
    (mexpt expt)
    (mlist list)
;;    (mequal =) ; comparison operators not really useful with Maxima
;;    (mgreaterp >)
    (mabs abs)
    (rat /)
    (%sin sin)
    (%cos cos)
    (%tan tan)
    (%exp exp)
    (%log log)
    (%sqrt sqrt)
    (%sinh sinh)
    (%cosh cosh)
    (%atan atan)
    (%atan2 atan2)
    (%signum signum)
    (%max max))
  "lookup table of maxima > lisp expressions")

(defmethod atan2 ((n1 number)(n2 number))
  (atan n1 n2))

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
  (let ((maxima-string (remove #\Newline lisp-string)))
    (loop for (maxima lisp) in *maxima-lisp-table-string*
       do (setq maxima-string
		(regex-replace-all ; only replace with ( prefix & space suffix
		 (regexify-specials (concatenate 'string "(" lisp " "))
		 maxima-string 
		 (concatenate 'string "((" maxima ") "))))
    maxima-string))

(defun maxima-to-lisp-string (maxima-string)
  "convert simplified maxima output string to lisp string"
  (let* ((*read-default-float-format* 'double-float)
	 (lisp-string (format nil "~a" (read-from-string maxima-string)))) ; fix Maxima output
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

(defun simplify-lisp-strings (&rest lisp-strings)
  (let* ((maxima-string-list (mapcar #'lisp-to-maxima-string lisp-strings))
	 (lisp-funs-list (mapcar #'match-lisp-funs maxima-string-list))
	 (ren-funs-list (loop for lisp-funs in lisp-funs-list
			   collect (loop for lisp-fun in lisp-funs
				      collect (format nil "~a" (gensym)))))
	 (renamed-funs-strings (mapcar #'rename-lisp-funs maxima-string-list lisp-funs-list ren-funs-list))
	 (maxima-output (run-maxima-lisp (format nil "(mapcar #'simplify '(~{~a ~}))" renamed-funs-strings)))
	 (maxima-output-list (mapcar #'(lambda (l) (format nil "~a" l)) (read-from-string maxima-output))))
    (mapcar #'(lambda (output lisp-funs ren-funs)
		(maxima-to-lisp-string
		 (re-rename-lisp-funs output lisp-funs ren-funs)))
	    maxima-output-list
	    lisp-funs-list
	    ren-funs-list)))

(defun simp (lisp-expr)
  "Simplify a mathematical lisp expression"
    (if *delay* 
	lisp-expr
	(let ((*read-default-float-format* 'double-float))
	  (read-from-string (simplify-lisp-string (format nil "~a" lisp-expr))))))

(defun simp-exprs (&rest exprs)
  "Simplify a list of mathematical Lisp expressions"
  (if *delay*
      exprs
      (let ((*read-default-float-format* 'double-float))
	(mapcar #'read-from-string 
		(apply #'simplify-lisp-strings 
		       (mapcar #'(lambda (expr) (format nil "~a" expr)) 
			       exprs))))))

(defmacro delay (&body body)
  "Delay simplification of a block of code"
  `(let ((bld-maxima::*delay* t))
     ,@body))
