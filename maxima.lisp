;; Maxima interface
;; Converts CL math expressions to Maxima equivalent and evaluates them in Maxima's Lisp level to simplify them
;; Results are returned and extracted from the Maxima output


(in-package :bld-maxima)

(defvar *delay* nil "Check whether to defer evaluation")
(defvar *maxima-binary* #+win32 "maxima.bat" #+unix "maxima")
(defvar *maxima-batch-options* "-q -r \"display2d : false$ ")
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

(defmacro delay (&body body)
  "Delay simplification of a block of code"
  `(let ((bld-maxima::*delay* t))
     ,@body))
