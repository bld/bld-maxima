;; Maxima interface
;; Converts CL math expressions to Maxima equivalent and evaluates them in Maxima's Lisp level to simplify them
;; Results are returned and extracted from the Maxima output


(in-package :bld-maxima)

(defvar *delay* nil "Check whether to defer evaluation")
(defvar *maxima-binary* #+win32 "maxima.bat" #+unix "maxima")
(defvar *maxima-init-expressions* nil)

;; Utility functions to fix rationals

(defun rat-string-to-ratio (ratstring)
  "Replace Maxima RAT expression with Lisp ratio"
  (regex-replace-all 
   "\\(\\(RAT( SIMP)?( RATSIMP)?\\) (-?\\d+) (\\d+)\\)"
   ratstring "\\3/\\4"))

(defun ratio-to-rat-string (ratiostring)
  "Replace Lisp ratio with RAT expression"
  (regex-replace-all
   "(-?\\d+)/(-?\\d+)"
   ratiostring
   "((RAT) \\1 \\2)"))

;; Fix exponent functions to (sqrt ...), (/ ...), (/ (sqrt ...)) 

(defun expt-to-sqrt (exptstring)
  "Replace (EXPT FORM 1/2) to SQRT expression"
  (regex-replace-all
   "\\(EXPT (.*) 1/2\\)"
   (remove #\Newline exptstring) "(SQRT \\1)"))

(defun expt-to-invsqrt (exptstring)
  "Replace Lisp (EXPT FORM -1/2) to (/ (SQRT FORM)) expression"
  (regex-replace-all
   "\\(EXPT (.*) -1/2\\)"
   (remove #\Newline exptstring) "(/ (SQRT \\1))"))

(defun expt-to-/ (exptstring)
  (regex-replace-all
   "\\(EXPT (.*) -1\\)"
   (remove #\Newline exptstring) "(/ \\1)"))

;; PI and E
(defparameter %pi '$%pi)
(defparameter %e '$%e)

;; Lookup table of maxima vs lisp symbols
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
    (%atan2 atan2)
    (%asin asin)
    (%acos acos)
    (%signum signum)
    ($max max)
    ($min min)
    (rat /))
  "lookup table of maxima > lisp expressions")

(defmethod atan2 ((n1 number)(n2 number))
  (atan n1 n2))

(defparameter *maxima-lisp-table-string*
  (loop for (m l) in *maxima-lisp-table*
     collect (list (princ-to-string m) (princ-to-string l)))
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

;; Regular expressions to use for matching lisp expressions
(defparameter *lisp-fun-regex* "\\([^\\(^\\)^\\s]+ [^\\(^\\)]+\\)")
(defparameter *keyword-regex* ":\\w+")

(defun match-lisp-funs (string)
  "Regular expression match of (function args). Doesn't match ((maximafun) args). Returns list of matches."
  (remove-duplicates 
   (all-matches-as-strings 
    (format nil "(~a|~a)" *lisp-fun-regex* *keyword-regex*)
    string) :test #'equal))

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
    (ratio-to-rat-string maxima-string)))

(defun maxima-to-lisp-string (maxima-string)
  "convert simplified maxima output string to lisp string"
  (let* ((*read-default-float-format* 'double-float)
	 ;; fix Maxima output & replace RAT with ratios
	 (lisp-string
	  (rat-string-to-ratio
	   maxima-string)))
    (loop for (maxima lisp) in *maxima-lisp-table-string*
       do (setq lisp-string
		(regex-replace-all 
		 (format nil "\\(~a( SIMP)?( RATSIMP)?\\)" (regexify-specials maxima)) 
		 lisp-string lisp)))
    (expt-to-sqrt
     (expt-to-invsqrt
      (expt-to-/
       lisp-string)))))

(defmacro delay (&body body)
  "Delay simplification of a block of code"
  `(let ((bld-maxima::*delay* t))
     ,@body))

(defun simplify-lisp-expr (lexpr &optional (simpfun '$ev))
  "Simplify a lisp expression using socket to Maxima"
  (let* ((mstring (lisp-to-maxima-string (prin1-to-string lexpr)))
	 (lfuns (match-lisp-funs mstring))
	 (rfuns (loop for lfun in lfuns
		   collect (princ-to-string (gensym)))))
    (read-from-string
     (re-rename-lisp-funs
      (maxima-to-lisp-string
       (princ-to-string
	(second
	 (maxima-send-lisp
	  (format nil "(mfuncall '~a '~a)" 
		  simpfun
		  (rename-lisp-funs mstring lfuns rfuns))))))
      lfuns rfuns))))

(defun simp (lexpr &optional (simpfun '$ev))
  "Simplify a lisp math expression using specified Maxima
simplification function. Just return the expression if *DELAY* is T."
  (if *delay* lexpr (simplify-lisp-expr lexpr simpfun)))

(defun trigreduce (lexpr)
  (simp lexpr '$trigreduce))

(defun trigexpand (lexpr)
  (simp lexpr '$trigexpand))

(defun trigsimp (lexpr)
  (simp lexpr '$trigsimp))

(defun trigrat (lexpr)
  (simp lexpr '$trigrat))
