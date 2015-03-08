;;;; Maxima interface
;;; Converts CL math expressions to Maxima equivalent and evaluates them in Maxima's Lisp level to simplify them
;;; Results are returned and extracted from the Maxima output

(in-package :bld-maxima)

(defvar *delay* nil "Check whether to defer evaluation")
(defvar *maxima-binary* #+win32 "maxima.bat" #+unix "maxima")
(defvar *maxima-init-expressions* nil)

;;; PI and E
(defparameter %pi '$%pi)
(defparameter %e '$%e)

;;; Lookup table of maxima vs lisp symbols
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

(defparameter *lisp-maxima-table*
  (mapcar #'reverse *maxima-lisp-table*))

(defun lisp-atom-to-maxima (atom)
  "Convert lisp atom to maxima RAT"
  (if (typep atom 'ratio) ; convert ratio to RAT
      `((rat) ,(numerator atom) ,(denominator atom))
      atom)) ; else use as is

(defun lisp-expr-to-maxima (lexpr &optional (fntable (make-hash-table :test 'equal)))
  "Convert tree of lisp math expressions to maxima lisp format"
  ;; Return Maxima expression
  (values
   (if (atom lexpr) ; Check if atomic
       (lisp-atom-to-maxima lexpr) ; If so, return as is
       (let* ((l (first lexpr)) ; Else grab Lisp function
	      (m (rest (assoc (intern (symbol-name l) :bld-maxima) *lisp-maxima-table*)))) ; & lookup Maxima
	(if (not m) ; Check if Lisp function not in table
	    ;; If not, gensym, put in table, & return
	    (progn
	      (unless (gethash lexpr fntable)
		(setf (gethash lexpr fntable) (intern (symbol-name (gensym)))))
	      (gethash lexpr fntable))
	    ;; Otherwise, convert to Maxima form & convert args
	    (let ((a (mapcar #'(lambda (l) (lisp-expr-to-maxima l fntable)) (rest lexpr))))
	      (if (equal l '/) ; Check if division, & treat different
		  (if (rest a) ; One or 2+ args?
		      `((mtimes) ,(first a) ((mexpt) ((mtimes) ,@(rest a)) -1))
		      `((mexpt) ,(first a) -1)) ; 1 arg: invert
		  `(,m ,@a)))))) ; Else form non-division Maxima expr
   ;; Also return fntable
   fntable))

(defun reverse-hash-table-keys-values (h)
  "Reverse the keys and values of a hash table"
  (let ((hout (make-hash-table :test (hash-table-test h))))
    (loop for k being the hash-keys in h
       for v being the hash-values in h
       do (setf (gethash v hout) k))
    hout))

(defun maxima-expr-to-lisp (mexpr rfntable)
  "Convert maxima expression to lisp"
  (if (atom mexpr)
      (if (gethash mexpr rfntable)
      	  (gethash mexpr rfntable)
	  mexpr)
      (let* ((m (first (first mexpr)))
	     (l (intern (symbol-name (second (assoc (intern (symbol-name m) :bld-maxima) *maxima-lisp-table*)))))
	     (a (mapcar #'(lambda (m) (maxima-expr-to-lisp m rfntable)) (rest mexpr))))
	(if (equal (symbol-name m) "RAT")
	    (apply #'/ a)
	    `(,l ,@a)))))

(defun simp-lisp-expr (lexpr &optional (evfn '$ev))
  (multiple-value-bind (mexpr fntable) (lisp-expr-to-maxima lexpr)
    (let ((mexpr-simp (second (maxima-send-lisp (format nil "(mfuncall '~a '~a)" evfn mexpr))))
	  (rfntable (reverse-hash-table-keys-values fntable)))
      (maxima-expr-to-lisp mexpr-simp rfntable))))

(defmethod atan2 ((n1 number)(n2 number))
  (atan n1 n2))

(defmacro delay (&body body)
  "Delay simplification of a block of code"
  `(let ((bld-maxima::*delay* t))
     ,@body))

(defun simp (lexpr &optional (simpfun '$ev))
  "Simplify a lisp math expression using specified Maxima
simplification function. Just return the expression if *DELAY* is T."
  (if *delay* lexpr (simp-lisp-expr lexpr simpfun)))

(defun trigreduce (lexpr)
  (simp lexpr '$trigreduce))

(defun trigexpand (lexpr)
  (simp lexpr '$trigexpand))

(defun trigsimp (lexpr)
  (simp lexpr '$trigsimp))

(defun trigrat (lexpr)
  (simp lexpr '$trigrat))
