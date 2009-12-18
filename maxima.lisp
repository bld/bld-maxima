;; Maxima interface
;; Converts CL math expressions to Maxima equivalent and evaluates them in Maxima's Lisp level to simplify them
;; Results are returned and extracted from the Maxima output
;; Socket version is still being debugged, so not included in export list

(defpackage :bld-maxima
  (:use :common-lisp :cl-ppcre :usocket)
  (:import-from :kmrcl :command-output :run-shell-command)
  (:import-from :alexandria :alist-hash-table)
  (:import-from :split-sequence :split-sequence)
  (:export :*maxima-binary*
	   :*maxima-batch-options*
	   :simplify-lisp-expr))

(in-package :bld-maxima)

(defparameter *maxima-binary* "/usr/bin/maxima")
(defparameter *maxima-batch-options* "-q --batch-string \"display2d : false$ ")

(defparameter *maxima-lisp-table*
  (alist-hash-table
   '((mplus . +)
     (mminus . -)
     (mtimes . *)
     (rat . /)
     (mexpt . expt) 
     (mlist . list)
     (mequal . =)
     (mgreaterp . >)
     (mabs . abs)
     (%sin . sin)
     (%cos . cos)
     (%tan . tan)
     (%exp . exp)
     (%log . log)
     (%sqrt . sqrt)
     (%sinh . sinh)
     (%cosh . cosh)
     (%atan . atan)
     (%atan2 . atan2)
     (%sum . sum)
     (%derivative . %diff)
     (factor . factor)
     (expand . expand)
     (ratexpand . ratexpand)
     (%signum . signum)
     (%max . max))))

(defun run-maxima-command (string)
  (command-output "~a ~a ~a;\"" *maxima-binary* *maxima-batch-options* string))

(defun run-maxima-lisp (string)
  (second 
   (split-sequence 
    #\Newline 
    (run-maxima-command 
     (format nil ":lisp ~a" string)))))

(defun match-lisp-funs (string)
  "Regular expression match of (function args). Doesn't match ((maximafun) args). Returns list of matches."
  (remove-duplicates (all-matches-as-strings "\\([^\\(^\\)^\\s]+ [^\\(^\\)]+\\)" string) :test #'equal))

(defun match-re-lisp-funs (string)
  "Match renamed lisp functions"
  (all-matches-as-strings "#:\\|\\([^\\(^\\)^\\s]+ [^\\(^\\)]+\\)\\|" string))

(defun regexify-parens (string)
  "Add \\ to parentheses in a string"
  (regex-replace-all "\\(" (regex-replace-all "\\)" string "\\\\)")  "\\\\("))

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

(defun lisp-to-maxima (lisp-expr)
  (let ((maxima-expr lisp-expr))
    (maphash #'(lambda (maxima lisp)
		 (setq maxima-expr (subst (list maxima) lisp maxima-expr :test #'equal)))
	     *maxima-lisp-table*)
    maxima-expr))

(defun maxima-to-lisp (maxima-expr)
  (let ((lisp-expr maxima-expr))
    (maphash #'(lambda (maxima lisp)
		 (setq lisp-expr (subst lisp (list maxima 'simp) lisp-expr :test #'equal)))
	     *maxima-lisp-table*)
    lisp-expr))

(defun simplify-lisp-expr (lisp-expr)
  (let* ((maxima-string (format nil "~a" (lisp-to-maxima lisp-expr)))
	 (lisp-funs (match-lisp-funs maxima-string))
	 (ren-funs (loop for lisp-fun in lisp-funs collect (format nil "~a" (gensym)))))
    (maxima-to-lisp
     (read
      (make-string-input-stream
       (re-rename-lisp-funs
	(run-maxima-lisp
	 (format nil "(simplify '~a)"
		 (rename-lisp-funs maxima-string lisp-funs ren-funs)))
	lisp-funs
	ren-funs))))))

;; Socket version has problems if Maxima stopped & restarted
;; Left off export list until

(defparameter *maxima-port* 4011)
(defparameter *maxima-socket-options* "-q -r \"display2d : false;\" -s")
(defparameter *maxima-host* "localhost")
(defvar *maxima-socket-passive* nil)
(defvar *maxima-socket* nil)
(defvar *maxima-pid* nil)

(defun maxima-listen ()
  (setq *maxima-socket-passive* (socket-listen *maxima-host* *maxima-port*)))

(defun maxima-run ()
  (run-shell-command "~a ~a ~a &~%" *maxima-binary* *maxima-socket-options* *maxima-port*))

(defun maxima-accept ()
  (setq *maxima-socket* (socket-accept *maxima-socket-passive*))
  (setq *maxima-pid* (read-line (socket-stream *maxima-socket*))))

(defun maxima-send (string)
  (let ((stream (socket-stream *maxima-socket*)))
    (format stream "~a;~%" string)
    (finish-output stream)
    (loop while (listen stream)
       collect (read-line stream))))

(defun maxima-send-lisp (string)
  (first (maxima-send (format nil ":lisp ~a" string))))

(defun maxima-quit ()
  (maxima-send "quit()"))

(defun maxima-shutdown ()
  (maxima-quit)
  (socket-close *maxima-socket*))

(defun maxima-flush ()
  (let ((stream (socket-stream *maxima-socket*)))
    (loop while (listen stream)
       collect (read-line stream))))

(defun maxima-start ()
  (unless *maxima-socket-passive* (maxima-listen))
  (maxima-run)
  (maxima-accept)
  (maxima-flush))

(defun simplify-lisp-expr-socket (lisp-expr)
  (let* ((maxima-string (format nil "~a" (lisp-to-maxima lisp-expr)))
	 (lisp-funs (match-lisp-funs maxima-string))
	 (ren-funs (loop for lisp-fun in lisp-funs collect (format nil "~a" (gensym)))))
    (maxima-to-lisp
     (read
      (make-string-input-stream
       (re-rename-lisp-funs
	(maxima-send-lisp
	 (format nil "(simplify '~a)"
		 (rename-lisp-funs maxima-string lisp-funs ren-funs)))
	lisp-funs
	ren-funs))))))
