;; Maxima interface
;; Converts CL math expressions to Maxima equivalent and evaluates them in Maxima's Lisp level to simplify them
;; Results are returned and extracted from the Maxima output

(in-package :bld-maxima)

(defvar *maxima-port* 4011)
(defvar *maxima-socket-options* "-q")
(defvar *maxima-socket-init-forms* ; list of initial Maxima forms to execute on start
  '("display2d : false" ; Show results without 2D rendering
    "load(\"linearalgebra\")")) ; load linearalgebra library for jacobi routine
(defvar *maxima-host* "127.0.0.1")
(defvar *maxima-socket-passive* nil)
(defvar *maxima-socket* nil)
(defvar *maxima-pid* nil)
(defvar *maxima-process* nil)

(defun maxima-listen ()
  "Setup a server socket to listen to Maxima process"
  (setq *maxima-socket-passive* (socket-listen *maxima-host* *maxima-port*)))

(defun maxima-run ()
  "Run Maxima in the background, connecting to previously setup *MAXIMA-SOCKET*"
  (setq *maxima-process*
	(uiop/run-program::%run-program
	 (list *maxima-binary* 
	       *maxima-socket-options*
	       "-s"
	       (princ-to-string *maxima-port*))
	 :wait nil)))

(defun maxima-accept ()
  "Accept socket connection to Maxima process"
  (setq *maxima-socket* (socket-accept *maxima-socket-passive*))
  (setq *maxima-pid* (read-line (socket-stream *maxima-socket*))))

(defun maxima-read (&key (timeout 1) (num 1))
  "Read next output of Maxima"
  (loop repeat num
     collect
       (read (socket-stream 
	      (wait-for-input *maxima-socket* :timeout timeout)))))

(defun maxima-send (string &key (num 3))
  "Send Maxima form as string"
  (let ((stream (socket-stream *maxima-socket*)))
    (format stream "~a;~%" string)
    (force-output stream))
  (maxima-read :num num))

(defun maxima-send-lisp (string &key (num 2))
  "Send Maxima lisp form as string"
  (maxima-send (format nil ":lisp ~a" string) :num num))

(defun maxima-quit ()
  "Send quit() command to maxima"
  (maxima-send "quit()" :num 0))

(defun maxima-shutdown ()
  "Shutdown maxima"
  (maxima-quit)
  (socket-close *maxima-socket*))

(defun maxima-start ()
  "Start a Maxima process and send initial forms"
  (unless *maxima-socket-passive* (maxima-listen))
  (maxima-run)
  (maxima-accept)
  (mapcar #'maxima-send *maxima-socket-init-forms*))

(defmacro with-maxima (&body body)
  "Create an environment with Maxima running in the background to access"
  (let ((result (gensym)))
    `(progn
       (maxima-start)
       (let ((,result (progn ,@body)))
	 (maxima-shutdown)
	 ,result))))
