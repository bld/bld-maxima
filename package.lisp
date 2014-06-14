(defpackage :bld-maxima
  (:use :common-lisp :cl-ppcre :usocket)
  (:export :*delay*
	   :*maxima-binary*
	   :*maxima-batch-options*
	   :*maxima-init-expressions*
	   :*maxima-lisp-table*
	   :simp
	   :simp-exprs
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
	   :delay
	   :with-maxima))
