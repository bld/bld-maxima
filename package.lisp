(defpackage :bld-maxima
  (:use :common-lisp :cl-ppcre :usocket)
  (:export :*delay*
	   :*maxima-binary*
	   :*maxima-init-expressions*
	   :*maxima-lisp-table*
	   :simp
	   :*maxima-port*
	   :*maxima-socket-options*
	   :*maxima-socket-init-forms*
	   :*maxima-host*
	   :*maxima-socket-passive*
	   :*maxima-socket*
	   :*maxima-pid*
	   :*maxima-process*
	   :maxima-start
	   :maxima-shutdown
	   :maxima-read
	   :maxima-send
	   :maxima-send-lisp
	   :atan2
	   :delay
	   :with-maxima
	   :trigreduce
	   :trigexpand
	   :trigsimp
	   :trigrat))
