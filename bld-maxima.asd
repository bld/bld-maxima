(asdf:defsystem :bld-maxima
    :name "bld-maxima"
    :author "Benjamin L. Diedrich <ben@solarsails.info>"
    :license "MIT"
    :description "Send commands to Maxima program, including simplification of Lisp math expressions."
    :depends-on ("cl-ppcre" "usocket")
    :serial t
    :components ((:file "package")
		 (:file "maxima")
		 (:file "maxima-socket")))
