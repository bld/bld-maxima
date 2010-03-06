(defpackage :bld.maxima.system
  (:use :asdf :cl))
(in-package :bld.maxima.system)
(defsystem :bld-maxima
    :name "bld-maxima"
    :author "Benjamin L. Diedrich <ben@solarsails.info>"
    :version "0.0.1"
    :maintainer "Benjamin L. Diedrich <ben@solarsails.info>"
    :license "MIT"
    :description "Send commands to Maxima program, including simplification of Lisp math expressions."
    :components 
    ((:file "maxima")
     (:file "maxima-socket" :depends-on ("maxima")))
    :depends-on ("kmrcl" "cl-ppcre" "usocket"))
