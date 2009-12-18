(defpackage :bld.maxima
  (:use :asdf :cl))
(in-package :bld.maxima)
(defsystem :bld-maxima
    :name "bld-maxima"
    :author "Benjamin L. Diedrich <ben@solarsails.info>"
    :version "0.0.1"
    :maintainer "Benjamin L. Diedrich <ben@solarsails.info>"
    :license ""
    :description "Send commands to Maxima program. Includes Lisp math code simplification."
    :components 
    ((:file "maxima"))
    :depends-on ("kmrcl" "alexandria" "split-sequence" "cl-ppcre" "usocket"))
