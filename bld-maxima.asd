(defpackage :bld.maxima.system
  (:use :asdf :cl))
(in-package :bld.maxima.system)
(defsystem :bld-maxima
    :name "bld-maxima"
    :author "Benjamin L. Diedrich <ben@solarsails.info>"
    :version "0.0.1"
    :maintainer "Benjamin L. Diedrich <ben@solarsails.info>"
    :license "MIT"
    :description "Simplify Lisp expressions using Maxima"
    :components 
    ((:file "maxima"))
    :depends-on ("cl-ppcre" "embeddable-maxima"))
