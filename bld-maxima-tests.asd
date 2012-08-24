(asdf:defsystem :bld-maxima-tests
    :name "bld-maxima"
    :author "Benjamin L. Diedrich <ben@solarsails.info>"
    :license "MIT"
    :description "Send commands to Maxima program, including simplification of Lisp math expressions."
    :components 
    ((:file "tests"))
    :depends-on ("bld-maxima" "fiveam" "usocket" "cl-ppcre"))

