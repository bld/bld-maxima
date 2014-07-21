bld-maxima
==========

BLD-MAXIMA runs a Maxima process in the background with a socket
connection through with commands and Lisp math code can be sent for
evaluation. It includes translation of Lisp math expressions to Maxima
for algebraic simplification. Lisp forms that aren't in the
translation table are identified and treated as symbols in Maxima.
Depends on CL-PPCRE for translating Lisp -> Maxima -> Lisp. Currently,
only SBCL is supported, using SB-EXT:RUN-PROGRAM to run Maxima in the
background. This has been tested on both Win32 and Linux (both
X86-64). The WITH-MAXIMA macro is provided to start & shutdown the
Maxima process around whatever expressions are passed to it.

Routines
--------

Allow running a single Maxima process and sending it commands or lisp
math code to simplify over a network socket.  Depends on USOCKET.
Requires MAXIMA-START to run a socket connected Maxima session, and
MAXIMA-SHUTDOWN once finished sending computations. Or, run inside
WITH-MAXIMA macro.

Usage
-----

    CL-USER> (bld-maxima:maxima-start)
    (((%I1) (%O1) FALSE)
     ((%I2) (%O2) "/usr/share/maxima/5.20.1/share/linearalgebra/linearalgebra.mac"))
    CL-USER> (bld-maxima:simp '(+ (aref a 2) (aref a 2)))
    (* 2 (AREF A 2))
    16
    CL-USER> (bld-maxima:maxima-shutdown)
    T

Alternatively, you can run these routines inside the WITH-MAXIMA macro:

    CL-USER> (with-maxima
                (simp '(+ a a)))
    (* 2 A)
    CL-USER> 

Also, trigonometric simplification functions are available
corresponding to those in Maxima:

    CL-USER> (with-maxima (trigreduce '(expt (cos x) 2)))
    (* (/ 1 2) (+ 1 (COS (* 2 X))))
    CL-USER> (with-maxima (trigexpand '(cos (* 2 x))))
    (+ (EXPT (COS X) 2) (* -1 (EXPT (SIN X) 2)))
    CL-USER> (with-maxima (trigsimp '(+ (expt (sin x) 2) (expt (cos x) 2))))
    1


Delay
-----

Wrapping the 'delay' macro around a 'simp expression prevents
evaluation so it can be deferred until later, which can speed
computations in certain circumstances because of the overhead incurred
by 'simp.
