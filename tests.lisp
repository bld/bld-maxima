(defpackage :bld-maxima-tests
  (:use :cl :bld-maxima :fiveam))

(in-package :bld-maxima-tests)

(def-suite :bld-maxima)

(in-suite :bld-maxima)

(test simp
  (is (null (simp nil)))
  (is (equal (simp 'a) 'a))
  (is (equal (simp '(+ a a)) '(* 2 a)))
  (is (equal (simp '(aref a 2)) '(aref a 2)))
  (is (equal (simp '(+ (aref a 2) (aref a 2))) '(* 2 (aref a 2)))))

(test +
  (is (equal (simp '(+ a a)) '(* 2 a)))
  (is (equal (simp '(+ 1 2)) 3))
  (is (equal (simp '(+ (aref a 2) (aref a 2))) '(* 2 (aref a 2)))))

(test -
  (is (equal (simp '(- a)) '(* -1 a)))
  (is (equal (simp '(- a a)) 0))
  (is (equal (simp '(- 1)) -1))
  (is (equal (simp '(- (aref a 2) (aref a 2))) 0)))

(test *
  (is (equal (simp '(* 2 3)) 6))
  (is (equal (simp '(* a a)) '(expt a 2)))
  (is (equal (simp '(* (aref a 2) (aref a 2))) '(expt (aref a 2) 2)))
  (is (equal (simp '(* a 0)) 0))
  (is (equal (simp '(* a (expt a -1))) 1))
  (is (equal (simp '(* a 2)) '(* 2 a))))

(test expt
  (is (equal (simp (list 'expt 'a 2)) '(expt a 2)))
  (is (equal (simp '(expt a 0)) 1))
  (is (equal (simp (list 'expt 2 'a)) '(expt 2 a)))
  (is (equal (simp '(expt (* a a) 1/2)) '(abs a))))

(test list
  (is (equal (simp (list 'a 'b 'c 'd)) '(a b c d))))

(test abs
  (is (equal (simp (list 'abs 'a)) '(abs a)))
  (is (equal (simp '(abs 2)) 2))
  (is (equal (simp '(abs -2)) 2)))

(test /
  (is (equal (simp '(/ 2 2)) 1))
  (is (equal (simp '(/ a a)) 1))
  (is (equal (simp '(/ (aref a 2) (aref a 2))) 1))
  (is (equal (simp '(/ (* a a) a)) 'a)))

(test sin
  (is (zerop (simp `(sin ,%pi))))
  (is (equal (simp `(sin (/ ,%pi 2))) 1))
  (is (equal (simp `(sin (/ ,%pi 3))) (simp '(/ (sqrt 3) 2))))
  (is (equal (simp `(sin (/ ,%pi 4))) (simp '(/ (sqrt 2) 2))))
  (is (equal (simp `(sin (/ ,%pi 6))) '(/ 1 2)))
  (is (equal (simp '(sin 0)) 0)))

(test cos
  (is (equal (simp `(cos ,%pi)) -1))
  (is (equal (simp `(cos (/ ,%pi 2))) 0))
  (is (equal (simp `(cos (/ ,%pi 3))) '(/ 1 2)))
  (is (equal (simp `(cos (/ ,%pi 4))) (simp '(/ (sqrt 2) 2))))
  (is (equal (simp `(cos (/ ,%pi 6))) (simp '(/ (sqrt 3) 2))))
  (is (equal (simp '(cos 0)) 1)))


;;;(test simp-exprs)

(test atan2)

(test delay
  (is (equal (delay (simp '(+ a a))) '(+ a a))))

