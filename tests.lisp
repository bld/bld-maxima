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
  (is (equal (simp '(+ a)) 'a))
  (is (equal (simp '(+ a a)) '(* 2 a)))
  (is (equal (simp '(+ 1 2)) 3))
  (is (equal (simp '(+ (aref a 2) (aref a 2))) '(* 2 (aref a 2)))))

(test -
  (is (equal (simp '(- a)) '(* -1 a)))
  (is (equal (simp '(- a a)) 0))
  (is (equal (simp '(- 1)) -1))
  (is (equal (simp '(- (aref a 2) (aref a 2))) 0)))

(test *
  (is (equal (simp '(* a)) 'a))
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
  (is (equal (simp '(abs (- a))) '(abs a)))
  (is (equal (simp '(abs 2)) 2))
  (is (equal (simp '(abs -2)) 2)))

(test /
  (is (equal (simp '(/ 2 2)) 1))
  (is (equal (simp '(/ a a)) 1))
  (is (equal (simp '(/ (aref a 2) (aref a 2))) 1))
  (is (equal (simp '(/ (* a a) a)) 'a))
  (is (equal (simp (list '/ 1 2)) '(/ 1 2)))
  (is (equal (simp (list '/ 2 4)) '(/ 1 2))))

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

(test tan
  (is (equal (simp '(tan 0)) 0))
  (is (equal (simp `(tan (/ ,%pi 4))) 1)))

(test exp
  (is (equal (simp '(exp 0)) 1))
  (is (equal (simp '(exp 1)) %e))
  (is (equal (simp '(exp 2)) `(expt ,%e 2))))

(test log
  (is (equal (simp '(log 1)) 0))
  (is (equal (simp `(log ,%e)) 1))
  (is (equal (simp `(log (expt ,%e -2))) -2)))

(test sqrt
  (is (equal (simp '(sqrt 0)) 0))
  (is (equal (simp '(sqrt 1)) 1))
  (is (equal (simp '(sqrt 4)) 2))
  (is (equal (simp '(sqrt (* a a))) '(abs a))))

;;; Could use more tests for hyperbolic functions
(test sinh
  (is (equal (simp '(sinh 0)) 0)))

(test cosh
  (is (equal (simp '(cosh 0)) 1)))

(test tanh
  (is (equal (simp '(tanh 0)) 0)))

(test atan
  (is (equal (simp (list 'atan 'a)) '(atan a)))
  (is (equal (simp '(atan 0)) 0))
  (is (equal (simp '(atan 1)) (simp `(/ ,%pi 4)))))

(test atan2
  (is (equal (simp '(atan2 0 1)) 0))
  (is (equal (simp '(atan2 1 1))
	     (simp `(/ ,%pi 4)))))

(test asin
  (is (equal (simp (list 'asin 'a)) '(asin a)))
  (is (equal (simp '(asin 0)) 0))
  (is (equal (simp '(asin 1)) (simp `(/ ,%pi 2))))
  (is (equal (simp '(asin (/ (sqrt 2) 2))) (simp `(/ ,%pi 4)))))

(test acos
  (is (equal (simp (list 'acos 'a)) '(acos a)))
  (is (equal (simp '(acos 0)) (simp `(/ ,%pi 2))))
  (is (equal (simp '(acos 1)) 0))
  (is (equal (simp '(acos (/ (sqrt 2) 2))) (simp `(/ ,%pi 4)))))

(test signum
  (is (equal (simp (list 'signum 'a)) '(signum a)))
  (is (equal (simp '(signum (- a))) (simp '(- (signum a)))))
  (is (equal (simp '(signum 2)) 1))
  (is (equal (simp '(signum -2)) -1)))

(test max
  (is (equal (simp (list 'max 'a)) 'a))
  (is (equal (simp '(max 1 3 5 4 2)) 5))
  (is (equal (simp '(max 1 3 5 a 4 3)) (simp '(max 5 a)))))

(test min
  (is (equal (simp (list 'min 'a)) 'a))
  (is (equal (simp '(min 1 3 5 4 2)) 1))
  (is (equal (simp '(min 1 3 5 a 4 3)) (simp '(min 1 a)))))

(test delay
  (is (equal (delay (simp '(+ a a))) '(+ a a))))

