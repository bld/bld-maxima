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

(test simp-exprs)

(test simp-socket)

(test jacobi)

(test maxima-start)

(test maxima-shutdown)

(test maxima-read)

(test maxima-send)

(test maxima-send-lisp)

(test jacobi-socket)

(test atan2)

(test delay
  (is (equal (delay (simp '(+ a a))) '(+ a a))))

