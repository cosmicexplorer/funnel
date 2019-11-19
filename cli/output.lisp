(load "reader-macros.lisp")

(defpackage :macro-usage
  (:use :cl :named-readtables :funnel)
  (:export "EXAMPLE"))

(in-package :macro-usage)

(defun print (&rest args)
  (apply #'cl:print args))

(defun list (&rest args)
  (apply #'cl:list args))

(defconstant example
  (let (res)
    (in-readtable funnel:syntax)
    (setq res (read-from-string "`(,:b...+a ,+a)"))
    (in-readtable :common-lisp)
    res)
  "???")
