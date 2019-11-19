;; (ql:quickload "named-readtables")

;; (defpackage :funnel
;;   (:use :cl :named-readtables)
;;   (:export "SYNTAX" "EXTRACT-COMPLETE-COLON-EXPR"))

;; (defmacro -> (x &optional form &rest more)
;;   (cond
;;    ((null form) x)
;;    ((null more) (if (listp form)
;;                     `(,(car form) ,x ,@(cdr form))
;;                   (list form x)))
;;    (:else `(-> (-> ,x ,form) ,@more))))


;; (defmacro ->> (x &optional form &rest more)
;;   (cond
;;    ((null form) x)
;;    ((null more) (if (listp form)
;;                     `(,@form ,x)
;;                   (list form x)))
;;    (:else `(->> (->> ,x ,form) ,@more))))

;; (defmacro --> (x &rest forms)
;;   `(-as-> ,x it ,@forms))
