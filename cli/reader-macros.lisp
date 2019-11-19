(ql:quickload "named-readtables")

(defpackage :funnel
  (:use :cl :named-readtables)
  (:export "SYNTAX" "EXTRACT-COMPLETE-COLON-EXPR"))

(in-package :funnel)

(defun extract-complete-colon-expr (stream)
  (loop
     with base = (symbol-name (read stream))
     thereis (if  (eq (peek-char t stream nil nil t) #\+)
                 (progn
                  (setq base
                        (concatenate 'string base (symbol-name (read stream))))
                  ;; Ensure we don't activate the `thereis` yet by returning nil.
                  nil)
                 ;; Otherwise, we return non-nil and exit the loop.
                 (make-symbol base))))


(defreadtable funnel:syntax
  (:merge :standard)
  (:macro-char
   #\:
   #'(lambda (stream received-char)
       (assert (eq received-char #\:))
       (let ((expr (list 'quote (funnel:extract-complete-colon-expr stream)))
             (s (gensym)))
         `(let ((,s ,expr))
            `(parseable-function ,,s))))
   t)
  (:macro-char
   #\+
   #'(lambda (stream received-char)
       (assert (eq received-char #\+))
       (let ((expr (list 'quote (read stream)))
             (s (gensym)))
         `(let ((,s ,expr))
            `(conditional-maybe ,,s))))
   t)
  (:case :preserve))

(defconstant syntax (find-readtable 'funnel:syntax))
