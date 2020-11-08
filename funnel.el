;; ???/why not?


(require 'pcase)
(require 'rx)


(defgroup funnel nil
  "???"
  :group 'unix
  :group 'execute
  :group 'terminals
  :group 'tools
  :group 'languages
  :group 'matching
  :group 'text
  :group 'data
  :group 'files)


(defgroup funnel-why nil
  "Interoperable type stubs in normal shell scripts."
  :group 'funnel)

;;;###autoload
(define-derived-mode funnel-why-mode
  sh-mode "?"
  :group 'funnel-why
  (setq-local sh-shell 'zsh))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.why\\'" . funnel-why-mode))


(defgroup funnel-env nil
  "Changes to the streams that the 'funnel' command read or write to."
  :group 'funnel)


(provide 'funnel)

;;; funnel.el ends here
