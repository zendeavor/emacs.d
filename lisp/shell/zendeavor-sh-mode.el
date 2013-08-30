;;; zendeavor-sh-script -- init for shell-script specifics
;;; Commentary:
;;;
;;; Code:


(defun zendeavor-sh-mode ()
  "Custom quick setup for shell script mode."
  (define-key sh-mode-map (kbd "RET") 'reindent-then-newline-and-indent))

(add-hook 'sh-mode-hook 'zendeavor-sh-mode)
(provide 'zendeavor-sh-mode)
;;; zendeavor-sh-script.el ends here
