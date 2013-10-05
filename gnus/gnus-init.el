;;; gnus-init-file --- Gnus init file
;;; Commentary:
;;;
;;; Code:

(let ((authinfo-file (concat (file-name-as-directory user-emacs-directory)
                             "gnus/authinfo")))
  (setq
  ;; gmail through imap
  gnus-select-method '(nnimap "gmail"
                              (nnimap-address "imap.gmail.com")
                              (nnimap-server-port 993)
                              (nnimap-stream ssl))

  message-send-mail-function 'smtpmail-send-it
  smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
  smtpmail-auth-credentials '(("smtp.gmail.com" 587
                               "josh.s.mcgee@gmail.com" nil))
  smtpmail-default-smtp-server "smtp.gmail.com"
  smtpmail-smtp-server "smtp.gmail.com"
  smtpmail-smtp-service 587
  gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
  nntp-authinfo-file authinfo-file
  nnimap-authinfo-file authinfo-file))
