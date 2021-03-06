;;; gnus-init-file --- Gnus init file
;;; Commentary:
;;;
;;; Code:

(let ((authinfo-file (concat (file-name-as-directory user-emacs-directory)
                             "gnus/authinfo")))
  (setq-default
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

(setq-default gnus-secondary-select-methods
              '((nntp "news.gmane.org")
               (nnimap "gmane.emacs.devel")))

(setq-default
 gnus-group-line-format "%M%S%p%P%5y:%B%g\n"
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-thread-sort-functions '(gnus-thread-sort-by-date)
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "╰► "
 gnus-sum-thread-tree-vertical "│")
