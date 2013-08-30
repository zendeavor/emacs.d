;;; init -- personal init.el
;;; Commentary:
;;; Code:

;; requires
(require 'better-defaults)
(require 'smartparens-config)
(require 'thingatpt)

;; delayed loads
(eval-after-load "sh-mode" (require 'zendeavor-sh-mode))

;; lists
(let ((default-directory
        (concat
         (file-name-as-directory user-emacs-directory)
         (convert-standard-filename "lisp/"))))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'auto-mode-alist '("^\\.?bash" . sh-mode))
(add-to-list 'auto-mode-alist '("^\\.?zsh" . sh-mode))
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; hooks
;; this should obviously be doable without repetitions...
(add-hook 'prog-mode-hook rainbow-delimiters-mode)
(add-hook 'prog-mode-hook smartparens-mode)
(add-hook 'prog-mode-hook rainbow-mode)
(add-hook 'prog-mode-hook flycheck-mode)
(add-hook 'prog-mode-hook auto-complete-mode)
(add-hook 'emacs-lisp-mode-hook eldoc-mode)

;; variables
(setq-default
 ;; whitespace settings
 show-trailing-whitespace t
 require-final-newline t
 ;; browse-url
 browse-url-browser-function 'browse-url-firefox
 browse-url-new-window-flag t
 browse-url-firefox-new-window-is-tab t
 ;; future defaults
 )

;; fuck this fucking customize thing
(setq custom-file
      (concat
       (file-name-as-directory user-emacs-directory) "emacs-custom.el"))

;; theme
(load-theme 'zenburn t)

;; keybindings
(global-set-key (kbd "C-c C-b") 'browse-url-at-point)
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))
;; Apsu
;; Make M-f/b/d/bkspc not suck!
(global-set-key (kbd "M-f") 'forward-same-syntax)
(global-set-key (kbd "M-b")
                (lambda()
                  (interactive)
                  (forward-same-syntax -1)))
(defun kill-syntax (&optional arg)
  "Kill ARG sets of syntax characters after point."
  (interactive "p")
  (let ((opoint (point)))
    (forward-same-syntax arg)
    (kill-region opoint (point))))
(global-set-key (kbd "M-d") 'kill-syntax)
(global-set-key [(meta backspace)]
                (lambda ()
                  (interactive)
                  (kill-syntax -1)))
;; Symbols are interesting too
(global-set-key (kbd "M-F") 'forward-symbol)
(global-set-key (kbd "M-B")
                (lambda ()
                  (interactive)
                  (forward-symbol -1)))


;; that's as far as i got
;;; init.el ends here
