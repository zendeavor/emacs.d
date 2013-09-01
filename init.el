;;; init -- personal init.el
;;; Commentary:
;;; Code:

(package-initialize)
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

;; requires
(require 'smartparens-config)
(require 'thingatpt)


;; delayed loads
(eval-after-load "sh-mode"
  '(progn
    (require 'zendeavor-sh-mode)))

;; hooks
(defun common-prog-modes ()
  "Default modes for prog-mode-hook."
  (rainbow-delimiters-mode +1)
  (smartparens-mode +1)
  (flycheck-mode +1))

(add-hook 'prog-mode-hook 'common-prog-modes)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'after-init-hook 'global-company-mode)

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

(setq inhibit-splash-screen t)
(setq ido-save-directory-list-file (concat user-emacs-directory "ido"))
;; fuck this fucking customize thing
(setq custom-file
      (concat
       (file-name-as-directory user-emacs-directory) "emacs-custom.el"))

;; theme
(load-theme 'solarized-dark t)

;; keybindings
(global-set-key (kbd "C-c C-b") 'browse-url-at-point)
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode-pop-mark)
;; Apsu
;; Make M-f/b/d/bkspc not suck!
(global-set-key (kbd "M-f") 'forward-same-syntax)
(global-set-key (kbd "M-b")
                (lambda()
                  (interactive)
                  (forward-same-syntax -1)))
(defun kill-syntax (&optional arg)
  "Kill ARG set of syntax characters after point."
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
