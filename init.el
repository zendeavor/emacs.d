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
(require 'ido-ubiquitous)
(require 'dired-details)

;; general macros and functions
(defun common-prog-modes ()
  "Default modes for 'prog-mode-hook."
  (rainbow-delimiters-mode 1)
  (smartparens-mode 1)
  (flycheck-mode 1))

(defun common-global-modes ()
  "Default modes for 'after-init-hook."
  (global-company-mode 1)
  (show-smartparens-global-mode 1)
  (global-auto-revert-mode 1)
  (ido-ubiquitous-mode 1))


(defun common-elisp-modes ()
  "Default modes for 'emacs-lisp-mode-hook."
  (turn-on-eldoc-mode)
  (elisp-slime-nav-mode 1))

(defun rotate-windows ()
  "Rotate your windows." ; whattheemacsd
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

;; (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
;;   "Fix ido-ubiquitous for CMD in PACKAGE." ; whattheemacsd
;;   `(eval-after-load ,package
;;      '(defadvice ,cmd (around ido-ubiquitous-new activate)
;;         (let ((ido-ubiquitous-enable-compatibility nil))
;;          ad-do-it))))

(defun toggle-window-split ()
  "Shuffle window orientation." ; whattheemacsd
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a 'before-save-hook, and that
might be bad." ; whattheemacsd
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))


(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save." ; whattheemacsd
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(defun kill-syntax (&optional arg)
  "Kill ARG set of syntax characters after point." ; Apsu
  (interactive "p")
  (let ((opoint (point)))
    (forward-same-syntax arg)
    (kill-region opoint (point))))

;; hooks and modes
; universal
(add-hook 'after-init-hook 'common-global-modes)
(add-hook 'prog-mode-hook 'common-prog-modes)
(add-hook 'before-save-hook 'cleanup-buffer-safe)

; yasnippets
;; (ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
;; (ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

; lisps
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

; elisp
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook 'common-elisp-modes)
;;; not ready to change modelines yet
;(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

; python
(eval-after-load "python"
  '(progn
     (elpy-enable)
     (elpy-use-ipython)
     (elpy-clean-modeline)))

; shell
(eval-after-load "sh-script"
  '(require 'zendeavor-sh-mode))

;; variable settings
(setq-default
 ;; whitespace settings
 show-trailing-whitespace t
 require-final-newline t
 ;; browse-url
 browse-url-browser-function 'browse-url-firefox
 browse-url-new-window-flag t
 browse-url-firefox-new-window-is-tab t
 ;; cleaner dired
 dired-details-hidden-string "--- "
 ;; future defaults
 )

(setq
 ;; make dired clean itself
 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil
 ;; shut off the lame startup
 inhibit-splash-screen t
 ido-save-directory-list-file (concat user-emacs-directory "ido.state")
 ;; move customize save file
 custom-file (concat
              (file-name-as-directory user-emacs-directory)
              "emacs-custom.el")
 )
(dired-details-install)
;; theme
(load-theme 'solarized-dark t)

;; keybindings
(sp-use-smartparens-bindings)
(global-set-key (kbd "C-c c") 'cleanup-buffer)
(global-set-key (kbd "C-c w") 'toggle-window-split)
(global-set-key (kbd "C-c C-w") 'rotate-windows)
(global-set-key (kbd "C-c C-b") 'browse-url-at-point)
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))
(global-set-key (kbd "C-c SPC") 'ace-jump-mode-pop-mark)
(global-set-key (kbd "C-c C-SPC") 'ace-jump-mode)
(global-set-key (kbd "M-f") 'forward-same-syntax)
(global-set-key (kbd "M-b")
                (lambda()
                  (interactive)
                  (forward-same-syntax -1)))

(global-set-key (kbd "M-d") 'kill-syntax)
(global-set-key [(meta backspace)]
                (lambda ()
                  (interactive)
                  (kill-syntax -1)))
(global-set-key (kbd "M-F") 'forward-symbol)
(global-set-key (kbd "M-B")
                (lambda ()
                  (interactive)
                  (forward-symbol -1)))



;;; init.el ends here
