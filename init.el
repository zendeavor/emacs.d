;;; init -- personal init.el
;;; Commentary:
;;; Code:

(package-initialize)

;; lists
(let ((default-directory (concat
         (file-name-as-directory user-emacs-directory)
         (convert-standard-filename "elisp/"))))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'auto-mode-alist '("^\\.?bash" . sh-mode))
(add-to-list 'auto-mode-alist '("^\\.?zsh" . sh-mode))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; requires
(require 'smartparens-config)
(require 'thingatpt)
(require 'dired-details)
(require 'uniquify)
(require 'saveplace)
(require 'helm-config)
(require 'morlock)
(require 'erc-znc)

;; general macros and functions

(defun rotate-windows ()
  "Rotate your windows."                ; whattheemacsd
  (interactive)
  (cond ((not (> (count-windows) 1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq num-windows (count-windows))
         (while  (< i num-windows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i num-windows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2)))
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun toggle-window-split ()
  "Shuffle window orientation."         ; whattheemacsd
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
might be bad."                          ; whattheemacsd
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

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input." ; whattheemacsd
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun kill-syntax (&optional arg)
  "Kill ARG set of syntax characters after point." ; Apsu
  (interactive "p")
  (let ((opoint (point)))
    (forward-same-syntax arg)
    (kill-region opoint (point))))

(defun w3-tidy-page (&optional buff)
    "Use html tidy to clean up the HTML in the current BUFF."
    (save-excursion
        (if buff
            (set-buffer buff)
          (setq buff (current-buffer)))
        (widen)
        (call-process-region (point-min) (point-max)
                             w3-fast-parse-tidy-program
                             t (list buff nil) nil ;nil nil nil;
                             "--show-warnings" "no" "--show-errors" "0"
                             "--force-output" "yes" "-quiet" "-clean" "-bare"
                             "-omit" "--drop-proprietary-attributes" "yes"
                             "--hide-comments" "yes")))
(defun isml ()
  "If sml repl exists, then restart it else create a new repl"
  (interactive)
  (when (get-buffer "*sml*")
    (with-current-buffer "*sml*"
      (when (process-live-p "sml")
        (comint-send-eof)))
    (sleep-for 0.2))
  (sml-run "sml" ""))

(defun common-global-modes ()
  "Default modes for `after-init-hook'."
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode 1)
  (helm-mode 1)
  (global-undo-tree-mode 1)
  (global-hl-line-mode 1)
  (global-auto-revert-mode 1))

(defun run-eshell-buried ()
  "Start a buried eshell."
  (interactive)
  (let ((default-directory (getenv "HOME")))
    (command-execute 'eshell)
    (bury-buffer)))

(defun emacs-after-init ()
  "Initial setup for Emacs."
  (common-global-modes)
  (run-eshell-buried))

(defun common-prog-modes ()
  "Default modes for `prog-mode-hook'."
  (rainbow-delimiters-mode 1)
  (company-mode 1)
  (smartparens-mode 1)
  (show-smartparens-mode 1)
  (flycheck-mode 1)
  (yas-minor-mode 1))

(defun common-elisp-modes ()
  "Default modes for `emacs-lisp-mode-hook'."
  (turn-on-eldoc-mode)
  (elisp-slime-nav-mode 1)
  (purty-mode 1))

(defun elpy-mode-setup ()
  "Config elpy."
  (elpy-enable t)
  (elpy-use-ipython)
  (elpy-clean-modeline)
  (setq-local python-check-command "flake8")
  (setq-local flymake-no-changes-timeout 60)
  (setq-local flymake-start-syntax-check-on-newline nil)
  (elpy-initialize-local-variables))

(defun eshell-mode-setup ()
  "Setup fn for eshell."
  (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
  (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))

;; hooks and modes
; universal
(add-hook 'after-init-hook 'emacs-after-init)
(add-hook 'prog-mode-hook 'common-prog-modes)
(add-hook 'before-save-hook 'cleanup-buffer-safe)
(add-hook 'auto-complete-mode-hook (lambda () (auto-complete-mode -1)))

; lisps
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

; elisp
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'common-elisp-modes)
(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

; python
(add-hook 'python-mode-hook 'elpy-mode-setup)

; shell
(eval-after-load 'sh-script '(require 'zendeavor-sh-mode))

; eshell
(add-hook 'eshell-mode-hook 'eshell-mode-setup)

; w3
(add-hook 'w3-parse-hooks 'w3-tidy-page)

;; variable settings
(setq-default
 package-enable-at-startup nil

 ;; whitespace
 require-final-newline t
 indent-tabs-mode nil

 ;; selection/clipboard
 x-select-enable-clipboard t
 x-select-enable-primary t
 save-interprogram-paste-before-kill t

 ;; browse-url
 browse-url-generic-program (executable-find "conkeror")
 browse-url-browser-function 'browse-url-generic
 browse-url-new-window-flag nil
 browse-url-firefox-new-window-is-tab t

 ;; w3
 w3-fast-parse-tidy-program "/usr/bin/tidy"

 ;; eww
 eww-download-path "~/downloads"
 ;; cleaner dired
 dired-details-hidden-string "--- "
 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil

 ;; shut off the lame startup
 inhibit-splash-screen t

 ;; cursor position
 save-place t

 ;; buffer name
 uniquify-buffer-name-style 'forward

 ;; auxiliary directories
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
 save-place-file (concat user-emacs-directory "places")
 ido-save-directory-list-file (concat
                               (file-name-as-directory user-emacs-directory)
                               "ido.state")

 ;; move customize save file
 custom-file (concat
              (file-name-as-directory user-emacs-directory) "emacs-custom.el")

 ;; show-paren-mode
 show-paren-style 'mixed

 ;; geiser-mode
 geiser-racket-use-gracket-p t

 ;; mail
 user-mail-address "josh.s.mcgee@gmail.com"
 user-full-name "Josh McGee"

 ;; gnus
 gnus-init-file (concat
                 (file-name-as-directory user-emacs-directory) "gnus/gnus-init"))

(dired-details-install)
(font-lock-add-keywords 'emacs-lisp-mode morlock-font-lock-keywords)
(set-default-font "Consolas-9:weight=bold")

;; theme
(load-theme 'solarized-dark t)

;; keybindings
(sp-use-smartparens-bindings)
(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "C-c c") 'cleanup-buffer)
(global-set-key (kbd "C-c w") 'toggle-window-split)
(global-set-key (kbd "C-c W") 'rotate-windows)
(global-set-key (kbd "C-c C-b") 'browse-url-at-point)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "C-c C-SPC") 'ace-jump-word-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-char-mode)
(global-set-key (kbd "C-c M-SPC") 'ace-jump-mode-pop-mark)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c TAB") 'yas-expand)
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; (global-set-key (kbd "C-x C-f") 'helm-find-files)

;;; init.el ends here
