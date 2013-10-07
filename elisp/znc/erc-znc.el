;;; erc-znc -- Config for erc with znc
;;; Commentary:
;;; Code:

(require 'erc)
(require 'znc)

(setq znc-servers '(("auriga.kiwilight.com" 65000 nil
                    ((freenode "zendeavor" "densetsu"))))
      erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
      erc-current-nick-highlight-type 'nick
      erc-mode-line-format "%t"
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 18
      erc-modules '(autojoin
                    button
                    completion
                    fill
                    hl-nicks
                    irccontrols
                    list
                    match
                    menu
                    move-to-prompt
                    netsplit
                    networks
                    noncommands
                    pcomplete
                    readonly
                    ring
                    stamp
                    truncate))
;; (erc-update-modules)

(defun irc-frame ()
  "Start to waste time on IRC with ERC."
  (interactive)
  (select-frame (make-frame '((name . "Emacs IRC")
                              (minibuffer . t))))
  (znc-erc))

(provide 'erc-znc)
;;; erc-znc.el ends here
