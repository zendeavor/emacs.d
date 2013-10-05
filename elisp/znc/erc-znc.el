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
      erc-modules '(pcomplete
                    hl-nicks
                    netsplit
                    fill
                    button
                    match
                    completion
                    readonly
                    networks
                    ring
                    autojoin
                    noncommands
                    irccontrols
                    move-to-prompt
                    stamp
                    menu
                    list))
;; (erc-update-modules)

(defun irc-frame ()
  "Start to waste time on IRC with ERC."
  (interactive)
  (select-frame (make-frame '((name . "Emacs IRC")
                              (minibuffer . t))))
  (znc-erc))

(provide 'erc-znc)
;;; erc-znc.el ends here
