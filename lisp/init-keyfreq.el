;; -*- codingc: utf-8; lexical-binding: t -*-

(defun turnon-keyfreq-mode ()
  "Turn on keyfreq."
  (interactive)
  ;; Fire up keyfreq a few seconds later to start up emacs faster
  (my-run-with-idle-timer 4 (lambda ()
                               (keyfreq-mode 1)
                               (keyfreq-autosave-mode 1))))

(with-eval-after-load 'keyfreq

(setq keyfreq-excluded-commands
      '(self-insert-command
        abort-recursive-edit
        ace-jump-done
        ace-jump-move
        ace-window
        backward-char
        backward-kill-word
        backward-word
        company-complete-common
        company-complete-number
        company-complete-selection
        company-ignore
        delete-backward-char
        describe-variable
        dired
        dired-find-file
        dired-next-line
        dired-previous-line
        diredp-next-line
        diredp-previous-line
        electric-pair-delete-pair
        eval-buffer
        forward-char
        forward-word
        goto-line
        hippie-expand
        ido-complete
        ido-delete-backward-updir
        ido-exit-minibuffer
        ido-switch-buffer
        indent-new-comment-line
        isearch-abort
        isearch-backward-regexp
        isearch-cancel
        isearch-delete-char
        isearch-exit
        isearch-forward-regexp
        isearch-other-control-char
        isearch-other-meta-char
        isearch-printing-char
        isearch-repeat-forward
        isearch-ring-retreat
        ispell-minor-check
        ivy-backward-delete-char
        ivy-backward-kill-word
        ivy-done
        ivy-next-line
        ivy-occur
        ivy-occur-next-line
        ivy-occur-press-and-switch
        ivy-occur-previous-line
        ivy-previous-line
        ivy-wgrep-change-to-wgrep-mode
        keyboard-escape-quit
        keyboard-quit
        keyfreq-mode
        keyfreq-save-now
        keyfreq-show
        lsp
        move-dup-move-lines-up
        move-dup-move-lines-down
        my-kill-line
        my-ediff-files
        my-newline-at-end-of-line
        my-kill-back-to-indentation
        symbol-overlay-put
        symbol-overlay-jump-prev
        symbol-overlay-jump-next
        whole-line-or-region
        ))

  (my-write-to-missing-file "()" keyfreq-file))

;; And use keyfreq-show to see how many times you used a command.
;; It's recommended to use `keyfreq-mode' (could be in "~/.custom.el").
;; It's reported keyfreq is not compatible with `latex-mode'
;; @see https://github.com/redguardtoo/emacs.d/issues/767
(turnon-keyfreq-mode)

(provide 'init-keyfreq)
