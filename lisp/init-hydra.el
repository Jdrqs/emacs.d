;; -*- coding: utf-8; lexical-binding: t; -*-

(defhydra hydra-describe (:color blue :hint nil)
  "
Describe Something: (q to quit)
_a_ all help for everything screen
_b_ bindings
_B_ personal bindings
_c_ char
_C_ coding system
_f_ function
_F_ flycheck checker
_i_ input method
_k_ key briefly
_K_ key
_l_ language environment
_m_ major mode
_M_ minor mode
_n_ current coding system briefly
_N_ current coding system full
_o_ lighter indicator
_O_ lighter symbol
_p_ package
_P_ text properties
_s_ symbol
_t_ theme
_v_ variable
_w_ where is something defined
"
  ("b" describe-bindings)
  ("B" describe-personal-keybindings)
  ("C" describe-categodries)
  ("c" describe-char)
  ("C" describe-coding-system)
  ("f" describe-function)
  ("F" flycheck-describe-checker)
  ("i" describe-input-method)
  ("K" describe-key)
  ("k" describe-key-briefly)
  ("l" describe-language-environment)
  ("M" describe-minor-mode)
  ("m" describe-mode)
  ("N" describe-current-coding-system)
  ("n" describe-current-coding-system-briefly)
  ("o" describe-minor-mode-from-indicator)
  ("O" describe-minor-mode-from-symbol)
  ("p" describe-package)
  ("P" describe-text-properties)
  ("q" nil)
  ("a" help)
  ("s" describe-symbol)
  ("t" describe-theme)
  ("v" describe-variable)
  ("w" where-is))
(global-set-key (kbd "C-c C-d") 'hydra-describe/body)


;; {{ git-gutter, @see https://github.com/abo-abo/hydra/wiki/Git-gutter
(defhydra hydra-git (:body-pre
                     (progn
                       (git-gutter-mode 1)
                       (setq git-link-use-commit t))
                     :after-exit (setq git-link-use-commit nil)
                     :color blue)
"
Git:
[_dd_] Diff               [_ri_] Rebase closest
[_dc_] Diff staged        [_s_] Show commit
[_dr_] Diff range         [_rr_] Reset gutter
[_au_] Add modified       [_rh_] Gutter => HEAD
[_cc_] Commit             [_l_] Log selected/file
[_ca_] Amend              [_b_] Branches
[_ja_] Amend silent       [_k_] Git commit link
[_tt_] Stash              [_Q_] Quit gutter
[_ta_] Apply stash        [_cr_] Cherry pick from reflog
[_f_] Find file in commit
"
  ("ri" my-git-rebase-interactive)
  ("rr" git-gutter-reset-to-default)
  ("rh" my-git-gutter-reset-to-head-parent)
  ("s" my-git-show-commit)
  ("l" magit-log-buffer-file)
  ("b" magit-show-refs-popup)
  ("k" git-link)
  ("g" magit-status)
  ("ta" magit-stash-apply)
  ("tt" magit-stash)
  ("dd" magit-diff-dwim)
  ("dc" magit-diff-staged)
  ("dr" (magit-diff-range (my-git-commit-id)))
  ("cc" magit-commit-create)
  ("ca" magit-commit-amend)
  ("ja" (magit-commit-amend '("--reuse-message=HEAD" "--no-verify")))
  ("au" magit-stage-modified)
  ("Q" git-gutter-toggle)
  ("f" my-git-find-file-in-commit)
  ("cr" my-git-cherry-pick-from-reflog)
  ("q" nil))
(global-set-key (kbd "C-c C-v") 'hydra-git/body)
;; }}


;; {{ @see https://github.com/abo-abo/hydra/wiki/Window-Management

;; helpers from https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let* ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let* ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let* ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let* ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defhydra hydra-window ()
  "
Movement^^   ^Split^         ^Switch^     ^Resize^
-----------------------------------------------------
_h_ Left     _v_ertical      _b_uffer     _q_ X left
_j_ Down     _x_ horizontal  _f_ind files _w_ X Down
_k_ Top      _z_ undo        _a_ce 1      _e_ X Top
_l_ Right    _Z_ reset       _s_wap       _r_ X Right
_F_ollow     _D_elete Other  _S_ave       max_i_mize
_SPC_ cancel _o_nly this     _d_elete
"
  ("h" windmove-left )
  ("j" windmove-down )
  ("k" windmove-up )
  ("l" windmove-right )
  ("q" hydra-move-splitter-left)
  ("w" hydra-move-splitter-down)
  ("e" hydra-move-splitter-up)
  ("r" hydra-move-splitter-right)
  ("b" ivy-switch-buffer)
  ("f" counsel-find-file)
  ("F" follow-mode)
  ("a" (lambda ()
         (interactive)
         (ace-window 1)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("s" (lambda ()
         (interactive)
         (ace-window 4)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("S" save-buffer)
  ("d" delete-window)
  ("D" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("o" delete-other-windows)
  ("i" ace-delete-other-windows)
  ("z" (progn
         (winner-undo)
         (setq this-command 'winner-undo)))
  ("Z" winner-redo)
  ("SPC" nil))
(global-set-key (kbd "C-c C-w") 'hydra-window/body)
;; }}


;; increase and decrease font size in GUI emacs
;; @see https://oremacs.com/download/london.pdf
(when (display-graphic-p)
  ;; Since we already use GUI Emacs, f2 is definitely available
  (defhydra hydra-zoom (global-map "<f2>")
    "Zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")
    ("r" (text-scale-set 0) "reset")
    ("0" (text-scale-set 0) :bind nil :exit t)
    ("1" (text-scale-set 0) nil :bind nil :exit t)))
(defvar whitespace-mode nil)

;; {{ @see https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
(defhydra hydra-toggle (:color pink)
  "
_u_ company-ispell     %(if (memq 'company-ispell company-backends) t)
_a_ abbrev-mode:       %`abbrev-mode
_d_ debug-on-error:    %`debug-on-error
_f_ auto-fill-mode:    %`auto-fill-function
_t_ truncate-lines:    %`truncate-lines
_w_ whitespace-mode:   %`whitespace-mode
_i_ indent-tabs-mode:   %`indent-tabs-mode
"
  ("u" toggle-company-ispell nil)
  ("a" abbrev-mode nil)
  ("d" toggle-debug-on-error nil)
  ("f" auto-fill-mode nil)
  ("t" toggle-truncate-lines nil)
  ("w" whitespace-mode nil)
  ("i" (lambda () (interactive) (setq indent-tabs-mode (not indent-tabs-mode))) nil)
  ("q" nil "quit"))
;; Recommended binding:
(global-set-key (kbd "C-c C-t") 'hydra-toggle/body)
;; }}

;; {{ dired

(with-eval-after-load 'dired
  (defun my-replace-dired-base (base)
    "Change file name in `wdired-mode'"
    (let* ((fp (dired-file-name-at-point))
           (fb (file-name-nondirectory fp))
           (ext (file-name-extension fp))
           (dir (file-name-directory fp))
           (nf (concat base "." ext)))
      (when (yes-or-no-p (format "%s => %s at %s?"
                                 fb nf dir))
        (rename-file fp (concat dir nf)))))

  (defun my-copy-file-info (fn)
    (message "%s => clipboard & yank ring"
             (copy-yank-str (funcall fn (dired-file-name-at-point)))))
  (defhydra hydra-dired (:color blue)
    "
^File^
--------------------------
[_cf_] New
[_R_]  Move
[_nn_] Name
[_pp_] Path
[_bb_] Base
[_rr_] Rename
[_ff_] Find
[_dd_] directory
[_C_]  Copy
[_rb_] Change base
[_df_] Diff 2 files
[_+_] Create directory
"
    ("pp" (my-copy-file-info 'file-truename))
    ("nn" (my-copy-file-info 'file-name-nondirectory))
    ("bb" (my-copy-file-info 'file-name-base))
    ("dd" (my-copy-file-info 'file-name-directory))
    ("rb" (my-replace-dired-base (car kill-ring)))
    ("C" dired-do-copy)
    ("R" dired-do-rename)
    ("cf" find-file)
    ("df" my-ediff-files)
    ("rr" dired-toggle-read-only)
    ("ff" (lambda (regexp)
            (interactive "sMatching regexp: ")
            (find-lisp-find-dired default-directory regexp)))
    ("+" dired-create-directory)
    ("q" nil)))

(defun dired-mode-hook-hydra-setup ()
  (local-set-key (kbd "y") 'hydra-dired/body))
(add-hook 'dired-mode-hook 'dired-mode-hook-hydra-setup)
;; }}

(provide 'init-hydra)
