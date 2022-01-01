;; -*- codingc: utf-8; lexical-binding: t -*-

(with-eval-after-load 'cliphist
  (defun cliphist-routine-before-insert-hack (&optional arg)
    (ignore arg)
    (my-delete-selected-region))
  (advice-add 'cliphist-routine-before-insert :before #'cliphist-routine-before-insert-hack))

;; {{ Write backup files to its own directory
;; @see https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-and-Backup.html
(defvar my-binary-file-name-regexp
  "\\.\\(avi\\|wav\\|pdf\\|mp[34g]\\|mkv\\|exe\\|3gp\\|rmvb\\|rm\\|pyim\\|\\.recentf\\)$"
  "Is binary file name?")

(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not (string-match-p my-binary-file-name-regexp name)))))

(let* ((backup-dir (expand-file-name "~/.backups")))
  (unless (file-exists-p backup-dir) (make-directory backup-dir))
  (setq backup-by-copying t ; don't clobber symlinks
        backup-directory-alist (list (cons "." backup-dir))
        delete-old-versions t
        version-control t  ;use versioned backups
        kept-new-versions 8
        kept-old-versions 4))

;; Donot make backups of files, not safe
;; @see https://github.com/joedicastro/dotfiles/tree/master/emacs
(setq vc-make-backup-files nil)

;; close file backup "~xxx.xx"
(setq make-backup-files nil)
;; }}

;;; {{ GUI frames
;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; Show a marker in the left fringe for lines not in the buffer
(setq indicate-empty-lines t)

(defun my-mini-ui ()
  "Minimum ui."
  ;; NO tool bar, scroll-bar
  (when window-system
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (horizontal-scroll-bar-mode -1)))
(my-mini-ui)
(my-run-with-idle-timer 2 #'my-mini-ui)
;; }}

;; no menu bar
(menu-bar-mode -1)

;; Nicer naming of buffers for files with identical names
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))
(global-set-key (kbd "M-/") 'hippie-expand)

(defun beacon-per-setup ()
  "beacon setup before load beacon mode."
  ;;(setq-default beacon-color "#3cffcc")
  (setq-default beacon-lighter "")
  (setq-default beacon-size 24))

(my-run-with-idle-timer 1.9 'beacon-per-setup)
(my-run-with-idle-timer 2 'beacon-mode)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (file-name-directory buffer-file-name))
                 "%b"))))
(defun my-font-setup ()
  (if (eq system-type 'windows-nt)
      (set-frame-font "PragmataPro 16")
    (set-frame-font "Source Code Pro 16"))

  (if (eq system-type 'windows-nt)
      (setq-local font-string "文泉驿等宽正黑")
    (setq-local font-string "WenQuanYi MicroHei Mono"))

  ;; set chinses font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family font-string :size 22))))

(when (window-system)
  (my-font-setup))
;; }}

;; open header file under cursor
(global-set-key (kbd "C-x C-o") 'ffap)

;; newline beachavier
(define-key global-map (kbd "RET") 'newline-and-indent)

;; {{ isearch
;; Use regex to search by default
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
;; }}

(setq-default buffers-menu-max-size 30
              case-fold-search t
              compilation-scroll-output t
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              grep-highlight-matches t
              grep-scroll-output t
              indent-tabs-mode nil
              line-spacing 0
              mouse-yank-at-point t
              set-mark-command-repeat-pop t
              tooltip-delay 1.5
              ;; void problems with crontabs, etc.
              ;; require-final-newline t ; bad idea, could accidentally edit others' code
              truncate-lines nil
              truncate-partial-width-windows nil
              ;; visible-bell has some issue
              ;; @see https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/issues/9#issuecomment-97848938
              visible-bell nil)

;; @see http://www.emacswiki.org/emacs/SavePlace
(cond
 ((fboundp 'save-place-mode)
  (save-place-mode 1))
 (t
  (require 'saveplace)
  (setq-default save-place t)))

;; {{ bookmark
;; use my own bookmark if it exists
(with-eval-after-load 'bookmark
  (if (file-exists-p (file-truename "~/.emacs.bmk"))
      (setq bookmark-file (file-truename "~/.emacs.bmk"))))
;; }}

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-m") 'counsel-M-x)

;;; {{ add && remove display long lines in style (end line with $)
(defun my-hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\^M []))

(defun my-remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; }}

;; some project prefer tab, so be it
;; @see http://stackoverflow.com/questions/69934/set-4-space-indent-in-emacs-in-text-mode
(setq-default tab-width 4)

(setq history-delete-duplicates t)

;; NO automatic new line when scrolling down at buffer bottom
(setq next-line-add-newlines nil)

;; @see http://stackoverflow.com/questions/4222183/emacs-how-to-jump-to-function-definition-in-el-file
(global-set-key (kbd "C-h C-f") 'find-function)

;; {{ time format
;; If you want to customize time format, read document of `format-time-string'
;; and customize `display-time-format'.
;; (setq display-time-format "%a %b %e")

;; from RobinH, Time management
(setq display-time-24hr-format t) ; the date in modeline is English too, magic!
(setq display-time-day-and-date t)
(my-run-with-idle-timer 2 #'display-time)
;; }}

;;a no-op function to bind to if you want to set a keystroke to null
(defun void ()
  "This is a no-op."
  (interactive))

(defalias 'list-buffers 'ibuffer)

(setq system-time-locale "C")

(setq imenu-max-item-length 256)

;; {{ recentf-mode
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-saved-items 2048
      recentf-exclude '("/tmp/"
                        "/ssh:"
                        "/sudo:"
                        "recentf$"
                        "company-statistics-cache\\.el$"
                        ;; ctags
                        "/TAGS$"
                        ;; global
                        "/GTAGS$"
                        "/GRAGS$"
                        "/GPATH$"
                        ;; binary
                        "\\.mkv$"
                        "\\.mp[34]$"
                        "\\.avi$"
                        "\\.wav$"
                        "\\.docx?$"
                        "\\.xlsx?$"
                        ;; sub-titles
                        "\\.sub$"
                        "\\.srt$"
                        "\\.ass$"
                        ;; "/home/[a-z]\+/\\.[a-df-z]" ; configuration file should not be excluded
                        ))
;; }}

;; {{
(defun my-minibuffer-setup-hook ()
  (local-set-key (kbd "M-y") 'paste-from-x-clipboard)
  (local-set-key (kbd "C-k") 'kill-line)
  (subword-mode 1) ; enable subword movement in minibuffer
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 67108864 ))

;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(defun minibuffer-inactive-mode-hook-setup ()
  "Mibibuffer more profermance."
  ;; Make `try-expand-dabbrev' from `hippie-expand' work in mini-buffer.
  ;; @see `he-dabbrev-beg', so we need re-define syntax for '/'.
  (set-syntax-table (let* ((table (make-syntax-table)))
                      (modify-syntax-entry ?/ "." table)
                      table)))

(add-hook 'minibuffer-inactive-mode-hook 'minibuffer-inactive-mode-hook-setup)
;; }}

;; {{ indention management
(defun my-toggle-indentation ()
  "Toggle INDENT-TABS-MODE."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "indent-tabs-mode=%s" indent-tabs-mode))

(defun my-indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indent selected region."))
      (progn
        (indent-region (point-min) (point-max))
        (message "Indent buffer.")))))
;; }}

(defun my-get-total-hours ()
  (interactive)
  (let* ((str (if (region-active-p) (my-selected-str)
                (my-buffer-str)))
         (total-hours 0)
         (lines (nonempty-lines str)))
    (dolist (l lines)
      (if (string-match " \\([0-9][0-9.]*\\)h[ \t]*$" l)
          (setq total-hours (+ total-hours (string-to-number (match-string 1 l))))))
    (message "total-hours=%s" total-hours)))


;; flymake
(with-eval-after-load 'flymake
  (setq flymake-gui-warnings-enabled nil))

;; @see https://stackoverflow.com/questions/3417438/closing-all-other-buffers-in-emacs
(defun kill-all-but-current-buffer ()
  "Kill other buffers."
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(with-eval-after-load 'grep
  ;; eacl and other general grep (rgrep, grep ...) setup
  (dolist (v '("auto"
               "target"
               "node_modules"
               "bower_components"
               "*dist"
               ".sass_cache"
               ".cache"
               ".npm"
               "elpa"))
    (add-to-list 'grep-find-ignored-directories v))
  (dolist (v '("*.min.js"
               "*.map"
               "*.bundle.js"
               "*.min.css"
               "tags"
               "TAGS"
               "GTAGS"
               "GRTAGS"
               "GPATH"
               "cscope.files"
               "*.json"
               "*.log"))
    (add-to-list 'grep-find-ignored-files v))

  ;; wgrep and rgrep, inspired by http://oremacs.com/2015/01/27/my-refactoring-workflow/
  (define-key grep-mode-map
    (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))

(defun my-wgrep-mark-deletion-hack (&optional arg)
  "After mark a line for deletion, move to next line.
ARG is ignored."
  (ignore arg)
  (forward-line))
(advice-add 'wgrep-mark-deletion :after #'my-wgrep-mark-deletion-hack)

;; wgrep and rgrep, inspired by http://oremacs.com/2015/01/27/my-refactoring-workflow/
(with-eval-after-load 'wgrep
  '(define-key grep-mode-map
     (kbd "C-c C-c") 'wgrep-finish-edit))

;; {{ https://www.emacswiki.org/emacs/EmacsSession better than "desktop.el" or "savehist".
;; Any global variable matching `session-globals-regexp' is saved *automatically*.
(setq session-save-file (expand-file-name (concat my-emacs-dir ".session")))
(setq session-globals-max-size 2048)
;; can store 8Mb string
(setq session-globals-max-string (* 8 1024 1024))
(setq session-globals-include '(kill-ring
                                (session-file-alist 100 t)
                                my-dired-commands-history
                                file-name-history
                                search-ring
                                regexp-search-ring))
(setq session-save-file-coding-system 'utf-8)
(add-hook 'after-init-hook 'session-initialize)
;; }}


(global-set-key (kbd "M-Y") 'browse-kill-ring)
(with-eval-after-load 'browse-kill-ring
  (setq browse-kill-ring-separator "--------------------------")
  (define-key browse-kill-ring-mode-map (kbd "q") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "C-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "C-p") 'browse-kill-ring-previous))

(with-eval-after-load 'page-break-lines
  (add-to-list 'page-break-lines-modes 'browse-kill-ring-mode))

(defun switch-to-builtin-shell ()
  "Switch to builtin shell.
If the shell is already opened in some buffer, switch to that buffer."
  (interactive)
  (let* ((buf-name (if (eq system-type 'windows-nt) "*shell*" "*ansi-term*"))
         (buf (get-buffer buf-name))
         (wins (window-list))
         current-frame-p)
    (cond
     ;; A shell buffer is already opened
     ((buffer-live-p buf)
      (dolist (win wins)
        (when (string= (buffer-name (window-buffer win)) buf-name)
          (when (window-live-p win)
            (setq current-frame-p t)
            (select-window win))))
      (unless current-frame-p
        (switch-to-buffer buf)))
     ;; Windows
     ((eq system-type 'windows-nt)
      (shell))
     ;; Linux
     (t
      (ansi-term my-term-program)))))

;; global-auto-revert-mode is a mirror mode that update file when file is changed in otherwise.
(unless (eq system-type 'windows-nt)
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  (my-run-with-idle-timer 4 #'global-auto-revert-mode))

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Ctrl-X, u/l  to upper/lowercase regions without confirm
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; midnight mode purges buffers which haven't been displayed in 3 days
(my-run-with-idle-timer 4 #'midnight-mode)

;; I'm in Australia now, so I set the locale to "en_AU"
(defun my-insert-date (prefix)
  "Insert the current date with prefix-argument, use ISO format.
With two PREFIX arguments, write out the day and month name."
  (interactive "P")
  (let* ((format (cond
                  ((not prefix) "%d.%m.%Y")
                  ((equal prefix '(4)) "%Y-%m-%d")
                  ((equal prefix '(16)) "%d %B %Y"))))
    (insert (format-time-string format))))

;;compute the length of the marked region
(defun region-length ()
  "Length of a selected region."
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))

;; show ascii table
(defun ascii-table ()
  "Print the ascii table."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let* ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (beginning-of-buffer))

;; {{ unique lines
;; https://gist.github.com/ramn/796527
;; uniq-lines
(defun uniq-lines (start end)
  "Unique lines(START END)."
  (interactive "*r")
  (delete-duplicate-lines start end))
;; }}

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a BUFFER-SAVE-HOOK, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

;; {{ wgrep setup
(with-eval-after-load 'wgrep
  ;; save the change after wgrep finishes the job
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-too-many-file-length 2024))
;; }}

;; {{ which-key-mode
(defvar my-show-which-key-when-press-C-h nil)
(with-eval-after-load 'which-key
  (setq which-key-allow-imprecise-window-fit t) ; performance
  (setq which-key-separator ":")
  (setq which-key-idle-delay 1.5)
  (when my-show-which-key-when-press-C-h
    ;; @see https://twitter.com/bartuka_/status/1327375348959498240?s=20
    ;; Therefore, the which-key pane only appears if I hit C-h explicitly.
    ;; C-c <C-h> for example - by Wanderson Ferreira
    (setq which-key-idle-delay 10000)
    (setq which-key-show-early-on-C-h t))
  (setq which-key-idle-secondary-delay 0.05))
(my-run-with-idle-timer 2 #'which-key-mode)
;; }}

;; {{ pomodoro
(with-eval-after-load 'pomodoro
  (setq pomodoro-play-sounds nil) ; *.wav is not installed
  (setq pomodoro-break-time 2)
  (setq pomodoro-long-break-time 5)
  (setq pomodoro-work-time 15)
  ;; Instead of calling `pomodoro-add-to-mode-line`
  (push '(pomodoro-mode-line-string pomodoro-mode-line-string) mode-line-format))

;; {{

;; https://stackoverflow.com/questions/4506249/how-can-i-make-emacs-org-mode-open-links-to-sites-in-google-chrome
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")
(if (eq system-type 'windows-nt)
    (setq browse-url-browser-function 'browse-url-default-windows-browser))


(setq browse-url-generic-program (executable-find "vivaldi-stable")
      browse-url-browser-function 'browse-url-generic)

(defun my-browse-file (file)
  "Browse FILE as url using `browse-url'."
  (when (and file (file-exists-p file))
    (browse-url-generic (concat "file://" file))))

(defun my-browse-current-file ()
  "Browse current file."
  (interactive)
  (my-browse-file buffer-file-name))

(defun my-browse-current-file-as-html ()
  "Browse current file as html."
  (interactive)
  (cond
   ((or (not buffer-file-name)
        (not (file-exists-p buffer-file-name))
        (not (string-match-p "html?$" buffer-file-name)))
    (let* ((file (make-temp-file "my-browse-file-" nil ".html")))
      (my-write-to-file (format "<html><body>%s</body></html>" (buffer-string)) file)
      (my-browse-file file)
      (my-run-with-idle-timer 4 (lambda (delete-file file)))))
   (t
    (my-browse-file buffer-file-name))))

;; }}

(defun prog-mode-hook-setup ()
  "Personal config fo PROG-MODE."
  (rainbow-delimiters-mode 1)
  (symbol-overlay-mode 1)
  ;; highlight-escape-sequences
  ;;(hes-mode 1)
  (when (boundp 'display-fill-column-indicator)
    (setq-default indicate-buffer-boundaries 'left)
    (setq-default display-fill-column-indicator-character ?\u254e)))

(add-hook 'prog-mode-hook 'prog-mode-hook-setup)

;; Train myself to use M-f and M-b instead
;;(global-unset-key [M-left])
;;(global-unset-key [M-right])

;; {{
;; Random line sorting
(defun my-sort-lines-random (beg end)
  "Sort lines in region from BEG to END randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))
;; }}

;; {{ vc-msg
(defun vc-msg-hook-setup (vcs-type commit-info)
  ;; copy commit id to clipboard
  (my-pclip (plist-get commit-info :id)))
(add-hook 'vc-msg-hook 'vc-msg-hook-setup)

(defun vc-msg-show-code-setup ()
  "Use `ffip-diff-mode' instead of `diff-mode'."
  (my-ensure 'find-file-in-project)
  (ffip-diff-mode))

(add-hook 'vc-msg-show-code-hook 'vc-msg-show-code-setup)
;; }}

;; {{ Whitespace
(setq-default show-trailing-whitespace nil)
(defun my-show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'my-show-trailing-whitespace))

(require-package 'whitespace-cleanup-mode)
(my-run-with-idle-timer 1 'global-whitespace-cleanup-mode)
(global-set-key [remap just-one-space] 'cycle-spacing)
;; }}

;; {{ provides a simple system for tracking and displaying the uptimes of your Emacs sessions.
;; (setq-default uptimes-keep-count 200)
;; (add-hook 'after-init-hook (lambda () (require 'uptimes)))
;; }}

;; {{
;; problem :error in process sentinel: Wrong type argument: (or eieio-object class), nil, obj
;; Work around a bug where esup tries to step into the byte-compiled
;; version of `cl-lib', and fails horribly.
;; (setq esup-depth 0)
;; }}

(if (eq system-type 'windows-nt)
    (setq default-directory "~/"))

;; {{ Answer Yes/No programmically when asked by `y-or-n-p'
(defvar my-default-yes-no-answers nil
  "Usage: (setq my-default-yes-no-answers '((t . \"question1\") (t . \"question2\")))).")
(defun my-y-or-n-p-hack (orig-func &rest args)
  "Answer yes or no automatically for question(ORIG-FUNC ARGS)."
  (let* ((prompt (car args))
         rlt)
    (cond
     ((and my-default-yes-no-answers
           (listp my-default-yes-no-answers))
      (let* ((i 0) found cand)
        (while (and (setq cand (nth i my-default-yes-no-answers))
                    (not found))
          (when (string-match-p (cdr cand) prompt)
            (setq found t)
            (setq rlt (car cand)))
          (setq i (1+ i)))
        (unless found (setq rlt (apply orig-func args)))))
     (t
      (setq rlt (apply orig-func args))))
    rlt))
(advice-add 'y-or-n-p :around #'my-y-or-n-p-hack)
;; }}

(with-eval-after-load 'symbol-overlay
  (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
  (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-remove-all)
  (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
  (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev))

;; Work around a bug where esup tries to step into the byte-compiled
;; version of `cl-lib', and fails horribly.
(setq esup-depth 0)

(provide 'init-essential)
