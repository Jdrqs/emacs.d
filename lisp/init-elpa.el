;; -*- codingc: utf-8; lexical-binding: t -*-

(defun my-initialize-package ()
  "Configure package load."
  (cond
   ((>= emacs-major-version 27)
      (setq package-quickstart t))
   (t
    (package-initialize))))

(my-initialize-package)

(defvar melpa-include-packages
  '(
    esup
    popup
    pomodoro
    highlight-escape-sequences
    native-complete
    company-native-complete
    company-ctags
    company-c-headers
    ivy-rich
    popuptip
    auto-yasnippet
    buffer-move
    evil
    evil-exchange
    evil-find-char-pinyin
    undo-fu
    undo-tree
    dhall-mode
    general
    )
  "Packages to install from melpa-unstable.")

(defvar melpa-stable-banned-packages nil
  "Banned packages from melpa-stable.")

;; I don't use any packages from GNU ELPA because I want to minimize
;; dependency on 3rd party web site.
(setq package-archives
      '(
        ;; uncomment below line if you need use GNU ELPA
         ("gnu" . "https://elpa.gnu.org/packages/")
         ("melpa" . "https://melpa.org/packages/")
         ("melpa-stable" . "https://stable.melpa.org/packages/")

        ;; Use either 163 or tsinghua mirror repository when official melpa
        ;; is slow or shutdown.

        ;; ;; {{ Option 1: 163 mirror repository:
        ;;("gnu" . "https://mirrors.163.com/elpa/gnu/")
        ;;("melpa" . "https://mirrors.163.com/elpa/melpa/")
        ;;("melpa-stable" . "https://mirrors.163.com/elpa/melpa-stable/")
        ;; ;; }}

        ;; ;; {{ Option 2: tsinghua mirror repository
        ;; ;; @see https://mirror.tuna.tsinghua.edu.cn/help/elpa/ on usage:
        ;; ;; ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ;; ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ;; ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
        ;; }}
        ))

(defvar my-ask-elpa-mirror nil)
(when (and (not noninteractive) ; no popup in batch mode
           my-ask-elpa-mirror
           (not (file-exists-p (file-truename (concat my-emacs-dir "elpa"))))
           (yes-or-no-p "Switch to faster package repositories in China temporarily?
You still need modify `package-archives' in \"init-elpa.el\" to PERMANENTLY use this ELPA mirror."))
  (setq package-archives
        '(("melpa" . "https://mirrors.163.com/elpa/melpa/")
          ("melpa-stable" . "https://mirrors.163.com/elpa/melpa-stable/"))))

;; Un-comment below line if you follow "Install stable version in easiest way"
;; (setq package-archives '(("myelpa" . "~/myelpa/")))

;; my local repository is always needed.
(push (cons "localelpa" (concat my-emacs-dir "localelpa/")) package-archives)

(defun my-package-generate-autoloads-hack (pkg-desc pkg-dir)
  "Stop (PKG-DESC PKG-DIR) package.el from leaving open autoload files lying around."
  (let* ((path (expand-file-name (concat
                                  ;; pkg-desc is string in emacs 24.3.1,
                                  (if (symbolp pkg-desc) (symbol-name pkg-desc) pkg-desc)
                                  "-autoloads.el")
                                 pkg-dir)))
    (with-current-buffer (find-file-existing path)
      (kill-buffer nil))))
(advice-add 'package-generate-autoloads :after #'my-package-generate-autoloads-hack)

(defun my-package--add-to-archive-contents-hack (orig-func &rest args)
  "Some (ORIG-FUNC ARGS) packages should be hidden."
  (let* ((package (nth 0 args))
         (archive (nth 1 args))
         (pkg-name (car package))
         (version (package--ac-desc-version (cdr package)))
         (add-to-p t))
    (cond
     ((string= archive "melpa-stable")
      (setq add-to-p
            (not (memq pkg-name melpa-stable-banned-packages))))

     ;; We still need use some unstable packages
     ((string= archive "melpa")
      (setq add-to-p
            (or (member pkg-name melpa-include-packages)
                ;; color themes are welcomed
                (string-match-p "-theme" (format "%s" pkg-name))))))

    (when add-to-p
      ;; The package is visible through package manager
      (apply orig-func args))))
(advice-add 'package--add-to-archive-contents :around #'my-package--add-to-archive-contents-hack)

;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Ask elpa (MIN-VERSION) to install given PACKAGE."
  (my-ensure 'package)
  (cond
   ((package-installed-p package min-version)
    t)
   ((or (assoc package package-archive-contents) no-refresh)
    (package-install package))
   (t
    (package-refresh-contents)
    (require-package package min-version t))))

;;------------------------------------------------------------------------------
;; Fire up package.el and ensure the following packages are installed.
;;------------------------------------------------------------------------------
;;(require-package 'esup) ; esup hava a bug, remove it
;;(require-package 'diminish) ; toggle mirror mode modeline str
(require-package 'popup) ; some old package need it
(require-package 'paredit)
(require-package 'scratch)
(require-package 'rainbow-delimiters)
(require-package 'winum)
(require-package 'ace-window)
(require-package 'company)
(require-package 'company-statistics)
(require-package 'company-ctags)
(require-package 'native-complete); dependency download
(require-package 'company-c-headers)
(require-package 'company-native-complete)
;;(require-package 'company-quickhelp)
(require-package 'session)
(require-package 'counsel)
(require-package 'swiper)
(require-package 'ivy)
(require-package 'ivy-rich)
(require-package 'flx)
(require-package 'smex) ;; sort command by frequently
(require-package 'wgrep) ;; fomate grep result buffer
(require-package 'diredfl)
(require-package 'diff-hl)
(require-package 'shackle)
(require-package 'find-file-in-project)
(require-package 'neotree)
(require-package 'which-key)
(require-package 'pomodoro)
(require-package 'beacon)
(require-package 'symbol-overlay)
(require-package 'browse-kill-ring)
(require-package 'hungry-delete)
(require-package 'flycheck)
(require-package 'workgroups2)
(require-package 'magit)
(require-package 'auto-yasnippet)
(require-package 'yasnippet)
(require-package 'yasnippet-snippets)
(require-package 'hydra)
(require-package 'buffer-move)
(require-package 'evil)
(require-package 'general)
(require-package 'evil-escape)
(require-package 'evil-exchange)
(require-package 'evil-find-char-pinyin)
(require-package 'evil-mark-replace)
(require-package 'evil-matchit)
(require-package 'evil-nerd-commenter)
(require-package 'evil-surround)
(require-package 'evil-visualstar)
(require-package 'undo-fu)
(require-package 'undo-tree)
(require-package 'xclip) ;;=> from gun
(require-package 'cliphist)
(require-package 'haskell-mode)
(require-package 'dante)
(require-package 'reformatter)
(require-package 'dhall-mode)
(require-package 'keyfreq)

(defun my-install-popular-themes (popular-themes)
  "Install POPULAR-THEMES from melpa."
 (dolist (theme popular-themes)
    (require-package theme)))

;; speed up CI
(unless my-disable-idle-timer
  ;; most popular 100 themes
  (my-install-popular-themes
   '(
     color-theme-sanityinc-tomorrow
     modus-themes
     doom-themes
     dracula-theme
     )))

;; }}

;; kill buffer without my confirmation
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(provide 'init-elpa)
