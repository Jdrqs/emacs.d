;; -*- coding: utf-8; lexical-binding: t; -*-

(setq enable-local-variables :safe)

(setq user-init-file (or load-file-name (buffer-file-name)))

(setq user-emacs-directory (file-name-directory user-init-file))

(defconst my-emacs-dir (file-name-as-directory user-emacs-directory)
  "Directory of emacs.d.")

(defconst my-site-lisp-dir (concat my-emacs-dir "site-lisp")
  "Directory of site-lisp.")

(defconst my-lisp-dir (concat my-emacs-dir "lisp")
  "Directory of download.")

(defun require-init (init-file)
  "Load PKG if MAYBE-DISABLED is nil or it's nil but start up in normal slowly."
  (load (file-truename (format "%s/%s" my-lisp-dir init-file)) t t))

(defun my-add-subdirs-to-load-path (lisp-dir)
  "Add sub-directories under LISP-DIR into `load-path'."
  (let* ((default-directory lisp-dir))
    (setq load-path
          (append
           (delq nil
                 (mapcar (lambda (dir)
                           (unless (string-match-p "^\\." dir)
                             (expand-file-name dir)))
                         (directory-files my-site-lisp-dir)))
           load-path))))

;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(let* ((file-name-handler-alist nil))

  (require-init 'init-autoload)
  (require-init 'init-modeline)
  (require-init 'init-utils)
  (require-init 'init-file-type)
  (require-init 'init-elpa)
  (require-init 'init-windows)
  (require-init 'init-linum-mode)
  (require-init 'init-ibuffer)
  (require-init 'init-company)
  (require-init 'init-lisp)
  (require-init 'init-ivy)
  (require-init 'init-clipboard)
  (require-init 'init-essential)
  (require-init 'init-theme)
  (require-init 'init-dired)
  (require-init 'init-yasnippet)
  (require-init 'init-keyfreq)
  (require-init 'init-term-mode)
  (require-init 'init-workgroup)
  (require-init 'init-hydra)
  (require-init 'init-shackle)
  (require-init 'init-haskell)
  (require-init 'init-evil)
  (require-init 'init-diff)

(setq custom-file (expand-file-name (concat my-emacs-dir "custom-set-variables.el"))))

(if (file-exists-p custom-file) (load custom-file t t))
(load (expand-file-name "~/.custom.el") t nil)

;; set windos gc
(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  67108864) ; 64M
  (setq gc-cons-percentage 0.1) ; original value
  (garbage-collect))

(run-with-idle-timer 4 nil #'my-cleanup-gc)

;;; Local Variables:
;;; no-byte-compile: t
;;; End:
