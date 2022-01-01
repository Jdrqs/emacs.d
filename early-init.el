;; don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq inhibit-startup-message t
      initial-major-mode 'fundamental-mode)
