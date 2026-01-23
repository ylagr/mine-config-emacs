; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs

;;; code:
;;;** main

(defvar l/init-plugin t)
;; first load config
(load-file (expand-file-name "init-builtin-mini.el" user-emacs-directory))

;; 使用default-frame-alist设置了默认字体


;; (mini-frame)
;;(l/plugin-start)


(setq view-lossage-auto-refresh t) ;; C-h l auto refresh

(setq electric-indent-actions '(yank ))

(add-to-list 'electric-pair-pairs '("/\\*" " */" t))
;;(add-to-list 'electric-pair-text-pairs '("/*" . " */"))


; Local Variables: outline-regexp: ";;;\\*+\\|\\`" eval: (outline-minor-mode 1) eval: (outline-hide-sublevels 4) End:
