; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; code:

(defvar l/init-plugin t)
;; first load config
(load-file (expand-file-name "init-builtin-mini.el" user-emacs-directory))

;; 使用default-frame-alist设置了默认字体


;; (mini-frame)
;;(l/plugin-start)


(setq eldoc-help-at-pt t) 		;;
(setq view-lossage-auto-refresh t) ;; C-h l auto refresh

(setq electric-indent-actions '(yank ))


;;(setq electric-pair-pairs nil)
;;(setq electric-pair-text-pairs nil)

(add-to-list 'electric-pair-pairs '("/\\*" " */" t))
;;(add-to-list 'electric-pair-text-pairs '("/*" . " */"))
;;aa*/*/ /*    */    */*/*/*/;;;ggggg*/*aaaaa/*/*/*/*/*/*/aaa
(setq hs-display-lines-hidden t)
(setq hs-show-indicators t)
(setq hs-indicator-type nil)
(setq ibuffer-use-header-line 'title)
(setq ibuffer-human-readable-size t)
(setq Buffer-menu-human-readable-sizes t)
(setq even-window-heights nil)
