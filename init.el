; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; code:

;; first load config
(load-file (expand-file-name "init-builtin-mini.el" user-emacs-directory))

;; 使用default-frame-alist设置了默认字体了
;; (unless (daemonp)
;;   (l/normal-font)
;;  )


;; (mini-frame)


