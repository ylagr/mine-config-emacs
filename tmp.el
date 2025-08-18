;;; tmp.el --- elisp tmp                             -*- lexical-binding: t; -*-
;;; Commentary:
;; Copyright (C) 2025  DESKTOP-77TCU49

;; Author: DESKTOP-77TCU49 <suiwp@DESKTOP-77TCU49>
;; Keywords: lib,comm
;;
;;; Code:


;; set head line
(setq-default
   header-line-format
   '("GC: " (:eval (number-to-string gcs-done)) " - " (:eval (number-to-string gc-elapsed)) "s"))



(use-package wgrep
  :bind
  (:map grep-mode-map
	("C-c C-p" . wgrep-change-to-wgrep-mode))
  :config
;;  (keymap-set grep-mode-map "M-." #'wgrep-change-to-wgrep-mode)
  )


(provide 'tmp)

;;; tmp.el ends here.
