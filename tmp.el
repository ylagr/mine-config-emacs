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

(setq-default mode-line-format (append '("🅚") mode-line-format))
(defvar modeline)
(setq modeline mode-line-format)
(setq-default mode-line-format modeline)
(setq-default mode-line-format (append '((:eval (curstate))) mode-line-format))
(:eval (state))
(defvar state)
(defun l/repeat-map-curstate ()
  'l/repeat-map-state
  )
(setq state "🅘")
(setq state "🅚")

(use-package wgrep
  :bind
  (:map grep-mode-map
	("C-c C-p" . wgrep-change-to-wgrep-mode))
  :config
;;  (keymap-set grep-mode-map "M-." #'wgrep-change-to-wgrep-mode)
  )













;; key bind mode
(use-package meow
  :disabled
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-define-key
  ; '("j" . meow-next)
  ; '("k" . meow-prev)
  ; '("<escape>" . ignore)
   )
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   )
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-right)
   '("B" . clear-line)
   ;; '("b" . meow-back-word)
   ;; '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-kill)
   '("d" . meow-next)
   '("D" . meow-next-expand)
   ;; '("d" . meow-delete)
   ;; '("D" . meow-backward-delete)
   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   ;; '("e" . meow-next-word)
   ;; '("E" . meow-next-symbol)
   '("f" . meow-right)
   '("F" . meow-right-expand)
   ;; '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-next)
   ;; '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   ;; '("p" . meow-yank)
   '("p" . meow-prev)
   '("P" . meow-yank)
   ;; '("q" . meow-quit)
   '("q" . meow-cancel-selection)
   '("Q" . meow-quit)
   ;; '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-left)
   '("S" . meow-left-expand)
   ;; '("s" . meow-kill)
   '("t" . meow-find)
   '("T" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-next-word)
   '("V" . meow-next-symbol)
   ;; '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   ;; '("z" . meow-pop-selection)
   '("`" . meow-pop-selection)
   '("z" . meow-back-word)
   '("Z" . meow-back-symbol)
   '("'" . repeat)
   '("<escape>" . ignore)
   '("/" . meow-visit)
   '("?" . meow-search)
   '(":" . execute-extended-command)
   )
   (setq
    meow-cursor-type-normal 'hbar
    meow-cursor-type-insert '(bar . 4)
    meow-expand-hint-remove-delay 60.0
    )
   (add-to-list 'meow-mode-state-list '(ement-room-mode . insert))
   )
  (defun meow-setup-modeline ()
      (setq meow-replace-state-name-list
        '((normal . "🅝 NORMAL")
          (beacon . "🅑 BEACON")
          (insert . "🅘 INSERT")
          (motion . "🅜 MOTION")
          (keypad . "🅚 KEYPAD"))
        )
    )
  (meow-setup)
  (meow-setup-modeline)
  ;(meow-setup-line-number)
  (meow-setup-indicator)
  (meow-global-mode 1)
 
  ;; (meow-motion-define-key '("n" . next-line))
  ;; (meow-motion-define-key '("p" . previous-line))
  ;; (meow-motion-define-key '("f" . forward-char))
  ;; (meow-motion-define-key '("b" . backward-char))
  ;; (meow-motion-define-key `("x" . ,(kbd "C-x")))
  ;; (meow-motion-define-key `("<escape>" . ,(kbd "<escape>")))
  ;; (meow-motion-define-key '("q" . meow-motion-mode))
  ;; (meow-motion-define-key '("i" . meow-motion-mode))
  ;; (global-set-key (kbd "C-c m") 'meow-motion-mode)
  ;; (defun meow-motion-enable()
  ;;   (interactive)
  ;;   (setq meow-motion-mode t)
  ;;   (message "Meow-Motion mode enable in current buffer" )
  ;;   )
  ;; (global-set-key (kbd "C-'") 'meow-motion-enable)
  (defun meow-enable ()
    "Enable meow."
    (interactive)
    (meow-global-mode +1)
    (meow-setup-indicator)
    )
  (defun meow-disable ()
    "Disable meow."
    (interactive)
    (meow-global-mode -1)
    (cursor-type-default)
    )
  (global-set-key (kbd "C-'") 'meow-enable)
  (global-set-key (kbd "C-.") 'meow-disable)
  )










(provide 'tmp)

;;; tmp.el ends here.
