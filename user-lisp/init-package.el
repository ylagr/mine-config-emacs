; init-package.el --- Initialization package file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; code:

(use-package meow
  :ensure t
  :config
  ;; 1. 注册纯净的 passthrough 状态（完全空的 keymap，按键 100% 穿透到 Major Mode 和 Global）
  (defvar l:meow-passthrough-state-keymap (make-sparse-keymap))

  (meow-define-state passthrough
    "Native Emacs passthrough state without any key intercept."
    :lighter " [emacs]"
    :keymap meow-passthrough-state-keymap)

  ;; 兼容别名
  (defalias 'l:meow-passthrough-mode #'meow-passthrough-mode)
  (defalias 'l:meow-passthrough-mode-p #'meow-passthrough-mode-p)

  ;; 2. 绑定从 passthrough 回到 normal 模式的退回按键（通常保留 ESC）
  (meow-define-keys 'passthrough
    '("<escape>" . meow-normal-mode))

  ;; 3. 将原本进入 insert 模式的命令与状态重定向到 passthrough
  ;; 这样你在 normal 模式按 i, a, c, s, o 等任何会进 insert 的键，都会自动进入 passthrough
  ;; (defun meow--redirect-insert-to-passthrough (orig-fun state &rest args)
  ;;   "Redirect 'insert state to 'passthrough state."
  ;;   (if (eq state 'insert)
  ;;       (apply orig-fun 'passthrough args)
  ;;     (apply orig-fun state args)))
  ;; (advice-add 'meow--switch-state :around #'meow--redirect-insert-to-passthrough)
  (defalias 'meow-insert #'meow-passthrough-mode)

  ;; 4. 配置 major-mode 默认状态表，彻底移除 insert，换成 passthrough（注：值必须为 state 符号）
  (setq meow-mode-state-list
	'((prog-mode . passthrough)
          (text-mode . passthrough)
          (conf-mode . passthrough)
          (special-mode . motion)
          (dired-mode . motion)))

  ;; 5. 设置光标样式（保持与 insert 一样的竖线光标）
  (setq meow-cursor-type-passthrough '(bar . 2))

  ;; 6. 切换窗口/Buffer 时自动激活安全切换到 passthrough
  (defun l:func:meow-switch-to-passthrough-safe ()
    "Switch to passthrough mode automatically in editable buffers."
    (when (and (bound-and-true-p meow-mode)
               (not (minibufferp))
               (not buffer-read-only)
               (not (derived-mode-p 'special-mode 'dired-mode))
               (not (meow-passthrough-mode-p)))
      (meow-passthrough-mode 1)))

  (add-hook 'window-selection-change-functions
            (lambda (_frame) (l:func:meow-switch-to-passthrough-safe)))

  ;; 7. 兼容 lsp-bridge 补全检查：允许在 passthrough 状态下弹出补全
  (with-eval-after-load 'lsp-bridge
    (defun l/lsp-bridge--is-meow-state-advice (orig-fun &rest args)
      (or (apply orig-fun args)
          (bound-and-true-p meow-passthrough-mode)))
    (advice-add 'lsp-bridge--is-meow-state :around #'l/lsp-bridge--is-meow-state-advice))

  :init
  (defun l:func:meow-append ()
    (interactive)
    (let ((use-region (use-region-p)))
      (meow-append)
      (unless use-region
	(forward-char)))))

(provide 'init-package)
