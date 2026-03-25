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


;;(add-to-list 'electric-pair-pairs '("/\\*" " */" t))
;;(add-to-list 'electric-pair-text-pairs '("/\\*" . " */"))


; Local Variables: outline-regexp: ";;;\\*+\\|\\`" eval: (outline-minor-mode 1) eval: (outline-hide-sublevels 4) End:


(if t
    "test"
  (make-local-variable 'tool-bar-map)
  (setq tool-bar-map (copy-keymap tool-bar-map))
  (tool-bar-local-item "save" 'customize-group 'cfg-local
		       tool-bar-map :help "config current"
		       )
  ;;(force-mode-line-update t)
  (setq org-agenda-todo-list-sublevels t)
  ;; 设置 agenda的显示效果，但是没有super agenda的显示效果好
  (setq org-agenda-prefix-format
      '((agenda  . " %i %-12:c%?-12t% s %-10b") ; %b 增加父级路径显示
        (todo    . " %i %-12:c %-10b")           ; TODO 视图也显示
        (tags    . " %i %-12:c")
        (search  . " %i %-12:c")))
  ;; 设置面包屑的分隔符和最大长度
(setq org-agenda-breadcrumbs-separator " -> \n\t")
  )

(if nil
    ""
  (setq org-agenda-files '("~/org/gtd.org"))
  )
(if nil
    ""
  (defun my-new-scratch ()
    "创建一个新的、唯一的 scratch buffer。"
    (interactive)
    (let ((n 1)
          (bufname ""))
      (while (progn
               (setq bufname (format "*scratch-<%d>*" n))
               (get-buffer bufname))
	(setq n (1+ n)))
      (switch-to-buffer (get-buffer-create bufname))
      (lisp-interaction-mode)))

  ;; 绑定快捷键
  (global-set-key (kbd "C-c n") 'my-new-scratch)

  )
  

