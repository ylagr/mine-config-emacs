; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs

;;; code:
;;;** main
;; first load config

(if t
    ""
  (use-package benchmark-init :ensure t
    :disabled
    :init
    (benchmark-init/activate)
    (add-hook 'after-init-hook 'benchmark-init/deactivate)
    )
  )
(if t
    ""
  (defun elpaca-init()
    (defvar elpaca-installer-version 0.12)
    (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
    (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
    (defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
    (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
				  :ref nil :depth 1 :inherit ignore
				  :files (:defaults "elpaca-test.el" (:exclude "extensions"))
				  :build (:not elpaca-activate)))
    (let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
	   (build (expand-file-name "elpaca/" elpaca-builds-directory))
	   (order (cdr elpaca-order))
	   (default-directory repo))
      (add-to-list 'load-path (if (file-exists-p build) build repo))
      (unless (file-exists-p repo)
	(make-directory repo t)
	(when (<= emacs-major-version 28) (require 'subr-x))
	(condition-case-unless-debug err
            (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                      ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                      ,@(when-let* ((depth (plist-get order :depth)))
							  (list (format "--depth=%d" depth) "--no-single-branch"))
                                                      ,(plist-get order :repo) ,repo))))
                      ((zerop (call-process "git" nil buffer t "checkout"
                                            (or (plist-get order :ref) "--"))))
                      (emacs (concat invocation-directory invocation-name))
                      ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                            "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                      ((require 'elpaca))
                      ((elpaca-generate-autoloads "elpaca" repo)))
		(progn (message "%s" (buffer-string)) (kill-buffer buffer))
              (error "%s" (with-current-buffer buffer (buffer-string))))
	  ((error) (warn "%s" err) (delete-directory repo 'recursive))))
      (unless (require 'elpaca-autoloads nil t)
	(require 'elpaca)
	(elpaca-generate-autoloads "elpaca" repo)
	(let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
    (add-hook 'after-init-hook #'elpaca-process-queues)
    (elpaca `(,@elpaca-order))

    ;; Install use-package support
    (elpaca elpaca-use-package
	    ;; Enable use-package :ensure support for Elpaca.
	    (elpaca-use-package-mode))
    )
  )
;; (elpaca-init)
(defvar l/init-plugin t)
(setq native-comp-jit-compilation t)
(setq native-comp-jit-compilation-deny-list '("lsp-bridge"))
(setq native-comp-speed 3)
(setq native-comp-async-report-warnings-errors 'silent)
(setq native-comp-async-jobs-number 2)

(setq l/builtin-file (expand-file-name "init-builtin-mini.el" user-emacs-directory))
(load-file l/builtin-file)
;; (native-compile-async l/builtin-file nil t)


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


