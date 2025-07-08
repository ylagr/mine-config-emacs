;;; -*- lexical-binding: t; -*-

;; restore dumped load-path
(when (boundp '+saved-load-path-during-dump)
  (message "starting from a dump file ...")
  (setq load-path +saved-load-path-during-dump)
  (message "%s" +saved-load-path-during-dump)
  )


;; set package archives. possibly set mirrors
(defconst +i-am-in-china +1)

(with-eval-after-load 'package
  (if +i-am-in-china
      (setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                               ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
                               ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")))
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))))

;; quick start
(setq package-quickstart t)
(setq package-quickstart-file (expand-file-name "var/package-quickstart.el" user-emacs-directory))

;; set frame parameters early to prevent flickering.
(setq default-frame-alist
      '((height . 50)
	(width . 120)
	(vertical-scroll-bars . +1)
	(tool-bar-mode . +1)
	)
      )

;; must be set before loading use-package
(setq use-package-enable-imenu-support +1)

;; customization file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
