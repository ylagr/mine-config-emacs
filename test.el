;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with ‘C-x C-f’ and enter text in its buffer.

;; 1. 定义命令
(defun my-hello-world ()
  "Say hello."
  (interactive)
  (message "Hello from toolbar!"))

;; 2. 内联 XPM 图标（一个简单的笑脸）
(defvar my-hello-icon
  (create-image
   (vector
    "/* XPM */"
    "static char * hello_xpm[] = {"
    "\"16 16 2 1\","
    "\"  c None\","
    "\". c #FF0000\","
    "\"                \","
    "\"   .....        \","
    "\"  .     .       \","
    "\" .  . .  .      \","
    "\" .       .      \","
    "\" .  ...  .      \","
    "\"  .     .       \","
    "\"   .....        \","
    "\"                \","
    "\"                \","
    "\"                \","
    "\"                \","
    "\"                \","
    "\"                \","
    "\"                \","
    "\"                \"};")
   'xpm t))
(defvar my-hello-icon
   (find-image
	(cond
	 (#6=(not (display-color-p))
	     '(#4=
	       (:type pbm :file "close.pbm" . #1=
		      (:foreground #7="black" :background #8="grey75"))
	       #5=(:type xbm :file "close.xbm" . #1#) #2=
	       (:type xpm :file "low-color/close.xpm") #3=
	       (:type xpm :file "close.xpm")))
	 (#10=(< (display-color-cells) 256) '(#2# #3# #4# #5#))
	 (t '(#3# #4# #5#)))
	. #15=(t))
      )

;; 3. 添加到 toolbar
(define-key-after tool-bar-map [my-hello]
  `(menu-item "Hello" my-hello-world
              :help "Say hello"
              :image ,my-hello-icon)
  'help-about)

(define-key tool-bar-map [my-hello]
	    `(menu-item "hello" my-hello-world
			:help "say hello"
			:image ,my-hello-icon)
	    )


(define-key-after tool-bar-map [my-hello]
  `(menu-item "hello" my-hello-world
	      :help "say hello"
	      :image  ,(find-image
	(cond
	 (#6=(not (display-color-p))
	     '(#4=
	       (:type pbm :file "close.pbm" . #1=
		      (:foreground #7="black" :background #8="grey75"))
	       #5=(:type xbm :file "close.xbm" . #1#) #2=
	       (:type xpm :file "low-color/close.xpm") #3=
	       (:type xpm :file "close.xpm")))
	 (#10=(< (display-color-cells) 256) '(#2# #3# #4# #5#))
	 (t '(#3# #4# #5#)))
	. #15=(t))
      )

  )

(lookup-key tool-bar-map [my-hello])

(describe-vector tool-bar-map)

(force-mode-line-update)
(redraw-display)

(insert-image my-hello-icon)




(setq tool-bar-map (make-sparse-keymap))

(define-key tool-bar-map [my-hello]  `(menu-item "Test" my-hello-world :image ,my-safe-icon))




;; 1. 确保在 GUI 模式
(when (display-graphic-p)
  ;; 2. 清空 toolbar
  ;; (setq tool-bar-map (make-sparse-keymap))
  
  ;; 3. 定义一个无害命令
  (defun my-test-func () (interactive) (message "Clicked!"))
  (defvar my-hello-icon
    (find-image
     (cond
      (#6=(not (display-color-p))
	  '(#4=
	    (:type pbm :file "close.pbm" . #1=
		   (:foreground #7="black" :background #8="grey75"))
	    #5=(:type xbm :file "close.xbm" . #1#) #2=
	    (:type xpm :file "low-color/close.xpm") #3=
	    (:type xpm :file "close.xpm")))
      (#10=(< (display-color-cells) 256) '(#2# #3# #4# #5#))
      (t '(#3# #4# #5#)))
     . #15=(t))
    )
  ;; 4. 使用 Emacs 内置图标（避免 XPM 问题）
  ;; (define-key tool-bar-map [my-test]
  ;; '(menu-item "Test" my-test-func
  ;; :image (lambda () (tool-bar--image 'new-file))))
  ;; (define-key tool-bar-map [my-hello]
	      ;; `(menu-item "hello" my-test-func
			  ;; :help "say hello"
			  ;; :image ,my-hello-icon
			  ;; )
	      ;; )
  (define-key-after tool-bar-map [my-hello]
	      `(menu-item "hello" my-test-func
			  :help "say hello"
			  :image ,my-hello-icon
			  )
	      'paste
	      )
  

  (redraw-display)
  )


(when (display-graphic-p)
  ;; 2. 清空 toolbar
  (setq tool-bar-map (make-sparse-keymap))
  
  ;; 3. 定义一个无害命令
  (defun my-test-func () (interactive) (message "Clicked!"))
  (defvar my-hello-icon
    (find-image
     (cond
      (#6=(not (display-color-p))
	  '(#4=
	    (:type pbm :file "close.pbm" . #1=
		   (:foreground #7="black" :background #8="grey75"))
	    #5=(:type xbm :file "close.xbm" . #1#) #2=
	    (:type xpm :file "low-color/close.xpm") #3=
	    (:type xpm :file "close.xpm")))
      (#10=(< (display-color-cells) 256) '(#2# #3# #4# #5#))
      (t '(#3# #4# #5#)))
     . #15=(t))
    )
  ;; 4. 使用 Emacs 内置图标（避免 XPM 问题）
  ;; (define-key tool-bar-map [my-test]
  ;; '(menu-item "Test" my-test-func
  ;; :image (lambda () (tool-bar--image 'new-file))))
  (define-key tool-bar-map [my-hello]
	      `(menu-item "hello" my-test-func
			  :help "say hello"
			  :image ,my-hello-icon
			  )
	      )
  (define-key tool-bar-map [my-hello1]
    `(menu-item "he1" my-test-func
		:help "say hello"
		:image ,my-hello-icon
		)
    )
  (define-key tool-bar-map [my-hello2]
    `(menu-item "he2" my-test-func
		:help "say hello"
		:image ,my-hello-icon
		)
    )
  (define-key tool-bar-map [my-hello3]
	      `(menu-item "he3" my-test-func
			  :help "say hello"
			  :image ,my-hello-icon
			  )
	      )

  (define-key tool-bar-map [my-hello4]
	      `(menu-item "he4" my-test-func
			  :help "say hello"
			  :image ,my-hello-icon
			  )
	      )

  (define-key tool-bar-map [my-hello5]
	      `(menu-item "he5" my-test-func
			  :help "say hello"
			  :image ,my-hello-icon
			  )
	      )

  (define-key tool-bar-map [my-hello6]
	      `(menu-item "he6" my-test-func
			  :help "say hello"
			  :image ,my-hello-icon
			  )
	      )

  (define-key tool-bar-map [my-hello7]
	      `(menu-item "he7" my-test-func
			  :help "say hello"
			  :image ,my-hello-icon
			  )
	      )

  (define-key tool-bar-map [my-hello8]
	      `(menu-item "he8" my-test-func
			  :help "say hello"
			  :image ,my-hello-icon
			  )
	      )

  (define-key tool-bar-map [my-hello9]
	      `(menu-item "he9" my-test-func
			  :help "say hello"
			  :image ,my-hello-icon
			  )
	      )

  (define-key tool-bar-map [my-hello0]
	      `(menu-item "he0" my-test-func
			  :help "say hello"
			  :image ,my-hello-icon
			  )
	      )

  (define-key tool-bar-map [my-h1]
	      `(menu-item "h1" my-test-func
			  :help "say hello"
			  :image ,my-hello-icon
			  )
	      )

  (define-key tool-bar-map [my-h2]
	      `(menu-item "h2" my-test-func
			  :help "say hello"
			  :image ,my-hello-icon
			  )
	      )

  (redraw-display)
  )


  	      )
(tool-bar-mode 1)
(tool-bar-mode -1)


(progn
(setq tool-bar-map (make-sparse-keymap))
(define-key tool-bar-map [my-h4]
	    `(menu-item "h4" my-test-func
		        :help "say hello"
			;; :image ,my-hello-icon
			:image nil
			)
	    )
)
(setq tool-bar-style 'both-horiz)
(setq tool-bar-style 'text)
(setq tool-bar-style 'text-image-horiz)



