(setq ring-bell-function #'ignore)

(add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")
(golden-ratio-mode 1)

(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("[A-Z]\\.md\\'" . gfm-mode))

(menu-bar-mode -99)

(defalias 'yes-or-no-p 'y-or-n-p)

(defalias 'ncd 'neotree-dir)

(add-to-list 'exec-path "$HOME/bin")
(setq ns-use-srgb-colorspace t)
(tool-bar-mode -1)

;;; display line numbers in margin, col nums at bottom.
(global-linum-mode 1)
(column-number-mode 1)

(projectile-global-mode)

;;; Set font

(when (window-system)
  (let* ((font-name "Fira Code Light")
         (font-size "12")
         (font-str (concat font-name "-" font-size)))
    (set-default-font font-name)
    ;; (add-to-list 'default-frame-alist '(font . ,font-str))
    (set-face-attribute 'default nil :font font-str)
    ;; (set-frame-position (selected-frame) 0 0)
                                        ;(add-hook 'after-init-hook 'toggle-frame-maximized)
    ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
    ;; (set-frame-size (selected-frame) 91 63)
    )
  (smooth-scrolling-mode t)
  )

(defun enable-fira-code-ligatures ()
  (let ((alist '(
                 (33 . ".\\(?:\\(?:==\\)\\|[!=]\\)")
                 (35 . ".\\(?:[(?[_{]\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*\\)\\|[*/]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|\\+\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 ;;               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (58 . ".\\(?:[:=]\\)")
                 (59 . ".\\(?:;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 ;; (63 . ".\\(?:[:=?]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:[=@~-]\\)")
                 )))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))

(enable-fira-code-ligatures)

;;; Close with CMD-w
;; (global-set-key (kbd "s-w") 'delete-window)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-bar-mode nil)
(scroll-bar-mode -1)

(setq require-final-newline nil)
;; (setq mode-require-final-newline t)
(global-whitespace-mode +1)
;; (setq whitespace-line-column 80)
(setq whitespace-line-column 1000)

(setq whitespace-style
      '(face lines-tail spaces tabs newline space-mark tab-mark newline-mark))
(setq whitespace-empty t)

(setq ac-ignore-case nil)
(setq-default indent-tabs-mode nil)

(add-to-list 'auto-mode-alist '("\\.vm\\'" . html-mode))
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; (add-hook 'html-mode-hook (lambda () (vtl-mode 1)))

(setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch %f"))

(setq org-time-clocksum-use-effort-durations t)

(defun indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (and (looking-at "\\_>"))
        (company-complete-common)
      (indent-according-to-mode))))

(global-set-key [tab] 'indent-or-complete)
(setq company-idle-delay 0.2)

;; (setq x-select-enable-clipboard nil)

(setq js-indent-level 2)

(setq org-reveal-root "file:///home/andrewmcveigh/code/reveal.js")

(defun fci-mode-override-advice (&rest args))

(advice-add 'org-html-fontify-code :around
            (lambda (fun &rest args)
              (advice-add 'fci-mode :override #'fci-mode-override-advice)
              (let ((result  (apply fun args)))
                (advice-remove 'fci-mode #'fci-mode-override-advice)
                result)))

(add-hook 'text-mode-hook 'flyspell-mode)

(provide 'init-settings)
