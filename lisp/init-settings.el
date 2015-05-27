(setq ring-bell-function #'ignore)
(add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")
(golden-ratio-mode 1)
(powerline-default-theme)
(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'clojure-mode-hook (lambda () (yas/minor-mode 1)))

(defun cd-energy (dir)
  (interactive (list
                (progn (cd "~/Projects/uswitch/energy")
                       (read-file-name "Open energy dir:"))) )
  (message "prefix %S" dir)
  (cd dir)
  (neo-global--open-dir dir))

(defalias 'ecd 'cd-energy)
(defalias 'ncd 'neotree-dir)
(add-to-list 'exec-path "$HOME/bin")
(setq ns-use-srgb-colorspace t)
(setq julia-basic-repl-path "$HOME/bin/julia")
(tool-bar-mode -1)
(set-frame-position (selected-frame) 0 0)
(add-hook 'after-init-hook 'toggle-frame-maximized)

;;; display line numbers in margin, col nums at bottom.
(global-linum-mode 1)
(column-number-mode 1)


;;; Set font
(set-face-attribute 'default nil :font "Monaco")

;;; Close with CMD-w
(global-set-key (kbd "s-w") 'delete-window)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-bar-mode nil)
(scroll-bar-mode -1)

(setq require-final-newline nil)
;; (setq mode-require-final-newline t)
(global-whitespace-mode +1)
(setq whitespace-line-column 80)
(setq whitespace-style
      '(face lines-tail spaces tabs newline space-mark tab-mark newline-mark))
(setq whitespace-empty t)

(setq ac-ignore-case nil)
(setq-default indent-tabs-mode nil)

(add-to-list 'auto-mode-alist '("\\.vm\\'" . html-mode))
(add-hook 'html-mode-hook (lambda () (vtl-mode 1)))

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

(provide 'init-settings)
