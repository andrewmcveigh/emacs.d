(tool-bar-mode -1)
(set-frame-position (selected-frame) 0 0)
;;; Set fullscreen 27"
(setq default-frame-alist '((width . 362) (height . 92)))

;;; display line numbers in margin, col nums at bottom.
(global-linum-mode 1)
(column-number-mode 1)

;; (setq require-final-newline nil)
(setq mode-require-final-newline t)
;; (global-whitespace-mode +1)

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

;; (setq whitespace-line-column 80)
;; (setq whitespace-style
;;       '(face lines spaces tabs newline space-mark tab-mark newline-mark))

;; (setq whitespace-empty t)
(setq ac-ignore-case nil)
(setq-default indent-tabs-mode nil)

(provide 'init-settings)
