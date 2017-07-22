(require-packages 'fill-column-indicator 'monokai-theme 'nlinum 'paren-face)
(require 'fill-column-indicator)
(require 'monokai-theme)
(require 'nlinum)
(require 'paren-face)

(setq monokai-background "#111111")

(load-theme 'monokai)

(setq paren-face-regexp "[()]")

(set-face-foreground 'parenthesis "#666666")

(custom-theme-set-faces
 'monokai
 '(org-block-begin-line
   ((t (:underline "#333333" :foreground "#AAAAAA" :background "#333333"))))
 '(org-block ((t (:background "#111111"))))
 '(org-block-end-line
   ((t (:overline "#333333" :foreground "#AAAAAA" :background "#333333")))))

(defun on-off-fci-before-company(command)
  (when (string= "show" command)
    (turn-off-fci-mode))
  (when (string= "hide" command)
    (turn-on-fci-mode)))

(advice-add 'company-call-frontends :before #'on-off-fci-before-company)

(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(setq fci-rule-width 1)
(setq fci-rule-column 80)
(setq fci-rule-color "#666666")
(global-fci-mode 1)

(defface todo-comment '((t (:foreground "#ffa198"))) "Pink")
(defface special-comment '((t (:foreground "#2aa198"))) "Cyan")
(font-lock-add-keywords 'clojure-mode '(("; \\(TODO:\\)" 1 'todo-comment t)))
(font-lock-add-keywords 'clojure-mode '((";: \\(.*\\)" 1 'special-comment t)))

(provide 'init-theme)
