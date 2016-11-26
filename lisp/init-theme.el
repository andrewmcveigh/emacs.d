(require 'monokai-theme)

(setq monokai-background "#111111")

;; (load-theme 'material t)
;; (load-theme 'moe-dark)
(load-theme 'monokai)

(setq paren-face-regexp "[()]")

;; (set-face-foreground 'parenthesis "#555555")
(set-face-foreground 'parenthesis "#666666")
;; (set-face-foreground 'parenthesis "#00AAFF")

(custom-theme-set-faces
 'monokai
 '(org-block-begin-line
   ((t (:underline "#333333" :foreground "#AAAAAA" :background "#333333"))))
 '(org-block ((t (:background "#111111"))))
 '(org-block-end-line
   ((t (:overline "#333333" :foreground "#AAAAAA" :background "#333333")))))
;; (setq org-src-fontify-natively t)

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

(provide 'init-theme)
