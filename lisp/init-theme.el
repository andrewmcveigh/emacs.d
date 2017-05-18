(require 'monokai-theme)

(setq monokai-background "#111111")

(load-theme 'monokai)

(let* ((font-name "Fira Code Light")
       (font-size "13")
       (font-str (concat font-name "-" font-size)))
  (set-default-font font-name)
  (set-face-attribute 'default nil
                      :font font-str
                      :inherit 'fixed-pitch
                      :weight 'normal)
  (set-face-attribute 'font-lock-keyword-face nil
                      :font font-str
                      :inherit 'fixed-pitch
                      :weight 'normal))

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
                 ;; (59 . ".\\(?:;\\)")
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

;; (setq monokai-background "#111111")
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

(defface special-comment '((t (:foreground "#2aa198"))) "Cyan")
(font-lock-add-keywords 'clojure-mode '((";: \\(.*\\)" 1 'special-comment t)))

(provide 'init-theme)
