(require-package 'magit-todos)
(require 'magit-todos)

(setq magit-todos-section-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "jT" #'magit-todos-jump-to-todos)
    ;; (define-key map "jl" #'magit-todos-list)
    map))

(add-hook 'magit-mode-hook 'magit-todos-mode)

(provide 'init-magit)
