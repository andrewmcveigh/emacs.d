(require-package 'company)
(add-hook 'after-init-hook
          (progn
            (require 'company)
            (global-company-mode)))

;; Use auto-complete as completion at point
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook
          'set-auto-complete-as-completion-at-point-function)

(provide 'init-autocomplete)
