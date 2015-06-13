(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; Use auto-complete as completion at point
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(provide 'init-autocomplete)
