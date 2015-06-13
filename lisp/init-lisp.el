(require 'paredit)
(require 'evil-paredit)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)

(provide 'init-lisp)
