(require-package 'racket-mode)

(require 'racket-mode)

(add-hook 'racket-mode-hook 'paredit-mode)
;; (add-hook 'racket-mode-hook 'evil-paredit-mode)
