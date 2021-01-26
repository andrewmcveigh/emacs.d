(require 'dreame-mode)

(add-hook 'dreame-mode-hook 'paredit-mode)
(add-hook 'dreame-mode-hook 'evil-paredit-mode)
(add-hook 'dreame-mode-hook 'paren-face-mode)
(add-hook 'dreame-mode-hook 'hs-minor-mode)
(add-hook 'dreame-mode-hook 'math-prettify-symbols)

(provide 'init-dreame)
