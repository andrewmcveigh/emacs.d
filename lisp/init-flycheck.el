(require 'init-evil)

(evil-leader/set-key-for-mode 'flycheck-mode
  "jp" 'flycheck-previous-error
  "je" 'flycheck-next-error)

(evil-leader/set-key-for-mode 'flyspell-mode
  "je" 'flyspell-goto-next-error)

(evil-leader/set-key-for-mode 'text-mode
  "je" 'flyspell-goto-next-error)

(evil-leader/set-key-for-mode 'org-mode
  "je" 'flyspell-goto-next-error)

(provide 'init-flycheck)
