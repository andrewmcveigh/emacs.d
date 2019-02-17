(require-packages 'rust-mode 'flycheck-rust 'flycheck-inline)
(require 'rust-mode)
(require 'flycheck-rust)
(require 'init-flycheck)

(add-hook 'rust-mode-hook 'flycheck-mode)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

(evil-leader/set-key-for-mode 'rust-mode
  "jp" 'flycheck-previous-error
  "je" 'flycheck-next-error)

(provide 'init-rust)
