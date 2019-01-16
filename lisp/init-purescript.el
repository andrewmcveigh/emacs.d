(require-packages 'purescript-mode 'psc-ide)

(require 'purescript-mode)
(require 'psc-ide)

(setq psc-ide-use-npm-bin t)

(add-hook 'purescript-mode-hook
  (lambda ()
    (psc-ide-mode)
    (company-mode)
    (flycheck-mode)
    (turn-on-purescript-indentation)))

(evil-leader/set-key-for-mode 'purescript-mode
  ;; "ee" 'dante-eval-block
  "jp" 'flycheck-previous-error
  "je" 'flycheck-next-error)

(provide 'init-purescript)
