(require-package 'idris-mode)
(require 'init-haskell)
(require 'idris-mode)

(setq idris-interpreter-path "/home/andrewmcveigh/bin/nix-shell-idris")

(evil-leader/set-key-for-mode 'idris-mode
  "ir" 'idris-load-file
  "it" 'idris-type-at-point
  "id" 'idris-add-clause
  "il" 'idris-make-lemma
  "ic" 'idris-case-split
  "if" 'idris-make-cases-from-hole
  "iw" 'idris-make-with-block
  "im" 'idris-add-missing
  "ip" 'idris-proof-search
  "ih" 'idris-docs-at-point)

(add-hook 'idris-mode-hook 'flycheck-mode)

(provide 'init-idris)
