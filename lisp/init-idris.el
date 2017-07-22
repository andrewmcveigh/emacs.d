(require-package 'idris-mode)
(require 'idris-mode)

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

(provide 'init-idris)
