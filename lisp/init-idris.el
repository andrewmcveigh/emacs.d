(require-package 'idris-mode)
(require 'init-haskell)
(require 'idris-mode)

;; (setq idris-interpreter-path "/home/andrewmcveigh/bin/nix-shell-idris")
(setq idris-stay-in-current-window-on-compiler-error t)

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

(defun idris-highlight-column (idris-col)
  "Compute the Emacs position offset of the Idris column IDRIS-COL, for highlighting.

In particular, this takes bird tracks into account in literate Idris."
  (+ idris-col (if (idris-lidr-p) -1 -1)))

(provide 'init-idris)
