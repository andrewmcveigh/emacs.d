(require-packages 'haskell-mode 'dante)
(require 'haskell-mode)
(require 'dante)

;; (setq dante-repl-command-line '("nix-shell" "--run" "'cabal repl'"))

(setq
 ;; ghc-ghc-options '("-fno-warn-missing-signatures" "-fwarn-incomplete-patterns")
 ;; haskell-compile-cabal-build-command "nix-shell --run=\"cabal install --builddir=dist/dante\""
 ;; haskell-process-type 'ghci
 haskell-process-type 'cabal-repl
 haskell-interactive-popup-errors nil
 haskell-process-args-cabal-repl '("--ghc-options=-fshow-loaded-modules")
 ;; haskell-process-args-stack-ghci '("--no-load")
 ;; haskell-process-path-ghci "stack")
 ;; haskell-process-path-cabal "cabal"
 ;; haskell-process-path-cabal '("nix-shell" "--run" "\"cabal repl --builddir=dist/dante\"")
 ;; haskell-process-path-ghci '("ghci")
 haskell-process-wrapper-function
 (lambda (argv) (append (list "nix-shell" "-I" "." "--command")
                   (list (mapconcat 'identity argv " "))))
 )

;; "--ghc-options=-ferror-spans" "--with-ghc=ghci-ng"

;; (defun set-exec-path-from-shell-PATH ()
;;   "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell. "
;;   (interactive)
;;   (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))

;; (set-exec-path-from-shell-PATH)

;; (setenv "INTERACTIVE" "-i")


(evil-leader/set-key-for-mode 'haskell-mode
  "ee" 'dante-eval-block
  "je" 'flycheck-next-error)

(evil-define-key 'normal haskell-mode-map (kbd "gf") 'xref-find-definitions)

;; (evil-define-key 'normal clojure-mode-map (kbd "K")  'doc-for-var)
;; (define-key evil-motion-state-map "gd" 'evil-goto-definition)

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :bind (("C-c C-c" . dante-eval-block))
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

(provide 'init-haskell)
