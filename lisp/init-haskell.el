(require-packages 'haskell-mode 'intero)

;; 'dante 'hlint-refactor 'flycheck-haskell
(require 'haskell-mode)
(require 'intero)
;; (require 'dante)
;; (require 'hlint-refactor)

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

;; (evil-define-key 'normal haskell-mode-map (kbd "gf") 'xref-find-definitions)
(evil-define-key 'normal haskell-mode-map (kbd "gf") 'intero-goto-definition)

;; (evil-define-key 'normal clojure-mode-map (kbd "K")  'doc-for-var)
;; (define-key evil-motion-state-map "gd" 'evil-goto-definition)


(defun flycheck-haskell-runghc-command (args)
  "Customised cos nix-shell"
  (list "bash" "-c"
        (format "cd %s && nix-shell --run 'runghc -i %s'"
                (projectile-project-root)
                (string-join args " "))))

(defun hlint-hook ()
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

(with-eval-after-load 'intero
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint))
)
(use-package intero
  :ensure t
  :after haskell-mode
  :commands 'intero-mode
  ;; :bind (("C-c C-c" . dante-eval-block))
  :init
  (add-hook 'haskell-mode-hook 'intero-mode)
  ;; (add-hook 'haskell-mode-hook 'flycheck-mode)
  ;; (add-hook 'haskell-mode-hook 'flycheck-haskell-setup)
  ;; (add-hook 'haskell-mode-hook 'hlint-refactor-mode)
  ;; (add-hook 'dante-mode-hook 'hlint-hook)
  ;; (add-hook 'intero-mode-hook 'hlint-hook)
  )

(provide 'init-haskell)
