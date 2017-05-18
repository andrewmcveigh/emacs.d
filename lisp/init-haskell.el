
(setq
 ;; ghc-ghc-options '("-fno-warn-missing-signatures")
 haskell-compile-cabal-build-command "cd %s && stack build"
 haskell-process-type 'stack-ghci
 haskell-interactive-popup-errors nil
 ;; haskell-process-args-stack-ghci '("--ghc-options=-ferror-spans" "--with-ghc=ghci-ng")
 haskell-process-path-ghci "stack")

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell. "
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)
(setenv "INTERACTIVE" "-i")

(provide 'init-haskell)
