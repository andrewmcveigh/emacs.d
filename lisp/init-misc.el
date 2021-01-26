(require-packages 'ag 'dockerfile-mode 'evil-magit 'evil-paredit 'f 'helm
                  'helm-ag 'helm-projectile 'inf-ruby 'magit 'markdown-mode
                  'nix-mode 'paredit 'projectile 'restclient 'rust-mode 's
                  'session 'sparql-mode 'ttl-mode 'use-package)
(require 'ag)
(require 'dockerfile-mode)
(require 'evil-magit)
(require 'evil-paredit)
(require 'helm)
(require 'helm-ag)
(require 'helm-projectile)
(require 'inf-ruby)
(require 'magit)
(require 'markdown-mode)
(require 'nix-mode)
(require 'paredit)
(require 'projectile)
(require 'restclient)
(require 'rust-mode)
(require 'session)
(require 'sparql-mode)
(require 'ttl-mode)
(require 'use-package)

(setq ag-arguments '("--follow" "--smart-case" "--stats"))

;;; paredit customisations
(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(add-to-list 'auto-mode-alist '("\\.sparql\\'" . sparql-mode))

(provide 'init-misc)
