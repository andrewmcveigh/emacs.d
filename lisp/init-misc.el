(require-packages 'dockerfile-mode 'evil-magit 'evil-paredit 'f 'helm
                  'helm-projectile 'inf-ruby 'magit 'markdown-mode 'nix-mode
                  'paredit 'projectile 'restclient 'rust-mode 's 'session
                  'use-package)
(require 'dockerfile-mode)
(require 'evil-magit)
(require 'evil-paredit)
(require 'helm)
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
(require 'use-package)

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

(provide 'init-misc)
