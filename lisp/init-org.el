(require 'ox-reveal)
(require 'speaking-time)

(defun custom-evil-org-mode-hook ()
  (define-key evil-normal-state-map (kbd "<f8>") 'org-reveal-export-to-html)
  (define-key evil-normal-state-map (kbd "<f9>") 'speaking-time)
  (define-key evil-normal-state-map (kbd "<") 'org-promote-subtree)
  (define-key evil-normal-state-map (kbd ">") 'org-demote-subtree))

(add-hook 'org-mode-hook 'custom-evil-org-mode-hook)

(provide 'init-org)
