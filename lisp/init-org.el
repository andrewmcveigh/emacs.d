(require 'ox-reveal)
(require 'speaking-time)

(defun custom-evil-org-mode-hook ()
  (define-key evil-normal-state-map (kbd "<f8>") 'org-reveal-export-to-html)
  (define-key evil-normal-state-map (kbd "<f9>") 'org-set-speaking-time)
  (define-key evil-normal-state-map (kbd "<") 'org-promote-subtree)
  (define-key evil-normal-state-map (kbd ">") 'org-demote-subtree)
  (evil-leader/set-key "ll" 'org-latex-export-to-pdf)
  (evil-leader/set-key "lr" 'org-reveal-export-to-html)
  (evil-leader/set-key "ost" 'org-set-speaking-time)
  )

(add-hook 'org-mode-hook 'custom-evil-org-mode-hook)

(setq org-reveal-title-slide "<h1 class=\"title\">%t</h1>")

(provide 'init-org)
