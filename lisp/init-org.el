(org-babel-do-load-languages
 'org-babel-load-languages
'((clojure . t)))

(setq org-babel-clojure-backend 'cider)
(setq org-src-fontify-natively t)

;; Show syntax highlighting per language native mode in *.org
(setq org-src-fontify-natively t)
;; For languages with significant whitespace like Python:
(setq org-src-preserve-indentation t)

(provide 'init-org)
