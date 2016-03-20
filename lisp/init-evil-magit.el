(require 'evil-leader)

(evil-leader/set-key
  "gs" 'magit-status
  "gd" 'magit-diff
  )

(provide 'init-evil-magit)
