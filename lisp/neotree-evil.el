(require 'neotree)
(require 'evil)

(define-minor-mode neotree-evil
  "Use NERDTree bindings on neotree."
  :lighter " NT"
  :keymap (progn
            (evil-make-overriding-map neotree-mode-map 'normal t)
            (evil-define-key 'normal neotree-mode-map
              "r" 'neotree-refresh
              "o" (lambda ()
                    (interactive)
                    (neotree-enter))
              (kbd "<return>") 'neotree-enter
              "s" (lambda ()
                    (interactive)
                    (setq w (next-window))
                    (split-window w nil t)
                    (neotree-enter))
              "i" (lambda ()
                    (interactive)
                    (setq w (next-window))
                    (split-window w nil)
                    (neotree-enter))
              "ma" (lambda ()
                     (interactive)
                     (let* ((file (read-from-minibuffer "Create (directories end with /): "))
                            (dir (pe/get-filename))
                            (path (mapconcat 'identity `(,dir ,file) "")))
                       (if (string/ends-with dir "/")
                           (if (string/ends-with file "/")
                               (make-directory path)
                             (write-region "" nil path))
                         (message "Can't create directory in a file"))))
              "mc" 'pe/copy-file
              "md" 'pe/delete-file
              "mm" 'pe/rename-file
              "gi" (lambda ()
                     (interactive)
                     (if (string= pe/get-directory-tree-external-command
                                  nt/gitignore-files-cmd)
                         (progn (setq pe/get-directory-tree-external-command
                                      nt/all-files-cmd))
                       (progn (setq pe/get-directory-tree-external-command
                                    nt/gitignore-files-cmd)))
                     (nt/refresh))
              "I" (lambda ()
                    (interactive)
                    (if pe/omit-enabled
                        (progn (setq pe/directory-tree-function
                                     'pe/get-directory-tree-async)
                               (pe/toggle-omit nil))
                      (progn (setq pe/directory-tree-function
                                   'pe/get-directory-tree-external)
                             (pe/toggle-omit t)))))
            neotree-mode-map))

(setq neo-hidden-files-regexp "^\\.")

(provide 'neotree-evil)
