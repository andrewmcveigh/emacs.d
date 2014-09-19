(require 'project-explorer)
(require 'evil)

;;; /usr/local/bin/gfind . \( ! -path '*/.*' \) \( -type d -printf "%p/\\n" , -type f -print \) | grep -v -f ./.gitignore | grep -v '.git/'
(defvar nt/gitignore-files-cmd
  "/usr/local/bin/gfind . \\( ! -path '*/.*' \\) \\( -type d -printf \"%p/\\n\" , -type f -print \\) | grep -v -f ./.gitignore | grep -v '.git/'")

(defvar nt/all-files-cmd
  "/usr/local/bin/gfind . \\( ! -path '*/.*' \\) \\( -type d -printf \"%p/\\n\" , -type f -print \\)")

;;; Set up to use external command for dir listing
(setq pe/directory-tree-function 'pe/get-directory-tree-external)

;;; use gfind & grep to only list the nice stuff
(setq pe/get-directory-tree-external-command nt/gitignore-files-cmd)

(defun string/ends-with (s ending)
  "return non-nil if string S ends with ENDING."
  (let ((elength (length ending)))
    (string= (substring s (- 0 elength)) ending)))

(defun nt/refresh ()
  (interactive)
  (funcall pe/directory-tree-function
           default-directory
           (apply-partially 'pe/set-tree
                            (current-buffer)
                            'refresh)))

(defun nt/balance-windows ()
  (interactive)
  (balance-windows-area)
  (let* ((project-explorer-buffers (pe/get-project-explorer-buffers))
         (window (cl-find-if
                  (lambda (window)
                    (and (memq (window-buffer window) project-explorer-buffers)
                         (window-parameter window 'window-side)))
                  (window-list))))
    (when window
      (es-set-window-body-width window pe/width))))

(define-key evil-window-map "=" 'nt/balance-windows)

(define-minor-mode nerdtree-project-explorer-mode
  "Use NERDTree bindings on project-explorer."
  :lighter " NT"
  :keymap (progn
            (evil-make-overriding-map project-explorer-mode-map 'normal t)
            (evil-define-key 'normal project-explorer-mode-map
              "r" 'nt/refresh
              "o" (lambda ()
                    (interactive)
                    (if (string/ends-with (pe/user-get-filename) "/")
                        (pe/tab)
                      (pe/return))) ;; 'pe/return
              (kbd "<return>") 'pe/return
              "s" (lambda ()
                    (interactive)
                    (setq w (next-window))
                    (split-window w nil t)
                    (pe/return))
              "i" (lambda ()
                    (interactive)
                    (setq w (next-window))
                    (split-window w nil)
                    (pe/return))
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
            project-explorer-mode-map))

(add-hook 'project-explorer-mode-hook 'nerdtree-project-explorer-mode)

(provide 'nerdtree-project-explorer)
