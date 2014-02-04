(require 'project-explorer)
(require 'evil)

(defun string/ends-with (s ending)
  "return non-nil if string S ends with ENDING."
  (let ((elength (length ending)))
    (string= (substring s (- 0 elength)) ending)))

(define-minor-mode nerdtree-project-explorer-mode
  "Use NERDTree bindings on project-explorer."
  :lighter " NT"
  :keymap (progn
            (evil-make-overriding-map project-explorer-mode-map 'normal t)
            (evil-define-key 'normal project-explorer-mode-map
              "r" (lambda ()
                    (interactive)
                    (let (project-root (funcall pe/project-root-function))
                      (pe/set-directory pe/project-root)))
              "o" 'pe/return
              "v" (lambda ()
                    (interactive)
                    (setq w (next-window))
                    (split-window w nil t)
                    (pe/return))
              "s" (lambda ()
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
                         (message "Can't create directory in a file")))))
            project-explorer-mode-map))

(add-hook 'project-explorer-mode-hook 'nerdtree-project-explorer-mode)

(provide 'nerdtree-project-explorer)
