(require-packages 'htmlize)
(require-packages 'gnuplot)
;; 'ox-reveal
;; (require 'org-mode)
(require 'ox-reveal)
(require 'htmlize)
(require 'evil)
(require 'evil-leader)
(require 'gnuplot)

(defun custom-evil-org-mode-hook ()
  (define-key evil-normal-state-map (kbd "<f8>") 'org-reveal-export-to-html)
  (define-key evil-normal-state-map (kbd "<f9>") 'org-set-speaking-time)
  (define-key evil-normal-state-map (kbd "<") 'org-promote-subtree)
  (define-key evil-normal-state-map (kbd ">") 'org-demote-subtree)
  (define-key evil-insert-state-map (kbd "C-y") (lambda () (interactive) (insert "λ")))
  (define-key evil-insert-state-map (kbd "C-S-Y") (lambda () (interactive) (insert "Λ")))
  (evil-leader/set-key-for-mode 'org-mode "ll" 'org-latex-export-to-pdf)
  (evil-leader/set-key-for-mode 'org-mode "lr" 'org-reveal-export-to-html)
  (evil-leader/set-key-for-mode 'org-mode "ost" 'org-set-speaking-time))

(add-hook 'org-mode-hook 'custom-evil-org-mode-hook)

(setq org-reveal-root "/home/andrewmcveigh/code/reveal.js")
(setq org-reveal-title-slide "<h1 class=\"title\">%t</h1>")
(setq org-src-preserve-indentation t)
(setq org-src-tab-acts-natively t)

(defun set-keys (mode)
  (evil-leader/set-key-for-mode mode
    "ts" 'subscript
    "tg" 'greek
    "tm" 'mathsym

    ))

(set-keys 'org-mode)

(defconst org-speaking-time-options-alist
  '((:wpm "WPM" nil nil )))

(defconst org-export-options-alist
  (org-combine-plists org-export-options-alist
                      org-speaking-time-options-alist))

(defun wpm ()
  (let ((wpm (plist-get (org-export--get-inbuffer-options) :wpm)))
    (if wpm
        (float (string-to-number wpm))
      120.0)))

(defun speaking-time (start end)
  (interactive "r")
  (save-excursion
    (let ((n 0))
      (goto-char start)
      (while (< (point) end)
        (if (forward-word 1)
            (setq n (1+ n))))
      (message "Speaking time is %s for %d words"
               (format-seconds "%M, %S"
                               (* 60 (/ (float n) 120.0)))
               n)
      n)))

(defun org-speaking-time (start end)
  (save-excursion
    (let ((n 0))
      (goto-char start)
      (while (< (point) end)
        (if (forward-word 1)
            (setq n (1+ n))))
      (format-seconds "%m:%02s" (* 60 (/ (float n) (wpm)))))))

(defun find-next-paragraph ()
  (let ((element (org-element-at-point)))
    (if (eq 'paragraph (org-element-type element))
        element
      (let ((next-elem-start (+ 1 (org-element-property :end element))))
        (goto-char next-elem-start)
        (find-next-paragraph)))))

(defun paragraphs-end ()
  (let ((element (org-element-at-point)))
    (if (eq 'paragraph (org-element-type element))
        (let ((next-elem-start (+ 1 (org-element-property :end element))))
          (if (>= next-elem-start (point-max))
              (point-max)
            (progn
              (goto-char next-elem-start)
              (paragraphs-end))))
      (- (org-element-property :begin element) 1))))

(defun org-set-speaking-time (x)
  (interactive "P")
  (org-set-effort
   (save-excursion
     (let* ((para (org-element-context))
            (back (org-back-to-heading t))
            (heading-start (if (eq t back)
                               (point)
                             back))
            (heading (org-element-at-point)))
       (goto-char (org-element-property :contents-begin heading))
       (let* ((para-start (org-element-property :begin (find-next-paragraph)))
              (para-end (paragraphs-end))
              (time (org-speaking-time para-start para-end)))
         time)))))

;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))

(setq org-plantuml-jar-path (substitute-in-file-name "$PLANTUML_JAR/lib/plantuml.jar"))

(provide 'init-org)
