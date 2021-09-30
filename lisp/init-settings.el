(require-packages 'nlinum 'projectile 'undo-tree)
(require 'undo-tree)

(setq ring-bell-function #'ignore)

;; (add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")
;; (golden-ratio-mode 1)
(global-undo-tree-mode)
(evil-set-undo-system 'undo-tree)

(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("[A-Z]\\.md\\'" . gfm-mode))

(menu-bar-mode -99)

(defalias 'yes-or-no-p 'y-or-n-p)

;; (defalias 'ncd 'neotree-dir)

(add-to-list 'exec-path "$HOME/bin")
(setq ns-use-srgb-colorspace t)
(tool-bar-mode -1)

(column-number-mode 1)
(global-nlinum-mode 1)

(setq projectile-generic-command "find -L . -type f -print0")
(setq projectile-project-root-files-top-down-recurring '(".git"))
(setq projectile-project-root-files-functions '(projectile-root-top-down-recurring))
(setq helm-ag-base-command "ag --nocolor --nogroup --follow")
(setq projectile-indexing-method 'alien)
(setq projectile-git-command "~/bin/sub-ls")

(setq projectile-enable-caching nil)

(projectile-global-mode)

;;; Set font

;; (when (window-system) (smooth-scrolling-mode t))

;;; Close with CMD-w
;; (global-set-key (kbd "s-w") 'delete-window)

;; scroll one line at a time (less "jumpy" than defaults)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-bar-mode nil)
(scroll-bar-mode -1)

(setq require-final-newline nil)
;; (setq mode-require-final-newline t)
(global-whitespace-mode +1)
;; (setq whitespace-line-column 80)
(setq whitespace-line-column 1000)

(setq whitespace-style
      '(face lines-tail spaces tabs newline space-mark tab-mark newline-mark))
(setq whitespace-empty t)

(defun delete-whitespace ()
  (message (buffer-name))
  (if (not (string-prefix-p "slides.org" (buffer-name)))
      (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'delete-whitespace)

(setq ac-ignore-case nil)
(setq-default indent-tabs-mode nil)

(add-to-list 'auto-mode-alist '("\\.vm\\'" . html-mode))
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; (add-hook 'html-mode-hook (lambda () (vtl-mode 1)))

(setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch %f"))

(setq org-time-clocksum-use-effort-durations t)

(defun indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (and (looking-at "\\_>"))
        (company-complete-common)
      (indent-according-to-mode))))

(global-set-key [tab] 'indent-or-complete)
(setq company-idle-delay 0.2)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  )

;; (setq x-select-enable-clipboard nil)

(setq js-indent-level 2)


(setq org-reveal-root "file:///home/andrewmcveigh/code/reveal.js")

(defun fci-mode-override-advice (&rest args))

(advice-add 'org-html-fontify-code :around
            (lambda (fun &rest args)
              (advice-add 'fci-mode :override #'fci-mode-override-advice)
              (let ((result  (apply fun args)))
                (advice-remove 'fci-mode #'fci-mode-override-advice)
                result)))

(add-hook 'text-mode-hook 'flyspell-mode)

(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))

(setq tab-width 4)
(setq default-tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))


(if (eq system-type 'darwin) (setq ns-use-native-fullscreen nil))

(setq-default fill-column 80)

(setq projectile-enable-caching t)
;; (setq shell-file-name "/bin/sh")

; Map escape to cancel (like C-g)...
(global-set-key (kbd "ESC") 'keyboard-quit) ;; all platforms?
(define-key isearch-mode-map (kbd "ESC") 'isearch-abort)   ;; isearch
(define-key isearch-mode-map (kbd "ESC") 'isearch-abort)   ;; \e seems to work better for terminals
(define-key minibuffer-local-map (kbd "ESC") 'keyboard-escape-quit)
(define-key query-replace-map (kbd "ESC") 'keyboard-quit)

(add-hook 'dired-mode-hook 'auto-revert-mode)

(defun replace-digits-by-subscript (string)
  (replace-regexp-in-string "[0-9]"
    (lambda (v) (format "%c" (+ (string-to-number v) 8320))) string))
(replace-digits-by-subscript "123456789")

(defun char-to-subscript (c)
  (cond ((and (>= c 97) (<= c 122))
         (format "%c" (aref "ₐ   ₑ  ₕᵢⱼₖₗₘₙₒₚ ᵣₛₜᵤᵥ ₓ  " (- c 97))))
        ((and (>= c 48) (<= c 57))
         (format "%c" (aref "₀₁₂₃₄₅₆₇₈₉" (- c 48))))))

(defun subscript ()
  (interactive)
  (let ((replacement (char-to-subscript (char-after (point)))))
    (delete-char 1)
    (insert replacement)))

(defconst greek-letter-alist
  '((Alpha . "Α")
    (Beta . "Β")
    (Gamma . "Γ")
    (Delta . "Δ")
    (Epsilon . "Ε")
    (Zeta . "Ζ")
    (Eta . "Η")
    (Theta . "Θ")
    (Iota . "Ι")
    (Kappa . "Κ")
    (Lambda . "Λ")
    (Mu . "Μ")
    (Nu . "Ν")
    (Xi . "Ξ")
    (Omicron . "Ο")
    (Pi . "Π")
    (Rho . "Ρ")
    (Sigma . "Σ")
    (Tau . "Τ")
    (Upsilon . "Υ")
    (Phi . "Φ")
    (Chi . "Χ")
    (Psi . "Ψ")
    (Omega . "Ω")
    (alpha . "α")
    (beta . "β")
    (gamma . "γ")
    (delta . "δ")
    (epsilon . "ε")
    (zeta . "ζ")
    (eta . "η")
    (theta . "θ")
    (iota . "ι")
    (kappa . "κ")
    (lambda . "λ")
    (mu . "μ")
    (nu . "ν")
    (xi . "ξ")
    (omicron . "ο")
    (pi . "π")
    (rho . "ρ")
    (sigma . "σ")
    (tau . "τ")
    (upsilon . "υ")
    (phi . "φ")
    (chi . "χ")
    (psi . "ψ")
    (omega . "ω")
    (forall . "∀")
    (arr . "\u2192")
    ))

(defun replace-word-from-alist (alist)
  (interactive)
  (let* ((b (bounds-of-thing-at-point 'word))
         (k (thing-at-point 'word))
         (l (alist-get (intern-soft k) alist)))
    (kill-region (car b) (cdr b))
    (insert l)))

(defun replace-symbol-from-alist (alist)
  (interactive)
  (let* ((b (bounds-of-thing-at-point 'symbol))
         (k (thing-at-point 'symbol))
         (l (alist-get (intern-soft k) alist)))
    (kill-region (car b) (cdr b))
    (insert l)))

(defun greek ()
  (interactive)
  (replace-word-from-alist greek-letter-alist))

(defconst mathsym-alist
  '((<= . "≤")
    (>= . "≥")
    (union . "∪")
    (prime . "′")))

(defun mathsym ()
  (interactive)
  (replace-symbol-from-alist mathsym-alist))

;; Configure desktop saving
(setq desktop-restore-eager 5)
(setq desktop-save (quote if-exists))
(desktop-save-mode 1)

(setq ruby-insert-encoding-magic-comment nil)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq css-indent-offset 2)

(setq auto-revert-verbose nil)

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 4
                                  tab-width 4
                                  indent-tabs-mode nil)))
(add-hook 'makefile-mode (lambda ()
                            (setq c-basic-offset 4
                                  tab-width 4
                                  indent-tabs-mode t)))
(setq-default indent-tabs-mode nil)
(setq-default kotlin-tab-width 2)

(set-frame-parameter (selected-frame) 'alpha '(90 . 50))
(add-to-list 'default-frame-alist '(alpha . (90 . 50)))

(provide 'init-settings)
