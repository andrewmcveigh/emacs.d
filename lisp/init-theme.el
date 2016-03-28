;; (require 'powerline)

;; (powerline-center-evil-theme)

;; (setq powerline-default-separator 'utf-8)

;; (setq powerline-color1 "#073642")
;; (setq powerline-color2 "#002b36")
;; (setq powerline-color3 "#002b36")

(set-face-attribute 'mode-line-inactive nil :box nil)

(setq x-use-underline-position-properties nil)
(setq underline-minimum-offset 4)
(setq x-underline-at-descent-line t)

;; (setq solarized-high-contrast-mode-line t)
;; (load-theme 'solarized-dark t)

;; (deftheme idris-material-theme "Idris-mode colors compatible with Leuven")

(defvar color-keyword "#FD8A66")
(defvar color-string "#9DCB6B")
(defvar color-quoted "#8CC152")
(defvar color-default "#DCDCDC")
(defvar color-define "#FFF4A2")
(defvar color-name "#85F7EA")
(defvar color-comment "#8F9DA4")
(defvar color-docstring "#FDE2B7")

;; (custom-theme-set-faces
;;  'idris-material-theme
;;  `(idris-equals-face ((t (:foreground ,color-define :weight semi-bold))))
;;  `(idris-colon-face ((t (:foreground ,color-define :weight semi-bold))))
;;  `(idris-metavariable-face ((t (:underline (:color ,color-keyword :style wave) :slant italic))))
;;  `(idris-operator-face ((t (:foreground ,color-default :weight semi-bold))))
;;  `(idris-repl-output-face ((t (:foreground ,color-default))))
;;  `(idris-semantic-function-face ((t (:foreground ,color-name))))
;;  `(idris-semantic-implicit-face ((t (:slant italic))))
;;  `(idris-keyword-face ((t (:foreground ,color-keyword :weight bold :underline t)))))
;; 
;; (provide-theme 'idris-material-theme)
;; 
(load-theme 'material t)

;; (set-face-attribute 'mode-line nil
;;                     :foreground "#333333"
;;                     :background "#2aa198"
;;                     :box nil)

(setq paren-face-regexp "[()]")


(provide 'init-theme)
