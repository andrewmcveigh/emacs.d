(require 'powerline)

(powerline-center-evil-theme)


(setq powerline-default-separator 'utf-8)

(setq powerline-color1 "#073642")
(setq powerline-color2 "#002b36")
(setq powerline-color3 "#002b36")

(set-face-attribute 'mode-line-inactive nil :box nil)

(setq x-use-underline-position-properties nil)
(setq underline-minimum-offset 4)
(setq x-underline-at-descent-line t)

;; (setq solarized-high-contrast-mode-line t)
;; (load-theme 'solarized-dark t)
(load-theme 'material t)

(set-face-attribute 'mode-line nil
                    :foreground "#333333"
                    :background "#2aa198"
                    :box nil)

(setq paren-face-regexp "[({})]")

(provide 'init-theme)
