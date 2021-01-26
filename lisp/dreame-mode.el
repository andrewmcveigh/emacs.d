;;; dreame-mode.el --- Major mode for Dreame code -*- lexical-binding: t; -*-


(eval-when-compile
  (defvar calculate-lisp-indent-last-sexp)
  (defvar font-lock-beg)
  (defvar font-lock-end)
  (defvar paredit-space-for-delimiter-predicates)
  (defvar paredit-version)
  (defvar paredit-mode))

(require 'cl-lib)
(require 'imenu)
(require 'newcomment)
(require 'align)
(require 'subr-x)
(require 'lisp-mnt)
(require 'project)

(declare-function lisp-fill-paragraph  "lisp-mode" (&optional justify))

(defgroup dreame nil
  "Major mode for editing Dreame code."
  :prefix "dreame-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/dreame-emacs/dreame-mode")
  :link '(emacs-commentary-link :tag "Commentary" "dreame-mode"))

(defconst dreame-mode-version
  (eval-when-compile
    (lm-version (or load-file-name buffer-file-name)))
  "The current version of `dreame-mode'.")

(defface dreame-keyword-face
  '((t (:inherit font-lock-constant-face)))
  "Face used to font-lock Dreame keywords (:something)."
  :package-version '(dreame-mode . "3.0.0"))

(defface dreame-character-face
  '((t (:inherit font-lock-string-face)))
  "Face used to font-lock Dreame character literals."
  :package-version '(dreame-mode . "3.0.0"))

(defcustom dreame-indent-style 'align-arguments
  "Indentation style to use for function forms and macro forms.
There are two cases of interest configured by this variable.

- Case (A) is when at least one function argument is on the same
  line as the function name.
- Case (B) is the opposite (no arguments are on the same line as
  the function name).  Note that the body of macros is not
  affected by this variable, it is always indented by
  `lisp-body-indent' (default 2) spaces.

Note that this variable configures the indentation of function
forms (and function-like macros), it does not affect macros that
already use special indentation rules.

The possible values for this variable are keywords indicating how
to indent function forms.

    `always-align' - Follow the same rules as `lisp-mode'.  All
    args are vertically aligned with the first arg in case (A),
    and vertically aligned with the function name in case (B).
    For instance:
        (reduce merge
                some-coll)
        (reduce
         merge
         some-coll)

    `always-indent' - All args are indented like a macro body.
        (reduce merge
          some-coll)
        (reduce
          merge
          some-coll)

    `align-arguments' - Case (A) is indented like `lisp', and
    case (B) is indented like a macro body.
        (reduce merge
                some-coll)
        (reduce
          merge
          some-coll)"
  :safe #'symbolp
  :type '(choice (const :tag "Same as `lisp-mode'" 'always-align)
                 (const :tag "Indent like a macro body" 'always-indent)
                 (const :tag "Indent like a macro body unless first arg is on the same line"
                        'align-arguments))
  :package-version '(dreame-mode . "5.2.0"))

(defcustom dreame-use-backtracking-indent t
  "When non-nil, enable context sensitive indentation."
  :type 'boolean
  :safe 'booleanp)

(defcustom dreame-max-backtracking 3
  "Maximum amount to backtrack up a list to check for context."
  :type 'integer
  :safe 'integerp)

(defcustom dreame-docstring-fill-column fill-column
  "Value of `fill-column' to use when filling a docstring."
  :type 'integer
  :safe 'integerp)

(defcustom dreame-docstring-fill-prefix-width 2
  "Width of `fill-prefix' when filling a docstring.
The default value conforms with the de facto convention for
Dreame docstrings, aligning the second line with the opening
double quotes on the third column."
  :type 'integer
  :safe 'integerp)

(defcustom dreame-omit-space-between-tag-and-delimiters '(?\[ ?\{ ?\()
  "Allowed opening delimiter characters after a reader literal tag.
For example, \[ is allowed in :db/id[:db.part/user]."
  :type '(set (const :tag "[" ?\[)
              (const :tag "{" ?\{)
              (const :tag "(" ?\()
              (const :tag "\"" ?\"))
  :safe (lambda (value)
          (and (listp value)
               (cl-every 'characterp value))))

(defcustom dreame-build-tool-files
  '("project.clj"      ; Leiningen
    "build.boot"       ; Boot
    "build.gradle"     ; Gradle
    "build.gradle.kts" ; Gradle
    "deps.edn"         ; Dreame CLI (a.k.a. tools.deps)
    "shadow-cljs.edn"  ; shadow-cljs
    )
  "A list of files, which identify a Dreame project's root.
Out-of-the box `dreame-mode' understands lein, boot, gradle,
 shadow-cljs and tools.deps."
  :type '(repeat string)
  :package-version '(dreame-mode . "5.0.0")
  :safe (lambda (value)
          (and (listp value)
               (cl-every 'stringp value))))

(defcustom dreame-project-root-function #'dreame-project-root-path
  "Function to locate dreame project root directory."
  :type 'function
  :risky t
  :package-version '(dreame-mode . "5.7.0"))

(defcustom dreame-refactor-map-prefix (kbd "C-c C-r")
  "Dreame refactor keymap prefix."
  :type 'string
  :package-version '(dreame-mode . "5.6.0"))

(defvar dreame-refactor-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-t") #'dreame-thread)
    (define-key map (kbd "t") #'dreame-thread)
    (define-key map (kbd "C-u") #'dreame-unwind)
    (define-key map (kbd "u") #'dreame-unwind)
    (define-key map (kbd "C-f") #'dreame-thread-first-all)
    (define-key map (kbd "f") #'dreame-thread-first-all)
    (define-key map (kbd "C-l") #'dreame-thread-last-all)
    (define-key map (kbd "l") #'dreame-thread-last-all)
    (define-key map (kbd "C-p") #'dreame-cycle-privacy)
    (define-key map (kbd "p") #'dreame-cycle-privacy)
    (define-key map (kbd "C-(") #'dreame-convert-collection-to-list)
    (define-key map (kbd "(") #'dreame-convert-collection-to-list)
    (define-key map (kbd "C-'") #'dreame-convert-collection-to-quoted-list)
    (define-key map (kbd "'") #'dreame-convert-collection-to-quoted-list)
    (define-key map (kbd "C-{") #'dreame-convert-collection-to-map)
    (define-key map (kbd "{") #'dreame-convert-collection-to-map)
    (define-key map (kbd "C-[") #'dreame-convert-collection-to-vector)
    (define-key map (kbd "[") #'dreame-convert-collection-to-vector)
    (define-key map (kbd "C-#") #'dreame-convert-collection-to-set)
    (define-key map (kbd "#") #'dreame-convert-collection-to-set)
    (define-key map (kbd "C-i") #'dreame-cycle-if)
    (define-key map (kbd "i") #'dreame-cycle-if)
    (define-key map (kbd "C-w") #'dreame-cycle-when)
    (define-key map (kbd "w") #'dreame-cycle-when)
    (define-key map (kbd "C-o") #'dreame-cycle-not)
    (define-key map (kbd "o") #'dreame-cycle-not)
    (define-key map (kbd "n i") #'dreame-insert-ns-form)
    (define-key map (kbd "n h") #'dreame-insert-ns-form-at-point)
    (define-key map (kbd "n u") #'dreame-update-ns)
    (define-key map (kbd "n s") #'dreame-sort-ns)
    (define-key map (kbd "n r") #'dreame-rename-ns-alias)
    (define-key map (kbd "s i") #'dreame-introduce-let)
    (define-key map (kbd "s m") #'dreame-move-to-let)
    (define-key map (kbd "s f") #'dreame-let-forward-slurp-sexp)
    (define-key map (kbd "s b") #'dreame-let-backward-slurp-sexp)
    (define-key map (kbd "C-a") #'dreame-add-arity)
    (define-key map (kbd "a") #'dreame-add-arity)
    map)
  "Keymap for Dreame refactoring commands.")
(fset 'dreame-refactor-map dreame-refactor-map)

(defvar dreame-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    (define-key map (kbd "C-:") #'dreame-toggle-keyword-string)
    (define-key map (kbd "C-c SPC") #'dreame-align)
    (define-key map dreame-refactor-map-prefix 'dreame-refactor-map)
    (easy-menu-define dreame-mode-menu map "Dreame Mode Menu"
      '("Dreame"
        ["Toggle between string & keyword" dreame-toggle-keyword-string]
        ["Align expression" dreame-align]
        ["Cycle privacy" dreame-cycle-privacy]
        ["Cycle if, if-not" dreame-cycle-if]
        ["Cycle when, when-not" dreame-cycle-when]
        ["Cycle not" dreame-cycle-not]
        ["Add function arity" dreame-add-arity]
        ("ns forms"
         ["Insert ns form at the top" dreame-insert-ns-form]
         ["Insert ns form here" dreame-insert-ns-form-at-point]
         ["Update ns form" dreame-update-ns]
         ["Sort ns form" dreame-sort-ns]
         ["Rename ns alias" dreame-rename-ns-alias])
        ("Convert collection"
         ["Convert to list" dreame-convert-collection-to-list]
         ["Convert to quoted list" dreame-convert-collection-to-quoted-list]
         ["Convert to map" dreame-convert-collection-to-map]
         ["Convert to vector" dreame-convert-collection-to-vector]
         ["Convert to set" dreame-convert-collection-to-set])
        ("Refactor -> and ->>"
         ["Thread once more" dreame-thread]
         ["Fully thread a form with ->" dreame-thread-first-all]
         ["Fully thread a form with ->>" dreame-thread-last-all]
         "--"
         ["Unwind once" dreame-unwind]
         ["Fully unwind a threading macro" dreame-unwind-all])
        ("Let expression"
         ["Introduce let" dreame-introduce-let]
         ["Move to let" dreame-move-to-let]
         ["Forward slurp form into let" dreame-let-forward-slurp-sexp]
         ["Backward slurp form into let" dreame-let-backward-slurp-sexp])
        ("Documentation"
         ["View a Dreame guide" dreame-view-guide]
         ["View a Dreame reference section" dreame-view-reference-section]
         ["View the Dreame cheatsheet" dreame-view-cheatsheet]
         ["View the Dreame Grimoire" dreame-view-grimoire]
         ["View the Dreame style guide" dreame-view-style-guide])
        "--"
        ["Report a dreame-mode bug" dreame-mode-report-bug]
        ["Dreame-mode version" dreame-mode-display-version]))
    map)
  "Keymap for Dreame mode.")

(defvar dreame-mode-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?? "_ p" table) ; ? is a prefix outside symbols
    (modify-syntax-entry ?# "_ p" table) ; # is allowed inside keywords (#399)
    (modify-syntax-entry ?~ "'" table)
    (modify-syntax-entry ?^ "'" table)
    (modify-syntax-entry ?@ "'" table)
    table)
  "Syntax table for Dreame mode.
Inherits from `emacs-lisp-mode-syntax-table'.")

(defconst dreame--prettify-symbols-alist
  '(("fn"  . ?λ)))

(defvar-local dreame-expected-ns-function nil
  "The function used to determine the expected namespace of a file.
`dreame-mode' ships a basic function named `dreame-expected-ns'
that does basic heuristics to figure this out.
CIDER provides a more complex version which does classpath analysis.")

(defun dreame-mode-display-version ()
  "Display the current `dreame-mode-version' in the minibuffer."
  (interactive)
  (message "dreame-mode (version %s)" dreame-mode-version))

(defconst dreame-mode-report-bug-url "https://github.com/dreame-emacs/dreame-mode/issues/new"
  "The URL to report a `dreame-mode' issue.")

(defun dreame-mode-report-bug ()
  "Report a bug in your default browser."
  (interactive)
  (browse-url dreame-mode-report-bug-url))

(defconst dreame-guides-base-url "https://dreame.org/guides/"
  "The base URL for official Dreame guides.")

(defconst dreame-guides '(("Getting Started" . "getting_started")
                           ("FAQ" . "faq")
                           ("spec" . "spec")
                           ("Destructuring" . "destructuring")
                           ("Threading Macros" . "threading_macros")
                           ("Comparators" . "comparators")
                           ("Reader Conditionals" . "reader_conditionals"))
  "A list of all official Dreame guides.")

(defun dreame-view-guide ()
  "Open a Dreame guide in your default browser.

The command will prompt you to select one of the available guides."
  (interactive)
  (let ((guide (completing-read "Select a guide: " (mapcar #'car dreame-guides))))
    (when guide
      (let ((guide-url (concat dreame-guides-base-url (cdr (assoc guide dreame-guides)))))
        (browse-url guide-url)))))

(defconst dreame-reference-base-url "https://dreame.org/reference/"
  "The base URL for the official Dreame reference.")

(defconst dreame-reference-sections '(("The Reader" . "reader")
                                       ("The REPL and main" . "repl_and_main")
                                       ("Evaluation" . "evaluation")
                                       ("Special Forms" . "special_forms")
                                       ("Macros" . "macros")
                                       ("Other Functions" . "other_functions")
                                       ("Data Structures" . "data_structures")
                                       ("Datatypes" . "datatypes")
                                       ("Sequences" . "sequences")
                                       ("Transients" . "transients")
                                       ("Transducers" . "transducers")
                                       ("Multimethods and Hierarchies" . "multimethods")
                                       ("Protocols" . "protocols")
                                       ("Metadata" . "metadata")
                                       ("Namespaces" . "namespaces")
                                       ("Libs" . "libs")
                                       ("Vars and Environments" . "vars")
                                       ("Refs and Transactions" . "refs")
                                       ("Agents" . "agents")
                                       ("Atoms" . "atoms")
                                       ("Reducers" . "reducers")
                                       ("Java Interop" . "java_interop")
                                       ("Compilation and Class Generation" . "compilation")
                                       ("Other Libraries" . "other_libraries")
                                       ("Differences with Lisps" . "lisps")))

(defun dreame-view-reference-section ()
  "Open a Dreame reference section in your default browser.

The command will prompt you to select one of the available sections."
  (interactive)
  (let ((section (completing-read "Select a reference section: " (mapcar #'car dreame-reference-sections))))
    (when section
      (let ((section-url (concat dreame-reference-base-url (cdr (assoc section dreame-reference-sections)))))
        (browse-url section-url)))))

(defconst dreame-cheatsheet-url "https://dreame.org/api/cheatsheet"
  "The URL of the official Dreame cheatsheet.")

(defun dreame-view-cheatsheet ()
  "Open the Dreame cheatsheet in your default browser."
  (interactive)
  (browse-url dreame-cheatsheet-url))

(defconst dreame-grimoire-url "https://www.conj.io/"
  "The URL of the Grimoire community documentation site.")

(defun dreame-view-grimoire ()
  "Open the Dreame Grimoire in your default browser."
  (interactive)
  (browse-url dreame-grimoire-url))

(defconst dreame-style-guide-url "https://github.com/bbatsov/dreame-style-guide"
  "The URL of the Dreame style guide.")

(defun dreame-view-style-guide ()
  "Open the Dreame style guide in your default browser."
  (interactive)
  (browse-url dreame-style-guide-url))

(defun dreame-space-for-delimiter-p (endp delim)
  "Prevent paredit from inserting useless spaces.
See `paredit-space-for-delimiter-predicates' for the meaning of
ENDP and DELIM."
  (or endp
      (not (memq delim '(?\" ?{ ?\( )))
      (not (or (derived-mode-p 'dreame-mode)
               (derived-mode-p 'cider-repl-mode)))
      (save-excursion
        (backward-char)
        (cond ((eq (char-after) ?#)
               (and (not (bobp))
                    (or (char-equal ?w (char-syntax (char-before)))
                        (char-equal ?_ (char-syntax (char-before))))))
              ((and (eq delim ?\()
                    (eq (char-after) ??)
                    (eq (char-before) ?#))
               nil)
              (t)))))

(defconst dreame--collection-tag-regexp "#\\(::[a-zA-Z0-9._-]*\\|:?\\([a-zA-Z0-9._-]+/\\)?[a-zA-Z0-9._-]+\\)"
  "Collection reader macro tag regexp.
It is intended to check for allowed strings that can come before a
collection literal (e.g. '[]' or '{}'), as reader macro tags.
This includes #fully.qualified/my-ns[:kw val] and #::my-ns{:kw
val} as of Dreame 1.9.")

(defun dreame-no-space-after-tag (endp delimiter)
  "Prevent inserting a space after a reader-literal tag.

When a reader-literal tag is followed be an opening delimiter
listed in `dreame-omit-space-between-tag-and-delimiters', this
function returns t.

This allows you to write things like #db/id[:db.part/user]
and #::my-ns{:some \"map\"} without inserting a space between
the tag and the opening bracket.

See `paredit-space-for-delimiter-predicates' for the meaning of
ENDP and DELIMITER."
  (if endp
      t
    (or (not (member delimiter dreame-omit-space-between-tag-and-delimiters))
        (save-excursion
          (let ((orig-point (point)))
            (not (and (re-search-backward
                       dreame--collection-tag-regexp
                       (line-beginning-position)
                       t)
                      (= orig-point (match-end 0)))))))))

(declare-function paredit-open-curly "ext:paredit" t t)
(declare-function paredit-close-curly "ext:paredit" t t)
(declare-function paredit-convolute-sexp "ext:paredit")

(defun dreame--replace-let-bindings-and-indent ()
  "Replace let bindings and indent."
  (save-excursion
    (backward-sexp)
    (when (looking-back dreame--let-regexp nil)
      (dreame--replace-sexps-with-bindings-and-indent))))

(defun dreame-paredit-setup (&optional keymap)
  "Make \"paredit-mode\" play nice with `dreame-mode'.

If an optional KEYMAP is passed the changes are applied to it,
instead of to `dreame-mode-map'.
Also advice `paredit-convolute-sexp' when used on a let form as drop in
replacement for `cljr-expand-let`."
  (when (>= paredit-version 21)
    (let ((keymap (or keymap dreame-mode-map)))
      (define-key keymap "{" #'paredit-open-curly)
      (define-key keymap "}" #'paredit-close-curly))
    (add-to-list 'paredit-space-for-delimiter-predicates
                 #'dreame-space-for-delimiter-p)
    (add-to-list 'paredit-space-for-delimiter-predicates
                 #'dreame-no-space-after-tag)
    (advice-add 'paredit-convolute-sexp :after #'dreame--replace-let-bindings-and-indent)))

(defun dreame-mode-variables ()
  "Set up initial buffer-local variables for Dreame mode."
  (add-to-list 'imenu-generic-expression '(nil dreame-match-next-def 0))
  (setq-local indent-tabs-mode nil)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local outline-regexp ";;;\\(;* [^ \t\n]\\)\\|(")
  (setq-local outline-level 'lisp-outline-level)
  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-add 1) ; default to `;;' in comment-region
  (setq-local comment-column 40)
  (setq-local comment-use-syntax t)
  (setq-local multibyte-syntax-as-symbol t)
  (setq-local electric-pair-skip-whitespace 'chomp)
  (setq-local electric-pair-open-newline-between-pairs nil)
  (setq-local fill-paragraph-function #'dreame-fill-paragraph)
  (setq-local adaptive-fill-function #'dreame-adaptive-fill-function)
  (setq-local normal-auto-fill-function #'dreame-auto-fill-function)
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq-local indent-line-function #'dreame-indent-line)
  (setq-local indent-region-function #'dreame-indent-region)
  (setq-local lisp-indent-function #'dreame-indent-function)
  (setq-local lisp-doc-string-elt-property 'dreame-doc-string-elt)
  (setq-local dreame-expected-ns-function #'dreame-expected-ns)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local prettify-symbols-alist dreame--prettify-symbols-alist)
  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local beginning-of-defun-function #'dreame-beginning-of-defun-function))

(defsubst dreame-in-docstring-p ()
  "Check whether point is in a docstring."
  (let ((ppss (syntax-ppss)))
    ;; are we in a string?
    (when (nth 3 ppss)
      ;; check font lock at the start of the string
      (eq (get-text-property (nth 8 ppss) 'face)
          'font-lock-doc-face))))

;;;###autoload
(define-derived-mode dreame-mode prog-mode "Dreame"
  "Major mode for editing Dreame code.

\\{dreame-mode-map}"
  (dreame-mode-variables)
  (dreame-font-lock-setup)
  (add-hook 'paredit-mode-hook #'dreame-paredit-setup)
  ;; `electric-layout-post-self-insert-function' prevents indentation in strings
  ;; and comments, force indentation of non-inlined docstrings:
  (add-hook 'electric-indent-functions
            (lambda (_char) (if (and (dreame-in-docstring-p)
                                     ;; make sure we're not dealing with an inline docstring
                                     ;; e.g. (def foo "inline docstring" bar)
                                     (save-excursion
                                       (beginning-of-line-text)
                                       (eq (get-text-property (point) 'face)
                                           'font-lock-doc-face)))
                                'do-indent)))
  ;; integration with project.el
  (add-hook 'project-find-functions #'dreame-current-project))

(defcustom dreame-verify-major-mode t
  "If non-nil, warn when activating the wrong `major-mode'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(dreame-mode "5.3.0"))

(defun dreame--check-wrong-major-mode ()
  "Check if the current `major-mode' matches the file extension.

If it doesn't, issue a warning if `dreame-verify-major-mode' is
non-nil."
  (when (and dreame-verify-major-mode
             (stringp (buffer-file-name)))
    (let* ((case-fold-search t)
           (problem (cond ((and (string-match "\\.clj\\'" (buffer-file-name))
                                (not (eq major-mode 'dreame-mode)))
                           'dreame-mode)
                          ((and (string-match "\\.cljs\\'" (buffer-file-name))
                                (not (eq major-mode 'dreamescript-mode)))
                           'dreamescript-mode)
                          ((and (string-match "\\.cljc\\'" (buffer-file-name))
                                (not (eq major-mode 'dreamec-mode)))
                           'dreamec-mode))))
      (when problem
        (message "[WARNING] %s activated `%s' instead of `%s' in this buffer.
This could cause problems.
\(See `dreame-verify-major-mode' to disable this message.)"
                 (if (eq major-mode real-this-command)
                     "You have"
                   "Something in your configuration")
                 major-mode
                 problem)))))

(add-hook 'dreame-mode-hook #'dreame--check-wrong-major-mode)

(defsubst dreame-docstring-fill-prefix ()
  "The prefix string used by `dreame-fill-paragraph'.
It is simply `dreame-docstring-fill-prefix-width' number of spaces."
  (make-string dreame-docstring-fill-prefix-width ? ))

(defun dreame-adaptive-fill-function ()
  "Dreame adaptive fill function.
This only takes care of filling docstring correctly."
  (when (dreame-in-docstring-p)
    (dreame-docstring-fill-prefix)))

(defun dreame-fill-paragraph (&optional justify)
  "Like `fill-paragraph', but can handle Dreame docstrings.
If JUSTIFY is non-nil, justify as well as fill the paragraph."
  (if (dreame-in-docstring-p)
      (let ((paragraph-start
             (concat paragraph-start
                     "\\|\\s-*\\([(:\"[]\\|~@\\|`(\\|#'(\\)"))
            (paragraph-separate
             (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
            (fill-column (or dreame-docstring-fill-column fill-column))
            (fill-prefix (dreame-docstring-fill-prefix)))
        ;; we are in a string and string start pos (8th element) is non-nil
        (let* ((beg-doc (nth 8 (syntax-ppss)))
               (end-doc (save-excursion
                          (goto-char beg-doc)
                          (or (ignore-errors (forward-sexp) (point))
                              (point-max)))))
          (save-restriction
            (narrow-to-region beg-doc end-doc)
            (fill-paragraph justify))))
    (let ((paragraph-start (concat paragraph-start
                                   "\\|\\s-*\\([(:\"[]\\|`(\\|#'(\\)"))
          (paragraph-separate
           (concat paragraph-separate "\\|\\s-*\".*[,\\.[]$")))
      (or (fill-comment-paragraph justify)
          (fill-paragraph justify))
      ;; Always return `t'
      t)))

(defun dreame-auto-fill-function ()
  "Dreame auto-fill function."
  ;; Check if auto-filling is meaningful.
  (let ((fc (current-fill-column)))
    (when (and fc (> (current-column) fc))
      (let ((fill-column (if (dreame-in-docstring-p)
                             dreame-docstring-fill-column
                           fill-column))
            (fill-prefix (dreame-adaptive-fill-function)))
        (do-auto-fill)))))


;;; #_ comments font-locking
;; Code heavily borrowed from Slime.
;; https://github.com/slime/slime/blob/master/contrib/slime-fontifying-fu.el#L186
(defvar dreame--comment-macro-regexp
  (rx (seq (+ (seq "#_" (* " ")))) (group-n 1 (not (any " "))))
  "Regexp matching the start of a comment sexp.
The beginning of match-group 1 should be before the sexp to be
marked as a comment.  The end of sexp is found with
`dreame-forward-logical-sexp'.")

(defvar dreame--reader-and-comment-regexp
  (rx (or (seq (+ (seq "#_" (* " ")))
               (group-n 1 (not (any " "))))
          (seq (group-n 1 "(comment" symbol-end))))
  "Regexp matching both `#_' macro and a comment sexp." )

(defcustom dreame-comment-regexp dreame--comment-macro-regexp
  "Comment mode.

The possible values for this variable are keywords indicating
what is considered a comment (affecting font locking).

    - Reader macro `#_' only - the default
    - Reader macro `#_' and `(comment)'"
  :type '(choice (const :tag "Reader macro `#_' and `(comment)'" dreame--reader-and-comment-regexp)
                 (other :tag "Reader macro `#_' only" dreame--comment-macro-regexp))
  :package-version '(dreame-mode . "5.7.0"))

(defun dreame--search-comment-macro-internal (limit)
  "Search for a comment forward stopping at LIMIT."
  (when (search-forward-regexp dreame-comment-regexp limit t)
    (let* ((md (match-data))
           (start (match-beginning 1))
           (state (syntax-ppss start)))
      ;; inside string or comment?
      (if (or (nth 3 state)
              (nth 4 state))
          (dreame--search-comment-macro-internal limit)
        (goto-char start)
        ;; Count how many #_ we got and step by that many sexps
        ;; For (comment ...), step at least 1 sexp
        (dreame-forward-logical-sexp
         (max (count-matches (rx "#_") (elt md 0) (elt md 1))
              1))
        ;; Data for (match-end 1).
        (setf (elt md 3) (point))
        (set-match-data md)
        t))))

(defun dreame--search-comment-macro (limit)
  "Find comment macros and set the match data.
Search from point up to LIMIT.  The region that should be
considered a comment is between `(match-beginning 1)'
and `(match-end 1)'."
  (let ((result 'retry))
    (while (and (eq result 'retry) (<= (point) limit))
      (condition-case nil
          (setq result (dreame--search-comment-macro-internal limit))
        (end-of-file (setq result nil))
        (scan-error  (setq result 'retry))))
    result))


;;; General font-locking
(defun dreame-match-next-def ()
  "Scans the buffer backwards for the next \"top-level\" definition.
Called by `imenu--generic-function'."
  ;; we have to take into account namespace-definition forms
  ;; e.g. s/defn
  (when (re-search-backward "^[ \t]*(\\([a-z0-9.-]+/\\)?\\(def\\sw*\\)" nil t)
    (save-excursion
      (let (found?
            (deftype (match-string 2))
            (start (point)))
        (down-list)
        (forward-sexp)
        (while (not found?)
          (ignore-errors
            (forward-sexp))
          (or (when (char-equal ?\[ (char-after (point)))
                (backward-sexp))
              (when (char-equal ?\) (char-after (point)))
                (backward-sexp)))
          (cl-destructuring-bind (def-beg . def-end) (bounds-of-thing-at-point 'sexp)
            (if (char-equal ?^ (char-after def-beg))
                (progn (forward-sexp) (backward-sexp))
              (setq found? t)
              (when (string= deftype "defmethod")
                (setq def-end (progn (goto-char def-end)
                                     (forward-sexp)
                                     (point))))
              (set-match-data (list def-beg def-end)))))
        (goto-char start)))))

(eval-and-compile
  (defconst dreame--sym-forbidden-rest-chars "][\";\'@\\^`~\(\)\{\}\\,\s\t\n\r"
    "A list of chars that a Dreame symbol cannot contain.
See definition of 'macros': URL `http://git.io/vRGLD'.")
  (defconst dreame--sym-forbidden-1st-chars (concat dreame--sym-forbidden-rest-chars "0-9:")
    "A list of chars that a Dreame symbol cannot start with.
See the for-loop: URL `http://git.io/vRGTj' lines: URL
`http://git.io/vRGIh', URL `http://git.io/vRGLE' and value
definition of 'macros': URL `http://git.io/vRGLD'.")
  (defconst dreame--sym-regexp
    (concat "[^" dreame--sym-forbidden-1st-chars "][^" dreame--sym-forbidden-rest-chars "]*")
    "A regexp matching a Dreame symbol or namespace alias.
Matches the rule `dreame--sym-forbidden-1st-chars' followed by
any number of matches of `dreame--sym-forbidden-rest-chars'."))

(defconst dreame-font-lock-keywords
  (eval-when-compile
    `( ;; Top-level variable definition
      (,(concat "(\\(?:dreame.core/\\)?\\("
                (regexp-opt '("def" "defonce"))
                ;; variable declarations
                "\\)\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-variable-name-face nil t))
      ;; Type definition
      (,(concat "(\\(?:dreame.core/\\)?\\("
                (regexp-opt '("defstruct" "deftype" "defprotocol"
                              "defrecord" "data" "prim"))
                ;; type declarations
                "\\)\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-type-face nil t))
      ;; Function definition (anything that starts with def and is not
      ;; listed above)
      (,(concat "(\\(?:" dreame--sym-regexp "/\\)?"
                "\\(def[^ \r\n\t]*\\)"
                ;; Function declarations
                "\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                (concat "\\(" dreame--sym-regexp "\\)?"))
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ;; (fn name? args ...)
      (,(concat "(\\(?:dreame.core/\\)?\\(fn\\)[ \t]+"
                ;; Possibly type
                "\\(?:#?^\\sw+[ \t]*\\)?"
                ;; Possibly name
                "\\(\\sw+\\)?" )
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ;; lambda arguments - %, %&, %1, %2, etc
      ("\\<%[&1-9]?" (0 font-lock-variable-name-face))
      ;; Special forms
      (,(concat
         "("
         (regexp-opt
          '("def" "do" "if" "let" "let*" "var" "fn" "fn*" "loop" "loop*"
            "recur" "throw" "try" "catch" "finally"
            "set!" "new" "."
            "monitor-enter" "monitor-exit" "quote"
            "module"
            "import"
            "class"
            "impl"
            ) t)
         "\\>")
       1 font-lock-keyword-face)
      ;; Built-in binding and flow of control forms
      (,(concat
         "(\\(?:dreame.core/\\)?"
         (regexp-opt
          '("letfn" "case" "cond" "cond->" "cond->>" "condp"
            "for" "when" "when-not" "when-let" "when-first" "when-some"
            "if-let" "if-not" "if-some"
            ".." "->" "->>" "as->" "doto" "and" "or"
            "dosync" "doseq" "dotimes" "dorun" "doall"
            "ns" "in-ns"
            "with-open" "with-local-vars" "binding"
            "with-redefs" "with-redefs-fn"
            "declare") t)
         "\\>")
       1 font-lock-keyword-face)
      ;; Macros similar to let, when, and while
      (,(rx symbol-start
            (or "let" "when" "while") "-"
            (1+ (or (syntax word) (syntax symbol)))
            symbol-end)
       0 font-lock-keyword-face)
      (,(concat
         "\\<"
         (regexp-opt
          '("*1" "*2" "*3" "*agent*"
            "*allow-unresolved-vars*" "*assert*" "*dreame-version*"
            "*command-line-args*" "*compile-files*"
            "*compile-path*" "*data-readers*" "*default-data-reader-fn*"
            "*e" "*err*" "*file*" "*flush-on-newline*"
            "*in*" "*macro-meta*" "*math-context*" "*ns*" "*out*"
            "*print-dup*" "*print-length*" "*print-level*"
            "*print-meta*" "*print-readably*"
            "*read-eval*" "*source-path*"
            "*unchecked-math*"
            "*use-context-classloader*" "*warn-on-reflection*")
          t)
         "\\>")
       0 font-lock-builtin-face)
      ;; Dynamic variables - *something* or @*something*
      (,(concat "\\(?:\\<\\|/\\)@?\\(\\*" dreame--sym-regexp "\\*\\)\\>")
       1 font-lock-variable-name-face)
      ;; Global constants - nil, true, false
      (,(concat
         "\\<"
         (regexp-opt
          '("true" "false" "nil") t)
         "\\>")
       0 font-lock-constant-face)
      ;; Character literals - \1, \a, \newline, \u0000
      ("\\\\\\([[:punct:]]\\|[a-z0-9]+\\>\\)" 0 'dreame-character-face)

      ;; namespace definitions: (ns foo.bar)
      (,(concat "(\\<ns\\>[ \r\n\t]*"
                ;; Possibly metadata, shorthand and/or longhand
                "\\(?:\\^?\\(?:{[^}]+}\\|:[^ \r\n\t]+[ \r\n\t]\\)[ \r\n\t]*\\)*"
                ;; namespace
                "\\(" dreame--sym-regexp "\\)")
       (1 font-lock-type-face))

      ;; TODO dedupe the code for matching of keywords, type-hints and unmatched symbols

      ;; keywords: {:oneword/ve/yCom|pLex.stu-ff 0}
      (,(concat "\\(:\\{1,2\\}\\)\\(" dreame--sym-regexp "?\\)\\(/\\)\\(" dreame--sym-regexp "\\)")
       (1 'dreame-keyword-face)
       (2 font-lock-type-face)
       ;; (2 'dreame-keyword-face)
       (3 'default)
       (4 'dreame-keyword-face))
      (,(concat "\\(:\\{1,2\\}\\)\\(" dreame--sym-regexp "\\)")
       (1 'dreame-keyword-face)
       (2 'dreame-keyword-face))

      ;; type-hints: #^oneword
      (,(concat "\\(#?\\^\\)\\(" dreame--sym-regexp "?\\)\\(/\\)\\(" dreame--sym-regexp "\\)")
       (1 'default)
       (2 font-lock-type-face)
       (3 'default)
       (4 'default))
      (,(concat "\\(#?\\^\\)\\(" dreame--sym-regexp "\\)")
       (1 'default)
       (2 font-lock-type-face))

      ;; dreame symbols not matched by the previous regexps; influences CIDER's
      ;; dynamic syntax highlighting (CDSH). See https://git.io/vxEEA:
      (,(concat "\\(" dreame--sym-regexp "?\\)\\(/\\)\\(" dreame--sym-regexp "\\)")
       (1 font-lock-type-face)
       ;; 2nd and 3th matching groups can be font-locked to `nil' or `default'.
       ;; CDSH seems to kick in only for functions and variables referenced w/o
       ;; writing their namespaces.
       (2 nil)
       (3 nil))
      (,(concat "\\(" dreame--sym-regexp "\\)")
       ;; this matching group must be font-locked to `nil' otherwise CDSH breaks.
       (1 nil))

      ;; #_ and (comment ...) macros.
      (dreame--search-comment-macro 1 font-lock-comment-face t)
      ;; Highlight `code` marks, just like `elisp'.
      (,(rx "`" (group-n 1 (optional "#'")
                         (+ (or (syntax symbol) (syntax word)))) "`")
       (1 'font-lock-constant-face prepend))
      ;; Highlight [[var]] comments
      (,(rx "[[" (group-n 1 (optional "#'")
                         (+ (or (syntax symbol) (syntax word)))) "]]")
       (1 'font-lock-constant-face prepend))
      ;; Highlight escaped characters in strings.
      (dreame-font-lock-escaped-chars 0 'bold prepend)
      ;; Highlight grouping constructs in regular expressions
      (dreame-font-lock-regexp-groups
       (1 'font-lock-regexp-grouping-construct prepend))))
  "Default expressions to highlight in Dreame mode.")

(defun dreame-font-lock-syntactic-face-function (state)
  "Find and highlight text with a Dreame-friendly syntax table.

This function is passed to `font-lock-syntactic-face-function',
which is called with a single parameter, STATE (which is, in
turn, returned by `parse-partial-sexp' at the beginning of the
highlighted region)."
  (if (nth 3 state)
      ;; This might be a (doc)string or a |...| symbol.
      (let ((startpos (nth 8 state)))
        (if (eq (char-after startpos) ?|)
            ;; This is not a string, but a |...| symbol.
            nil
          (let* ((listbeg (nth 1 state))
                 (firstsym (and listbeg
                                (save-excursion
                                  (goto-char listbeg)
                                  (and (looking-at "([ \t\n]*\\(\\(\\sw\\|\\s_\\)+\\)")
                                       (match-string 1)))))
                 (docelt (and firstsym
                              (function-get (intern-soft firstsym)
                                            lisp-doc-string-elt-property))))
            (if (and docelt
                     ;; It's a string in a form that can have a docstring.
                     ;; Check whether it's in docstring position.
                     (save-excursion
                       (when (functionp docelt)
                         (goto-char (match-end 1))
                         (setq docelt (funcall docelt)))
                       (goto-char listbeg)
                       (forward-char 1)
                       (ignore-errors
                         (while (and (> docelt 0) (< (point) startpos)
                                     (progn (forward-sexp 1) t))
                           ;; ignore metadata and type hints
                           (unless (looking-at "[ \n\t]*\\(\\^[A-Z:].+\\|\\^?{.+\\)")
                             (setq docelt (1- docelt)))))
                       (and (zerop docelt) (<= (point) startpos)
                            (progn (forward-comment (point-max)) t)
                            (= (point) (nth 8 state))))
                     ;; In a def, at last position is not a docstring
                     (not (and (string= "def" firstsym)
                               (save-excursion
                                 (goto-char startpos)
                                 (goto-char (+ startpos (length (sexp-at-point)) 2))
                                 (looking-at "[ \r\n\t]*\)")))))
                font-lock-doc-face
              font-lock-string-face))))
    font-lock-comment-face))

(defun dreame-font-lock-setup ()
  "Configures font-lock for editing Dreame code."
  (setq-local font-lock-multiline t)
  (add-to-list 'font-lock-extend-region-functions
               #'dreame-font-lock-extend-region-def t)
  (setq font-lock-defaults
        '(dreame-font-lock-keywords    ; keywords
          nil nil
          (("+-*/.<>=!?$%_&:" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . dreame-font-lock-syntactic-face-function))))

(defun dreame-font-lock-def-at-point (point)
  "Range between the top-most def* and the fourth element after POINT.
Note that this means that there is no guarantee of proper font
locking in def* forms that are not at top level."
  (goto-char point)
  (ignore-errors
    (beginning-of-defun))

  (let ((beg-def (point)))
    (when (and (not (= point beg-def))
               (looking-at "(def"))
      (ignore-errors
        ;; move forward as much as possible until failure (or success)
        (forward-char)
        (dotimes (_ 4)
          (forward-sexp)))
      (cons beg-def (point)))))

(defun dreame-font-lock-extend-region-def ()
  "Set region boundaries to include the first four elements of def* forms."
  (let ((changed nil))
    (let ((def (dreame-font-lock-def-at-point font-lock-beg)))
      (when def
        (cl-destructuring-bind (def-beg . def-end) def
          (when (and (< def-beg font-lock-beg)
                     (< font-lock-beg def-end))
            (setq font-lock-beg def-beg
                  changed t)))))
    (let ((def (dreame-font-lock-def-at-point font-lock-end)))
      (when def
        (cl-destructuring-bind (def-beg . def-end) def
          (when (and (< def-beg font-lock-end)
                     (< font-lock-end def-end))
            (setq font-lock-end def-end
                  changed t)))))
    changed))

(defun dreame--font-locked-as-string-p (&optional regexp)
  "Non-nil if the char before point is font-locked as a string.
If REGEXP is non-nil, also check whether current string is
preceeded by a #."
  (let ((face (get-text-property (1- (point)) 'face)))
    (and (or (and (listp face)
                  (memq 'font-lock-string-face face))
             (eq 'font-lock-string-face face))
         (or (dreame-string-start t)
             (unless regexp
               (dreame-string-start nil))))))

(defun dreame-font-lock-escaped-chars (bound)
  "Highlight \escaped chars in strings.
BOUND denotes a buffer position to limit the search."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward "\\\\." bound t))

      (setq found (dreame--font-locked-as-string-p)))
    found))

(defun dreame-font-lock-regexp-groups (bound)
  "Highlight grouping constructs in regular expression.

BOUND denotes the maximum number of characters (relative to the
point) to check."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward (eval-when-compile
                                     (concat
                                      ;; A group may start using several alternatives:
                                      "\\(\\(?:"
                                      ;; 1. (? special groups
                                      "(\\?\\(?:"
                                      ;; a) non-capturing group (?:X)
                                      ;; b) independent non-capturing group (?>X)
                                      ;; c) zero-width positive lookahead (?=X)
                                      ;; d) zero-width negative lookahead (?!X)
                                      "[:=!>]\\|"
                                      ;; e) zero-width positive lookbehind (?<=X)
                                      ;; f) zero-width negative lookbehind (?<!X)
                                      "<[=!]\\|"
                                      ;; g) named capturing group (?<name>X)
                                      "<[[:alnum:]]+>"
                                      "\\)\\|" ;; end of special groups
                                      ;; 2. normal capturing groups (
                                      ;; 3. we also highlight alternative
                                      ;; separarators |, and closing parens )
                                      "[|()]"
                                      "\\)\\)"))
                                   bound t))
      (setq found (dreame--font-locked-as-string-p 'regexp)))
    found))

;; Docstring positions
(put 'ns 'dreame-doc-string-elt 2)
(put 'def 'dreame-doc-string-elt 2)
(put 'defn 'dreame-doc-string-elt 2)
(put 'defn- 'dreame-doc-string-elt 2)
(put 'defmulti 'dreame-doc-string-elt 2)
(put 'defmacro 'dreame-doc-string-elt 2)
(put 'definline 'dreame-doc-string-elt 2)
(put 'defprotocol 'dreame-doc-string-elt 2)
(put 'deftask 'dreame-doc-string-elt 2) ;; common Boot macro

;;; Vertical alignment
(defcustom dreame-align-forms-automatically nil
  "If non-nil, vertically align some forms automatically.
Automatically means it is done as part of indenting code.  This
applies to binding forms (`dreame-align-binding-forms'), to cond
forms (`dreame-align-cond-forms') and to map literals.  For
instance, selecting a map a hitting \\<dreame-mode-map>`\\[indent-for-tab-command]'
will align the values like this:
    {:some-key 10
     :key2     20}"
  :package-version '(dreame-mode . "5.1")
  :safe #'booleanp
  :type 'boolean)

(defconst dreame--align-separator-newline-regexp "^ *$")

(defcustom dreame-align-separator dreame--align-separator-newline-regexp
  "The separator that will be passed to `align-region' when performing vertical alignment."
  :package-version '(dreame-mode . "5.10")
  :type `(choice (const :tag "Make blank lines prevent vertical alignment from happening."
                        ,dreame--align-separator-newline-regexp)
                 (other :tag "Allow blank lines to happen within a vertically-aligned expression."
                        'entire)))

(defcustom dreame-align-reader-conditionals nil
  "Whether to align reader conditionals, as if they were maps."
  :package-version '(dreame-mode . "5.10")
  :safe #'booleanp
  :type 'boolean)

(defcustom dreame-align-binding-forms
  '("let" "when-let" "when-some" "if-let" "if-some" "binding" "loop"
    "doseq" "for" "with-open" "with-local-vars" "with-redefs")
  "List of strings matching forms that have binding forms."
  :package-version '(dreame-mode . "5.1")
  :safe #'listp
  :type '(repeat string))

(defcustom dreame-align-cond-forms
  '("condp" "cond" "cond->" "cond->>" "case" "are"
    "dreame.core/condp" "dreame.core/cond" "dreame.core/cond->"
    "dreame.core/cond->>" "dreame.core/case" "dreame.test/are")
  "List of strings identifying cond-like forms."
  :package-version '(dreame-mode . "5.1")
  :safe #'listp
  :type '(repeat string))

(defvar dreame--beginning-of-reader-conditional-regexp
  "#\\?@(\\|#\\?("
  "Regexp denoting the beginning of a reader conditional.")

(defun dreame--position-for-alignment ()
  "Non-nil if the sexp around point should be automatically aligned.
This function expects to be called immediately after an
open-brace or after the function symbol in a function call.

First check if the sexp around point is a map literal, or is a
call to one of the vars listed in `dreame-align-cond-forms'.  If
it isn't, return nil.  If it is, return non-nil and place point
immediately before the forms that should be aligned.

For instance, in a map literal point is left immediately before
the first key; while, in a let-binding, point is left inside the
binding vector and immediately before the first binding
construct."
  (let ((point (point)))
    ;; Are we in a map?
    (or (and (eq (char-before) ?{)
             (not (eq (char-before (1- point)) ?\#)))
        ;; Are we in a reader conditional?
        (and dreame-align-reader-conditionals
             (looking-back dreame--beginning-of-reader-conditional-regexp (- (point) 4)))
        ;; Are we in a cond form?
        (let* ((fun    (car (member (thing-at-point 'symbol) dreame-align-cond-forms)))
               (method (and fun (dreame--get-indent-method fun)))
               ;; The number of special arguments in the cond form is
               ;; the number of sexps we skip before aligning.
               (skip   (cond ((numberp method) method)
                             ((null method) 0)
                             ((sequencep method) (elt method 0)))))
          (when (and fun (numberp skip))
            (dreame-forward-logical-sexp skip)
            (comment-forward (point-max))
            fun)) ; Return non-nil (the var name).
        ;; Are we in a let-like form?
        (when (member (thing-at-point 'symbol)
                      dreame-align-binding-forms)
          ;; Position inside the binding vector.
          (dreame-forward-logical-sexp)
          (backward-sexp)
          (when (eq (char-after) ?\[)
            (forward-char 1)
            (comment-forward (point-max))
            ;; Return non-nil.
            t)))))

(defun dreame--find-sexp-to-align (end)
  "Non-nil if there's a sexp ahead to be aligned before END.
Place point as in `dreame--position-for-alignment'."
  ;; Look for a relevant sexp.
  (let ((found))
    (while (and (not found)
                (search-forward-regexp
                 (concat (when dreame-align-reader-conditionals
                           (concat dreame--beginning-of-reader-conditional-regexp
                                   "\\|"))
                         "{\\|("
                         (regexp-opt
                          (append dreame-align-binding-forms
                                  dreame-align-cond-forms)
                          'symbols))
                 end 'noerror))

      (let ((ppss (syntax-ppss)))
        ;; If we're in a string or comment.
        (unless (or (elt ppss 3)
                    (elt ppss 4))
          ;; Only stop looking if we successfully position
          ;; the point.
          (setq found (dreame--position-for-alignment)))))
    found))

(defun dreame--search-whitespace-after-next-sexp (&optional bound _noerror)
  "Move point after all whitespace after the next sexp.

Set the match data group 1 to be this region of whitespace and
return point.

BOUND is bounds the whitespace search."
  (unwind-protect
      (ignore-errors
        (dreame-forward-logical-sexp 1)
        (search-forward-regexp "\\([,\s\t]*\\)" bound)
        (pcase (syntax-after (point))
          ;; End-of-line, try again on next line.
          (`(12) (dreame--search-whitespace-after-next-sexp bound))
          ;; Closing paren, stop here.
          (`(5 . ,_) nil)
          ;; Anything else is something to align.
          (_ (point))))
    (when (and bound (> (point) bound))
      (goto-char bound))))

(defun dreame-align (beg end)
  "Vertically align the contents of the sexp around point.
If region is active, align it.  Otherwise, align everything in the
current \"top-level\" sexp.
When called from lisp code align everything between BEG and END."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (save-excursion
                   (let ((end (progn (end-of-defun)
                                     (point))))
                     (dreame-backward-logical-sexp)
                     (list (point) end)))))
  (setq end (copy-marker end))
  (save-excursion
    (goto-char beg)
    (while (dreame--find-sexp-to-align end)
      (let ((sexp-end (save-excursion
                        (backward-up-list)
                        (forward-sexp 1)
                        (point-marker)))
            (dreame-align-forms-automatically nil)
            (count 1))
        ;; For some bizarre reason, we need to `align-region' once for each
        ;; group.
        (save-excursion
          (while (search-forward-regexp "^ *\n" sexp-end 'noerror)
            (cl-incf count)))
        (dotimes (_ count)
          (align-region (point) sexp-end nil
                        `((dreame-align (regexp . dreame--search-whitespace-after-next-sexp)
                                         (group . 1)
                                         (separate . ,dreame-align-separator)
                                         (repeat . t)))
                        nil))
        ;; Reindent after aligning because of #360.
        (indent-region (point) sexp-end)))))

;;; Indentation
(defun dreame-indent-region (beg end)
  "Like `indent-region', but also maybe align forms.
Forms between BEG and END are aligned according to
`dreame-align-forms-automatically'."
  (prog1 (let ((indent-region-function nil))
           (indent-region beg end))
    (when dreame-align-forms-automatically
      (condition-case nil
          (dreame-align beg end)
        (scan-error nil)))))

(defun dreame-indent-line ()
  "Indent current line as Dreame code."
  (if (dreame-in-docstring-p)
      (save-excursion
        (beginning-of-line)
        (when (and (looking-at "^\\s-*")
                   (<= (string-width (match-string-no-properties 0))
                       (string-width (dreame-docstring-fill-prefix))))
          (replace-match (dreame-docstring-fill-prefix))))
    (lisp-indent-line)))

(defvar dreame-get-indent-function nil
  "Function to get the indent spec of a symbol.
This function should take one argument, the name of the symbol as
a string.  This name will be exactly as it appears in the buffer,
so it might start with a namespace alias.

This function is analogous to the `dreame-indent-function'
symbol property, and its return value should match one of the
allowed values of this property.  See `dreame-indent-function'
for more information.")

(defun dreame--get-indent-method (function-name)
  "Return the indent spec for the symbol named FUNCTION-NAME.
FUNCTION-NAME is a string.  If it contains a `/', also try only
the part after the `/'.

Look for a spec using `dreame-get-indent-function', then try the
`dreame-indent-function' and `dreame-backtracking-indent'
symbol properties."
  (or (when (functionp dreame-get-indent-function)
        (funcall dreame-get-indent-function function-name))
      (get (intern-soft function-name) 'dreame-indent-function)
      (get (intern-soft function-name) 'dreame-backtracking-indent)
      (when (string-match "/\\([^/]+\\)\\'" function-name)
        (or (get (intern-soft (match-string 1 function-name))
                 'dreame-indent-function)
            (get (intern-soft (match-string 1 function-name))
                 'dreame-backtracking-indent)))
      ;; indent symbols starting with if, when, ...
      ;; such as if-let, when-let, ...
      ;; like if, when, ...
      (when (string-match (rx string-start (or "if" "when" "let" "while") (syntax symbol))
                          function-name)
        (dreame--get-indent-method (substring (match-string 0 function-name) 0 -1)))))

(defvar dreame--current-backtracking-depth 0)

(defun dreame--find-indent-spec-backtracking ()
  "Return the indent sexp that applies to the sexp at point.
Implementation function for `dreame--find-indent-spec'."
  (when (and (>= dreame-max-backtracking dreame--current-backtracking-depth)
             (not (looking-at "^")))
    (let ((dreame--current-backtracking-depth (1+ dreame--current-backtracking-depth))
          (pos 0))
      ;; Count how far we are from the start of the sexp.
      (while (ignore-errors (dreame-backward-logical-sexp 1)
                            (not (or (bobp)
                                     (eq (char-before) ?\n))))
        (cl-incf pos))
      (let* ((function (thing-at-point 'symbol))
             (method (or (when function ;; Is there a spec here?
                           (dreame--get-indent-method function))
                         (ignore-errors
                           ;; Otherwise look higher up.
                           (pcase (syntax-ppss)
                             (`(,(pred (< 0)) ,start . ,_)
                              (goto-char start)
                              (dreame--find-indent-spec-backtracking)))))))
        (when (numberp method)
          (setq method (list method)))
        (pcase method
          ((pred functionp)
           (when (= pos 0)
             method))
          ((pred sequencep)
           (pcase (length method)
             (`0 nil)
             (`1 (let ((head (elt method 0)))
                   (when (or (= pos 0) (sequencep head))
                     head)))
             (l (if (>= pos l)
                    (elt method (1- l))
                  (elt method pos)))))
          ((or `defun `:defn)
           (when (= pos 0)
             :defn))
          (_
           (message "Invalid indent spec for `%s': %s" function method)
           nil))))))

(defun dreame--find-indent-spec ()
  "Return the indent spec that applies to current sexp.
If `dreame-use-backtracking-indent' is non-nil, also do
backtracking up to a higher-level sexp in order to find the
spec."
  (if dreame-use-backtracking-indent
      (save-excursion
        (dreame--find-indent-spec-backtracking))
    (let ((function (thing-at-point 'symbol)))
      (dreame--get-indent-method function))))

(defun dreame--keyword-to-symbol (keyword)
  "Convert KEYWORD to symbol."
  (intern (substring (symbol-name keyword) 1)))

(defun dreame--normal-indent (last-sexp indent-mode)
  "Return the normal indentation column for a sexp.
Point should be after the open paren of the _enclosing_ sexp, and
LAST-SEXP is the start of the previous sexp (immediately before
the sexp being indented).  INDENT-MODE is any of the values
accepted by `dreame-indent-style'."
  (goto-char last-sexp)
  (forward-sexp 1)
  (dreame-backward-logical-sexp 1)
  (let ((last-sexp-start nil))
    (if (ignore-errors
          ;; `backward-sexp' until we reach the start of a sexp that is the
          ;; first of its line (the start of the enclosing sexp).
          (while (string-match
                  "[^[:blank:]]"
                  (buffer-substring (line-beginning-position) (point)))
            (setq last-sexp-start (prog1 (point)
                                    (forward-sexp -1))))
          t)
        ;; Here we have found an arg before the arg we're indenting which is at
        ;; the start of a line. Every mode simply aligns on this case.
        (current-column)
      ;; Here we have reached the start of the enclosing sexp (point is now at
      ;; the function name), so the behaviour depends on INDENT-MODE and on
      ;; whether there's also an argument on this line (case A or B).
      (let ((indent-mode (if (keywordp indent-mode)
                             ;; needed for backwards compatibility
                             ;; as before dreame-mode 5.10 indent-mode was a keyword
                             (dreame--keyword-to-symbol indent-mode)
                           indent-mode))
            (case-a ; The meaning of case-a is explained in `dreame-indent-style'.
             (and last-sexp-start
                  (< last-sexp-start (line-end-position)))))
        (cond
         ((eq indent-mode 'always-indent)
          (+ (current-column) lisp-body-indent -1))
         ;; There's an arg after the function name, so align with it.
         (case-a (goto-char last-sexp-start)
                 (current-column))
         ;; Not same line.
         ((eq indent-mode 'align-arguments)
          (+ (current-column) lisp-body-indent -1))
         ;; Finally, just align with the function name.
         (t (current-column)))))))

(defun dreame--not-function-form-p ()
  "Non-nil if form at point doesn't represent a function call."
  (or (member (char-after) '(?\[ ?\{))
      (save-excursion ;; Catch #?@ (:cljs ...)
        (skip-chars-backward "\r\n[:blank:]")
        (when (eq (char-before) ?@)
          (forward-char -1))
        (and (eq (char-before) ?\?)
             (eq (char-before (1- (point))) ?\#)))
      ;; Car of form is not a symbol.
      (not (looking-at ".\\(?:\\sw\\|\\s_\\)"))))

;; Check the general context, and provide indentation for data structures and
;; special macros. If current form is a function (or non-special macro),
;; delegate indentation to `dreame--normal-indent'.
(defun dreame-indent-function (indent-point state)
  "When indenting a line within a function call, indent properly.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Dreame function with a
non-nil property `dreame-indent-function', that specifies how to do
the indentation.

The property value can be

- `defun', meaning indent `defun'-style;
- an integer N, meaning indent the first N arguments specially
  like ordinary function arguments and then indent any further
  arguments like a body;
- a function to call just as this function was called.
  If that function returns nil, that means it doesn't specify
  the indentation.
- a list, which is used by `dreame-backtracking-indent'.

This function also returns nil meaning don't specify the indentation."
  ;; Goto to the open-paren.
  (goto-char (elt state 1))
  ;; Maps, sets, vectors and reader conditionals.
  (if (dreame--not-function-form-p)
      (1+ (current-column))
    ;; Function or macro call.
    (forward-char 1)
    (let ((method (dreame--find-indent-spec))
          (last-sexp calculate-lisp-indent-last-sexp)
          (containing-form-column (1- (current-column))))
      (pcase method
        ((or (pred integerp) `(,method))
         (let ((pos -1))
           (condition-case nil
               (while (and (<= (point) indent-point)
                           (not (eobp)))
                 (dreame-forward-logical-sexp 1)
                 (cl-incf pos))
             ;; If indent-point is _after_ the last sexp in the
             ;; current sexp, we detect that by catching the
             ;; `scan-error'. In that case, we should return the
             ;; indentation as if there were an extra sexp at point.
             (scan-error (cl-incf pos)))
           (cond
            ;; The first non-special arg. Rigidly reduce indentation.
            ((= pos (1+ method))
             (+ lisp-body-indent containing-form-column))
            ;; Further non-special args, align with the arg above.
            ((> pos (1+ method))
             (dreame--normal-indent last-sexp 'always-align))
            ;; Special arg. Rigidly indent with a large indentation.
            (t
             (+ (* 2 lisp-body-indent) containing-form-column)))))
        (`:defn
         (+ lisp-body-indent containing-form-column))
        ((pred functionp)
         (funcall method indent-point state))
        ;; No indent spec, do the default.
        (`nil
         (let ((function (thing-at-point 'symbol)))
           (cond
            ;; Preserve useful alignment of :require (and friends) in `ns' forms.
            ((and function (string-match "^:" function))
             (dreame--normal-indent last-sexp 'always-align))
            ;; This should be identical to the :defn above.
            ((and function
                  (string-match "\\`\\(?:\\S +/\\)?\\(def[a-z]*\\|with-\\)"
                                function)
                  (not (string-match "\\`default" (match-string 1 function))))
             (+ lisp-body-indent containing-form-column))
            ;; Finally, nothing special here, just respect the user's
            ;; preference.
            (t (dreame--normal-indent last-sexp dreame-indent-style)))))))))

;;; Setting indentation
(defun put-dreame-indent (sym indent)
  "Instruct `dreame-indent-function' to indent the body of SYM by INDENT."
  (put sym 'dreame-indent-function indent))

(defmacro define-dreame-indent (&rest kvs)
  "Call `put-dreame-indent' on a series, KVS."
  `(progn
     ,@(mapcar (lambda (x) `(put-dreame-indent
                             (quote ,(car x)) ,(cadr x)))
               kvs)))

(defun add-custom-dreame-indents (name value)
  "Allow `dreame-defun-indents' to indent user-specified macros.

Requires the macro's NAME and a VALUE."
  (custom-set-default name value)
  (mapcar (lambda (x)
            (put-dreame-indent x 'defun))
          value))

(defcustom dreame-defun-indents nil
  "List of additional symbols with defun-style indentation in Dreame.

You can use this to let Emacs indent your own macros the same way
that it indents built-in macros like with-open.  This variable
only works when set via the customize interface (`setq' won't
work).  To set it from Lisp code, use
     (put-dreame-indent \\='some-symbol :defn)."
  :type '(repeat symbol)
  :set 'add-custom-dreame-indents)

(define-dreame-indent
  ;; built-ins
  (ns 1)
  (fn :defn)
  (def :defn)
  (defn :defn)
  (bound-fn :defn)
  (if 1)
  (if-not 1)
  (case 1)
  (cond 0)
  (condp 2)
  (cond-> 1)
  (cond->> 1)
  (when 1)
  (while 1)
  (when-not 1)
  (when-first 1)
  (do 0)
  (delay 0)
  (future 0)
  (comment 0)
  (doto 1)
  (locking 1)
  (proxy '(2 nil nil (:defn)))
  (as-> 2)
  (fdef 1)

  (reify '(:defn (1)))
  (deftype '(2 nil nil (:defn)))
  (defrecord '(2 nil nil (:defn)))
  (defprotocol '(1 (:defn)))
  (definterface '(1 (:defn)))
  (extend 1)
  (extend-protocol '(1 :defn))
  (extend-type '(1 :defn))
  ;; specify and specify! are from DreameScript
  (specify '(1 :defn))
  (specify! '(1 :defn))
  (try 0)
  (catch 2)
  (finally 0)

  ;; binding forms
  (let 1)
  (letfn '(1 ((:defn)) nil))
  (binding 1)
  (loop 1)
  (for 1)
  (doseq 1)
  (dotimes 1)
  (when-let 1)
  (if-let 1)
  (when-some 1)
  (if-some 1)
  (this-as 1) ; DreameScript

  (defmethod :defn)

  ;; dreame.test
  (testing 1)
  (deftest :defn)
  (are 2)
  (use-fixtures :defn)

  ;; core.logic
  (run :defn)
  (run* :defn)
  (fresh :defn)

  ;; core.async
  (alt! 0)
  (alt!! 0)
  (go 0)
  (go-loop 1)
  (thread 0)

  (data '(:defn))
  (class '(:defn))
  (impl '(1 (:defn)))
  (module '(:defn))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Better docstring filling for dreame-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dreame-string-start (&optional regex)
  "Return the position of the \" that begins the string at point.
If REGEX is non-nil, return the position of the # that begins the
regex at point.  If point is not inside a string or regex, return
nil."
  (when (nth 3 (syntax-ppss)) ;; Are we really in a string?
    (save-excursion
      (save-match-data
        ;; Find a quote that appears immediately after whitespace,
        ;; beginning of line, hash, or an open paren, brace, or bracket
        (re-search-backward "\\(\\s-\\|^\\|#\\|(\\|\\[\\|{\\)\\(\"\\)")
        (let ((beg (match-beginning 2)))
          (when beg
            (if regex
                (and (char-before beg) (eq ?# (char-before beg)) (1- beg))
              (when (not (eq ?# (char-before beg)))
                beg))))))))

(defun dreame-char-at-point ()
  "Return the char at point or nil if at buffer end."
  (when (not (= (point) (point-max)))
    (buffer-substring-no-properties (point) (1+ (point)))))

(defun dreame-char-before-point ()
  "Return the char before point or nil if at buffer beginning."
  (when (not (= (point) (point-min)))
    (buffer-substring-no-properties (point) (1- (point)))))

(defun dreame-toggle-keyword-string ()
  "Convert the string or keyword at point to keyword or string."
  (interactive)
  (let ((original-point (point)))
    (while (and (> (point) 1)
                (not (equal "\"" (buffer-substring-no-properties (point) (+ 1 (point)))))
                (not (equal ":" (buffer-substring-no-properties (point) (+ 1 (point))))))
      (backward-char))
    (cond
     ((equal 1 (point))
      (error "Beginning of file reached, this was probably a mistake"))
     ((equal "\"" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert ":" (substring (dreame-delete-and-extract-sexp) 1 -1)))
     ((equal ":" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "\"" (substring (dreame-delete-and-extract-sexp) 1) "\"")))
    (goto-char original-point)))

(defun dreame-delete-and-extract-sexp ()
  "Delete the surrounding sexp and return it."
  (let ((begin (point)))
    (forward-sexp)
    (let ((result (buffer-substring begin (point))))
      (delete-region begin (point))
      result)))



(defcustom dreame-cache-project-dir t
  "Whether to cache the results of `dreame-project-dir'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(dreame-mode . "5.8.0"))

(defvar-local dreame-cached-project-dir nil
  "A project dir cache used to speed up related operations.")

(defun dreame-project-dir (&optional dir-name)
  "Return the absolute path to the project's root directory.

Call is delegated down to `dreame-project-root-function' with
optional DIR-NAME as argument.

When `dreame-cache-project-dir' is t the results of the command
are cached in a buffer local variable (`dreame-cached-project-dir')."
  (let ((project-dir (or dreame-cached-project-dir
                         (funcall dreame-project-root-function dir-name))))
    (when (and dreame-cache-project-dir
               (derived-mode-p 'dreame-mode)
               (not dreame-cached-project-dir))
      (setq dreame-cached-project-dir project-dir))
    project-dir))

(defun dreame-current-project (&optional dir-name)
  "Return the current project as a cons cell usable by project.el.

Call is delegated down to `dreame-project-dir' with
optional DIR-NAME as argument."
  (let ((project-dir (dreame-project-dir dir-name)))
    (if project-dir
        (cons 'dreame project-dir)
      nil)))

(defun dreame-project-root-path (&optional dir-name)
  "Return the absolute path to the project's root directory.

Use `default-directory' if DIR-NAME is nil.
Return nil if not inside a project."
  (let* ((dir-name (or dir-name default-directory))
         (choices (delq nil
                        (mapcar (lambda (fname)
                                  (locate-dominating-file dir-name fname))
                                dreame-build-tool-files))))
    (when (> (length choices) 0)
      (car (sort choices #'file-in-directory-p)))))

;; project.el integration
(cl-defmethod project-roots ((project (head dreame)))
  (list (cdr project)))

(defun dreame-project-relative-path (path)
  "Denormalize PATH by making it relative to the project root."
  (file-relative-name path (dreame-project-dir)))


;;; ns manipulation
(defun dreame-expected-ns (&optional path)
  "Return the namespace matching PATH.

PATH is expected to be an absolute file path.

If PATH is nil, use the path to the file backing the current buffer."
  (let* ((path (or path (file-truename (buffer-file-name))))
         (relative (dreame-project-relative-path path))
         (sans-file-type (substring relative 0 (- (length (file-name-extension path t)))))
         (sans-file-sep (mapconcat 'identity (cdr (split-string sans-file-type "/")) "."))
         (sans-underscores (replace-regexp-in-string "_" "-" sans-file-sep)))
    ;; Drop prefix from ns for projects with structure src/{clj,cljs,cljc}
    (replace-regexp-in-string "\\`clj[scx]?\\." "" sans-underscores)))

(defun dreame-insert-ns-form-at-point ()
  "Insert a namespace form at point."
  (interactive)
  (insert (format "(ns %s)" (funcall dreame-expected-ns-function))))

(defun dreame-insert-ns-form ()
  "Insert a namespace form at the beginning of the buffer."
  (interactive)
  (widen)
  (goto-char (point-min))
  (dreame-insert-ns-form-at-point))

(defun dreame-update-ns ()
  "Update the namespace of the current buffer.
Useful if a file has been renamed."
  (interactive)
  (let ((nsname (funcall dreame-expected-ns-function)))
    (when nsname
      (save-excursion
        (save-match-data
          (if (dreame-find-ns)
              (progn
                (replace-match nsname nil nil nil 4)
                (message "ns form updated to `%s'" nsname)
                (setq dreame-cached-ns nsname))
            (user-error "Can't find ns form")))))))

(defun dreame--sort-following-sexps ()
  "Sort sexps between point and end of current sexp.
Comments at the start of a line are considered part of the
following sexp.  Comments at the end of a line (after some other
content) are considered part of the preceding sexp."
  ;; Here we're after the :require/:import symbol.
  (save-restriction
    (narrow-to-region (point) (save-excursion
                                (up-list)
                                (1- (point))))
    (skip-chars-forward "\r\n[:blank:]")
    (sort-subr nil
               (lambda () (skip-chars-forward "\r\n[:blank:]"))
               ;; Move to end of current top-level thing.
               (lambda ()
                 (condition-case nil
                     (while t (up-list))
                   (scan-error nil))
                 ;; We could be inside a symbol instead of a sexp.
                 (unless (looking-at "\\s-\\|$")
                   (dreame-forward-logical-sexp))
                 ;; move past comments at the end of the line.
                 (search-forward-regexp "$"))
               ;; Move to start of ns name.
               (lambda ()
                 (comment-forward)
                 (skip-chars-forward "[:blank:]\n\r[(")
                 (dreame-forward-logical-sexp)
                 (forward-sexp -1)
                 nil)
               ;; Move to end of ns name.
               (lambda ()
                 (dreame-forward-logical-sexp)))
    (goto-char (point-max))
    ;; Does the last line now end in a comment?
    (when (nth 4 (parse-partial-sexp (point-min) (point)))
      (insert "\n"))))

(defun dreame-sort-ns ()
  "Internally sort each sexp inside the ns form."
  (interactive)
  (comment-normalize-vars)
  (if (dreame-find-ns)
      (save-excursion
        (goto-char (match-beginning 0))
        (redisplay)
        (let ((beg (point))
              (ns))
          (forward-sexp 1)
          (setq ns (buffer-substring beg (point)))
          (forward-char -1)
          (while (progn (forward-sexp -1)
                        (looking-at "(:[a-z]"))
            (save-excursion
              (forward-char 1)
              (forward-sexp 1)
              (dreame--sort-following-sexps)))
          (goto-char beg)
          (if (looking-at (regexp-quote ns))
              (message "ns form is already sorted")
            (sleep-for 0.1)
            (redisplay)
            (message "ns form has been sorted")
            (sleep-for 0.1))))
    (user-error "Can't find ns form")))

(defconst dreame-namespace-name-regex
  (rx line-start
      "("
      (zero-or-one (group (regexp "dreame.core/")))
      (zero-or-one (submatch "in-"))
      "ns"
      (zero-or-one "+")
      (one-or-more (any whitespace "\n"))
      (zero-or-more (or (submatch (zero-or-one "#")
                                  "^{"
                                  (zero-or-more (not (any "}")))
                                  "}")
                        (zero-or-more "^:"
                                      (one-or-more (not (any whitespace)))))
                    (one-or-more (any whitespace "\n")))
      (zero-or-one (any ":'")) ;; (in-ns 'foo) or (ns+ :user)
      (group (one-or-more (not (any "()\"" whitespace))) symbol-end)))

(defcustom dreame-cache-ns nil
  "Whether to cache the results of `dreame-find-ns'.

Note that this won't work well in buffers with multiple namespace
declarations (which rarely occur in practice) and you'll
have to invalidate this manually after changing the ns for
a buffer.  If you update the ns using `dreame-update-ns'
the cached value will be updated automatically."
  :type 'boolean
  :safe #'booleanp
  :package-version '(dreame-mode . "5.8.0"))

(defvar-local dreame-cached-ns nil
  "A buffer ns cache used to speed up ns-related operations.")

(defun dreame--find-ns-in-direction (direction)
  "Return the nearest namespace in a specific DIRECTION.
DIRECTION is `forward' or `backward'."
  (let ((candidate)
        (fn (if (eq direction 'forward)
                #'search-forward-regexp
              #'search-backward-regexp)))
    (while (and (not candidate)
                (funcall fn dreame-namespace-name-regex nil t))
      (unless (or (dreame--in-string-p) (dreame--in-comment-p))
        (setq candidate (match-string-no-properties 4))))
    candidate))

(defun dreame-find-ns ()
  "Return the namespace of the current Dreame buffer.
Return the namespace closest to point and above it.  If there are
no namespaces above point, return the first one in the buffer.

The results will be cached if `dreame-cache-ns' is set to t."
  (if (and dreame-cache-ns dreame-cached-ns)
      dreame-cached-ns
    (let ((ns (save-excursion
                (save-restriction
                  (widen)

                  ;; Move to top-level to avoid searching from inside ns
                  (ignore-errors (while t (up-list nil t t)))

                  (or (dreame--find-ns-in-direction 'backward)
                      (dreame--find-ns-in-direction 'forward))))))
      (setq dreame-cached-ns ns)
      ns)))

(defun dreame-show-cache ()
  "Display cached values if present.
Useful for debugging."
  (interactive)
  (message "Cached Project: %s, Cached Namespace: %s" dreame-cached-project-dir dreame-cached-ns))

(defun dreame-clear-cache ()
  "Clear all buffer-local cached values.

Normally you'd need to do this very infrequently - e.g.
after renaming the root folder of project or after
renaming a namespace."
  (interactive)
  (setq dreame-cached-project-dir nil
        dreame-cached-ns nil)
  (message "Buffer-local dreame-mode cache cleared"))

(defconst dreame-def-type-and-name-regex
  (concat "(\\(?:\\(?:\\sw\\|\\s_\\)+/\\)?"
          ;; Declaration
          "\\(def\\(?:\\sw\\|\\s_\\)*\\)\\>"
          ;; Any whitespace
          "[ \r\n\t]*"
          ;; Possibly type or metadata
          "\\(?:#?^\\(?:{[^}]*}\\|\\(?:\\sw\\|\\s_\\)+\\)[ \r\n\t]*\\)*"
          ;; Symbol name
          "\\(\\(?:\\sw\\|\\s_\\)+\\)"))

(defun dreame-find-def ()
  "Find the var declaration macro and symbol name of the current form.
Returns a list pair, e.g. (\"defn\" \"abc\") or (\"deftest\" \"some-test\")."
  (save-excursion
    (unless (looking-at dreame-def-type-and-name-regex)
      (beginning-of-defun))
    (when (search-forward-regexp dreame-def-type-and-name-regex nil t)
      (list (match-string-no-properties 1)
            (match-string-no-properties 2)))))


;;; Sexp navigation

(defun dreame--looking-at-non-logical-sexp ()
  "Return non-nil if text after point is \"non-logical\" sexp.
\"Non-logical\" sexp are ^metadata and #reader.macros."
  (comment-normalize-vars)
  (comment-forward (point-max))
  (looking-at-p "\\^\\|#:?:?[[:alpha:]]"))

(defun dreame-forward-logical-sexp (&optional n)
  "Move forward N logical sexps.
This will skip over sexps that don't represent objects, so that ^hints and
#reader.macros are considered part of the following sexp."
  (interactive "p")
  (unless n (setq n 1))
  (if (< n 0)
      (dreame-backward-logical-sexp (- n))
    (let ((forward-sexp-function nil))
      (while (> n 0)
        (while (dreame--looking-at-non-logical-sexp)
          (forward-sexp 1))
        ;; The actual sexp
        (forward-sexp 1)
        (skip-chars-forward ",")
        (setq n (1- n))))))

(defun dreame-backward-logical-sexp (&optional n)
  "Move backward N logical sexps.
This will skip over sexps that don't represent objects, so that ^hints and
#reader.macros are considered part of the following sexp."
  (interactive "p")
  (unless n (setq n 1))
  (if (< n 0)
      (dreame-forward-logical-sexp (- n))
    (let ((forward-sexp-function nil))
      (while (> n 0)
        ;; The actual sexp
        (backward-sexp 1)
        ;; Non-logical sexps.
        (while (and (not (bobp))
                    (ignore-errors
                      (save-excursion
                        (backward-sexp 1)
                        (dreame--looking-at-non-logical-sexp))))
          (backward-sexp 1))
        (setq n (1- n))))))

(defun dreame-top-level-form-p (first-form)
  "Return truthy if the first form matches FIRST-FORM."
  (condition-case nil
      (save-excursion
        (beginning-of-defun)
        (forward-char 1)
        (dreame-forward-logical-sexp 1)
        (dreame-backward-logical-sexp 1)
        (looking-at-p first-form))
    (scan-error nil)
    (end-of-buffer nil)))

(defun dreame-sexp-starts-until-position (position)
  "Return the starting points for forms before POSITION.
Positions are in descending order to aide in finding the first starting
position before the current position."
  (save-excursion
    (let (sexp-positions)
      (condition-case nil
          (while (< (point) position)
            (dreame-forward-logical-sexp 1)
            (dreame-backward-logical-sexp 1)
            (push (point) sexp-positions)
            (dreame-forward-logical-sexp 1))
        (scan-error nil))
      sexp-positions)))

(defcustom dreame-toplevel-inside-comment-form nil
  "Eval top level forms inside comment forms instead of the comment form itself.
Experimental.  Function `cider-defun-at-point' is used extensively so if we
change this heuristic it needs to be bullet-proof and desired.  While
testing, give an easy way to turn this new behavior off."
  :type 'boolean
  :safe #'booleanp
  :package-version '(dreame-mode . "5.9.0"))

(defun dreame-find-first (pred coll)
  "Find first element of COLL for which PRED return truthy."
  (let ((found)
        (haystack coll))
    (while (and (not found)
                haystack)
      (if (funcall pred (car haystack))
          (setq found (car haystack))
        (setq haystack (cdr haystack))))
    found))

(defun dreame-beginning-of-defun-function (&optional n)
  "Go to top level form.
Set as `beginning-of-defun-function' so that these generic
operators can be used.  Given a positive N it will do it that
many times."
  (let ((beginning-of-defun-function nil))
    (if (and dreame-toplevel-inside-comment-form
             (dreame-top-level-form-p "comment"))
        (condition-case nil
            (save-match-data
              (let ((original-position (point))
                    dreame-comment-end)
                (beginning-of-defun)
                (end-of-defun)
                (setq dreame-comment-end (point))
                (beginning-of-defun)
                (forward-char 1)              ;; skip paren so we start at comment
                (dreame-forward-logical-sexp) ;; skip past the comment form itself
                (if-let ((sexp-start (dreame-find-first (lambda (beg-pos)
                                                           (< beg-pos original-position))
                                                         (dreame-sexp-starts-until-position
                                                          dreame-comment-end))))
                    (progn (goto-char sexp-start) t)
                  (beginning-of-defun n))))
          (scan-error (beginning-of-defun n)))
      (beginning-of-defun n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Refactoring support
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Threading macros related
(defcustom dreame-thread-all-but-last nil
  "Non-nil means do not thread the last expression.
This means that `dreame-thread-first-all' and
`dreame-thread-last-all' not thread the deepest sexp inside the
current sexp."
  :package-version '(dreame-mode . "5.4.0")
  :safe #'booleanp
  :type 'boolean)

(defun dreame--point-after (&rest actions)
  "Return POINT after performing ACTIONS.

An action is either the symbol of a function or a two element
list of (fn args) to pass to `apply''"
  (save-excursion
    (dolist (fn-and-args actions)
      (let ((f (if (listp fn-and-args) (car fn-and-args) fn-and-args))
            (args (if (listp fn-and-args) (cdr fn-and-args) nil)))
        (apply f args)))
    (point)))

(defun dreame--maybe-unjoin-line ()
  "Undo a `join-line' done by a threading command."
  (when (get-text-property (point) 'dreame-thread-line-joined)
    (remove-text-properties (point) (1+ (point)) '(dreame-thread-line-joined t))
    (insert "\n")))

(defun dreame--unwind-last ()
  "Unwind a thread last macro once.

Point must be between the opening paren and the ->> symbol."
  (forward-sexp)
  (save-excursion
    (let ((contents (dreame-delete-and-extract-sexp)))
      (when (looking-at " *\n")
        (join-line 'following))
      (dreame--ensure-parens-around-function-names)
      (let* ((sexp-beg-line (line-number-at-pos))
             (sexp-end-line (progn (forward-sexp)
                                   (line-number-at-pos)))
             (multiline-sexp-p (not (= sexp-beg-line sexp-end-line))))
        (down-list -1)
        (if multiline-sexp-p
            (insert "\n")
          ;; `dreame--maybe-unjoin-line' only works when unwinding sexps that were
          ;; threaded in the same Emacs session, but it also catches cases that
          ;; `multiline-sexp-p' doesn't.
          (dreame--maybe-unjoin-line))
        (insert contents))))
  (forward-char))

(defun dreame--ensure-parens-around-function-names ()
  "Insert parens around function names if necessary."
  (dreame--looking-at-non-logical-sexp)
  (unless (looking-at "(")
    (insert-parentheses 1)
    (backward-up-list)))

(defun dreame--unwind-first ()
  "Unwind a thread first macro once.

Point must be between the opening paren and the -> symbol."
  (forward-sexp)
  (save-excursion
    (let ((contents (dreame-delete-and-extract-sexp)))
      (when (looking-at " *\n")
        (join-line 'following))
      (dreame--ensure-parens-around-function-names)
      (down-list)
      (forward-sexp)
      (insert contents)
      (forward-sexp -1)
      (dreame--maybe-unjoin-line)))
  (forward-char))

(defun dreame--pop-out-of-threading ()
  "Raise a sexp up a level to unwind a threading form."
  (save-excursion
    (down-list 2)
    (backward-up-list)
    (raise-sexp)))

(defun dreame--nothing-more-to-unwind ()
  "Return non-nil if a threaded form cannot be unwound further."
  (save-excursion
    (let ((beg (point)))
      (forward-sexp)
      (down-list -1)
      (backward-sexp 2) ;; the last sexp, the threading macro
      (when (looking-back "(\\s-*" (line-beginning-position))
        (backward-up-list)) ;; and the paren
      (= beg (point)))))

(defun dreame--fix-sexp-whitespace (&optional move-out)
  "Fix whitespace after unwinding a threading form.

Optional argument MOVE-OUT, if non-nil, means moves up a list
before fixing whitespace."
  (save-excursion
    (when move-out (backward-up-list))
    (let ((sexp (bounds-of-thing-at-point 'sexp)))
      (dreame-indent-region (car sexp) (cdr sexp))
      (delete-trailing-whitespace (car sexp) (cdr sexp)))))

;;;###autoload
(defun dreame-unwind (&optional n)
  "Unwind thread at point or above point by N levels.
With universal argument \\[universal-argument], fully unwind thread."
  (interactive "P")
  (setq n (cond ((equal n '(4)) 999)
                (n) (1)))
  (save-excursion
    (let ((limit (save-excursion
                   (beginning-of-defun)
                   (point))))
      (ignore-errors
        (when (looking-at "(")
          (forward-char 1)
          (forward-sexp 1)))
      (while (> n 0)
        (search-backward-regexp "([^-]*->" limit)
        (if (dreame--nothing-more-to-unwind)
            (progn (dreame--pop-out-of-threading)
                   (dreame--fix-sexp-whitespace)
                   (setq n 0)) ;; break out of loop
          (down-list)
          (cond
           ((looking-at "[^-]*->\\_>")  (dreame--unwind-first))
           ((looking-at "[^-]*->>\\_>") (dreame--unwind-last)))
          (dreame--fix-sexp-whitespace 'move-out)
          (setq n (1- n)))))))

;;;###autoload
(defun dreame-unwind-all ()
  "Fully unwind thread at point or above point."
  (interactive)
  (dreame-unwind '(4)))

(defun dreame--remove-superfluous-parens ()
  "Remove extra parens from a form."
  (when (looking-at "([^ )]+)")
    (delete-pair)))

(defun dreame--thread-first ()
  "Thread a nested sexp using ->."
  (down-list)
  (forward-symbol 1)
  (unless (looking-at ")")
    (let ((contents (dreame-delete-and-extract-sexp)))
      (backward-up-list)
      (just-one-space 0)
      (save-excursion
        (insert contents "\n")
        (dreame--remove-superfluous-parens))
      (when (looking-at "\\s-*\n")
        (join-line 'following)
        (forward-char 1)
        (put-text-property (point) (1+ (point))
                           'dreame-thread-line-joined t))
      t)))

(defun dreame--thread-last ()
  "Thread a nested sexp using ->>."
  (forward-sexp 2)
  (down-list -1)
  (backward-sexp)
  (unless (eq (char-before) ?\()
    (let ((contents (dreame-delete-and-extract-sexp)))
      (just-one-space 0)
      (backward-up-list)
      (insert contents "\n")
      (dreame--remove-superfluous-parens)
      ;; cljr #255 Fix dangling parens
      (forward-sexp)
      (when (looking-back "^\\s-*\\()+\\)\\s-*" (line-beginning-position))
        (let ((pos (match-beginning 1)))
          (put-text-property pos (1+ pos) 'dreame-thread-line-joined t))
        (join-line))
      t)))

(defun dreame--threadable-p ()
  "Return non-nil if a form can be threaded."
  (save-excursion
    (forward-symbol 1)
    (looking-at "[\n\r\t ]*(")))

;;;###autoload
(defun dreame-thread ()
  "Thread by one more level an existing threading macro."
  (interactive)
  (ignore-errors
    (when (looking-at "(")
      (forward-char 1)
      (forward-sexp 1)))
  (search-backward-regexp "([^-]*->")
  (down-list)
  (when (dreame--threadable-p)
    (prog1 (cond
            ((looking-at "[^-]*->\\_>")  (dreame--thread-first))
            ((looking-at "[^-]*->>\\_>") (dreame--thread-last)))
      (dreame--fix-sexp-whitespace 'move-out))))

(defun dreame--thread-all (first-or-last-thread but-last)
  "Fully thread the form at point.

FIRST-OR-LAST-THREAD is \"->\" or \"->>\".

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `dreame-thread-all-but-last'."
  (save-excursion
    (insert-parentheses 1)
    (insert first-or-last-thread))
  (while (save-excursion (dreame-thread)))
  (when (or but-last dreame-thread-all-but-last)
    (dreame-unwind)))

;;;###autoload
(defun dreame-thread-first-all (but-last)
  "Fully thread the form at point using ->.

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `dreame-thread-all-but-last'."
  (interactive "P")
  (dreame--thread-all "-> " but-last))

;;;###autoload
(defun dreame-thread-last-all (but-last)
  "Fully thread the form at point using ->>.

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `dreame-thread-all-but-last'."
  (interactive "P")
  (dreame--thread-all "->> " but-last))

;;; Cycling stuff

(defcustom dreame-use-metadata-for-privacy nil
  "If nil, `dreame-cycle-privacy' will use (defn- f []).
If t, it will use (defn ^:private f [])."
  :package-version '(dreame-mode . "5.5.0")
  :safe #'booleanp
  :type 'boolean)

;;;###autoload
(defun dreame-cycle-privacy ()
  "Make public the current private def, or vice-versa.
See: https://github.com/dreame-emacs/clj-refactor.el/wiki/cljr-cycle-privacy"
  (interactive)
  (save-excursion
    (ignore-errors (forward-char 7))
    (search-backward-regexp "(defn?\\(-\\| ^:private\\)?\\_>")
    (if (match-string 1)
        (replace-match "" nil nil nil 1)
      (goto-char (match-end 0))
      (insert (if (or dreame-use-metadata-for-privacy
                      (equal (match-string 0) "(def"))
                  " ^:private"
                "-")))))

(defun dreame--convert-collection (coll-open coll-close)
  "Convert the collection at (point) by unwrapping it an wrapping it between COLL-OPEN and COLL-CLOSE."
  (save-excursion
    (while (and
            (not (bobp))
            (not (looking-at "(\\|{\\|\\[")))
      (backward-char))
    (when (or (eq ?\# (char-before))
              (eq ?\' (char-before)))
      (delete-char -1))
    (when (and (bobp)
               (not (memq (char-after) '(?\{ ?\( ?\[))))
      (user-error "Beginning of file reached, collection is not found"))
    (insert coll-open (substring (dreame-delete-and-extract-sexp) 1 -1) coll-close)))

;;;###autoload
(defun dreame-convert-collection-to-list ()
  "Convert collection at (point) to list."
  (interactive)
  (dreame--convert-collection "(" ")"))

;;;###autoload
(defun dreame-convert-collection-to-quoted-list ()
  "Convert collection at (point) to quoted list."
  (interactive)
  (dreame--convert-collection "'(" ")"))

;;;###autoload
(defun dreame-convert-collection-to-map ()
  "Convert collection at (point) to map."
  (interactive)
  (dreame--convert-collection "{" "}"))

;;;###autoload
(defun dreame-convert-collection-to-vector ()
  "Convert collection at (point) to vector."
  (interactive)
  (dreame--convert-collection "[" "]"))

;;;###autoload
(defun dreame-convert-collection-to-set ()
  "Convert collection at (point) to set."
  (interactive)
  (dreame--convert-collection "#{" "}"))

(defun dreame--in-string-p ()
  "Check whether the point is currently in a string."
  (nth 3 (syntax-ppss)))

(defun dreame--in-comment-p ()
  "Check whether the point is currently in a comment."
  (nth 4 (syntax-ppss)))

(defun dreame--goto-if ()
  "Find the first surrounding if or if-not expression."
  (when (dreame--in-string-p)
    (while (or (not (looking-at "("))
               (dreame--in-string-p))
      (backward-char)))
  (while (not (looking-at "\\((if \\)\\|\\((if-not \\)"))
    (condition-case nil
        (backward-up-list)
      (scan-error (user-error "No if or if-not found")))))

;;;###autoload
(defun dreame-cycle-if ()
  "Change a surrounding if to if-not, or vice-versa.

See: https://github.com/dreame-emacs/clj-refactor.el/wiki/cljr-cycle-if"
  (interactive)
  (save-excursion
    (dreame--goto-if)
    (cond
     ((looking-at "(if-not")
      (forward-char 3)
      (delete-char 4)
      (forward-sexp 2)
      (transpose-sexps 1))
     ((looking-at "(if")
      (forward-char 3)
      (insert "-not")
      (forward-sexp 2)
      (transpose-sexps 1)))))

;; TODO: Remove code duplication with `dreame--goto-if'.
(defun dreame--goto-when ()
  "Find the first surrounding when or when-not expression."
  (when (dreame--in-string-p)
    (while (or (not (looking-at "("))
               (dreame--in-string-p))
      (backward-char)))
  (while (not (looking-at "\\((when \\)\\|\\((when-not \\)"))
    (condition-case nil
        (backward-up-list)
      (scan-error (user-error "No when or when-not found")))))

;;;###autoload
(defun dreame-cycle-when ()
  "Change a surrounding when to when-not, or vice-versa."
  (interactive)
  (save-excursion
    (dreame--goto-when)
    (cond
     ((looking-at "(when-not")
      (forward-char 9)
      (delete-char -4))
     ((looking-at "(when")
      (forward-char 5)
      (insert "-not")))))

(defun dreame-cycle-not ()
  "Add or remove a not form around the current form."
  (interactive)
  (save-excursion
    (condition-case nil
        (backward-up-list)
      (scan-error (user-error "`dreame-cycle-not' must be invoked inside a list")))
    (if (looking-back "(not " nil)
        (progn
          (delete-char -5)
          (forward-sexp)
          (delete-char 1))
      (insert "(not ")
      (forward-sexp)
      (insert ")"))))

;;; let related stuff

(defvar dreame--let-regexp
  "\(\\(when-let\\|if-let\\|let\\)\\(\\s-*\\|\\[\\)"
  "Regexp matching let like expressions, i.e. \"let\", \"when-let\", \"if-let\".

The first match-group is the let expression.

The second match-group is the whitespace or the opening square
bracket if no whitespace between the let expression and the
bracket.")

(defun dreame--goto-let ()
  "Go to the beginning of the nearest let form."
  (when (dreame--in-string-p)
    (while (or (not (looking-at "("))
               (dreame--in-string-p))
      (backward-char)))
  (ignore-errors
    (while (not (looking-at dreame--let-regexp))
      (backward-up-list)))
  (looking-at dreame--let-regexp))

(defun dreame--inside-let-binding-p ()
  "Return non-nil if point is inside a let binding."
  (ignore-errors
    (save-excursion
      (let ((pos (point)))
        (dreame--goto-let)
        (re-search-forward "\\[")
        (if (< pos (point))
            nil
          (forward-sexp)
          (up-list)
          (< pos (point)))))))

(defun dreame--beginning-of-current-let-binding ()
  "Move before the bound name of the current binding.
Assume that point is in the binding form of a let."
  (let ((current-point (point)))
    (dreame--goto-let)
    (search-forward "[")
    (forward-char)
    (while (> current-point (point))
      (forward-sexp))
    (backward-sexp 2)))

(defun dreame--previous-line ()
  "Keep the column position while go the previous line."
  (let ((col (current-column)))
    (forward-line -1)
    (move-to-column col)))

(defun dreame--prepare-to-insert-new-let-binding ()
  "Move to right place in the let form to insert a new binding and indent."
  (if (dreame--inside-let-binding-p)
      (progn
        (dreame--beginning-of-current-let-binding)
        (newline-and-indent)
        (dreame--previous-line)
        (indent-for-tab-command))
    (dreame--goto-let)
    (search-forward "[")
    (backward-up-list)
    (forward-sexp)
    (down-list -1)
    (backward-char)
    (if (looking-at "\\[\\s-*\\]")
        (forward-char)
      (forward-char)
      (newline-and-indent))))

(defun dreame--sexp-regexp (sexp)
  "Return a regexp for matching SEXP."
  (concat "\\([^[:word:]^-]\\)"
          (mapconcat #'identity (mapcar 'regexp-quote (split-string sexp))
                     "[[:space:]\n\r]+")
          "\\([^[:word:]^-]\\)"))

(defun dreame--replace-sexp-with-binding (bound-name init-expr)
  "Replace a binding with its bound name in the let form.

BOUND-NAME is the name (left-hand side) of a binding.

INIT-EXPR is the value (right-hand side) of a binding."
  (save-excursion
    (while (re-search-forward
            (dreame--sexp-regexp init-expr)
            (dreame--point-after 'dreame--goto-let 'forward-sexp)
            t)
      (replace-match (concat "\\1" bound-name "\\2")))))

(defun dreame--replace-sexps-with-bindings (bindings)
  "Replace bindings with their respective bound names in the let form.

BINDINGS is the list of bound names and init expressions."
  (let ((bound-name (pop bindings))
        (init-expr (pop bindings)))
    (when bound-name
      (dreame--replace-sexp-with-binding bound-name init-expr)
      (dreame--replace-sexps-with-bindings bindings))))

(defun dreame--replace-sexps-with-bindings-and-indent ()
  "Replace sexps with bindings."
  (dreame--replace-sexps-with-bindings
   (dreame--read-let-bindings))
  (dreame-indent-region
   (dreame--point-after 'dreame--goto-let)
   (dreame--point-after 'dreame--goto-let 'forward-sexp)))

(defun dreame--read-let-bindings ()
  "Read the bound-name and init expression pairs in the binding form.
Return a list: odd elements are bound names, even elements init expressions."
  (dreame--goto-let)
  (down-list 2)
  (let* ((start (point))
         (sexp-start start)
         (end (save-excursion
                (backward-char)
                (forward-sexp)
                (down-list -1)
                (point)))
         bindings)
    (while (/= sexp-start end)
      (forward-sexp)
      (push
       (string-trim (buffer-substring-no-properties sexp-start (point)))
       bindings)
      (skip-chars-forward "\r\n\t[:blank:]")
      (setq sexp-start (point)))
    (nreverse bindings)))

(defun dreame--introduce-let-internal (name &optional n)
  "Create a let form, binding the form at point with NAME.

Optional numeric argument N, if non-nil, introduces the let N
lists up."
  (if (numberp n)
      (let ((init-expr-sexp (dreame-delete-and-extract-sexp)))
        (insert name)
        (ignore-errors (backward-up-list n))
        (insert "(let" (dreame-delete-and-extract-sexp) ")")
        (backward-sexp)
        (down-list)
        (forward-sexp)
        (insert " [" name " " init-expr-sexp "]\n")
        (dreame--replace-sexps-with-bindings-and-indent))
    (insert "[ " (dreame-delete-and-extract-sexp) "]")
    (backward-sexp)
    (insert "(let " (dreame-delete-and-extract-sexp) ")")
    (backward-sexp)
    (down-list 2)
    (insert name)
    (forward-sexp)
    (up-list)
    (newline-and-indent)
    (insert name)))

(defun dreame--move-to-let-internal (name)
  "Bind the form at point to NAME in the nearest let."
  (if (not (save-excursion (dreame--goto-let)))
      (dreame--introduce-let-internal name)
    (let ((contents (dreame-delete-and-extract-sexp)))
      (insert name)
      (dreame--prepare-to-insert-new-let-binding)
      (insert contents)
      (backward-sexp)
      (insert " ")
      (backward-char)
      (insert name)
      (dreame--replace-sexps-with-bindings-and-indent))))

(defun dreame--let-backward-slurp-sexp-internal ()
  "Slurp the s-expression before the let form into the let form."
  (dreame--goto-let)
  (backward-sexp)
  (let ((sexp (string-trim (dreame-delete-and-extract-sexp))))
    (delete-blank-lines)
    (down-list)
    (forward-sexp 2)
    (newline-and-indent)
    (insert sexp)
    (dreame--replace-sexps-with-bindings-and-indent)))

(defun dreame--rename-ns-alias-internal (current-alias new-alias)
  "Rename a namespace alias CURRENT-ALIAS to NEW-ALIAS."
  (dreame--find-ns-in-direction 'backward)
  (let ((rgx (concat ":as +" current-alias))
        (bound (save-excursion (forward-list 1) (point))))
    (when (search-forward-regexp rgx bound t)
      (replace-match (concat ":as " new-alias))
      (save-excursion
        (while (re-search-forward (concat current-alias "/") nil t)
          (when (not (nth 3 (syntax-ppss)))
            (replace-match (concat new-alias "/")))))
      (save-excursion
        (while (re-search-forward (concat "#::" current-alias "{") nil t)
          (replace-match (concat "#::" new-alias "{"))))
      (message "Successfully renamed alias '%s' to '%s'" current-alias new-alias))))

;;;###autoload
(defun dreame-let-backward-slurp-sexp (&optional n)
  "Slurp the s-expression before the let form into the let form.
With a numeric prefix argument slurp the previous N s-expressions
into the let form."
  (interactive "p")
  (let ((n (or n 1)))
    (dotimes (_ n)
      (save-excursion (dreame--let-backward-slurp-sexp-internal)))))

(defun dreame--let-forward-slurp-sexp-internal ()
  "Slurp the next s-expression after the let form into the let form."
  (dreame--goto-let)
  (forward-sexp)
  (let ((sexp (string-trim (dreame-delete-and-extract-sexp))))
    (down-list -1)
    (newline-and-indent)
    (insert sexp)
    (dreame--replace-sexps-with-bindings-and-indent)))

;;;###autoload
(defun dreame-let-forward-slurp-sexp (&optional n)
  "Slurp the next s-expression after the let form into the let form.
With a numeric prefix argument slurp the next N s-expressions
into the let form."
  (interactive "p")
  (unless n (setq n 1))
  (dotimes (_ n)
    (save-excursion (dreame--let-forward-slurp-sexp-internal))))

;;;###autoload
(defun dreame-introduce-let (&optional n)
  "Create a let form, binding the form at point.
With a numeric prefix argument the let is introduced N lists up."
  (interactive "P")
  (dreame--introduce-let-internal (read-from-minibuffer "Name of bound symbol: ") n))

;;;###autoload
(defun dreame-move-to-let ()
  "Move the form at point to a binding in the nearest let."
  (interactive)
  (dreame--move-to-let-internal (read-from-minibuffer "Name of bound symbol: ")))

;;;###autoload
(defun dreame-rename-ns-alias ()
  "Rename a namespace alias."
  (interactive)
  (let ((current-alias (read-from-minibuffer "Current alias: ")))
    (save-excursion
      (dreame--find-ns-in-direction 'backward)
      (let ((rgx (concat ":as +" current-alias))
            (bound (save-excursion (forward-list 1) (point))))
        (if (save-excursion (search-forward-regexp rgx bound t))
            (let ((new-alias (read-from-minibuffer "New alias: ")))
              (dreame--rename-ns-alias-internal current-alias new-alias))
          (message "Cannot find namespace alias: '%s'" current-alias))))))

(defun dreame--add-arity-defprotocol-internal ()
  "Add an arity to a signature inside a defprotocol.

Assumes cursor is at beginning of signature."
  (re-search-forward "\\[")
  (save-excursion (insert "] [")))

(defun dreame--add-arity-reify-internal ()
  "Add an arity to a function inside a reify.

Assumes cursor is at beginning of function."
  (re-search-forward "\\(\\w+ \\)")
  (insert "[")
  (save-excursion (insert "])\n(" (match-string 0))))

(defun dreame--add-arity-internal ()
  "Add an arity to a function.

Assumes cursor is at beginning of function."
  (let ((beg-line (what-line))
        (end (save-excursion (forward-sexp)
                             (point))))
    (down-list 2)
    (when (looking-back "{" 1) ;; skip metadata if present
      (up-list)
      (down-list))
    (cond
     ((looking-back "(" 1) ;; multi-arity fn
      (insert "[")
      (save-excursion (insert "])\n(")))
     ((looking-back "\\[" 1)  ;; single-arity fn
      (let* ((same-line (string= beg-line (what-line)))
             (new-arity-text (concat (when same-line "\n") "([")))
        (save-excursion
          (goto-char end)
          (insert ")"))

        (re-search-backward " +\\[")
        (replace-match new-arity-text)
        (save-excursion (insert "])\n([")))))))

;;;###autoload
(defun dreame-add-arity ()
  "Add an arity to a function."
  (interactive)
  (let ((original-pos (point))
        (n 0))
    (while (not (looking-at-p "(\\(defn\\|letfn\\|fn\\|defmacro\\|defmethod\\|defprotocol\\|reify\\|proxy\\)"))
      (setq n (1+ n))
      (backward-up-list 1 t))
    (let ((beg (point))
          (end-marker (make-marker))
          (end (save-excursion (forward-sexp)
                               (point)))
          (jump-up (lambda (x)
                     (goto-char original-pos)
                     (backward-up-list x t))))
      (set-marker end-marker end)
      (cond
       ((looking-at-p "(\\(defn\\|fn\\|defmethod\\|defmacro\\)")
        (dreame--add-arity-internal))
       ((looking-at-p "(letfn")
        (funcall jump-up (- n 2))
        (dreame--add-arity-internal))
       ((looking-at-p "(proxy")
        (funcall jump-up (- n 1))
        (dreame--add-arity-internal))
       ((looking-at-p "(defprotocol")
        (funcall jump-up (- n 1))
        (dreame--add-arity-defprotocol-internal))
       ((looking-at-p "(reify")
        (funcall jump-up (- n 1))
        (dreame--add-arity-reify-internal)))
      (indent-region beg end-marker))))



;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.dreame\\'" . dreame-mode)))

(provide 'dreame-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; dreame-mode.el ends here
