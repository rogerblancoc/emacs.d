;;;; init.el --- Emacs configuration

;;; INSTALL PACKAGES
;;; --------------------------------------------

(require 'package)

;; NOTE(roger): Add MELPA repository
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
;; NOTE(roger): Update the packages that are not up to date
(when (not package-archive-contents)
  (package-refresh-contents))

;; NOTE(roger): Names of the packages needed from the repositories
(setq package-list '(gruvbox-theme
                     php-mode
                     web-mode
                     magit
                     diff-hl
                     hungry-delete
                     py-autopep8
                     dired-sidebar
                     auctex
                     buffer-move
                     browse-kill-ring
                     markdown-mode
                     flycheck
                     company
                     ))

;; NOTE(roger): Installs all the packages that are not installed yet
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; NOTE(roger): Tell Emacs where my custom.el file is
(setq custom-file "~/.config/emacs/lisp/custom.el")

;;; BASIC CUSTOMIZATION
;;; --------------------------------------------

;; NOTE(roger): Load custom.el file if exists
(when (file-exists-p custom-file)
  (load custom-file))

;; NOTE(roger): Force Emacs to use UTF-8 encoding by default
;; TODO(roger): Check if this all of this is necesary
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; NOTE(roger): Maximize the screen at start-up
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; NOTE(roger): Remove the menu, tool and the scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
;; NOTE(roger): Hide the start-up message
(setq inhibit-startup-message t)
;; NOTE(roger): Load Gruvbox theme
(load-theme 'gruvbox-dark-hard' t)
;; NOTE(roger): Enable line numbers globally
(global-linum-mode t)
;; NOTE(roger): Enable column numbers globally
(setq column-number-mode t)
;; NOTE(roger): Highlights the matching parentheses
(show-paren-mode 1)
;; NOTE(roger): Instead of tabs use spaces
(setq-default indent-tabs-mode nil)
;; NOTE(roger): Automatically add a new line at the end of the file
(setq require-final-newline t)
;; NOTE(roger): Set default font
(set-face-attribute 'default nil :font "Hack Nerd Font-11")
;; NOTE(roger): Before saving a document clean-up the whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)
;; NOTE(roger): Disable bell alarm
(setq visible-bell t)
;; NOTE(roger): Highlight the current line
(global-hl-line-mode 1)
;; NOTE(roger): Better buffer autocomplete
(ido-mode t)
(setq ido-enable-flex-matching t)

;; NOTE(roger): Better buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; NOTE(roger): Store backup files (~file_name) in "backups" folder
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; NOTE(roger): Purcell fun scratch message
(setq-default initial-scratch-message
              (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

;; NOTE(roger): Activated macros for converting to Lower or Upper case the region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; NOTE(roger): Enable mode for stopping in between camelCase words
(global-subword-mode 1)

;; NOTE(roger): Enable mode for inserting the right bracket automatically
(electric-pair-mode 1)

;;; GLOBAL MACROS
;;; --------------------------------------------

;; NOTE(roger): Toggle between truncated lines and continuation lines
(global-set-key (kbd "C-c $") 'toggle-truncate-lines)

;; NOTE(roger): Render html code in the current buffer
(global-set-key (kbd "C-c h") 'shr-render-buffer)

;; NOTE(roger): "C-c d" duplicates the line
;; TODO(roger): Be able to duplicate multiple lines
(defun duplicate-line()
  "Duplicates the current line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)1
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )
(global-set-key (kbd "C-c d") 'duplicate-line)

;; NOTE(roger): Replacement for the default buffer menu
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-hungry-delete-mode)
(global-set-key (kbd "C-c <backspace>") 'hungry-delete-backward)
(global-set-key (kbd "C-c <deletechar>") 'hungry-delete-forward)

;; NOTE(roger): Swap keybindings for searching
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; NOTE(roger): Swap keybindings for replacing
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)

;; NOTE(roger): Move the current line UP or DOWN
;; TODO(roger): Be able to move multiple lines
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(global-set-key [(control shift up)]  'move-line-up)

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(control shift down)]  'move-line-down)

;; NOTE(roger): Toggles tree view of project directory
(global-set-key (kbd "C-x t") 'dired-sidebar-toggle-sidebar)

;; NOTE(roger): Shift arround the buffers with the arrow keys
(global-set-key (kbd "C-c <up>") 'buf-move-up)
(global-set-key (kbd "C-c <down>") 'buf-move-down)
(global-set-key (kbd "C-c <left>") 'buf-move-left)
(global-set-key (kbd "C-c <right>") 'buf-move-right)

;; NOTE(roger): open new buffer with list of past killed rings
;; TODO(roger): find a more up to date package
(global-set-key (kbd "C-c y") 'browse-kill-ring)

;; NOTE(roger): Disabled prior, next, home and end keys and ctrl combinations
(global-unset-key (kbd "<prior>"))
(global-unset-key (kbd "<next>"))
(global-unset-key (kbd "<home>"))
(global-unset-key (kbd "<end>"))

(global-unset-key (kbd "<C-prior>"))
(global-unset-key (kbd "<C-next>"))
(global-unset-key (kbd "<C-home>"))
(global-unset-key (kbd "<C-end>"))

;;; PROGRAMMING CUSTOMIZATION
;;; --------------------------------------------

;; NOTE(roger): List of programming modes
(defvar programming-modes '(emacs-lisp-mode sql-mode java-mode php-mode web-mode python-mode c-mode js-mode))

;; NOTE(roger): Activate 80 Column line in certain modes
;; TODO(roger): Add more modes and try to use a variable instead of repeating the sentence
(add-hook 'java-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'php-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'c-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'js-mode-hook 'display-fill-column-indicator-mode)

;; NOTE(roger): Casey Muratori todo, note and imporant comment
(setq fixme-modes programming-modes)
(make-face 'font-lock-note-face)
(make-face 'font-lock-todo-face)
(make-face 'font-lock-docs-face)
(make-face 'font-lock-important-face)
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(
           ("\\<\\(NOTE\\)(\\w+):" 1 'font-lock-note-face t)
           ("\\<\\(TODO\\)(\\w+):" 1 'font-lock-todo-face t)
           ("\\<\\(DOCS\\)(\\w+):" 1 'font-lock-docs-face t)
           ("\\<\\(IMPORTANT\\)(\\w+):" 1 'font-lock-important-face t)
           )))
      fixme-modes)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)
(modify-face 'font-lock-todo-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-docs-face "Medium Blue" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face "Yellow" nil nil t nil t nil nil)

;; NOTE(roger): Macro which properly indents the buffer
(defun indent-buffer ()
  "Properly indents the buffer"
  (interactive)
  (local-set-key (kbd "<C-tab>") (kbd "C-x h <tab>")))

(add-hook 'emacs-lisp-mode-hook 'indent-buffer)
(add-hook 'sql-mode-hook 'indent-buffer)
(add-hook 'java-mode-hook 'indent-buffer)
(add-hook 'php-mode-hook 'indent-buffer)
(add-hook 'web-mode-hook 'indent-buffer)
(add-hook 'c-mode-hook 'indent-buffer)
(add-hook 'js-mode-hook 'indent-buffer)

;; NOTE(roger): Autoinsert template when creating a file with specific extension
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/templates/")
(setq auto-insert-query nil)

(define-auto-insert "\.tex" "latex-template.tex") ; Template for LaTeX files
(define-auto-insert "\.php" "php-template.php") ; Template for PHP files

;; NOTE(roger): Enable Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; NOTE(roger): Enable Company Mode
(add-hook 'after-init-hook 'global-company-mode)

;;; SQL MODE
;;; --------------------------------------------

;; NOTE(roger): Sets the product to MySQL
(add-hook 'sql-mode-hook
          (lambda()
            (sql-set-product "mysql")))

;;; MAGIT MODE
;;; --------------------------------------------

;; TODO(roger): Auto start magit automatically of somesorts
(global-set-key (kbd "C-x g") 'magit-status)

(add-hook 'magit-mode-hook 'global-diff-hl-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(setq magit-git-executable "git")

;;; WEB MODE
;;; --------------------------------------------

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

(setq web-mode-engines-alist '(("smarty" . "\\.tpl\\'")))

(setq web-mode-markup-indent-offset 2) ; web-mode, html tag in html file
(setq web-mode-css-indent-offset 2) ; web-mode, css in html file
(setq web-mode-code-indent-offset 2) ; web-mode, js code in html file
(setq web-mode-indent-style 2)

;; NOTE(roger): set indentation level to 2 for js-mode
(setq js-indent-level 2)

;; NOTE(roger): Grep an expression inside specific file extensions
(defun web-grep (expression)
  "Search recursively an expression inside files related to web dev"
  (interactive "sExpression to search: ")
  (grep (concat "grep -rniE '" expression "' --include \\*.php --include \\*.tpl --include \\*.js --include \\*.css .")))

(defun super-grep (expression)
  "Search recursively an expression"
  (interactive "sExpression to search: ")
  (grep (concat "grep -rniE '" expression "' .")))

;;; PHP MODE
;;; --------------------------------------------

;; NOTE(roger): due to some bug with web-mode some .php file is not getting the correct mode, so I'm forcing it
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; NOTE(roger): When pressing F1 while standing on a symbol quickly look up in the documentation online the meaning
(add-hook 'php-mode-hook 'my-php-mode-stuff)

(defun my-php-mode-stuff ()
  (local-set-key (kbd "C-c f") 'my-php-symbol-lookup))

(defun my-php-symbol-lookup ()
  (interactive)
  (let ((symbol (symbol-at-point)))
    (if (not symbol)
        (message "No symbol at point.")

      (browse-url (concat "http://php.net/manual-lookup.php?pattern="
                          (symbol-name symbol))))))

;;; PYTHON MODE
;;; --------------------------------------------

;; TODO(roger): configure flycheck or another package to check syntaxis
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;;; AUCTEX MODE
;;; --------------------------------------------

;; TODO(roger): check if it's necesary for Linux, on windows I'm certain.
(if (eq system-type 'gnu/linux)
    (setq TeX-view-program-selection '((output-pdf "Zathura"))))

;; NOTE(roger): Bind TeX-command-master (C-c C-c) to always execute LaTeX command
;; TODO(roger): Define a new wrapper function so set this variable dynamically to
;; be able to use the other options in the original command like this answer suggests
;; (https://stackoverflow.com/q/14664829), although the other options are not used
(setq TeX-command-force "LaTeX")

;; NOTE(roger): Save file automatically before compiling
(setq TeX-save-query nil)

;;; MARKDOWN MODE
;;; --------------------------------------------
(setq markdown-command
      (concat
       "pandoc"
       " --from=markdown --to=html")) ;; https://leanpub.com/markdown-mode/read#configuring-markdown
