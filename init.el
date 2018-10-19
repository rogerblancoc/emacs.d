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
                     fill-column-indicator
                     py-autopep8))
;; NOTE(roger): Installs all the packages that are not installed yet
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; NOTE(roger): Tell Emacs where my lisp dir is and load the packages
(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq custom-file "~/.emacs.d/lisp/custom.el")

;;; BASIC CUSTOMIZATION
;;; --------------------------------------------

;; NOTE(roger): Load lisp files
(load custom-file)

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
(load-theme 'gruvbox' t)
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
(set-face-attribute 'default nil :font "Inconsolata-11")
;; NOTE(roger): Before saving a document clean-up the whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)
;; NOTE(roger): Disable bell alarm
(setq visible-bell t)
;; NOTE(roger): Highlight the current line
(global-hl-line-mode 1)
;; NOTE(roger): Better buffer autocomplete
(ido-mode t)
(setq ido-enable-flex-matching t)
;; NOTE(roger): Disable truncate lines and enable continuation lines
(setq-default truncate-lines nil)

;; NOTE(roger): Better buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; NOTE(roger): Store backup files (~file_name) in "backups" folder
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; NOTE(roger): Purcell fun scratch message
(setq-default initial-scratch-message
              (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

;;; GLOBAL MACROS
;;; --------------------------------------------

;; NOTE(roger): Toggle between truncated lines and continuation lines
(global-set-key (kbd "C-c $") 'toggle-truncate-lines)

;; NOTE(roger): Render html code in the current buffer
(global-set-key (kbd "C-c h") 'shr-render-buffer)

;; NOTE(roger): "C-c d" duplicates the line
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

;; NOTE(roger): Move the current line UP or DOWN
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

;;; PROGRAMMING CUSTOMIZATION
;;; --------------------------------------------

;; NOTE(roger): List of programming modes
(defvar programming-modes '(emacs-lisp-mode sql-mode java-mode php-mode web-mode))

;; NOTE(roger): Activate 80 Column line in certain modes
;; TODO(roger): Add more modes and try to use a variable instead of repeating the sentence
(add-hook 'java-mode-hook 'fci-mode)
(add-hook 'php-mode-hook 'fci-mode)
(setq fci-rule-column 80)

;; NOTE(roger): Casey Muratori todo, note and imporant comment
(setq fixme-modes programming-modes)
(make-face 'font-lock-todo-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\)(\\w+):" 1 'font-lock-todo-face t)
           ("\\<\\(IMPORTANT\\)(\\w+):" 1 'font-lock-important-face t)
           ("\\<\\(NOTE\\)(\\w+):" 1 'font-lock-note-face t))))
      fixme-modes)
(modify-face 'font-lock-todo-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

;; NOTE(roger): Macro which tabs the document
;; TODO(roger): Do it in one function
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "<C-tab>") (kbd "C-x h <tab>"))))
(add-hook 'sql-mode-hook
          (lambda ()
            (local-set-key (kbd "<C-tab>") (kbd "C-x h <tab>"))))
(add-hook 'java-mode-hook
          (lambda ()
            (local-set-key (kbd "<C-tab>") (kbd "C-x h <tab>"))))
(add-hook 'php-mode-hook
          (lambda ()
            (local-set-key (kbd "<C-tab>") (kbd "C-x h <tab>"))))
(add-hook 'web-mode-hook
          (lambda ()
            (local-set-key (kbd "<C-tab>") (kbd "C-x h <tab>"))))

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
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; NOTE(roger): Grep an expression inside PHP and TPL files
;; TODO(roger): Add other similar functions for diferent types of files
(defun php-grep (expression)
  "Search recursively an expression inside files with .php and .tpl extension"
  (interactive "sExpression to search: ")
  (grep (concat "grep -rni '" expression "' --include \\*.php --include \\*.tpl .")))

;;; PYTHON MODE
;;; --------------------------------------------

(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
