;;;; init.el --- Emacs configuration

;;; INSTALL PACKAGES
;;; --------------------------------------------

(require 'package)

;; NOTE(roger): Add MELPA repository
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(package-initialize)
;; NOTE(roger): Update the packages that are not up to date
(when (not package-archive-contents)
  (package-refresh-contents))

;; NOTE(roger): Names of the packages needed from the repositories
(setq package-list '(gruvbox-theme
                     magit
                     fill-column-indicator))
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
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
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

;;; GLOBAL MACROS
;;; --------------------------------------------

;; NOTE(roger): "C-c d" duplicates the line
(defun duplicate-line()
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

;;; PROGRAMMING CUSTOMIZATION
;;; --------------------------------------------

;; NOTE(roger): List of programming modes
(defvar programming-modes '(c++-mode c-mode emacs-lisp-mode sql-mode java-mode nxml-mode python-mode))

;; NOTE(roger): Activate 80 Column line in certain modes
;; TODO(roger): Add more modes and try to use a variable instead of repeating the sentence
(add-hook 'java-mode-hook 'fci-mode)
(add-hook 'python-mode-hook 'fci-mode)
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

;;; XML MODE
;;; --------------------------------------------

;; NOTE(roger): Macro which tabs the document
(add-hook 'nxml-mode-hook
  (lambda ()
    (local-set-key (kbd "<C-tab>") (kbd "C-x h <tab>"))))

;;; SQL MODE
;;; --------------------------------------------

;; NOTE(roger): Sets the product to MySQL
(add-hook 'sql-mode-hook
  (lambda()
    (sql-set-product "mysql")))