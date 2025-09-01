(setq custom-file "~/.emacs.custom.el")

(load-file custom-file)
(require 'package)
(load-file "~/.emacs.rc/rc.el")

;; set the path for emacs bin folder
(setenv "PATH" (concat "D:/Softwares/emacs-30.1/bin;" (getenv "PATH")))
(add-to-list 'exec-path "D:/Softwares/emacs-30.1/bin")

;; disable the default behavior
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)

(setq default-directory "D:/github/")
;;(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; to start emacs in fullscreen

;; Move Text
(rc/require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;; smex
(rc/require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; ido
(rc/require 'smex 'ido-completing-read+)
(require 'ido-completing-read+)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

;; appearance
(global-display-line-numbers-mode)
;;(setq display-line-numbers-type 'relative) ; relative line numbers

(rc/require-theme 'gruber-darker)
;;(rc/require-theme 'constant)
;;(rc/require-theme 'naysayer)
(set-face-attribute 'default nil
		    :family "Consolas"
		    :height 120
		    :weight 'normal) ; font

;; *** plugins ***
(add-to-list 'load-path "~/.emacs.local/")

(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

(require 'tasm-mode)
(add-to-list 'auto-mode-alist '("\\.tasm\\'" . tasm-mode))

;; certain other modes to include
(rc/require
 'yaml-mode
 'lua-mode
 'less-css-mode
 'clojure-mode
 'cmake-mode
 'rust-mode
 'csharp-mode
 'markdown-mode
 'kotlin-mode
 'go-mode
 'php-mode
 'typescript-mode
 )

(add-to-list 'auto-mode-alist '("\\.csx\\'" . csharp-mode))

(rc/require 'powershell)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))
(add-to-list 'auto-mode-alist '("\\.psm1\\'" . powershell-mode))


;; multiple cursors
(rc/require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)


;; Whitespace mode
(defun rc/set-up-whitespace-handling ()
  (interactive)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(add-hook 'c++-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'c-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'simpc-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode 'rc/set-up-whitespace-handling)
(add-hook 'java-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'lua-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'rust-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'markdown-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'python-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'asm-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'go-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'yaml-mode-hook 'rc/set-up-whitespace-handling)

;; magit
(rc/require 'cl-lib)
(rc/require 'magit)

(setq magit-auto-revert-mode nil)

(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m l") 'magit-log)

;; word-wrap
(defun rc/enable-word-wrap ()
  (interactive)
  (toggle-word-wrap 1))

(add-hook 'markdown-mode-hook 'rc/enable-word-wrap)

;; Company (complete-any)
(rc/require 'company)
(require 'company)

(global-company-mode)

(add-hook 'tuareg-mode-hook
          (lambda ()
            (interactive)
            (company-mode 0)))

;; cycle through buffers (skipping internal buffers)
(defun my/next-user-buffer ()
  "Switch to next user buffer."
  (interactive)
  (next-buffer)
  (while (string-match-p "^\\*" (buffer-name))
    (next-buffer)))

(defun my/previous-user-buffer ()
  "Switch to previous user buffer."
  (interactive)
  (previous-buffer)
  (while (string-match-p "^\\*" (buffer-name))
    (previous-buffer)))

(global-set-key (kbd "<f7>") 'my/previous-user-buffer)
(global-set-key (kbd "<f8>") 'my/next-user-buffer)

;; generate project tags (for using etags commands like M-., M-,, etc)
;; creates a filelist.txt and TAGS file at the path specified
(defun generate-project-tags (directory)
  "Prompt for a project directory and generate TAGS file for all files in it (PowerShell version)."
  (interactive "DSelect project root: ")
  (let ((default-directory (expand-file-name directory))
        (ps-command "Get-ChildItem -Recurse -Filter *.* | ForEach-Object { $_.FullName } > filelist.txt; Get-Content filelist.txt | etags -o TAGS -"))
    (shell-command (concat "powershell -Command \"" ps-command "\""))
    (message "TAGS generated in %s" default-directory)))
