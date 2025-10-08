(setq custom-file "~/.emacs.custom.el")

(load-file custom-file)
(require 'package)
(load-file "~/.emacs.rc/rc.el")

;; set the path for emacs bin folder
(setenv "PATH" (concat "D:/softwares/emacs/emacs-30.2/bin;" (getenv "PATH")))
(add-to-list 'exec-path "D:/softwares/emacs/emacs-30.2/bin")

;; disable the default behavior
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)

(setq default-directory "D:/github/")
;;(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; to start emacs in fullscreen

;; set opacity
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

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

;;(rc/require-theme 'gruber-darker)
;;(rc/require-theme 'constant)
;;(rc/require-theme 'naysayer)

(rc/require 'doom-themes)

(setq doom-themes-enable-bold nil
      doom-themes-enable-italic nil)

(load-theme 'doom-dark+ t)

;;(set-face-attribute 'font-lock-preprocessor-face nil
;;                   :foreground "#d9d2e9")

(set-face-attribute 'default nil
		    :family "Consolas"
		    :height 110
		    :weight 'normal) ; font

;; *** plugins ***
(add-to-list 'load-path "~/.emacs.local/")

(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.c\\'" . simpc-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;;(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

(require 'simpcpp-mode)
(add-hook 'c++-mode-hook #'simpcpp-indent-mode)

(require 'tasm-mode)
(add-to-list 'auto-mode-alist '("\\.tasm\\'" . tasm-mode))

(require 'em-mode)
(add-to-list 'auto-mode-alist '("\\.em\\'" . em-mode))

(require 'llvm-mode)
(add-to-list 'auto-mode-alist '("\\.ll\\'" . llvm-mode))

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
;; creates a filelist.txt and TAGS file at ~/.emacs.tags/<PROJECT_NAME>/
(defun generate-project-tags (directory project-name)
  "Generate TAGS and filelist.txt for a project into ~/.emacs.tags/<project-name>/.
DIRECTORY is the project root, PROJECT-NAME is prompted from the user."
  (interactive "DSelect project root: \nsProject name: ")
  (let* ((default-directory (expand-file-name directory))
         (tags-root (expand-file-name (concat "~/.emacs.tags/" project-name "/")))
         (filelist (concat tags-root "filelist.txt"))
         (tagsfile (concat tags-root "TAGS"))
         (ps-command (format "Get-ChildItem -Recurse -File | ForEach-Object { $_.FullName } > '%s'; Get-Content '%s' | etags -o '%s' -"
                             filelist filelist tagsfile)))
    (make-directory tags-root t) ;; ensure directory exists
    (shell-command (concat "powershell -Command \"" ps-command "\""))
    (message "TAGS generated in %s" tags-root)))

(defun load-project-tags (tags-dir)
  "Prompt for a TAGS directory under ~/.emacs.tags and load the TAGS file."
  (interactive "DSelect TAGS directory: ")
  (let ((tagsfile (expand-file-name "TAGS" tags-dir)))
    (if (file-exists-p tagsfile)
        (progn
          (visit-tags-table tagsfile)
          (message "Loaded TAGS from %s" tagsfile))
      (message "No TAGS file found in %s" tags-dir))))
