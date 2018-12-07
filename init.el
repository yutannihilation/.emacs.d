;; Key Bindings ------------------------------------------

;; moving to other window is frequent action when using ESS
(define-key global-map (kbd "C-t") 'other-window)

;; Packages ----------------------------------------------

;; setup repositories
(require 'package)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq pkgs '(ivy
	     ivy-hydra
	     counsel
	     swiper
             company
             ess
             powerline
	     dracula-theme
             flycheck
             quickrun
             projectile
	     counsel-projectile
	     magit
	     markdown-mode
	     polymode
	     yaml-mode
	     company-go))

(require 'cl)
;; if there are any packages not yet installed, get package list and install it
(let ((pkgs-not-yet-installed (remove-if 'package-installed-p pkgs)))
  (if pkgs-not-yet-installed
      (progn
	(package-refresh-contents)
	(dolist (pkg pkgs-not-yet-installed)
	 (package-install pkg)))))

;; Flycheck ----------------------------------------------

(global-flycheck-mode)
;; enable go-flycheck
(add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
(require 'go-flycheck)

;; Auto-Completion ----------------------------------------

(add-hook 'after-init-hook 'global-company-mode)

;; Theme --------------------------------------------------

(load-theme 'dracula t)

;; hide menu bar and tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; use powerline
(require 'powerline)
(powerline-default-theme)

;; disable beeps
(setq ring-bell-function 'ignore)

;; Ivy -----------------------------------------------------

(ivy-mode)
(counsel-mode)
(setq ivy-use-virtual-buffers t)
(global-set-key (kbd "C-s") 'swiper)

;; Projectile ----------------------------------------------

(counsel-projectile-mode)

;; ignore files that git ignores
(setq projectile-indexing-method 'git)
(setq projectile-use-git-grep t)

;; magit ---------------------------------------------------

(global-set-key (kbd "C-x g") 'magit-status)

;; Ess -----------------------------------------------------

(require 'ess-site)

;; shut up and start R
(setq ess-ask-for-ess-directory nil)

;; set default indentation as the same as RStudio
(setq ess-default-style 'RStudio)

;; we are so young that _ no more means <-
(ess-toggle-underscore nil)

(defun R-mode-hooks ()
  ;; show help of the active candidate of completion
  (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)
  ;; set the working directory as the project root
  (setq ess-startup-directory (projectile-project-root))
  ;; start R
  (ess-request-a-process "Start R process" t)
  ;; use TAB for completion
  (define-key inferior-ess-mode-map (kbd "TAB") 'company-complete))
(add-hook 'R-mode-hook 'R-mode-hooks)

(defun inferior-ess-mode-hooks ()
  ;; always use company-complete instead of completion-at-point
  ;; See https://github.com/emacs-ess/ESS/blob/37b9fdc7383417643cabb6af9c39f037908403bf/lisp/ess-inf.el#L2008
  (define-key inferior-ess-mode-map (kbd "TAB") 'company-complete))
(add-hook 'inferior-ess-mode-hook 'inferior-ess-mode-hooks)

;; Go ---------------------------------------------------

(require 'company-go)
;; Use company-mode only
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))
;; fmt codes
(add-hook 'before-save-hook 'gofmt-before-save)
(setq gofmt-command "goimports")

;; Markdown ----------------------------------------------

;; polymode
(require 'poly-R)
(require 'poly-markdown)

(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

;; (autoload 'gfm-mode "markdown-mode"
;;   "Major mode for editing GitHub Flavored Markdown files" t)
;; (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; Others ------------------------------------------------

;; load variables
(load-file "~/.emacs.d/vars.el")

;; move custom to another file
(setq custom-file "./custom.el")
(load custom-file)

;; do not create files with ~
(setq make-backup-files nil)

;; Use LF for line ending
(setq default-buffer-file-coding-system 'utf-8-unix)

;; start server
(server-start)
