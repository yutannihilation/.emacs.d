;; Key Bindings ------------------------------------------

;; moving to other window is frequent action when using ESS
(define-key global-map (kbd "C-t") 'other-window)

;; Packages ----------------------------------------------

;; setup repositories
(require 'package)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(setq pkgs '(ivy
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
	     magit))

(require 'cl)
;; if there are any packages not yet installed, get package list and install it
(let ((pkgs-not-yet-installed (remove-if 'package-installed-p pkgs)))
  (if pkgs-not-yet-installed
      (progn
	(package-refresh-contents)
	(dolist (pkg pkgs-not-yet-installed)
	 (package-install pkg)))))

;; Auto-Completion ----------------------------------------

(add-hook 'after-init-hook 'global-company-mode)

;; Theme --------------------------------------------------

(load-theme 'dracula t)
(set-default-font "Fira Code")

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

;; use fuzzy matching
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

;; Projectile ----------------------------------------------

(counsel-projectile-mode)

;; Ess -----------------------------------------------------

(require 'ess-site)

;; shut up and start R
(setq ess-ask-for-ess-directory nil)

;; we are so young that _ no more means <-
(ess-toggle-underscore nil)

(defun R-mode-hooks ()
  ;; show help
  (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)
  ;; start R
  (ess-request-a-process "Start R process" t))
(add-hook 'R-mode-hook 'R-mode-hooks)

;; Others ------------------------------------------------

;; move custom to another file
(setq custom-file "./custom.el")
(load custom-file)
