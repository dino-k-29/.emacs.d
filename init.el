; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; list the packages you want
(setq package-list '(better-defaults
                     base16-theme
                     helm
                     helm-projectile
                     helm-ag
                     ruby-electric
                     seeing-is-believing
                     rbenv
                     inf-ruby
                     ruby-test-mode
                     rinari
                     exec-path-from-shell
                     magit
                     git-gutter))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'better-defaults)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq auto-save-file-name-transforms
                `((".*" ,(concat user-emacs-directory "auto-save/") t))) 

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'ruby-mode)

(setq next-line-add-newlines t)

(load-theme 'base16-eighties t)

;; Show line numbers
(global-linum-mode)

;; Typography
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 150
                    :weight 'normal
                    :width 'normal)

(require 'helm)
(require 'helm-projectile)
(require 'helm-ag)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "s-f") #'helm-projectile-ag)
(global-set-key (kbd "s-t") #'helm-projectile-find-file-dwim)

;; Allow hash to be entered  
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; Autoclose paired syntax elements like parens, quotes, etc
(require 'ruby-electric)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(global-rbenv-mode)
(rbenv-use-global)

(setq seeing-is-believing-prefix "C-.")
(add-hook 'ruby-mode-hook 'seeing-is-believing)
(require 'seeing-is-believing)

(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(require 'ruby-test-mode)
(add-hook 'ruby-mode-hook 'ruby-test-mode)

(require 'rinari)
(global-rinari-mode)
(require 'ido)
(ido-mode t)

(global-set-key (kbd "C-x g") 'magit-status)

(add-hook 'compilation-finish-functions
          (lambda (buf strg)
            (switch-to-buffer-other-window "*compilation*")
            (read-only-mode)
            (goto-char (point-max))
            (local-set-key (kbd "q")
(lambda () (interactive) (quit-restore-window)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit solarized-theme seeing-is-believing ruby-test-mode ruby-electric rbenv inf-ruby helm-projectile helm-ag chruby better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
