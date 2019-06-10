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
                     rbenv
                     inf-ruby
                     ruby-test-mode
                     rubocop
                     exec-path-from-shell
                     auto-complete
                     magit
                     git-gutter+
                     git-gutter-fringe+
                     rust-mode
                     writeroom-mode
                     emojify))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'better-defaults)

(global-git-gutter+-mode)
(require 'git-gutter-fringe+)

(setq writeroom-major-modes '(text-mode ruby-mode rust-mode))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq auto-save-file-name-transforms
                `((".*" ,(concat user-emacs-directory "auto-save/") t)))

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'ruby-mode)

(setq ruby-insert-encoding-magic-comment nil)

(setq next-line-add-newlines t)

(load-theme 'base16-eighties t)

(setq visible-bell nil)

;; Typography
(set-face-attribute 'default nil
                    :family "Menlo"
                    :height 150
                    :weight 'normal
                    :width 'normal)

(setq-default line-spacing 4)

(global-hl-line-mode 1)

(require 'helm)
(require 'helm-projectile)
(require 'helm-ag)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "s-f") #'helm-projectile-ag)
(global-set-key (kbd "s-t") #'helm-projectile-find-file)

;; Allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; Autoclose paired syntax elements like parens, quotes, etc
(require 'ruby-electric)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(global-rbenv-mode)
(rbenv-use-global)

(ac-config-default)
(setq web-mode-ac-sources-alist
  '(("css" . (ac-source-css-property))
    ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

(add-hook 'ruby-mode-hook #'rubocop-mode)

(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(require 'ruby-test-mode)
(add-hook 'ruby-mode-hook 'ruby-test-mode)

(require 'ido)
(ido-mode t)

(add-hook 'after-init-hook #'global-emojify-mode)

(add-hook 'before-save-hook 'whitespace-cleanup)

(global-set-key (kbd "C-x g") 'magit-status)

(setq css-indent-offset 2)
(setq js-indent-level 2)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (emojify writeroom-mode rust-mode magit ruby-test-mode ruby-electric rbenv inf-ruby helm-projectile helm-ag better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "#515151" :foreground "#a09f93" :box nil)))))
(put 'downcase-region 'disabled nil)
