;;; Based off https://github.com/ianpan870102/yay-evil-emacs/blob/master/init.el
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; Setting up the package manager. Install if missing.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t))

;; Load main config file "./config.org"
(require 'org)
(org-babel-load-file (expand-file-name (concat user-emacs-directory "config.org")))

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-sideline-delay 0.5)
 '(lsp-ui-sideline-show-hover t)
 '(lsp-ui-sideline-update-mode nil)
 '(org-agenda-files '("~/org/school.org" "~/org/calum.org" "~/org/notes.org"))
 '(package-selected-packages
   '(chezmoi-company chezmoi org flex-mode flex ligature yaml-mode lsp-treemacs flycheck evil-iedit-state lsp-ui lsp-ido lsp-haskell lsp-mode prolog-mode writeroom-mode zen-mode ox-latex org-super-agenda haskell-mode org-download org-contrib ox-extra org-web-tools ox-md evil-numbers magit-todos evil-easymotion lispyville evil-exchange evil-args evil-textobj-line forge general company-posframe yasnippet auctex texmathp cdlatex rich-minority quelpa-use-package org-sort-tasks org-roam undo-tree org-indent visual-line org-sidebar iflipb restart-emacs poet-theme doom-themes highlight-escape-sequences highlight-numbers evil-surround which-key diminish org-bullets use-package))
 '(safe-local-variable-values '((eval defun org-babel-execute:yaml (body params) body)))
 '(warning-suppress-types '((comp) (comp) (comp) (comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t (:family calum/code-font))))
 '(company-tooltip-selection ((t (:background "dim gray"))))
 '(lsp-face-highlight-read ((t (:background "#363c4a" :foreground "#F0F4FC" :weight bold))))
 '(lsp-ui-sideline-symbol-info ((t (:extend t :background "#2E3440" :foreground "#656c7c"))))
 '(show-paren-match ((t (:background "dim gray" :foreground "white")))))
