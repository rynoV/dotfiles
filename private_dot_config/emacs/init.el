;;; Based off https://github.com/ianpan870102/yay-evil-emacs/blob/master/init.el
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; This is needed for org-plus-contrib which provides ox-extra
;; https://emacs.stackexchange.com/questions/8182/how-to-use-org-plus-contrib
;; https://emacs.stackexchange.com/questions/38184/org-mode-ignore-heading-when-exporting-to-latex/41685#41685
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
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
 '(browse-url-browser-function 'browse-url-default-browser)
 '(lsp-ui-sideline-delay 0.5)
 '(lsp-ui-sideline-show-hover t)
 '(lsp-ui-sideline-update-mode nil)
 '(org-agenda-files
   '("~/org/mlabs.org" "/home/calum/org/school.org" "/home/calum/org/calum.org" "/home/calum/org/notes.org"))
 '(org-journal-enable-encryption t)
 '(org-journal-mode-hook
   '(turn-on-visual-line-mode org-journal-default-enable-encryption
                              (lambda nil
                                (setq-local buffer-auto-save-file-name nil))))
 '(package-selected-packages
   '(docker lsp-purescript purescript-mode minimap treemacs-projectile ripgrep rg projectile crm-custom ido-ubiquitous unicode-fonts nix-mode direnv org-crypt org-journal sudo-edit chezmoi-company chezmoi org flex-mode flex ligature yaml-mode lsp-treemacs flycheck evil-iedit-state lsp-ui lsp-ido lsp-haskell lsp-mode prolog-mode writeroom-mode zen-mode ox-latex org-super-agenda haskell-mode org-download org-contrib ox-extra org-web-tools ox-md evil-numbers magit-todos evil-easymotion lispyville evil-exchange evil-args evil-textobj-line forge general company-posframe yasnippet auctex texmathp cdlatex rich-minority quelpa-use-package org-sort-tasks org-roam undo-tree org-indent visual-line org-sidebar iflipb restart-emacs poet-theme doom-themes highlight-escape-sequences highlight-numbers evil-surround which-key diminish org-bullets use-package))
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
