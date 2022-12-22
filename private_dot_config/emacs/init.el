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

;; Quelpa for installing from source/git repos
;; https://github.com/quelpa/quelpa
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
;; Install quelpa use-package integration
;; https://github.com/quelpa/quelpa-use-package
(unless (package-installed-p 'quelpa)
  (quelpa
   '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git")))
(require 'quelpa-use-package)

;; Load main config file "./config.org"
(require 'org)
(org-babel-load-file (expand-file-name (concat user-emacs-directory "config.org")))

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(browse-url-browser-function 'browse-url-default-browser)
 '(custom-safe-themes
   '("1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" default))
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
   '(adaptive-wrap org-superstar org-inlinetask consult-flycheck tempel-collection tempel emacs-lisp kind-icon cape corfu all-the-icons-completion code-review graphql-mode ox-json imenu-list markdown hercules quelpa emacs-surround meow magit-delta git-link emacs-open-github-from-here flycheck-ledger ledger-mode git-gutter dhall-mode all-the-icons slack dirvish typescript-mode typescript ranger consult-lsp affe consult-dir consult-projectile embark-consult embark marginalia orderless vertico consult docker lsp-purescript purescript-mode minimap treemacs-projectile ripgrep rg projectile crm-custom ido-ubiquitous unicode-fonts nix-mode direnv org-crypt org-journal sudo-edit chezmoi-company chezmoi org flex-mode flex ligature yaml-mode lsp-treemacs flycheck lsp-ui lsp-ido lsp-haskell lsp-mode prolog-mode writeroom-mode zen-mode ox-latex org-super-agenda haskell-mode org-download org-contrib ox-extra org-web-tools ox-md magit-todos forge auctex texmathp cdlatex rich-minority quelpa-use-package org-sort-tasks org-roam undo-tree org-indent visual-line org-sidebar iflipb restart-emacs poet-theme doom-themes highlight-escape-sequences highlight-numbers which-key diminish use-package))
 '(safe-local-variable-values
   '((minor-mode . outline)
     (lsp-haskell-server-path . "haskell-language-server-wrapper")
     (lsp-haskell-server-path . "haskell-language-server-8.8.4")
     (lsp-haskell-formatting-provider . "fourmolu")
     (eval defun org-babel-execute:yaml
           (body params)
           body)))
 '(warning-suppress-types '((comp) (comp) (comp) (comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hi-blue ((t (:background "#81A1C1" :foreground "black"))))
 '(hi-green ((t (:background "#A3BE8C" :foreground "black"))))
 '(hi-pink ((t (:background "#B48EAD" :foreground "black"))))
 '(hi-yellow ((t (:background "#EBCB8B" :foreground "black"))))
 '(lsp-face-highlight-read ((t (:background "#363c4a" :foreground "#F0F4FC" :weight bold))))
 '(lsp-ui-sideline-symbol-info ((t (:extend t :background "#2E3440" :foreground "#656c7c"))))
 '(meow-region-cursor-1 ((t (:background "#6eee88c8a463" :foreground "#ECEFF4"))))
 '(meow-region-cursor-2 ((t (:background "#5c5c6fef8706" :foreground "#ECEFF4"))))
 '(meow-region-cursor-3 ((t (:background "#49c9571669a9" :foreground "#ECEFF4"))))
 '(org-block ((t (:extend t :background "#00000000"))))
 '(org-block-begin-line ((t (:inherit org-block :extend t :background "#373E4C" :foreground "#6f7787"))))
 '(org-hide ((t nil)))
 '(region ((t (:extend t :background "#373e4c"))))
 '(secondary-selection ((t (:extend t :background "#373e4c"))))
 '(show-paren-match ((t (:background "dim gray" :foreground "white")))))
(put 'scroll-left 'disabled nil)
