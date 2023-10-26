;;; Based off https://github.com/ianpan870102/yay-evil-emacs/blob/master/init.el
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
;; For latest version of eglot
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))
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

;; Need this as it is not loaded early enough with the daemon for some
;; reason, gave error "Eager macro-expansion failure: (void-function
;; loop)", see
;; http://makble.com/emacs-error-eager-macroexpansion-failure-voidfunction-loop
(require 'cl)

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
   '("6096c0ff80d528b38ee77ab01bdbd5cdb7c513e09254dd8630c68240e2da8293" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" default))
 '(lsp-ui-sideline-delay 0.5)
 '(lsp-ui-sideline-show-hover t)
 '(lsp-ui-sideline-update-mode nil)
 '(org-agenda-files
   '("/home/calum/org/mlabs.org" "/home/calum/org/school.org" "/home/calum/org/calum.org" "/home/calum/org/notes.org"))
 '(org-journal-enable-encryption t)
 '(org-journal-mode-hook
   '(turn-on-visual-line-mode org-journal-default-enable-encryption
                              (lambda nil
                                (setq-local buffer-auto-save-file-name nil))))
 '(org-latex-default-packages-alist
   '(("AUTO" "inputenc" t
      ("pdflatex"))
     ("T1" "fontenc" t
      ("pdflatex"))
     ("" "graphicx" t nil)
     ("" "longtable" nil nil)
     ("" "wrapfig" nil nil)
     ("" "rotating" nil nil)
     ("normalem" "ulem" t nil)
     ("" "amsmath" t nil)
     ("" "amssymb" t nil)
     ("" "capt-of" nil nil)
     ("colorlinks=true,linkcolor=darkgray,unicode=true,psdextra" "hyperref" nil nil)))
 '(package-selected-packages
   '(org-roam-ui org-mind-map persp-mode-projectile-bridge bufler centaur-tabs web-mode eglot eldoc-box persp-mode eyebrowse spaceline spaceline-config autothemer os1-theme org-fragtog rust-mode elfeed goto-last-change smart-region topsy scroll-on-jump scrollkeeper aggressive-indent mosey hungry-delete dashboard smartparens goggles org-appear origami org-modern ox-ipynb pdf-tools json-navigator json-mode dart-mode dart ox-ravel ess diff-hl elisp-format visual-regexp multiple-cursors combobulate tree-sitter-langs tree-sitter delight ox-gfm general consult-flymake consult-eglot adaptive-wrap org-superstar org-inlinetask tempel-collection tempel emacs-lisp kind-icon cape corfu all-the-icons-completion code-review graphql-mode ox-json imenu-list markdown hercules quelpa emacs-surround meow magit-delta git-link emacs-open-github-from-here ledger-mode git-gutter dhall-mode all-the-icons slack dirvish typescript-mode typescript ranger affe consult-dir consult-projectile embark-consult embark marginalia orderless vertico consult docker lsp-purescript purescript-mode minimap treemacs-projectile ripgrep rg projectile crm-custom ido-ubiquitous unicode-fonts nix-mode direnv org-crypt org-journal sudo-edit chezmoi-company chezmoi org flex-mode flex ligature yaml-mode lsp-ido prolog-mode writeroom-mode zen-mode ox-latex org-super-agenda haskell-mode org-download org-contrib ox-extra org-web-tools ox-md magit-todos forge auctex texmathp cdlatex rich-minority quelpa-use-package org-sort-tasks org-roam undo-tree org-indent visual-line org-sidebar iflipb restart-emacs poet-theme doom-themes highlight-escape-sequences highlight-numbers which-key diminish use-package))
 '(safe-local-variable-values
   '((org-export-initial-scope . subtree)
     (org-fragtog-mode)
     (projectile-project-src-dir . "lib/")
     (projectile-project-test-cmd . "flutter test")
     (projectile-project-compilation-cmd . "flutter build")
     (projectile-project-run-cmd . "flutter run")
     (projectile-project-src-dir . "src/")
     (projectile-project-test-dir . "test/")
     (projectile-project-test-cmd . "stack build --fast --test")
     (projectile-project-compilation-cmd . "stack build --fast --test --no-run-tests")
     (projectile-project-run-cmd . "stack build --fast --test --no-run-tests && stack exec organizer-server")
     (auto-fill-mode . t)
     (visual-line-mode)
     (org-deadline-warning-days . 21)
     (org-deadline-warning-days . 14)
     (visual-fill-column-mode)
     (minor-mode . outline)
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
 '(hi-yellow ((t (:background "#EBCB8B" :foreground "black")))))
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
