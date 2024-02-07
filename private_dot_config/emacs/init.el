(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'org)
(setq use-package-always-demand t)

;; Need this as it is not loaded early enough with the daemon for some
;; reason, gave error "Eager macro-expansion failure: (void-function
;; loop)", see
;; http://makble.com/emacs-error-eager-macroexpansion-failure-voidfunction-loop
(require 'cl)

;; Load main config file "./config.org"
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
   '("3cdd0a96236a9db4e903c01cb45c0c111eb1492313a65790adb894f9f1a33b2d" "6096c0ff80d528b38ee77ab01bdbd5cdb7c513e09254dd8630c68240e2da8293" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" default))
 '(ledger-reports
   '(("Balance w/o budget" "ledger [[ledger-mode-flags]] -f /home/calum/org/ledger/ledger.dat bal Assets Liabilities --end tomorrow --effective --real")
     ("Moving expenses summary" "ledger [[ledger-mode-flags]] -f /home/calum/org/ledger/ledger.dat bal Expenses:Home: Expenses:Moving: --period \"to 2023/04/24\"")
     ("Monthly expense total by category" "ledger [[ledger-mode-flags]] -f /home/calum/org/ledger/ledger.dat bal Expenses: --period \"every month from 2023/04/24 to 2023/05/23\"")
     ("Monthly expense total" "ledger [[ledger-mode-flags]] -f /home/calum/org/ledger/ledger.dat reg Expenses: --period \"every month from 2023/04/24 to 2023/05/23\"")
     ("Peter's tab" "ledger [[ledger-mode-flags]] -f /home/calum/org/ledger/ledger.dat reg \"/Shared|Assets:Repayable:Peter/\" --period \"from 2023/06/24 to 2023/07/25\"")
     ("Balance with budget" "ledger [[ledger-mode-flags]] -f /home/calum/org/ledger/ledger.dat bal Assets Liabilities --end tomorrow --effective")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(lsp-ui-sideline-delay 0.5)
 '(lsp-ui-sideline-show-hover t)
 '(lsp-ui-sideline-update-mode nil)
 '(org-agenda-files
   '("/home/calum/org/notes.org" "/home/calum/org/auspice.org" "/home/calum/org/organizer.org" "/home/calum/org/ledger/hledger/reports.org" "/home/calum/org/calum.org"))
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
 '(org-refile-targets '((org-agenda-files :maxlevel . 3)) nil nil "Customized with use-package org")
 '(org-refile-use-outline-path 'file nil nil "Customized with use-package org")
 '(package-selected-packages
   '(image-roll org-noter org-roam-ui org-mind-map persp-mode-projectile-bridge bufler centaur-tabs web-mode eglot eldoc-box persp-mode eyebrowse spaceline spaceline-config autothemer os1-theme org-fragtog rust-mode elfeed goto-last-change smart-region topsy scroll-on-jump scrollkeeper aggressive-indent mosey hungry-delete dashboard smartparens goggles org-appear origami org-modern ox-ipynb json-navigator json-mode dart-mode dart ox-ravel ess diff-hl elisp-format visual-regexp multiple-cursors combobulate tree-sitter-langs tree-sitter delight ox-gfm general consult-flymake consult-eglot adaptive-wrap org-superstar org-inlinetask tempel-collection tempel emacs-lisp kind-icon cape corfu all-the-icons-completion code-review graphql-mode ox-json imenu-list markdown hercules quelpa emacs-surround meow magit-delta git-link emacs-open-github-from-here ledger-mode git-gutter dhall-mode all-the-icons slack dirvish typescript-mode typescript ranger affe consult-dir consult-projectile embark-consult embark marginalia orderless vertico consult docker lsp-purescript purescript-mode minimap treemacs-projectile ripgrep rg projectile crm-custom ido-ubiquitous unicode-fonts nix-mode direnv org-crypt org-journal sudo-edit chezmoi-company chezmoi org flex-mode flex ligature yaml-mode lsp-ido prolog-mode writeroom-mode zen-mode ox-latex org-super-agenda haskell-mode org-download org-contrib ox-extra org-web-tools ox-md magit-todos forge auctex texmathp cdlatex rich-minority quelpa-use-package org-sort-tasks org-roam undo-tree org-indent visual-line org-sidebar iflipb restart-emacs poet-theme doom-themes highlight-escape-sequences highlight-numbers which-key diminish use-package))
 '(safe-local-variable-values
   '((calc-embedded-open-plain . "# ")
     (org-use-property-inheritance "EXPORT_OPTIONS")
     (org-use-property-inheritance quote
                                   ("EXPORT_OPTIONS"))
     (org-latex-hyperref-template . "\\hypersetup{pdfauthor={%a}, pdftitle={%t}, pdfkeywords={%k}, pdfsubject={%d}, pdfcreator={%c}, pdflang={%L},colorlinks=true,linkcolor=blue,urlcolor=blue,filecolor=blue,citecolor=blue,anchorcolor=blue,linktocpage=true,unicode=true}")
     (org-latex-hyperref-template . "")
     (aggressive-indent-mode)
     (org-export-initial-scope . subtree)
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
 '(hi-yellow ((t (:background "#EBCB8B" :foreground "black"))))
 '(org-column ((t (:inherit default :strike-through nil :underline nil :slant normal :weight normal)))))
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
