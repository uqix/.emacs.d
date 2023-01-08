(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-background nil)
 '(column-number-mode t)
 '(corfu-auto t)
 '(create-lockfiles nil)
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(dabbrev-case-fold-search nil)
 '(datetime-timezone 'Etc/GMT+8)
 '(dired-listing-switches "-alh")
 '(ffap-machine-p-known 'reject)
 '(flyspell-duplicate-distance 0)
 '(flyspell-mark-duplications-flag nil)
 '(imenu-max-item-length nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-dictionary "american")
 '(ispell-extra-args '("--camel-case"))
 '(ispell-program-name "aspell")
 '(magit-diff-refine-hunk 'all)
 '(make-backup-files nil)
 '(markdown-command "multimarkdown")
 '(markdown-fontify-code-block-default-mode 'ignore)
 '(markdown-fontify-code-blocks-natively t)
 '(nxml-child-indent 4)
 '(orderless-matching-styles '(orderless-regexp orderless-literal orderless-initialism))
 '(package-archive-priorities '(("melpa" . 1)))
 '(package-selected-packages
   '(cape eglot corfu wgrep vertico orderless embark-consult embark marginalia consult yaml-pro highlight-indentation dockerfile-mode markdown-mode yaml-mode json-mode doom-modeline all-the-icons ztree multiple-cursors ace-window magit yasnippet symbol-overlay expand-region))
 '(scroll-bar-mode nil)
 '(scroll-error-top-bottom t)
 '(tool-bar-mode nil)
 '(use-package-always-ensure t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Monaco"))))
 '(aw-leading-char-face ((t (:foreground "blue" :weight bold :height 2.0))))
 '(flyspell-duplicate ((t (:underline "gray85"))))
 '(flyspell-incorrect ((t (:underline "gray85"))))
 '(line-number ((t (:inherit (shadow default) :background "ivory" :foreground "gray92"))))
 '(line-number-current-line ((t (:inherit line-number :foreground "gray80"))))
 '(magit-diff-added ((t (:extend t :foreground "#22aa22"))))
 '(magit-diff-added-highlight ((t (:extend t :foreground "#22aa22"))))
 '(magit-diff-removed ((t (:extend t :foreground "#aa2222"))))
 '(magit-diff-removed-highlight ((t (:extend t :foreground "#aa2222"))))
 '(markdown-inline-code-face ((t (:inherit (markdown-code-face font-lock-constant-face) :background "#F5F8FA" :foreground "#657786"))))
 '(markdown-link-face ((t nil))))
