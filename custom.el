(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(company-dabbrev-downcase nil)
 '(create-lockfiles nil)
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(dabbrev-case-fold-search nil)
 '(datetime-timezone 'Etc/GMT+8)
 '(dired-listing-switches "-alh")
 '(flycheck-disabled-checkers '(json-python-json))
 '(helm-ag-insert-at-point 'symbol)
 '(helm-display-function 'helm-display-buffer-in-own-frame)
 '(helm-use-undecorated-frame-option nil)
 '(imenu-max-item-length nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-dictionary "american")
 '(ispell-extra-args '("--camel-case"))
 '(ispell-program-name "aspell")
 '(lsp-enable-indentation nil)
 '(lsp-enable-on-type-formatting nil)
 '(lsp-file-watch-threshold 100000)
 '(lsp-java-vmargs
   '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx1G" "-Xms100m" "-javaagent:/Users/zjq/opt/lombok.jar"))
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-max-height 30)
 '(lsp-ui-doc-use-webkit t)
 '(lsp-ui-sideline-enable nil)
 '(magit-delta-delta-args
   '("--max-line-distance" "0.6" "--true-color" "always" "--color-only" "--minus-style" "syntax \"#ffeeee\"" "--minus-emph-style" "syntax \"#ffcccc\"" "--plus-style" "syntax \"#eeffee\"" "--plus-emph-style" "syntax \"#bbffbb\""))
 '(magit-diff-refine-hunk t)
 '(make-backup-files nil)
 '(markdown-command "multimarkdown")
 '(markdown-fontify-code-block-default-mode 'ignore)
 '(markdown-fontify-code-blocks-natively t)
 '(nxml-child-indent 4)
 '(package-archive-priorities '(("melpa" . 1)))
 '(package-selected-packages
   '(solidity-mode highlight-indentation dockerfile-mode markdown-mode lsp-mode highlight-parentheses helm-lsp which-key use-package yaml-mode json-mode dap-mode lsp-java lsp-ui company-lsp hydra helm-projectile projectile helm-ag helm doom-modeline all-the-icons ztree multiple-cursors ace-window magit yasnippet symbol-overlay flycheck company expand-region))
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
 '(line-number ((t (:inherit (shadow default) :background "ivory" :foreground "gray92"))))
 '(markdown-inline-code-face ((t (:inherit (markdown-code-face font-lock-constant-face) :background "#F5F8FA" :foreground "#657786"))))
 '(markdown-link-face ((t nil))))
