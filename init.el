;; <----------------------------------------------------------------------------------------------------
;; package

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; custom-file

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; load-path

;; https://www.emacswiki.org/emacs/LoadPath
(add-to-list 'load-path "~/.emacs.d/lisp/")
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; company

;; fail fast before unset keys when refresh install emacs
(global-company-mode)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; exec-path

(setq exec-path (append exec-path '("/usr/local/bin")))
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; theme

(load-theme 'dichromacy)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; maximize emacs

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; flyspell

(add-hook 'prog-mode-hook 'flyspell-mode)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; flycheck

(global-flycheck-mode)
;; https://www.flycheck.org/en/latest/user/error-interaction.html#navigate-errors
;; By default Flycheck hooks into Emacs’ standard error navigation on M-g n (next-error) and M-g p (previous-error).

(global-set-key [f1] 'flycheck-list-errors)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; yaml-mode

(add-hook 'yaml-mode-hook 'highlight-indentation-mode)
(add-hook 'yaml-mode-hook 'flyspell-mode)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; minibuffer

(add-hook 'minibuffer-setup-hook 'subword-mode)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; global keys

(global-unset-key (kbd "s-h"))          ; hide emacs
(global-unset-key (kbd "s-z"))          ; undo
(global-unset-key (kbd "C-x C-c"))      ; quit emacs
(global-unset-key (kbd "C-z"))          ; minimize emacs
;; (global-unset-key (kbd "C-x C-z"))      ; minimize emacs

(global-set-key [f3] 'toggle-truncate-lines)
(global-set-key [f4] 'kmacro-end-or-call-macro)
(global-set-key [f5] 'delete-trailing-whitespace)
(global-set-key [f9] 'subword-mode)
(global-set-key [f10] 'replace-string)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; expand-region

(global-set-key (kbd "C-=") 'er/expand-region)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; LSP

;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
;; (setq lsp-idle-delay 0.500)

;; <<< lsp-java

;; https://emacs-lsp.github.io/lsp-java/#quick-start

(use-package projectile)
(use-package flycheck)
(use-package yasnippet :config (yas-global-mode))
(use-package lsp-mode
  ;; :hook ((lsp-mode . lsp-enable-which-key-integration))

  ;; or no imports
  ;; :config (setq lsp-completion-enable-additional-text-edit nil)
  )
(use-package hydra)
(use-package company)
(use-package lsp-ui)
;; (use-package which-key :config (which-key-mode))
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
(use-package helm-lsp)
(use-package helm
  :config (helm-mode))
(use-package lsp-treemacs)
;; >>>

(require 'lsp-mode)

;; https://github.com/emacs-lsp/lsp-ui#lsp-ui-peek

;; You may remap xref-find-{definitions,references} (bound to M-. M-? by default):
;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

(define-key lsp-mode-map (kbd "s-l g d") 'lsp-ui-peek-find-implementation)
(define-key lsp-mode-map (kbd "s-l g u") 'lsp-java-open-super-implementation)
(define-key lsp-mode-map (kbd "s-l h s") 'lsp-ui-doc-show)

(define-key lsp-signature-mode-map (kbd "M-n") nil)
(define-key lsp-signature-mode-map (kbd "M-p") nil)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; java-mode

;; https://stackoverflow.com/a/6952408
(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '+))
(add-hook 'java-mode-hook 'my-indent-setup)

;; https://www.emacswiki.org/emacs/java-mode-indent-annotations.el
(require 'java-mode-indent-annotations)
(add-hook 'java-mode-hook 'java-mode-indent-annotations-setup)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; js

;; (add-hook 'js-mode-hook 'lsp)
;; (define-key js-mode-map (kbd "M-.") 'lsp-find-definition)
;; (define-key js-mode-map (kbd "M-?") 'lsp-find-references)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; highlight-parentheses

(global-highlight-parentheses-mode)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; symbol-overlay

(global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
(global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
(global-set-key [f2] 'symbol-overlay-put)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; magit

;; https://magit.vc/manual/magit/Default-Bindings.html
;; C-x g	magit-status
;; C-x M-g	magit-dispatch
;; C-c M-g	magit-file-dispatch
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; ace-window

(global-set-key [f7] 'ace-window)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; compilation-mode

;; https://stackoverflow.com/a/20788623
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; multiple-cursors

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key [f6] 'mc/edit-lines)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; ztree

;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; all-the-icons

;; M-x all-the-icons-install-fonts
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; doom-modeline

(add-hook 'after-init-hook 'doom-modeline-mode)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; helm

;; http://tuhdo.github.io/helm-intro.html

;; https://github.com/emacs-helm/helm/wiki#if-installed-from-emacs-package-manager-packageel
;; However you will not have the global bindings enabled until you require helm with either require or use-package.

;; (require 'helm)
;; (require 'helm-config)

;; https://github.com/emacs-helm/helm/wiki#preconfigured-helm-commands
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-c h") 'helm-command-prefix)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)

;; https://github.com/emacs-helm/helm/wiki#helm-mode
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; (global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key [f8] 'helm-occur)

(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

;; https://github.com/emacs-helm/helm/wiki#general-helm-commands
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

;; (helm-mode 1)

;; (require 'helm-ag)
(global-set-key [f11] 'helm-do-ag)

;; (require 'helm-ls-git)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; projectile

;; https://docs.projectile.mx/projectile/usage.html#basic-usage

;; https://docs.projectile.mx/projectile/installation.html#installation-via-package-el
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(projectile-mode +1)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; helm-projectile

;; https://github.com/bbatsov/helm-projectile#usage

;; (setq helm-projectile-fuzzy-match nil)
;; (require 'helm-projectile)
;; (helm-projectile-on)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; markdown-mode

(add-hook 'markdown-mode-hook 'flyspell-mode)
(define-key markdown-mode-map (kbd "M-n") nil)
(define-key markdown-mode-map (kbd "M-p") nil)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; markdown-preview-mode

(setq markdown-preview-stylesheets
        (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.9.0/github-markdown.min.css"
              "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css" "
<style>
 .markdown-body {
   box-sizing: border-box;
   min-width: 200px;
   max-width: 980px;
   margin: 0 auto;
   padding: 45px;
 }

 @media (max-width: 767px) {
   .markdown-body {
     padding: 15px;
   }
 }
</style>
  "))

(setq markdown-preview-javascript
        (list "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js" "
<script>
 $(document).on('mdContentChange', function() {
   $('pre code').each(function(i, block) {
     hljs.highlightBlock(block);
   });
 });
</script>
  "))
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; misc

(electric-pair-mode)
;; (global-display-line-numbers-mode)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; magit-delta

;; https://github.com/dandavison/magit-delta
;; https://github.com/dandavison/magit-delta/issues/9
;; (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))
;; >----------------------------------------------------------------------------------------------------
