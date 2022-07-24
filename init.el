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

;; https://github.com/company-mode/company-mode

;; http://company-mode.github.io/
;;
;; * Use M-n and M-p to select
;; * <return> to complete
;; * <tab> to complete the common part
;; * Search through the completions with C-s, C-r and C-o
;; * Press M-(digit) to quickly complete with one of the first 10 candidates

(add-hook 'after-init-hook 'global-company-mode)
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

;; https://github.com/flycheck/flycheck

;; https://www.flycheck.org/en/latest/user/quickstart.html
;;
(add-hook 'after-init-hook #'global-flycheck-mode)

;; https://www.flycheck.org/en/latest/user/error-interaction.html#navigate-errors
;; By default Flycheck hooks into Emacs’ standard error navigation:
;; M-g n (next-error)
;; M-g p (previous-error)

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

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Keys.html
;;
;; help-map is the global keymap for the C-h prefix key.
;; mode-specific-map is the global keymap for the prefix key C-c.
;; ctl-x-map is the global keymap used for the C-x prefix key.
;; goto-map is the global keymap used for the M-g prefix key.

;; alias C-z=C-x
(global-set-key (kbd "C-z") ctl-x-map)

(global-unset-key (kbd "C-x C-c"))      ; quit emacs

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
;; helm

;; https://github.com/emacs-helm/helm/wiki#if-installed-from-emacs-package-manager-packageel
;;
;; not have the global bindings enabled until you require helm with either require or use-package.
(require 'helm)

;; https://github.com/emacs-helm/helm/wiki#general-helm-commands
;;
;; <tab> or C-i lists available actions
;; C-j or C-z invokes the persistent action

;; https://github.com/emacs-helm/helm/wiki#preconfigured-helm-commands
;;
(global-set-key (kbd "s-h") 'helm-command-prefix)

;; https://github.com/emacs-helm/helm/wiki#helm-mode
;;
;; alias s-z=M-x
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "s-z") 'helm-M-x)
;;
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-mode 1)

(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key [f8] 'helm-occur)
;; (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; helm-ag

(global-set-key [f11] 'helm-do-ag)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; helm-ls-git

(global-set-key (kbd "C-x C-d") 'helm-browse-project)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; projectile

;; https://docs.projectile.mx/projectile/usage.html#basic-usage

;; https://docs.projectile.mx/projectile/installation.html#installation-via-package-el
;;
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(projectile-mode +1)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; helm-projectile

;; https://github.com/bbatsov/helm-projectile#usage

;; * capable of opening multiple files by marking
;; * can fire many actions
;;
;; replace the normal Projectile commands:
;;
;; (setq helm-projectile-fuzzy-match nil)
(require 'helm-projectile)
(helm-projectile-on)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; LSP

;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;;
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
;; (setq lsp-idle-delay 0.500)

;; https://emacs-lsp.github.io/lsp-java/#quick-start
;;
(use-package yasnippet :config (yas-global-mode))
(use-package lsp-mode)
(use-package hydra)
(use-package lsp-ui)
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
(use-package helm-lsp)
(use-package lsp-treemacs)

(require 'lsp-mode)

;; https://github.com/emacs-lsp/lsp-ui#lsp-ui-peek
;;
;; You may remap xref-find-{definitions,references} (bound to M-. M-? by default):
;;
;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

(define-key lsp-mode-map (kbd "s-l d") 'lsp-ui-peek-find-implementation)
(define-key lsp-mode-map (kbd "s-l u") 'lsp-java-open-super-implementation)
(define-key lsp-mode-map (kbd "s-l d") 'lsp-ui-doc-show)

;; TODO what's this?
;; (define-key lsp-signature-mode-map (kbd "M-n") nil)
;; (define-key lsp-signature-mode-map (kbd "M-p") nil)
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

;; https://sr.ht/~tsdh/highlight-parentheses.el/#usage
;;
(add-hook 'prog-mode-hook #'highlight-parentheses-mode)
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

;; TODO still needed?

;; https://stackoverflow.com/a/20788623
;;
;; (ignore-errors
;;   (require 'ansi-color)
;;   (defun my-colorize-compilation-buffer ()
;;     (when (eq major-mode 'compilation-mode)
;;       (ansi-color-apply-on-region compilation-filter-start (point-max))))
;;   (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; multiple-cursors

(global-set-key [f6] 'mc/edit-lines)

;; add multiple cursors not based on continuous lines, but based on keywords in the buffer, use:
;;
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
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

;; performance issue
;; https://github.com/dandavison/magit-delta/issues/9
;; (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))
;; >----------------------------------------------------------------------------------------------------
