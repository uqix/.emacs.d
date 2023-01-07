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

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-mode)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; flycheck

;; https://github.com/flycheck/flycheck

;; https://www.flycheck.org/en/latest/user/quickstart.html
;;
(add-hook 'after-init-hook #'global-flycheck-mode)

;; https://www.flycheck.org/en/latest/user/error-interaction.html#navigate-errors

(global-set-key [f3] 'flycheck-next-error)
(global-set-key [f4] 'flycheck-previous-error)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; yaml-mode

;; https://github.com/antonj/Highlight-Indentation-for-Emacs
(add-hook 'yaml-mode-hook 'highlight-indentation-mode)

;; (add-hook 'yaml-mode-hook 'display-line-numbers-mode)

;; https://github.com/zkry/yaml-pro
(add-hook 'yaml-mode-hook 'yaml-pro-mode)
;;
;; https://github.com/zkry/yaml-pro#usage-1
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

(global-set-key (kbd "C-z") ctl-x-map)  ; more handy
(global-unset-key (kbd "C-x C-c"))      ; quit emacs
(global-unset-key (kbd "C-x C-z"))      ; suspend-frame (minimize)
(global-set-key [f5] 'toggle-truncate-lines)
;; (global-set-key [f5] 'kmacro-end-or-call-macro)
(global-set-key [f9] 'subword-mode)

(global-set-key (kbd "C-x u") 'browse-url)                      ; open [u]rl; was undo, prefer C-/
(global-set-key (kbd "C-x d") 'ediff-buffers)                   ; e[d]iff; was dired
(global-set-key (kbd "C-x w") 'diff-delete-trailing-whitespace) ; [w]hitespace

;; (global-set-key (kbd "C-x r") 'replace-string) ; [r]eplace; was registers/bookmarks prefix, prefer helm-all-mark-rings
;; C-x r o -> open-rectangle
;; C-x r k -> kill-rectangle

(global-set-key (kbd "C-x q") 'replace-string) ; was kbd-macro-query

;; prefix C-x j for low frequency
;; (global-set-key (kbd "C-x j r") 'replace-string) ; [r]eplace
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; expand-region

(global-set-key (kbd "C-=") 'er/expand-region)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; helm

;; https://github.com/emacs-helm/helm/wiki#if-installed-from-emacs-package-manager-packageel
;;
(require 'helm)

;; https://github.com/emacs-helm/helm/wiki#helm-mode
;;
(helm-mode 1)

;; https://github.com/emacs-helm/helm/wiki#general-helm-commands
;;
;; <tab> lists available actions
;; C-j invokes the persistent action

;; https://github.com/emacs-helm/helm/wiki#preconfigured-helm-commands
;;
(global-set-key (kbd "s-h") 'helm-command-prefix)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "s-z") 'helm-M-x)              ; more handy

(global-set-key (kbd "C-x C-f") 'helm-find-files)   ; was find-file
(global-set-key (kbd "C-x f") 'helm-find-files)     ; was set-fill-column

(global-set-key [f1] 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key [f8] 'helm-occur)
(global-set-key (kbd "s-h m") 'helm-all-mark-rings)  ; [m]ark rings; was helm-man-woman
;; (global-set-key (kbd "s-h s") nil)                ; was helm-surfraw
(global-set-key (kbd "s-h g") 'helm-google-suggest)  ; [g]oogle


;; <---
;; helm-ag

;; https://github.com/emacsorphanage/helm-ag#basic-usage
;;
(global-set-key [f11] 'helm-do-ag)
;; >---

;; https://github.com/emacs-helm/helm/blob/941da0db9ef54d5e895c53d9265f2dd4a6c93db2/helm-core.el#L1715
(add-hook 'helm-after-initialize-hook 'helm-init-relative-display-line-numbers)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; projectile

;; https://docs.projectile.mx/projectile/usage.html#basic-usage
;;
;; helm-projectile-ag: s-p s s

;; https://docs.projectile.mx/projectile/installation.html#installation-via-package-el
;;
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(projectile-mode +1)


;; <---
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
;; >---


(global-set-key [f2] 'helm-projectile-switch-project)
(define-key projectile-mode-map (kbd "s-p s") 'helm-projectile-ag)    ; [s]earch by ag; was prefix
(define-key projectile-mode-map (kbd "s-p x") 'projectile-run-eshell) ; e[x]ecute eshell; was prefix
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; LSP

;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;;
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
;; (setq lsp-idle-delay 0.500)

;; https://emacs-lsp.github.io/lsp-java/#quick-start
(require 'yasnippet)
(yas-global-mode 1)

(require 'lsp-mode)

(require 'lsp-ui)

;; https://emacs-lsp.github.io/dap-mode/page/configuration/#dap-mode-configuration
(dap-auto-configure-mode)

;; https://emacs-lsp.github.io/lsp-java/#install-via-melpa
(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)
;; https://emacs-lsp.github.io/dap-mode/page/configuration/#java
(require 'dap-java)

;; https://github.com/emacs-lsp/lsp-ui#lsp-ui-peek
;;
;; You may remap xref-find-{definitions,references} (bound to M-. M-? by default):
;;
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

(define-key lsp-mode-map (kbd "s-l d") 'lsp-ui-peek-find-implementation)    ; find [d]ownward
(define-key lsp-mode-map (kbd "s-l u") 'lsp-java-open-super-implementation) ; find [u]pward
(define-key lsp-mode-map (kbd "s-l h") 'lsp-ui-doc-show)                    ; doc [h]elp
(define-key lsp-mode-map (kbd "s-l i") 'lsp-organize-imports)               ; [i]mport
(define-key lsp-mode-map (kbd "s-l f") 'lsp-execute-code-action)            ; action to [f]ix
(define-key lsp-mode-map (kbd "s-l t") 'lsp-jt-lens-mode)                   ; [t]est lens
(define-key lsp-mode-map (kbd "s-l s") 'lsp-signature-activate)             ; method [s]ignature
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; java-mode

;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Customizing-C-and-C_002b_002b-indentation.html
;;
(defun my/java-mode-hook ()
  (c-set-offset 'arglist-cont-nonempty '++)
  (c-set-offset 'arglist-intro '++))
(add-hook 'java-mode-hook 'my/java-mode-hook)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; symbol-overlay

;; https://github.com/wolray/symbol-overlay/#usage
;;
(global-set-key [f10] 'symbol-overlay-put)
(global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
(global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
(global-set-key (kbd "C-x o") 'symbol-overlay-remove-all) ; was other-window, prefer f7
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; magit

;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; ace-window

(global-set-key [f7] 'ace-window)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; multiple-cursors

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
;; markdown-mode

;; prefer symbol-overlay
(define-key markdown-mode-map (kbd "M-n") nil) ; was markdown-next-link
(define-key markdown-mode-map (kbd "M-p") nil)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; compilation-mode

;; prefer symbol-overlay
;; (define-key compilation-mode-map (kbd "M-n") nil) ; was compilation-next-error
;; (define-key compilation-mode-map (kbd "M-p") nil)
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; eshell-mode

(require 'em-hist)

;; prefer symbol-overlay
(define-key eshell-hist-mode-map (kbd "M-n") nil)
(define-key eshell-hist-mode-map (kbd "M-p") nil)           ; was eshell-previous-matching-input-from-input
(define-key eshell-hist-mode-map [up] 'helm-eshell-history) ; was eshell-previous-matching-input-from-input
;; >----------------------------------------------------------------------------------------------------



;; <----------------------------------------------------------------------------------------------------
;; misc

(electric-pair-mode)
;; >----------------------------------------------------------------------------------------------------
