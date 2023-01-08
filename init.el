;; <--------------------------------------------------
;; package

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; custom-file

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; dabbrev

;; * Use Dabbrev with Corfu!

;; swap
(global-set-key (kbd "M-/") 'dabbrev-completion)
(global-set-key (kbd "C-M-/") 'dabbrev-expand)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; orderless

;; https://github.com/oantolin/orderless

;; * required by vertico and corfu

;; https://github.com/minad/vertico#configuration
;;
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))
;;
;; https://github.com/minad/vertico#tramp-hostname-completion

;; https://github.com/oantolin/orderless#component-matching-styles
;; orderless-matching-styles
;; >--------------------------------------------------



;; <--------------------------------------------------
;; corfu

;; https://github.com/minad/corfu

;; https://github.com/minad/corfu#installation-and-configuration
;;
(global-corfu-mode)

;; https://github.com/minad/corfu#key-bindings
;;
;; M-g -> corfu-info-location
;; M-h -> corfu-info-documentation
;; M-SPC -> corfu-insert-separator
;; >--------------------------------------------------



;; <--------------------------------------------------
;; exec-path

(setq exec-path (append exec-path '("/usr/local/bin")))
;; >--------------------------------------------------



;; <--------------------------------------------------
;; theme

(load-theme 'dichromacy)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; maximize emacs

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; >--------------------------------------------------



;; <--------------------------------------------------
;; flyspell

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-mode)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; flymake

;; https://www.gnu.org/software/emacs/manual/html_node/flymake/index.html

(require 'flymake)

;; https://www.gnu.org/software/emacs/manual/html_node/flymake/Finding-diagnostics.html
;;
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; yaml-mode

;; https://github.com/antonj/Highlight-Indentation-for-Emacs
;;
(add-hook 'yaml-mode-hook 'highlight-indentation-mode)

;; (add-hook 'yaml-mode-hook 'display-line-numbers-mode)

;; https://github.com/zkry/yaml-pro
;;
(add-hook 'yaml-mode-hook 'yaml-pro-mode)
;;
;; https://github.com/zkry/yaml-pro#usage-1
;; >--------------------------------------------------



;; <--------------------------------------------------
;; minibuffer

(add-hook 'minibuffer-setup-hook 'subword-mode)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; global keys

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Keys.html
;;
;; help-map is the global keymap for the C-h prefix key.
;; mode-specific-map is the global keymap for the prefix key C-c.
;; ctl-x-map is the global keymap used for the C-x prefix key.
;; goto-map is the global keymap used for the M-g prefix key.

(global-unset-key (kbd "C-x C-c"))                              ; quit emacs
(global-unset-key (kbd "C-x C-z"))                              ; suspend-frame (minimize)
(global-unset-key (kbd "s-h"))                                  ; ns-do-hide-emacs

(global-set-key (kbd "s-z") 'execute-extended-command)          ; more handy
(global-set-key (kbd "C-z") ctl-x-map)                          ; more handy
(global-set-key [f9] 'subword-mode)
(global-set-key (kbd "C-x d") 'ediff-buffers)                   ; e[d]iff; was dired
(global-set-key (kbd "C-x w") 'diff-delete-trailing-whitespace) ; [w]hitespace
(global-set-key (kbd "C-x f") 'find-file)                       ; was set-fill-column
(global-set-key (kbd "C-x q") 'replace-string)                  ; was kbd-macro-query
(global-set-key (kbd "C-x l") 'toggle-truncate-lines)           ; was count-lines-page
(global-set-key [f4] 'kmacro-end-or-call-macro)

;; C-x r o -> open-rectangle
;; C-x r k -> kill-rectangle
;; >--------------------------------------------------



;; <--------------------------------------------------
;; expand-region

(global-set-key (kbd "C-=") 'er/expand-region)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; eshell-mode

(require 'em-hist)

;; prefer symbol-overlay
(define-key eshell-hist-mode-map (kbd "M-n") nil)
(define-key eshell-hist-mode-map (kbd "M-p") nil)       ; was eshell-previous-matching-input-from-input
;; >--------------------------------------------------



;; <--------------------------------------------------
;; consult

;; https://github.com/minad/consult#use-package-example

;; <-------------------------
;; recentf

;; https://www.emacswiki.org/emacs/RecentFiles
;; 
(recentf-mode 1)
;; >-------------------------

;; <-------------------------
;; vertico

;; replace helm, use s-h key prefix out of habit

;; https://github.com/minad/vertico#configuration
;; 
(vertico-mode)

;; https://www.emacswiki.org/emacs/SaveHist
;;
(savehist-mode 1)

;; https://github.com/minad/vertico/blob/main/extensions/vertico-repeat.el
;;
(global-set-key (kbd "s-h b") #'vertico-repeat)
(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
;; >-------------------------

;; <-------------------------
;; marginalia

;; https://github.com/minad/marginalia#configuration
;; 
(marginalia-mode)
;; >-------------------------

;; <-------------------------
;; embark

;; https://github.com/oantolin/embark#quick-start
;; 
(global-set-key [f5] 'embark-act)
;; >-------------------------

;; <-------------------------
;; embark-consult

;; https://github.com/oantolin/embark#consult
;; >-------------------------

(global-set-key [f1] 'consult-buffer)
(global-set-key [f8] 'consult-line)
(global-set-key [f11] 'consult-ripgrep)
(global-set-key (kbd "M-y") 'consult-yank-from-kill-ring)
(global-set-key (kbd "s-h i") 'consult-imenu)

(define-key eshell-hist-mode-map [up] 'consult-history) ; was eshell-previous-matching-input-from-input
;; >--------------------------------------------------



;; <--------------------------------------------------
;; projectile

;; https://docs.projectile.mx/projectile/usage.html#basic-usage

;; https://docs.projectile.mx/projectile/installation.html#installation-via-package-el
;;
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(projectile-mode +1)

(global-set-key [f2] 'projectile-switch-project)

(define-key projectile-mode-map (kbd "s-p x") 'projectile-run-eshell) ; e[x]ecute eshell; was prefix
;; >--------------------------------------------------



;; <--------------------------------------------------
;; java-mode

;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Customizing-C-and-C_002b_002b-indentation.html
;;
(defun my/java-mode-hook ()
  (c-set-offset 'arglist-cont-nonempty '++)
  (c-set-offset 'arglist-intro '++))
(add-hook 'java-mode-hook 'my/java-mode-hook)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; symbol-overlay

;; https://github.com/wolray/symbol-overlay/#usage
;;
(global-set-key [f10] 'symbol-overlay-put)
(global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
(global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
(global-set-key (kbd "C-x o") 'symbol-overlay-remove-all) ; was other-window, prefer f7
;; >--------------------------------------------------



;; <--------------------------------------------------
;; magit

(global-set-key [f3] 'magit-status)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; ace-window

(global-set-key [f7] 'ace-window)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; multiple-cursors

(global-set-key [f6] 'mc/edit-lines)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; ztree

;; >--------------------------------------------------



;; <--------------------------------------------------
;; all-the-icons

;; M-x all-the-icons-install-fonts
;; >--------------------------------------------------



;; <--------------------------------------------------
;; doom-modeline

(add-hook 'after-init-hook 'doom-modeline-mode)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; markdown-mode

(require 'markdown-mode)

;; prefer symbol-overlay
(define-key markdown-mode-map (kbd "M-n") nil) ; was markdown-next-link
(define-key markdown-mode-map (kbd "M-p") nil)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; compilation-mode

;; prefer symbol-overlay
;; (define-key compilation-mode-map (kbd "M-n") nil) ; was compilation-next-error
;; (define-key compilation-mode-map (kbd "M-p") nil)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; wgrep

;; https://github.com/mhayashi1120/Emacs-wgrep

;; https://github.com/mhayashi1120/Emacs-wgrep#usage
;; >--------------------------------------------------



;; <--------------------------------------------------
;; yasnippet

;; * required by eglot

(yas-global-mode 1)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; xref

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html

;; * required by eglot

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Looking-Up-Identifiers.html
;;
;; M-. -> Find definitions of an identifier (xref-find-definitions).
;; M-, -> Go back to where you previously invoked M-. and friends (xref-pop-marker-stack).

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref-Commands.html

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Identifier-Search.html
;;
;; M-? -> Find all the references for the identifier at point.
;; >--------------------------------------------------



;; <--------------------------------------------------
;; eglot

;; https://github.com/joaotavora/eglot

;; replace lsp-mode, use s-l key prefix out of habit

(require 'eglot)

;; https://github.com/joaotavora/eglot/pull/937
;; https://github.com/joaotavora/eglot/pull/937/files
;;
;;; eclipse-jdt breaks the spec which in turn breaks code actions
;;; This behaviour can't be disabled and needs to be worked around
(cl-defmethod eglot-execute-command
  (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
  "Eclipse JDT breaks spec and replies with edits as arguments."
  (mapc #'eglot--apply-workspace-edit arguments))

;; https://joaotavora.github.io/eglot/#index-starting-Eglot
;;
 (add-hook 'java-mode-hook 'eglot-ensure)

(define-key eglot-mode-map (kbd "s-l a") 'eglot-code-actions)
(define-key eglot-mode-map (kbd "s-l r") 'eglot-rename)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(java-mode . ("jdtls"
                              ;; "-noverify" "-Xmx2G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication"
                              "--jvm-arg=-javaagent:/Users/zjq/opt/lombok.jar"))))
;; >--------------------------------------------------



;; <--------------------------------------------------
;; misc

(electric-pair-mode)
;; >--------------------------------------------------
