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

;; dabbrev-completion

;; With a prefix argument ARG, it searches all buffers accepted by the
;; function pointed out by ‘dabbrev-friend-buffer-function’ to find the
;; completions.
;;
;; Its value is ‘dabbrev--same-major-mode-p’

;; If the prefix argument is 16 (which comes from C-u C-u),
;; then it searches *all* buffers.
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

;; <-------------------------
;; cape

;; https://github.com/minad/cape#configuration
;;
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; exec-path

(setq exec-path (append exec-path '("/usr/local/bin")))
;; >--------------------------------------------------



;; <--------------------------------------------------
;; doom-themes

;; https://www.emacswiki.org/emacs/SetFonts#h5o-16
;;
(set-face-attribute 'default nil :family "JetBrains Mono" )

;; https://github.com/doomemacs/themes#manually--use-package
;;
(load-theme 'doom-one-light t)
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

;; https://github.com/yoshiki/yaml-mode

;; <-------------------------
;; yaml-pro

;; https://github.com/zkry/yaml-pro

(require 'yaml-pro)

(add-hook 'yaml-mode-hook 'yaml-pro-mode)

;; https://github.com/zkry/yaml-pro#usage-1

(define-key yaml-pro-mode-map [left] 'yaml-pro-unindent-subtree) ; was left-char
(define-key yaml-pro-mode-map [right] 'yaml-pro-indent-subtree)  ; was right-char
(define-key yaml-pro-mode-map [up] 'yaml-pro-jump)               ; was previous-line
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; minibuffer

(add-hook 'minibuffer-setup-hook 'subword-mode)

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Recursive-Mini.html
;;
;; enable-recursive-minibuffers

(define-key minibuffer-mode-map [up] 'consult-history) ; was vertico-previous
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
(global-set-key (kbd "M-'") 'subword-mode)                      ; was abbrev-prefix-mark
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
;; global vars

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Completion.html#index-completion_002dignored_002dextensions
;;
(add-to-list 'completion-ignored-extensions ".DS_Store")
;; >--------------------------------------------------



;; <--------------------------------------------------
;; expand-region

(global-set-key (kbd "C-=") 'er/expand-region)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; shell-mode

(require 'shell)

(define-key shell-mode-map [up] 'consult-history) ; was previous-line
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
(global-set-key (kbd "M-i") 'embark-act) ; was tab-to-tab-stop
;; >-------------------------

;; <-------------------------
;; embark-consult

;; https://github.com/oantolin/embark#consult
;; >-------------------------

(global-set-key (kbd "s-j") 'consult-buffer)             ; was exchange-point-and-mark, prefer C-x C-x
(global-set-key (kbd "s-f") 'consult-line)               ; was isearch-forward
(define-key isearch-mode-map (kbd "s-f") 'consult-line)
(global-set-key (kbd "C-x i") 'consult-isearch-history)  ; was insert-file
(global-set-key (kbd "s-g") 'consult-ripgrep)            ; was isearch-repeat-forward
(global-set-key (kbd "M-y") 'consult-yank-replace)
(global-set-key (kbd "s-h i") 'consult-imenu)
(global-set-key (kbd "C-x m") 'consult-flymake)          ; was compose-mail
(global-set-key (kbd "C-x r s") 'consult-register-store) ; was copy-to-register
(global-set-key (kbd "C-x r l") 'consult-register)       ; was bookmark-bmenu-list

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; project

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html

(require 'project)

(global-set-key (kbd "s-p") project-prefix-map)
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
(global-unset-key (kbd "s-o"))
(global-set-key (kbd "s-o o") 'symbol-overlay-put)
(global-set-key (kbd "s-o n") 'symbol-overlay-switch-forward)
(global-set-key (kbd "s-o p") 'symbol-overlay-switch-backward)
(global-set-key (kbd "s-o r") 'symbol-overlay-remove-all)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; magit

(global-set-key (kbd "s-m") 'magit-status)     ; was iconify-frame
;; >--------------------------------------------------



;; <--------------------------------------------------
;; ace-window

;; https://github.com/abo-abo/ace-window

;; https://github.com/abo-abo/ace-window#customization
;;
(global-set-key (kbd "s-n") 'ace-window) ; was make-frame
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
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

;; (require 'markdown-mode)
;; markdown-mode-map
;; >--------------------------------------------------



;; <--------------------------------------------------
;; compilation-mode

;; compilation-mode-map
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
;; eldoc

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Doc.html

;; * required by eglot

;; C-h . -> eldoc-doc-buffer
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

(global-set-key (kbd "C-x e") 'eglot)   ; was kmacro-end-and-call-macro
(define-key eglot-mode-map (kbd "s-l a") 'eglot-code-actions)
(define-key eglot-mode-map (kbd "s-l r") 'eglot-rename)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(java-mode . ("jdtls"
                              ;; "-noverify" "-Xmx2G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication"
                              "--jvm-arg=-javaagent:/Users/zjq/opt/lombok.jar"))))
;; >--------------------------------------------------



;; <--------------------------------------------------
;; nxml-mode

;; ref rng-nxml-mode-init
(defun my/nxml-mode-hook ()
  (remove-hook 'completion-at-point-functions #'rng-completion-at-point t))
(add-hook 'nxml-mode-hook 'my/nxml-mode-hook)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; misc

(electric-pair-mode)
;; >--------------------------------------------------
