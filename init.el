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

;; https://github.com/minad/corfu#completing-in-the-minibuffer
;;
(defun my/corfu-minibuffer-setup-hook ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map))
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'my/corfu-minibuffer-setup-hook 1)
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

(require 'flyspell)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-mode)

;; C-, -> flyspell-goto-next-error
;; C-. -> flyspell-auto-correct-word
;; C-; -> flyspell-auto-correct-previous-word

(define-key flyspell-mode-map (kbd "C-,") nil)
(define-key flyspell-mode-map (kbd "C-.") nil)
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
;; yaml-pro

;; https://github.com/zkry/yaml-pro

(require 'yaml-pro)

(use-package yaml-ts-mode
  :mode "\\.yaml\\'")

(add-hook 'yaml-ts-mode-hook 'yaml-pro-mode)

;; https://github.com/zkry/yaml-pro#usage-1

(define-key yaml-pro-mode-map (kbd "C-c l") 'yaml-pro-unindent-subtree)  ; [l]eft move
(define-key yaml-pro-mode-map (kbd "C-c r") 'yaml-pro-indent-subtree)    ; [r]ight move
(define-key yaml-pro-mode-map (kbd "C-c u") 'yaml-pro-move-subtree-up)   ; [u]p move
(define-key yaml-pro-mode-map (kbd "C-c d") 'yaml-pro-move-subtree-down) ; [d]own move
(define-key yaml-pro-mode-map (kbd "C-c c") 'yaml-pro-fold-at-point)     ; [c]ollapse
(define-key yaml-pro-mode-map (kbd "C-c e") 'yaml-pro-unfold-at-point)   ; [e]xpand
(define-key yaml-pro-mode-map (kbd "C-c k") 'yaml-pro-kill-subtree)      ; [k]ill
(define-key yaml-pro-mode-map (kbd "C-M-n") 'yaml-pro-next-subtree)      ; [n]ext sibling; was forward-list
(define-key yaml-pro-mode-map (kbd "C-M-p") 'yaml-pro-prev-subtree)      ; [p]revious sibling; was backward-list
(define-key yaml-pro-mode-map (kbd "C-M-u") 'yaml-pro-up-level)          ; [u]p level; was backward-up-list

;; >--------------------------------------------------



;; <--------------------------------------------------
;; minibuffer

(add-hook 'minibuffer-setup-hook 'subword-mode)

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Recursive-Mini.html
;;
;; enable-recursive-minibuffers

(define-key minibuffer-mode-map (kbd "C-c h") 'consult-history)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; global keys

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Keys.html
;;
;; help-map is the global keymap for the C-h prefix key.
;; mode-specific-map is the global keymap for the prefix key C-c.
;; ctl-x-map is the global keymap used for the C-x prefix key.
;; goto-map is the global keymap used for the M-g prefix key.

;; s-i -> misc prefix key

(global-unset-key (kbd "C-x C-c"))                              ; quit emacs
(global-unset-key (kbd "C-x C-z"))                              ; suspend-frame (minimize)
(global-unset-key (kbd "s-h"))                                  ; ns-do-hide-emacs

(global-set-key (kbd "s-z") 'execute-extended-command) ; more handy
(global-set-key (kbd "C-z") ctl-x-map)                 ; more handy
(global-set-key (kbd "M-'") 'subword-mode)             ; was abbrev-prefix-mark
(global-set-key (kbd "C-x d") 'ediff-buffers)          ; e[d]iff; was dired
(global-set-key (kbd "C-x f") 'find-file)              ; was set-fill-column
(global-set-key (kbd "C-,") 'replace-string)           ; was flyspell-goto-next-error
(global-set-key (kbd "C-\\") 'toggle-truncate-lines)   ; was toggle-input-method
(global-set-key (kbd "C-.") 'pop-to-mark-command)      ; was flyspell-auto-correct-word
(global-set-key (kbd "s-i p") 'previous-error)
(global-set-key (kbd "s-i n") 'next-error)

;; C-x r o -> open-rectangle
;; C-x r k -> kill-rectangle
;; f4 -> kmacro-end-or-call-macro

;; M-[ -> t
;; M-] -> nil
;; M-\ -> delete-horizontal-space
;; M-; -> comment-dwim
;; M-' -> t
;; M-, -> xref-pop-marker-stack
;; M-. -> xref-find-definitions
;; M-/ -> dabbrev-completion

;; C-[ -> ESC-
;; C-] -> abort-recursive-edit
;; C-\ -> t
;; C-; -> flyspell-auto-correct-previous-word
;; C-' -> nil; external binding: macOS Keyboard Shortcuts - Input Sources - Select the previous input source
;; C-, -> t
;; C-. -> t
;; C-/ -> undo

;; s-2 nil; external binding: macOS Keyboard Shortcuts - Mission Control - Switch to Desktop 2 (emacs)
;; s-3 nil; external binding: macOS Keyboard Shortcuts - Mission Control - Switch to Desktop 3 (chrome)

;; >--------------------------------------------------



;; <--------------------------------------------------
;; global vars

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Completion.html#index-completion_002dignored_002dextensions
;;
(add-to-list 'completion-ignored-extensions ".DS_Store")
;; >--------------------------------------------------



;; <--------------------------------------------------
;; diff-mode

(require 'diff-mode)

(define-key diff-mode-shared-map (kbd "t") 'diff-delete-trailing-whitespace) ; [t]rim
;; >--------------------------------------------------



;; <--------------------------------------------------
;; expand-region

;; https://github.com/magnars/expand-region.el

(global-set-key (kbd "M-[") 'er/expand-region)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; shell-mode

(require 'shell)

(define-key shell-mode-map (kbd "C-c h") 'consult-history)
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
(global-set-key (kbd "s-h b") #'vertico-repeat) ; [b]ack to last interaction
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

(global-set-key (kbd "s-j") 'consult-buffer)                         ; [j]ump; was exchange-point-and-mark, prefer C-x C-x
(global-set-key (kbd "s-f") 'consult-line)                           ; [f]ind; was isearch-forward
(define-key isearch-mode-map (kbd "s-f") 'consult-line)              ; [f]ind
(define-key isearch-mode-map (kbd "C-c h") 'consult-isearch-history) ; [h]istory
(global-set-key (kbd "s-g") 'consult-ripgrep)                        ; [g]rep; was isearch-repeat-forward
(global-set-key (kbd "M-y") 'consult-yank-replace)                   ; [y]ank
(global-set-key (kbd "s-h i") 'consult-imenu)                        ; [i]menu
(global-set-key (kbd "s-h I") 'consult-imenu-multi)                  ; [I]menu
(global-set-key (kbd "s-h e") 'consult-flymake)                      ; [e]rrors
(global-set-key (kbd "s-h r") 'consult-register-store)               ; [r]egister
(global-set-key (kbd "s-h t") 'consult-register)                     ; [t] next to [r]
(global-set-key (kbd "s-h f") 'consult-line-multi)                   ; [f]ind
(global-set-key (kbd "s-h k") 'consult-keep-lines)                   ; [k]eep
(global-set-key (kbd "s-h h") 'consult-focus-lines)                  ; [h]ide
(global-set-key (kbd "s-h m") 'consult-mark)                         ; [m]ark
(global-set-key (kbd "s-h M") 'consult-global-mark)                  ; [M]ark
(global-set-key (kbd "s-h o") 'consult-outline)                      ; [o]utline

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

(require 'cc-mode)

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
(global-set-key (kbd "M-o") 'symbol-overlay-put)               ; [o]verlay
(global-set-key (kbd "s-o n") 'symbol-overlay-switch-forward)  ; [n]ext
(global-set-key (kbd "s-o p") 'symbol-overlay-switch-backward) ; [p]revious
(global-set-key (kbd "s-o r") 'symbol-overlay-remove-all)      ; [r]emove
;; >--------------------------------------------------



;; <--------------------------------------------------
;; magit

(global-set-key (kbd "s-m") 'magit-status)     ; [m]agit; was iconify-frame
;; >--------------------------------------------------



;; <--------------------------------------------------
;; avy

;; https://github.com/abo-abo/avy

;; https://github.com/abo-abo/avy#bindings
;;
;; (avy-setup-default)
(define-key isearch-mode-map (kbd "M-j") 'avy-isearch) ; [j]ump

(global-set-key (kbd "M-j") 'avy-goto-char-timer)      ; was default-indent-new-line
(global-set-key (kbd "M-g") 'avy-goto-line)            ; was goto-line

;; ? -> actions
;; https://karthinks.com/software/avy-can-do-anything/#avy-actions
;; >--------------------------------------------------



;; <--------------------------------------------------
;; ace-window

;; https://github.com/abo-abo/ace-window

;; https://github.com/abo-abo/ace-window#customization
;;
(global-set-key (kbd "s-n") 'ace-window) ; [n]ext window; was make-frame
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;; >--------------------------------------------------



;; <--------------------------------------------------
;; multiple-cursors

;; https://github.com/magnars/multiple-cursors.el

(global-set-key (kbd "s-i i") 'mc/edit-lines)
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

;; https://github.com/jrblevin/markdown-mode/issues/578#issuecomment-1126380098
;; 
(setq native-comp-jit-compilation-deny-list '("markdown-mode\\.el$"))

(require 'markdown-mode)

(define-key markdown-mode-map (kbd "C-c b") 'markdown-insert-gfm-code-block)       ; code [b]lock
(define-key markdown-mode-map (kbd "C-c q") 'markdown-insert-blockquote)           ; [q]uote
(define-key markdown-mode-map (kbd "C-c e") 'markdown-insert-bold)                 ; [e]mphasize
(define-key markdown-mode-map (kbd "C-c l") 'markdown-insert-link)                 ; [l]ink
(define-key markdown-mode-map (kbd "C-c n") 'markdown-next-visible-heading)        ; [n]ext heading
(define-key markdown-mode-map (kbd "C-c p") 'markdown-previous-visible-heading)    ; [p]revious heading
(define-key markdown-mode-map (kbd "C-c h") 'markdown-insert-header-dwim)          ; insert [h]eader
(define-key markdown-mode-map (kbd "C-M-n") 'markdown-outline-next-same-level)     ; [n]ext sibling; was forward-list
(define-key markdown-mode-map (kbd "C-M-p") 'markdown-outline-previous-same-level) ; [p]revious sibling; was backward-list
(define-key markdown-mode-map (kbd "C-M-u") 'markdown-up-heading)                  ; [u]p level; was backward-up-list

;; >--------------------------------------------------



;; <--------------------------------------------------
;; compilation-mode

;; compilation-mode-map
;; >--------------------------------------------------



;; <--------------------------------------------------
;; wgrep

;; https://github.com/mhayashi1120/Emacs-wgrep

;; https://github.com/mhayashi1120/Emacs-wgrep#usage
;;
;; C-c C-p -> edit
;; C-c C-c -> commit
;; C-c C-k -> abort
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

;; https://joaotavora.github.io/eglot/#index-starting-Eglot
;;
;; (add-hook 'java-mode-hook 'eglot-ensure)

(global-unset-key (kbd "s-l"))                                ; was goto-line
(global-set-key (kbd "s-l e") 'eglot)                         ; [e]glot
(define-key eglot-mode-map (kbd "s-l a") 'eglot-code-actions) ; [a]ctions
(define-key eglot-mode-map (kbd "s-l r") 'eglot-rename)       ; [r]ename

;; <-------------------------
;; eglot-java

;; https://github.com/yveszoundi/eglot-java

(define-key java-mode-map (kbd "s-l e") 'eglot-java-mode)

;; You can specify JVM arguments for the LSP server (eglot-java-eclipse-jdt-args variable)

;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; nxml-mode

;; ref rng-nxml-mode-init
(defun my/nxml-mode-hook ()
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))
(add-hook 'nxml-mode-hook 'my/nxml-mode-hook)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; vundo

;; https://github.com/casouri/vundo

(global-set-key (kbd "C-x u") 'vundo) ; was undo
;; >--------------------------------------------------



;; <--------------------------------------------------
;; misc

(electric-pair-mode)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; >--------------------------------------------------
