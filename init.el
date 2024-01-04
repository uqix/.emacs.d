;;; -*- lexical-binding: t -*-

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
;; builtin ts modes

(require 'dockerfile-ts-mode)
(require 'go-ts-mode)
(require 'typescript-ts-mode)
(require 'java-ts-mode)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; dabbrev

;; * Use Dabbrev with Corfu!

;; M-/ -> dabbrev-expand

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

;; https://github.com/minad/corfu/blob/main/extensions/corfu-quick.el
;;
(keymap-set corfu-map "M-g" #'corfu-quick-complete)

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
    (setq-local corfu-auto nil
                corfu-echo-delay nil
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'my/corfu-minibuffer-setup-hook 1)

;; https://github.com/minad/corfu#extensions
;; 
(corfu-popupinfo-mode)
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
;; (load-theme 'doom-one-light t)
(load-theme 'doom-one t)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; Frame

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; https://www.emacswiki.org/emacs/FrameTitle#h5o-6
(setq frame-title-format "%b <%f>")

;; >--------------------------------------------------



;; <--------------------------------------------------
;; flyspell

(require 'flyspell)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-mode)

;; C-; -> flyspell-auto-correct-previous-word

(define-key flyspell-mode-map (kbd "C-,") nil) ; flyspell-goto-next-error
(define-key flyspell-mode-map (kbd "C-.") nil) ; flyspell-auto-correct-word
;; >--------------------------------------------------



;; <--------------------------------------------------
;; flymake

;; https://www.gnu.org/software/emacs/manual/html_node/flymake/index.html

(require 'flymake)

;; https://www.gnu.org/software/emacs/manual/html_node/flymake/Finding-diagnostics.html
;;
(define-key flymake-mode-map (kbd "M-N") 'flymake-show-buffer-diagnostics)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

(add-hook 'json-ts-mode-hook 'flymake-mode)
(add-hook 'yaml-ts-mode-hook 'flymake-mode)

;; <-------------------------
;; flymake-collection

;; https://github.com/mohkale/flymake-collection

(use-package json-ts-mode
  :flymake-hook
  (json-ts-mode flymake-collection-jq))

(use-package yaml-ts-mode
  :flymake-hook
  (yaml-ts-mode flymake-collection-yamllint))

(use-package flymake-collection
  :hook (after-init . flymake-collection-hook-setup))
;; >-------------------------

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

(global-set-key (kbd "s-z") 'execute-extended-command)    ; more handy
(global-set-key (kbd "C-z") ctl-x-map)                    ; more handy
(global-set-key (kbd "M-'") 'subword-mode)                ; was abbrev-prefix-mark
(global-set-key (kbd "C-x d") 'duplicate-dwim)            ; [d]uplicate; was dired
(global-set-key (kbd "C-x e") 'ediff-buffers)             ; [e]diff; was kmacro-end-and-call-macro
(global-set-key (kbd "C-x f") 'find-file)                 ; find [f]ile; was set-fill-column
(global-set-key (kbd "C-,") 'replace-string)              ; was flyspell-goto-next-error
(global-set-key (kbd "C-\\") 'toggle-truncate-lines)      ; was toggle-input-method
(global-set-key (kbd "C-.") 'pop-to-mark-command)         ; was flyspell-auto-correct-word
(global-set-key (kbd "s-i p") 'previous-error)            ; [p]revious error
(global-set-key (kbd "s-i n") 'next-error)                ; [n]ext error
(global-set-key (kbd "s-i m t") 'text-mode)               ; [t]ext-[m]ode
(global-set-key (kbd "s-i m y") 'yaml-ts-mode)            ; [y]aml-ts-[m]ode
(global-set-key (kbd "s-i e") 'hs-toggle-hiding)          ; [e]xpand/collapse code block
(global-set-key (kbd "s-i w") 'whitespace-mode)           ; [w]hitespace-mode
(global-set-key (kbd "M-L") 'downcase-region)             ; [L]owercase region
(global-set-key (kbd "M-U") 'upcase-region)               ; [U]ppercase region
(global-set-key (kbd "M-\\") 'delete-trailing-whitespace) ; was delete-horizontal-space, prefer M-SPC

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

;; s-1 nil; external binding: macOS Keyboard Shortcuts - Mission Control - Switch to Desktop 1
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

;; https://github.com/minad/vertico/blob/main/extensions/vertico-quick.el
;;
(keymap-set vertico-map "M-g" #'vertico-quick-insert)

;; >-------------------------

;; <-------------------------
;; marginalia

;; https://github.com/minad/marginalia#configuration
;; 
(marginalia-mode)
;; >-------------------------

;; <-------------------------
;; embark

(require 'embark)

;; https://github.com/oantolin/embark#quick-start
;; 
(global-set-key (kbd "M-i") 'embark-act) ; was tab-to-tab-stop

(define-key embark-file-map (kbd "$") 'shell)

(define-key embark-region-map (kbd "e") nil)              ; was eval-region
(define-key embark-region-map (kbd "e e") #'eval-region)  ; [e]val elisp
(define-key embark-region-map (kbd "a") nil)              ; was align
(define-key embark-region-map (kbd "a a") #'align)        ; [a]lign
(define-key embark-region-map (kbd "a r") #'align-regexp) ; [a]lign-[r]egexp

;; >-------------------------

;; <-------------------------
;; embark-consult

;; https://github.com/oantolin/embark#consult
;; >-------------------------

(defun my/region/with-str (fn &optional fn-without-str)
  (let ((command
         (lambda (&optional begin end)
           (interactive (if (use-region-p) (list (region-beginning) (region-end))))
           (if begin
               (let ((region-str (buffer-substring begin end)))
                 (deactivate-mark)
                 (funcall fn region-str))
             (call-interactively (or fn-without-str fn))))))
    (call-interactively command)))

(defun my/consult-line ()
  (interactive)
  (my/region/with-str 'consult-line))

(defun my/consult-ripgrep/without-dir (initial)
  (consult-ripgrep nil initial))

(defun my/consult-ripgrep ()
  (interactive)
  (my/region/with-str 'my/consult-ripgrep/without-dir 'consult-ripgrep))

(global-set-key (kbd "s-j") 'consult-buffer)                         ; [j]ump; was exchange-point-and-mark, prefer C-x C-x
(global-set-key (kbd "s-f") 'my/consult-line)                        ; [f]ind; was isearch-forward
(define-key isearch-mode-map (kbd "s-f") 'consult-line)              ; [f]ind
(define-key isearch-mode-map (kbd "C-c h") 'consult-isearch-history) ; [h]istory
(global-set-key (kbd "s-g") 'my/consult-ripgrep)                     ; [g]rep; was isearch-repeat-forward
(global-set-key (kbd "M-y") 'consult-yank-replace)                   ; [y]ank
(global-set-key (kbd "s-h i") 'consult-imenu)                        ; [i]menu
(global-set-key (kbd "s-h I") 'consult-imenu-multi)                  ; [I]menu
(global-set-key (kbd "s-h e") 'consult-flymake)                      ; [e]rrors
(global-set-key (kbd "s-h r") 'consult-register-store)               ; [r]egister
(global-set-key (kbd "s-h R") 'consult-register)                     ; [R]egister
(global-set-key (kbd "s-h f") 'consult-line-multi)                   ; [f]ind
(global-set-key (kbd "s-h k") 'consult-keep-lines)                   ; [k]eep
(global-set-key (kbd "s-h F") 'consult-focus-lines)                  ; [F]ocus
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
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0)
  (c-set-offset 'case-label '+))
(add-hook 'java-mode-hook 'my/java-mode-hook)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; symbol-overlay

;; https://github.com/wolray/symbol-overlay/#usage

(global-unset-key (kbd "s-o"))

(global-set-key (kbd "M-o") 'symbol-overlay-put)               ; [o]verlay
(global-set-key (kbd "s-o n") 'symbol-overlay-switch-forward)  ; [n]ext
(global-set-key (kbd "s-o p") 'symbol-overlay-switch-backward) ; [p]revious
(global-set-key (kbd "s-o r") 'symbol-overlay-remove-all)      ; [r]emove
(global-set-key (kbd "s-o m") 'symbol-overlay-mode)            ; [m]ode

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
(global-set-key (kbd "s-n") 'ace-window) ; [n]umber windows; was make-frame
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
;; doom-modeline

;; https://github.com/seagle0128/doom-modeline#install
;;
;; M-x nerd-icons-install-fonts
(add-hook 'after-init-hook #'doom-modeline-mode)
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
;; (add-hook 'java-mode-hook 'eglot-ensure)

(global-unset-key (kbd "s-l"))                                       ; [l]sp; was goto-line
(global-set-key (kbd "s-l e") 'eglot)                                ; [e]glot
(define-key eglot-mode-map (kbd "s-l a") 'eglot-code-actions)        ; [a]ctions
(define-key eglot-mode-map (kbd "s-l r") 'eglot-rename)              ; [r]ename
(define-key eglot-mode-map (kbd "s-l i") 'eglot-find-implementation) ; [i]mplementation
(define-key eglot-mode-map (kbd "s-l t") 'eglot-find-typeDefinition) ; [t]ype

(with-eval-after-load 'eglot
  (add-to-list
   'eglot-server-programs
   '(java-ts-mode
     .
     ("jdtls"
      "--jvm-arg=-javaagent:/Users/zjq/opt/lombok.jar"
      "--jvm-arg=-Xmx4G"
      "--jvm-arg=-XX:+UseStringDeduplication"
      :initializationOptions (:extendedClientCapabilities (:classFileContentsSupport t))))))

;; <-------------------------
;; java jdtls, handle uri jdt://

;; https://github.com/yveszoundi/eglot-java/blob/ff0f9515d78f94b8dfe158bf9a2c4f52216504c0/eglot-java.el#L770
;;
(defun eglot-java--jdt-uri-handler (operation &rest args)
  "Support Eclipse jdtls `jdt://' uri scheme."
  (let* ((uri (car args))
         (cache-dir (expand-file-name ".eglot-java" (temporary-file-directory)))
         (source-file
          (expand-file-name
           (eglot-java--make-path
            cache-dir
            (save-match-data
              (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri)
                (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))))))
    (unless (file-readable-p source-file)
      (let ((content (jsonrpc-request (eglot-current-server) :java/classFileContents (list :uri uri)))
            (metadata-file (format "%s.%s.metadata"
                                   (file-name-directory source-file)
                                   (file-name-base source-file))))
        (unless (file-directory-p cache-dir) (make-directory cache-dir t))
        (with-temp-file source-file (insert content))
        (with-temp-file metadata-file (insert uri))))
    source-file))

(defun eglot-java--make-path (root-dir &rest path-elements)
  (let ((new-path          (expand-file-name (if (listp root-dir)
                                                 (car root-dir)
                                               root-dir)))
        (new-path-elements (if (listp root-dir)
                               (rest root-dir)
                             path-elements)))
    (dolist (p new-path-elements)
      (setq new-path (concat (file-name-as-directory new-path) p)))
    new-path))

(add-to-list 'file-name-handler-alist '("\\`jdt://" . eglot-java--jdt-uri-handler))
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
;; s

;; https://github.com/magnars/s.el

;; https://stackoverflow.com/a/61745441/9154901
;;
(defun my/region/convert (fn)
  (let ((command
         (lambda (begin end)
           (interactive "r")
           (let ((region-str (buffer-substring begin end)))
             (delete-region begin end)
             (insert (funcall fn region-str))))))
    (call-interactively command)))

(defun my/region/convert/snake-case ()
  (interactive)
  (my/region/convert 's-snake-case))

(defun my/region/convert/camel-case ()
  (interactive)
  (my/region/convert 's-lower-camel-case))

(defun my/region/convert/kebab-case ()
  (interactive)
  (my/region/convert 's-dashed-words))

(defun my/region/convert/capitalize ()
  (interactive)
  (my/region/convert 's-capitalized-words))

(define-key embark-region-map (kbd "c") nil)

(define-key embark-region-map (kbd "c s") #'my/region/convert/snake-case) ; [c]ase: to [s]nake
(define-key embark-region-map (kbd "c c") #'my/region/convert/camel-case) ; [c]ase: to [c]amel
(define-key embark-region-map (kbd "c k") #'my/region/convert/kebab-case) ; [c]ase: to [k]ebab
(define-key embark-region-map (kbd "c a") #'my/region/convert/capitalize) ; [c]ase: to c[a]pitalized

;; >--------------------------------------------------



;; <--------------------------------------------------
;; polymode

;; https://github.com/polymode/polymode

(setq polymode-prefix-key (kbd "s-,"))

(require 'polymode)

(defun my/polymode/edit-chunk ()
  (interactive)
  (call-interactively 'polymode-mark-or-extend-chunk)
  (call-interactively 'edit-indirect-region))

(define-key polymode-mode-map (kbd "s-, n") 'polymode-next-chunk)             ; [n]ext
(define-key polymode-mode-map (kbd "s-, p") 'polymode-previous-chunk)         ; [p]revious
(define-key polymode-mode-map (kbd "s-, w") 'polymode-toggle-chunk-narrowing) ; narrow or [w]iden
(define-key polymode-mode-map (kbd "s-, k") 'polymode-kill-chunk)             ; [k]ill
(define-key polymode-mode-map (kbd "s-, m") 'polymode-mark-or-extend-chunk)   ; [m]ark
(define-key polymode-mode-map (kbd "s-, e") 'my/polymode/edit-chunk)          ; [e]dit by edit-indirect

;; https://polymode.github.io/defining-polymodes/

(define-innermode poly-bash-innermode
  :mode 'bash-ts-mode
  :head-matcher "^ *#!/usr/bin/env \\(sh\\|bash\\)\n"
  :tail-matcher "^ *# </bash>$"
  :head-mode 'body
  :tail-mode 'body)

;; <-------------------------
;; yaml

(define-hostmode poly-yaml-ts-hostmode
  :mode 'yaml-ts-mode)
(define-polymode poly-yaml-ts-mode
  :hostmode 'poly-yaml-ts-hostmode
  :innermodes '(poly-bash-innermode))

;; https://github.com/polymode/polymode/issues/324#issuecomment-1872441449
;; (add-hook 'yaml-ts-mode-hook 'poly-yaml-ts-mode)

(define-key yaml-ts-mode-map (kbd "C-c p") 'poly-yaml-ts-mode)
;; >-------------------------

;; <-------------------------
;; groovy

(define-hostmode poly-groovy-hostmode
  :mode 'groovy-mode)
(define-polymode poly-groovy-mode
  :hostmode 'poly-groovy-hostmode
  :innermodes '(poly-bash-innermode))

;; https://github.com/polymode/polymode/issues/324#issuecomment-1872441449
;; (add-hook 'groovy-mode-hook 'poly-groovy-mode)

(require 'groovy-mode)
(define-key groovy-mode-map (kbd "C-c p") 'poly-groovy-mode)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; misc

(electric-pair-mode)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Fix generate-dockerfile.sh|docker-dockerfile.md opened in this mode
(delete '("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"
          . dockerfile-ts-mode)
        auto-mode-alist)
(add-to-list 'auto-mode-alist
             '("Dockerfile$" . dockerfile-ts-mode))

(define-key isearch-mode-map (kbd "C-,") #'isearch-query-replace)

(require 'whitespace)
(setq whitespace-style (delete 'lines whitespace-style))

;; >--------------------------------------------------



;; <--------------------------------------------------
;; bash-ts-mode

(require 'sh-script)
(advice-remove 'bash-ts-mode #'sh--redirect-bash-ts-mode)

(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
;; >--------------------------------------------------



;; <--------------------------------------------------
;; edit-indirect

(require 'edit-indirect)

(define-key embark-region-map (kbd "e i") #'edit-indirect-region) ; [e]dit-[i]ndirect

(defun my/edit-indirect/guess-mode (_parent-buffer _beg _end)
  (setq-local buffer-file-name (format "%s.-ei-" (buffer-file-name _parent-buffer)))
  (funcall (buffer-local-value 'major-mode _parent-buffer)))
(setq edit-indirect-guess-mode-function #'my/edit-indirect/guess-mode)

;; <-------------------------
;; https://github.com/Fanael/edit-indirect/issues/6#issuecomment-387945773

(require 's)
(require 'dash)

(defvar edit-indirect--left-margin 0)

(defun vbe/compute-left-margin (code)
  "Compute left margin of a string of code."
  (-min
   (-map #'(lambda (line) (length (car (s-match "^\\s-*" line))))
         (-remove 's-blank? (s-lines code)))))

(defun my/edit-indirect/commit-on-save ()
  (edit-indirect--commit)
  (delete-overlay edit-indirect--overlay)
  (setq edit-indirect--overlay nil)
  (if edit-indirect--should-quit-window
      (quit-windows-on (current-buffer) t)
    (kill-buffer))
  t)

(defun vbe/edit-indirect/remove-left-margin ()
  "Remove left-margin and save it into a local variable."
  (let ((lm (vbe/compute-left-margin (buffer-substring (point-min) (point-max)))))
    (indent-rigidly (point-min) (point-max) (* -1 lm))
    (setq-local edit-indirect--left-margin lm)
    (setq-local write-contents-functions '(my/edit-indirect/commit-on-save))
    ;; https://github.com/Fanael/edit-indirect/issues/6#issuecomment-1055542145
    ;; buffer-local variable whose value should not be reset when changing major modes
    (put 'edit-indirect--left-margin 'permanent-local t)))

(defun vbe/edit-indirect/restore-left-margin ()
  "Restore left-margin before commiting."
  (indent-rigidly (point-min) (point-max) edit-indirect--left-margin))

(add-hook 'edit-indirect-after-creation-hook #'vbe/edit-indirect/remove-left-margin)
(add-hook 'edit-indirect-before-commit-hook #'vbe/edit-indirect/restore-left-margin)

;; https://github.com/Fanael/edit-indirect/issues/6#issuecomment-1284144173
(define-key edit-indirect-mode-map [remap save-buffer] #'edit-indirect-commit)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; Select text

(defun my/select-text/between (separator-char)
  (re-search-backward (format "%c\\|^" separator-char))
  (when (= (char-after) separator-char)
    (forward-char))
  (set-mark (point))
  (re-search-forward (format "%c\\|$" separator-char))
  (when (= (char-before) separator-char)
    (backward-char)))

(defun my/select-text/between-spaces ()
  (interactive)
  (my/select-text/between ?\s))

(defun my/select-text/between-slashes ()
  (interactive)
  (my/select-text/between ?/))

(defun my/select-text/between-commas ()
  (interactive)
  (my/select-text/between ?,))

(defun my/select-text/between-single-quotes ()
  (interactive)
  (my/select-text/between ?'))

(defun my/select-text/between-double-quotes ()
  (interactive)
  (my/select-text/between 34)) ; ?" is mis-parsed as string begin by elisp-mode

(defalias 'my/select-text/java-text-block
  (kmacro "M-[ C-n C-a C-x C-x C-p C-e"))

(global-set-key (kbd "s-i s s") 'my/select-text/between-spaces)         ; [s]elect text between [s]paces
(global-set-key (kbd "s-i s /") 'my/select-text/between-slashes)        ; [s]elect text between /
(global-set-key (kbd "s-i s ,") 'my/select-text/between-commas)         ; [s]elect text between ,
(global-set-key (kbd "s-i s '") 'my/select-text/between-single-quotes)  ; [s]elect text between '
(global-set-key (kbd "s-i s \"") 'my/select-text/between-double-quotes) ; [s]elect text between "
(global-set-key (kbd "s-i s j") 'my/select-text/java-text-block)        ; [s]elect text in [j]ava text block

;; >--------------------------------------------------



;; <--------------------------------------------------
;; yaml-pro

;; https://github.com/zkry/yaml-pro

(require 'yaml-pro)

(add-hook 'yaml-ts-mode-hook #'yaml-pro-ts-mode)

;; https://github.com/zkry/yaml-pro#usage-1

(define-key yaml-pro-ts-mode-map (kbd "C-c l") #'yaml-pro-ts-unindent-subtree)  ; [l]eft move
(define-key yaml-pro-ts-mode-map (kbd "C-c r") #'yaml-pro-ts-indent-subtree)    ; [r]ight move
(define-key yaml-pro-ts-mode-map (kbd "C-c u") #'yaml-pro-ts-move-subtree-up)   ; [u]p move
(define-key yaml-pro-ts-mode-map (kbd "C-c d") #'yaml-pro-ts-move-subtree-down) ; [d]own move
(define-key yaml-pro-ts-mode-map (kbd "C-c k") #'yaml-pro-ts-kill-subtree)      ; [k]ill
(define-key yaml-pro-ts-mode-map (kbd "C-M-n") #'yaml-pro-ts-next-subtree)      ; [n]ext sibling; was forward-list
(define-key yaml-pro-ts-mode-map (kbd "C-M-p") #'yaml-pro-ts-prev-subtree)      ; [p]revious sibling; was backward-list
(define-key yaml-pro-ts-mode-map (kbd "C-M-u") #'yaml-pro-ts-up-level)          ; [u]p level; was backward-up-list
(define-key yaml-pro-ts-mode-map (kbd "C-c m") #'yaml-pro-ts-mark-subtree)      ; [m]ark
(define-key yaml-pro-ts-mode-map (kbd "C-c y") #'yaml-pro-ts-paste-subtree)     ; [y]ank

;; This is not available for tree-sitter variant.
;; Presumably some tree-sitter folding package will exist in the future.
;; 
;; (define-key yaml-pro-ts-mode-map (kbd "C-c c") 'yaml-pro-ts-fold-at-point)     ; [c]ollapse
;; (define-key yaml-pro-ts-mode-map (kbd "C-c e") 'yaml-pro-ts-unfold-at-point)   ; [e]xpand

;; >--------------------------------------------------



;; <--------------------------------------------------
;; markdown-mode

(require 'markdown-mode)

(define-key markdown-mode-map (kbd "C-c c") #'markdown-insert-code)                 ; inline [c]ode
(define-key markdown-mode-map (kbd "C-c b") #'markdown-insert-gfm-code-block)       ; code [b]lock
(define-key markdown-mode-map (kbd "C-c q") #'markdown-insert-blockquote)           ; [q]uote
(define-key markdown-mode-map (kbd "C-c e") #'markdown-insert-bold)                 ; [e]mphasize
(define-key markdown-mode-map (kbd "C-c l") #'markdown-insert-link)                 ; [l]ink
(define-key markdown-mode-map (kbd "C-c n") #'markdown-next-visible-heading)        ; [n]ext heading
(define-key markdown-mode-map (kbd "C-c p") #'markdown-previous-visible-heading)    ; [p]revious heading
(define-key markdown-mode-map (kbd "C-c h") #'markdown-insert-header-dwim)          ; insert [h]eader
(define-key markdown-mode-map (kbd "C-c i") #'markdown-insert-image)                ; insert [i]mage
(define-key markdown-mode-map (kbd "C-c I") #'markdown-toggle-inline-images)        ; show [I]mages
(define-key markdown-mode-map (kbd "C-M-n") #'markdown-outline-next-same-level)     ; [n]ext sibling; was forward-list
(define-key markdown-mode-map (kbd "C-M-p") #'markdown-outline-previous-same-level) ; [p]revious sibling; was backward-list
(define-key markdown-mode-map (kbd "C-M-u") #'markdown-up-heading)                  ; [u]p level; was backward-up-list

;; >--------------------------------------------------



;; <--------------------------------------------------
;; docker

;; https://github.com/Silex/docker.el

(global-set-key (kbd "s-i d") #'docker)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; jinja2-mode

;; https://github.com/paradoxxxzero/jinja2-mode

(require 'jinja2-mode)

(define-key jinja2-mode-map (kbd "M-o") nil) ; facemenu-set-
;; >--------------------------------------------------



;; <--------------------------------------------------
;; git-timemachine

;; https://codeberg.org/pidu/git-timemachine
;; >--------------------------------------------------
