;;; -*- lexical-binding: t; outline-regexp: ";; #+ "; -*-

;; <--------------------------------------------------
;; # package

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setopt package-archive-priorities '(("melpa" . 1)))

(package-initialize)

(setopt package-install-upgrade-built-in t)
(setopt package-native-compile t)

;; <-------------------------
;; ## custom-file

;; Use it only for package-selected-packages

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # treesit

(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (json "https://github.com/tree-sitter/tree-sitter-json")))

(defun my/treesit/install-language-grammars ()
  (interactive)
  (mapc #'treesit-install-language-grammar
        (mapcar #'car treesit-language-source-alist)))
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Util libs

(require 's)
(require 'dash)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # modus-themes

(require 'modus-themes)
(load-theme 'modus-vivendi t)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # DAbbrev

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html

(require 'dabbrev)

;; M-/ -> dabbrev-expand

(setopt dabbrev-case-fold-search nil)

(keymap-global-set "C-M-/" #'my/dabbrev/completion) ; was dabbrev-completion

(defun my/dabbrev/completion ()
  (interactive)
  (dabbrev-completion 16))
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # repeat

(require 'repeat)

(repeat-mode)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Completion

(add-to-list 'completion-ignored-extensions ".DS_Store")

;; <-------------------------
;; ## completion-at-point

;; <----------
;; ### cape

;; https://github.com/minad/cape

(require 'cape)

(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)

(setopt cape-dabbrev-min-length 1)
;; >----------

;; >-------------------------

;; ## Candidate matching styles

;; completion-at-point functions use completion styles

;; <----------
;; ### orderless

;; https://github.com/oantolin/orderless

(require 'orderless)

(setopt completion-styles '(orderless basic))
(setopt completion-category-overrides '((file (styles basic partial-completion))))
(setopt orderless-matching-styles
        '(orderless-literal orderless-regexp orderless-initialism orderless-prefixes))
;; >----------

;; >-------------------------

;; <-------------------------
;; ## In-buffer completion UI

;; <----------
;; ### corfu

;; https://github.com/minad/corfu

(require 'corfu)

(global-corfu-mode)

;; https://github.com/minad/corfu/blob/main/extensions/corfu-quick.el
(keymap-set corfu-map "M-g" #'corfu-quick-complete)

(setopt corfu-auto t)

;; https://github.com/minad/corfu#extensions
(corfu-popupinfo-mode)
;; >----------

;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # exec-path

(setq exec-path (append exec-path '("/usr/local/bin")))
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Frame

(add-to-list 'initial-frame-alist '(fullscreen . maximized))


;; <-------------------------
;; ## Title

;; https://www.emacswiki.org/emacs/FrameTitle#h5o-6

(setq frame-title-format '(:eval (my/frame-title-format)))

(defun my/frame-title-format ()
  (let ((buffer-name (buffer-name))
        (file-path (buffer-file-name))
        (dir-path (and default-directory (expand-file-name (directory-file-name default-directory))))
        (project-root (consult--project-root)))
    (if project-root
        (let* ((project-path (directory-file-name project-root))
               (project-name (file-name-nondirectory project-path))
               (file-subpath (and file-path (file-relative-name file-path project-path)))
               (dir-subpath (and dir-path (file-relative-name dir-path project-path)))
               (file-parent-subpath (and file-subpath (f-dirname file-subpath)))
               (dir-parent-subpath (and dir-subpath (directory-file-name (f-dirname dir-subpath))))
               (project-parent-path (f-dirname project-path))
               (dir-is-project-root (string= dir-path project-path)))
          (format "%s 💙%s💙/%s 🛖%s"
                  (my/frame-title-format/buffer-name buffer-name file-path dir-path dir-is-project-root)
                  project-name

                  (cond (file-parent-subpath
                         (my/abbreviate-path file-parent-subpath))
                        ((derived-mode-p 'vterm-mode)
                         (if (string-prefix-p my/vterm/buffer-name-prefix buffer-name)
                             (if dir-is-project-root "." dir-parent-subpath)
                           dir-subpath))
                        ((derived-mode-p 'dired-mode 'grep-mode)
                         (if dir-is-project-root "." dir-parent-subpath))
                        (t
                         ""))

                  (my/abbreviate-path
                   (my/frame-title-format/project-parent-path project-parent-path))))
      (format "%s 💢%s"
              (my/frame-title-format/buffer-name buffer-name file-path dir-path)

              (cond
               (file-path
                (my/abbreviate-path (f-dirname file-path)))
               ((derived-mode-p 'vterm-mode)
                (if (string-prefix-p my/vterm/buffer-name-prefix buffer-name)
                    (my/abbreviate-path (f-dirname dir-path))
                  (my/abbreviate-path dir-path)))
               ((derived-mode-p 'dired-mode)
                (my/abbreviate-path (f-dirname dir-path)))
               (t
                ""))))))

(defun my/abbreviate-path (path)
  (let* ((result (abbreviate-file-name path))
         (result (directory-file-name result)))
    result))

(defun my/frame-title-format/buffer-name (buffer-name file-path dir-path &optional dir-is-project-root)
  (let ((edit-indirect-prefix "*edit-indirect ")
        (filename (and file-path (file-name-nondirectory file-path)))
        (dir-name (and dir-path (file-name-nondirectory dir-path))))
    (cond ((derived-mode-p 'vterm-mode)
           (format "🖥️ %s%s"
                   (if vterm-copy-mode
                       "🛑 "
                     "")
                   (if (string-prefix-p my/vterm/buffer-name-prefix buffer-name)
                       (if dir-is-project-root "." dir-name)
                     (format "@%s" buffer-name))))
          ((derived-mode-p 'dired-mode)
           (format "📂 %s" (if dir-is-project-root "." dir-name)))
          ((derived-mode-p 'grep-mode)
           (format "🔎 %s" (if dir-is-project-root "." dir-name)))
          ((string-prefix-p edit-indirect-prefix buffer-name)
           (string-replace edit-indirect-prefix "*💥" buffer-name))
          (filename
           filename)
          (t
           buffer-name))))

(defun my/frame-title-format/project-parent-path (project-parent-path)
  (let ((repo-root (vc-git-root project-parent-path)))
    (if repo-root
        (let* ((repo-path (directory-file-name repo-root))
               (repo-name (file-name-nondirectory repo-path))
               (repo-parent-path (f-dirname repo-path))
               (project-parent-subpath (file-relative-name project-parent-path repo-path)))
          (format "%s/📚%s📚/%s" repo-parent-path repo-name project-parent-subpath))
      project-parent-path)))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Spellcheck

;; <-------------------------
;; ## ispell

(require 'ispell)

;; $ brew install aspell

(setopt ispell-dictionary "american")
(setopt ispell-program-name "aspell")
(setopt ispell-extra-args '("--camel-case"))

(keymap-global-set "s-i s c" #'ispell-word) ; [s]pell [c]heck
;; >-------------------------

;; <-------------------------
;; ## flyspell

(require 'flyspell)

(setopt flyspell-mark-duplications-flag nil)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-mode)

(keymap-set flyspell-mode-map "C-." nil) ; was flyspell-auto-correct-word

;; <----------
;; ### flyspell-correct

(require 'flyspell-correct)

;; was flyspell-auto-correct-previous-word
(keymap-set flyspell-mode-map "C-;" #'flyspell-correct-wrapper)
;; >----------

;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # flymake

;; https://www.gnu.org/software/emacs/manual/html_node/flymake/index.html

(require 'flymake)

;; https://www.gnu.org/software/emacs/manual/html_node/flymake/Finding-diagnostics.html
(keymap-set flymake-mode-map "M-N" 'flymake-show-buffer-diagnostics)
(keymap-set flymake-mode-map "M-n" 'flymake-goto-next-error)
(keymap-set flymake-mode-map "M-p" 'flymake-goto-prev-error)

(defvar-keymap my/flymake/error-repeat-map
  :repeat t
  "n" #'flymake-goto-next-error
  "p" #'flymake-goto-prev-error)

(keymap-global-set "s-i m d" #'flymake-mode) ; [d]iagnostics

;; <-------------------------
;; ## flymake-collection

;; https://github.com/mohkale/flymake-collection

(require 'flymake-collection)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # minibuffer

(setopt enable-recursive-minibuffers t)

(add-hook 'minibuffer-setup-hook #'subword-mode)
(add-hook 'minibuffer-setup-hook #'flyspell-mode)

(keymap-set minibuffer-mode-map "C-h" 'consult-history)

;; <-------------------------
;; ## vertico

(require 'vertico)

(keymap-global-unset "s-h") ; was ns-do-hide-emacs

;; https://github.com/minad/vertico#configuration
(vertico-mode)

;; https://www.emacswiki.org/emacs/SaveHist
(savehist-mode 1)

;; https://github.com/minad/vertico/blob/main/extensions/vertico-repeat.el
(keymap-global-set "s-h b" #'vertico-repeat) ; [b]ack
(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

;; https://github.com/minad/vertico/blob/main/extensions/vertico-quick.el
(keymap-set vertico-map "M-g" #'vertico-quick-exit)

;; https://github.com/minad/vertico/blob/main/extensions/vertico-multiform.el
(vertico-multiform-mode)

;; <----------
;; ### Group repeat-map

(defvar-keymap my/vertico/group-repeat-map
  :repeat t
  "{" #'vertico-previous-group
  "}" #'vertico-next-group
  "[" #'vertico-previous-group
  "]" #'vertico-next-group)
;; >----------

;; >-------------------------

;; <-------------------------
;; ## corfu

(add-hook 'minibuffer-setup-hook #'my/minibuffer/corfu-setup-hook 1)

;; https://github.com/minad/corfu#completing-in-the-minibuffer
(defun my/minibuffer/corfu-setup-hook ()
  (setq-local corfu-auto nil
              corfu-echo-delay nil
              corfu-popupinfo-delay nil)
  (corfu-mode 1))
;; >-------------------------

;; <-------------------------
;; ## yasnippet

(add-hook 'minibuffer-setup-hook #'yas-minor-mode)
(add-hook 'minibuffer-setup-hook #'my/minibuffer/yasnippet-setup-hook)

(defvar-local my/minibuffer/current-command nil)

(defun my/minibuffer/yasnippet-setup-hook ()
  (setq-local my/minibuffer/current-command this-command)
  (setq-local yas-buffer-local-condition
              '(pcase my/minibuffer/current-command
                 ((or 'my/grep/project
                      (app documentation "crd"))
                  '(require-snippet-condition . consult-ripgrep))
                 (cmd
                  `(require-snippet-condition . ,cmd)))))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # embark

(require 'embark)

(setopt embark-confirm-act-all nil)
(setopt embark-mixed-indicator-delay 1.5)

(keymap-global-set "M-i" 'embark-act) ; was tab-to-tab-stop

(keymap-set embark-region-map "e" nil)              ; was eval-region
(keymap-set embark-region-map "e e" #'eval-region)  ; [e]val [e]lisp
(keymap-set embark-region-map "a" nil)              ; was align
(keymap-set embark-region-map "a a" #'align)
(keymap-set embark-region-map "a r" #'align-regexp)

;; <-------------------------
;; ## marginalia

(require 'marginalia)

;; https://github.com/minad/marginalia#configuration
(marginalia-mode)
;; >-------------------------

;; <-------------------------
;; ## embark-consult

;; https://github.com/oantolin/embark#consult

;; Embark will automatically load it after Consult if found.

(keymap-set embark-general-map "s-f" #'consult-line)
(keymap-set embark-general-map "s-F" #'consult-line-multi)
(keymap-set embark-general-map "s-g" #'consult-ripgrep)
(keymap-set embark-general-map "s-G" #'my/grep/dir/read-initial)

(defun my/grep/dir/read-initial ()
  (interactive)
  (consult-ripgrep '(4) (read-from-minibuffer "Grep: ")))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Block hideshow

(require 'hideshow)

(add-hook 'prog-mode-hook 'hs-minor-mode)

(defvar-keymap my/hideshow/repeat-map
  :repeat t
  "t" #'hs-toggle-hiding
  "h" #'hs-hide-all
  "s" #'hs-show-all
  "1" #'my/hideshow/hide-level/1
  "2" #'my/hideshow/hide-level/2
  "3" #'my/hideshow/hide-level/3
  "4" #'my/hideshow/hide-level/4
  "5" #'my/hideshow/hide-level/5)

(keymap-global-set "s-i b" my/hideshow/repeat-map)

(defun my/hideshow/hide-level/1 ()
  (interactive)
  (hs-hide-level 1))

(defun my/hideshow/hide-level/2 ()
  (interactive)
  (hs-hide-level 2))

(defun my/hideshow/hide-level/3 ()
  (interactive)
  (hs-hide-level 3))

(defun my/hideshow/hide-level/4 ()
  (interactive)
  (hs-hide-level 4))

(defun my/hideshow/hide-level/5 ()
  (interactive)
  (hs-hide-level 5))
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Search

;; <-------------------------
;; ## isearch

(require 'isearch)

(keymap-set key-translation-map "s-u" "C-s")
(keymap-set key-translation-map "s-y" "C-r")

(keymap-set isearch-mode-map "s-r" #'isearch-query-replace)
(keymap-set isearch-mode-map "s-f" #'consult-line)
(keymap-set isearch-mode-map "C-h" #'consult-isearch-history)
(keymap-set isearch-mode-map "M-j" 'my/avy-isearch)

(defun my/avy-isearch ()
  (interactive)
  (isearch-done)
  (avy-isearch))

(setopt isearch-lazy-count t)

;; <----------
;; ### Repeat

(defvar-keymap my/isearch/repeat-map
  :repeat t
  "s" #'isearch-repeat-forward
  "r" #'isearch-repeat-backward
  "u" #'isearch-repeat-forward
  "y" #'isearch-repeat-backward)
;; >----------

;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Buffer

(keymap-global-set "s-N" #'revert-buffer-quick) ; re[N]ew
(keymap-global-set "s-i s a" #'write-file)      ; [s]ave [a]s

(setopt revert-buffer-quick-short-answers t)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Global keys

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Keys.html
;;
;; help-map is the global keymap for the C-h prefix key.
;; mode-specific-map is the global keymap for the prefix key C-c.
;; ctl-x-map is the global keymap used for the C-x prefix key.
;; goto-map is the global keymap used for the M-g prefix key.

;; s-i -> misc prefix key

(keymap-global-unset "C-x C-c") ; was quit emacs
(keymap-global-unset "C-x C-z") ; was suspend-frame (minimize)
(keymap-global-unset "C-s-SPC") ; was ns-do-show-character-palette

(keymap-set key-translation-map "s-SPC" "C-g")
(keymap-set key-translation-map "s-," "C-c") ; was customize

(keymap-global-set "C-z" ctl-x-map)
(keymap-global-set "s-z" #'execute-extended-command)

(keymap-global-set "M-'" #'subword-mode) ; was abbrev-prefix-mark

(keymap-global-set "s-d" #'duplicate-dwim) ; was isearch-repeat-backward

(keymap-global-set "s-i f" #'find-file)

(keymap-global-set "C-\\" #'toggle-truncate-lines) ; was toggle-input-method

(keymap-global-set "C-." #'pop-to-mark-command) ; was flyspell-auto-correct-word

(keymap-global-set "s-i E" #'ns-do-show-character-palette) ; [E]moji

;; <-------------------------
;; ## next-error

;; Normally uses the most recently started compilation, grep, or occur buffer

(keymap-global-set "s-i e" next-error-repeat-map)
;; >-------------------------

(keymap-global-set "s-i m t" #'text-mode)
(keymap-global-set "s-i m v" #'view-mode)
(keymap-global-set "s-i m r" #'auto-revert-mode)

(keymap-global-set "C-h F" #'describe-face)
(keymap-global-set "C-h K" #'describe-keymap)

;; <-------------------------
;; ## Rename file or buffer

(keymap-global-set "s-i R" #'my/rename-file-or-buffer)

(defun my/rename-file-or-buffer ()
  (interactive)
  (if (buffer-file-name)
      (call-interactively 'rename-visited-file)
    (call-interactively 'rename-buffer)))
;; >-------------------------

(keymap-global-set "s-S" #'scratch-buffer)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Highlight

(require 'hl-line)

;; <-------------------------
;; ## Regexp

;; <----------
;; ### Highlight (region aware)

(keymap-global-set "s-i h r" #'my/highlight-regexp)

(defface my/highlight-regexp-face
  '((t (:strike-through "DarkSlateGray1")))
  "my/highlight-regexp face")

;; <---
;; #### repeat-map

(keymap-global-set "C-'" #'my/highlight-regexp/all)

(defvar-keymap my/highlight-regexp/all/repeat-map
  :repeat (:enter
           (my/highlight-regexp/all)
           :exit
           (my/unhighlight-regexp/all))
  "'" #'my/unhighlight-regexp/all)
;; >---

(defun my/highlight-regexp ()
  (interactive)
  (my/region/with-str 'my/highlight-regexp/str))

(defun my/highlight-regexp/str (&optional str)
  (interactive)
  (let ((regexp (or
                 (and str (regexp-quote str))
                 (read-regexp "RegExp: "))))
    (highlight-regexp regexp 'my/highlight-regexp-face)))

(defun my/highlight-regexp/all ()
  (interactive)
  (my/region/with-str 'my/highlight-regexp/all/str))

(defun my/highlight-regexp/all/str (&optional str)
  (interactive)
  (let ((regexp (or
                 (and str (regexp-quote str))
                 (read-regexp "RegExp: "))))
    (walk-windows
     (lambda (window)
       (with-current-buffer (window-buffer window)
         (highlight-regexp regexp 'my/highlight-regexp-face))))))
;; >----------

;; <----------
;; ### Current column

(keymap-global-set "s-i h c" #'my/highlight-regexp/current-column)

(defvar-keymap my/highlight-regexp/current-column/repeat-map
  :repeat (:enter
           (my/highlight-regexp/current-column)
           :exit
           (my/unhighlight-regexp))
  "c" #'my/unhighlight-regexp)

(defun my/highlight-regexp/current-column ()
  (interactive)
  (let ((regexp (format "^.\\{%s\\}\\(.\\)" (current-column))))
    (highlight-regexp regexp 'lazy-highlight 1)))
;; >----------

;; <----------
;; ### Unhighlight

(keymap-global-set "s-i h u" #'my/unhighlight-regexp/all)
(keymap-global-set "s-i h U" #'unhighlight-regexp)

(defun my/unhighlight-regexp ()
  (interactive)
  (unhighlight-regexp t))

(defun my/unhighlight-regexp/all ()
  (interactive)
  (walk-windows
   (lambda (window)
     (with-current-buffer (window-buffer window)
       (unhighlight-regexp t)))))
;; >----------

;; >-------------------------

;; <-------------------------
;; ## symbol-overlay

(require 'symbol-overlay)

(setopt symbol-overlay-priority 100)

(keymap-global-unset "s-o")

(keymap-global-set "M-o" 'symbol-overlay-put)
(keymap-global-set "s-o n" 'symbol-overlay-switch-forward)  ; [n]ext
(keymap-global-set "s-o p" 'symbol-overlay-switch-backward) ; [p]revious
(keymap-global-set "s-o u" 'symbol-overlay-remove-all)
(keymap-global-set "s-o m" 'symbol-overlay-mode)

(keymap-set symbol-overlay-map "C-s" #'symbol-overlay-isearch-literally)
(keymap-set symbol-overlay-map "o" #'symbol-overlay-put)
(keymap-set symbol-overlay-map "'" #'my/highlight-regexp/all)
(keymap-set symbol-overlay-map "u" #'symbol-overlay-remove-all)
(keymap-set symbol-overlay-map "R" #'symbol-overlay-query-replace)

;; <----------
;; ### avy jump

(keymap-global-set "s-o j" #'my/symbol-overlay/avy-jump)
(keymap-set symbol-overlay-map "j" #'my/symbol-overlay/avy-jump)

(defun my/symbol-overlay/avy-jump ()
  (interactive)
  (let* ((overlays (symbol-overlay-get-list 0))
         (symbols (seq-map
                   (lambda (overlay)
                     (overlay-get overlay 'symbol))
                   overlays))
         (symbols (seq-uniq symbols))
         (symbols-regexp (seq-map
                          (lambda (symbol)
                            (symbol-overlay-regexp symbol))
                          symbols))
         (symbols-regexp (string-join symbols-regexp "\\|")))
    (if (string-empty-p symbols-regexp)
        (message "No symbol overlay")
      (avy-with my/symbol-overlay/avy-jump
        (avy-jump
         symbols-regexp
         :window-flip nil
         :beg nil
         :end nil)))))
;; >----------

;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Diff

(require 'diff-mode)

(keymap-set diff-mode-shared-map "M-\\" 'diff-delete-trailing-whitespace)

;; <-------------------------
;; ## ediff

(require 'ediff)

(keymap-global-set "s-i d b" #'ediff-buffers)
(keymap-global-set "s-i d r" #'my/ediff/show-registry)

(setopt ediff-split-window-function #'split-window-horizontally)
(setopt ediff-window-setup-function #'ediff-setup-windows-plain)

(add-hook 'ediff-after-quit-hook-internal #'winner-undo)

(defun my/ediff/show-registry ()
  (interactive)
  (unless (one-window-p)
    (walk-windows
     (lambda (window)
       (if (eq (window-dedicated-p window) 'side)
           (delete-window window))))
    (when (ediff-in-control-buffer-p)
      (other-window 1))
    (delete-other-windows))
  (ediff-show-registry))
;; >-------------------------

;; <-------------------------
;; ## ztree

(require 'ztree)

;; Prefer winner-undo
(defun ztree-diff-ediff (file-a file-b &optional startup-hooks)
  (ediff file-a file-b startup-hooks))

;; Prefer manual refresh
(defun ztree-view-on-window-configuration-changed ())

(keymap-global-set "s-i d d" #'ztree-diff)

(keymap-set ztree-mode-map "n" #'ztree-next-line)
(keymap-set ztree-mode-map "p" #'ztree-previous-line)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Shell

(require 'shell)

(keymap-set shell-mode-map "C-h" 'consult-history)
(keymap-set shell-mode-map "C-c p" 'comint-previous-prompt)
(keymap-set shell-mode-map "C-c n" 'comint-next-prompt)
(keymap-set shell-mode-map "C-c c" 'comint-clear-buffer)

;; <-------------------------
;; ## vterm

;; https://github.com/akermu/emacs-libvterm

;; $ brew install cmake

(require 'vterm)

(setopt vterm-max-scrollback 10000)
(setopt vterm-clear-scrollback-when-clearing t)

(keymap-global-set "s-i t" #'vterm)
(keymap-global-set "s-i T" #'my/vterm)

(keymap-set vterm-mode-map "C-c c" #'vterm-copy-mode)
(keymap-set vterm-copy-mode-map "<return>" #'vterm-copy-mode) ; was vterm-copy-mode-done
(keymap-set vterm-mode-map "M-g" #'avy-goto-line)
(keymap-set vterm-mode-map "M-j" #'avy-goto-char-2)
(keymap-set vterm-mode-map "M-v" #'my/vterm/scroll-down)
(keymap-set vterm-mode-map "M-y" #'vterm-yank-pop)
(keymap-set vterm-mode-map "s-f" #'my/vterm/find)
(keymap-set vterm-mode-map "M-<" #'my/vterm/beginning-of-buffer)
(keymap-set vterm-mode-map "M-n" #'vterm-next-prompt)
(keymap-set vterm-mode-map "M-p" #'vterm-previous-prompt)

(defun my/vterm/scroll-down ()
  (interactive)
  (vterm-copy-mode)
  (scroll-down-command))

(defun my/vterm/find ()
  (interactive)
  (vterm-copy-mode)
  (my/find))

(defun my/vterm/beginning-of-buffer ()
  (interactive)
  (vterm-copy-mode)
  (beginning-of-buffer))

;; <----------
;; ### my/vterm (default-directory aware)

(defvar my/vterm/buffer-name-prefix
  (format "%s " vterm-buffer-name))

(defun my/vterm ()
  (interactive)
  (let* ((dir (my/dirname default-directory))
         (name (format "%s%s" my/vterm/buffer-name-prefix dir))
         (buffer (get-buffer name)))
    (if buffer
        (switch-to-buffer buffer)
      (vterm name))))

(defun my/dirname (path)
  (let* ((result (if (f-file-p path) (f-dirname path) path))
         (result (abbreviate-file-name result))
         (result (directory-file-name result)))
    result))
;; >----------

;; <----------
;; ### embark

(add-to-list 'embark-around-action-hooks '(my/vterm embark--cd))
(keymap-set embark-file-map "s" #'my/vterm) ; was make-symbolic-link
;; >----------

;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Region

(keymap-global-set "M-]" #'exchange-point-and-mark)

(defvar-keymap my/exchange-point-and-mark/repeat-map
  :repeat t
  "]" #'exchange-point-and-mark)

;; <-------------------------
;; ## Utils

;; symbol-overlay aware
(defun my/region/with-str (fn &optional fn-without-str)
  (cond ((use-region-p)
         (let ((str (buffer-substring (region-beginning) (region-end))))
           (deactivate-mark)
           (funcall fn str)))
        ((when-let* ((symbol (symbol-overlay-get-symbol t))
                     (keyword (symbol-overlay-assoc symbol)))
           (funcall fn symbol)
           t))
        (t
         (call-interactively (or fn-without-str fn)))))

(defun my/region/set-mark (command-name)
  (if (eq last-command command-name)
      (if (region-active-p)
          (progn
            (deactivate-mark)
            (message "Mark deactivated"))
        (activate-mark)
        (message "Mark activated"))
    (set-mark-command nil)))
;; >-------------------------

;; <-------------------------
;; ## Narrowing

(put 'narrow-to-region 'disabled nil)

(keymap-global-set "s-i n f" #'narrow-to-defun)
(keymap-global-set "s-W" #'widen)
;; >-------------------------

;; <-------------------------
;; ## expand-region

;; https://github.com/magnars/expand-region.el

(require 'expand-region)

(keymap-global-set "M-[" #'er/expand-region)

(defvar-keymap my/expand-region/repeat-map
  :repeat t
  "[" #'er/expand-region)
;; >-------------------------

;; <-------------------------
;; ## Select by separator

;; <----------
;; ### Single line

(keymap-global-set "s-i s SPC" #'my/select-text/between/spaces)
(keymap-global-set "s-i s /" #'my/select-text/between/slashes)
(keymap-global-set "s-i s ," #'my/select-text/between/commas)
(keymap-global-set "s-i s '" #'my/select-text/between/single-quotes)
(keymap-global-set "s-i s \"" #'my/select-text/between/double-quotes)
(keymap-global-set "s-i s `" #'my/select-text/between/back-quotes)
(keymap-global-set "s-i s \\" #'my/select-text/between/backslashes)
(keymap-global-set "s-i s |" #'my/select-text/between/pipes)
(keymap-global-set "s-i s :" #'my/select-text/between/colons)
(keymap-global-set "s-i s =" #'my/select-text/between/equal-signs)
(keymap-global-set "s-i s ." #'my/select-text/between/dots)
(keymap-global-set "s-i s *" #'my/select-text/between/asterisks)

(keymap-global-set "s-i s (" #'my/select-text/between/parentheses)
(keymap-global-set "s-i s [" #'my/select-text/between/square-brackets)
(keymap-global-set "s-i s {" #'my/select-text/between/braces)
(keymap-global-set "s-i s <" #'my/select-text/between/angle-brackets)

(defun my/select-text/between/spaces ()
  (interactive)
  (my/select-text/between ?\s))

(defun my/select-text/between/slashes ()
  (interactive)
  (my/select-text/between ?/))

(defun my/select-text/between/commas ()
  (interactive)
  (my/select-text/between ?,))

(defun my/select-text/between/single-quotes ()
  (interactive)
  (my/select-text/between ?'))

(defun my/select-text/between/double-quotes ()
  (interactive)
  (my/select-text/between 34)) ; ?" is mis-parsed as string begin by elisp-mode

(defun my/select-text/between/back-quotes ()
  (interactive)
  (my/select-text/between ?`))

(defun my/select-text/between/backslashes ()
  (interactive)
  (my/select-text/between ?\\))

(defun my/select-text/between/pipes ()
  (interactive)
  (my/select-text/between ?|))

(defun my/select-text/between/colons ()
  (interactive)
  (my/select-text/between ?:))

(defun my/select-text/between/equal-signs ()
  (interactive)
  (my/select-text/between ?=))

(defun my/select-text/between/dots ()
  (interactive)
  (my/select-text/between ?.))

(defun my/select-text/between/asterisks ()
  (interactive)
  (my/select-text/between ?*))

(defun my/select-text/between/parentheses ()
  (interactive)
  (my/select-text/between ?\( ?\)))

(defun my/select-text/between/square-brackets ()
  (interactive)
  (my/select-text/between ?\[ ?\]))

(defun my/select-text/between/braces ()
  (interactive)
  (my/select-text/between ?{ ?}))

(defun my/select-text/between/angle-brackets ()
  (interactive)
  (my/select-text/between ?< ?>))

(defun my/select-text/between (opening-sep &optional closing-sep)
  (let ((closing-sep (or closing-sep opening-sep)))
    (re-search-backward (format "%s\\|^" (regexp-quote (string opening-sep))))
    (when (= (char-after) opening-sep)
      (forward-char))
    (set-mark (point))
    (re-search-forward (format "%s\\|$" (regexp-quote (string closing-sep))))
    (when (= (char-before) closing-sep)
      (backward-char))))
;; >----------

;; <----------
;; ### Multiline

(keymap-global-set "s-i m \"" #'my/select-text/multiline/between/triple-double-quotes)
(keymap-global-set "s-i m '" #'my/select-text/multiline/between/triple-single-quotes)
(keymap-global-set "s-i m `" #'my/select-text/multiline/between/triple-back-quotes)

(defun my/select-text/multiline/between/triple-double-quotes ()
  (interactive)
  (my/select-text/multiline/between "\"\"\""))

(defun my/select-text/multiline/between/triple-single-quotes ()
  (interactive)
  (my/select-text/multiline/between "'''"))

(defun my/select-text/multiline/between/triple-back-quotes ()
  (interactive)
  (my/select-text/multiline/between "```"))

(defun my/select-text/multiline/between (separator)
  (when (search-backward separator nil t)
    (goto-char (+ (pos-eol) 1))
    (set-mark (point))
    (search-forward separator nil t)
    (goto-char (pos-bol))))
;; >----------

;; >-------------------------

;; <-------------------------
;; ## Case converters

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; embark-region-map not works in minibuffer
(keymap-global-set "s-i c s" #'my/region/convert/snake-case)
(keymap-global-set "s-i c c" #'my/region/convert/camel-case)
(keymap-global-set "s-i c k" #'my/region/convert/kebab-case)
(keymap-global-set "s-i c C" #'my/region/convert/capitalize)

(keymap-set embark-region-map "c" nil)
(keymap-set embark-region-map "c s" #'my/region/convert/snake-case)
(keymap-set embark-region-map "c c" #'my/region/convert/camel-case)
(keymap-set embark-region-map "c k" #'my/region/convert/kebab-case)
(keymap-set embark-region-map "c C" #'my/region/convert/capitalize)

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

(defun my/region/convert (fn)
  (if (use-region-p)
      (let* ((begin (region-beginning))
             (end (region-end))
             (str (buffer-substring begin end)))
        (delete-region begin end)
        (insert (funcall fn str)))
    (message "No region")))
;; >-------------------------

;; <-------------------------
;; ## Format converters

;; $ brew install yq

(keymap-set embark-region-map "c j y" #'my/region/convert/json/to-yaml)
(keymap-set embark-region-map "c y j" #'my/region/convert/yaml/to-json)
(keymap-set embark-region-map "c y J" #'my/region/convert/yaml/to-json/compact)
(keymap-set embark-region-map "c y p" #'my/region/convert/yaml/to-properties)
(keymap-set embark-region-map "c p y" #'my/region/convert/properties/to-yaml)
(keymap-set embark-region-map "c x y" #'my/region/convert/xml/to-yaml)
(keymap-set embark-region-map "c v y" #'my/region/convert/csv/to-yaml)

(defun my/region/convert/json/to-yaml ()
  (interactive)
  (my/region/convert/by-shell-command "yq -p=json -o=yaml -"))

(defun my/region/convert/yaml/to-json ()
  (interactive)
  (my/region/convert/by-shell-command "yq -p=yaml -o=json -"))

(defun my/region/convert/yaml/to-json/compact ()
  (interactive)
  (my/region/convert/by-shell-command "yq -p=yaml -o=json -I 0 -"))

(defun my/region/convert/yaml/to-properties ()
  (interactive)
  (my/region/convert/by-shell-command "yq -p=yaml -o=props -"))

(defun my/region/convert/properties/to-yaml ()
  (interactive)
  (my/region/convert/by-shell-command "yq -p=props -o=yaml -"))

(defun my/region/convert/xml/to-yaml ()
  (interactive)
  (my/region/convert/by-shell-command "yq -p=xml -o=yaml -"))

(defun my/region/convert/csv/to-yaml ()
  (interactive)
  (my/region/convert/by-shell-command "yq -p=csv -o=yaml -"))

(defun my/region/convert/by-shell-command (command)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   command
   t
   t))
;; >-------------------------

;; <-------------------------
;; ## Code formatters

;; $ brew install sqlfluff

(keymap-set embark-region-map "f" nil)
(keymap-set embark-region-map "f s m" #'my/region/format/sql/mysql)
(keymap-set embark-region-map "f s p" #'my/region/format/sql/postgresql)
(keymap-set embark-region-map "f j" #'my/region/format/json)
(keymap-set embark-region-map "f J" #'my/region/format/json/compact)
(keymap-set embark-region-map "f y" #'my/region/format/yaml)
(keymap-set embark-region-map "f x" #'my/region/format/xml)

(defun my/region/format/sql/mysql ()
  (interactive)
  (my/region/convert/by-shell-command "sqlfluff format -n -d mysql --disable-progress-bar -"))

(defun my/region/format/sql/postgresql ()
  (interactive)
  (my/region/convert/by-shell-command "sqlfluff format -n -d postgres --disable-progress-bar -"))

(defun my/region/format/json ()
  (interactive)
  (my/region/convert/by-shell-command "jq -S"))

(defun my/region/format/json/compact ()
  (interactive)
  (my/region/convert/by-shell-command "yq -p=json -o=json -I 0 -"))

(defun my/region/format/yaml ()
  (interactive)
  (my/region/convert/by-shell-command "yq -p=yaml -o=yaml -"))

(defun my/region/format/xml ()
  (interactive)
  (my/region/convert/by-shell-command "yq -p=xml -o=xml -I4 -"))
;; >-------------------------

;; <-------------------------
;; ## Path converters

(keymap-set embark-region-map "p" nil) ; was fill-region-as-paragraph
(keymap-set embark-region-map "p f" #'fill-region-as-paragraph)
(keymap-set embark-region-map "p e" #'my/region/path/expand)
(keymap-set embark-region-map "p a" #'my/region/path/abbreviate)

(defun my/region/path/expand ()
  (interactive)
  (my/region/convert #'expand-file-name))

(defun my/region/path/abbreviate ()
  (interactive)
  (my/region/convert #'abbreviate-file-name))
;; >-------------------------

;; <-------------------------
;; ## Escape & unescape

(keymap-set embark-region-map "e j" #'my/region/unescape/json-string)
(keymap-set embark-region-map "e J" #'my/region/escape/json-string)

(defun my/region/unescape/json-string ()
  (interactive)
  (my/region/convert/by-shell-command "jq -r '.' -"))

(defun my/region/escape/json-string ()
  (interactive)
  (my/region/convert/by-shell-command "jq --raw-input --slurp ."))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # consult

;; https://github.com/minad/consult#use-package-example

(require 'consult)

;; <-------------------------
;; ## recentf

;; https://www.emacswiki.org/emacs/RecentFiles
(recentf-mode 1)
;; >-------------------------

(keymap-global-set "s-j" 'consult-buffer) ; [j]ump; was exchange-point-and-mark

(keymap-global-set "M-y" 'consult-yank-replace)

(keymap-global-set "s-h i" #'consult-imenu)
(keymap-global-set "s-h I" #'consult-imenu-multi)
(keymap-global-set "s-h e" #'consult-flymake)
(keymap-global-set "s-h f" #'consult-focus-lines)
(keymap-global-set "s-h k" #'consult-keep-lines)
(keymap-global-set "s-h m" #'consult-mark)
(keymap-global-set "s-h M" #'consult-global-mark)
(keymap-global-set "s-h o" #'consult-outline)
(keymap-global-set "s-P" #'consult-bookmark) ; [p]ositions
(keymap-global-set "s-h c" #'consult-compile-error)

(keymap-global-set "s-L" #'my/locate/dir)

(defun my/locate/dir ()
  (interactive)
  (my/region/with-str 'my/locate/dir/initial))

(defun my/locate/dir/initial (&optional initial)
  (interactive)
  (let ((dir (read-directory-name "Locate in: ")))
    (setq this-command
          (lambda ()
            (interactive)
            (consult-fd dir)))
    (consult-fd dir initial)))
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # dired

(require 'dired)

(setq insert-directory-program "gls")
(setopt dired-listing-switches "-lAhG")

(setopt dired-recursive-deletes 'always)
(setopt dired-free-space nil)

(keymap-set dired-mode-map "l" #'magit-dired-log) ; was dired-do-redisplay

(add-hook 'dired-mode-hook #'toggle-truncate-lines)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'hl-line-mode)

;; <-------------------------
;; ## wdired

(require 'wdired)

(keymap-set dired-mode-map "e" #'dired-toggle-read-only)
(keymap-set wdired-mode-map "<remap> <save-buffer>" #'wdired-finish-edit)
(keymap-set wdired-mode-map "C-c k" #'wdired-abort-changes)
;; >-------------------------

;; <-------------------------
;; ## ediff

;; https://oremacs.com/2017/03/18/dired-ediff/

(keymap-set dired-mode-map "=" #'my/dired/diff) ; was dired-diff

(defun my/dired/diff ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (if (<= (length files) 2)
        (let* ((file1 (car files))
               (file2 (if (cdr files)
                          (cadr files)
                        (read-file-name (format "Ediff %s with: " (file-name-nondirectory file1))
                                        (dired-dwim-target-directory)))))
          (window-toggle-side-windows)
          (if (f-file-p file1)
              (if (file-newer-than-file-p file1 file2)
                  (ediff-files file2 file1)
                (ediff-files file1 file2))
            (ztree-diff file1 file2)))
      (error "No more than 2 files should be marked"))))
;; >-------------------------

;; <-------------------------
;; ## dired-hacks

;; https://github.com/Fuco1/dired-hacks

;; <----------
;; ### dired-subtree

(keymap-set dired-mode-map "i" #'dired-subtree-insert) ; was dired-maybe-insert-subdir
(keymap-set dired-mode-map "TAB" #'dired-subtree-cycle)

;; <---
;; #### Tree repeat-map

(keymap-set dired-mode-map "C-M-n" #'dired-subtree-next-sibling)     ; was dired-next-subdir
(keymap-set dired-mode-map "C-M-p" #'dired-subtree-previous-sibling) ; was dired-prev-subdir
(keymap-set dired-mode-map "C-M-u" #'dired-subtree-up)               ; was dired-tree-up
(keymap-set dired-mode-map "C-M-d" #'dired-subtree-down)             ; was dired-tree-down
(keymap-set dired-mode-map "C-M-a" #'dired-subtree-beginning)        ; was beginning-of-defun
(keymap-set dired-mode-map "C-M-e" #'dired-subtree-end)              ; was end-of-defun

(keymap-set dired-mode-map "C-M-k" #'dired-subtree-remove)           ; was kill-sexp
(keymap-set dired-mode-map "C-M-<backspace>" #'dired-subtree-remove) ; was backward-kill-sexp

(keymap-set dired-mode-map "N" #'dired-subtree-narrow) ; was dired-do-man
(keymap-set dired-mode-map "C-c g" #'dired-subtree-revert)
(keymap-set dired-mode-map "TAB" #'dired-subtree-cycle)

(defvar-keymap my/dired/tree-repeat-map
  :repeat t
  "n" #'dired-subtree-next-sibling
  "p" #'dired-subtree-previous-sibling
  "u" #'dired-subtree-up
  "d" #'dired-subtree-down
  "a" #'dired-subtree-beginning
  "e" #'dired-subtree-end)
;; >---

;; >----------

;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # project

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html

(require 'project)

;; $ brew install maven

(setopt project-vc-extra-root-markers
        '("Chart.yaml"
          "my.proj"))

(keymap-global-set "s-p" project-prefix-map)

(keymap-set project-prefix-map "b" #'consult-project-buffer)
(keymap-set project-prefix-map "g" #'my/grep/project)
(keymap-set project-prefix-map "s" #'my/project/shell)
(keymap-set project-prefix-map "m" #'magit-project-status)

(setopt project-switch-commands
        '((project-find-file "File")
          (project-find-dir "Dir")
          (consult-project-buffer "Buffer")
          (my/grep/project "Grep")
          (my/project/shell "Shell")
          (magit-project-status "Magit")))

(defun my/project/shell ()
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (my/vterm)))

;; <-------------------------
;; ## Test file

(keymap-set project-prefix-map "t f" #'my/project/test/file)

(defun my/project/test/file ()
  (interactive)
  (let* ((file (buffer-file-name))
         (is-main-file (string-match "^\\(.+/src/main/java/.+/.+\\).java$" file))
         (is-test-file (and
                        (not is-main-file)
                        (string-match "^\\(.+/src/test/java/.+/.+\\)Test.java$" file))))
    (if is-main-file
        (let* ((test-file (match-string 1 file))
               (test-file (string-replace "/src/main/java/" "/src/test/java/" test-file))
               (test-file (format "%sTest.java" test-file)))
          (find-file test-file))
      (if is-test-file
          (let* ((main-file (match-string 1 file))
                 (main-file (string-replace "/src/test/java/" "/src/main/java/" main-file))
                 (main-file (format "%s.java" main-file)))
            (find-file main-file))
        (message "No match")))))
;; >-------------------------

;; <-------------------------
;; ## Test class

(keymap-set project-prefix-map "t c" #'my/project/test/class)

(defun my/project/test/class (&optional method)
  (interactive)
  (or
   (when-let* ((file (buffer-file-name))
               (is-test-file (string-match "^.+/src/test/java/\\(.+Test\\).java$" file))
               (class (match-string 1 file))
               (class (string-replace "/" "." class)))
     (let ((default-directory (project-root (project-current t)))
           (compilation-buffer-name-function
            (or project-compilation-buffer-name-function
                compilation-buffer-name-function))
           (command (format "mvn test -Dtest=%s%s"
                            class
                            (if method (format "#%s" method) ""))))
       (compile command)))
   (message "No match")))
;; >-------------------------

;; <-------------------------
;; ## Test method

(keymap-set project-prefix-map "t m" #'my/project/test/method)
(keymap-set embark-identifier-map "t" #'my/project/test/method)

(defun my/project/test/method ()
  (interactive)
  (my/project/test/class (read-from-minibuffer "Method: ")))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # VC

;; <-------------------------
;; ## magit

(require 'magit)

(setopt magit-save-repository-buffers 'dontask)
(setopt magit-diff-refine-hunk 'all)
(setopt magit-diff-highlight-hunk-body nil)
(setopt magit-commit-show-diff nil)
(setopt magit-diff-extra-stat-arguments #'magit-diff-use-window-width-as-stat-width)

(keymap-global-set "s-m" 'magit-status) ; was iconify-frame

(keymap-global-set "s-i v d" #'magit-diff-buffer-file)
(keymap-global-set "s-i v l" #'magit-log-buffer-file)
(keymap-global-set "s-i v m" #'magit-submodule)
(keymap-global-set "s-i v b" #'magit-blame)

;; [o]pen; was magit-submodule
(keymap-set magit-revision-mode-map "o" #'magit-diff-visit-worktree-file-other-window)
(keymap-set magit-status-mode-map "o" #'magit-diff-visit-worktree-file-other-window)
(keymap-set magit-diff-mode-map "o" #'magit-diff-visit-worktree-file-other-window)

(keymap-set magit-revision-mode-map "C-c j" #'magit-dired-jump)
(keymap-set magit-status-mode-map "C-c j" #'magit-dired-jump)
(keymap-set magit-diff-mode-map "C-c j" #'magit-dired-jump)

(require 'with-editor)
(keymap-set with-editor-mode-map "<remap> <save-buffer>" #'with-editor-finish)
(keymap-set with-editor-mode-map "C-c k" #'with-editor-cancel)

;; <----------
;; ### Repeat

(defvar-keymap my/magit/repeat-map
  :repeat t
  "n" #'magit-section-forward-sibling
  "p" #'magit-section-backward-sibling)
;; >----------

;; <----------
;; ### magit-todos

(require 'magit-todos)

(keymap-set project-prefix-map "t" #'magit-todos-list)

(keymap-set magit-todos-item-section-map "o" #'magit-todos-jump-to-item)
;; >----------

;; <----------
;; ### difftastic

;; https://github.com/pkryger/difftastic.el

(require 'difftastic)

;; $ brew install difftastic

(eval-after-load 'magit-diff
  '(transient-append-suffix 'magit-diff '(-1 -1)
     [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
      ("S" "Difftastic show" difftastic-magit-show)]))
(add-hook 'magit-blame-read-only-mode-hook
          (lambda ()
            (keymap-set magit-blame-read-only-mode-map
                        "D" #'difftastic-magit-show)
            (keymap-set magit-blame-read-only-mode-map
                        "S" #'difftastic-magit-show)))

(setopt difftastic-bright-colors-vector
        [ansi-color-bright-black
         magit-diff-removed
         magit-diff-added
         magit-filename
         font-lock-comment-face
         default
         font-lock-warning-face
         ansi-color-bright-white])

(defun my/difftastic/requested-window-width ()
  (- (frame-width)
     (fringe-columns 'left)
     (fringe-columns 'right)))

(setopt difftastic-requested-window-width-function
        #'my/difftastic/requested-window-width)

(keymap-global-set "s-i d D" #'difftastic-buffers)
;; >----------

;; >-------------------------

;; <-------------------------
;; ## git-timemachine

;; https://codeberg.org/pidu/git-timemachine

(require 'git-timemachine)

(keymap-global-set "s-i v t" #'git-timemachine)
;; >-------------------------

;; <-------------------------
;; ## diff-hl

(global-diff-hl-mode)

(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(keymap-global-set "s-i v =" #'diff-hl-diff-goto-hunk)
(keymap-global-set "s-i v n" #'diff-hl-show-hunk-next)
(keymap-global-set "s-i v p" #'diff-hl-show-hunk-previous)

(add-hook 'dired-mode-hook #'diff-hl-dired-mode)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Window

(add-to-list 'window-selection-change-functions #'my/pulse-line)

(defun my/pulse-line (_)
  (pulse-momentary-highlight-one-line))

(setopt switch-to-buffer-obey-display-actions t)
;; switch-to-buffer-in-dedicated-window pop?

;; <-------------------------
;; ## winner

(require 'winner)

(winner-mode)

(keymap-global-set "s-<" #'winner-undo)
(keymap-global-set "s->" #'winner-redo)

(defvar-keymap my/winner/repeat-map
  :repeat t
  "<" #'winner-undo
  "," #'winner-undo
  ">" #'winner-redo
  "." #'winner-redo)
;; >-------------------------

;; <-------------------------
;; ## Repeat

(defvar-keymap my/window/repeat-map
  :repeat t
  "t" #'enlarge-window              ; [t]aller
  "s" #'shrink-window               ; [s]horter
  "w" #'enlarge-window-horizontally ; [w]ider
  "n" #'shrink-window-horizontally  ; [n]arrower
  "b" #'balance-windows)

(keymap-global-set "s-i w" my/window/repeat-map)
;; >-------------------------

;; <-------------------------
;; ## Split

(keymap-global-set "s-t" #'my/window/split-sensibly)

(defun my/window/split-sensibly ()
  (interactive)
  (or
   (let ((split-width-threshold 150)
         (split-height-threshold nil))
     (split-window-sensibly))
   (let ((split-width-threshold nil)
         (split-height-threshold 30))
     (split-window-sensibly))
   (message "Not splittable")))
;; >-------------------------

(keymap-global-set "s-w" #'delete-window)
(keymap-global-set "s-e" #'delete-other-windows) ; was isearch-yank-kill

;; <-------------------------
;; ## avy jump

(require 'avy)

;; (setopt avy-timeout-seconds 0.7)

(keymap-global-set "M-j" #'avy-goto-char-2) ; was default-indent-new-line
(keymap-global-set "M-J" #'avy-resume)
(keymap-global-set "M-g" #'avy-goto-line)   ; was goto-line
(keymap-global-set "s-;" #'avy-pop-mark)

;; <----------
;; ### embark

;; https://karthinks.com/software/avy-can-do-anything/#a-division-of-responsibility

(setf (alist-get ?. avy-dispatch-alist) #'my/avy/action-embark)

(defun my/avy/action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)
;; >----------

;; >-------------------------

;; <-------------------------
;; ## ace-window

;; https://github.com/abo-abo/ace-window

(require 'ace-window)

(setopt aw-background nil)

(keymap-global-set "s-n" 'ace-window) ; [n]umber; was make-frame

(setopt aw-keys '(?a ?s ?d ?f ?g ?h ?k ?l))

(setq aw-dispatch-alist
      '((?w aw-delete-window "Delete")
        (?x aw-swap-window "Swap")
        (?m aw-move-window "Move")
        (?c aw-copy-window "Copy")
        (?j aw-switch-buffer-in-window "Buffer")))
;; >-------------------------

;; <-------------------------
;; ## Side window

(setopt window-sides-slots '(0 0 1 1)) ;; left top right bottom

(keymap-global-set "s-/" #'window-toggle-side-windows)

;; <----------
;; ### Redisplay

(keymap-global-set "s-'" #'my/window/redisplay-side) ; was next-window-any-frame

(defun my/window/redisplay-side ()
  (interactive)
  (walk-windows
   (lambda (window)
     (if (eq (window-dedicated-p window) 'side)
         (let* ((buffer (window-buffer window))
                (select (eq window (selected-window)))
                (_ (delete-window window))
                (new-side-window (display-buffer buffer)))
           (when select
             (select-window new-side-window)))))))
;; >----------

;; >-------------------------

;; <-------------------------
;; ## display-buffer-alist

(setopt display-buffer-base-action
        '((display-buffer-reuse-window
           display-buffer-use-some-window)))

(defun my/display-buffer-alist/condition-by-major-modes (major-modes)
  (lambda (buffer-name action)
    (with-current-buffer buffer-name (apply #'derived-mode-p major-modes))))

(add-to-list
 'display-buffer-alist
 `(,(rx (| "*vterm*"
           "*Help*"
           "*info*"
           "*Customize "
           "*Messages*"
           "*compilation*"
           "*eldoc*"
           "*just"
           "*Warnings*"
           "*docker-containers*"
           "*diff-syntax:"
           "*Embark Actions*"
           "*Flymake diagnostics for "
           "*EGLOT "
           "*sqls result*"
           "*Backtrace*"
           "*Deletions*"
           "*KeePass*"
           "*Error*"
           "*Register Preview*"))
   display-buffer-in-side-window
   (window-height . 0.3)
   (window-parameters (no-delete-other-windows . t))))

(add-to-list
 'display-buffer-alist
 `(,(rx (| "*edit-indirect "
           "*yaml-pro-edit*"))
   display-buffer-same-window))

(add-to-list
 'display-buffer-alist
 `(,(my/display-buffer-alist/condition-by-major-modes
     '(grep-mode
       occur-mode
       justl-mode
       xref--xref-buffer-mode
       vterm-mode))
   display-buffer-in-side-window
   (window-height . 0.3)
   (window-parameters (no-delete-other-windows . t))))

;; <----------
;; ### dired

(add-to-list
 'display-buffer-alist
 `(,(my/display-buffer-alist/condition-by-major-modes
     '(dired-mode))
   display-buffer-in-side-window
   (side . right)
   (window-width . 0.2)
   (window-parameters (no-delete-other-windows . t))))
;; >----------

;; <----------
;; ### Magit

(setopt magit-display-buffer-function #'display-buffer)

(add-to-list
 'display-buffer-alist
 `(,(my/display-buffer-alist/condition-by-major-modes
     '(magit-mode
       difftastic-mode))
   display-buffer-in-side-window
   (window-height . 0.8)
   (window-parameters (no-delete-other-windows . nil))))

(add-to-list
 'display-buffer-alist
 '("COMMIT_EDITMSG"
   display-buffer-in-side-window
   (window-height . 0.8)
   (window-parameters (no-delete-other-windows . nil))))
;; >----------

;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # multiple-cursors

;; https://github.com/magnars/multiple-cursors.el

(require 'multiple-cursors)

(keymap-global-set "s-i m c" 'mc/edit-lines)

(setopt mc/always-run-for-all t)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Mode Line

;; <-------------------------
;; ## doom-modeline

;; M-x nerd-icons-install-fonts

(require 'doom-modeline)

(setopt doom-modeline-check-simple-format t)

(doom-modeline-mode 1)

(setopt doom-modeline-buffer-encoding 'nondefault)
(setopt doom-modeline-buffer-file-name-style 'file-name-with-project)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # compilation-mode

;; compilation-mode-map

(setopt compilation-scroll-output t)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Find

(keymap-set occur-edit-mode-map "<remap> <save-buffer>" #'occur-cease-edit)

;; <-------------------------
;; ## In current buffer (region aware)

(keymap-global-set "s-f" 'my/find) ; was isearch-forward

(defun my/find ()
  (interactive)
  (my/region/with-str 'consult-line))
;; >-------------------------

;; <-------------------------
;; ## In project buffers (region aware)

(keymap-global-set "s-F" 'my/find/project)

(defun my/find/project ()
  (interactive)
  (my/region/with-str 'my/find/project/initial))

(defun my/find/project/initial (&optional initial)
  (interactive)
  (consult-line-multi nil initial))
;; >-------------------------

;; <-------------------------
;; ## Replace

(keymap-global-set "s-i r s" #'replace-string)
(keymap-global-set "s-R" #'query-replace)
(keymap-global-set "s-i r r" #'query-replace-regexp)
;; >-------------------------

;; <-------------------------
;; ## Remove empty lines in region

(keymap-global-set "s-i r e" #'my/remove-empty-lines)

(defun my/remove-empty-lines ()
  (interactive)
  (if (use-region-p)
      (let ((begin (region-beginning))
            (end (region-end)))
        (flush-lines "^$" begin end))
    (message "No region")))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Grep

(require 'grep)

;; $ brew install ripgrep

(keymap-set grep-mode-map "o" #'compile-goto-error)

;; <-------------------------
;; ## wgrep

(require 'wgrep)

;; https://github.com/mhayashi1120/Emacs-wgrep

(keymap-set grep-mode-map "e" #'wgrep-change-to-wgrep-mode)
(keymap-set wgrep-mode-map "<remap> <save-buffer>" #'wgrep-finish-edit) ; commit

;; >-------------------------

;; <-------------------------
;; ## In project (region aware)

(keymap-global-set "s-g" 'my/grep/project) ; was isearch-repeat-forward

(defun my/grep/project ()
  (interactive)
  (my/region/with-str 'my/grep/project/initial))

(defun my/grep/project/initial (&optional initial)
  (interactive)
  (consult-ripgrep nil initial))
;; >-------------------------

;; <-------------------------
;; ## In specific dir (region aware)

(keymap-global-set "s-G" 'my/grep/dir)

(defun my/grep/dir ()
  (interactive)
  (my/region/with-str 'my/grep/dir/initial))

(defun my/grep/dir/initial (&optional initial)
  (interactive)
  (let ((dir (read-directory-name "Grep in: ")))
    (setq this-command
          (lambda ()
            "crd"
            (interactive)
            (consult-ripgrep dir)))
    (consult-ripgrep dir initial)))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # yasnippet

(require 'yasnippet)

(yas-global-mode 1)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # xref

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html

(require 'xref)

;; M-. -> xref-find-definitions
;; M-, -> xref-pop-marker-stack
;; M-? -> xref-find-references

(setopt xref-history-storage #'xref-window-local-history)

;; <-------------------------
;; ## consult

(setopt xref-show-xrefs-function #'consult-xref)
(setopt xref-show-definitions-function #'consult-xref)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # eldoc

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Doc.html
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # lisp

(keymap-global-set "s-i m e" #'emacs-lisp-mode)

;; <-------------------------
;; ## Tree repeat-map

(defvar-keymap my/lisp/tree-repeat-map
  :repeat (:enter
           (backward-kill-sexp))
  "n" #'forward-list
  "p" #'backward-list
  "u" #'backward-up-list
  "d" #'down-list
  "a" #'beginning-of-defun
  "e" #'end-of-defun
  "f" #'forward-sexp
  "b" #'backward-sexp
  "k" #'kill-sexp
  "<backspace>" #'my/backward-kill-sexp/tree-repeat
  "SPC" #'my/lisp/set-mark)

(dolist (cmd '(treesit-beginning-of-defun
               treesit-end-of-defun))
  (put cmd 'repeat-map 'my/lisp/tree-repeat-map))

(defun my/lisp/set-mark ()
  (interactive)
  (my/region/set-mark 'my/lisp/set-mark))

(defun my/backward-kill-sexp/tree-repeat ()
  (interactive)
  (if (use-region-p)
      (delete-active-region)
    (backward-kill-sexp)))
;; >-------------------------

;; <-------------------------
;; ## highlight-function-calls

(require 'highlight-function-calls)

;; https://github.com/alphapapa/highlight-function-calls
(add-hook 'emacs-lisp-mode-hook #'highlight-function-calls-mode)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # eglot

(require 'eglot)

(fset #'jsonrpc--log-event #'ignore)

(setopt eglot-events-buffer-config '(:size 0 :format full))
(setopt eglot-report-progress nil)

(keymap-global-unset "s-l") ; [l]sp; was goto-line

(keymap-global-set "s-l e" #'eglot)

(keymap-set eglot-mode-map "s-." #'eglot-code-actions) ; was customize
(keymap-set eglot-mode-map "s-l r" #'eglot-rename)
(keymap-set eglot-mode-map "s-l i" #'eglot-find-implementation)
(keymap-set eglot-mode-map "s-l t" #'eglot-find-typeDefinition)

(keymap-set eglot-mode-map "s-l s" #'eglot-shutdown)
(keymap-set eglot-mode-map "s-l S" #'eglot-shutdown-all)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Java

(require 'java-ts-mode)

;; <-------------------------
;; ## eglot

;; $ brew install jdtls

(add-to-list
 'eglot-server-programs
 '(java-ts-mode
   .
   ("jdtls"
    "--jvm-arg=-javaagent:/Users/zjq/opt/lombok.jar"
    "--jvm-arg=-Xmx4G"
    "--jvm-arg=-XX:+UseStringDeduplication"
    :initializationOptions (:extendedClientCapabilities (:classFileContentsSupport t)))))

;; <----------
;; ### Support jdt://

;; https://github.com/yveszoundi/eglot-java

(add-to-list 'file-name-handler-alist '("\\`jdt://" . eglot-java--jdt-uri-handler))

(defun eglot-java--jdt-uri-handler (_operation &rest args)
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
  "Compose a path from a base folder ROOT-DIR and a set of items PATH-ELEMENTS."
  (let ((new-path          (expand-file-name root-dir))
        (new-path-elements path-elements))
    (dolist (p new-path-elements)
      (setq new-path (concat (file-name-as-directory new-path) p)))
    new-path))
;; >----------

;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # XML

(require 'nxml-mode)

(keymap-global-set "s-i m x" #'xml-mode)

;; ref rng-nxml-mode-init

(add-hook 'nxml-mode-hook 'my/nxml-mode-hook)

(defun my/nxml-mode-hook ()
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(setopt nxml-child-indent 4)

(keymap-set nxml-mode-map "C-c i" #'nxml-balanced-close-start-tag-inline)
(keymap-set nxml-mode-map "C-c b" #'nxml-balanced-close-start-tag-block)

;; <-------------------------
;; ## Tree repeat-map

(defvar-keymap my/xml/tree-repeat-map
  :repeat t
  "n" #'nxml-forward-element
  "p" #'nxml-backward-element
  "u" #'nxml-backward-up-element
  "d" #'nxml-down-element
  "SPC" #'my/xml/set-mark)

(defun my/xml/set-mark ()
  (interactive)
  (my/region/set-mark 'my/xml/set-mark))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # vundo

;; https://github.com/casouri/vundo

(require 'vundo)

(keymap-global-set "s-i u" 'vundo)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # polymode

;; https://github.com/polymode/polymode

(setq polymode-prefix-key "\C-cP")

(require 'polymode)

(defvar-keymap my/polymode/map
  "n" #'polymode-next-chunk
  "p" #'polymode-previous-chunk
  "k" #'polymode-kill-chunk
  "m" #'polymode-mark-or-extend-chunk)

(keymap-set polymode-mode-map "C-c p" my/polymode/map)
(keymap-set polymode-mode-map "C-c '" #'my/polymode/edit-chunk)

(defun my/polymode/edit-chunk ()
  (interactive)
  (call-interactively 'polymode-mark-or-extend-chunk)
  (call-interactively 'edit-indirect-region))

;; <-------------------------
;; ## Bash

(define-innermode poly-bash-innermode
  :mode 'bash-ts-mode
  :head-matcher "^ *#!/usr/bin/env \\(sh\\|bash\\)\n"
  :tail-matcher "^ *# </bash>$"
  :head-mode 'body
  :tail-mode 'body)
;; >-------------------------

;; <-------------------------
;; ## Nginx

(define-innermode poly-nginx-innermode
  :mode 'nginx-mode
  :head-matcher "^ *# <nginx>\n"
  :tail-matcher "^ *# </nginx>$"
  :head-mode 'body
  :tail-mode 'body)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Whitespace

(require 'whitespace)

(setopt indent-tabs-mode nil)
(setopt whitespace-line-column 100)
(setopt whitespace-style '(face tab-mark tabs trailing lines-char))

(add-hook 'text-mode-hook #'whitespace-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)

(keymap-global-set "s-i m w" #'whitespace-mode)

(defvar-keymap my/whitespace-mode/repeat-map
  :repeat t
  "w" #'whitespace-mode)

(keymap-global-set "M-\\" #'delete-trailing-whitespace) ; was delete-horizontal-space
(keymap-global-set "M-|" #'delete-all-space)
(keymap-global-set "s-i j" #'join-line)

;; <-------------------------
;; ## Indent

(keymap-set embark-region-map "i" #'indent-rigidly)
(keymap-set embark-region-map "T" #'untabify)

(keymap-set indent-rigidly-map "i" #'indent-rigidly-right)
(keymap-set indent-rigidly-map "u" #'indent-rigidly-left)

;; <----------
;; ### New line

(keymap-global-set "s-i RET" #'my/indent/newline)

(defun my/indent/newline ()
  (interactive)
  (back-to-indentation)
  (let ((col (current-column)))
    (move-end-of-line nil)
    (electric-indent-just-newline nil)
    (indent-to-column col)))
;; >----------

;; >-------------------------

;; <-------------------------
;; ## Delete backward

(keymap-global-set "s-[" #'my/whitespace/delete-backward)

(defvar-keymap my/whitespace/delete-backward/repeat-map
  :repeat t
  "[" #'my/whitespace/delete-backward)

(defun my/whitespace/delete-backward ()
  (interactive)
  (when-let ((from (point))
             (_ (search-backward " " (pos-bol) t)))
    (delete-char 1)
    (goto-char (- from 1))))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Bookmar & register

(setopt bookmark-save-flag 1)

(keymap-global-set "s-r" #'my/register)
(keymap-global-set "s-q" #'consult-register)
(keymap-global-set "s-i r w" #'window-configuration-to-register)

(defvar my/register/point-name ?A)

(defun my/register ()
  (interactive)
  (if (use-region-p)
      (call-interactively 'consult-register-store)
    (if (>= my/register/point-name (+ ?A 15))
        (setq my/register/point-name ?A))
    (point-to-register my/register/point-name)
    (cl-incf my/register/point-name)))
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Misc

(setopt inhibit-startup-screen t)
(setopt initial-major-mode 'text-mode)
(setopt fill-column 85)
(setopt make-backup-files nil)
(setopt scroll-bar-mode nil)
(setopt tool-bar-mode nil)

(setq y-or-n-p-use-read-key t)
(setopt use-short-answers t)

(electric-pair-mode)

(setopt ring-bell-function #'ignore)

;; (setopt debug-on-error t)

(defvar-keymap my/paragraphs/repeat-map
  :repeat t
  "{" #'backward-paragraph
  "}" #'forward-paragraph
  "[" #'backward-paragraph
  "]" #'forward-paragraph)

(keymap-global-set "s-i m f" #'follow-mode)

;; <-------------------------
;; ## find-file

(keymap-global-set "s-i o" #'my/find-file/dev-notes)

(defun my/find-file/dev-notes ()
  (interactive)
  (let ((default-directory "~/Documents/notes/dev/"))
    (call-interactively #'find-file)))

;; https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/
(defun er-auto-create-missing-dirs ()
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))

(add-to-list 'find-file-not-found-functions #'er-auto-create-missing-dirs)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Bash

(require 'sh-script)

;; $ brew install bash-language-server

(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
(advice-remove 'bash-ts-mode #'sh--redirect-bash-ts-mode)

(keymap-global-set "s-i m b" #'my/bash-ts-mode)

;; <-------------------------
;; ## eglot

;; https://github.com/polymode/polymode/issues/331

(define-derived-mode my/bash-ts-mode
  bash-ts-mode "MyBash"
  "Major mode for Bash in non-poly buffer.")

(add-to-list 'interpreter-mode-alist '("bash\\|sh" . my/bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.sh$" . my/bash-ts-mode))

(add-to-list 'eglot-server-programs
             '(bash-ts-mode . ("disabled")))

(add-to-list 'eglot-server-programs
             '(my/bash-ts-mode . ("bash-language-server" "start")))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; Dockerfile

(require 'dockerfile-ts-mode)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Go

(require 'go-ts-mode)

(setopt go-ts-mode-indent-offset 4)

(add-hook 'go-ts-mode-hook #'my/go-ts-mode-hook)

(defun my/go-ts-mode-hook ()
  (setq-local tab-width 4)
  (whitespace-toggle-options '(tab-mark tabs)))
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # TypeScript

(require 'typescript-ts-mode)

(setopt typescript-ts-mode-indent-offset 4)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # edit-indirect

(require 'edit-indirect)

(keymap-set embark-region-map "e i" #'edit-indirect-region)

(setq edit-indirect-guess-mode-function #'my/edit-indirect/guess-mode)

(defun my/edit-indirect/guess-mode (_parent-buffer _beg _end)
  (setq-local buffer-file-name (format "%s.-ei-" (buffer-file-name _parent-buffer)))
  (let ((parent-mode (buffer-local-value 'major-mode _parent-buffer)))
    (if (eq parent-mode #'bash-ts-mode)
        (my/bash-ts-mode)
      (funcall parent-mode))))

;; <-------------------------
;; ## Tweak for left margin non-repeatability

(keymap-set edit-indirect-mode-map "<remap> <save-buffer>" #'edit-indirect-commit)
(keymap-set edit-indirect-mode-map "C-c k" #'edit-indirect-abort)

(defun edit-indirect--commit-on-save ()
  (edit-indirect-commit)
  t)

(defun edit-indirect--abort (kill)
  "Abort an indirect edit and clean up the edit-indirect buffer."
  (let ((parent-buffer (overlay-buffer edit-indirect--overlay)))
    (delete-overlay edit-indirect--overlay)
    (setq edit-indirect--overlay nil)
    (when kill
      (kill-buffer)
      (display-buffer-same-window parent-buffer nil))))
;; >-------------------------

;; <-------------------------
;; ## Left margin

;; https://github.com/Fanael/edit-indirect/issues/6#issuecomment-387945773

(add-hook 'edit-indirect-after-creation-hook #'vbe/edit-indirect/remove-left-margin)
(add-hook 'edit-indirect-before-commit-hook #'vbe/edit-indirect/restore-left-margin)

(defvar-local edit-indirect--left-margin 0)

(defun vbe/edit-indirect/remove-left-margin ()
  "Remove left-margin and save it into a local variable."
  (let ((lm (vbe/compute-left-margin (buffer-substring (point-min) (point-max)))))
    (indent-rigidly (point-min) (point-max) (* -1 lm))
    (setq-local edit-indirect--left-margin lm)
    (put 'edit-indirect--left-margin 'permanent-local t)))

(defun vbe/compute-left-margin (code)
  "Compute left margin of a string of code."
  (-min
   (-map #'(lambda (line) (length (car (s-match "^\\s-*" line))))
         (-remove 's-blank? (s-lines code)))))

(defun vbe/edit-indirect/restore-left-margin ()
  "Restore left-margin before commiting."
  (indent-rigidly (point-min) (point-max) edit-indirect--left-margin))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # JSON

(require 'json-ts-mode)

(keymap-global-set "s-i m j" #'json-ts-mode)

;; <-------------------------
;; ## flymake

;; $ brew install jq

(add-hook 'json-ts-mode-hook #'my/json/flymake-hook)

(defun my/json/flymake-hook ()
  (add-hook 'flymake-diagnostic-functions #'flymake-collection-jq nil t)
  (flymake-mode))
;; >-------------------------

;; <-------------------------
;; ## jq-mode

(keymap-set json-ts-mode-map "C-c q" #'jq-interactively)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # YAML

(require 'yaml-ts-mode)

(keymap-global-set "s-i m y" #'yaml-ts-mode)

;; <-------------------------
;; ## flymake

;; $ brew install yamllint

(add-hook 'yaml-ts-mode-hook #'my/yaml/flymake-hook)

(defun my/yaml/flymake-hook ()
  (add-hook 'flymake-diagnostic-functions #'flymake-collection-yamllint nil t)
  (flymake-mode))
;; >-------------------------

;; <-------------------------
;; ## yaml-pro

;; https://github.com/zkry/yaml-pro

(require 'yaml-pro)

(setopt yaml-pro-ts-yank-subtrees nil)

(add-hook 'yaml-ts-mode-hook #'yaml-pro-ts-mode)

(keymap-set yaml-pro-ts-mode-map "C-c e" #'yaml-pro-edit-ts-scalar)
(keymap-set yaml-pro-ts-mode-map "RET" #'my/indent/newline)

;; <----------
;; ### Indent repeat-map

(defvar-keymap my/yaml-pro/indent-repeat-map
  :repeat t
  "i" #'yaml-pro-ts-indent-subtree
  "u" #'yaml-pro-ts-unindent-subtree)

(keymap-set yaml-pro-ts-mode-map "C-c i" my/yaml-pro/indent-repeat-map)
;; >----------

;; <----------
;; ### Tree repeat-map

(keymap-set yaml-pro-ts-mode-map "C-M-n" #'yaml-pro-ts-next-subtree)           ; was forward-list
(keymap-set yaml-pro-ts-mode-map "C-M-p" #'yaml-pro-ts-prev-subtree)           ; was backward-list
(keymap-set yaml-pro-ts-mode-map "C-M-u" #'yaml-pro-ts-up-level)               ; was backward-up-list
(keymap-set yaml-pro-ts-mode-map "C-M-d" #'yaml-pro-ts-down-level)             ; was down-list
(keymap-set yaml-pro-ts-mode-map "C-M-k" #'yaml-pro-ts-kill-subtree)           ; was kill-sexp
(keymap-set yaml-pro-ts-mode-map "C-M-<backspace>" #'yaml-pro-ts-kill-subtree) ; was backward-kill-sexp
(keymap-set yaml-pro-ts-mode-map "C-M-a" #'yaml-pro-ts-first-sibling)          ; was beginning-of-defun
(keymap-set yaml-pro-ts-mode-map "C-M-e" #'yaml-pro-ts-last-sibling)           ; was end-of-defun
(keymap-set yaml-pro-ts-mode-map "C-M-t" #'yaml-pro-ts-move-subtree-up)        ; was transpose-sexps

(defvar-keymap my/yaml-pro/tree-repeat-map
  :repeat t
  "n" #'yaml-pro-ts-next-subtree
  "p" #'yaml-pro-ts-prev-subtree
  "u" #'yaml-pro-ts-up-level
  "d" #'yaml-pro-ts-down-level
  "m" #'yaml-pro-ts-mark-subtree
  "k" #'yaml-pro-ts-kill-subtree
  "<backspace>" #'my/yaml-pro-ts-kill-subtree/tree-repeat
  "a" #'yaml-pro-ts-first-sibling
  "e" #'yaml-pro-ts-last-sibling
  "t" #'yaml-pro-ts-move-subtree-up
  "T" #'yaml-pro-ts-move-subtree-down
  "SPC" #'my/yaml-pro/set-mark)

(defun my/yaml-pro/set-mark ()
  (interactive)
  (my/region/set-mark 'my/yaml-pro/set-mark))

(defun my/yaml-pro-ts-kill-subtree/tree-repeat ()
  (interactive)
  (if (use-region-p)
      (delete-active-region)
    (yaml-pro-ts-kill-subtree)))
;; >----------

(keymap-set yaml-pro-ts-mode-map "C-c m" #'yaml-pro-ts-mark-subtree)
(keymap-set yaml-pro-ts-mode-map "C-c y" #'yaml-pro-ts-paste-subtree)
(keymap-set yaml-pro-ts-mode-map "C-c w" #'yaml-pro-copy-node-path-at-point)

;; <----------
;; ### Edit scalar

(keymap-set yaml-pro-edit-mode-map "<remap> <save-buffer>" #'yaml-pro-edit-complete)
(keymap-set yaml-pro-edit-mode-map "C-c k" #'yaml-pro-edit-quit)
;; >----------

;; >-------------------------

;; <-------------------------
;; ## Polymode

(define-hostmode poly-yaml-ts-hostmode
  :mode 'yaml-ts-mode)

(define-polymode poly-yaml-ts-mode
  :hostmode 'poly-yaml-ts-hostmode
  :innermodes
  '(poly-bash-innermode
    poly-nginx-innermode))

(add-to-list 'auto-mode-alist '("\\.ya?ml$" . poly-yaml-ts-mode))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Markdown

(require 'markdown-mode)

(setopt markdown-command "multimarkdown")
(setopt markdown-display-remote-images t)
(setopt markdown-fontify-code-blocks-natively t)
(setopt markdown-max-image-size '(500 . 500))
(setopt markdown-asymmetric-header t)
(setopt markdown-gfm-additional-languages '("helm"))
(setopt markdown-hide-urls t)

(add-to-list 'markdown-code-lang-modes '("shell" . bash-ts-mode))
(add-to-list 'markdown-code-lang-modes '("freemarker" . my/freemarker-mode))
(add-to-list 'markdown-code-lang-modes '("helm" . my/helm-template-mode))
(add-to-list 'markdown-code-lang-modes '("jinja" . jinja2-mode))
(add-to-list 'markdown-code-lang-modes '("toml" . conf-toml-mode))

(keymap-set markdown-mode-map "C-c c" #'markdown-insert-code)
(keymap-set markdown-mode-map "C-c b" #'markdown-insert-gfm-code-block)
(keymap-set markdown-mode-map "C-c q" #'markdown-insert-blockquote)
(keymap-set markdown-mode-map "C-c e" #'markdown-insert-bold) ; [e]mphasize
(keymap-set markdown-mode-map "C-c h" #'markdown-insert-header-dwim)
(keymap-set markdown-mode-map "C-c i" #'markdown-insert-image)
(keymap-set markdown-mode-map "C-c I" #'markdown-toggle-inline-images)
(keymap-set markdown-mode-map "C-c m" #'markdown-mark-block)
(keymap-set markdown-mode-map "C-c k" #'markdown-kill-block)
(keymap-set markdown-mode-map "C-c '" #'my/markdown-edit-code-block)
(keymap-set markdown-mode-map "C-c f" #'markdown-follow-thing-at-point)
(keymap-set markdown-mode-map "C-c T" #'markdown-insert-table)

(keymap-set markdown-mode-map "C-c t" #'markdown-insert-gfm-checkbox) ; [t]odo
(keymap-set markdown-mode-map "C-c d" #'markdown-toggle-gfm-checkbox) ; [d]one

(defun my/markdown-edit-code-block ()
  (interactive)
  (search-backward "```" nil t)
  (markdown-edit-code-block))

;; <-------------------------
;; ## Heading repeat-map

(keymap-set markdown-mode-map "C-M-n" #'markdown-forward-same-level)   ; was forward-list
(keymap-set markdown-mode-map "C-M-p" #'markdown-backward-same-level)  ; was backward-list
(keymap-set markdown-mode-map "C-M-u" #'markdown-up-heading)           ; was backward-up-list
(keymap-set markdown-mode-map "C-M-d" #'markdown-next-visible-heading) ; was down-list
(keymap-set markdown-mode-map "C-M-f" #'markdown-forward-block)        ; was forward-sexp
(keymap-set markdown-mode-map "C-M-b" #'markdown-backward-block)       ; was backward-sexp

(defvar-keymap my/markdown/heading-repeat-map
  :repeat t
  "n" #'markdown-forward-same-level
  "p" #'markdown-backward-same-level
  "u" #'markdown-up-heading
  "d" #'markdown-next-visible-heading
  "U" #'markdown-previous-visible-heading
  "f" #'markdown-forward-block
  "b" #'markdown-backward-block
  "SPC" #'my/markdown/set-mark)

(defun my/markdown/set-mark ()
  (interactive)
  (my/region/set-mark 'my/markdown/set-mark))
;; >-------------------------

;; <-------------------------
;; ## Paragraph repeat-map

(defvar-keymap my/markdown/paragraph-repeat-map
  :repeat t
  "{" #'markdown-backward-paragraph
  "}" #'markdown-forward-paragraph
  "[" #'markdown-backward-paragraph
  "]" #'markdown-forward-paragraph)
;; >-------------------------

;; <-------------------------
;; ## Link repeat-map

(keymap-set markdown-mode-map "C-c l" #'markdown-insert-link)

(defvar-keymap my/markdown/link-repeat-map
  :repeat (:exit
           (markdown-insert-link))
  "n" #'markdown-next-link
  "p" #'markdown-previous-link
  "f" #'markdown-follow-thing-at-point
  "l" #'markdown-insert-link)
;; >-------------------------

;; <-------------------------
;; ## URL hiding repeat-map

(keymap-set markdown-mode-map "C-c L" #'markdown-toggle-url-hiding)

(defvar-keymap my/markdown/url-hiding-repeat-map
  :repeat t
  "l" #'markdown-toggle-url-hiding)
;; >-------------------------

;; <-------------------------
;; ## preview

(defun markdown-live-preview-get-filename ()
  "Standardize the filename exported by `markdown-live-preview-export'."
  (let* ((result (markdown-export-file-name ".html"))
         (result (format "~/tmp/.emacs-markdown-preview%s" result))
         (result (expand-file-name result))
         (dir (f-dirname result)))
    (make-directory dir t)
    result))

(keymap-set markdown-mode-map "C-c p" #'markdown-live-preview-mode)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # docker

;; https://github.com/Silex/docker.el
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # jinja2-mode

;; https://github.com/paradoxxxzero/jinja2-mode

(require 'jinja2-mode)

(keymap-set jinja2-mode-map "M-o" nil) ; facemenu-set-
(keymap-set jinja2-mode-map "RET" #'my/indent/newline)

(keymap-global-set "s-i m J" #'jinja2-mode)

(modify-syntax-entry ?. "." jinja2-mode-syntax-table)
(modify-syntax-entry ?: "." jinja2-mode-syntax-table)

;; <-------------------------
;; ## Helm template

(define-derived-mode my/helm-template-mode
  jinja2-mode "HelmTpl"
  "Major mode for Helm templates.")

(font-lock-add-keywords
 'my/helm-template-mode
 '(("{{-? \\(include\\|if\\|else\\|else if\\|range\\|end\\|with\\) [^}]*}}"
    1 font-lock-preprocessor-face t))
 t)

(add-to-list 'auto-mode-alist '("\\.htl$" . my/helm-template-mode))

(keymap-global-set "s-i m h" #'my/helm-template-mode)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # pdf-tools

;; https://github.com/vedang/pdf-tools#view-and-navigate-pdfs

(require 'pdf-tools)

(pdf-loader-install)

(add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)

(keymap-set pdf-view-mode-map  "M-g" #'pdf-view-goto-page)
(keymap-set pdf-view-mode-map  "j" #'pdf-view-next-line-or-next-page)
(keymap-set pdf-view-mode-map  "k" #'pdf-view-previous-line-or-previous-page)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Groovy

(require 'groovy-mode)

;; <-------------------------
;; ## polymode

(define-hostmode poly-groovy-hostmode
  :mode 'groovy-mode)

(define-polymode poly-groovy-mode
  :hostmode 'poly-groovy-hostmode
  :innermodes '(poly-bash-innermode))

(add-to-list 'auto-mode-alist '("\\.groovy$" . poly-groovy-mode))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # webjump

(require 'webjump)

(setopt webjump-sites
        '(("Google" .
           [simple-query "www.google.com"
                         "www.google.com/search?q=" ""])))

(keymap-global-set "s-]" #'webjump)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # just

;; <-------------------------
;; ## just-mode

(require 'just-mode)

(modify-syntax-entry ?_ "_" just-mode-syntax-table)
(modify-syntax-entry ?- "_" just-mode-syntax-table)
;; >-------------------------

;; <-------------------------
;; ## justl

(require 'justl)

(keymap-set project-prefix-map "j" #'justl)

(setopt justl-recipe-width 50)

(keymap-set justl-mode-map "o" #'justl-go-to-recipe)

;; https://github.com/psibi/justl.el/issues/43#issuecomment-1955686203
;; (setopt justl-include-private-recipes nil)
;; TODO
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # TRAMP

(setopt tramp-connection-timeout 3)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # SQL

;; <-------------------------
;; ## eglot

(defclass eglot-sqls (eglot-lsp-server) ())
(add-to-list 'eglot-server-programs '(sql-mode . (eglot-sqls "sqls")))

(defvar my/eglot/sqls/current-connection nil)
(defvar my/eglot/sqls/current-database nil)

(cl-defmethod eglot-execute
  :around
  ((server eglot-sqls) action)

  (pcase (plist-get action :command)
    ("executeQuery"
     (if (use-region-p)
         (let* ((begin (region-beginning))
                (end (region-end))
                (begin-lsp (eglot--pos-to-lsp-position begin))
                (end-lsp (eglot--pos-to-lsp-position end))
                (action (plist-put action :range `(:start ,begin-lsp :end ,end-lsp)))
                (result (cl-call-next-method server action)))
           (my/eglot/sqls/show-result result))
       (message "No region")))

    ((or
      "showConnections"
      "showDatabases"
      "showSchemas"
      "showTables")
     (my/eglot/sqls/show-result (cl-call-next-method)))

    ("switchConnections"
     (let* ((connections (eglot--request server :workspace/executeCommand
                                         '(:command "showConnections")))
            (collection (split-string connections "\n"))
            (connection (completing-read "Switch to connection: " collection nil t))
            (index (number-to-string (string-to-number connection)))
            (action (plist-put action :arguments (vector index))))
       (cl-call-next-method server action)
       (setq my/eglot/sqls/current-connection connection)))

    ("switchDatabase"
     (let* ((databases (eglot--request server :workspace/executeCommand
                                       '(:command "showDatabases")))
            (collection (split-string databases "\n"))
            (database (completing-read "Switch to database: " collection nil t))
            (action (plist-put action :arguments (vector database))))
       (cl-call-next-method server action)
       (setq my/eglot/sqls/current-database database)))

    (_
     (cl-call-next-method))))

(defun my/eglot/sqls/show-result (result)
  (with-current-buffer (get-buffer-create "*sqls result*")
    (setq-local header-line-format
                '(:eval (my/eglot/sqls/show-result/header-line-format)))
    (erase-buffer)
    (unless (derived-mode-p 'org-mode)
      (org-mode))
    (insert result)
    (display-buffer (current-buffer))))

(defun my/eglot/sqls/show-result/header-line-format ()
  (let* ((connection (or my/eglot/sqls/current-connection ""))
         (parts (split-string connection " "))
         (driver (nth 1 parts))
         (alias (nth 2 parts))
         (result (format "[%s] %s/%s"
                         (or driver "?")
                         (or alias "?")
                         (or my/eglot/sqls/current-database "?"))))
    (propertize result
                'face 'my/eglot/sqls/show-result/header-line-face)))

(defface my/eglot/sqls/show-result/header-line-face
  '((t (:inherit 'magit-header-line)))
  "*sqls result* header-line face")
;; >-------------------------

;; <-------------------------
;; ## flymake

;; $ gem install sqlint
;; $ brew install sql-lint

(add-hook 'sql-mode-hook #'my/sql/flymake-hook)

(defun my/sql/flymake-hook ()
  (add-hook 'flymake-diagnostic-functions #'flymake-collection-sqlint nil t)
  (add-hook 'flymake-diagnostic-functions #'flymake-collection-sql-lint nil t)
  (flymake-mode)
  (setq-local eglot-stay-out-of '(flymake))
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t))

(setopt flymake-collection-sql-lint-driver "mysql")
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # sops

;; $ brew install sops

(keymap-global-set "s-i s e" 'my/sops/encrypt)
(keymap-global-set "s-i s d" 'my/sops/decrypt)

(defun my/sops/encrypt ()
  (interactive)
  (when-let ((file (buffer-file-name)))
    (shell-command (format "sops -e -i '%s'" file))))

(defun my/sops/decrypt ()
  (interactive)
  (when-let ((file (buffer-file-name)))
    (shell-command (format "sops -d -i '%s'" file))))
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # keepass

(keymap-global-set "s-i k" #'my/keepass/export)

(defun my/keepass/export ()
  (interactive)
  (shell-command "~/Documents/notes/keepass/export.sh" "*KeePass*"))
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # web-mode

(require 'web-mode)

(setopt web-mode-enable-auto-indentation nil)

;; <-------------------------
;; ## FreeMarker

(define-derived-mode my/freemarker-mode
  web-mode "FreeMarker"
  "Major mode for FreeMarker templates.")

(keymap-set my/freemarker-mode-map "RET" #'my/indent/newline)

(add-hook 'my/freemarker-mode-hook #'my/freemarker-mode/hook)

(defun my/freemarker-mode/hook ()
  (web-mode-set-engine "freemarker"))

(add-to-list 'auto-mode-alist '("\\.ftl$" . my/freemarker-mode))

(font-lock-add-keywords
 'my/freemarker-mode
 '(("\\[=\\([^]]+\\)\\]"
    (0 font-lock-preprocessor-face)
    (1 font-lock-variable-name-face t)))
 t)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Yank

;; <-------------------------
;; ## yank-to-buffer

(keymap-global-set "s-i y" #'my/yank-to-buffer)
(keymap-global-set "s-i Y" #'my/yank-to-buffer/append)

(defun my/yank-to-buffer (&optional append)
  (interactive)
  (and (use-region-p) (kill-ring-save nil nil t))
  (with-current-buffer (get-buffer-create "*yank*")
    (or append (erase-buffer))
    (end-of-buffer)
    (yank)
    (select-window (display-buffer (current-buffer)))))

(defun my/yank-to-buffer/append ()
  (interactive)
  (my/yank-to-buffer t))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Custom faces

(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 130)

(set-face-attribute 'aw-leading-char-face nil
                    :height 1.0)

;; <-------------------------
;; ## Markdown

(set-face-attribute 'markdown-bold-face nil
                    :foreground (face-foreground 'markdown-header-face-6 nil t)
                    :weight 'normal)
;; >-------------------------

;; <-------------------------
;; ## ELisp

(set-face-attribute 'highlight-function-calls-face nil
                    :inherit 'font-lock-function-call-face
                    :underline nil)
;; >-------------------------

;; <-------------------------
;; ## Eglot

(set-face-attribute 'eglot-highlight-symbol-face nil
                    :background (face-background 'symbol-overlay-default-face nil t))
;; >-------------------------

;; >--------------------------------------------------
