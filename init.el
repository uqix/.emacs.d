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
;; # Util libs

(require 's)
(require 'dash)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Theme

;; <-------------------------
;; ## doom-one

;; (load-theme 'doom-one t)
;; >-------------------------

;; <-------------------------
;; ## gruvbox

(load-theme 'gruvbox t)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Dabbrev

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html

(require 'dabbrev)

;; M-/ -> dabbrev-expand

(setopt dabbrev-case-fold-search nil)
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
         (project-path (consult--project-root)))
    (if project-path
        (let* ((project-path (directory-file-name project-path))
               (project-name (file-name-nondirectory project-path))
               (file-subpath (and file-path (file-relative-name file-path project-path)))
               (file-parent-subpath (and file-subpath (f-dirname file-subpath)))
               (project-parent-path (f-dirname project-path)))
          (format "%s 💙 %s%s 🚦⤴ %s"
                  (my/frame-title-format/buffer-name buffer-name project-path)
                  project-name
                  (if file-parent-subpath
                      (format " 🚦⤵ %s" (my/abbreviate-path file-parent-subpath))
                    "")
                  (my/abbreviate-path project-parent-path)))
      (format "%s 💢 %s"
              (my/frame-title-format/buffer-name buffer-name)
              (if file-path (my/abbreviate-path file-path) "")))))

(defun my/abbreviate-path (path)
  (let* ((result (abbreviate-file-name path))
         (result (directory-file-name result)))
    result))

(defun my/frame-title-format/buffer-name (buffer-name &optional project-path)
  (let ((edit-indirect-prefix "*edit-indirect "))
    (cond ((eq major-mode 'vterm-mode)
           (let* ((vterm-prefix (format "%s " vterm-buffer-name))
                  (wd
                   (if (string-prefix-p vterm-prefix buffer-name)
                       (let* ((working-dir (string-remove-prefix vterm-prefix buffer-name))
                              (working-dir-subpath
                               (and project-path
                                    (file-relative-name working-dir
                                                        (my/abbreviate-path project-path)))))
                         (or working-dir-subpath working-dir)))))
             (format "%s%s%s"
                     vterm-prefix
                     (if vterm-copy-mode "🛑 " "")
                     (or wd ""))))
          ((string-prefix-p edit-indirect-prefix buffer-name)
           (string-replace edit-indirect-prefix "*💥" buffer-name))
          (t
           buffer-name))))
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

; was flyspell-auto-correct-previous-word
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

(keymap-global-set "s-i m f" #'flymake-mode)

;; <-------------------------
;; ## flymake-collection

;; https://github.com/mohkale/flymake-collection

(require 'flymake-collection)

(use-package flymake-collection
  :hook (after-init . flymake-collection-hook-setup))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # minibuffer

(setopt enable-recursive-minibuffers t)

(add-hook 'minibuffer-setup-hook #'subword-mode)
(add-hook 'minibuffer-setup-hook #'flyspell-mode)
(add-hook 'minibuffer-setup-hook #'yas-minor-mode)

(keymap-set minibuffer-mode-map "C-c h" 'consult-history)

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
(keymap-set vertico-map "M-g" #'vertico-quick-insert)

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

(add-hook 'minibuffer-setup-hook #'my/corfu/minibuffer-setup-hook 1)

;; https://github.com/minad/corfu#completing-in-the-minibuffer
(defun my/corfu/minibuffer-setup-hook ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map))
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (setq-local corfu-auto nil
                corfu-echo-delay nil
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
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
;; # isearch

(require 'isearch)

(keymap-set key-translation-map "s-u" "C-s")
(keymap-set key-translation-map "s-y" "C-r")

(keymap-set isearch-mode-map "s-r" #'isearch-query-replace)
(keymap-set isearch-mode-map "s-f" #'consult-line)
(keymap-set isearch-mode-map "C-c h" #'consult-isearch-history)
(keymap-set isearch-mode-map "M-j" 'my/avy-isearch)

(defun my/avy-isearch ()
  (interactive)
  (isearch-done)
  (avy-isearch))

(setopt isearch-lazy-count t)

;; <-------------------------
;; ## Repeat

(defvar-keymap my/isearch/repeat-map
  :repeat t
  "s" #'isearch-repeat-forward
  "r" #'isearch-repeat-backward
  "u" #'isearch-repeat-forward
  "y" #'isearch-repeat-backward)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; Buffer

(keymap-global-set "s-i r b" #'revert-buffer)
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

(keymap-set key-translation-map "s-SPC" "C-g")
(keymap-set key-translation-map "s-," "C-c") ; was customize

(keymap-global-set "C-z" ctl-x-map)
(keymap-global-set "s-z" #'execute-extended-command)

(keymap-global-set "M-'" #'subword-mode) ; was abbrev-prefix-mark

(keymap-global-set "s-d" #'duplicate-dwim) ; was isearch-repeat-backward

(keymap-global-set "s-i f" #'find-file)

(keymap-global-set "s-r" #'replace-string)

(keymap-global-set "C-\\" #'toggle-truncate-lines) ; was toggle-input-method

(keymap-global-set "C-." #'pop-to-mark-command) ; was flyspell-auto-correct-word

;; <-------------------------
;; ## next-error

;; Normally uses the most recently started compilation, grep, or occur buffer

(keymap-global-set "s-i e" next-error-repeat-map)
;; >-------------------------

(keymap-global-set "s-i m t" #'text-mode)
(keymap-global-set "s-i m v" #'view-mode)

(keymap-global-set "C-h F" #'describe-face)
(keymap-global-set "C-h K" #'describe-keymap)

(keymap-global-set "s-i r f" #'rename-visited-file)

(keymap-global-set "s-i s b" #'scratch-buffer)

;; s-1: macOS Keyboard Shortcuts - Mission Control - Switch to Desktop 1 (Misc)
;; s-2: macOS Keyboard Shortcuts - Mission Control - Switch to Desktop 2 (Emacs)
;; s-3: macOS Keyboard Shortcuts - Mission Control - Switch to Desktop 3 (Chrome)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Highlight

(require 'hl-line)

;; <-------------------------
;; ## Regexp

;; <----------
;; ### Highlight (region aware)

(keymap-global-set "s-i h r" 'my/highlight-regexp)
(keymap-global-set "s-i h h" 'my/highlight-regexp/all)

(defvar-keymap my/highlight-regexp/all/repeat-map
  :repeat (:enter
           (my/highlight-regexp/all)
           :exit
           (my/unhighlight-regexp/all))
  "h" #'my/unhighlight-regexp/all)

(defun my/highlight-regexp ()
  (interactive)
  (my/region/with-str 'my/highlight-regexp/str))

(defun my/highlight-regexp/str (&optional str)
  (interactive)
  (let ((regexp (or
                 (and str (regexp-quote str))
                 (read-regexp "RegExp: "))))
    (highlight-regexp regexp 'symbol-overlay-face-8)))

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
         (highlight-regexp regexp 'symbol-overlay-face-8))))))
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
    (highlight-regexp regexp 'symbol-overlay-face-7 1)))
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
(keymap-set symbol-overlay-map "h" #'my/highlight-regexp/all) ; was symbol-overlay-map-help
(keymap-set symbol-overlay-map "u" #'symbol-overlay-remove-all)

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

(setopt ediff-split-window-function #'split-window-horizontally)
(setopt ediff-window-setup-function #'ediff-setup-windows-plain)

(add-hook 'ediff-after-quit-hook-internal #'winner-undo)
;; >-------------------------

;; <-------------------------
;; ## ztree

(require 'ztree)

(keymap-global-set "s-i d d" #'ztree-diff)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Shell

(require 'shell)

(keymap-set shell-mode-map "C-c h" 'consult-history)
(keymap-set shell-mode-map "C-c p" 'comint-previous-prompt)
(keymap-set shell-mode-map "C-c n" 'comint-next-prompt)
(keymap-set shell-mode-map "C-c c" 'comint-clear-buffer)

;; <-------------------------
;; ## vterm

;; https://github.com/akermu/emacs-libvterm

;; $ brew install cmake

(require 'vterm)

(keymap-global-set "s-i t" #'vterm)

(keymap-set vterm-mode-map "C-c c" #'vterm-copy-mode)
(keymap-set vterm-copy-mode-map "C-c c" #'vterm-copy-mode)

(with-eval-after-load 'embark
  (add-to-list 'embark-around-action-hooks '(my/vterm embark--cd))
  (keymap-set embark-file-map "$" #'my/vterm))

(defun my/vterm ()
  (interactive)
  (let* ((dir (my/dirname default-directory))
         (name (format "%s %s" vterm-buffer-name dir))
         (buffer (get-buffer name)))
    (if buffer
        (switch-to-buffer buffer)
      (vterm name))))

(defun my/dirname (path)
  (let* ((result (if (f-file-p path) (f-dirname path) path))
         (result (abbreviate-file-name result))
         (result (directory-file-name result)))
    result))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Region

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

(keymap-global-set "s-i n n" #'narrow-to-region)
(keymap-global-set "s-i n f" #'narrow-to-defun)
(keymap-global-set "s-i n w" #'widen)
;; >-------------------------

;; <-------------------------
;; ## expand-region

;; https://github.com/magnars/expand-region.el

(require 'expand-region)

(keymap-global-set "M-[" 'er/expand-region)

(defvar-keymap my/expand-region/repeat-map
  :repeat t
  "[" #'er/expand-region)
;; >-------------------------

;; <-------------------------
;; ## Select by separator

;; <----------
;; ### Single line

(keymap-global-set "s-i s SPC" 'my/select-text/between/spaces)
(keymap-global-set "s-i s /" 'my/select-text/between/slashes)
(keymap-global-set "s-i s ," 'my/select-text/between/commas)
(keymap-global-set "s-i s '" 'my/select-text/between/single-quotes)
(keymap-global-set "s-i s \"" 'my/select-text/between/double-quotes)
(keymap-global-set "s-i s `" 'my/select-text/between/back-quotes)
(keymap-global-set "s-i s \\" 'my/select-text/between/backslashes)

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

(defun my/select-text/between (separator)
  (let ((separator-regexp (regexp-quote (string separator))))
    (re-search-backward (format "%s\\|^" separator-regexp))
    (when (= (char-after) separator)
      (forward-char))
    (set-mark (point))
    (re-search-forward (format "%s\\|$" separator-regexp))
    (when (= (char-before) separator)
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
;; ## Case convert

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(with-eval-after-load 'embark
  (keymap-set embark-region-map "c" nil)
  (keymap-set embark-region-map "c s" #'my/region/convert/snake-case)
  (keymap-set embark-region-map "c c" #'my/region/convert/camel-case)
  (keymap-set embark-region-map "c k" #'my/region/convert/kebab-case)
  (keymap-set embark-region-map "c C" #'my/region/convert/capitalize))

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
;; ## Format convert

;; $ brew install yq

(with-eval-after-load 'embark
  (keymap-set embark-region-map "c j y" #'my/region/convert/json/to-yaml)
  (keymap-set embark-region-map "c j j" #'my/region/convert/json/prettify)
  (keymap-set embark-region-map "c y j" #'my/region/convert/yaml/to-json)
  (keymap-set embark-region-map "c y y" #'my/region/convert/yaml/prettify)
  (keymap-set embark-region-map "c y p" #'my/region/convert/yaml/to-properties)
  (keymap-set embark-region-map "c p y" #'my/region/convert/properties/to-yaml))

(defun my/region/convert/json/to-yaml ()
  (interactive)
  (my/region/convert/by-shell-command "yq -p=json -o=yaml -"))

(defun my/region/convert/json/prettify ()
  (interactive)
  (my/region/convert/by-shell-command "yq -p=json -o=json -"))

(defun my/region/convert/yaml/to-json ()
  (interactive)
  (my/region/convert/by-shell-command "yq -p=yaml -o=json -"))

(defun my/region/convert/yaml/prettify ()
  (interactive)
  (my/region/convert/by-shell-command "yq -p=yaml -o=yaml -"))

(defun my/region/convert/yaml/to-properties ()
  (interactive)
  (my/region/convert/by-shell-command "yq -p=yaml -o=props -"))

(defun my/region/convert/properties/to-yaml ()
  (interactive)
  (my/region/convert/by-shell-command "yq -p=props -o=yaml -"))

(defun my/region/convert/by-shell-command (command)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   command
   t
   t))
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

(keymap-global-set "s-h i" 'consult-imenu)
(keymap-global-set "s-h I" 'consult-imenu-multi)
(keymap-global-set "s-h e" 'consult-flymake)
(keymap-global-set "s-h r" 'consult-register-store)
(keymap-global-set "s-h R" 'consult-register)
(keymap-global-set "s-h f" 'consult-focus-lines)
(keymap-global-set "s-h k" 'consult-keep-lines)
(keymap-global-set "s-h m" 'consult-mark)
(keymap-global-set "s-h M" 'consult-global-mark)
(keymap-global-set "s-h o" 'consult-outline)
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
(keymap-set embark-region-map "a a" #'align)        ; [a]lign
(keymap-set embark-region-map "a r" #'align-regexp) ; [a]lign-[r]egexp

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
;; # dired

(require 'dired)

(setq insert-directory-program "gls")
(setopt dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")

(setopt dired-recursive-deletes 'always)

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

(keymap-set dired-mode-map "=" #'my/dired/ediff) ; was dired-diff

(defun my/dired/ediff ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (if (<= (length files) 2)
        (let* ((file1 (car files))
               (file2 (if (cdr files)
                          (cadr files)
                        (read-file-name (format "Ediff %s with: " (file-name-nondirectory file1))
                                        (dired-dwim-target-directory)))))
          (window-toggle-side-windows)
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2)))
      (error "No more than 2 files should be marked"))))
;; >-------------------------

;; <-------------------------
;; ## dired-hacks

;; https://github.com/Fuco1/dired-hacks

;; <----------
;; ### dired-subtree

(keymap-set dired-mode-map "i" #'dired-subtree-insert) ; was dired-maybe-insert-subdir
(keymap-set dired-mode-map "TAB" #'dired-subtree-toggle)

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

(defvar-keymap my/dired-subtree/tree-repeat-map
  :repeat t
  "n" #'dired-subtree-next-sibling
  "p" #'dired-subtree-previous-sibling
  "u" #'dired-subtree-up
  "d" #'dired-subtree-down
  "a" #'dired-subtree-beginning
  "e" #'dired-subtree-end
  "m" #'dired-subtree-mark-subtree
  "U" #'dired-subtree-unmark-subtree
  "N" #'dired-subtree-narrow
  "k" #'dired-subtree-remove
  "<backspace>" #'dired-subtree-remove
  "g" #'dired-subtree-revert
  "TAB" #'dired-subtree-toggle)
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
        '("Chart.yaml"))

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

(keymap-global-set "s-m" 'magit-status) ; was iconify-frame

(keymap-global-set "s-i v d" #'magit-diff-buffer-file)
(keymap-global-set "s-i v l" #'magit-log-buffer-file)
(keymap-global-set "s-i v m" #'magit-submodule)
(keymap-global-set "s-i v b" #'magit-blame)

; [o]pen; was magit-submodule
(keymap-set magit-revision-mode-map "o" #'magit-diff-visit-worktree-file-other-window)
(keymap-set magit-status-mode-map "o" #'magit-diff-visit-worktree-file-other-window)
(keymap-set magit-diff-mode-map "o" #'magit-diff-visit-worktree-file-other-window)

(keymap-set magit-revision-mode-map "C-c d" #'magit-dired-jump)
(keymap-set magit-status-mode-map "C-c d" #'magit-dired-jump)
(keymap-set magit-diff-mode-map "C-c d" #'magit-dired-jump)

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

(keymap-set project-prefix-map "t d" #'magit-todos-list)

(keymap-set magit-todos-item-section-map "o" #'magit-todos-jump-to-item)
;; >----------

;; <----------
;; ### difftastic.el

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
         magit-diff-hunk-heading
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

(keymap-global-set "M-j" #'avy-goto-char-timer) ; was default-indent-new-line
(keymap-global-set "M-J" #'avy-resume)
(keymap-global-set "M-g" #'avy-goto-line)       ; was goto-line
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

(setopt aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(setq aw-dispatch-alist
      '((?w aw-delete-window "Delete")
        (?x aw-swap-window "Swap")
        (?m aw-move-window "Move")
        (?c aw-copy-window "Copy")))
;; >-------------------------

;; <-------------------------
;; ## winner

(require 'winner)

(winner-mode)

(keymap-global-set "s-i l u" #'winner-undo) ; [l]ayout
(keymap-global-set "s-i l r" #'winner-redo)

(defvar-keymap my/winner/repeat-map
  :repeat t
  "u" #'winner-undo
  "r" #'winner-redo)
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

(defun my/display-buffer-alist/condition-by-major-modes (major-modes)
  (lambda (buffer-name action)
    (with-current-buffer buffer-name (apply #'derived-mode-p major-modes))))

(add-to-list
 'display-buffer-alist
 `(,(rx (| "*vterm*"
           "*Help*"
           "*Customize "
           "*Messages*"
           "*compilation*"
           "*eldoc*"
           "*just"
           "*Warnings*"
           "*docker-containers*"
           "*diff-syntax:"))
   display-buffer-in-side-window
   (window-height . 0.3)
   (window-parameters (no-delete-other-windows . t))))

(add-to-list
 'display-buffer-alist
 `(,(rx (| "*edit-indirect "
           "*yaml-pro-edit*"))
   display-buffer-in-side-window
   (window-height . 0.45)
   (window-parameters (no-delete-other-windows . t))))

(add-to-list
 'display-buffer-alist
 `(,(my/display-buffer-alist/condition-by-major-modes
     '(grep-mode
       occur-mode
       justl-mode
       xref--xref-buffer-mode))
   display-buffer-in-side-window
   (window-height . 0.3)
   (window-parameters (no-delete-other-windows . t))))

;; <----------
;; ### dired

(add-to-list
 'display-buffer-alist
 `(,(my/display-buffer-alist/condition-by-major-modes '(dired-mode))
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
   (window-height . 0.6)))

(add-to-list
 'display-buffer-alist
 '("COMMIT_EDITMSG"
   display-buffer-in-side-window
   (window-height . 0.6)
   (window-parameters (no-delete-other-windows . t))))
;; >----------

;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # multiple-cursors

;; https://github.com/magnars/multiple-cursors.el

(require 'multiple-cursors)

(keymap-global-set "s-i M" 'mc/edit-lines)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Mode Line

;; <-------------------------
;; ## doom-modeline

;; https://github.com/seagle0128/doom-modeline#install

(require 'doom-modeline)

(doom-modeline-mode 1)

;; M-x nerd-icons-install-fonts
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

(keymap-global-set "s-i r r" #'query-replace-regexp)
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
  (consult-ripgrep '(4) initial))
;; >-------------------------

;; <-------------------------
;; ## No .gitignore

(keymap-global-set "s-i g" #'consult-grep)
(keymap-global-set "s-i G" #'my/consult-grep/dir)

(defun my/consult-grep/dir ()
  (interactive)
  (consult-grep '(4) nil))
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

;; <-------------------------
;; ## eldoc-box

;; (keymap-global-set "C-h ." #'eldoc-box-help-at-point) ; was eldoc-doc-buffer
;; Deleted, prefer side window
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # lisp

(keymap-global-set "s-i m e" #'emacs-lisp-mode)

;; <-------------------------
;; ## Tree repeat-map

(defvar-keymap my/lisp/tree-repeat-map
  :repeat t
  "n" #'forward-list
  "p" #'backward-list
  "u" #'backward-up-list
  "d" #'down-list
  "a" #'beginning-of-defun
  "e" #'end-of-defun
  "f" #'forward-sexp
  "b" #'backward-sexp
  "k" #'kill-sexp
  "<backspace>" #'backward-kill-sexp
  "SPC" #'my/lisp/set-mark)

(dolist (cmd '(treesit-beginning-of-defun
               treesit-end-of-defun))
  (put cmd 'repeat-map 'my/lisp/tree-repeat-map))

(defun my/lisp/set-mark ()
  (interactive)
  (my/region/set-mark 'my/lisp/set-mark))
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

;; https://github.com/joaotavora/eglot/pull/937
;; https://github.com/joaotavora/eglot/pull/937/files
;;
;;; eclipse-jdt breaks the spec which in turn breaks code actions
;;; This behaviour can't be disabled and needs to be worked around
(cl-defmethod eglot-execute-command
  (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
  "Eclipse JDT breaks spec and replies with edits as arguments."
  (mapc #'eglot--apply-workspace-edit arguments))

(keymap-global-unset "s-l") ; [l]sp; was goto-line

(keymap-global-set "s-l e" #'eglot)

(keymap-set eglot-mode-map "s-." #'eglot-code-actions) ; was customize
(keymap-set eglot-mode-map "s-l r" #'eglot-rename)
(keymap-set eglot-mode-map "s-l i" #'eglot-find-implementation)
(keymap-set eglot-mode-map "s-l t" #'eglot-find-typeDefinition)
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
;; # nxml-mode

;; ref rng-nxml-mode-init

(add-hook 'nxml-mode-hook 'my/nxml-mode-hook)

(defun my/nxml-mode-hook ()
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))
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

(require 'polymode)

(defvar-keymap my/polymode/repeat-map
  :repeat (:exit
           (my/polymode/edit-chunk))
  "n" #'polymode-next-chunk
  "p" #'polymode-previous-chunk
  "k" #'polymode-kill-chunk
  "m" #'polymode-mark-or-extend-chunk
  "e" #'my/polymode/edit-chunk
  "SPC" #'my/polymode/set-mark)

(keymap-set polymode-mode-map "C-c p" my/polymode/repeat-map)

(defun my/polymode/edit-chunk ()
  (interactive)
  (call-interactively 'polymode-mark-or-extend-chunk)
  (call-interactively 'edit-indirect-region))

(defun my/polymode/set-mark ()
  (interactive)
  (my/region/set-mark 'my/polymode/set-mark))

;; <-------------------------
;; ## Bash

(define-innermode poly-bash-innermode
  :mode 'bash-ts-mode
  :head-matcher "^ *#!/usr/bin/env \\(sh\\|bash\\)\n"
  :tail-matcher "^ *# </bash>$"
  :head-mode 'body
  :tail-mode 'body)

;; <----------
;; ### Disable eglot

;; https://github.com/polymode/polymode/issues/331

(add-hook 'eglot-managed-mode-hook #'my/polymode/poly-bash-innermode/eglot-managed-mode-hook)

(defun my/polymode/poly-bash-innermode/eglot-managed-mode-hook ()
  (when (and polymode-mode
             (eq major-mode 'bash-ts-mode)
             (eglot-managed-p))
    (eglot--managed-mode-off)))
;; >----------

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

(setq whitespace-style (delete 'lines whitespace-style))

(setopt indent-tabs-mode nil)

(keymap-global-set "s-i m w" #'whitespace-mode)
(keymap-global-set "M-\\" #'delete-trailing-whitespace) ; was delete-horizontal-space
(keymap-global-set "M-|" #'delete-all-space)

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
;; # Misc

(setopt inhibit-startup-screen t)
(setopt initial-major-mode 'text-mode)
(setopt fill-column 90)
(setopt make-backup-files nil)
(setopt scroll-bar-mode nil)
(setopt tool-bar-mode nil)

(setq y-or-n-p-use-read-key t)
(setopt use-short-answers t)

(electric-pair-mode)

(setopt ring-bell-function #'ignore)

(defvar-keymap my/paragraphs/repeat-map
  :repeat t
  "{" #'backward-paragraph
  "}" #'forward-paragraph
  "[" #'backward-paragraph
  "]" #'forward-paragraph)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Bash

(require 'sh-script)

;; $ brew install bash-language-server

(advice-remove 'bash-ts-mode #'sh--redirect-bash-ts-mode)

(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))

(keymap-global-set "s-i m b" #'bash-ts-mode)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; Dockerfile

(require 'dockerfile-ts-mode)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Go

(require 'go-ts-mode)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # TypeScript

(require 'typescript-ts-mode)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # edit-indirect

(require 'edit-indirect)

(keymap-set embark-region-map "e i" #'edit-indirect-region)

(setq edit-indirect-guess-mode-function #'my/edit-indirect/guess-mode)

(defun my/edit-indirect/guess-mode (_parent-buffer _beg _end)
  (setq-local buffer-file-name (format "%s.-ei-" (buffer-file-name _parent-buffer)))
  (funcall (buffer-local-value 'major-mode _parent-buffer)))

;; <-------------------------
;; ## Left margin

;; https://github.com/Fanael/edit-indirect/issues/6#issuecomment-387945773
(add-hook 'edit-indirect-after-creation-hook #'vbe/edit-indirect/remove-left-margin)
(add-hook 'edit-indirect-before-commit-hook #'vbe/edit-indirect/restore-left-margin)

;; https://github.com/Fanael/edit-indirect/issues/6#issuecomment-1284144173
(keymap-set edit-indirect-mode-map "<remap> <save-buffer>" #'edit-indirect-commit)

(defvar edit-indirect--left-margin 0)

(defun vbe/edit-indirect/remove-left-margin ()
  "Remove left-margin and save it into a local variable."
  (let ((lm (vbe/compute-left-margin (buffer-substring (point-min) (point-max)))))
    (indent-rigidly (point-min) (point-max) (* -1 lm))
    (setq-local edit-indirect--left-margin lm)
    (setq-local write-contents-functions '(my/edit-indirect/commit-on-save))
    ;; https://github.com/Fanael/edit-indirect/issues/6#issuecomment-1055542145
    ;; buffer-local variable whose value should not be reset when changing major modes
    (put 'edit-indirect--left-margin 'permanent-local t)))

(defun vbe/compute-left-margin (code)
  "Compute left margin of a string of code."
  (-min
   (-map #'(lambda (line) (length (car (s-match "^\\s-*" line))))
         (-remove 's-blank? (s-lines code)))))

(defun my/edit-indirect/commit-on-save ()
  (edit-indirect--commit)
  (delete-overlay edit-indirect--overlay)
  (setq edit-indirect--overlay nil)
  (kill-buffer)
  t)

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

(add-hook 'json-ts-mode-hook 'flymake-mode)

(use-package json-ts-mode
  :flymake-hook
  (json-ts-mode flymake-collection-jq))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # YAML

(require 'yaml-ts-mode)

(keymap-global-set "s-i m y" #'yaml-ts-mode)

;; <-------------------------
;; ## flymake

;; $ brew install yamllint

(add-hook 'yaml-ts-mode-hook 'flymake-mode)

(use-package yaml-ts-mode
  :flymake-hook
  (yaml-ts-mode flymake-collection-yamllint))
;; >-------------------------

;; <-------------------------
;; ## yaml-pro

;; https://github.com/zkry/yaml-pro

(require 'yaml-pro)

(setopt yaml-pro-ts-yank-subtrees nil)

(add-hook 'yaml-ts-mode-hook #'yaml-pro-ts-mode)

;; https://github.com/zkry/yaml-pro/issues/45
;; (keymap-set yaml-pro-ts-mode-map "<return>" nil)
;; TODO

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
  "a" #'yaml-pro-ts-first-sibling
  "e" #'yaml-pro-ts-last-sibling
  "t" #'yaml-pro-ts-move-subtree-up
  "T" #'yaml-pro-ts-move-subtree-down
  "SPC" #'my/yaml-pro/set-mark)

(defun my/yaml-pro/set-mark ()
  (interactive)
  (my/region/set-mark 'my/yaml-pro/set-mark))
;; >----------

(keymap-set yaml-pro-ts-mode-map "C-c m" #'yaml-pro-ts-mark-subtree)
(keymap-set yaml-pro-ts-mode-map "C-c y" #'yaml-pro-ts-paste-subtree)

;; <----------
;; ### Save path

(keymap-set yaml-pro-ts-mode-map "C-c w" #'my/yaml-pro/save-path)

(defun my/yaml-pro/save-path ()
  (interactive)
  (kill-new (yaml-pro-ts-eldoc)))
;; >----------

;; This is not available for tree-sitter variant.
;; Presumably some tree-sitter folding package will exist in the future.
;; (keymap-set yaml-pro-ts-mode-map "C-c c" 'yaml-pro-ts-fold-at-point)     ; [c]ollapse
;; (keymap-set yaml-pro-ts-mode-map "C-c e" 'yaml-pro-ts-unfold-at-point)   ; [e]xpand

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

(add-to-list 'auto-mode-alist '("\\.yaml" . poly-yaml-ts-mode))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # markdown-mode

(require 'markdown-mode)

(setopt markdown-command "multimarkdown")
(setopt markdown-display-remote-images t)
(setopt markdown-fontify-code-blocks-natively t)
(setopt markdown-max-image-size '(500 . 500))

(keymap-set markdown-mode-map "C-c c" #'markdown-insert-code)
(keymap-set markdown-mode-map "C-c b" #'markdown-insert-gfm-code-block)
(keymap-set markdown-mode-map "C-c q" #'markdown-insert-blockquote)
(keymap-set markdown-mode-map "C-c e" #'markdown-insert-bold) ; [e]mphasize
(keymap-set markdown-mode-map "C-c l" #'markdown-insert-link)
(keymap-set markdown-mode-map "C-c h" #'markdown-insert-header-dwim)
(keymap-set markdown-mode-map "C-c i" #'markdown-insert-image)
(keymap-set markdown-mode-map "C-c I" #'markdown-toggle-inline-images)
(keymap-set markdown-mode-map "C-c m" #'markdown-mark-block)
(keymap-set markdown-mode-map "C-c k" #'markdown-kill-block)

;; <-------------------------
;; ## Heading jump (repeat-map)

(keymap-set markdown-mode-map "C-M-n" #'markdown-forward-same-level)   ; was forward-list
(keymap-set markdown-mode-map "C-M-p" #'markdown-backward-same-level)  ; was backward-list
(keymap-set markdown-mode-map "C-M-u" #'markdown-up-heading)           ; was backward-up-list
(keymap-set markdown-mode-map "C-M-d" #'markdown-next-visible-heading) ; was down-list
(keymap-set markdown-mode-map "C-M-f" #'markdown-forward-block)        ; was forward-sexp
(keymap-set markdown-mode-map "C-M-b" #'markdown-backward-block)       ; was backward-sexp

(defvar-keymap my/markdown/heading-jump-repeat-map
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

(keymap-global-set "s-i m J" #'jinja2-mode)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # pdf-tools

;; https://github.com/vedang/pdf-tools#view-and-navigate-pdfs

(require 'pdf-tools)

(pdf-loader-install)
(add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)
(keymap-set pdf-view-mode-map  "M-g" #'pdf-view-goto-page)
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

(add-to-list 'auto-mode-alist '("\\.groovy" . poly-groovy-mode))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # eglot-booster

;; (require 'eglot-booster)
;; (eglot-booster-mode)
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
;; # Custom faces

;; <-------------------------
;; Util

(defun my/face/color-name-to-hex (name)
  (apply 'color-rgb-to-hex (color-name-to-rgb name)))

(defun my/color-darken-name (name percent &optional desaturate-percent)
  (color-desaturate-name (color-darken-name name percent)
                         (or desaturate-percent percent)))
;; >-------------------------

(set-face-attribute 'default nil :family "JetBrains Mono" :height 130)

(set-face-attribute 'aw-leading-char-face nil :height 1.0 :weight 'bold :foreground "green")

(set-face-attribute
 'hl-line nil :background
 (my/color-darken-name (face-attribute 'hl-line :background) 20))

(set-face-attribute
 'highlight-function-calls-face nil
 :underline nil :inherit 'font-lock-function-call-face)

;; <-------------------------
;; ## Magit

(let ((added (face-attribute 'magit-diff-added :foreground))
      (removed (face-attribute 'magit-diff-removed :foreground)))
  (set-face-attribute 'magit-diff-added nil :foreground (my/color-darken-name added 30 20))
  (set-face-attribute 'magit-diff-removed nil :foreground (my/color-darken-name removed 30))
  (set-face-attribute 'diff-refine-added nil :foreground added)
  (set-face-attribute 'diff-refine-removed nil :foreground removed))

(set-face-attribute
 'magit-diff-hunk-heading-highlight nil :background
 (my/color-darken-name (face-attribute 'magit-diff-base-highlight :background) 45))

(set-face-attribute
 'magit-section-highlight nil :background
 (my/color-darken-name (face-attribute 'magit-section-highlight :background) 20))

(set-face-attribute
 'magit-diff-hunk-heading nil :background
 (my/color-darken-name (face-attribute 'magit-diff-hunk-heading :background) 45))

(set-face-attribute
 'magit-blame-heading nil :background
 (my/color-darken-name (face-attribute 'magit-blame-heading :background) 35))
;; >-------------------------

(set-face-attribute
 'highlight nil :background
 (my/color-darken-name (face-attribute 'highlight :background) 30))

(set-face-attribute 'consult-file nil :inherit 'consult-buffer)

(set-face-attribute 'avy-lead-face nil :foreground "green" :background "reset")

(set-face-attribute 'eglot-highlight-symbol-face nil :overline "SpringGreen3")

;; <-------------------------
;; ## symbol-overlay

(set-face-attribute 'symbol-overlay-default-face nil :inherit 'eglot-highlight-symbol-face)

(set-face-attribute 'symbol-overlay-face-2 nil :background "lavender")
(set-face-attribute 'symbol-overlay-face-5 nil :background "burlywood")

(dotimes (i 8)
  (let ((face (intern (format "symbol-overlay-face-%s" (+ i 1)))))
    (set-face-attribute
     face nil
     :background
     (my/color-darken-name (face-attribute face :background) 30)
     :foreground
     (face-attribute 'lazy-highlight :foreground))))
;; >-------------------------

;; <-------------------------
;; ## ediff

(let* ((current (face-attribute 'ediff-even-diff-A :background))
       ;; (current (my/color-darken-name current 5))
       (even (my/color-darken-name current 18))
       (odd (my/color-darken-name current 20)))

  (set-face-attribute 'ediff-current-diff-A nil :inherit 'magit-diff-removed :background current)
  (set-face-attribute 'ediff-current-diff-B nil :inherit 'magit-diff-added :background current)

  (set-face-attribute 'ediff-fine-diff-A nil :inherit 'diff-refine-removed :background current)
  (set-face-attribute 'ediff-fine-diff-B nil :inherit 'diff-refine-added :background current)

  (set-face-attribute 'ediff-even-diff-A nil :background even)
  (set-face-attribute 'ediff-even-diff-B nil :background even)

  (set-face-attribute 'ediff-odd-diff-A nil :background odd)
  (set-face-attribute 'ediff-odd-diff-B nil :background odd))
;; >-------------------------

;; <-------------------------
;; ## Markdown

(let ((quote (face-attribute 'markdown-blockquote-face :foreground nil t)))
  (set-face-attribute 'markdown-blockquote-face nil :foreground (my/color-darken-name quote 20))
  (set-face-attribute 'markdown-bold-face nil :foreground quote :weight 'normal))

(let ((h1 (face-attribute 'font-lock-keyword-face :foreground))
      (h2 (face-attribute 'font-lock-function-name-face :foreground)))
  (set-face-attribute 'markdown-header-face-1 nil
                      :weight 'bold :foreground h1)
  (set-face-attribute 'markdown-header-face-3 nil
                      :weight 'bold :foreground (my/color-darken-name h1 15))
  (set-face-attribute 'markdown-header-face-5 nil
                      :weight 'bold :foreground (my/color-darken-name h1 30))
  (set-face-attribute 'markdown-header-face-2 nil
                      :weight 'bold :foreground h2)
  (set-face-attribute 'markdown-header-face-4 nil
                      :weight 'bold :foreground (my/color-darken-name h2 15))
  (set-face-attribute 'markdown-header-face-6 nil
                      :weight 'bold :foreground (my/color-darken-name h2 30)))
;; >-------------------------

;; >--------------------------------------------------
