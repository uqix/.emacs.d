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

;; (load-theme 'doom-one t)

(load-theme 'gruvbox t)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Dynamic abbrev expansion

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html

;; M-/ -> dabbrev-expand
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

;; https://www.emacswiki.org/emacs/FrameTitle#h5o-6
(setq frame-title-format "%b <%f>")

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Spellcheck

;; <-------------------------
;; ## ispell

(require 'ispell)

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

(keymap-global-set "s-i m f" #'flymake-mode) ; [m]ode: [f]lymake

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

(keymap-set minibuffer-mode-map "C-c h" 'consult-history)

;; <-------------------------
;; ## vertico

(require 'vertico)

(global-unset-key (kbd "s-h")) ; was ns-do-hide-emacs

;; https://github.com/minad/vertico#configuration
(vertico-mode)

;; https://www.emacswiki.org/emacs/SaveHist
(savehist-mode 1)

;; https://github.com/minad/vertico/blob/main/extensions/vertico-repeat.el
(keymap-global-set "s-h b" #'vertico-repeat) ; [b]ack to last interaction
(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

;; https://github.com/minad/vertico/blob/main/extensions/vertico-quick.el
(keymap-set vertico-map "M-g" #'vertico-quick-insert)
;; >-------------------------

;; <-------------------------
;; ## corfu

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
(add-hook 'minibuffer-setup-hook #'my/corfu/minibuffer-setup-hook 1)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Block collapse/expand

(require 'hideshow)

(add-hook 'prog-mode-hook 'hs-minor-mode)

(keymap-global-set "s-i b t" #'hs-toggle-hiding) ; [b]lock: [t]oggle
(keymap-global-set "s-i b H" #'hs-hide-all)      ; [b]lock: [H]ide all
(keymap-global-set "s-i b S" #'hs-show-all)      ; [b]lock: [S]how all

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # isearch

(require 'isearch)

(keymap-set key-translation-map "s-u" "C-s")
(keymap-set key-translation-map "s-y" "C-r")

(keymap-set isearch-mode-map "s-r" #'isearch-query-replace)
(keymap-set isearch-mode-map "s-f" #'consult-line)
(keymap-set isearch-mode-map "C-c h" #'consult-isearch-history)

(defun my/avy-isearch ()
  (interactive)
  (isearch-done)
  (avy-isearch))

(keymap-set isearch-mode-map "M-j" 'my/avy-isearch)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; Buffer

(keymap-global-set "s-i b r" #'revert-buffer)       ; [b]uffer: [r]evert
(keymap-global-set "s-i b k" #'kill-current-buffer) ; [b]uffer: [k]ill

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

(global-unset-key (kbd "C-x C-c")) ; was quit emacs
(global-unset-key (kbd "C-x C-z")) ; was suspend-frame (minimize)

(keymap-set key-translation-map "s-SPC" "C-g")

(keymap-global-set "C-z" ctl-x-map)
(keymap-global-set "s-z" #'execute-extended-command)

(keymap-global-set "M-'" #'subword-mode) ; was abbrev-prefix-mark

(keymap-global-set "s-d" #'duplicate-dwim) ; was isearch-repeat-backward

(keymap-global-set "s-i f" #'find-file)

(keymap-global-set "s-r" #'replace-string)

(keymap-global-set "C-\\" #'toggle-truncate-lines) ; was toggle-input-method

(keymap-global-set "C-." #'pop-to-mark-command) ; was flyspell-auto-correct-word

(keymap-global-set "s-i z" #'repeat) ; z z z…

;; <-------------------------
;; Next match

;; normally uses the most recently started compilation, grep, or occur buffer

(keymap-global-set "s-{" #'next-error-select-buffer)
(keymap-global-set "s-[" #'previous-error)
(keymap-global-set "s-]" #'next-error)
;; >-------------------------

(keymap-global-set "s-i m t" #'text-mode)    ; [m]ode: [t]ext

(keymap-global-set "C-h F" #'describe-face)   ; [h]elp: [F]ace
(keymap-global-set "C-h K" #'describe-keymap) ; [h]elp: [K]eymap

;; s-1: macOS Keyboard Shortcuts - Mission Control - Switch to Desktop 1 (Misc)
;; s-2: macOS Keyboard Shortcuts - Mission Control - Switch to Desktop 2 (Emacs)
;; s-3: macOS Keyboard Shortcuts - Mission Control - Switch to Desktop 3 (Chrome)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Highlight

;; <-------------------------
;; ## Regexp

;; <----------
;; ### Highlight (region aware)
(keymap-global-set "s-i h r" 'my/highlight-regexp) ; [h]ighlight: [r]egexp

(defun my/highlight-regexp ()
  (interactive)
  (my/region/with-str 'my/highlight-regexp//initial 'highlight-regexp))

(defun my/highlight-regexp//initial (initial)
  (highlight-regexp (regexp-quote initial) 'diff-error))
;; >----------

;; <----------
;; ### Unhighlight

(keymap-global-set "s-i h u" 'unhighlight-regexp) ; [h]ighlight: [u]nhighlight

(keymap-global-set "s-i h U" #'my/unhighlight-regexp/all) ; [h]ighlight: [U]nhighlight all

(defun my/unhighlight-regexp/all ()
  (interactive)
  (unhighlight-regexp t))
;; >----------

;; <-------------------------
;; ## symbol-overlay

(require 'symbol-overlay)

;; https://github.com/wolray/symbol-overlay/#usage

(global-unset-key (kbd "s-o"))

(keymap-global-set "M-o" 'symbol-overlay-put)               ; [o]verlay
(keymap-global-set "s-o n" 'symbol-overlay-switch-forward)  ; [n]ext
(keymap-global-set "s-o p" 'symbol-overlay-switch-backward) ; [p]revious
(keymap-global-set "s-o r" 'symbol-overlay-remove-all)      ; [r]emove
(keymap-global-set "s-o m" 'symbol-overlay-mode)            ; [m]ode

(keymap-set symbol-overlay-map "C-s" #'symbol-overlay-isearch-literally)

;; <----------
;; ### avy jump

(keymap-set symbol-overlay-map "j" #'my/symbol-overlay/avy-jump)

(defun my/symbol-overlay/avy-jump ()
  (interactive)
  (avy-with my/symbol-overlay/avy-jump
    (avy-jump
     (symbol-overlay-regexp (symbol-overlay-get-symbol))
     :window-flip nil
     :beg nil
     :end nil)))
;; >----------

;; <----------
;; ### avy jump among all

(keymap-global-set "s-o j" #'my/symbol-overlay/avy-jump/all)
(keymap-set symbol-overlay-map "J" #'my/symbol-overlay/avy-jump/all)

(defun my/symbol-overlay/avy-jump/all ()
  (interactive)
  (let* ((overlays (symbol-overlay-get-list 0))
         (symbols (seq-map
                   (lambda (overlay)
                     (overlay-get overlay 'symbol))
                   overlays))
         (symbols (seq-uniq symbols))
         (symbols-regex (seq-map
                         (lambda (symbol)
                           (symbol-overlay-regexp symbol))
                         symbols))
         (symbols-regex (string-join symbols-regex "\\|")))
    (message (format "regex: %s (%s)" symbols-regex (length symbols-regex)))
    (if (string-empty-p symbols-regex)
        (message "No symbol overlay")
      (avy-with my/symbol-overlay/avy-jump/all
        (avy-jump
         symbols-regex
         :window-flip nil
         :beg nil
         :end nil)))))
;; >----------

;; <----------
;; ### Find in current buffer

(keymap-set symbol-overlay-map "s-f" #'my/symbol-overlay/find)

(defun my/symbol-overlay/find ()
  (interactive)
  (consult-line (symbol-overlay-get-symbol)))
;; >----------

;; <----------
;; ### Find in project buffers

(keymap-set symbol-overlay-map "s-F" #'my/symbol-overlay/find/project)

(defun my/symbol-overlay/find/project ()
  (interactive)
  (consult-line-multi nil (symbol-overlay-get-symbol)))
;; >----------

;; <----------
;; ### Grep in project

(keymap-set symbol-overlay-map "s-g" #'my/symbol-overlay/grep/project)

(defun my/symbol-overlay/grep/project ()
  (interactive)
  (consult-ripgrep nil (symbol-overlay-get-symbol)))
;; >----------

;; <----------
;; ### Grep in specific dir

(keymap-set symbol-overlay-map "s-G" #'my/symbol-overlay/grep/dir)

(defun my/symbol-overlay/grep/dir ()
  (interactive)
  (consult-ripgrep '(4) (symbol-overlay-get-symbol)))
;; >----------

;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Diff

(require 'diff-mode)

(keymap-set diff-mode-shared-map "t" 'diff-delete-trailing-whitespace) ; [t]rim

(keymap-global-set "s-i d b" #'ediff-buffers) ; [d]iff: [b]uffers

;; <-------------------------
;; ## ztree

(require 'ztree)

(keymap-global-set "s-i d d" #'ztree-diff) ; [d]iff: [d]irs
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Shell

(require 'shell)

(keymap-set shell-mode-map "C-c h" 'consult-history)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Region

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

;; <-------------------------
;; ## Narrowing

(put 'narrow-to-region 'disabled nil)

(keymap-global-set "s-i n n" #'narrow-to-region) ; [n]arrow: region
(keymap-global-set "s-i n f" #'narrow-to-defun)  ; [n]arrow: [f]unction
(keymap-global-set "s-i n w" #'widen)            ; [n]arrow: [w]iden
;; >-------------------------

;; <-------------------------
;; ## expand-region

;; https://github.com/magnars/expand-region.el

(require 'expand-region)

(keymap-global-set "M-[" 'er/expand-region)
;; >-------------------------

;; <-------------------------
;; ## Select by separator

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

;; TODO
(keymap-global-set "s-i s s" 'my/select-text/between-spaces)         ; [s]elect text between [s]paces
(keymap-global-set "s-i s /" 'my/select-text/between-slashes)        ; [s]elect text between /
(keymap-global-set "s-i s ," 'my/select-text/between-commas)         ; [s]elect text between ,
(keymap-global-set "s-i s '" 'my/select-text/between-single-quotes)  ; [s]elect text between '
(keymap-global-set "s-i s \"" 'my/select-text/between-double-quotes) ; [s]elect text between "
(keymap-global-set "s-i s j" 'my/select-text/java-text-block)        ; [s]elect text in [j]ava text block
;; >-------------------------

;; <-------------------------
;; ## Case convert

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(keymap-global-set "M-L" #'downcase-region) ; [L]owercase region
(keymap-global-set "M-U" #'upcase-region)   ; [U]ppercase region

;; https://stackoverflow.com/a/61745441/9154901
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

(with-eval-after-load 'embark
  (keymap-set embark-region-map "c" nil)
  (keymap-set embark-region-map "c s" #'my/region/convert/snake-case)
  (keymap-set embark-region-map "c c" #'my/region/convert/camel-case)
  (keymap-set embark-region-map "c k" #'my/region/convert/kebab-case)
  (keymap-set embark-region-map "c C" #'my/region/convert/capitalize))
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

(keymap-global-set "s-j" 'consult-buffer) ; [j]ump to buffer; was exchange-point-and-mark

(keymap-global-set "M-y" 'consult-yank-replace)                   ; [y]ank

(keymap-global-set "s-h i" 'consult-imenu)          ; [i]menu
(keymap-global-set "s-h I" 'consult-imenu-multi)    ; [I]menu
(keymap-global-set "s-h e" 'consult-flymake)        ; [e]rrors
(keymap-global-set "s-h r" 'consult-register-store) ; [r]egister: add
(keymap-global-set "s-h R" 'consult-register)       ; [R]egister: list
(keymap-global-set "s-h f" 'consult-focus-lines)    ; [f]ocus
(keymap-global-set "s-h k" 'consult-keep-lines)     ; [k]eep
(keymap-global-set "s-h m" 'consult-mark)           ; [m]ark
(keymap-global-set "s-h M" 'consult-global-mark)    ; [M]ark
(keymap-global-set "s-h o" 'consult-outline)        ; [o]utline
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # embark

(require 'embark)

(setopt embark-confirm-act-all nil)
(setopt embark-mixed-indicator-delay 1.5)

(keymap-global-set "M-i" 'embark-act) ; was tab-to-tab-stop

(keymap-set embark-file-map "$" 'shell)

(keymap-set embark-region-map "e" nil)              ; was eval-region
(keymap-set embark-region-map "e e" #'eval-region)  ; [e]val elisp
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
(keymap-set embark-general-map "s-G" #'my/grep/dir//read-initial)

(defun my/grep/dir//read-initial ()
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

;; <-------------------------
;; ## dirvish

(require 'dirvish)

;; https://github.com/alexluigit/dirvish?tab=readme-ov-file#macos
;; $ brew install coreutils fd poppler ffmpegthumbnailer mediainfo imagemagick

;; https://github.com/alexluigit/dirvish/blob/main/docs/CUSTOMIZING.org#dirvish

(dirvish-override-dired-mode)

(setq dirvish-attributes
      '(all-the-icons file-time file-size subtree-state))

(keymap-set dirvish-mode-map "TAB" #'dirvish-subtree-toggle)
(keymap-set dirvish-mode-map "s" #'dirvish-quicksort)         ; [s]ort; was dired-sort-toggle-or-edit
(keymap-set dirvish-mode-map "M" #'dirvish-mark-menu)         ; [m]ark menu; was dired-do-chmod
(keymap-set dirvish-mode-map "C-c m" #'dired-do-chmod)        ; ch[m]od
(keymap-set dirvish-mode-map "C-c l" #'dirvish-layout-toggle) ; [l]ayout

(keymap-global-set "s-J" 'dirvish-history-jump) ; [J]ump to dired

;; <----------
;; ### Mouse

;; https://github.com/alexluigit/dirvish/blob/main/docs/CUSTOMIZING.org#mouse-settings

(defun my/dirvish-setup-hook ()
  (setq-local mouse-1-click-follows-link nil))

(add-hook 'dirvish-setup-hook #'my/dirvish-setup-hook)

(keymap-set dirvish-mode-map "<mouse-1>" #'dirvish-subtree-toggle-or-open)
(keymap-set dirvish-mode-map "<mouse-2>" #'dirvish-subtree-toggle-or-open)
(keymap-set dirvish-mode-map "<mouse-3>" #'dired-mouse-find-file-other-window)
;; >----------

;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # project

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html

(require 'project)

(setopt project-vc-extra-root-markers
        '("Chart.yaml"))

(keymap-global-set "s-p" project-prefix-map)

(keymap-set project-prefix-map "b" #'consult-project-buffer)

;; <-------------------------
;; ## Test file

(defun my/project/test/file ()
  (interactive)
  (let* ((file (buffer-file-name))
         (is-main-file (string-match "^\\(.+/src/main/java/.+/.+\\).java$" file))
         (is-test-file (and (not is-main-file) (string-match "^\\(.+/src/test/java/.+/.+\\)Test.java$" file))))
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

(keymap-set project-prefix-map "t f" #'my/project/test/file) ; [t]est: jump between main and test [f]iles
;; >-------------------------

;; <-------------------------
;; ## Test class

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

(keymap-set project-prefix-map "t c" #'my/project/test/class) ; [t]est: [c]lass
;; >-------------------------

;; <-------------------------
;; ## Test method

(defun my/project/test/method ()
  (interactive)
  (my/project/test/class (read-from-minibuffer "Method: ")))

(keymap-set project-prefix-map "t m" #'my/project/test/method) ; [t]est: [m]ethod
(keymap-set embark-identifier-map "t" #'my/project/test/method)
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

(keymap-global-set "s-m" 'magit-status)     ; [m]agit; was iconify-frame

(keymap-global-set "s-i v d" #'magit-diff-buffer-file) ; [v]c: [d]iff
(keymap-global-set "s-i v l" #'magit-log-buffer-file)  ; [v]c: [l]og
(keymap-global-set "s-i v m" #'magit-submodule)        ; [v]c: sub[m]odule

(keymap-set magit-revision-mode-map "o" #'magit-diff-visit-worktree-file-other-window) ; [o]pen; was magit-submodule
(keymap-set magit-status-mode-map "o" #'magit-diff-visit-worktree-file-other-window)   ; [o]pen; was magit-submodule
(keymap-set magit-diff-mode-map "o" #'magit-diff-visit-worktree-file-other-window)     ; [o]pen; was magit-submodule

(require 'with-editor)
(keymap-set with-editor-mode-map "<remap> <save-buffer>" #'with-editor-finish)
;; >-------------------------

;; <-------------------------
;; ## git-timemachine

;; https://codeberg.org/pidu/git-timemachine

(require 'git-timemachine)

(keymap-global-set "s-i v t" #'git-timemachine) ; [v]c: git-[t]imemachine
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Window

(keymap-global-set "s-i w t" #'enlarge-window)              ; [w]indow: [t]aller
(keymap-global-set "s-i w s" #'shrink-window)               ; [w]indow: [s]horter
(keymap-global-set "s-i w w" #'enlarge-window-horizontally) ; [w]indow: [w]ider
(keymap-global-set "s-i w n" #'shrink-window-horizontally)  ; [w]indow: [n]arrower
(keymap-global-set "s-i w b" #'balance-windows)             ; [w]indow: [b]alance

;; <-------------------------
;; ## Split

(defun my/split-window-sensibly ()
  (interactive)
  (or
   (let ((split-height-threshold nil))
     (split-window-sensibly))
   (let ((split-width-threshold nil)
         (split-height-threshold 30))
     (split-window-sensibly))
   (message "Not splittable")))

(keymap-global-set "s-t" #'my/split-window-sensibly)
;; >-------------------------

(keymap-global-set "s-w" #'delete-window)
(keymap-global-set "s-e" #'delete-other-windows) ; was isearch-yank-kill

;; <-------------------------
;; ## avy jump

(require 'avy)

(setopt avy-timeout-seconds 0.7)

(keymap-global-set "M-j" 'avy-goto-char-timer) ; was default-indent-new-line
(keymap-global-set "M-J" 'avy-resume)
(keymap-global-set "M-g" 'avy-goto-line)       ; was goto-line

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

(keymap-global-set "s-n" 'ace-window) ; [w]indow: [n]umber; was make-frame

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(setq aw-dispatch-alist
  '((?w aw-delete-window "Delete Window")
    (?x aw-swap-window "Swap Windows")
    ;; (?M aw-move-window "Move Window")
    ;; (?c aw-copy-window "Copy Window")
    ;; (?j aw-switch-buffer-in-window "Select Buffer")
    (?b aw-flip-window)
    ;; (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
    ;; (?e aw-execute-command-other-window "Execute Command Other Window")
    ;; (?F aw-split-window-fair "Split Fair Window")
    ;; (?v aw-split-window-vert "Split Vert Window")
    ;; (?b aw-split-window-horz "Split Horz Window")
    ;; (?o delete-other-windows "Delete Other Windows")
    ;; (?T aw-transpose-frame "Transpose Frame")
    ;; ;; ?i ?r ?t are used by hyperbole.el
    ;; (?? aw-show-dispatch-help)
    ))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # multiple-cursors

;; https://github.com/magnars/multiple-cursors.el

(require 'multiple-cursors)

(keymap-global-set "s-i i m" 'mc/edit-lines) ; [i]nsert: [m]ultiple-cursors
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
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Find

(keymap-set occur-edit-mode-map "<remap> <save-buffer>" #'occur-cease-edit)

;; <-------------------------
;; ## In current buffer (region aware)

(keymap-global-set "s-f" 'my/find) ; [f]ind; was isearch-forward

(defun my/find ()
  (interactive)
  (my/region/with-str 'consult-line))
;; >-------------------------

;; <-------------------------
;; ## In project buffers (region aware)

(keymap-global-set "s-F" 'my/find/project)

(defun my/find/project ()
  (interactive)
  (my/region/with-str 'my/find/project//initial 'consult-line-multi))

(defun my/find/project//initial (initial)
  (consult-line-multi nil initial))
;; >-------------------------

;; <-------------------------
;; ## Replace

(keymap-global-set "s-i r r" #'query-replace-regexp) ; [r]eplace: regexp
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Grep

(require 'grep)

;; <-------------------------
;; ## wgrep

(require 'wgrep)

;; https://github.com/mhayashi1120/Emacs-wgrep

(keymap-set grep-mode-map "e" #'wgrep-change-to-wgrep-mode)             ; [e]dit
(keymap-set wgrep-mode-map "<remap> <save-buffer>" #'wgrep-finish-edit) ; commit

;; >-------------------------

;; <-------------------------
;; ## In project (region aware)

(keymap-global-set "s-g" 'my/grep/project) ; [g]rep; was isearch-repeat-forward

(defun my/grep/project ()
  (interactive)
  (my/region/with-str 'my/grep/project//initial 'consult-ripgrep))

(defun my/grep/project//initial (initial)
  (consult-ripgrep nil initial))

;; >-------------------------

;; <-------------------------
;; ## In specific dir (region aware)

(keymap-global-set "s-G" 'my/grep/dir)

(defun my/grep/dir ()
  (interactive)
  (my/region/with-str 'my/grep/dir//initial 'my/grep/dir//))

(defun my/grep/dir//initial (initial)
  (consult-ripgrep '(4) initial))

(defun my/grep/dir// ()
  (interactive)
  (consult-ripgrep '(4) nil))

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

;; * required by eglot

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

;; * required by eglot

;; C-h . -> eldoc-doc-buffer
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # eglot

(require 'eglot)

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

(global-unset-key (kbd "s-l")) ; was goto-line

(keymap-global-set "s-l e" #'eglot) ; [l]sp: [e]glot

(keymap-set eglot-mode-map "s-," #'eglot-code-actions)          ; [l]sp: actions; was customize
(keymap-set eglot-mode-map "s-l r" #'eglot-rename)              ; [l]sp: [r]ename
(keymap-set eglot-mode-map "s-l i" #'eglot-find-implementation) ; [l]sp: [i]mplementation
(keymap-set eglot-mode-map "s-l t" #'eglot-find-typeDefinition) ; [l]sp: [t]ype
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Java

(require 'java-ts-mode)

;; <-------------------------
;; ## eglot

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

;; https://github.com/yveszoundi/eglot-java/blob/ff0f9515d78f94b8dfe158bf9a2c4f52216504c0/eglot-java.el#L770

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
;; >----------

;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # nxml-mode

;; ref rng-nxml-mode-init
(defun my/nxml-mode-hook ()
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))
(add-hook 'nxml-mode-hook 'my/nxml-mode-hook)
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

(defun my/polymode/edit-chunk ()
  (interactive)
  (call-interactively 'polymode-mark-or-extend-chunk)
  (call-interactively 'edit-indirect-region))

(keymap-set polymode-mode-map "C-c p n" 'polymode-next-chunk)             ; [n]ext
(keymap-set polymode-mode-map "C-c p p" 'polymode-previous-chunk)         ; [p]revious
(keymap-set polymode-mode-map "C-c p t" 'polymode-toggle-chunk-narrowing) ; [t]oggle narrowing
(keymap-set polymode-mode-map "C-c p k" 'polymode-kill-chunk)             ; [k]ill
(keymap-set polymode-mode-map "C-c p m" 'polymode-mark-or-extend-chunk)   ; [m]ark
(keymap-set polymode-mode-map "C-c p e" 'my/polymode/edit-chunk)          ; [e]dit by edit-indirect

;; https://polymode.github.io/defining-polymodes/

(define-innermode poly-bash-innermode
  :mode 'bash-ts-mode
  :head-matcher "^ *#!/usr/bin/env \\(sh\\|bash\\)\n"
  :tail-matcher "^ *# </bash>$"
  :head-mode 'body
  :tail-mode 'body)

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Whitespaces

(require 'whitespace)

(setq whitespace-style (delete 'lines whitespace-style))

(setopt indent-tabs-mode nil)

(keymap-global-set "s-i m w" #'whitespace-mode) ; [m]ode: [w]hitespace
(keymap-global-set "M-\\" #'delete-trailing-whitespace) ; was delete-horizontal-space
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
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Bash

(require 'sh-script)

(advice-remove 'bash-ts-mode #'sh--redirect-bash-ts-mode)

(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
;; >--------------------------------------------------



;; <--------------------------------------------------
;; Dockerfile

(require 'dockerfile-ts-mode)

;; Fix generate-dockerfile.sh|docker-dockerfile.md opened in this mode
(delete '("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"
          . dockerfile-ts-mode)
        auto-mode-alist)
(add-to-list 'auto-mode-alist
             '("Dockerfile$" . dockerfile-ts-mode))
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

(keymap-set embark-region-map "e i" #'edit-indirect-region) ; [e]dit-[i]ndirect

(defun my/edit-indirect/guess-mode (_parent-buffer _beg _end)
  (setq-local buffer-file-name (format "%s.-ei-" (buffer-file-name _parent-buffer)))
  (funcall (buffer-local-value 'major-mode _parent-buffer)))
(setq edit-indirect-guess-mode-function #'my/edit-indirect/guess-mode)

;; <-------------------------
;; ## Left margin

;; https://github.com/Fanael/edit-indirect/issues/6#issuecomment-387945773

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
(keymap-set edit-indirect-mode-map "<remap> <save-buffer>" #'edit-indirect-commit)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # JSON

(require 'json-ts-mode)

(keymap-global-set "s-i m j" #'json-ts-mode) ; [m]ode: [j]son

;; <-------------------------
;; ## flymake

(add-hook 'json-ts-mode-hook 'flymake-mode)

(use-package json-ts-mode
  :flymake-hook
  (json-ts-mode flymake-collection-jq))
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # YAML

(require 'yaml-ts-mode)

(keymap-global-set "s-i m y" #'yaml-ts-mode) ; [m]ode: [y]aml

;; <-------------------------
;; ## flymake

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

;; https://github.com/zkry/yaml-pro#usage-1

(keymap-set yaml-pro-ts-mode-map "C-c l" #'yaml-pro-ts-unindent-subtree)  ; [l]eft move
(keymap-set yaml-pro-ts-mode-map "C-c r" #'yaml-pro-ts-indent-subtree)    ; [r]ight move
(keymap-set yaml-pro-ts-mode-map "C-c u" #'yaml-pro-ts-move-subtree-up)   ; [u]p move
(keymap-set yaml-pro-ts-mode-map "C-c d" #'yaml-pro-ts-move-subtree-down) ; [d]own move
(keymap-set yaml-pro-ts-mode-map "C-c k" #'yaml-pro-ts-kill-subtree)      ; [k]ill
(keymap-set yaml-pro-ts-mode-map "C-M-n" #'yaml-pro-ts-next-subtree)      ; [n]ext sibling; was forward-list
(keymap-set yaml-pro-ts-mode-map "C-M-p" #'yaml-pro-ts-prev-subtree)      ; [p]revious sibling; was backward-list
(keymap-set yaml-pro-ts-mode-map "C-M-u" #'yaml-pro-ts-up-level)          ; [u]p level; was backward-up-list
(keymap-set yaml-pro-ts-mode-map "C-c m" #'yaml-pro-ts-mark-subtree)      ; [m]ark
(keymap-set yaml-pro-ts-mode-map "C-c y" #'yaml-pro-ts-paste-subtree)     ; [y]ank

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
;; >-------------------------

;; <-------------------------
;; ## polymode

(define-hostmode poly-yaml-ts-hostmode
  :mode 'yaml-ts-mode)

(define-polymode poly-yaml-ts-mode
  :hostmode 'poly-yaml-ts-hostmode
  :innermodes '(poly-bash-innermode))

;; https://github.com/polymode/polymode/issues/324#issuecomment-1872441449
;; (add-hook 'yaml-ts-mode-hook 'poly-yaml-ts-mode)

(keymap-set yaml-ts-mode-map "C-c P" 'poly-yaml-ts-mode)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # markdown-mode

(require 'markdown-mode)

(setopt markdown-command "multimarkdown")
(setopt markdown-display-remote-images t)
(setopt markdown-fontify-code-blocks-natively t)
(setopt markdown-max-image-size '(500 . 500))

(keymap-set markdown-mode-map "C-c c" #'markdown-insert-code)                 ; inline [c]ode
(keymap-set markdown-mode-map "C-c b" #'markdown-insert-gfm-code-block)       ; code [b]lock
(keymap-set markdown-mode-map "C-c q" #'markdown-insert-blockquote)           ; [q]uote
(keymap-set markdown-mode-map "C-c e" #'markdown-insert-bold)                 ; [e]mphasize
(keymap-set markdown-mode-map "C-c l" #'markdown-insert-link)                 ; [l]ink
(keymap-set markdown-mode-map "C-c n" #'markdown-next-visible-heading)        ; [n]ext heading
(keymap-set markdown-mode-map "C-c p" #'markdown-previous-visible-heading)    ; [p]revious heading
(keymap-set markdown-mode-map "C-c h" #'markdown-insert-header-dwim)          ; insert [h]eader
(keymap-set markdown-mode-map "C-c i" #'markdown-insert-image)                ; insert [i]mage
(keymap-set markdown-mode-map "C-c I" #'markdown-toggle-inline-images)        ; show [I]mages
(keymap-set markdown-mode-map "C-M-n" #'markdown-outline-next-same-level)     ; [n]ext sibling; was forward-list
(keymap-set markdown-mode-map "C-M-p" #'markdown-outline-previous-same-level) ; [p]revious sibling; was backward-list
(keymap-set markdown-mode-map "C-M-u" #'markdown-up-heading)                  ; [u]p level; was backward-up-list

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

;; https://github.com/polymode/polymode/issues/324#issuecomment-1872441449
;; (add-hook 'groovy-mode-hook 'poly-groovy-mode)

(keymap-set groovy-mode-map "C-c P" 'poly-groovy-mode)
;; >-------------------------

;; >--------------------------------------------------



;; <--------------------------------------------------
;; # eglot-booster

;; (require 'eglot-booster)
;; (eglot-booster-mode)
;; >--------------------------------------------------



;; <--------------------------------------------------
;; # Custom faces

;; Foreground:
;; https://github.com/ianyepan/jetbrains-darcula-emacs-theme/blob/master/jetbrains-darcula-theme.el

(set-face-attribute 'default nil :family "JetBrains Mono" :height 130)
(set-face-attribute 'aw-leading-char-face nil :height 1.0 :weight 'bold :foreground "green")

(let ((added (face-attribute 'magit-diff-added :foreground))
      (removed (face-attribute 'magit-diff-removed :foreground)))
  (set-face-attribute 'magit-diff-added nil :foreground (doom-darken added 0.3))
  (set-face-attribute 'magit-diff-removed nil :foreground (doom-darken removed 0.3))
  (set-face-attribute 'diff-refine-added nil :foreground added)
  (set-face-attribute 'diff-refine-removed nil :foreground removed))

(set-face-attribute
 'magit-diff-hunk-heading-highlight nil :background
 (doom-darken (face-attribute 'magit-diff-base-highlight :background) 0.3))

(set-face-attribute
 'highlight nil :background
 (doom-darken (face-attribute 'highlight :background) 0.5))

(set-face-attribute 'consult-file nil :inherit 'consult-buffer)

(set-face-attribute 'avy-lead-face nil :foreground "green" :background "reset")

(let ((fg "#a9b7c6"))
  ;; (set-face-attribute 'default nil :family "JetBrains Mono" :height 130 :foreground fg)

  ;; (custom-set-faces
  ;;  '(eglot-highlight-symbol-face ((t (:underline "DarkGoldenrod"))))
  ;;  '(flyspell-duplicate ((t (:underline nil)))))

  ;; (set-face-foreground 'corfu-default fg)
  ;; (set-face-attribute 'corfu-current nil :foreground fg :background "#42444a")

  ;; (set-face-foreground 'font-lock-function-call-face fg)
  ;; (set-face-foreground 'font-lock-variable-name-face fg)
  ;; (set-face-foreground 'font-lock-type-face "#5B6268")

  ;; (set-face-attribute 'diff-refine-added nil :background "unspecified" :inverse-video nil)
  ;; (set-face-attribute 'diff-refine-removed nil :inverse-video nil)

  ;; (set-face-attribute 'symbol-overlay-face-1 nil :background "unspecified" :underline "DeepSkyBlue1" :distant-foreground "unspecified")
  ;; (set-face-attribute 'symbol-overlay-face-2 nil :background "unspecified" :underline "yellow1" :distant-foreground "unspecified")
  ;; (set-face-attribute 'symbol-overlay-face-3 nil :background "unspecified" :underline "OrangeRed1" :distant-foreground "unspecified")
  ;; (set-face-attribute 'symbol-overlay-face-4 nil :background "unspecified" :underline "green1" :distant-foreground "unspecified")
  ;; (set-face-attribute 'symbol-overlay-face-5 nil :background "unspecified" :underline "MediumPurple1" :distant-foreground "unspecified")
  ;; (set-face-attribute 'symbol-overlay-face-6 nil :background "unspecified" :underline "magenta1" :distant-foreground "unspecified")
  ;; (set-face-attribute 'symbol-overlay-face-7 nil :background "unspecified" :underline "turquoise1" :distant-foreground "unspecified")
  ;; (set-face-attribute 'symbol-overlay-face-8 nil :background "unspecified" :underline "orange1" :distant-foreground "unspecified")
  )
;; >--------------------------------------------------
