;; Make sure to install rust-analyzer binary, otherwise lsp-mode
;; default to using rls instead of rust-analyzer.

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; (toggle-debug-on-error)

;; (set-frame-parameter nil 'undecorated t)
;; (setq frame-title-format "")

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(set-face-attribute 'region nil :background "#fffacd")

;; (setq default-frame-alist
;;       '(
;;         (width . 140) ; chars
;;         (height . 60) ; lines
;;         (left . 9000)
;;         (top . 0)))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (split-window-horizontally)
;; (split-window-horizontally)
;; (balance-windows)
;; (other-window 1)
;; (other-window 1)
;; (delete-window)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package zenburn-theme
	     :ensure t)
(use-package bind-key
	     :ensure t)

(use-package default-text-scale
    :ensure t
    :config
    (setq default-text-scale-amount 10)
    :bind
    ;; Plus makes it better
    ("M-+" . default-text-scale-increase)
    ;; Underscore makes it smaller (- is already bound)
    ("M-_" . default-text-scale-decrease))

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package highlight-symbol
  :hook (prog-mode . highlight-symbol-mode)
  :bind
  ("M-n" . highlight-symbol-next)
  ("M-p" . highlight-symbol-prev)
  :config
  (setq highlight-symbol-nav-mode t)
  (setq highlight-symbol-mode t)
  (setq highlight-symbol-idle-delay 0.3))

(use-package ag)

(use-package helm-ag
  :after ag)

(use-package helm-projectile
  :after helm
  :config
  (helm-projectile-on))

(use-package diminish)

(use-package helm
  :diminish helm-mode
  :init
  (require 'helm-config)
  :bind
  ("C-c f" . helm-projectile-find-file-dwim)
  ("M-x" . helm-M-x)
  ("C-x r b" . helm-filtered-bookmarks)
  ("C-x C-f" . helm-find-files)
  :init
  (helm-mode 1)
  (customize-set-variable 'helm-ff-lynx-style-map t))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :demand t
  :bind
  ("M-<down>" . next-error)
  ("M-<up>" . previous-error)
  ;; :init
  ;; (setq flycheck-standard-error-navigation nil)
  )

(use-package company
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :config (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  (setq company-async-timeout 10))

;; `company' backend for `lsp-mode'
(use-package company-lsp
  :ensure t
  :after company lsp-mode
  :init
  (push 'company-lsp company-backends))

(use-package lsp-mode
  :commands lsp
  :diminish lsp-mode
  :bind
  ("M-." . 'lsp-find-definition)
  ("M-t" . 'lsp-find-type-definition)
  ("M-?" . 'lsp-find-references)
  ;; ("C-c t" . rust-test)
  ;; ("C-c b" . cargo-process-build)
  :config
  (require 'lsp-clients)
  (setq lsp-prefer-flymake nil))

(use-package toml-mode)

(use-package eldoc
  :diminish eldoc-mode)

(use-package rust-mode
  :hook (rust-mode . lsp)
  :bind
  ("C-c g" . rust-run)
  ("C-c t" . rust-test)
  ("C-c b" . cargo-process-build)
  :init
  (which-function-mode 1)
  (setq compilation-error-regexp-alist-alist
      (cons '(cargo "^\\([^ \n]+\\):\\([0-9]+\\):\\([0-9]+\\): \\([0-9]+\\):\\([0-9]+\\) \\(?:[Ee]rror\\|\\([Ww]arning\\)\\):" 1 (2 . 4) (3 . 5) (6))
        compilation-error-regexp-alist-alist))
  :config
  (setq rust-format-on-save t))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :diminish cargo-minor-mode)

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
  :after rust-mode
  :diminish racer-mode
  :hook (rust-mode . racer-mode)
  :bind
  ("M-j" . racer-find-definition)
  ;; (:map racer-mode-map ("M-." . #'xref-find-definitions))
  (:map racer-mode-map ("M-." . nil))
  )

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))


(use-package yasnippet
  :demand t
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets")))



;; Issue #6887: Rather than inheriting the 'gnu compilation error
;; regexp (which is broken on a few edge cases), add our own 'rust
;; compilation error regexp and use it instead.
(defvar rustc-compilation-regexps
  (let ((file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)")
        (end-line   "\\([0-9]+\\)")
        (end-col    "\\([0-9]+\\)")
        (msg-type   "\\(?:[Ee]rror\\|\\([Ww]arning\\)\\|\\([Nn]ote\\|[Hh]elp\\)\\)"))
    (let ((re (concat "^" file ":" start-line ":" start-col
                      ": " end-line ":" end-col
                      " " msg-type ":")))
      (cons re '(1 (2 . 4) (3 . 5) (6 . 7)))))
  "Specifications for matching errors in rustc invocations.
    See `compilation-error-regexp-alist' for help on their format.")

(defvar rustc-new-compilation-regexps
  (let ((file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat "^ *--> " file ":" start-line ":" start-col ; --> 1:2:3
                      )))
      (cons re '(1 2 3))))
  "Specifications for matching errors in rustc invocations (new style).
    See `compilation-error-regexp-alist' for help on their format.")

;; Match test run failures and panics during compilation as
;; compilation warnings
(defvar cargo-compilation-regexps
  '("^\\s-+thread '[^']+' panicked at \\('[^']+', \\([^:]+\\):\\([0-9]+\\)\\)" 2 3 nil nil 1)
  "Specifications for matching panics in cargo test invocations.
    See `compilation-error-regexp-alist' for help on their format.")

(defun rustc-scroll-down-after-next-error ()
  "In the new style error messages, the regular expression
       matches on the file name (which appears after `-->`), but the
       start of the error appears a few lines earlier. This hook runs
       after `M-x next-error`; it simply scrolls down a few lines in
       the compilation window until the top of the error is visible."
  (save-selected-window
    (when (eq major-mode 'rust-mode)
      (select-window (get-buffer-window next-error-last-buffer 'visible))
      (when (save-excursion
              (beginning-of-line)
              (looking-at " *-->"))
        (let ((start-of-error
               (save-excursion
                 (beginning-of-line)
                 (while (not (looking-at "^[a-z]+:\\|^[a-z]+\\[E[0-9]+\\]:"))
                   (forward-line -1))
                 (point))))
          (set-window-start (selected-window) start-of-error))))))

(eval-after-load 'compile
  '(progn
     (add-to-list 'compilation-error-regexp-alist-alist
                  (cons 'rustc-new rustc-new-compilation-regexps))
     (add-to-list 'compilation-error-regexp-alist 'rustc-new)
     (add-hook 'next-error-hook 'rustc-scroll-down-after-next-error)
     (add-to-list 'compilation-error-regexp-alist-alist
                  (cons 'rustc rustc-compilation-regexps))
     (add-to-list 'compilation-error-regexp-alist 'rustc)
     (add-to-list 'compilation-error-regexp-alist-alist
                  (cons 'cargo cargo-compilation-regexps))
     (add-to-list 'compilation-error-regexp-alist 'cargo)))

;; (use-package company-jedi
;;   :ensure t
;;   :config
;;   :hook
;;   ((python-mode . jedi:setup))
;;   :init
;;   (setq jedi:complete-on-dot t)
;;   (setq jedi:use-shortcuts t)
;;   (add-hook 'python-mode-hook
;;             (lambda () (add-to-list 'company-backends 'company-jedi))))

;; (use-package ein
;;   :ensure t
;;   :config
;;   (setq ein:use-auto-complete t)
;;   :bind ("S-<return>" . ein:worksheet-execute-cell-and-goto-next)
;;   :commands (ein:notebooklist-open))

;; (use-package elpy
;;   :ensure t
;;   :init
;;   (with-eval-after-load 'python
;;     (elpy-enable))
;;   :bind
;;   ("C-c r" . python-shell-send-region)
;;   ("C-c c" . python-shell-send-buffer)
;;   :config
;;   (unbind-key "M-<right>" elpy-mode-map)
;;   (unbind-key "M-<left>" elpy-mode-map)
;;   (unbind-key "M-<up>" elpy-mode-map)
;;   (unbind-key "M-<down>" elpy-mode-map)
;;   ;; (setq python-shell-interpreter "ipython"
;;   ;; 	python-shell-interpreter-args "-i --simple-prompt console")
;;   )

;; other setups
;; (which-function-mode 1)
(global-set-key (kbd "C--") #'undo)
(global-set-key (kbd "M-g") #'goto-line)

(setq inhibit-startup-screen t)
(column-number-mode 1)

(defun copy-file-path ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))


;; (org-babel-do-load-languages
;;  'org-babel-load-languages '((python . t)))

;;(setq compilation-search-path '("/home/helin/env/rust/"))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(company-idle-delay 0.3)
 '(company-minimum-prefix-length 1)
 '(company-tooltip-align-annotations t)
 '(compilation-skip-threshold 2)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("cdb4ffdecc682978da78700a461cdc77456c3a6df1c1803ae2dd55c59fa703e3" default)))
 '(eww-search-prefix "http://www.google.com/search?q=")
 '(helm-ff-lynx-style-map t)
 '(lsp-rust-analyzer-server-display-inlay-hints t)
 '(lsp-rust-server (quote rust-analyzer))
 '(package-selected-packages
   (quote
    (dictionary zenburn-theme yasnippet use-package toml-mode racer magit highlight-symbol helm-projectile helm-ag flycheck-rust diminish company-lsp cargo ag ace-window)))
 '(server-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
