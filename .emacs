;; .emacs

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; enable visual feedback on selections
;  (setq transient-mark-mode t)

;; default to better frame titles
;  (setq frame-title-format
;      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
;  (setq diff-switches "-u")

;; always end a file with a newline
;  (setq require-final-newline 'query)

;;; uncomment for CJK utf-8 support for non-Asian users
;; (require 'un-define)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.


(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
      (add-hook hook (lambda () (flyspell-mode -1))))

;; easy spell check
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
(global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)


(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))


(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
)

(package-initialize)

(require 'cl)


(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)



;;------ Aspecto Inicial -----------

;(setenv "HOME" "d:\\NOTAS")

(set-background-color "black")
(set-foreground-color "white")
(set-cursor-color "yellow")

(tool-bar-mode -1) ;;quita las barra de herramienta
(menu-bar-mode -1) ;;quita la barra del menu
(scroll-bar-mode -1) ;;quita la barra de scroll

(setq inhibit-startup-message t) ;;quita el logo inicial

;; -------- Rueda Raton ---------

(defcustom mouse-wheel-distance 1
  "*Number of lines, maximum, to scroll the window when you move the mouse = wheel."
  :type `integer
  :group `mouse
  )

(defun mouse-wheelscroll-down ()
  " A function to scroll up or down in response to the mouse wheel."
  (interactive)
  (scroll-down
   (min mouse-wheel-distance
        (max 1 (- (window-height)
                  next-screen-context-lines)))
   )
  )

(defun mouse-wheelscroll-up ()
  " A function to scroll up or down in response to the mouse wheel."
  (interactive)
  (scroll-up
   (min mouse-wheel-distance
        (max 1 (- (window-height)
                  next-screen-context-lines)))
   )
  )

(global-set-key [mouse-4] (function mouse-wheelscroll-down))
(global-set-key [mouse-5] (function mouse-wheelscroll-up))




;; -------- Combinacions de teclas -----------
;(global-set-key "\C-l" 'goto-line)
(global-set-key "\C-g" 'goto-line)
;;(global-set-key "\M-i" 'indented-text-mode)
(global-set-key "\C-c\c" 'compile)
(global-set-key "\C-z" 'undo)
(global-set-key "\C-c" 'comment-region)
(global-set-key "\C-u" 'uncomment-region)
(global-set-key [f1]  'help)

;;------------------------ Programacion  --------------------------

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq-default c-default-style 
              "linux" c-basic-offset 4)

(setq c-style-variables-are-local-p 'nil)

(autoload 'c++-mode "cc-mode" "Major mode for editing C++." t)
(autoload 'c-mode "cc-mode" "Major mode for editing C." t)
(require 'cc-mode)

(load "cc-mode")
(add-hook 'c-mode 'auto-fill-mode)
(add-hook 'c++-mode 'auto-fill-mode)

;; define new syntax format style
(c-add-style "codemat"
             (list "user"
                   '(c-basic-offset . 4)
                   '(c-comment-only-line-offset . 0)
                   '(c-hanging-braces-alist
                     (substatement-open before after))
                   '(indent-tabs-mode . nil)
                   '(c-offsets-alist
                     (topmost-intro . 0)
                     (topmost-intro-cont . 0)
                     (substatement . +)
                     (substatement-open . 0)
                     (case-label . +)
                     (access-label . -2)
                     (inclass . +)
                     (inline-open . 0)
                     )
             )
)

;; C++
(defun my-cpp-mode-hook()
  (setq c-basic-offset 4)
)

(add-hook 'c++-mode-hook 'my-cpp-mode-hook)
(add-hook 'c++-mode-hook
                  '(lambda()
                         (local-set-key "\C-c\C-f" 'comment-function-c++ )
                         (local-set-key  [(meta p)] 'insert-print-statement-cpp )
                         (local-set-key  [(meta control p)] 'cout-helper-2 )
                         (local-set-key  [(meta f)] 'insert-for-statement )
                         (local-set-key  [(meta j)] 'insert-for-j-statement )
                         (turn-on-font-lock)
                         ()
                         ))

;; Latex


;; LaTeX editing prefs
(add-hook 'tex-mode 'latex-file-handler)
(defun latex-file-handler ()
  (nb-ide-settings)
  (turn-on-font-lock)
  (turn-on-auto-fill)
  (set-fill-column 80)
  (setq tex-dvi-view-command "xdvi"))

(add-hook 'tex-mode 'autoindent)


;; ----------- Reglas de coloreado -----------
(setq auto-mode-alist
      (append
       '(("\\.C$"    . c++-mode)
         ("\\.H$"    . c++-mode)
         ("\\.cc$"   . c++-mode)
         ("\\.hh$"   . c++-mode)
         ("\\.hpp$"  . c++-mode)
         ("\\.cpp$"  . c++-mode)
         ("\\.cg$"   . c++-mode)
         ("\\.h$"    . c++-mode)
         ("\\.c$"    . c-mode)
         ("\\.m$"    . objc-mode)
         ("\\.java$" . java-mode)
                 ("\\.tex$"  . tex-mode)
         ("\\.emacs$" . emacs-lisp-mode))
       auto-mode-alist))



;; ----- Miscelaneo ----------

;; Coloreado Sintaxis
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Colorea la region marcada
(transient-mark-mode t)

;; Ilumina el cierre de paréntesis
(show-paren-mode 1)

;; Resize fond
;;(set-face-attribute 
 ;;    "-outline-Courier New-normal-normal-normal-mono-11-*-*-*-c-*-iso8859-1")

;; AUTOINDENTACIÓN EN C
;(c-set-offset 'case-label '+)
;; AÑADO ENTRADA A LA LISTA DE LOAD-PATHS
;(setq loadn-path (cons "~/emacs/" load-path))

;; Visualiza hora
(setq display-time-24hr-format t) ; In 24 hour format
(display-time)    ; Display the time

;; Set titles for frame and icon (%f == file name, %b == buffer name)
(setq-default frame-title-format (list "Emacs: %b"))
(setq-default icon-title-format "Emacs - %b")

;; Deshabilita el pitido por el altavoz => graficamente
(setq visible-bell 1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-time-mode t)
 '(package-selected-packages (quote (vue-mode vue-html-mode markdown poker ##)))
 '(show-paren-mode t)
 '(transient-mark-mode t))


;; ------ DIRED SORTING

(setq dired-listing-switches "-aBhl --group-directories-first")

(global-set-key (kbd "C-x C-b") 'ibuffer)


(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))




(setq vue-mode-packages
  '(vue-mode))
(setq vue-mode-excluded-packages '())
(defun vue-mode/init-vue-mode ()
  "Initialize my package"
  (use-package vue-mode))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(defalias 'list-buffers 'ibuffer) ; make ibuffer de

