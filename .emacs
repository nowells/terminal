(defun system-emacs-load-dirs (dir)
  (add-to-list 'load-path dir)
  (let ((cwd default-directory))
    (cd dir)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))
    (cd cwd)))

(if (getenv "__REMOTE_TERMINAL_SCRIPT_DIR")
  (system-emacs-load-dirs (concat (getenv "__REMOTE_TERMINAL_SCRIPT_DIR") "/.emacs.d")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Libraries to autoload
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'ipython)
(require 'lorem-ipsum)
(require 'column-marker)
(require 'pager)
(require 'php-mode)

;; http://www.cua.dk/ido.html
(require 'ido)
(ido-mode t)

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(global-font-lock-mode t nil (font-lock))
 '(gud-gdb-command-name "gdb --annotate=1")
 '(inhibit-splash-screen t)
 '(large-file-warning-threshold nil)
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t)
 '(tab-always-indent nil)
 '(tool-bar-mode nil)
 '(transient-mark-mode t))
 ;; highlight the marked region
'(next-line-add-newlines nil)
'(message-log-max 512)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;;;LOOK AND FEEL
(setq default-frame-alist
                '((top . 1) (left . 394)
                  (width . 80) (height . 40)
                  (cursor-color . "yellow")
                  (cursor-type . box)
                  (foreground-color . "white")
                  (background-color . "black")))

;; turn on font-lock mode
(global-font-lock-mode t)

;; enable visual feedback on selections
(setq transient-mark-mode t)
;; turn off tab char
(setq-default indent-tabs-mode nil)

;; ===== Set standard indent to 4 ====
(setq default-tab-width 4)
(setq standard-indent 4)

;; delete \b at line ends before saving a file (delete trailing whitespace
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; show matching parens
(require 'paren) (show-paren-mode t)

(setq blink-matching-paren t)
(setq blink-matching-delay .5)

;; (global-set-key "\C-x\C-k" 'kill-region)
;; (global-set-key "\C-c\C-k" 'kill-region)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; enable mouse scrolling
(mouse-wheel-mode t)

(setq c-basic-offset 4)
(autoload 'javascript-mode "javascript-mode" "JavaScript mode" t)
(setq auto-mode-alist (append '(("\\.js$" . javascript-mode))
                 auto-mode-alist))

;(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
;(autoload 'css-mode "css-mode" nil t)

;(add-to-list 'auto-mode-alist '("\\.html\\'" . django-mode))
;(autoload 'django-mode "django-mode" nil t)

(autoload 'python-mode "python-mode" "Python Mode." t)
 (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
 (add-to-list 'interpreter-mode-alist '("python" . python-mode))

(add-hook 'python-mode-hook
           (lambda ()
             (set (make-variable-buffer-local 'beginning-of-defun-function)
                  'py-beginning-of-def-or-class)
             (setq outline-regexp "def\\|class ")))

;; Makes all tabs into spaces
(require 'cc-mode)
(defun my-build-tab-stop-list (width)
  (let ((num-tab-stops (/ 80 width))
        (counter 1)
        (ls nil))
    (while (<= counter num-tab-stops)
      (setq ls (cons (* width counter) ls))
      (setq counter (1+ counter)))
    (set (make-local-variable 'tab-stop-list) (nreverse ls))))
(defun my-c-mode-common-hook ()
  (setq tab-width 4) ;; change this to taste
  (my-build-tab-stop-list tab-width)
  (setq c-basic-offset tab-width)
  (setq indent-tabs-mode nil)) ;; force only spaces for indentation
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)
