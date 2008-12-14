;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)
(custom-set-variables
 '(case-fold-search t) ;; case-insensitive search
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(global-font-lock-mode    t nil (font-lock)) ;; Syntax higlighting
 '(global-font-lock-mode t nil (font-lock))
 '(inhibit-splash-screen t)
 '(inhibit-startup-message  t)          ;; no startup message
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t) ;; match parens
 '(tab-always-indent nil)
 '(tool-bar-mode nil) ;; highlight matches in query-replace
 '(transient-mark-mode t)) ;; highlight the marked region
'(next-line-add-newlines nil)
'(message-log-max 512)
(custom-set-faces
 )

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

(setq load-path (cons "~/.emacs.d/extra" load-path))

(setq c-basic-offset 4)
(autoload 'javascript-mode "javascript-mode" "JavaScript mode" t)
(setq auto-mode-alist (append '(("\\.js$" . javascript-mode))
                 auto-mode-alist))

;(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
;(autoload 'css-mode "css-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.html\\'" . django-mode))
(autoload 'django-mode "django-mode" nil t)

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
