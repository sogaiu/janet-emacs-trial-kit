;;; init.el --- emacs configuration

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; straight.el - basic infrastructure for elisp packages and dependencies

;; via:
;;   https://github.com/radian-software/straight.el#getting-started

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
			 user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "https://raw.githubusercontent.com/"
		 "radian-software/straight.el/develop/install.el")
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; the linter complains about this file, but see:
;;
;;   https://lists.gnu.org/archive/html/help-gnu-emacs/2020-10/msg00003.html

;; via:
;;   https://github.com/raxod502/straight.el#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; visual perception aids

;;; monokai
(straight-use-package
 '(monokai-theme :host github
                 :repo "oneKelvinSmith/monokai-emacs"
                 :files ("*.el")))

(load-theme 'monokai t)

;;; rainbow-delimiters
(straight-use-package
 '(rainbow-delimiters :host github
                      :repo "Fanael/rainbow-delimiters"
                      :files ("*.el")))

(use-package rainbow-delimiters
  :straight t
  :config
  (custom-set-faces
   ;; XXX: doesn't seem too helpful for the unclosed case?
   ;;'(rainbow-delimiters-unmatched-face ((t (:foreground "purple"))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))
  (add-hook 'janet-ts-mode-hook 'rainbow-delimiters-mode))

;;; xterm-color
(straight-use-package
 '(xterm-color :host github
               :repo "atomontage/xterm-color"
               :files ("*.el")))

;; XXX: although M-x shell works better, atm don't know how
;;      to get TERM set automatically when M-x shell starts.
;;      doing it manually with `TERM=xterm-256color' (no
;;      `export' in front) does work.
;;
;; XXX: ajrepl-mode was made to work, but atm that's via
;;      changes to ajrepl.el instead of via a hook.
(use-package xterm-color
  :straight t)

;; XXX: is this really a good way to do things?
(require 'xterm-color)

;; for more readable color in shell-mode
(add-hook 'shell-mode-hook
          (lambda ()
            ;; XXX: tried putting the following forms after :init
            ;;      in the `use-package' form above, but that
            ;;      gave worse results
            ;;
            ;; XXX: this did not work
            ;;(setq comint-terminfo-terminal "xterm-256color")
            ;; XXX: this did not work
            ;;(setenv "TERM" "xterm-256color")
            ;; XXX: atm am doing TERM=xterm-256color manually after
            ;;      starting M-x shell -- seems silly
            ;;
            ;; Disable font-locking in this buffer to improve performance
            (font-lock-mode -1)
            ;; Prevent font-locking from being re-enabled in this buffer
            (make-local-variable 'font-lock-function)
            (setq font-lock-function (lambda (_) nil))
            (add-hook 'comint-preoutput-filter-functions
                      'xterm-color-filter nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; janet-specific (mostly) things

;;; janet-ts-mode
(straight-use-package
 '(janet-ts-mode :host github
                 :repo "sogaiu/janet-ts-mode"
                 :files ("*.el")))

;; https://stackoverflow.com/a/28008006
(defun my-janet-ts-mode-faces ()
  "Customization of janet-ts-mode faces."
  (face-remap-add-relative
    ;; special forms in light blue
    'font-lock-keyword-face 'font-lock-type-face))

(use-package janet-ts-mode
  :straight t
  :config
  (add-hook 'janet-ts-mode-hook
            'my-janet-ts-mode-faces)
  (add-hook 'janet-ts-mode-hook
            'rainbow-delimiters-mode)
  ;; XXX: extra items
  '(require 'janet-ts-experiment))

;;; ajrepl
(straight-use-package
 '(ajrepl :host github
          :repo "sogaiu/ajrepl"
          :files ("*.el" "ajrepl")))

(use-package ajrepl
  :straight t
  :config
  (add-hook 'janet-ts-mode-hook
            #'ajrepl-interaction-mode)
  ;; making the colors in ajrepl-mode (comint-mode) easier to read
  ;; requires 'xterm-color, which is enabled earlier in this file
  (add-hook 'ajrepl-mode-hook
            (lambda ()
              ;;(require 'xterm-color)
              (setq comint-output-filter-functions
                    (remove 'ansi-color-process-output
                            comint-output-filter-functions))
              (setq comint-preoutput-filter-functions
                    ;; XXX: supposedly better for xterm-color-filter
                    ;;      to be at beginning, but may be this works
                    ;;      often enough?
                    (add-to-list 'comint-preoutput-filter-functions
                                 'xterm-color-filter)))))

;;; a-janet-spork-client
'(straight-use-package
 '(ajsc :host github
        :repo "sogaiu/a-janet-spork-client"
        :files ("*.el")))

'(use-package ajsc
  :straight t
  :config
  (add-hook 'janet-ts-mode-hook
            #'ajsc-interaction-mode))

;;; flycheck
(straight-use-package
 '(flycheck :host github
            :repo "flycheck/flycheck"
            :files ("*.el")))

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode)
  ;; https://github.com/flycheck/flycheck/issues/1559#issuecomment-478569550
  (setq flycheck-emacs-lisp-load-path 'inherit))

;;; flycheck-color-mode-line
(straight-use-package
 '(flycheck-color-mode-line :host github
                            :repo "flycheck/flycheck-color-mode-line"
                            :files ("*.el")))

(use-package flycheck-color-mode-line
  :straight t
  :config
  (eval-after-load "flycheck"
    '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

;;; flycheck-janet
(straight-use-package
 '(flycheck-janet :host github
               :repo "sogaiu/flycheck-janet"
               :files ("*.el")))

(use-package flycheck-janet
  :straight t)

;;; XXX: need to install review-janet for this to work
;;; flycheck-rjan
'(straight-use-package
 '(flycheck-rjan :host github
                 :repo "sogaiu/flycheck-rjan"
                 :files ("*.el")))

'(use-package flycheck-rjan
  :straight t
  :config
  (flycheck-add-next-checker 'janet-rjan 'janet-janet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; basic emacs settings

(setq resize-mini-windows nil)

(setq-default indent-tabs-mode nil)

(setq delete-trailing-lines nil)

;; via:
;;   https://emacsredux.com/blog/2013/04/07/ \
;;           display-visited-files-path-in-the-frame-title/
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(column-number-mode 1)

(global-display-line-numbers-mode 1)

;; from: https://www.emacswiki.org/emacs/BasicNarrowing
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(show-paren-mode t)

;; don't suddenly scroll multiple lines near the top or bottom of the window
(setq scroll-conservatively 10000)

(setq inhibit-startup-screen t)

(tool-bar-mode -1)

;; prevent using ui dialogs for prompts
(setq use-dialog-box nil)

;; automatically revert buffers for changed files
(global-auto-revert-mode 1)

;; better mouse wheel scrolling
;; https://stackoverflow.com/a/26053341
(setq mouse-wheel-scroll-amount '(0.07))
;; https://stackoverflow.com/a/445881
;; (setq mouse-wheel-scroll-amount
;;       '(1 ((shift) . 1)
;;           ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; show point in the mode line
;;
;; thanks to dannyfreeman
(add-to-list 'mode-line-position
             '(:eval (format "[%s|%s] " (point) (position-bytes (point)))))

;; XXX: trying this out for a while
(electric-pair-mode t)

;; to avoid indenting the line one was just on when pressing RET, we
;; do the following.  this works when RET is bound to newline, which
;; checks the value of electric-indent-mode
(setq electric-indent-mode nil)

;; use C-c <LEFT> to invoke winner-undo
;; this switches back to an earlier window configuration which can be
;; used to banish windows that have opened where you don't want them
;; e.g. the *Help* buffer
(require 'winner)
;; enable
(winner-mode 1)

;; dragging file name from mode line to other programs does something
(setq mouse-drag-mode-line-buffer t)

(setq mouse-drag-and-drop-region-cross-program t)

;; scrolling display up or down at pixel resolution
(pixel-scroll-precision-mode)

;; 'overlay and 'child-frame are other alternatives
(setq show-paren-context-when-offscreen t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; satisfying emacs lisp linter

(provide 'init)
;;; init.el ends here
