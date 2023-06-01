;; Performance tweaking for modern machines
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Hide UI
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Better default modes
(electric-pair-mode t)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)

;;(tab-bar-mode)

(setq display-line-numbers-type 'relative) 
(global-display-line-numbers-mode)

(setq-default
 inhibit-startup-screen t               ; Disable start-up screen
 inhibit-startup-message t              ; Disable startup message
 inhibit-startup-echo-area-message t    ; Disable initial echo message
 initial-scratch-message ""             ; Empty the initial *scratch* buffer
 initial-buffer-choice t)               ; Open *scratch* buffer at init

(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

(add-to-list 'default-frame-alist '(font . "Comic Mono 22"))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))



;; Always download packages if they are missing
(setq package-enable-at-startup nil)

(straight-use-package 'use-package)

(use-package evil
  :straight t
  :config
  (evil-mode 1))

;; Code completion at point
(use-package company
  :straight t
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0))

;; Better minibuffer completion
(use-package vertico
  :straight t
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode))

;; Save minibuffer results
(use-package savehist
  :straight t
  :init
  (savehist-mode))

;; Show lots of useful stuff in the minibuffer
(use-package marginalia
  :after vertico
  :straight t
  :init
  (marginalia-mode))

(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-palenight") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package olivetti
  :straight t
  :custom
  (olivetti-body-width 130))

(use-package rust-mode
  :straight t
  :config
  (setq rust-format-on-save t))

(use-package racer
  :straight t
  :after rust-mode
  :diminish racer-mode
  :hook (rust-mode . racer-mode)
  :bind
  ("M-j" . racer-find-definition)
  ;; (:map racer-mode-map ("M-." . #'xref-find-definitions))
  (:map racer-mode-map ("M-." . nil)))

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)))


(use-package treemacs
  :straight t
  :defer t)

(use-package all-the-icons
  :straight t)

(use-package treemacs-evil
  :straight t
  :after (treemacs evil))

;; TODO: install projectile
;;(use-package treemacs-projectile
  ;;:after (treemacs projectile)
  ;;:ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :straight t)

(use-package treemacs-magit
  :after (treemacs magit)
  :straight t)

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :straight t
  :config (treemacs-set-scope-type 'Tabs))

(use-package doom-modeline
  :straight t
  :init
  (doom-modeline-mode 1))

(use-package markdown-mode
  :straight t)

(use-package rainbow-delimiters
  :straight t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
