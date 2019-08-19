(setq custom-file (substitute-in-file-name "$HOME/.emacs.d/custom.el"))
(load custom-file)

(setq-default frame-title-format '("%b [%m]"))

;; turn off menu, scroll, tool bars
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (menu-bar-mode -1)))

;; undo/redo window configuration
(winner-mode 1)

;; turn off sounds
(setq ring-bell-function 'ignore)

;; make unix lineendings default
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq column-number-mode t)

;; saves the minibuffer history on every Emacs session.
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(savehist-mode 1)

;; turn off lock files
(setq create-lockfiles nil)
;; placing all files in one directory
(setq backup-directory-alist
      `((".*" ., temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*", temporary-file-directory t)))
;; automatically purge backup files not accessed in a week
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (nth 5 (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

;; set encoding for windows system shell
(when (eq system-type 'windows-nt)
  (progn
    (defun my-windows-system-shell-advice (arg)
      (set-buffer-process-coding-system 'cp1251 'cp1251))
    (advice-add 'shell :after 'my-windows-system-shell-advice)))


;; list the packages you want
(setq package-list
      '(company
        company-web
        counsel
        counsel-projectile
        dashboard
        emmet-mode
        evil
        evil-collection
        evil-leader
        git-gutter
        ivy-yasnippet
        js2-mode
        json-mode
        key-chord
        magit
        projectile
        rainbow-mode
        smartparens
        solarized-theme
        tide
        yasnippet
        yasnippet-snippets
        web-mode
        which-key))

;; list the repositories containing them
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; indicate line length
(setq whitespace-line-column 120)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

(setq-default indent-tabs-mode nil)


(require 'solarized-light-theme)

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

(global-set-key (kbd "C-d") 'duplicate-line)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)


;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; to make ediff to be horizontally split
(setq ediff-split-window-function 'split-window-horizontally)


;; evil
(setq evil-want-C-u-scroll t)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(require 'evil)
(global-evil-leader-mode)
(evil-mode 1)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key "fs" 'save-buffer)

(evil-collection-init '(company dired ibuffer tide))

;; change mode-line color by evil state
(require 'cl)
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
(add-hook 'post-command-hook
  (lambda ()
    (let ((color (cond ((minibufferp) default-color)
                       ((evil-insert-state-p) '("#a6a6a6" . "#ffffff")) ; grey
                       ((evil-emacs-state-p)  '("#444488" . "#ffffff")) ; purple
                       ((buffer-modified-p)   '("#006fa0" . "#ffffff")) ; blue
                       (t default-color))))
      (set-face-background 'mode-line (car color))
      (set-face-foreground 'mode-line (cdr color))))))


;; flychek
(require 'flycheck)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'my-use-flycheck-linters-from-node-modules)
  (setcar
    (memq 'source-inplace (flycheck-checker-get 'typescript-tslint 'command))
    'source-original))

(setq my-flycheck-executable-linters
      '((eslint flycheck-javascript-eslint-executable)
        (stylelint flycheck-css-stylelint-executable)
        (stylelint flycheck-less-stylelint-executable)
        (lessc flycheck-less-executable)
        (tslint flycheck-typescript-tslint-executable)))

(defun my-use-flycheck-linters-from-node-modules ()
  (let ((root (locate-dominating-file (or (buffer-file-name) default-directory)
                                      "node_modules"))
        (bin-ext (if (eq system-type 'windows-nt) ".cmd" "")))
    (dolist (linter my-flycheck-executable-linters)
      (let* ((linter-name (car linter))
             (linter-exe (car (cdr linter)))
             (linter-location (and root (expand-file-name (format "node_modules/.bin/%s%s"
                                                                  linter-name
                                                                  bin-ext)
                                                          root))))
        (when (and linter-location (file-exists-p linter-location))
          (customize-set-variable linter-exe linter-location))))))


;; web-mode
(defun my-web-mode-hook ()
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (rainbow-mode +1)
  (set-face-background 'web-mode-current-element-highlight-face "#e9e1c9")
  (set-face-background 'web-mode-current-column-highlight-face "#e9e1c9"))
(with-eval-after-load 'web-mode
  (add-hook 'web-mode-hook 'my-web-mode-hook))


;; tide
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (yas-activate-extra-mode 'typescript-mode)
  (setq-local company-backends '((company-tide)))
  (company-mode +1)
  (add-hook 'hack-local-variables-hook
            (lambda ()
              (when (or (not (boundp 'tide-format-before-save-enabled)) tide-format-before-save-enabled))
                (add-hook 'before-save-hook 'tide-format-before-save))))

(add-hook 'typescript-mode-hook #'setup-tide-mode)


;; tsx
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode)
              (flycheck-add-mode 'typescript-tslint 'web-mode))))


;; js
(require 'js2-mode)
(require 'tide)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (setq-local tide-filter-out-warning-completions t)
            (setup-tide-mode)
            (flycheck-add-mode 'javascript-eslint 'web-mode)
            (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)))

;; jsx
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode)
              (setq-local tide-filter-out-warning-completions t)
              (flycheck-add-mode 'javascript-eslint 'web-mode)
              (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append))))

;; html
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-match "html?$" (file-name-extension buffer-file-name))
              (setq-local company-backends '(company-web-html))
              (company-mode +1)
              (flycheck-add-mode 'html-tidy 'web-mode)
              (flycheck-mode +1))))

;; css
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-hook 'css-mode-hook
          (lambda ()
            (when (string-equal "css" (file-name-extension buffer-file-name))
              (setq-local company-backends '(company-css))
              (setq css-indent-offset 2)
              (company-mode +1)
              (flycheck-add-mode 'css-stylelint 'css-mode)
              (flycheck-mode +1))))

;; less
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))
(add-hook 'less-css-mode-hook
          (lambda ()
            (when (string-equal "less" (file-name-extension buffer-file-name))
              (setq-local company-backends '(company-css))
              (setq css-indent-offset 2)
              (company-mode +1)
              (flycheck-add-next-checker 'less 'less-stylelint 'append)
              (flycheck-mode +1))))


;; git
(global-git-gutter-mode +1)

;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)

;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

;; live updateing
(custom-set-variables
 '(git-gutter:update-interval 2))

;; don't ask about commit/revert
(custom-set-variables
 '(git-gutter:ask-p nil))

;; magit
(global-set-key (kbd "C-c g") 'magit-status)


;; ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-g") 'counsel-git)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> b") 'counsel-descbinds)


;; projectile
(add-hook 'web-mode-hook 'counsel-projectile-mode)
(setq projectile-completion-system 'ivy)
(setq projectile-indexing-method 'alien)


;; smartparens
(require 'smartparens-config)
(show-smartparens-global-mode t)
;; Always start smartparens mode in js-mode.
(add-hook 'prog-mode-hook #'smartparens-mode)

(with-eval-after-load 'smartparens
  (sp-with-modes
      '(typescript-mode web-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
    (sp-local-pair "(" nil :post-handlers '(:add ("||\n[i]" "RET")))))

;; yasnippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)


;; ivy-yasnippet
(with-eval-after-load 'yasnippet
  (evil-define-key 'insert yas-minor-mode-map (kbd "C-c y") #'ivy-yasnippet))


;; emmet-mode
(add-hook 'web-mode-hook 'emmet-mode)
(with-eval-after-load 'emmet-mode
  (add-hook 'emmet-mode-hook #'setup-emmet-mode))

(defun setup-emmet-mode ()
  (setq emmet-expand-jsx-className? t)
  (setq emmet-move-cursor-between-quotes t)
  (evil-define-key 'insert web-mode-map (kbd "C-y") 'emmet-expand-line)
  (defadvice emmet-expand-line (after evil-normal-state activate)
    "Enable Normal state after expansion"
    (evil-normal-state)))


;; dashboard
(require 'dashboard)
(with-eval-after-load 'dashboard
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5))))


;; emacs lisp mode
(evil-define-key 'normal emacs-lisp-mode-map (kbd "TAB") #'indent-for-tab-command)


;; company
(defvar company-mode/enable-yas nil
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(with-eval-after-load 'company
  (setq company-require-match 'nil);
  (setq company-idle-delay 0)
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 2)
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

;; which-key
(which-key-mode)

;; exit insert mode by pressing j and then k quickly
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)

;; making C-x easier to hit
(define-key key-translation-map [?\C-x] [?\C-u])
(define-key key-translation-map [?\C-u] [?\C-x])

;; buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
