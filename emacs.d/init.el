;; User info
(setq user-full-name "Dan Kee")
(setq user-mail-address "dan@dankee.com")

;; Packages
(require 'cl)
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(defvar dkee/packages '(clojure-mode
                        elixir-mode
                        ensime
                        find-file-in-project
                        find-file-in-repository
                        flx-ido
                        haskell-mode
                        magit
                        markdown-mode
                        neotree
                        paredit
                        queue ; For vendored cider
                        smex)
  "Default packages")
(defun dkee/packages-installed-p ()
  (loop for pkg in dkee/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))
(unless (dkee/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg dkee/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Vendored packages
(defvar dkee/vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path dkee/vendor-dir)
(dolist (project (directory-files dkee/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Start up
(setq inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message nil)

;; Display
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))
(load-theme 'wombat t)
(setq linum-format "%d ")
(global-linum-mode t)

;; Editing
(delete-selection-mode t)
(setq tab-width 2)
(setq-default indent-tabs-mode nil)
(setq make-backup-files nil)
(setq require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Navigation
(global-set-key (kbd "C-M-e") 'scroll-up-line)
(global-set-key (kbd "C-M-y") 'scroll-down-line)
(global-set-key (kbd "M-:") 'goto-line)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c h") 'windmove-left)

;; Smex mode (enhanced M-x)
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Ido (fuzzy finding)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching 1)
(setq ido-use-faces nil)
(global-set-key (kbd "C-x f") 'find-file-in-repository)

;; Neotree (file tree explorer)
(defun neotree-find-in-project-dir ()
  "Open NeoTree to current buffer relative to the git root."
  (interactive)
  (let ((project-dir (ffip-get-project-root-directory))
        (file-name (buffer-file-name)))
    (if project-dir
      (progn
        (neotree-dir project-dir)
        (neotree-find file-name))
      (message "Could not find git project root."))))
(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (ffip-get-project-root-directory))
        (file-name (buffer-file-name)))
    (if project-dir
      (neotree-dir project-dir)
      (message "Could not find git project root."))))
(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "C-c f") 'neotree-find-in-project-dir)
(global-set-key (kbd "C-c r") 'neotree-project-dir)

;; All Lisps
(show-paren-mode t)

;; Clojure
(when (locate-library "clojure-mode")
  (setq initial-major-mode 'clojure-mode))
(require 'cider)

;; SBCL SLIME
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/local/bin/sbcl --noinform --no-linedit")
; From http://www.emacswiki.org/SlimeMode
(add-hook 'slime-mode-hook
          (defun slime-sanitize-bindings ()
            "Removes SLIME's keybinding on M-n and M-p"
            (cond ((boundp 'slime-mode-map)
                   (define-key slime-mode-map (kbd "M-n") nil)
                   (define-key slime-mode-map (kbd "M-p") nil)
                   (message "slime keybindings on M-n and M-p have been sanitized"))
                  ('t (message "slime keybindings not sanitized")))))

;; Ruby
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda () (visual-line-mode t)))

;; Scala
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; Custom vars
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-cljs-repl
   "(do (require 'weasel.repl.websocket) (cemerick.piggieback/cljs-repl (weasel.repl.websocket/repl-env :ip \"127.0.0.1\" :port 9001)))")
 '(haskell-mode-hook (quote (turn-on-haskell-indent)) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
