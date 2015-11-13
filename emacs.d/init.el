(setq user-full-name "Dan Kee")
(setq user-mail-address "dan@dankee.com")
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(require 'cl)
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))
(defvar dkee/packages '(ac-slime
                        auto-complete
                        autopair
                        clojure-mode
                        coffee-mode
                        dash
                        deft
                        elixir-mode
                        gist
                        go-mode
                        haml-mode
                        haskell-mode
                        htmlize
                        magit
                        markdown-mode
                        marmalade
                        neotree
                        o-blog
                        org
                        paredit
                        pkg-info
                        puppet-mode
                        queue
                        restclient
                        rvm
                        smex
                        sml-mode
                        yaml-mode)
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
(setq inhibit-splash-screen t
      initial-scratch-message nil)

(when (locate-library "clojure-mode")
  (setq initial-major-mode 'clojure-mode))
(tool-bar-mode -1)
(menu-bar-mode -1)
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))
(setq tab-width 2
      indent-tabs-mode nil)
(setq make-backup-files nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)
(defvar dkee/vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path dkee/vendor-dir)

(dolist (project (directory-files dkee/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))
(load-theme 'wombat t)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-log-done t)
(setq org-todo-keywords
      '((sequence "TODO" "INPROGRESS" "DONE")))
(setq org-todo-keyword-faces
      '(("INPROGRESS" . (:foreground "blue" :weight bold))))
(setq org-agenda-files (list "~/Dropbox/org/personal.org"))
(require 'ob)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)))

(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(defvar org-babel-default-header-args:clojure
  '((:results . "silent") (:tangle . "yes")))

(defun org-babel-execute:clojure (body params)
  (lisp-eval-string body)
  "Done!")

(provide 'ob-clojure)

(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
(setq deft-directory "~/Dropbox/deft")
(setq deft-use-filename-as-title t)
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)
(setq column-number-mode t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(require 'autopair)
(setq lisp-modes '(lisp-mode
                   emacs-lisp-mode
                   common-lisp-mode
                   scheme-mode
                   clojure-mode))

(defvar lisp-power-map (make-keymap))
(define-minor-mode lisp-power-mode "Fix keybindings; add power."
  :lighter " (power)"
  :keymap lisp-power-map
  (paredit-mode t))
(define-key lisp-power-map [delete] 'paredit-forward-delete)
(define-key lisp-power-map [backspace] 'paredit-backward-delete)

(defun dkee/engage-lisp-power ()
  (lisp-power-mode t))

(dolist (mode lisp-modes)
  (add-hook (intern (format "%s-hook" mode))
            #'dkee/engage-lisp-power))



; SBCL SLIME

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



(add-hook 'ruby-mode-hook
          (lambda ()
            (autopair-mode)))

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(rvm-use-default)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(defun coffee-custom ()
  "coffee-mode-hook"
  (make-local-variable 'tab-width)
  (set 'tab-width 2))

(add-hook 'coffee-mode-hook 'coffee-custom)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda () (visual-line-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t html")

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

(setq-default indent-tabs-mode nil)

; scrolling
(global-set-key (kbd "C-M-e") 'scroll-up-line)
(global-set-key (kbd "C-M-y") 'scroll-down-line)

; Go to line
(global-set-key (kbd "M-:") 'goto-line)

(put 'upcase-region 'disabled nil)

;; neotree
(global-set-key [f8] 'neotree-toggle)

(require 'cider)

(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c h") 'windmove-left)

(setq linum-format "%d ")
(global-linum-mode t)
