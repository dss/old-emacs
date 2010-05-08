(add-to-list 'load-path "~/.emacs.d")
(push "/opt/local/bin" exec-path)
(push "~/bin" exec-path)

(require 'magit)
(require 'color-theme)
(load-file "~/.emacs.d/color-theme-twilight.el")
(color-theme-twilight)

(column-number-mode t)
(global-font-lock-mode t)
(line-number-mode t)
(setq delete-key-deletes-forward t)
(setq global-auto-revert-mode 1)
(setq auto-revert-mode nil)
(setq auto-save-default t)
(setq make-backup-files nil)
(setq mouse-yank-at-point t)
(setq truncate-lines t)
(setq truncate-partial-width-windows t)
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)
(auto-compression-mode)
(setq w32-use-w32-font-dialog nil)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)
;;(setq inhibit-eol-conversion t)

(defalias 'qrr 'query-replace-regexp)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(when window-system
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (server-start)
  (when (eq system-type 'darwin)
    (menu-bar-mode t)
    (defconst font 
      "-apple-monaco-medium-r-normal--10-120-72-72-m-120-mac-roman")
    (set-default-font font)
    (add-to-list 'default-frame-alist (cons 'font font))
    (setq browse-url-browser-function 'browse-url-default-macosx-browser)    
    (setq delete-by-moving-to-trash 1)
    (defun ns-raise-emacs ()
      (ns-do-applescript "tell application \"Emacs\" to activate"))
    (add-hook 'server-visit-hook 'ns-raise-emacs))
  (when (eq system-type 'gnu/linux)
    (defconst font 
      "-unknown-DejaVu Sans Mono-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1")
    (set-default-font font)
    (add-to-list 'default-frame-alist (cons 'font font)))
  (when (eq system-type 'windows-nt)
    (defconst font 
      "-*-Lucida Console-normal-r-*-*-12-112-96-96-c-*-iso8859-1")
    (set-default-font font)
    (add-to-list 'default-frame-alist (cons 'font font))))

(defun toggle-truncate-lines ()
  (interactive)
  (setq truncate-lines (not truncate-lines)))

(defun toggle-truncate-partial-width-windows ()
  (interactive)
  (setq truncate-partial-width-windows (not truncate-partial-width-windows)))

(defun indent-untabify-region ()
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (region-beginning) (region-end))
  (untabify (region-beginning) (region-end)))

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-ring-save)
(global-set-key "\M-C"     'calc)
(global-set-key "\M-M"     'gnus)
(global-set-key "\C-cg"    'ack)
(global-set-key "\C-xg"    'ack)
(global-set-key "\M-F"     'indent-untabify-region)
(global-set-key "\M-K"     'comment-region)
(global-set-key "\M-U"     'uncomment-region)
(global-set-key [\C-tab]   'other-frame)
(define-key global-map "\C-xw" 'what-line)
(define-key global-map "\C-z" 'undo-only)
(define-key global-map [delete] 'delete-char)
(define-key global-map [backspace] 'delete-backward-char)
(define-key global-map [f1] 'help-command)
(define-key global-map [f2] 'undo)
(define-key global-map [f3] 'isearch-forward)
(define-key global-map [f4] 'other-window)
(define-key global-map [f7] 'compile)
(define-key global-map [f12] 'revert-buffer)
(define-key global-map [button4] 'previous-line)
(define-key global-map [button5] 'next-line)

(setq ispell-program-name "aspell")
(add-hook 'text-mode-hook '(lambda() (flyspell-mode 1)))
(add-hook 'text-mode-hook '(lambda() (auto-fill-mode 1)))
(add-hook 'text-mode-hook '(lambda() (setq fill-column 70)))

(defadvice compile-internal (after my-scroll act comp)
  "Forces compile buffer to scroll. See around line 363 in compile.el"
  (let* ((ob (current-buffer))
         )
    (save-excursion
      (select-window (get-buffer-window ad-return-value))
      (goto-char (point-max))
      (select-window (get-buffer-window ob))
      )))
(setq compilation-scroll-output t)

(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode    "cc-mode" "C Editing Mode" t)
(autoload 'c-mode-common-hook "cc-mode" "C Mode Hooks" t)
(autoload 'c-add-style "cc-mode" "Add coding style" t)
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hrh$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.pan$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.rss$" . c++-mode))
(add-to-list 'auto-mode-alist '("make\\." . makefile-mode))

(setq blt-c-style
      '((c-auto-newline                 . nil)
        (c-basic-offset                 . 4)
        (c-comment-only-line-offset     . 0)
        (c-echo-syntactic-information-p . nil)
        (c-hungry-delete-key            . t)
        (c-tab-always-indent            . t)
        (c-toggle-hungry-state          . t)
        (c-hanging-braces-alist         . ((substatement-open after)
                                          (brace-list-open)))
        (c-offsets-alist                . ((arglist-close . c-lineup-arglist)
                                           (case-label . 4)
                                           (substatement-open . 0)
                                           (block-open . 0) ; no space before {
                                           (knr-argdecl-intro . -)))
        (c-hanging-colons-alist         . ((member-init-intro before)
                                           (inher-intro)
                                           (case-label after)
                                           (label after)
                                           (access-label after)))
        (c-cleanup-list                 . (scope-operator
                                           empty-defun-braces
                                           defun-close-semi))))

(defun lconfig-c-mode ()
  (progn (define-key c-mode-base-map "\C-m" 'newline-and-indent)
         (define-key c-mode-base-map "\C-z" 'undo)
         (define-key c-mode-base-map [f4] 'speedbar-get-focus)
         (define-key c-mode-base-map [f5] 'next-error)
         (define-key c-mode-base-map [f6] 'run-program)
         (define-key c-mode-base-map [f8] 'set-mark-command)
         (define-key c-mode-base-map [f9] 'insert-breakpoint)
         (define-key c-mode-base-map [f10] 'step-over)
         (define-key c-mode-base-map [f11] 'step-into)
         (c-add-style "BLT Coding Style" blt-c-style t)))
(add-hook 'c-mode-common-hook 'lconfig-c-mode)

;(autoload 'ruby-mode "ruby-mode" "Load ruby-mode")
;(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
;(add-hook 'ruby-mode-hook 'turn-on-font-lock)
;(autoload 'run-ruby "inf-ruby" "Inferior Ruby process")

;(defmacro add-to-hook (hook &rest forms)
;  "Add FORMS, as a lambda expression, to HOOK.
;   The lambda expression takes no arguments."
;  `(add-hook ,hook (lambda () ,@forms)))

;(defun g0-ruby-load-buffer ()
;  "Save current buffer, and load it into an inferior ruby process.
;   If there is no inferior ruby process running, start one.  Otherwise,
;   load the current buffer into the currently running process.  Switch to
;   `ruby-buffer'."
;  (interactive)
;  (save-buffer)
;  (let ((bufname (buffer-name))
;        (pop-up-windows t))
;    (run-ruby ruby-program-name)
;    (ruby-load-file bufname)))

;; For some ghastly reason, inf-ruby.el adds to ruby-mode-hook a
;; lambda that calls inf-ruby-keys, which resets the default
;; ruby-mode-map.
;(setq ruby-mode-hook nil)
;(add-to-hook 'ruby-mode-hook
;             (define-key ruby-mode-map [(control ?c) (control ?l)]
;'g0-ruby-load-buffer))
;(add-to-hook 'inferior-ruby-mode-hook
;             (define-key ruby-mode-map [(control ?c) (control ?l)]
;'g0-ruby-load-buffer))
;(add-to-hook 'inferior-ruby-mode-hook
;             (define-key ruby-mode-map [(meta return)] 'g0-ruby-end-block))
;(add-to-hook 'inferior-ruby-mode-hook
;             (define-key ruby-mode-map [(control ?c) (control ?p)]
;'g0-ruby-toggle-kirb-program))

;; Fix foolish calendar-mode scrolling.
;(add-hook 'calendar-load-hook
;          '(lambda ()
;             (setq mark-holidays-in-calendar t)
;             (define-key calendar-mode-map ">" 'scroll-calendar-left)
;             (define-key calendar-mode-map "<" 'scroll-calendar-right)
;             (define-key calendar-mode-map "\C-x>" 'scroll-calendar-left)
;             (define-key calendar-mode-map "\C-x<" 'scroll-calendar-right)))

(defun dss-toggle-shell ()
  (interactive)
  (if (get-buffer-window "*shell*" 'visible)
      (progn
        (delete-window (get-buffer-window "*shell*" 'visible))
        )
    (split-window-vertically)
    (shell)))

(define-key global-map "\M-`" 'dss-toggle-shell)

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond
   ((not (= (count-windows) 2))
    (message "You need exactly 2 windows to do this."))
   (t
    (let* ((w1 (first (window-list)))
       (w2 (second (window-list)))
       (b1 (window-buffer w1))
       (b2 (window-buffer w2))
       (s1 (window-start w1))
       (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
    (filename (buffer-file-name)))
    (if (not filename)
    (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
      (message "A buffer named '%s' already exists!" new-name)
    (progn
      (rename-file name new-name 1)
      (rename-buffer new-name)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
     (filename (buffer-file-name))
     (dir
      (if (string-match dir "\\(?:/\\|\\\\)$")
          (substring dir 0 -1) dir))
     (newname (concat dir "/" name)))

    (if (not filename)
    (message "Buffer '%s' is not visiting a file!" name)
      (progn
    (copy-file filename newname 1)
    (delete-file filename)
    (set-visited-file-name newname)
    (set-buffer-modified-p nil)
    t))))

;; This removes unsightly ^M characters that would otherwise
;; appear in the output of java applications.
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

(setq ediff-split-window-function 'split-window-horizontally)

(require 'cpp-font-lock)

(setq tramp-default-method "sshx")

(if (fboundp 'pc-selection-mode)
    (pc-selection-mode 1)
  (require 'pc-select))

(load-file "~/.emacs.d/sourcepair.el")
(define-key global-map "\C-o" 'sourcepair-load)
(setq sourcepair-source-path    '( "." "../*" ))
(setq sourcepair-header-path    '( "." "include" "../include" "../*" "../*/include"))
(setq sourcepair-recurse-ignore '( "CVS" "Obj" "Debug" "Release" ))

(require 'ido)
(ido-mode t)

(require 'regex-tool)
(require 'parenface)

(setq erc-fill-column 66)

(put 'narrow-to-region 'disabled nil)

; there is a tab here: ->   <-
(require 'show-wspace)
(add-hook 'c-mode-common-hook 'show-ws-highlight-tabs)

;; ack (grep replacement)
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link) [1]
(define-key global-map "\C-ca" 'org-agenda) [1]
(setq org-agenda-files (list "~/org/home.org"))

; Tab Completion Everywhere
; http://www.emacsblog.org/2007/03/12/tab-completion-everywhere/
(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
    (indent-according-to-mode)))

(defun my-tab-fix ()
  (local-set-key [tab] 'indent-or-expand))

(add-hook 'c++-mode-hook        'my-tab-fix)
(add-hook 'c-mode-hook          'my-tab-fix)
(add-hook 'sh-mode-hook         'my-tab-fix)
(add-hook 'emacs-lisp-mode-hook 'my-tab-fix)

(defun untabify-buffer ()
  "Untabify current buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(defun progmodes-hooks ()
  "Hooks for programming modes"
  (yas/minor-mode-on)
  (add-hook 'before-save-hook 'progmodes-write-hooks))

(defun progmodes-write-hooks ()
  "Hooks which run on file write for programming modes"
  (prog1 nil
    (set-buffer-file-coding-system 'utf-8-unix)
    (untabify-buffer)
    (delete-trailing-whitespace)))

(defun delete-trailing-whitespacep ()
  "Should we delete trailing whitespace when saving this file?"
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (next-line 25))
    (let ((pos (point)))
      (goto-char (point-min))
      (and (re-search-forward (concat "@author +" user-full-name) pos t) t))))

(defun maybe-delete-trailing-whitespace ()
  "Delete trailing whitespace if I am the author of this file."
  (interactive)
  (and (delete-trailing-whitespacep) (delete-trailing-whitespace)))

(add-hook 'c++-mode-hook 'progmodes-hooks)
(add-hook 'c-mode-hook 'progmodes-hooks)

;; Gnus / Mail
(setq user-mail-address "dss@orst.edu")
(setq user-full-name "Darren Shepard")
(load-library "smtpmail")
(load-library "nnimap")
(load-library "starttls")
(setq gnus-select-method '(nntp "news.free.fr"))
(setq gnus-secondary-select-method '(nnimap "gmail"
                                            (nnimap-address "imap.gmail.com")
                                            (nnimap-server-port 993)
                                            (nnimap-stream ssl)))

(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587
      smtpmail-auth-credentials 
      '(("smtp.gmail.com" 587 "darren.shepard@gmail.com" nil)))

(add-hook 'gnus-topic-mode-hook 'gnus-topic-mode)
(setq mm-attachment-override-types '("image/.*"))
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))
(setq gnus-ignored-newsgroups "") ;; show [Gmail]/* folders

;; VM
;; (require 'vm-autoloads)
;; (setq vm-primary-inbox
;;       "imap-ssl:imap.gmail.com:993:INBOX:login:darren.shepard@gmail.com:*")
;; (setq vm-imap-account-alist
;;       '("imap-ssl:imap.gmail.com:993:*:login:darren.shepard@gmail.com:*" "gmail"))
;; (setq vm-save-folder "~/mail")
;; (setq vm-auto-get-new-mail t)
;; (setq vm-imap-sync-on-get t)
;; (setq vm-preview-lines nil)

;; Mew
;; (autoload 'mew "mew" nil t)
;; (autoload 'mew-send "mew" nil t)
;; (if (boundp 'read-mail-command)
;;     (setq read-mail-command 'mew))
;; (autoload 'mew-user-agent-compose "mew" nil t)
;; (if (boundp 'mail-user-agent)
;;     (setq mail-user-agent 'mew-user-agent))
;; (if (fboundp 'define-mail-user-agent)
;;     (define-mail-user-agent
;;       'mew-user-agent
;;       'mew-user-agent-compose
;;       'mew-draft-send-message
;;       'mew-draft-kill
;;       'mew-send-hook))
;; (set-language-environment "Latin-1")
;; (set-input-method "latin-1-prefix")
;; (setq mew-name "Darren Shepard")
;; (setq mew-proto "%")
;; (setq mew-imap-user "darren.shepard@gmail.com")
;; (setq mew-imap-delete nil)
;; (setq mew-imap-server "imap.gmail.com")
;; (setq mew-imap-ssl t)

;; Little brother database
(autoload 'lbdb "lbdb" "Query the Little Brother's Database" t)
(autoload 'lbdb-region "lbdb" "Query the Little Brother's Database" t)
(autoload 'lbdb-maybe-region "lbdb" "Query the Little Brother's Database" t)
