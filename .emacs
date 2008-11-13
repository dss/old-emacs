
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
;;(setq inhibit-eol-conversion t)

(defalias 'qrr 'query-replace-regexp)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(when window-system
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
 
  (defconst color-scheme 'dark)
  (defconst background-color "black")
  (defconst foreground-color "white")

;;   (defconst color-scheme 'light)
;;   (defconst background-color "white")
;;   (defconst foreground-color "black")

  (set-background-color background-color)
  (set-foreground-color foreground-color)
  (set-cursor-color foreground-color)
  (add-to-list 'default-frame-alist (cons 'background-color background-color))
  (add-to-list 'default-frame-alist (cons 'foreground-color foreground-color))
  (add-to-list 'default-frame-alist (cons 'cursor-color foreground-color))

  (when (eq system-type 'windows-nt)
    (defconst font "-*-Lucida Console-normal-r-*-*-12-112-96-96-c-*-iso8859-1")
    (set-default-font font)
    (add-to-list 'default-frame-alist (cons 'font font))
    (add-to-list 'load-path (expand-file-name "~/elisp/"))
    (add-to-list 'load-path (expand-file-name "~/elisp/bbdb"))
    (add-to-list 'load-path (expand-file-name "~/elisp/jabber"))
    (add-to-list 'load-path (expand-file-name "~/elisp/nxml"))
    (add-to-list 'load-path (expand-file-name "~/elisp/w3m"))

    (require 'maxframe)
    (add-hook 'window-setup-hook 'maximize-frame t)

    (load "gnuserv")
    (gnuserv-start)
    (setq gnuserv-frame (car (frame-list))))

)

(defconst lconfig-font-lock-faces  
  (list '(font-lock-builtin-face 
          ((((class color) (background dark)) (:foreground "cyan" :bold t)) 
           (((class color)) (:foreground "DarkBlue" :bold t)))) 
        '(font-lock-comment-face 
          ((((class color) (background dark)) (:foreground "LightPink")) 
           (((class color)) (:foreground "FireBrick")))) 
        '(font-lock-constant-face
          ((((class color) (background dark)) (:foreground "SpringGreen")) 
           (((class color)) (:foreground "ForestGreen")))) 
        '(font-lock-doc-string-face
          ((((class color) (background dark)) (:foreground "SpringGreen")) 
           (((class color)) (:foreground "ForestGreen")))) 
        '(font-lock-function-name-face
          ((((class color) (background dark)) (:foreground "wheat3")) 
           (((class color)) (:foreground "DarkBlue")))) 
        '(font-lock-keyword-face
          ((((class color) (background dark)) (:foreground "SkyBlue" :bold t))
           (((class color)) (:foreground "DarkBlue" :bold t)))) 
        '(font-lock-preprocessor-face
          ((((class color) (background dark)) (:foreground "SkyBlue")) 
           (((class color)) (:foreground "gray40")))) 
        '(font-lock-reference-face
          ((((class color) (background dark)) (:foreground "yellow")) 
           (((class color)) (:foreground "maroon4")))) 
        '(font-lock-string-face
          ((((class color) (background dark)) (:foreground "SpringGreen")) 
           (((class color)) (:foreground "ForestGreen")))) 
        '(font-lock-type-face
          ((((class color) (background dark)) (:foreground "orange1")) 
           (((class color)) (:foreground "maroon4")))) 
        '(font-lock-variable-name-face
          ((((class color) (background dark)) (:foreground "yellow")) 
           (((class color)) (:foreground "SaddleBrown")))) 
        '(font-lock-warning-name-face
          ((((class color) (background dark)) (:foreground "DarkOrange")) 
           (((class color)) (:foreground "DarkOrange"))))))

(autoload 'custom-set-faces "font-lock" "Set the color scheme" t)
(autoload 'font-lock-fontify-buffer "font-lock" "Fontify Buffer" t)
(condition-case err
    (progn (apply 'custom-set-faces lconfig-font-lock-faces)
           (add-hook 'c-mode-common-hook 'font-lock-fontify-buffer)
           (add-hook 'emacs-lisp-mode-hook 'font-lock-fontify-buffer))
  (error (progn
           (message "Could not customize colors, disabling colored fonts.")
           (setq-default font-lock-auto-fontify t))))

(defun toggle-truncate-lines ()
  (interactive)
  (setq truncate-lines (not truncate-lines)))

(defun toggle-truncate-partial-width-windows ()
  (interactive)
  (setq truncate-partial-width-windows (not truncate-partial-width-windows)))

(defun indent-untabify-region ()
  (interactive)
  (indent-region (region-beginning) (region-end))
  (untabify (region-beginning) (region-end)))

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-ring-save)
(global-set-key "\M-C"     'calc)
(global-set-key "\M-M"     'gnus)
(global-set-key "\C-cg"    'rgrep)
(global-set-key "\C-xg"    'rgrep)
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

(autoload 'display-time "time" "Display Time" t)
(condition-case err
    (display-time)
  (error (message "Unable to load Time package.")))
(setq display-time-24hr-format nil)
(setq display-time-day-and-date t)

(add-hook 'text-mode-hook '(lambda() (auto-fill-mode 1)))
(add-hook 'text-mode-hook '(lambda() (setq fill-column 78)))

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

(autoload 'ruby-mode "ruby-mode" "Load ruby-mode")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-hook 'ruby-mode-hook 'turn-on-font-lock)
(autoload 'run-ruby "inf-ruby" "Inferior Ruby process")

(defmacro add-to-hook (hook &rest forms)
  "Add FORMS, as a lambda expression, to HOOK.
   The lambda expression takes no arguments."
  `(add-hook ,hook (lambda () ,@forms)))

(defun g0-ruby-load-buffer ()
  "Save current buffer, and load it into an inferior ruby process.
   If there is no inferior ruby process running, start one.  Otherwise,
   load the current buffer into the currently running process.  Switch to
   `ruby-buffer'."
  (interactive)
  (save-buffer)
  (let ((bufname (buffer-name))
        (pop-up-windows t))
    (run-ruby ruby-program-name)
    (ruby-load-file bufname)))

(defun g0-ruby-end-block ()
  "Insert an indented \"end\" on its own line.
   If the current line contains only whitespace, insert it on the current
   line.  Otherwise, insert the \"end\" line after the current line.
   Insert a newline after the \"end\" and leave point after it."
  (interactive)
  (unless (string-match "^[ \\t\\r\\f]*$"
                        (buffer-substring (point-at-bol) (point-at-eol)))
    (end-of-line)
    (insert ?\n))
  (insert "end")
  (indent-for-tab-command))

(defun g0-ruby-toggle-irb-program ()
  "Toggle `ruby-program-name' between \"ruby\" and \"ruby1.7\"."
  (interactive)
  (setq ruby-program-name (if (and (boundp 'ruby-program-name)
                                   (string= ruby-program-name "irb --inf-ruby-mode"))
                              "irb1.7 --inf-ruby-mode"
                            "irb --inf-ruby-mode"))
  (message "Using ruby: %s" ruby-program-name))

;; For some ghastly reason, inf-ruby.el adds to ruby-mode-hook a
;; lambda that calls inf-ruby-keys, which resets the default
;; ruby-mode-map.
(setq ruby-mode-hook nil)
(add-to-hook 'ruby-mode-hook
             (define-key ruby-mode-map [(control ?c) (control ?l)] 
'g0-ruby-load-buffer))
(add-to-hook 'ruby-mode-hook
             (define-key ruby-mode-map [(meta return)] 'g0-ruby-end-block))
(add-to-hook 'ruby-mode-hook
             (define-key ruby-mode-map [(control ?c) (control ?p)] 
'g0-ruby-toggle-irb-program))
(add-to-hook 'inferior-ruby-mode-hook
             (define-key ruby-mode-map [(control ?c) (control ?l)] 
'g0-ruby-load-buffer))
(add-to-hook 'inferior-ruby-mode-hook
             (define-key ruby-mode-map [(meta return)] 'g0-ruby-end-block))
(add-to-hook 'inferior-ruby-mode-hook
             (define-key ruby-mode-map [(control ?c) (control ?p)] 
'g0-ruby-toggle-kirb-program))

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

;; Ocaml
(setq auto-mode-alist (cons '("\\.ml[iyl]?$" .  caml-mode) auto-mode-alist))
(autoload 'caml-mode "ocaml" (interactive) "Major mode for editing Caml code." t)
(autoload 'camldebug "camldebug" (interactive) "Debug caml mode")
(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)

;; Fix foolish calendar-mode scrolling.
(add-hook 'calendar-load-hook 
          '(lambda ()
             (setq mark-holidays-in-calendar t)
             (define-key calendar-mode-map ">" 'scroll-calendar-left)
             (define-key calendar-mode-map "<" 'scroll-calendar-right)
             (define-key calendar-mode-map "\C-x>" 'scroll-calendar-left)
             (define-key calendar-mode-map "\C-x<" 'scroll-calendar-right)))

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

(setq shell-file-name "bash")
(setenv "SHELL" shell-file-name) 
(setq explicit-shell-file-name shell-file-name) 

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

;; (setq viper-mode t)
;; (require 'viper)

(load-file "~/elisp/sourcepair.el")
(define-key global-map "\C-o" 'sourcepair-load)
(setq sourcepair-source-path    '( "." "../*" ))
(setq sourcepair-header-path    '( "." "include" "../include" "../*" "../*/include"))
(setq sourcepair-recurse-ignore '( "CVS" "Obj" "Debug" "Release" ))

(require 'psvn)

(require 'jabber)
(setq jabber-username "darren.shepard")
(setq jabber-nickname "Darren")
(setq jabber-connection-type (quote ssl))
(setq jabber-network-server "talk.google.com")
(setq jabber-server "gmail.com")
(setq jabber-resource (concat "emacs." (downcase system-name)))
(setq jabber-vcard-avatars-retrieve nil)
(setq jabber-roster-show-bindings nil)
(setq jabber-activity-make-strings 'jabber-activity-make-strings-shorten)
(setq jabber-alert-presence-hooks nil)
(setq jabber-alert-muc-hooks nil)
(add-hook 'jabber-chat-mode-hook 'flyspell-mode)
(add-hook 'jabber-post-connect-hook 'jabber-autoaway-start)

(defun dss-jabber-toggle-chat ()
  (interactive)
  (split-window-vertically -15))

;; Nokia mail setup
(setq 
 user-full-name "Shepard Darren (Nokia-M/SanFrancisco)"
 user-mail-address "darren.shepard@nokia.com"
 smtpmail-smtp-server "dae102.yav4.com"
 smtpmail-default-smtp-server "dae102.yav4.com"
 send-mail-function 'smtpmail-send-it
 message-send-mail-function 'smtpmail-send-it
 smtpmail-smtp-service 25
 smtpmail-auth-credentials '(("dae102.yav4.com" 25 "NOE/dashepar" nil))
 )

(setq gnus-select-method 
      '(nnimap "nokia" (nnimap-address "dae102.yav4.com")))

(setq gnus-message-archive-group 
      '("nnimap+nokia:Sent Items"))

;; Tree view for groups.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Threads!  I hate reading un-threaded email -- especially mailing
;; lists.  This helps a ton!
(setq gnus-summary-thread-gathering-function 
      'gnus-gather-threads-by-subject)

;; Also, I prefer to see only the top level message.  If a message has
;; several replies or is part of a thread, only show the first
;; message.  'gnus-thread-ignore-subject' will ignore the subject and
;; look at 'In-Reply-To:' and 'References:' headers.
(setq gnus-thread-hide-subtree nil)
(setq gnus-thread-ignore-subject t)
(setq gnus-permanently-visible-groups "INBOX")
(setq gnus-fetch-old-headers t)
(setq gnus-posting-styles
      '((".*" (name "Darren Shepard"))))
;;(setq gnus-use-full-window t)

;; And finally, make sure all messags I've read (IMAP or NNTP) are
;; cached locally so I have them available when I'm offline:
;; (setq gnus-agent-cache t
;;       gnus-agent-consider-all-articles t
;;       gnus-agent-enable-expiration 'DISABLE
;;       gnus-select-article-hook 'gnus-agent-fetch-selected-article)

;; vm setup
(setq vm-init-file nil)
(autoload 'vm "vm" "Start VM on your primary inbox." t)
(autoload 'vm-other-frame "vm" "Like `vm' but starts in another frame." t)
(autoload 'vm-visit-folder "vm" "Start VM on an arbitrary folder." t)
(autoload 'vm-visit-virtual-folder "vm" "Visit a VM virtual folder." t)
(autoload 'vm-mode "vm" "Run VM major mode on a buffer" t)
(autoload 'vm-mail "vm" "Send a mail message using VM." t)
(autoload 'vm-submit-bug-report "vm" "Send a bug report about VM." t)
(setq vm-primary-inbox "~/mail/inbox.mbox")
(setq vm-crash-box "~/mail/inbox.crash.mbox")
(setq vm-spool-files '(("~/mail/inbox.mbox" "imap:dae102.yav4.com:143:INBOX:login:dashepar:*" "~/mail/inbox.crash.mbox")))
(setq vm-imap-server-list '("imap:dae102.yav4.com:143:inbox:login:dashepar:*"))
(setq vm-imap-expunge-after-retrieving nil)

(defun w32-browser (doc)
  "Browse to a particular file/URL using default web browser"
  (w32-shell-execute 1 doc))

(eval-after-load "dired"
  '(define-key dired-mode-map [f3] (lambda () 
				     (interactive)
				     (w32-browser
				      (dired-replace-in-string 
				       "/" "\\" 
                       (dired-get-filename))))))

(load "rng-auto.el")
(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg") t) "\\'")
                   'nxml-mode))
(unify-8859-on-decoding-mode)
(setq magic-mode-alist
      (cons '("<\\?xml " . nxml-mode)
            magic-mode-alist))
(fset 'xml-mode 'nxml-mode)
(fset 'html-mode 'nxml-mode)

(require 'w3m-load)
(setq mm-text-html-renderer 'w3m)
(setq w3m-use-tab t)

(require 'bbdb)
(bbdb-initialize 'gnus 'message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail)
(setq bbdb-north-american-phone-numbers-p nil)
(setq bbdb-always-add-addresses t)
(setq bbdb/mail-auto-create-p t)
(setq bbdb-use-pop-up nil)
(setq bbdb-offer-save nil)

;;(require 'iswitchb)
;;(iswitchb-mode t)

(require 'ido)
(ido-mode t)

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (ido-completing-read "Project file: "
                         (tags-table-files)
                         nil t)))

(global-set-key [(meta t)] 'ido-find-file-in-tag-files)

(cua-mode t)

(require 'regex-tool)

;; (require 'gtags)
;; (setq google-tags-host-port-alist '((c++-mode . (("localhost" . 2223)))))

;; ECB related
(add-to-list 'load-path (expand-file-name "~/elisp/cedet-1.0pre4/cogre"))
(add-to-list 'load-path (expand-file-name "~/elisp/cedet-1.0pre4/common"))
(add-to-list 'load-path (expand-file-name "~/elisp/cedet-1.0pre4/contrib"))
(add-to-list 'load-path (expand-file-name "~/elisp/cedet-1.0pre4/ede"))
(add-to-list 'load-path (expand-file-name "~/elisp/cedet-1.0pre4/eieio"))
(add-to-list 'load-path (expand-file-name "~/elisp/cedet-1.0pre4/semantic"))
(add-to-list 'load-path (expand-file-name "~/elisp/cedet-1.0pre4/speedbar"))
(add-to-list 'load-path (expand-file-name "~/elisp/ecb-2.32"))

;; Load CEDET
(load-file "~/elisp/cedet-1.0pre4/common/cedet.el")

;; Enabling various SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
;; Select one of the following:

;; * This enables the database and idle reparse engines
(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as the nascent intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;;(semantic-load-enable-guady-code-helpers)

;; * This turns on which-func support (Plus all other code helpers)
(semantic-load-enable-excessive-code-helpers)

;; This turns on modes that aid in grammar writing and semantic tool
;; development.  It does not enable any other features such as code
;; helpers above.
;;(semantic-load-enable-semantic-debugging-helpers)

;;(require 'ecb)

(add-to-list 'load-path (expand-file-name "~/elisp/gtags"))
(require 'gtags)
(setq google-tags-host-port-alist '((c++-mode . (("localhost" . 2223)))))

;; erlang
(setq load-path (cons  (expand-file-name "~/app/erl5.6.2/lib/tools-2.6.1/emacs") load-path))
(setq erlang-root-dir (expand-file-name "~/app/erl5.6.2"))
(setq exec-path (cons (expand-file-name "~/app/erl5.6.2/bin") exec-path))
(require 'erlang-start)

;; arc
(add-to-list 'load-path (expand-file-name "~/app/arc/extras"))
(autoload 'run-arc "inferior-arc"
  "Run an inferior Arc process, input and output via buffer *arc*.")
(autoload 'arc-mode "arc"
  "Major mode for editing Arc." t)
(add-to-list 'auto-mode-alist '("\\.arc$" . arc-mode))
(setq arc-program-name "arc.bat")

;; make arc REPL read-only
(add-hook 'inferior-arc-mode-hook
       (lambda ()
         (set (make-local-variable 'comint-use-prompt-regexp) t)
         (set (make-local-variable 'comint-prompt-read-only) t)))

;; arc use parenface
(require 'parenface)
(add-hook 'arc-mode-hook
         (paren-face-add-support arc-font-lock-keywords-2))
(add-hook 'arc-interaction-mode-hook
         (paren-face-add-support arc-font-lock-keywords-2))

(setq erc-fill-column 66)

(put 'narrow-to-region 'disabled nil)

; there is a tab here: ->	<-
(require 'show-wspace)
(add-hook 'c-mode-common-hook 'show-ws-highlight-tabs)

; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link) [1]
(define-key global-map "\C-ca" 'org-agenda) [1]

(shell)
