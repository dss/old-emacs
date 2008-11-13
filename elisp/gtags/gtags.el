;;; Emacs Lisp Code for handling google tags
;;; Authors: Piaw Na, Arthur A. Gleckler
;;; Copyright Google Inc. 2004-2005

;;; $Id: //depot/opensource/gtags/gtags.el#3 $

;;; xemacs users MUST load /home/build/eng/xemacs/etags.el
(defun google-xemacs()
  (string-match "XEmacs" emacs-version))

(if (google-xemacs)
    (progn
      (load "etags")
      (require 'wid-browse)))

(require 'cl)
(require 'etags)
(require 'google-emacs-utilities)

(defvar google-tags-default-mode 'c++-mode
  "Mode for the language to use to look up the Gtags server when the
buffer and file extension don't map to a Gtags server.  Must be one of
the symbols c++-mode, java-mode, jde-mode, or python-mode.")

(defvar google-tags-host-port-alist
  '((c++-mode . (("gtags.google.com" . 2223)))
    (java-mode . (("gtags.google.com" . 2224)))
    (jde-mode . (("gtags.google.com" . 2224)))
    (python-mode . (("gtags.google.com" . 2225))))
  "Host/port pairs for each language mode")


(defvar google-tags-extensions-host-port-alist
  '(("cc" . (("gtags.google.com" . 2223))))
  "Host/port pair for each file extension type. Note that files that
trigger a mode don't need this. (The default is provided as an example.)")

(defvar google-tags-callgraph-host-port-alist
  '((c++-mode . (("gtags.google.com" . 2233)))
    (java-mode . (("gtags.google.com" . 2234)))
    (jde-mode . (("gtags.google.com" . 2234)))
    (python-mode . (("gtags.google.com" . 2235))))
  "Host/port pair for each language mode for find-caller functionality.")

(defvar google-tags-extensions-callgraph-host-port-alist
  '(("cc" . (("gtags.google.com" . 2233))))
  "Host/port pair for each file extension type for find-caller functionality.
Note that files that trigger a mode don't need this. (The default is provided
as an example.")

(defconst google-gtags-opcode-search-snippets "$"
  "Gtags server opcode for searching tag snippets.")

(defconst google-gtags-opcode-search-tag ":"
  "Gtags server opcode for searching tags.")

(defconst google-gtags-opcode-search-tags-by-file "@"
  "Gtags server opcode for searching tags by file.")

(defun google-tags-find-host-port-pair (caller-p buffer)
  "For the caller/callee, find the host/port pair."
  (let* ((host-port-alist (if caller-p google-tags-callgraph-host-port-alist
                            google-tags-host-port-alist))
         (ext-host-port-alist
          (if caller-p google-tags-extensions-callgraph-host-port-alist
            google-tags-extensions-host-port-alist))
         (filename (buffer-file-name buffer))
         (file-ext (and filename (file-name-extension filename)))
         (fileport (and file-ext (assoc file-ext ext-host-port-alist)))
         (entry (assoc major-mode host-port-alist)))
    (cond (entry (cdr entry))
          (fileport (cdr fileport))
          (t (cdr (assoc google-tags-default-mode host-port-alist))))))

(defvar google-default-root "/home/build/google3/"
  "The default location to search for files that aren't checked out
      in your source root." )

(defvar google-source-root ""
  "The root of your source tree/google3 client.")

(defvar google-tags-history nil
  "a stack of visited locations")

(defvar google-first-tag-matches nil
  "List of gtags matches remaining for use with `google-next-tag'.")

(defcustom google-bias-to-header-files nil
  "If turned on, whenever there are only two entries and there's just a header
file and a .cc file, we ignore the .cc file and jump straight to the header.")

;;; Deduce the current source root from the current file
(defun find-google-source-root (buffername)
  "Deduce your google source root from your current buffer.
If it can't find it then use google-source-root."
  google-source-root)

(defun google-find-default-tag ()
  "Locate the current identifier that the cursor is pointed at, and
present it as the default, stripping out font information and other
such garbage."
  (save-excursion
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (or (re-search-backward "\\sw\\|\\s_"
                                (save-excursion (beginning-of-line) (point))
                                t)
            (re-search-forward "\\(\\sw\\|\\s_\\)+"
                               (save-excursion (end-of-line) (point))
                               t))
        (progn (goto-char (match-end 0))
               (buffer-substring-no-properties (point)
                                 (progn (forward-sexp -1)
                                        (while (looking-at "\\s'")
                                          (forward-char 1))
                                        (point))))
      nil)))

(defun google-find-tag-tag (string)
  "Present the default tag, and prompt the user for a tag. If the user hit enter
then the default is picked. Otherwise, use whatever the user wants."
  (let* ((default (google-find-default-tag))
         (x (print default))
         (spec (completing-read
                (if default (format "%s(default %s) " string default)
                  string)
                #'google-completion-collection-function)))
    (if (equal spec "")
        (or (list default) (error "No default"))
      (list spec))))

(defun google-generate-potential-filenames (filename default-filename)
  "Returns a list of filenames that might correspond to where
this tag actually is."
    (list filename default-filename))

(defun google-select-from-files (filelist)
  "Return a file that exist from this file, in order."
  (find-if '(lambda (x) (file-exists-p x)) filelist))

(defconst gtags-server-buffer-name " *TAGS server*"
  "buffer used for communication with gtags server")

;;; Structure definition and accessors for the records returned to us
;;; by the tags server
(defstruct (tagrecord (:type list))
  tag
  data)

;; Override the field definition above for `tagrecord-data' since it
;; is `cadr', not `cdr'.
(defun tagrecord-data (tagrecord)
  (cdr tagrecord))

(defstruct (tagrecord-data (:type list))
  snippet
  filename
  filesize
  lineno
  offset)

(defun network-streams-ready (network-streams buffer-names)
  (do ((streams network-streams (cdr streams))
       (names buffer-names (cdr names)))
      ((null streams) nil)
    (if (not (eq (process-status (car streams)) 'open))
	(return (car names)))))

(defun google-send-tag-to-server (command buffer-name caller-p buffer)
  "Send a tag to the tag server, then wait for the response."
  (if (get-buffer buffer-name) (kill-buffer buffer-name))
  (let ((host-port-pairs (google-tags-find-host-port-pair caller-p buffer))
	(network-streams nil)
	(temp-buffer-names nil)
	(ready-stream-buffer-name nil))
    (dolist (host-port-pair host-port-pairs)
      (let* ((host (car host-port-pair))
	     (port (cdr host-port-pair))
	     (tmp-buffer-name (generate-new-buffer-name "*gtags-temp*"))
	     (network-stream-name (symbol-name (gensym)))
	     ;;; we ignore all errors involved in opening and writing to network streams
	     ;;; since if one data center is down we want to go to the others
	     (s (condition-case nil
		    (open-network-stream network-stream-name tmp-buffer-name host port)
		  (error nil))))
	(condition-case nil
	    (process-send-string network-stream-name (concat command "\r\n"))
	  (error (setq s nil)))
	(cond (s (setq temp-buffer-names (cons tmp-buffer-name temp-buffer-names))
		 (setq network-streams (cons s network-streams))))))
    (while (not ready-stream-buffer-name)
      (setq ready-stream-buffer-name (network-streams-ready network-streams temp-buffer-names))
      (if (not ready-stream-buffer-name)
	  (sleep-for 0.1)))
    (dolist (buffer-name temp-buffer-names)
      (if (not (eq buffer-name ready-stream-buffer-name))
	       (kill-buffer buffer-name)))
    (save-excursion
      (set-buffer ready-stream-buffer-name)
      (rename-buffer buffer-name))))
    

(defun google-find-tag (tagname)
  "Locates the tag by talking to the google tag server."
   (interactive (google-find-tag-tag "Tag: "))
   (google-show-tag-locations tagname))

(defun google-search-definition-snippets (string)
  "Allows the user to search snippets."
  (interactive "sSearch snippets for: ")
  (google-show-tag-locations-with-opcode
   string
   '(lambda (x) x)
   nil
   "SNIPPETS:"
   google-gtags-opcode-search-snippets))

(defun google-search-caller-snippets (string)
  "Allows the user to search snippets."
  (interactive "sSearch snippets for: ")
  (google-show-tag-locations-with-opcode
   string
   '(lambda (x) x)
   t
   "SNIPPETS:"
   google-gtags-opcode-search-snippets))


(define-derived-mode google-browse-tags-mode text-mode
  "GTAGS"
  "Major mode for browsing Google tags.
\\[google-visit-tag-under-point] to jump to a particular tag
\\[google-show-all-results] to show all tags
\\[google-narrow-by-filename] to perform a narrowing
\\[delete-window] to quit browsing")

(defun google-visit-tagrecord (tagrecord push display-buffer)
  "Given a tag record, jump to the line/file."
  (let* ((tagname (tagrecord-tag tagrecord))
         (data (tagrecord-data tagrecord))
         (filename (tagrecord-data-filename data))
         (snippet (tagrecord-data-snippet data))
         (lineno (tagrecord-data-lineno data))
         (offset (tagrecord-data-offset data))
         (etagrecord (cons snippet (cons lineno offset)))
         (actual-filename
          (expand-file-name
           (concat (file-truename (find-google-source-root default-directory))
                   filename)))
         (default-filename (expand-file-name
                            (concat (file-truename google-default-root)
                                    filename)))
         (selected-file (google-select-from-files
                         (google-generate-potential-filenames
                          actual-filename
                          default-filename))))
    (if push (google-push-tag))  ; Save the tag so we can come back.
    (save-selected-window
      (if display-buffer
	  (select-window gdoc-display-window))
      (if selected-file
          (progn
            (find-file selected-file)
	    (etags-goto-tag-location etagrecord))
        (error "Can't find file any more.")))))

(defun google-visit-tag-under-point ()
  "In GTAGS mode, visit the tag represented by the line."
  (interactive)
  (let ((tagrecord (get-text-property (point) 'gtag)))
    (google-visit-tagrecord tagrecord t nil)))

(define-key google-browse-tags-mode-map
  "\C-m"
  'google-visit-tag-under-point)

(defun google-xemacs-visit-tag-under-point ()
  "Wierd hack to let xemacs work properly with the mouse."
  (interactive)
  (mouse-set-point current-mouse-event)
  (google-visit-tag-under-point))

(if (google-xemacs)
    (define-key google-browse-tags-mode-map
      [button1]
      'google-xemacs-visit-tag-under-point)
  (define-key google-browse-tags-mode-map
    [mouse-1]
    'google-visit-tag-under-point))
(define-key google-browse-tags-mode-map
  "q"
  'delete-window)
(define-key google-browse-tags-mode-map
  "a"
  'google-show-all-results)
(define-key google-browse-tags-mode-map
  "n"
  'google-narrow-by-filename)
(define-key google-browse-tags-mode-map
  "h"
  'describe-mode)

(defun google-show-callers (tagname)
  "Given a tag, show the callers."
  (interactive (google-find-tag-tag "Tag: "))
  (google-show-tag-locations-with-opcode
   tagname
   '(lambda (x) (string-equal tagname (tagrecord-tag x)))
   t
   "*CALLERS: "
   google-gtags-opcode-search-tag))

(defun google-show-callers-under-point ()
  "Immediately show the tag locations under the point."
  (interactive)
  (google-show-callers (google-find-default-tag)))

(defun google-show-tag-locations-under-point ()
  "Immediately jump to tag location under the point.  If the text has
a `gtag' property, jump to that specific location rather than looking
the tag up."
  (interactive)
  (let ((tagrecord (get-text-property (point) 'gtag)))
    (if tagrecord
        (google-visit-tagrecord tagrecord nil nil)
      (google-show-tag-locations (google-find-default-tag)))))

(defun google-show-tag-locations (tagname)
  "Query the tag server for a list of matches, and show the tag, the snippet,
and what file the tag is found it and allow the user to pick from one. If there
is only one match, then jump directly to the match"
  (interactive (google-find-tag-tag "Tag: "))
  (google-show-tag-locations-with-opcode
   tagname
   '(lambda (x) (string-equal tagname (tagrecord-tag x)))
   nil
   nil
   google-gtags-opcode-search-tag))

(defun google-show-tag-locations-regexp (tagname)
  "Given a regexp tag, if it is unique, perform a google-find-tag. Otherwise,
query the tag server for a list of matches, and show the tags, the snippet,
and what file the tag is found it and allow the user to pick from one."
  (interactive (google-find-tag-tag "Tag: "))
  (google-show-tag-locations-with-opcode
   tagname
   '(lambda (x) (string-match tagname (tagrecord-tag x)))
   nil
   nil
   google-gtags-opcode-search-tag))

(defun google-get-tags (command caller-p buffer)
  "Send `command' to a gtags server chosen based on the type of code
being edited in `buffer' and on `caller-p'.  If `caller-p' is true,
show callers.

Note that `buffer' is passed in rather than being implicit because we
want to be able to memoize the results of these calls, and the buffer
affects the results."
  (google-send-tag-to-server command gtags-server-buffer-name caller-p buffer)
  (save-excursion
    (set-buffer gtags-server-buffer-name)
    (goto-char (point-min))
    (read (current-buffer))))

(memoize 'google-get-tags)

(defun google-get-matches (regexp buffer)
  (google-get-tags (concat google-gtags-opcode-search-tag regexp) nil buffer))

(defun google-get-unique-matches (opcode tagname regexp-predicate caller-p)
  (let ((data (google-get-tags (concat opcode tagname) caller-p (current-buffer))))
    (remove-duplicates (google-filter regexp-predicate data)
                       :test 'equal)))

(defun google-fetch-matches (pattern predicate buffer)
  (google-filter
   (or predicate #'identity)
   (remove-duplicates
    (mapcar #'car (google-get-matches pattern buffer))
    :test #'equal)))

(defun google-all-matching-completions (string predicate)
  (let ((prefix-pattern (concat string ".*")))
    (google-fetch-matches prefix-pattern predicate (current-buffer))))

(defun google-exact-match (string predicate)
  (google-fetch-matches string predicate (current-buffer)))

(defun google-longest-common-prefix (string1 string2)
  (do ((index 0 (+ index 1)))
      ((or (= index (length string1))
           (= index (length string2))
           (not (= (aref string1 index)
                   (aref string2 index))))
       (substring string1 0 index))))

(defun google-longest-common-prefix-multi (strings)
  (reduce #'google-longest-common-prefix strings))

(defun google-completion-collection-function (string predicate operation-type)
  (case operation-type
    ((nil)
     (let ((completions (google-all-matching-completions string predicate)))
       (and completions
            (let ((first-completion (car completions)))
              (cond ((null (cdr completions))
                     (or (string-equal string first-completion)
                         first-completion))
                    ((member string completions) string)
                    (t (google-longest-common-prefix-multi completions)))))))
    ((t) (google-all-matching-completions string predicate))
    ((lambda) (google-exact-match string predicate))))

(defun google-complete-tag ()
  "Perform gtags completion on the text around point."
  (interactive)
  (let ((tag (google-find-default-tag)))
    (unless tag
      (error "Nothing to complete"))
    (let* ((start (progn (search-backward tag) (point)))
           (end (progn (forward-char (length tag)) (point)))
           (completions (google-all-matching-completions tag nil)))
      (cond ((null completions)
             (message "Can't find any completions for \"%s\"" tag))
            ((null (cdr completions))
             (if (string-equal tag (car completions))
                 (message "No more completions.")
               (delete-region start end)
               (insert (car completions))))
            (t
             (let ((longest-common-prefix
                    (google-longest-common-prefix-multi completions)))
               (if (string-equal tag longest-common-prefix)
                   (progn
                     (message "Making completion list...")
                     (with-output-to-temp-buffer "*Completions*"
                       (display-completion-list completions))
                     (message "Making completion list...%s" "done"))
                 (delete-region start end)
                 (insert (car completions)))))))))

(defun google-show-tag-locations-with-opcode (tagname
                                              regexp-pred
                                              caller-p
                                              prefix
                                              opcode)
  "Given `tagname', if it is unique, perform a
`google-find-tag'. Otherwise, query the tag server for a list of
matches, and show the tag, the snippet, and what file the tag is found
it and allow the user to pick from one.

`Regexp-pred' is a function that takes a tagrecord and returns non-nil
if it matches.

`Opcode' specifies which server \"opcode\" operation to perform,
e.g. `:' for direct tag lookup.

If 'caller-p' is set, then we probe the callgraph alist instead of the
standard alist.  The string in `prefix' is then used to name the buffer"
  (let ((current-directory default-directory))
    (google-push-tag)            ;; Save the tag for later.
    (let ((matches
           (google-get-unique-matches opcode tagname regexp-pred caller-p)))
      (cond ((= (length matches) 0) (error "No tag found!"))
	    ((and google-bias-to-header-files
		  (= (length matches) 2)
		  (let* ((match1 (car matches))
			 (match2 (cadr matches))
			 (file1 (tagrecord-data-filename (tagrecord-data match1)))
			 (file2 (tagrecord-data-filename (tagrecord-data match2))))
		    (and (equal (file-name-sans-extension file1)
				(file-name-sans-extension file2))
			 (or (equal (file-name-extension file1) "h")
			     (equal (file-name-extension file2) "h")))))
	     (google-visit-tagrecord
	      (if (equal (file-name-extension (tagrecord-data-filename
					       (tagrecord-data (car matches))))
			 "h")
		  (car matches)
		(cadr matches))
	      nil
	      nil))
            ((= (length matches) 1) (google-visit-tagrecord (car matches) nil nil))
            (t (google-show-tag-locations-inner
                matches
                tagname
                (concat
                 (or prefix "*TAGS: ") tagname "*")
                current-directory
                t))))))

(defun google-show-tag-locations-inner (matches
                                        tagname
                                        buffer-name
                                        current-directory
                                        switch-windows-p
					&optional win)
  (save-selected-window
    (if win
	(select-window win))
    (let ((face 'default)
	  (buffer (get-buffer-create buffer-name))
	  (text-width (1- (window-width))))
      (save-excursion
	(set-buffer buffer)
	(erase-buffer)
	(display-buffer buffer)
	(google-browse-tags-mode)
	(set (make-local-variable 'truncate-lines) t)
	(dolist (tagrecord matches)
	  ;; Alternate between default and highlight.
	  (if (eq face 'default)
	      (setq face 'highlight)
	    (setq face 'default))
	  (let* ((prop-start (point))
		 (tag (tagrecord-tag tagrecord))
		 (data (tagrecord-data tagrecord))
		 (snippet (tagrecord-data-snippet data))
		 (filename (tagrecord-data-filename data)))
	    (princ
	     (format (format "%s%%%ds" tag
			     (- text-width (length tag)))
		     filename)
	     buffer)
	    (princ "\n" buffer)
	    (princ snippet buffer)
	    (add-text-properties prop-start (point)
				 `(gtag ,tagrecord
					help-echo "mouse-1: go to this occurrence"
					mouse-face secondary-selection))
	    (princ "\n" buffer)
	    (if (google-xemacs)
		(let ((xemacs-region (make-extent prop-start (point))))
		  (set-extent-property xemacs-region 'face face)
		  (set-extent-property xemacs-region
				       'mouse-face
				       'widget-button-face))
	      (overlay-put (make-overlay prop-start (point)) 'face face))))
	(set (make-local-variable 'default-directory) current-directory)
	(if switch-windows-p
	    (switch-to-buffer-other-window buffer-name))))))

(define-derived-mode google-jump-to-tags-mode text-mode
  "GJTT"
  "Major mode for jumping to Google tags.")

(defun google-jump-to-tag-under-point ()
  "Perform google-find-tag under the point, without prompts."
  (interactive)
  (google-find-tag (google-find-default-tag)))

(defun gnuemacs-narrow-by-filename (regexp)
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (let* ((start-prop (overlay-start overlay))
           (tagrecord (get-text-property start-prop 'gtag))
           (filename (tagrecord-data-filename (tagrecord-data tagrecord))))
      (if (not (string-match regexp filename))
          (overlay-put overlay 'invisible t)))))

(defun xemacs-narrow-by-filename (regexp)
  (dolist (extent (extent-list))
    (let* ((startpos (extent-start-position extent))
           (endpos (extent-end-position extent))
           (tagrecord (get-text-property startpos 'gtag)))
      (if tagrecord
          (let ((filename (tagrecord-data-filename (tagrecord-data tagrecord))))
            (if (not (string-match regexp filename))
                (set-extent-property extent 'invisible t)))))))

(defun gnuemacs-show-all-overlays ()
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (overlay-put overlay 'invisible nil)))

(defun xemacs-show-all-overlays ()
  (dolist (extent (extent-list))
    (set-extent-property extent 'invisible nil)))

(defun google-narrow-by-filename (regexp)
  "Narrow the view to all files matching the provided regular expression"
  (interactive "sNarrow to filenames matching: ")
  (if (not (google-xemacs)) ;;; xemacs and gnu emacs are completely different
      (gnuemacs-narrow-by-filename regexp)
    (xemacs-narrow-by-filename regexp)))

(defun google-show-all-results ()
  "Undo the effects of a google-narrow-by-filename."
  (interactive)
  (if (not (google-xemacs))
      (gnuemacs-show-all-overlays)
    (xemacs-show-all-overlays)))

(define-key google-jump-to-tags-mode-map
  "\C-m"
  'google-show-tag-locations-under-point)
(if (google-xemacs)
    (define-key google-jump-to-tags-mode-map
      [button1]
      'google-show-tag-locations-under-point)
  (define-key google-jump-to-tags-mode-map
    [mouse-1]
    'google-show-tag-locations-under-point))
(define-key google-jump-to-tags-mode-map
  "q"
  'delete-window)
(define-key google-jump-to-tags-mode-map
  "h"
  'describe-mode)

(defun google-show-matching-tags (tagname)
  "Lists all tags matching the provided regexp"
  (interactive (google-find-tag-tag "Tag: "))
  (google-show-matching-tags-inner tagname
				   google-gtags-opcode-search-tag
				   gtags-server-buffer-name
				   nil))

(defun google-list-tags (filename)
  "List all the tags in `filename', defaulting to the one for the
current buffer."
  (interactive "fFilename: ")
  (let* ((pathname (google-remove-google3-prefix filename))
         (filename-only (file-name-nondirectory filename))
         (buffer-name (format "*TAGS: %s*" filename-only)))
  (google-show-matching-tags-inner pathname
				   google-gtags-opcode-search-tags-by-file
				   buffer-name
				   t)))

(defun google-show-matching-tags-inner (name
                                        opcode
                                        buffer-name
                                        use-tagrecords-p)
  (let* ((matches (google-get-tags (concat opcode name) nil (current-buffer)))
         (uniques (sort* (google-remove-association-dups matches)
                         #'string-lessp
                         :key #'tagrecord-tag))
         (temp-buffer-setup-hook 'google-jump-to-tags-mode))
    (with-output-to-temp-buffer buffer-name
      (set-buffer buffer-name)
      (if (google-xemacs)          ;; Must explicitly load mode in xemacs!
          (google-jump-to-tags-mode))
      (dolist (tagrecord uniques)
        (let ((start-point (point)))
          (princ (tagrecord-tag tagrecord))
          (if (google-xemacs)
              (let ((xemacs-region (make-extent start-point (point))))
                (set-extent-property xemacs-region
                                     'mouse-face
                                     'widget-button-face))
            (add-text-properties start-point
                                 (point)
                                 '(help-echo "mouse-1: go to this occurrence"
                                   mouse-face secondary-selection)))
          (if use-tagrecords-p
              (add-text-properties start-point (point) `(gtag ,tagrecord)))
          (princ "\n")))
      (switch-to-buffer-other-window buffer-name))))

(defun google-remove-google3-prefix (pathname)
  "Turn `pathname' into the form that the gtags server expects."
  (let* ((pathname-2 (expand-file-name pathname))
         (directory (file-name-directory pathname-2))
         (directory-components (remove* ""
                                        (split-string directory "/")
                                        :test #'equal))
         (google3-index (position "google3"
                                  directory-components
                                  :test #'string-equal))
         (components-sans-google3 (subseq directory-components
                                          (+ google3-index 1)))
         (directory-sans-prefix (mapconcat #'identity
                                           components-sans-google3
                                           "/")))
    (format "%s/%s"
            directory-sans-prefix
            (file-name-nondirectory pathname))))

(defun test-google-remove-google3-prefix ()
  "TODO(arthur): This could be used by a testing framework if we added one."
  (assert (string-equal
           (google-remove-google3-prefix "~/src/google3/java/foo/Bar.java")
           "java/foo/Bar.java")))

(defun google-first-tag (tagname)
  "Jump to the first match for `tagname' by talking to the google tag
server."
  (interactive (google-find-tag-tag "Tag: "))
  (let ((matches
         (remove-duplicates
          (google-get-matches tagname (current-buffer))
          :test #'equal)))
    (if (null matches)
        (progn
          (setq google-first-tag-matches nil)
          (error "No tag found!"))
      (setq google-first-tag-matches (cdr matches))
      (google-visit-tagrecord (car matches) t nil))))

(defun google-next-tag ()
  "Jump to the next tag in the results produced by `google-first-tag'."
  (interactive)
  (if google-first-tag-matches
      (let ((next-match (car google-first-tag-matches)))
        (setq google-first-tag-matches (cdr google-first-tag-matches))
        (google-visit-tagrecord next-match t nil))
    (error "No more matches found!")))

(defun google-pop-tag ()
  "Pop back up to the previous tag."
  (interactive)
  (if google-tags-history
      (let* ((google-tag-history-entry (car google-tags-history))
             (buffname (car google-tag-history-entry))
             (pt (cdr google-tag-history-entry)))
        (setq google-tags-history (cdr google-tags-history))
        (switch-to-buffer buffname)
        (goto-char pt))
    (error "No tag to pop!")))

(defun google-push-tag ()
  "Push current location onto the tag stack"
  (interactive)
  (let ((currloc (cons (buffer-name) (point))))
    (setq google-tags-history (cons currloc google-tags-history))))

(provide 'gtags)
