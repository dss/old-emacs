;;;; Copyright 2005 Google Inc.

;;;; Emacs Utilities for Google

;;; Cache results of function calls.  Note that this does not notice
;;; when the function or one of the functions it calls changes.
;;;
;;; To use this, do: (memoize 'foo)
;;;
;;; To turn the cache off, do: (unmemoize 'foo)

(require 'cl)

(defmacro memoize (function-name)
  "Cache results of calls to `function-name'.  Note that this does not
notice when the function or one of the functions it calls changes."
  (let ((function-name (eval function-name)))
    `(progn
       (put ',function-name 'memoized-value nil)
       (defadvice ,function-name (around memoize activate)
         "Memoize the results of the most recent call when all arguments
match.  Does not notice when the function itself or one of the
functions it calls changes."
         (let ((arguments (ad-get-args 0))
               (memoized-value (get ',function-name 'memoized-value)))
           (if (and memoized-value
                    (equal arguments
                           (car memoized-value)))
               (setq ad-return-value
                     (cdr memoized-value))
             (let ((new-result ad-do-it))
               (put ',function-name
                    'memoized-value
                    (cons arguments new-result)))))))))

(defun unmemoize (function-name)
  (interactive "aFunction: ")
  (ad-unadvise function-name)
  (put function-name 'memoized-value nil))

;;; Filters, etc.

(defun google-filter (pred list)
  "Return a list consisting of only those elements x of LIST for which
 (PRED x) is non-nil.  Use 'identity' predicate to remove nil elements."
  (let ((result ()))
    (dolist (item list)
      (if (funcall pred item)
          (setq result (cons item result))))
    (nreverse result)))

(defun google-remove-association-dups (alist)
  (let ((result nil))
    (dolist (association alist)
      (if (not (assoc (car association) result))
          (setq result (cons association result))))
    ;;; The above process reversed the results,
    ;;; so now we reverse it back.
    (reverse result)))

;;; Processes

(defun google-find-buffer-with-prefix (prefix)
  "Find some buffer whose name starts with `prefix' and which has an
active process.  This function is usually used under the assumption
that there is only one such buffer, or that the user will notice if
the wrong one is chosen."
  (let ((prefix-length (length prefix)))
    (some
     (lambda (buffer)
       (let ((name (buffer-name buffer)))
         (and (eq t (compare-strings name
                                     0
                                     prefix-length
                                     prefix
                                     0
                                     prefix-length))
              (let ((process
                     (find buffer (process-list) :key #'process-buffer)))
                (and process
                     (eq (process-status process) 'run)
                     buffer)))))
     (buffer-list))))

(provide 'google-emacs-utilities)
