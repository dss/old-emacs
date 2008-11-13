;;; w32-browser.el --- Run Windows application associated with a file.
;;
;; Filename: w32-browser.el
;; Description: Run Windows application associated with a file.
;; Author: Emacs Wiki, Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2004-2007, Drew Adams, all rights reserved.
;; Created: Thu Mar 11 13:40:52 2004
;; Version: 21.0
;; Last-Updated: Fri Jan 19 21:32:40 2007 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 131
;; URL: http://www.emacswiki.org/cgi-bin/wiki/w32-browser.el
;; Keywords: mouse, dired, w32, explorer
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `cl', `custom', `dired', `dired+', `dired-aux',
;;   `dired-x', `easymenu', `ediff-diff', `ediff-help', `ediff-init',
;;   `ediff-merg', `ediff-mult', `ediff-util', `ediff-wind',
;;   `fit-frame', `frame-cmds', `frame-fns', `info', `info+',
;;   `misc-fns', `mkhtml', `mkhtml-htmlize', `strings', `thingatpt',
;;   `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Run Windows application associated with a file.
;;
;; `w32-browser' & `dired-w32-browser' are from the Emacs Wiki.
;;
;; I modified `w32-browser' to `find-file' if it cannot
;; `w32-shell-execute'.  I modified `dired-multiple-w32-browser' to
;; use `w32-browser-wait-time'.  I wrote `dired-mouse-w32-browser'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/01/02 dadams
;;     Added: w32-browser-wait-time, soft require of dired+.el.
;;     Uncommented and updated dired-multiple-w32-browser and its binding.
;;     Thanks to Mathias Dahl [brakjoller@gmail.com] for recognizing this actually works.
;;     Conditionalized dired+ vs standard dired in bindings.
;; 2005/11/05 dadams
;;     Renamed menu-bar-dired-immediate-menu to diredp-menu-bar-immediate-menu.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'dired+ nil t) ;; diredp-menu-bar-immediate-menu, diredp-menu-bar-operate-menu

;;;;;;;;;;;;;;;;;;;;;;

(when (eq system-type 'windows-nt)

  (defcustom w32-browser-wait-time 0.1
    "*Delay to wait between `w32-browser' in `dired-multiple-w32-browser'.
On at least some Windows systems, this delay is needed between calls
to `w32-browser' within command `dired-multiple-w32-browser'.
Depending on your system, you might be able to set this to 0, meaning
no wait."
    :type 'integer :group 'convenience)

  (defun w32-browser (file)
    "Run default Windows application associated with FILE.
If no associated application, then `find-file' FILE."
    (or (condition-case nil
            (w32-shell-execute nil file) ; Use Windows file association
          (error nil))
        (find-file file)))              ; E.g. no Windows file association

  (defun dired-w32-browser ()
    "Run default Windows application associated with current line's file.
If file is a directory, then `dired-find-file' instead.
If no application is associated with file, then `find-file'."
    (interactive)
    (let ((file (dired-get-filename)))
      (if (file-directory-p file)
          (dired-find-file)
        (w32-browser (dired-replace-in-string "/" "\\" file)))))

  (defun dired-mouse-w32-browser (event)
    "Run default Windows application associated with file under mouse click.
If file is a directory or no application is associated with file, then
`find-file' instead."
    (interactive "e")
    (let (file)
      (save-excursion
        (set-buffer (window-buffer (posn-window (event-end event))))
        (save-excursion
          (goto-char (posn-point (event-end event)))
          (setq file (dired-get-filename))))
      (select-window (posn-window (event-end event)))
      (if (file-directory-p file)
          (find-file (file-name-sans-versions file t))
        (w32-browser (file-name-sans-versions file t)))))

  (defun dired-multiple-w32-browser ()
    "Run default Windows applications associated with marked files."
    (interactive)
    (let ((files (dired-get-marked-files)))
      (while files
        (w32-browser (car files))
        (sleep-for w32-browser-wait-time)
        (setq files (cdr files)))))

  (if (boundp 'diredp-menu-bar-immediate-menu) ; Use Dired+ if loaded.
      (eval-after-load "dired+"
        '(progn
          (define-key dired-mode-map [f3] 'dired-w32-browser)
          (define-key diredp-menu-bar-immediate-menu [dired-w32-browser]
            '("Open Associated Application" . dired-w32-browser))
          (define-key dired-mode-map [mouse-2] 'dired-mouse-w32-browser)
          (define-key diredp-menu-bar-operate-menu [dired-w32-browser]
            '("Open Associated Applications" . dired-multiple-w32-browser))))
    (eval-after-load "dired"
      '(progn
        (define-key dired-mode-map [f3] 'dired-w32-browser)
        (define-key dired-mode-map [menu-bar immediate dired-w32-browser]
          '("Open Associated Application" . dired-w32-browser))
        (define-key dired-mode-map [mouse-2] 'dired-mouse-w32-browser)
        (define-key dired-mode-map [menu-bar immediate dired-w32-browser]
          '("Open Associated Applications" . dired-multiple-w32-browser))))))

;;;;;;;;

(provide 'w32-browser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; w32-browser.el ends here
