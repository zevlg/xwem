;;; xwem-compat.el --- GNU Emacs compatibility layer.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Dec 12 15:51:10 MSK 2003
;; Keywords: xwem, xlib
;; Time-stamp: <31/10/2007 04:08:31 lg@h1>

;; This file is part of XWEM.

;; XWEM is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XWEM is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; Compatibility layer betwean XEmacs and GNU Emacs.  However xwem
;; still can't run on GNU Emacs.

;;; Code:

(defvar xwem-gnuemacs-p (string-match "GNU Emacs" emacs-version)
  "Non-nil when running under GNU Emacs.")

(defun xwem-define-prefix-command (name &optional mapvar)
  "Compat version for `define-prefix-command'."
  (if xwem-gnuemacs-p
      (define-prefix-command name)
    (define-prefix-command name t)))

(unless (fboundp 'set-keymap-default-binding)
  (fset 'set-keymap-default-binding
	#'(lambda (keym cmd) (define-key keym [t] cmd))))

(unless (fboundp 'events-to-keys)
  (defalias 'events-to-keys 'identity))

(unless (fboundp 'define-specifier-tag)
  (defalias 'define-specifier-tag 'ignore))

(unless (fboundp 'find-face)
  (defalias 'find-face 'identity))

(unless (fboundp 'locate-data-directory)
  (defun locate-data-directory (name &optional dir-list)
    data-directory))

(unless (fboundp 'locate-data-directory-list)
  (defun locate-data-directory-list (name &optional dir-list)
    (list data-directory)))

(unless (boundp 'log-message-ignore-labels)
  (defvar log-message-ignore-labels nil))

(unless (fboundp 'display-message)
  (defun display-message (label message &optional frame stdout-p)
    (message "%s" message)))

(unless (fboundp 'put-alist)
  (defun put-alist (item value alist)
    (let ((pair (assoc item alist)))
      (if pair
          (progn
            (setcdr pair value)
            alist)
        (cons (cons item value) alist)
        ))))

(require 'cl)
(unless (fboundp 'print-object-ignoring-custom-print-functions)
  (defun print-object-ignoring-custom-print-functions (o s pl)
    (let ((custom-print-functions nil))
      (princ o s))))


(provide 'xwem-compat)

;;; xwem-compat.el ends here
