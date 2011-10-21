;;; xwem-recover.el --- Autorecovery tool for xwem.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sat Sep 11 23:20:23 GMT 2004
;; Keywords: xlib, xwem
;; Time-stamp: <30/11/2006 00:01:18 lg@h1>

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

;; Sometimes xlib desinchronises with X server.  In such circumstatces
;; only restarting helps.  This tool tries to recover xlib from
;; desinchronisation.  Desync may occur because XEmacs can block
;; whenever he want, and there no visible way to control it.

;; xwem-recover installs x error hooks and if many x errors occurs in
;; a little time it starts recovering routines.

;; Any time you are feeling that something wrong, you can use
;; `xwem-recover-do-recover' command to force recovering.

;; Use xwem-recover only if you are sure and you know what you are
;; doing.

;;; Code:

(require 'xwem-load)

(defgroup xwem-recover nil
  "Group to customize xwem recovering tool."
  :prefix "xwem-recover-"
  :group 'xwem)

(defcustom xwem-recover-parameter '(12 . 3)
  "*How many errors allowed without recovering.
car specifies number of errors, cdr specifies time in seconds."
  :type '(cons number number)
  :group 'xwem-recover)

;;; Internal variables

(defvar xwem-recover-mode nil
  "Non-nil mean we are in recovering mode.
Use `xwem-recover-turn-on', `xwem-recover-turn-off' and
`xwem-recover-toggle' to change mode.")

(defvar xwem-recover-errors nil
  "List of times when X error occurs.
Internal variable.")


(define-xwem-deferred xwem-recover-real-recover ()
  "Do real recovering routines."
  (setf (X-Dpy-snd-queue (xwem-dpy)) nil)
  (setf (X-Dpy-message-buffer (xwem-dpy)) "")
  (setf (X-Dpy-evq (xwem-dpy)) nil))

;;;###autoload(autoload 'xwem-recover-do-recover "xwem-recover" "" t)
(define-xwem-command xwem-recover-do-recover (xdpy)
  "Recover XDPY from desinchronisation with X server."
  (xwem-interactive (list (xwem-dpy)))

  (flet ((old-x-dpy-filter (proc out)))
    (fset 'old-x-dpy-filter (symbol-function 'X-Dpy-filter))
    (flet ((X-Dpy-filter (proc out)
	     ;; Skip any data on XDPY, but continue processing on
	     ;; other displays.
	     (unless (eq (X-Dpy-proc xdpy) proc)
	       (old-x-dpy-filter proc out))))
      (while (accept-process-output (X-Dpy-proc xdpy) 2))

      ;; At this point all pending readed, so do cleanup things.  This
      ;; is not 100% will work.  In some circumstances this will only
      ;; add problems.
      (xwem-recover-real-recover))))

(defun xwem-recover-xerr-hook (xdpy xerr)
  "Called when on display XDPY X error XERR occured.
Check excedance of `xwem-recover-parameter' and if it seems like xlib
got desinchronised with X server, start recovering routines."

  (let ((ct (current-time)))
    (setq xwem-recover-errors (nreverse xwem-recover-errors))
    (while (and xwem-recover-errors
		(> (itimer-time-difference ct (car xwem-recover-errors))
		   (cdr xwem-recover-parameter)))
      (setq xwem-recover-errors (cdr xwem-recover-errors)))
    (setq xwem-recover-errors
	  (cons ct (nreverse xwem-recover-errors)))

    ;; Check (car xwem-recover-errors) is not exceeded
    (when (or (> (length xwem-recover-errors) (car xwem-recover-parameter))
              ;; Also recover when error code isn't recognized
              (not (memq (X-Event-xerror-code xerr)
                         '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 128 255))))
      (xwem-message 'alarm "Recovering from desinchronisation .. (errors = %d)\n"
		    (length xwem-recover-errors))
      (xwem-recover-do-recover xdpy))))

;;;###autoload(autoload 'xwem-recover-turn-on "xwem-recover" "" t)
(define-xwem-command xwem-recover-turn-on ()
  "Enable xwem recovering mode."
  (xwem-interactive)

  (unless xwem-recover-mode
    (pushnew 'xwem-recover-xerr-hook (X-Dpy-error-hooks (xwem-dpy)))
    (setq xwem-recover-mode t)))

;;;###autoload(autoload 'xwem-recover-turn-off "xwem-recover" "" t)
(define-xwem-command xwem-recover-turn-off ()
  "Turn off xwem recovering mode."
  (xwem-interactive)

  (when xwem-recover-mode
    (setf (X-Dpy-error-hooks (xwem-dpy))
	  (delq 'xwem-recover-xerr-hook (X-Dpy-error-hooks (xwem-dpy))))
    (setq xwem-recover-mode nil)))

;;;###autoload(autoload 'xwem-recover-toggle "xwem-recover" "" t)
(define-xwem-command xwem-recover-toggle (arg)
  "Toggle xwem recovering mode.
With positive ARG turn it on, with negative turn it off.
If ARG is ommited - toggle it."
  (xwem-interactive "P")

  (cond ((null arg)
	 (if xwem-recover-mode
	     (xwem-recover-turn-off)
	   (xwem-recover-turn-on)))
	((< (prefix-numeric-value arg) 0)
	 (xwem-recover-turn-off))
	(t (xwem-recover-turn-on))))


(provide 'xwem-recover)

;;; xwem-recover.el ends here
