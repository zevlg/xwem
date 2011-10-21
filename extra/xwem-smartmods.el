;;; xwem-smartmods.el --- Smart modifiers for XWEM.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Dec 12 18:42:05 MSK 2003
;; Keywords: xwem, xlib
;; Time-stamp: <30/11/2006 00:03:22 lg@h1>

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

;; XWEM minor mode.

;;; Code:


;; Smart modifiers:

;;  smart modifiers are modifiers which can work as normal modifiers
;;  and as key if it is not used with some other key(when you just
;;  click[press and release modifier]).

(require 'xwem-load)
(require 'xlib-xtest)
(require 'xwem-keyboard)

(defcustom xwem-smart-mods-alist (list (cons XK-Control-L XK-Space))
  "*Alist of smart modifiers list in form \(MOD-SYM . KEY-SYM\)."
  :type '(repeat (cons number number))
  :group 'xwem-keyboard)

(defcustom xwem-sm-mode-line "SmartMods"
  "*String to show when smart modifiers mode is on."
  :type 'string
  :group 'xwem-modes)

;;; Internal variables

(defvar xwem-smart-modifiers nil
  "List of smart mods generated from `xwem-smart-mods-alist'.
Internal variable, do not modify.")

(defvar xwem-sm-global-mode nil
  "Non-nil mean smart modifiers mode enabled globally.
Use `xwem-sm-global-mode' to toggle global smart mods mode.")

(defvar xwem-sm-mode nil
  "Non-nil mean smart modifiers mode is enabled.
Use `xwem-sm-mode' command to toggle smart mods mode.")
(xwem-make-variable-client-local 'xwem-sm-mode)


;;; Functions
(defun xwem-sm-install-grab (&optional xwin)
  "Begin to grab smart modifiers."
  (unless xwin
    (setq xwin (xwem-rootwin)))
  (setq xwem-smart-modifiers
	(mapcar (lambda (sm)
		  (let ((rc (cons (car (xwem-kbd-xksym->xkcode (car sm)))
				  (car (xwem-kbd-xksym->xkcode (cdr sm))))))
		    (XGrabKey (xwem-dpy) (car rc) 0 xwin nil nil X-GrabModeSync)
		    rc))
		xwem-smart-mods-alist)))

(defun xwem-sm-uninstall-grab (&optional xwin)
  "Stop grabbing smart modifiers."
  (unless xwin
    (setq xwin (xwem-rootwin)))
  (mapc (lambda (sm)
	  (XUngrabKey (xwem-dpy) (car sm) 0 xwin))
	xwem-smart-modifiers))

(defun xwem-sm-init (&optional xwin)
  "Enter smartmods mode."
  (unless xwin
    (setq xwin (xwem-rootwin)))

  (xwem-sm-install-grab xwin)
  (X-Win-EventHandler-add-new xwin 'xwem-sm-keypress 0 (list X-KeyPress))

  (setq xwem-sm-mode 1))

(defun xwem-sm-fini (&optional xwin)
  "Exit smartmods mode."
  (unless xwin
    (setq xwin (xwem-rootwin)))

  (X-Win-EventHandler-rem xwin 'xwem-sm-keypress)
  (xwem-sm-uninstall-grab xwin)

  (setq xwem-sm-mode nil))

;; Event handlers
(defun xwem-sm-keypress (xdpy win xev)
  "Handle keypress of smart modifier."
  (let ((sm (assq (X-Event-xkey-keycode xev) xwem-smart-modifiers)))
    (when sm
      (XGrabKeyboard (xwem-dpy) (xwem-rootwin) nil
		     X-GrabModeSync X-GrabModeSync)
      (XAllowEvents (xwem-dpy) X-SyncBoth)

      ;; Skip non-keyboard/pointer events
      (setq xev (xwem-next-event nil
                                 (list X-KeyPress X-KeyRelease
                                       X-ButtonPress X-ButtonRelease
                                       X-MotionNotify)))

      (when (and (= (X-Event-type xev) X-KeyRelease)
		 (setq sm (assq (X-Event-xkey-keycode xev)
				xwem-smart-modifiers)))
	;; Send smart keycode
	(xwem-key-send-xtest-internal
	 (list (cons X-Xtest-KeyPress (cdr sm))
	       (cons X-Xtest-KeyRelease (cdr sm)))))

      (XAllowEvents (xwem-dpy) X-ReplayKeyboard)
      (XUngrabKeyboard (xwem-dpy))
      (XUngrabPointer (xwem-dpy))
      )))

;;;###autoload(autoload 'xwem-sm-mode "xwem-smartmods" nil t)
(define-xwem-command xwem-sm-mode (client &optional arg)
  "Toggle smart modifiers mode for CLIENT.
With positive ARG - turn it on.
With negative ARG - turn it off.
Without ARG - toggle."
  (xwem-interactive (list (xwem-cl-selected)
                          xwem-prefix-arg))

  (cond ((null arg)
         (if xwem-sm-mode
             (xwem-sm-fini (xwem-cl-xwin client))
           (xwem-sm-init (xwem-cl-xwin client))))
        ((and (> arg 0)
              (not xwem-sm-mode))
         (xwem-sm-init (xwem-cl-xwin client)))
        ((and (< arg 0)
              xwem-sm-mode)
         (xwem-sm-fini (xwem-cl-xwin client)))))

;;;###autoload(autoload 'xwem-sm-global-mode "xwem-smartmods" nil t)
(define-xwem-command xwem-sm-global-mode (arg)
  "Toggle global smart modifiers mode.
With positive ARG - turn it on.
With negative ARG - turn it off.
Without ARG - toggle."
  (xwem-interactive "P")

  (cond ((null arg)
         (if xwem-sm-mode
             (xwem-sm-fini)
           (xwem-sm-init)))
        ((and (> arg 0)
              (not xwem-sm-mode))
         (xwem-sm-init))
        ((and (< arg 0)
              xwem-sm-mode)
         (xwem-sm-fini))))


;;; On-load actions
(add-to-list 'xwem-minor-mode-alist
             '(xwem-sm-mode xwem-sm-mode-line))


(provide 'xwem-smartmods)

;;; xwem-smartmods.el ends here
