;;; xwem-root.el --- Root window and geom operations.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;; Created: 21 Mar 2003
;; Keywords: xlib, xwem
;; Time-stamp: <4/11/2007 02:18:03 lg@h1>

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
;;
;; This file used to manipulate and agragate information about root
;; window. Also has macros to work with geometry.
;;

;;; TODO:

;; * WM_ICON_SIZE on root window. (ICCCM 4.1.3.2)

;;; Code

(require 'xwem-load)
(require 'xwem-misc)

;;; Variables
(defconst xwem-root-ev-mask
  (Xmask-or XM-SubstructureRedirect
            XM-KeyPress XM-ButtonPress XM-ButtonRelease)
  "Event mask for X root window.")

(defgroup xwem-root nil
  "Group to customize root screen."
  :prefix "xwem-root-"
  :group 'xwem)

(defcustom xwem-root-cursor-shape '(X-XC-left_ptr)
  "*Cursors shape which will be used when pointer is over root window."
  :type (xwem-cursor-shape-choice)
  :set (xwem-cus-set-cursor-shape xwem-root-cursor (xwem-rootwin))
  :initialize 'custom-initialize-default
  :group 'xwem-root)

(defcustom xwem-root-cursor-foreground-color "white"
  "*Cursor's foreground color used when pointer is over root window."
  :type 'color
  :set (xwem-cus-set-cursor-foreground xwem-root-cursor)
  :initialize 'custom-initialize-default
  :group 'xwem-root)

(defcustom xwem-root-cursor-background-color "black"
  "*Cursor's background color used when pointer is over root window."
  :type 'color
  :set (xwem-cus-set-cursor-background xwem-root-cursor)
  :initialize 'custom-initialize-default
  :group 'xwem-root)

(defcustom xwem-root-another-wm-mode nil
  "*Non-nil mean try to start even if another WM is running.
EXPERIMENTAL, NOT TESTED, DOES NOT WORK, set to non-nil on your own risk."
  :type 'boolean
  :group 'xwem-root)

;;; Internal variables

(defvar xwem-root-cursor nil
  "Internal variable, stores root cursor.")


;;; Functions
(defun xwem-root-install-cursor ()
  "Install cursor on root window."
  ;; Create root cursor
  (setq  xwem-root-cursor
         (xwem-make-cursor xwem-root-cursor-shape
                           xwem-root-cursor-foreground-color
                           xwem-root-cursor-background-color))
  (XSetWindowCursor (xwem-dpy) (xwem-rootwin) xwem-root-cursor))

;;;###xwem-autoload
(defvar xwem-another-wm-mode nil
  "Non-nil mean another wm is running.")

(defun xwem-init-root-xerr-hook (xdpy xerr)
  (if (not xwem-root-another-wm-mode)
      (error 'xwem-error "Another window manager running")

    (setq xwem-another-wm-mode t)
    (XSelectInput (xwem-dpy) (xwem-rootwin)
                  (Xmask-and xwem-root-ev-mask
                             (lognot XM-SubstructureRedirect)))))

;;;###xwem-autoload
(defun xwem-init-root (host)
  "Initialization part for root."
  (setf (xwem-dpy) (XOpenDisplay host))

  ;; Select input on root window
  (pushnew 'xwem-init-root-xerr-hook (X-Dpy-error-hooks (xwem-dpy)))
  (XSelectInput (xwem-dpy) (xwem-rootwin) xwem-root-ev-mask)
  (XSync (xwem-dpy))
  (setf (X-Dpy-error-hooks (xwem-dpy))
        (delq 'xwem-init-root-xerr-hook (X-Dpy-error-hooks (xwem-dpy))))

  (X-Win-EventHandler-add-new (xwem-rootwin) 'xwem-root-events-handler 100)

  ;; Install grabbing
  (xwem-kbd-install-grab 'xwem-root-prefix (xwem-rootwin))

  (add-hook 'xwem-after-init-hook 'xwem-root-install-cursor))

;;;###autoload
(defun xwem-fini-root ()
  (XSetInputFocus (xwem-dpy) X-PointerRoot X-RevertToPointerRoot)
  ;; Reset deferring
  (setq xwem-deferred-dll (dll-create))
  (XCloseDisplay (xwem-dpy)))

;;;###autoload
(defun xwem-root-refresh (x y width height)
  "Refresh area WIDTHxHEIGHT+X+Y.
Probably will not work if backing store enabled in some window."
  (let ((wn (XCreateWindow
             (xwem-dpy) nil
             x y width height 0
             nil nil nil
             :override-redirect t)))
    (XMapWindow (xwem-dpy) wn)
    (XDestroyWindow (xwem-dpy) wn)))

;;;###autoload
(defun xwem-root-events-handler (xdpy xwin xev)
  "Events handler for root window."
  (xwem-debug 'xwem-root "Event: ev=%s win = %S"
              '(X-Event-name xev) '(X-Win-id (X-Event-win xev)))

  (X-Event-CASE xev
    ((:X-KeyPress :X-ButtonPress :X-ButtonRelease)
     (xwem-debug 'xwem-root "KeyButton event: parent win=%S, evname=%S"
                 '(X-Win-id (X-Event-win xev)) '(X-Event-name xev))
     (xwem-overriding-local-map 'xwem-root-prefix
       (xwem-dispatch-command-xevent xev)))

    (:X-MappingNotify
     ;; Modifiers mapping has been changed - reintialize
     (when (= (X-Event-xmapping-request xev) 0) ; Modifier
       (xwem-debug 'xwem-root "MappingNotify: reinitializing modifiers ..")
       (xwem-deferred-funcall 'xwem-kbd-initialize-modifiers)))

    (:X-ConfigureRequest
     ;; Some of root win clients issued XConfigureWindow
     (xwem-ev-reconfig xdpy xwin xev))

    (:X-MapRequest
     (xwem-debug 'xwem-root "MapRequest event: parent win=%S, window=%S"
                 '(X-Win-id (X-Event-xmaprequest-parent xev))
                 '(X-Win-id (X-Event-xmaprequest-window xev)))
     (xwem-ev-mapreq xdpy xwin xev))

    (:X-UnmapNotify
     (xwem-debug 'xwem-root "UnmapNotify event: event win=%S, window=%S"
                 '(X-Win-id (X-Event-xunmap-event xev))
                 '(X-Win-id (X-Event-xunmap-window xev)))
     (xwem-ev-unmap xdpy xwin xev))

    (:X-DestroyNotify
     (xwem-debug 'xwem-root "DestroyNotify event: parent win=%S, window=%S"
                 '(X-Win-id (X-Event-xdestroywindow-event xev))
                 '(X-Win-id (X-Event-xdestroywindow-window xev)))
     (xwem-ev-destroy xdpy xwin xev))
    ))


(provide 'xwem-root)

;;; xwem-root.el ends here
