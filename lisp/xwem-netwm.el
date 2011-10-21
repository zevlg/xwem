;;; xwem-netwm.el --- NETWM stuff.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;; Created: Sat May 15 19:44:58 MSD 2004
;; Keywords: xwem
;; Time-stamp: <28/8/2008 01:46:58 lg@h1.lan>

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

;; Support for NETWM hints.  See specification at:
;; http://www.freedesktop.org/standards/wm-spec/index.html.

;;; Code:

(require 'xlib-xlib)
(require 'xlib-xinerama)

(require 'xwem-load)
(require 'xwem-manage)
(require 'xwem-version)

(defgroup xwem-fullscreen nil
  "Group to customize fullscreen managing model."
  :prefix "xwem-fullscreen-"
  :group 'xwem-modes)

(defcustom xwem-fullscreen-switch-any-other nil
  "*Non-nil mean switch to any other client, if no other fullscreen client available."
  :type 'boolean
  :group 'xwem-fullscreen)

(defcustom xwem-fullscreen-iconify-when-inactive t
  "*Non-nil mean iconify fullscreen clients, when they gets inactive/deselected."
  :type 'boolean
  :group 'xwem-fullscreen)

(defcustom xwem-fullscreen-ai-rank '(1000 . 0)
  "*Always on top rank pair for active and inactive states."
  :type '(cons :tag "Rank pair"
               (number :tag "Active rank")
               (number :tag "Inactive rank"))
  :group 'xwem-fullscreen)

(defcustom xwem-fullscreen-client-properties nil
  "*Plist of properties for fullscreen clients."
  :type '(restricted-sexp :match-alternatives (valid-plist-p))
  :group 'xwem-fullscreen)

;;; Internal variables

;;; Constants
(defconst _NET_WM_NAME "_NET_WM_NAME")
(defconst _NET_WM_VISIBLE_NAME "_NET_WM_VISIBLE_NAME")
(defconst _NET_WM_ICON_NAME "_NET_WM_ICON_NAME")
(defconst _NET_WM_VISIBLE_ICON_NAME "_NET_WM_VISIBLE_ICON_NAME")
(defconst _NET_WM_DESKTOP "_NET_WM_DESKTOP")
(defconst _NET_WM_WINDOW_TYPE "_NET_WM_WINDOW_TYPE")
(defconst _NET_WM_WINDOW_TYPE_DESKTOP "_NET_WM_WINDOW_TYPE_DESKTOP")
(defconst _NET_WM_WINDOW_TYPE_DOCK "_NET_WM_WINDOW_TYPE_DOCK")
(defconst _NET_WM_WINDOW_TYPE_TOOLBAR "_NET_WM_WINDOW_TYPE_TOOLBAR")
(defconst _NET_WM_WINDOW_TYPE_MENU "_NET_WM_WINDOW_TYPE_MENU")
(defconst _NET_WM_WINDOW_TYPE_UTILITY "_NET_WM_WINDOW_TYPE_UTILITY")
(defconst _NET_WM_WINDOW_TYPE_SPLASH "_NET_WM_WINDOW_TYPE_SPLASH")
(defconst _NET_WM_WINDOW_TYPE_DIALOG "_NET_WM_WINDOW_TYPE_DIALOG")
(defconst _NET_WM_WINDOW_TYPE_NORMAL "_NET_WM_WINDOW_TYPE_NORMAL")
(defconst _NET_WM_STATE "_NET_WM_STATE")
(defconst _NET_WM_STATE_MODAL "_NET_WM_STATE_MODAL")
(defconst _NET_WM_STATE_STICKY "_NET_WM_STATE_STICKY")
(defconst _NET_WM_STATE_MAXIMIZED_VERT "_NET_WM_STATE_MAXIMIZED_VERT")
(defconst _NET_WM_STATE_MAXIMIZED_HORZ "_NET_WM_STATE_MAXIMIZED_HORZ")
(defconst _NET_WM_STATE_SHADED "_NET_WM_STATE_SHADED")
(defconst _NET_WM_STATE_SKIP_TASKBAR "_NET_WM_STATE_SKIP_TASKBAR")
(defconst _NET_WM_STATE_SKIP_PAGER "_NET_WM_STATE_SKIP_PAGER")
(defconst _NET_WM_STATE_HIDDEN "_NET_WM_STATE_HIDDEN")
(defconst _NET_WM_STATE_FULLSCREEN "_NET_WM_STATE_FULLSCREEN")
(defconst _NET_WM_STATE_ABOVE "_NET_WM_STATE_ABOVE")
(defconst _NET_WM_STATE_BELOW "_NET_WM_STATE_BELOW")
(defconst _NET_WM_ALLOWED_ACTIONS "_NET_WM_ALLOWED_ACTIONS")
(defconst _NET_WM_ACTION_MOVE "_NET_WM_ACTION_MOVE")
(defconst _NET_WM_ACTION_RESIZE "_NET_WM_ACTION_RESIZE")
(defconst _NET_WM_ACTION_MINIMIZE "_NET_WM_ACTION_MINIMIZE")
(defconst _NET_WM_ACTION_SHADE "_NET_WM_ACTION_SHADE")
(defconst _NET_WM_ACTION_STICK "_NET_WM_ACTION_STICK")
(defconst _NET_WM_ACTION_MAXIMIZE_HORZ "_NET_WM_ACTION_MAXIMIZE_HORZ")
(defconst _NET_WM_ACTION_MAXIMIZE_VERT "_NET_WM_ACTION_MAXIMIZE_VERT")
(defconst _NET_WM_ACTION_FULLSCREEN "_NET_WM_ACTION_FULLSCREEN")
(defconst _NET_WM_ACTION_CHANGE_DESKTOP "_NET_WM_ACTION_CHANGE_DESKTOP")
(defconst _NET_WM_ACTION_CLOSE "_NET_WM_ACTION_CLOSE")
(defconst _NET_WM_STRUT "_NET_WM_STRUT")
(defconst _NET_WM_ICON_GEOMETRY "_NET_WM_ICON_GEOMETRY")
(defconst _NET_WM_ICON "_NET_WM_ICON")
(defconst _NET_WM_PID "_NET_WM_PID")
(defconst _NET_WM_HANDLED_ICONS "_NET_WM_HANDLED_ICONS")
(defconst _NET_WM_STRUT "_NET_WM_STRUT")

(defconst _NET_SUPPORTED "_NET_SUPPORTED")
(defconst _NET_SUPPORTING_WM_CHECK "_NET_SUPPORTING_WM_CHECK")
(defconst _NET_CURRENT_DESKTOP "_NET_CURRENT_DESKTOP")
(defconst _NET_SHOWING_DESKTOP "_NET_SHOWING_DESKTOP")
(defconst _NET_NUMBER_OF_DESKTOPS "_NET_NUMBER_OF_DESKTOPS")
(defconst _NET_DESKTOP_GEOMETRY "_NET_DESKTOP_GEOMETRY")
(defconst _NET_ACTIVE_WINDOW "_NET_ACTIVE_WINDOW")
(defconst _NET_DESKTOP_NAMES "_NET_DESKTOP_NAMES")
(defconst _NET_CLIENT_LIST "_NET_CLIENT_LIST")
(defconst _NET_CLIENT_LIST_STACKING "_NET_CLIENT_LIST_STACKING")

(defconst UTF8_STRING "UTF8_STRING")

(defconst xwem-nwm-atom-names
 (list UTF8_STRING
       _NET_WM_NAME _NET_WM_VISIBLE_NAME _NET_WM_ICON_NAME
       _NET_WM_VISIBLE_ICON_NAME _NET_WM_DESKTOP _NET_WM_WINDOW_TYPE
       _NET_WM_WINDOW_TYPE_DESKTOP _NET_WM_WINDOW_TYPE_DOCK
       _NET_WM_WINDOW_TYPE_TOOLBAR _NET_WM_WINDOW_TYPE_MENU
       _NET_WM_WINDOW_TYPE_UTILITY _NET_WM_WINDOW_TYPE_SPLASH
       _NET_WM_WINDOW_TYPE_DIALOG _NET_WM_WINDOW_TYPE_NORMAL _NET_WM_STATE
       _NET_WM_STATE_MODAL _NET_WM_STATE_STICKY _NET_WM_STATE_MAXIMIZED_VERT
       _NET_WM_STATE_MAXIMIZED_HORZ _NET_WM_STATE_SHADED
       _NET_WM_STATE_SKIP_TASKBAR _NET_WM_STATE_SKIP_PAGER
       _NET_WM_STATE_HIDDEN _NET_WM_STATE_FULLSCREEN _NET_WM_STATE_ABOVE
       _NET_WM_STATE_BELOW _NET_WM_ALLOWED_ACTIONS _NET_WM_ACTION_MOVE
       _NET_WM_ACTION_RESIZE _NET_WM_ACTION_MINIMIZE _NET_WM_ACTION_SHADE
       _NET_WM_ACTION_STICK _NET_WM_ACTION_MAXIMIZE_HORZ
       _NET_WM_ACTION_MAXIMIZE_VERT _NET_WM_ACTION_FULLSCREEN
       _NET_WM_ACTION_CHANGE_DESKTOP _NET_WM_ACTION_CLOSE _NET_WM_STRUT
       _NET_WM_ICON_GEOMETRY _NET_WM_ICON _NET_WM_PID _NET_WM_HANDLED_ICONS
       _NET_WM_STRUT

       _NET_CURRENT_DESKTOP
       _NET_SHOWING_DESKTOP
       _NET_SUPPORTING_WM_CHECK
       _NET_NUMBER_OF_DESKTOPS
       _NET_DESKTOP_GEOMETRY
       _NET_ACTIVE_WINDOW))

(defconst xwem-nwm-supported
  (list _NET_WM_NAME _NET_CURRENT_DESKTOP
        _NET_NUMBER_OF_DESKTOPS
        _NET_DESKTOP_NAMES _NET_SHOWING_DESKTOP
        _NET_WM_STATE _NET_WM_STATE_FULLSCREEN
        _NET_CLIENT_LIST _NET_CLIENT_LIST_STACKING
        _NET_ACTIVE_WINDOW
        )
  )

(defun xwem-nwm-init ()
  "Initialize netwm stuff."
  (xwem-message 'init "Initializing NET_WM support ...")

  (mapc #'(lambda (name)
            (XInternAtom (xwem-dpy) name))
        xwem-nwm-atom-names)

  ;; Add client message handler on root window
  (X-Win-EventHandler-add-new (xwem-rootwin) 'xwem-nwm-root-evhandler 100
                              (list X-ClientMessage X-MapRequest))
  (X-Dpy-EventHandler-add (xwem-dpy) 'xwem-nwm-root-clnmsg 100
                          (list X-ClientMessage))
;  ;; Update root event mask
;  (setq xwem-root-ev-mask (Xmask-or xwem-root-ev-mask XM-
;  (XSelectInput (xwem-dpy)

  ;; Add hooks
  (add-hook 'xwem-frame-select-hook 'xwem-nwm-on-frame-select)
  (add-hook 'xwem-frame-creation-hook 'xwem-nwm-set-number-of-desktops)
  (add-hook 'xwem-frame-destroy-hook 'xwem-nwm-set-number-of-desktops)

  (add-hook 'xwem-cl-create-hook 'xwem-nwm-set-client-list)
  (add-hook 'xwem-cl-destroy-hook 'xwem-nwm-set-client-list)
  (add-hook 'xwem-client-select-hook 'xwem-nwm-set-active-window)

  ;; Set some properties
  (xwem-nwm-set-supported)
  (xwem-nwm-set-supporting-wm-check)

  (xwem-nwm-set-number-of-desktops)
  (xwem-nwm-set-current-desk)
  (xwem-nwm-set-desk-geometry)

  (xwem-nwm-set-client-list)

  (xwem-message 'init "Initializing NET_WM support ... done"))

(defun xwem-nwm-on-frame-select ()
  "Called when frame switching occurs, i.e. member of `xwem-frame-select-hook'."
  (xwem-nwm-set-current-desk (xwem-frame-selected)))

(defun xwem-nwm-root-evhandler (xdpy xwin xev)
  "Handle netwm's event"
  (X-Event-CASE xev
    (:X-ClientMessage
     (xwem-nwm-root-clnmsg xdpy xwin xev))
    (:X-MapRequest
     (xwem-nwm-root-mapreq xdpy xwin xev))))

(defun xwem-nwm-root-clnmsg (xdpy xwin xev)
  "Dispatch ClientMessage event on root window."
  (cond ((string= (X-Atom-name xdpy (X-Event-xclient-atom xev))
                  _NET_CURRENT_DESKTOP)
         (let ((num (truncate (caar (X-Event-xclient-msg xev)))))
           (xwem-frame-switch-nth num)))

        ((string= (X-Atom-name xdpy (X-Event-xclient-atom xev)) _NET_WM_STATE)
         (let ((vop (caar (X-Event-xclient-msg xev)))
               (stype (caadr (X-Event-xclient-msg xev)))
               (cl (xwem-xwin-cl xwin)))

           ;; XXX Can handle only FULLSCREEN state
           (when (= stype (X-Atom-id (XInternAtom
                                      (xwem-dpy) _NET_WM_STATE_FULLSCREEN)))
             (cond ((= vop 0)
                    (xwem-toggle-fullscreen cl 'off))
                   ((= vop 1)
                    (xwem-toggle-fullscreen cl 'on))))))
        ))

(defun xwem-nwm-root-mapreq (xdpy xwin xev)
  "Dispatch map request of XWIN to set _NET_WM_STATE property."
  (XChangeProperty xdpy xwin
                   (XInternAtom xdpy _NET_WM_STATE)
                   XA-atom X-format-32 X-PropModeReplace
                   nil)
  )

(defun xwem-nwm-set-state (xwin &optional state)
  "Set XWIN's _NET_WM_STATE property to STATE.
If STATE is nil, then remove property."
  (XChangeProperty (xwem-dpy) xwin
                   (XInternAtom (xwem-dpy) _NET_WM_STATE)
                   XA-atom X-format-32 X-PropModeReplace
                   (if state
                       (list (XInternAtom (xwem-dpy) state))
                     nil)))

(defun xwem-nwm-set-supported (&rest notused)
  "Set _NET_SUPPORTED root window property."
  (XChangeProperty (xwem-dpy) (xwem-rootwin)
                   (XInternAtom (xwem-dpy) _NET_SUPPORTED)
                   XA-atom X-format-32 X-PropModeReplace
                   (mapcar #'(lambda (name)
                               (XInternAtom (xwem-dpy) name))
                           xwem-nwm-supported)))

(defun xwem-nwm-set-number-of-desktops (&rest notused)
  "Set _NET_NUMBER_OF_DESKTOPS."
  (XChangeProperty (xwem-dpy) (xwem-rootwin)
                   (XInternAtom (xwem-dpy) _NET_NUMBER_OF_DESKTOPS)
                   XA-cardinal X-format-32 X-PropModeReplace
                   (list (length (xwem-frames-list 'desktop)))))

(defun xwem-nwm-set-desk-geometry (&optional frame &rest notused)
  "Set _NET_DESKTOP_GEOMETRY."
  (unless frame
    (setq frame (xwem-frame-selected)))
  (when (xwem-frame-p frame)
    (XChangeProperty (xwem-dpy) (xwem-rootwin)
                     (XInternAtom (xwem-dpy) _NET_DESKTOP_GEOMETRY)
                     XA-cardinal X-format-32 X-PropModeReplace
                     (list (xwem-frame-width frame)
                           (xwem-frame-height frame)))))

(defun xwem-nwm-set-current-desk (&optional frame &rest notused)
  "Set _NET_CURRENT_DESKTOP."
  (unless frame
    (setq frame (xwem-frame-selected)))

  (when (xwem-frame-p frame)
    (XChangeProperty (xwem-dpy) (xwem-rootwin)
                     (XInternAtom (xwem-dpy) _NET_CURRENT_DESKTOP)
                     XA-cardinal X-format-32 X-PropModeReplace
                     (list (xwem-frame-num frame)))))

(defun xwem-nwm-set-showing-desk (&optional frame what &rest notused)
  "Set _NET_SHOWING_DESKTOP."
  (unless frame
    (setq frame (xwem-frame-selected)))

  (when (xwem-frame-p frame)
    (XChangeProperty (xwem-dpy) (xwem-rootwin)
                     (XInternAtom (xwem-dpy) _NET_CURRENT_DESKTOP)
                     XA-cardinal X-format-32 X-PropModeReplace
                     (list what))))

(defun xwem-nwm-set-active-window ()
  "Set _NET_ACTIVE_WINDOW."
  (XChangeProperty (xwem-dpy) (xwem-rootwin)
                   (XInternAtom (xwem-dpy) _NET_ACTIVE_WINDOW)
                   XA-window X-format-32 X-PropModeReplace
                   (list (and (xwem-cl-p (xwem-cl-selected))
                              (X-Win-id (xwem-cl-xwin (xwem-cl-selected)))))))

(defun xwem-nwm-set-wm-window-type (thing)
  "Set _NET_WM_WINDOW_TYPE."
  (let (xwin type)
    (setq type
          (cond ((and (X-Win-p thing)
                      (xwem-tray-find-dapp thing))
                 (setq xwin thing)
                 _NET_WM_WINDOW_TYPE_DOCK)

                ((xwem-frame-p thing)
                 (setq xwin (xwem-frame-xwin thing))
                 _NET_WM_WINDOW_TYPE_DESKTOP)

                ((xwem-cl-p thing)
                 (setq xwin (xwem-cl-xwin thing))
                 _NET_WM_WINDOW_TYPE_NORMAL)

                ((X-Win-p thing)
                 (setq xwin thing)
                 _NET_WM_WINDOW_TYPE_DIALOG)))

    (XChangeProperty (xwem-dpy) xwin
                     (XInternAtom (xwem-dpy) _NET_WM_WINDOW_TYPE)
                     XA-atom X-format-32 X-PropModeReplace
                     (list (XInternAtom (xwem-dpy) type)))
    ))

(defun xwem-nwm-set-supporting-wm-check (&rest unused)
  "Set _NET_SUPPORTING_WM_CHECK."
  (XChangeProperty (xwem-dpy) (xwem-rootwin)
                   (XInternAtom (xwem-dpy) _NET_SUPPORTING_WM_CHECK)
                   XA-window X-format-32 X-PropModeReplace
                   (list (xwem-minib-xwin xwem-minibuffer)))

  (XChangeProperty (xwem-dpy) (xwem-minib-xwin xwem-minibuffer)
                   (XInternAtom (xwem-dpy) _NET_SUPPORTING_WM_CHECK)
                   XA-window X-format-32 X-PropModeReplace
                   (list (xwem-minib-xwin xwem-minibuffer)))

;  (XChangeProperty (xwem-dpy) (xwem-minib-xwin xwem-minibuffer)
;                   (XInternAtom (xwem-dpy) _NET_WM_NAME)
;                   (XInternAtom (xwem-dpy) UTF8_STRING)
;                   X-format-32 X-PropModeReplace
;                   (list xwem-version))
  )

(defun xwem-nwm-set-client-list (&rest notused)
  "Set _NET_CLIENT_LIST and _NET_CLIENT_LIST_STACKING."
  (XChangeProperty (xwem-dpy) (xwem-rootwin)
                   (XInternAtom (xwem-dpy) _NET_CLIENT_LIST)
                   XA-window X-format-32 X-PropModeReplace
                   (mapcar 'xwem-cl-xwin xwem-clients))

  ;; TODO: order
  (XChangeProperty (xwem-dpy) (xwem-rootwin)
                   (XInternAtom (xwem-dpy) _NET_CLIENT_LIST_STACKING)
                   XA-window X-format-32 X-PropModeReplace
                   (mapcar 'xwem-cl-xwin xwem-clients)))

;;;; ---- Fullscreen manage methods ----
;;

;;; Fullscreen major mode

;; Supported CL properties:

;;    `fs-real-size'           - Do not resize client, use its size.

;;    `fs-avoid-minib-overlap' - Resize client to fullscreen avoiding
;;                               xwem minibuffer overlaping.

(defvar xwem-fullscreen-mode-hook nil
  "*Hooks to call when client enters fullscreen mode.
Called with one argument - client.")

(defvar xwem-fullscreen-mode-map
  (let ((map (make-sparse-keymap 'XWEM-fullscreen-map)))
    (define-key map (kbd "C-S-<button1>") 'xwem-client-imove)
    (define-key map (kbd "C-S-<button3>") 'xwem-client-iresize)
    map)
  "Keymap for fullscreen clients.")

;; Properties
(define-xwem-client-property fs-real-size fullscreen
  "Non-nil to manage fullscreen clients in their real sizes.
I.e. no resize to fullfill screen."
  :type 'boolean
  :set 'xwem-fullscreen-set-fs-real-size)

(define-xwem-client-property fs-avoid-minib-overlap fullscreen
  "Non-nil to not overlap xwem minibuffer."
  :type 'boolean
  :set 'xwem-fullscreen-set-fs-avoid-minib-overlap)

(defun xwem-fullscreen-set-fs-real-size (cl prop val)
  "Set `fs-real-size' property."
  (xwem-cl-put-prop cl prop val)

  (if val
      (let ((ig (xwem-cl-initial-xgeom cl)))
        (xwem-client-move-resize cl (X-Geom-x ig) (X-Geom-y ig)
                                 (X-Geom-width ig) (X-Geom-height ig)))
    (xwem-refit cl)))

(defun xwem-fullscreen-set-fs-avoid-minib-overlap (cl prop val)
  "Set `fs-avoid-minib-overlap' property."
  (xwem-cl-put-prop cl prop val)

  (xwem-refit cl))

(defun xwem-netwm-fullscreen-p (cl)
  "Return non-nil if CL is very like to be managed at fullscreen mode."
  (let ((states (cddr (XGetWindowProperty (xwem-dpy) (xwem-cl-xwin cl)
                        (XInternAtom (xwem-dpy) _NET_WM_STATE)))))
    (member (X-Atom-id (XInternAtom (xwem-dpy) _NET_WM_STATE_FULLSCREEN))
            states)))

(defun xwem-cl-fullscreen-p (cl)
  "Return non-nil if CL is managed using fullscreen manage model."
  (eq (xwem-cl-manage-type cl) 'fullscreen))

;;;###autoload(autoload 'xwem-fullscreen-mode "xwem-netwm" nil t)
(define-xwem-command xwem-fullscreen-mode (cl)
  "Toggle fullscreen major mode for selected client CL.
It is an alias for `xwem-toggle-fullscreen' command."
  (xwem-interactive (list (xwem-cl-selected)))

  (xwem-toggle-fullscreen cl))

;;;###autoload
(defun xwem-manage-fullscreen (cl)
  "Manage method for fullscreen client CL."
  ;; Find out the place where to manage client
  (let ((tpnt (car (XTranslateCoordinates
                    (xwem-dpy) (xwem-cl-xwin cl)
                    (xwem-rootwin) 0 0)))
        (xin (X-XIneramaQueryScreens (xwem-dpy)))
        (rx 0) (ry 0))
    ;; Xinerama stuff
    (when (car xin)
      ;; XInerama enabled
      (while (setq xin (cdr xin))
        (when (and (>= (X-Point-x tpnt) (X-Rect-x (car xin)))
                   (<= (X-Point-x tpnt)
                       (+ (X-Rect-x (car xin)) (X-Rect-width (car xin))))
                   (>= (X-Point-y tpnt) (X-Rect-y (car xin)))
                   (<= (X-Point-y tpnt)
                       (+ (X-Rect-y (car xin)) (X-Rect-height (car xin)))))
          (setq rx (X-Rect-x (car xin))
                ry (X-Rect-y (car xin))
                xin nil))))

    (XReparentWindow (xwem-dpy) (xwem-cl-xwin cl) (xwem-rootwin) rx ry))

  ;; Set geometry to initial
  (setf (xwem-cl-new-xgeom cl)
        (copy-X-Geom (xwem-cl-initial-xgeom cl)))
  (setf (X-Geom-border-width (xwem-cl-new-xgeom cl)) nil)
  (xwem-refit cl)

  ;; Setup fullscreen client CL
  (xwem-use-local-map xwem-fullscreen-mode-map cl)
  (xwem-select-client cl)

  (run-hook-with-args 'xwem-fullscreen-mode-hook cl))

;;;###autoload(put 'manage 'fullscreen 'xwem-manage-fullscreen)

(defun xwem-fullscreen-refit-full (cl)
  "Refit CL to fullscreen."
  (let* ((tpnt (car (XTranslateCoordinates (xwem-dpy) (xwem-cl-xwin cl)
                                           (xwem-rootwin) 0 0)))
         (crect (make-X-Rect :x (X-Point-x tpnt) :y (X-Point-y tpnt)
                             :width (X-Geom-width (xwem-cl-xgeom cl))
                             :height (X-Geom-height (xwem-cl-xgeom cl))))
         (xin (X-XIneramaQueryScreens (xwem-dpy))))
    (if (car xin)
        (progn
          ;; XInerama enabled
          (while (and (setq xin (cdr xin))
                      (not (X-Rect-intersect-p (car xin) crect))))
          (setq xin (X-Rect-to-X-Geom (car xin))))

      ;; No XInerama, so use root geometry
      (setq xin (xwem-rootgeom)))

    ;; Update geometry to not overlap xwem minibuffer, if
    ;; 'no-minib-overlap CL property is set.
    (when (xwem-client-property cl 'fs-avoid-minib-overlap)
      (setf (X-Geom-height xin)
            (- (X-Geom-height xin)
               (X-Geom-height (xwem-minib-xgeom xwem-minibuffer)))))

    (setf (X-Geom-border-width xin)
          (X-Geom-border-width (xwem-cl-xgeom cl)))

    ;; Update CL geometry
    (xwem-cl-correct-size-for-size cl xin)))

(defun xwem-fullscreen-refit-real (cl)
  "Refit to real CL's size."
  (xwem-cl-apply-new-xgeom cl))

(defun xwem-refit-fullscreen (cl)
  "Refit method for fullscreen client CL."
  (cond ((xwem-client-property cl 'fs-real-size)
         (xwem-fullscreen-refit-real cl))
        (t (xwem-fullscreen-refit-full cl)))

  (xwem-cl-apply-xgeom cl))

(define-xwem-deferred xwem-fullscreen-apply-state (cl)
  "Apply CL's state to life."
  (when (xwem-cl-p cl)
    (case (xwem-cl-state cl)
      (active
       (setf (xwem-xwin-rank (xwem-cl-xwin cl))
             (car xwem-fullscreen-ai-rank))
       (XMapWindow (xwem-dpy) (xwem-cl-xwin cl)))

      (inactive
       (setf (xwem-xwin-rank (xwem-cl-xwin cl))
             (cdr xwem-fullscreen-ai-rank))
       (when (and xwem-fullscreen-iconify-when-inactive
                  ;; XXX skip dummy clients and special
                  (not (or (xwem-dummy-client-p (xwem-cl-selected))
                           (xwem-special-p (xwem-cl-selected)))))
         (xwem-iconify cl)))

      (iconified
       (setf (xwem-xwin-rank (xwem-cl-xwin cl))
             (cdr xwem-fullscreen-ai-rank))
       (XUnmapWindow (xwem-dpy) (xwem-cl-xwin cl))))))

(defun xwem-activate-fullscreen (cl &optional type)
  "Activate method for fullscreen client CL."
  (xwem-fullscreen-apply-state cl))

(defun xwem-deactivate-fullscreen (cl &optional type)
  "Deactivate fullscreen client CL."
  (cond ((eq type 'deactivate)
         (xwem-fullscreen-apply-state cl))

        ((eq type 'deselect)
         (when (xwem-cl-active-p cl)
           (xwem-client-change-state cl 'inactive))
         (xwem-fullscreen-apply-state cl))))

(defun xwem-iconify-fullscreen (cl)
  "Iconify method for fullscreen client CL."
  (xwem-fullscreen-apply-state cl))

;;; Additional methods
(define-xwem-method other-client fullscreen (cl)
  "Return fullscreen client other then CL."
  (or (xwem-cl-other cl :clients (xwem-clients-list 'xwem-cl-fullscreen-p))
      (and xwem-fullscreen-switch-any-other
	   (xwem-cl-other cl))))

;;;; ---- Fullscreen Commands ----

;;;###autoload(autoload 'xwem-toggle-fullscreen "xwem-netwm" nil t)
(define-xwem-command xwem-toggle-fullscreen (cl &optional force)
  "Enable/disable fullscreen mode for CL.
Optional argument FORCE is one of:
  `on'  - Force fullscreen.
  `off' - Force non-fullscreen."
  (xwem-interactive (list (xwem-cl-selected)))

  (let ((xwin (xwem-cl-xwin cl)))
    (cond ((or (eq force 'on)
               (and (null force)
                    (or (not (xwem-cl-fullscreen-p cl))
                        (and (xwem-cl-fullscreen-p cl)
                             (xwem-client-property cl 'fs-real-size)))))
           (xwem-client-set-property cl 'fs-real-size nil)
           (xwem-nwm-set-state xwin _NET_WM_STATE_FULLSCREEN)
           (xwem-client-change-manage-type cl '(fullscreen)))

          ((or (eq force 'off)
               (and (null force)
                    (xwem-cl-fullscreen-p cl)
                    (not (xwem-client-property cl 'fs-real-size))))
           (xwem-client-set-property cl 'fs-real-size t)
           (xwem-nwm-set-state xwin nil)
           (xwem-client-change-manage-type cl (xwem-manda-find-match cl))))))

;;;###autoload(autoload 'xwem-switch-to-fullscreen-cl "xwem-netwm" nil t)
(define-xwem-command xwem-switch-to-fullscreen-cl ()
  "Switch to client that in fullscreen mode."
  (xwem-interactive)

  (let* ((fsclients (xwem-clients-list 'xwem-cl-fullscreen-p))
         (cl (and fsclients (xwem-read-client "Fullscreen CL: " fsclients))))

    (unless (xwem-cl-alive-p cl)
      (error 'xwem-error "No fullscreen clients"))

    (xwem-select-client cl)))


(provide 'xwem-netwm)
;;;; On-load actions

;; Fullscreen manage type
(define-xwem-manage-model fullscreen
  "Managing model to show client at fullscreen size."
  :qualifier '(predicate xwem-netwm-fullscreen-p)
  :manage-properties '(omit-aspect-ratio t) ; disregard aspect ratio

  :cl-properties xwem-fullscreen-client-properties
  :manage-method 'xwem-manage-fullscreen
  :activate-method 'xwem-activate-fullscreen
  :deactivate-method 'xwem-deactivate-fullscreen
  :refit-method 'xwem-refit-fullscreen
  :iconify-method 'xwem-iconify-fullscreen)

(if xwem-started
    (xwem-nwm-init)
  (add-hook 'xwem-after-init-hook 'xwem-nwm-init))

;;; xwem-netwm.el ends here
