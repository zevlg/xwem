;;; xwem-tray.el --- Tray support for XWEM.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: 1 Sep 2003
;; Keywords: xlib, xwem
;; Time-stamp: <30/7/2008 07:12:28 lg@h1.lan>

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
;; We should implement something like dockapp handler(or system tray),
;; that will be placed on free space of xwem-minibuffer or xwem-frame
;; and handle external X applications. It may receive some
;; ClientMessages and process them. Some of this ClientMessage should
;; be used to run elisp code.
;;
;; See how mbdock from matchbox made.
;;
;; xwem tray creates fake window which is only used to hold selection
;; needed for communication, xwem minibuffer window will be used for
;; holding apps.
;;
;;; TODO:
;;    - Proper possition in `xwem-minibuffer' calculation.
;;    - Run elisp support(almost already done).
;;    - Grouping  (yet EXPERIMENTAL)

;;; Code:

(require 'xlib-tray)
(require 'xwem-load)
(require 'xwem-help)
(require 'xwem-manage)

;;; xwem tray constants
(defconst xwem-tc-dock-req 0 "Dock place request.")
(defconst xwem-tc-message 1 "Message from dock app.")
(defconst xwem-tc-cancel-message 2 "Cancels message.")
(defconst xwem-tc-run-lisp 3 "Evaluate emacs lisp string")

(defvar xwem-tray-message-hook 'xwem-tray-message-defhook
  "*Hook to be called whin new message from dock app.
Function will be called with arg - dockapp.")

(defvar xwem-tray-id 0 "System tray identificator.")

(defconst xwem-tray-evmask (Xmask-or XM-SubstructureNotify
                                     XM-Exposure
                                     XM-StructureNotify
                                     XM-SubstructureRedirect
                                     XM-PropertyChange
                                     XM-ButtonPress
                                     XM-ButtonRelease))

;;; Configuration for xwem system tray
(defgroup xwem-tray nil
  "Group to customize XWEM system tray."
  :prefix "xwem-tray-"
  :group 'xwem)

(defcustom xwem-tray-use-groups nil
  "*Non-nil mean systray with use EXPERIMENTAL dockapp grouping.
Set it to non-nil on your own risk.
DOES NOT WORK."
  :type 'boolean
  :group 'xwem-tray)

(defcustom xwem-tray-default-align 'right
  "*Position in minibuffer where dockapps will placed."
  :type '(choice (const :tag "At Right" right)
                 (const :tag "At Left" left))
  :group 'xwem-tray)

(defcustom xwem-tray-minib-start-offset 4
  "*Offset in pixels from the minibuffer edge where first dockapp is placed."
  :type 'number
  :group 'xwem-tray)

(defcustom xwem-tray-minib-dock-offset 5
  "*Offset in pixels between dockapps."
  :type 'number
  :group 'xwem-tray)

(defcustom xwem-tray-groups-distance 5
  "*Minimum distance between systray groups.
Only when `xwem-tray-use-groups' is non-nil."
  :type 'number
  :group 'xwem-tray)

(defcustom xwem-tray-cursor-shape '(X-XC-right_ptr)
  "*Cursor shape which will be used when pointer is over dock app."
  :type (xwem-cursor-shape-choice)
  :set (xwem-cus-set-cursor-shape
        xwem-tray-cursor
        (and (xwem-tray-p xwem-tray) (xwem-tray-xwin xwem-tray)))
  :initialize 'custom-initialize-default
  :group 'xwem-tray)

(defcustom xwem-tray-cursor-foreground-color "#000075"
  "*Cursor's foreground color used when poniter is on dock app."
  :type 'color
  :set (xwem-cus-set-cursor-foreground xwem-tray-cursor)
  :initialize 'custom-initialize-default
  :group 'xwem-tray)

(defcustom xwem-tray-cursor-background-color "#ffffff"
  "*Cursor's background color used when poniter is on dock app."
  :type 'color
  :set (xwem-cus-set-cursor-background xwem-tray-cursor)
  :initialize 'custom-initialize-default
  :group 'xwem-tray)

(define-xwem-face xwem-tray-group-face
  `(((light) (:foreground "white"))
    ((medium) (:foreground "gray50"))
    ((dark) (:foreground "black")))
  "*Face to outline dockapp groups."
  :group 'xwem-tray
  :group 'xwem-faces)

;;; Delimiter stuff
(defcustom xwem-tray-delimiter-width 4
  "*Delimiter width."
  :type 'number
  :group 'xwem-tray)

(defcustom xwem-tray-delimiter-height-reminder 2
  "*How many pixels on top/bottom from xwem minibuffer.."
  :type 'number
  :group 'xwem-tray)

(define-xwem-face xwem-tray-delimiter-face
  `(((background light)
     (:foreground "gray40"))
    ((background light shadow)
     (:foreground "gray30"))
    ((background dark)
     (:foreground "gray70"))
    ((background dark shadow)
     (:foreground "gray80"))
    (t (:foreground "gray55")))
  "Face to draw systray delimiter."
  :group 'xwem-tray
  :group 'xwem-faces)

;;; Internal variables

(defvar xwem-tray-groups '("desktop"  "launch" "misc" "default")
  "List of predefined dockapp groups.")

(defconst xwem-tray-align-left 1)
(defconst xwem-tray-align-right 2)

;;; Internal variables
(defvar xwem-tray nil
  "Default xwem system tray.")

(defvar xwem-tray-cursor nil
  "Cursor used when pointer is over dock app.")
(defvar xwem-tray-curroffset 0
  "Current offset in pixels.")

;;; Dock applications
;;
;; Dock is array in form:
;;  [x-window geom-after-reparent]
(defvar xwem-tray-dapp-list nil "List of dockapp X windows.")

;; System tray
(defstruct xwem-tray
  xwin                                  ; Tray's X window
  xgeom                                 ; Tray's X geometry
  atoms

  groups                                ; list of groups
  dockapps                              ; compatible with old style
  plist)                                ; tray properties

(defmacro xwem-tray-rem-prop (tray prop)
  "From TRAY's property list, remove property PROP."
  `(setf (xwem-tray-plist ,tray)
         (plist-remprop (xwem-tray-plist ,tray) ,prop)))
(defmacro xwem-tray-put-prop (tray prop val)
  "In TRAY's plist put PROP with VAL."
  `(if ,val
       (setf (xwem-tray-plist ,tray)
             (plist-put (xwem-tray-plist ,tray) ,prop ,val))
     (xwem-tray-rem-prop ,tray ,prop)))
(defmacro xwem-tray-get-prop (tray prop)
  "Get TRAY's property PROP."
  `(plist-get (xwem-tray-plist ,tray) ,prop))

(defstruct xwem-tray-group
  name                                  ; group name
  align                                 ; group align
  xwin                                  ; group window
  xgeom                                 ; group window geometry

  dockapps
  plist)                                ; properties

;; Dockapp structure
(defstruct xwem-dapp
  xwin
  geom

  group                                 ; group
  plist                                 ; dockapp properties

  ;; for xembed messaging
  mess-type
  mess-waitlen
  mess-currlen
  mess)

(defmacro xwem-dapp-alive-p (dapp)
  "Return non-nil if DAPP is alive dock application."
  `(and (xwem-dapp-p ,dapp)
        (memq ,dapp xwem-tray-dapp-list)))

(defmacro xwem-dapp-rem-prop (dapp prop)
  `(setf (xwem-dapp-plist ,dapp)
         (plist-remprop (xwem-dapp-plist dapp) ,prop)))

(defmacro xwem-dapp-put-prop (dapp prop val)
  `(if ,val
       (setf (xwem-dapp-plist ,dapp)
             (plist-put (xwem-dapp-plist ,dapp) ,prop ,val))
     (xwem-dapp-rem-prop ,dapp ,prop)))

(defmacro xwem-dapp-get-prop (dapp prop)
  `(plist-get (xwem-dapp-plist ,dapp) ,prop))

(defmacro xwem-dapp-group-name (dapp)
  `(xwem-dapp-get-prop ,dapp 'group))
(defsetf xwem-dapp-group-name (dapp) (group)
  `(xwem-dapp-put-prop ,dapp 'group ,group))

(defmacro xwem-dapp-id (dapp)
  `(xwem-dapp-get-prop ,dapp 'id))
(defsetf xwem-dapp-id (dapp) (id)
  `(xwem-dapp-put-prop ,dapp 'id ,id))

(defmacro xwem-dapp-align (dapp)
  `(xwem-dapp-get-prop ,dapp 'align))
(defsetf xwem-dapp-align (dapp) (align)
  `(xwem-dapp-put-prop ,dapp 'align ,align))

(defmacro xwem-dapp-state (dapp)
  `(xwem-dapp-get-prop ,dapp 'state))
(defsetf xwem-dapp-state (dapp) (state)
  `(xwem-dapp-put-prop ,dapp 'state ,state))

;; Message is vector in form:
;;  [message-type message-waitlen message-currlen message-string]

;; message-type is one of `xwem-tc-message', `xwem-tc-cancel-message'
;; or `xwem-tc-run-lisp'.

;;; Functions

;;{{{  [-] Groups

(defun xwem-tray-group-get-position (group)
  "Get good position in system tray for tray GROUP."
  (let ((groups (xwem-tray-groups xwem-tray))
        (dtlen xwem-tray-minib-start-offset))
    (while groups
      (when (eq (xwem-tray-group-align (car groups))
                (xwem-tray-group-align group))
        (incf dtlen (X-Geom-width (xwem-tray-group-xgeom (car groups))))
        (incf dtlen xwem-tray-groups-distance))
      (setq groups (cdr groups)))

    (ecase (xwem-tray-group-align group)
      (right
       (- (X-Geom-width (xwem-minib-xgeom xwem-minibuffer))
          dtlen (X-Geom-width (xwem-tray-group-xgeom group))))
      (left
       (+ (X-Geom-width (xwem-minib-cl-xgeom xwem-minibuffer))
          dtlen)))))

(defun xwem-tray-group-find (name)
  "Search for tray group with NAME."
  (let ((groups (xwem-tray-groups xwem-tray)))
    (while (and groups
                (not (string= (xwem-tray-group-name (car groups)) name)))
      (setq groups (cdr groups)))
    (car groups)))

(defun xwem-tray-group-create (name &rest params)
  "Create and return new systray group, giving it NAME.
Additional PARAMS can be specified."
  (let ((xtg (apply #'make-xwem-tray-group :name name params)))
    (unless (xwem-tray-group-align xtg)
      (setf (xwem-tray-group-align xtg)
            xwem-tray-default-align))
    (unless (xwem-tray-group-xgeom xtg)
      (setf (xwem-tray-group-xgeom xtg)
            (make-X-Geom :x 0 :y 0 :width 16
                         :height 32 :border-width 1)))

    (setf (X-Geom-x (xwem-tray-group-xgeom xtg))
          (xwem-tray-group-get-position xtg))
    (setf (xwem-tray-group-xwin xtg)
          (XCreateWindow
           (xwem-dpy) (xwem-tray-xwin xwem-tray)
           (X-Geom-x (xwem-tray-group-xgeom xtg))
           (X-Geom-y (xwem-tray-group-xgeom xtg))
           (X-Geom-width (xwem-tray-group-xgeom xtg))
           (X-Geom-height (xwem-tray-group-xgeom xtg))
           (X-Geom-border-width (xwem-tray-group-xgeom xtg))
           nil nil nil
           :override-redirect t
           :background-pixel
           (X-Gc-foreground
            (xwem-face-get-gc 'xwem-tray-group-face '(medium)))
           :event-mask (Xmask-or XM-ButtonPress
                                 XM-ButtonRelease)))

    ;; Add to tray's groups
    (setf (xwem-tray-groups xwem-tray)
          (cons xtg (xwem-tray-groups xwem-tray)))

    ;; And show the window
    (XMapWindow (xwem-dpy) (xwem-tray-group-xwin xtg))
    xtg))

(defun xwem-tray-group-same-align-sorted (group)
  "Return list of groups with same align as GROUP."
  (sort (delq nil (mapcar #'(lambda (g)
                              (and (eql (xwem-tray-group-align g)
                                        (xwem-tray-group-align g)) g))
                          (xwem-tray-groups xwem-tray)))
        #'(lambda (g1 g2)
            (> (X-Geom-x (xwem-tray-group-xgeom g1))
               (X-Geom-x (xwem-tray-group-xgeom g2))))))

(defun xwem-tray-group-resize (group new-width new-height)
  "Resize tray GROUP to NEW-WIDTH and NEW-HEIGHT."
  (let ((glist (xwem-tray-group-same-align-sorted group))
        (off (- new-width
                (X-Geom-width (xwem-tray-group-xgeom group)))))
    (if (eq (xwem-tray-group-align group) 'left)
        (setq glist (cdr (memq group glist)))
      (setq glist (memq group (nreverse glist))
            off (- off)))

    (mapc #'(lambda (g)
              (incf (X-Geom-x (xwem-tray-group-xgeom g)) off)
              (XMoveWindow (xwem-dpy) (xwem-tray-group-xwin g)
                           (X-Geom-x (xwem-tray-group-xgeom g))
                           (X-Geom-y (xwem-tray-group-xgeom g))))
          glist)
    (setf (X-Geom-width (xwem-tray-group-xgeom group)) new-width)
    (XResizeWindow (xwem-dpy) (xwem-tray-group-xwin group)
                   (X-Geom-width (xwem-tray-group-xgeom group))
                   (X-Geom-height (xwem-tray-group-xgeom group)))
    ))

(defun xwme-tray-group-repositionate-dapps (group)
  "Repositionate GROUP's dock applications."
  (let ((gdapps (xwem-tray-group-dockapps group))
        (step 3)
        (coff 3))
    (while gdapps
      (setf (X-Geom-x (xwem-dapp-geom (car gdapps))) coff)
      ;; XXX do it deferring
      (XMoveWindow (xwem-dpy) (xwem-dapp-xwin (car gdapps))
                   (X-Geom-x (xwem-dapp-geom (car gdapps)))
                   (X-Geom-y (xwem-dapp-geom (car gdapps))))
      (incf coff (X-Geom-width (xwem-dapp-geom (car gdapps))))
      (incf coff step)
      (setq gdapps (cdr gdapps)))

    (when (> coff (X-Geom-width (xwem-tray-group-xgeom group)))
      (xwem-tray-group-resize
       group coff
       (X-Geom-height (xwem-tray-group-xgeom group))))))

(defun xwem-tray-group-attach-dapp (group dapp)
  "To tray GROUP attach dock application DAPP."
  (let ((gdapps (xwem-tray-group-dockapps group)))
    (XReparentWindow (xwem-dpy) (xwem-dapp-xwin dapp)
                     (xwem-tray-group-xwin group) 0 0)
    (while (and gdapps
                (> (xwem-dapp-id (car gdapps))
                   (xwem-dapp-id dapp)))
      (setq gdapps (cdr gdapps)))
    (if (not gdapps)
        (setf (xwem-tray-group-dockapps group)
              (append (xwem-tray-group-dockapps group)
                      (list dapp)))
      (setcdr gdapps (cons (car gdapps) (cdr gdapps)))
      (setcar gdapps dapp))

    (xwme-tray-group-repositionate-dapps group)))

(defun xwem-tray-group-find-create (name)
  "Find existing or create new group."
  (or (xwem-tray-group-find name)
      (xwem-tray-group-create name)))

;;}}}

;;;###xwem-autoload
(defun xwem-XTrayInit (xdpy xwin &optional dockid dockgroup dockalign)
  "Same as `XTrayInit'.
You should use this function instead of direct calls to `XTrayInit',
because in time you doing it xwem-tray may be uninitialised."
  ;; Import dapp properties
  (XChangeProperty xdpy xwin (XInternAtom xdpy "XWEM_DOCK_ID")
                   XA-integer X-format-16 X-PropModeReplace
                   (list (or dockid 0)))
  (XChangeProperty xdpy xwin (XInternAtom xdpy "XWEM_DOCK_GROUP")
                   XA-string X-format-8 X-PropModeReplace
                   (or dockgroup "default"))
  (XChangeProperty xdpy xwin (XInternAtom xdpy "XWEM_DOCK_ALIGN")
                   XA-integer X-format-16 X-PropModeReplace
                   (list (or dockalign xwem-tray-align-right)))

  (XTrayInit xdpy xwin))

;;;###xwem-autoload
(defun xwem-tray-find-dapp (xwin)
  "Finds dock application by X window XWIN."
  (car (member* xwin xwem-tray-dapp-list
                :test #'(lambda (xwin dapp)
                          (X-Win-equal xwin (xwem-dapp-xwin dapp))))))

(defun xwem-tray-message-defhook (dapp)
  "Default function for message from dock apps handling."
  (if (featurep 'xwem-special)
      (xwem-help-display "tray message"
        (insert (xwem-dapp-mess dapp)))
    (xwem-message 'error "Message arrived from dock app, but special frames not enabled.")))

;;; XXX these three functions:
;;
;;  - xwem-tray-remove-dapp
;;  - xwem-tray-hide-dapp
;;  - xwem-tray-show-dapp
;;
;; Has many of common code, get rid of it --lg

(defun xwem-tray-move-many-dapps (dapp-list x-offset x-lim)
  "Move dockapps in DAPP-LIST."
  (mapc #'(lambda (dapp)
            (ecase xwem-tray-default-align
              (right
               (when (< (X-Geom-x (xwem-dapp-geom dapp)) x-lim)
                 (xwem-tray-move-dapp
                  dapp (+ (X-Geom-x (xwem-dapp-geom dapp)) x-offset) nil)))

              (left
               (when (> (X-Geom-x (xwem-dapp-geom dapp)) x-lim)
                 (xwem-tray-move-dapp
                  dapp (- (X-Geom-x (xwem-dapp-geom dapp)) x-offset) nil)))))
        dapp-list))

(defun xwem-tray-remove-dapp (dapp)
  "Remove dock application DAPP from xwem tray dockapps list."
  (let ((dgeom (xwem-dapp-geom dapp))
        (state (xwem-dapp-state dapp)))
    ;; Remove from dapps list
    (setq xwem-tray-dapp-list
          (delq dapp xwem-tray-dapp-list))
    (X-invalidate-cl-struct dapp)

    ;; Move other dapps to fill free space
    (unless (eq state 'hidden)
      (xwem-tray-move-many-dapps
       xwem-tray-dapp-list (+ (X-Geom-width dgeom) xwem-tray-minib-dock-offset)
       (X-Geom-x dgeom)))))

(defun xwem-tray-hide-dapp (hide-dapp &optional unmap-p)
  "Hide dockapp DAPP temporary.
Non-nil UNMAP-P mean dockapp already unmapped."
  (unless (eq (xwem-dapp-state hide-dapp) 'hidden)
    (unless unmap-p
      (XUnmapWindow (xwem-dpy) (xwem-dapp-xwin hide-dapp)))
    (setf (xwem-dapp-state hide-dapp) 'hidden)

    (let ((dgeom (xwem-dapp-geom hide-dapp)))
      (mapc #'(lambda (dapp)
                (unless (eq dapp hide-dapp)
                  (ecase xwem-tray-default-align
                    (right
                     (when (< (X-Geom-x (xwem-dapp-geom dapp))
                              (X-Geom-x dgeom))
                       (xwem-tray-move-dapp dapp
                                            (+ (X-Geom-x (xwem-dapp-geom dapp))
                                               (X-Geom-width dgeom)
                                               xwem-tray-minib-dock-offset)
                                            nil)))

                    (left
                     (when (> (X-Geom-x (xwem-dapp-geom dapp))
                              (X-Geom-x dgeom))
                       (xwem-tray-move-dapp dapp
                                            (- (X-Geom-x (xwem-dapp-geom dapp))
                                               (X-Geom-width dgeom)
                                               xwem-tray-minib-dock-offset)
                                            nil))))))
            xwem-tray-dapp-list))))

(defun xwem-tray-show-dapp (show-dapp &optional map-p)
  "Show dockapp SHOW-DAPP that was hidden temporary.
Non-nil MAP-P mean dock app already mapped."
  (unless (eq (xwem-dapp-state show-dapp) 'shown)
    (unless map-p
      (XMapWindow (xwem-dpy) (xwem-dapp-xwin show-dapp)))
    (setf (xwem-dapp-state show-dapp) 'shown)

    (let ((dgeom (xwem-dapp-geom show-dapp)))
      (mapc #'(lambda (dapp)
                (unless (eq dapp show-dapp)
                  (ecase xwem-tray-default-align
                    (right
                     (when (< (X-Geom-x (xwem-dapp-geom dapp))
                              (+ (X-Geom-x dgeom) (X-Geom-width dgeom)))
                       (xwem-tray-move-dapp dapp
                                            (- (X-Geom-x (xwem-dapp-geom dapp))
                                               (X-Geom-width dgeom)
                                               xwem-tray-minib-dock-offset)
                                            nil)))

                    (left
                     (when (> (X-Geom-x (xwem-dapp-geom dapp))
                              (+ (X-Geom-x dgeom) (X-Geom-width dgeom)))
                       (xwem-tray-move-dapp dapp
                                            (+ (X-Geom-x (xwem-dapp-geom dapp))
                                               (X-Geom-width dgeom)
                                               xwem-tray-minib-dock-offset)
                                            nil))))))
            xwem-tray-dapp-list))))

(defun xwem-tray-get-proper-position (width)
  "Get good position in system tray for dapp with WIDTH."
  (let ((dapps xwem-tray-dapp-list)
        (dtlen xwem-tray-minib-start-offset))
    (while dapps
      (when (eq (xwem-dapp-state (car dapps)) 'shown)
        (incf dtlen (X-Geom-width (xwem-dapp-geom (car dapps))))
        (incf dtlen xwem-tray-minib-dock-offset))
      (setq dapps (cdr dapps)))

    (ecase xwem-tray-default-align
      (right
       (- (X-Geom-width (xwem-minib-xgeom xwem-minibuffer))
          dtlen width))
      (left
       (+ (X-Geom-width (xwem-minib-cl-xgeom xwem-minibuffer))
          dtlen)))))

(define-xwem-deferred xwem-tray-apply-dapp-x-position (dapp)
  "Apply DAPP's X position to life."
  (when (xwem-dapp-alive-p dapp)
    (XConfigureWindow (xwem-dpy) (xwem-dapp-xwin dapp)
                      :x (X-Geom-x (xwem-dapp-geom dapp)))))

(define-xwem-deferred xwem-tray-apply-dapp-y-position (dapp)
  "Apply DAPP's Y position."
  (when (xwem-dapp-alive-p dapp)
    (XConfigureWindow (xwem-dpy) (xwem-dapp-xwin dapp)
                      :y (X-Geom-y (xwem-dapp-geom dapp)))))

(defun xwem-tray-move-dapp (dapp new-x new-y)
  "Move DAPP to NEW-X, NEW-Y position.
If NEW-X or NEW-Y is nil - corresponding value is retained."
  (when new-x
    (setf (X-Geom-x (xwem-dapp-geom dapp)) new-x)
    (xwem-tray-apply-dapp-x-position dapp))
  (when new-y
    (setf (X-Geom-y (xwem-dapp-geom dapp)) new-y)
    (xwem-tray-apply-dapp-y-position dapp)))

(defun xwem-tray-new-dapp (xwin)
  "New dock application XWIN wants to be managed."
  (let* ((minb-hei (X-Geom-height (xwem-minib-xgeom xwem-minibuffer)))
         (wgeom (XGetGeometry (xwem-dpy) xwin))
         (w-wid (X-Geom-width wgeom))
         (w-hei (X-Geom-height wgeom))
         (dapp-geom 
          (progn
            ;; Fix w-wid and w-hei keeping aspect ratio
            (when (> w-hei minb-hei)
              (let* ((n-hei (- minb-hei 8)) ; XXX 8
                     (rat (/ (float n-hei) w-hei)))
                (setq w-hei n-hei
                      w-wid (int (* rat w-wid)))))
            (make-X-Geom
                     :x (xwem-tray-get-proper-position w-wid)
                     :y (/ (- minb-hei w-hei) 2)
                     :width w-wid :height w-hei)))
         (dapp (make-xwem-dapp :xwin xwin :geom dapp-geom)))

    ;; Set DAPP's window gravity
    (XChangeWindowAttributes (xwem-dpy) xwin
                             :win-gravity
                             (if (eq xwem-tray-default-align 'right)
                                 X-EastGravity
                               X-WestGravity))

    ;; Dockapp properties
    (setf (xwem-dapp-id dapp)
          (nth 2 (XGetWindowProperty
                  (xwem-dpy) xwin (XInternAtom (xwem-dpy) "XWEM_DOCK_ID"))))
    (setf (xwem-dapp-group-name dapp)
          (XGetPropertyString (xwem-dpy) xwin
                              (XInternAtom (xwem-dpy) "XWEM_DOCK_GROUP")))
    (setf (xwem-dapp-align dapp)
          (nth 2 (XGetWindowProperty
                  (xwem-dpy) xwin (XInternAtom (xwem-dpy) "XWEM_DOCK_ALIGN"))))

    (add-to-list 'xwem-tray-dapp-list dapp)

    (X-Win-EventHandler-add-new xwin 'xwem-dapp-handle-xevent
                                100 (list X-ClientMessage X-ConfigureNotify))

    (if xwem-tray-use-groups
        (xwem-tray-group-attach-dapp
         (xwem-tray-group-find-create (xwem-dapp-group-name dapp))
         dapp)
      (XReparentWindow (xwem-dpy) xwin (xwem-tray-xwin xwem-tray)
                       (X-Geom-x dapp-geom)
                       (X-Geom-y dapp-geom)))
    (XMapWindow (xwem-dpy) xwin)
    (setf (xwem-dapp-state dapp) 'shown)))

(defun xwem-dapp-handle-client-message (xev)
  "Handle ClientMessage from dock application."
  (xwem-debug 'xwem-tray "DOCK APP: ClientMessage")

  (let ((dapp (xwem-tray-find-dapp (X-Event-xclient-window xev)))
        (mes-type (X-Atom-id (X-Event-xclient-atom xev))))
    (cond ((= mes-type (X-Atom-id (aref (xwem-tray-atoms xwem-tray) 9)))
           ;; part of some message arrived
           (let* ((len (- (xwem-dapp-mess-waitlen dapp)
                          (xwem-dapp-mess-currlen dapp)))
                  (ltgo (if (> len 20) 20 len))) ;length to go
             (setf (xwem-dapp-mess dapp)
                   (concat (xwem-dapp-mess dapp)
                           (xwem-list-to-string
                            (mapcar 'car (X-Event-xclient-msg xev)) ltgo)))
             (setf (xwem-dapp-mess-currlen dapp)
                   (+ (xwem-dapp-mess-currlen dapp) ltgo)))

           (when (= (xwem-dapp-mess-currlen dapp)
                    (xwem-dapp-mess-waitlen dapp))
             ;; message accomplished
             (let ((dtype (xwem-dapp-mess-type dapp)))
               (cond ((= dtype xwem-tc-message)
                      ;; TODO: run hook?
                      (when xwem-tray-message-hook
                        (funcall xwem-tray-message-hook dapp)))

                     ((= dtype xwem-tc-run-lisp)
                      (xwem-debug 'xwem-tray "DOCK APP: ELISP: '%s'"
                                  '(xwem-dapp-mess dapp))
                      (with-temp-buffer
                        (insert (xwem-dapp-mess dapp))
                        (condition-case nil
                            (progn
                              (xwem-message 'info "evaling: %S"
                                            (xwem-dapp-mess dapp))
                              (eval-buffer))
                          (t nil))))
                     ))))

          ((= mes-type (X-Atom-id (aref (xwem-tray-atoms xwem-tray) 3)))
           ;; opcode arrived
           (let ((opc (truncate (car (nth 1 (X-Event-xclient-msg xev))))))
             (cond ((= opc xwem-tc-dock-req) nil)
                   (t (setf (xwem-dapp-mess dapp) "")
                      (setf (xwem-dapp-mess-currlen dapp) 0)
                      (setf (xwem-dapp-mess-waitlen dapp)
                            (truncate (car (nth 3 (X-Event-xclient-msg xev)))))
                      (setf (xwem-dapp-mess-type dapp) opc))
                   )))
          (t (xwem-message
              'warning "Unknown mes-type %d from dock app." mes-type)))
    nil))

(defun xwem-dapp-handle-xevent (xdpy xwin xev)
  "X Events handler for dockapps."
  (xwem-debug 'xwem-tray "DAPP: X Event: %S" '(X-Event-name xev))

  (X-Event-CASE xev
    (:X-ClientMessage
     (xwem-dapp-handle-client-message xev))

    (:X-ConfigureNotify
     (let ((dapp (xwem-tray-find-dapp (X-Event-xconfigure-window xev))))
       (when (xwem-dapp-alive-p dapp)
         (let ((dgeom (xwem-dapp-geom dapp))
               (nw (X-Event-xconfigure-width xev))
               (nh (X-Event-xconfigure-height xev)))
           (unless (= nw (X-Geom-width dgeom))
             ;; Dockapp has been resized, move other dockapps if
             ;; necessary
             (xwem-tray-move-many-dapps
              xwem-tray-dapp-list (- (X-Geom-width dgeom) nw)
              (+ (X-Geom-x dgeom) (X-Geom-width dgeom)))
             (setf (X-Geom-width (xwem-dapp-geom dapp)) nw))
           (unless (= nh (X-Geom-height dgeom))
             (xwem-tray-move-dapp
              dapp nil (/ (- (X-Geom-height
                              (xwem-minib-xgeom xwem-minibuffer)) nh) 2)))))))
     ))

(defun xwem-tray-create (dpy)
  "Creates new XWEM system tray on DPY.
Window is InputOnly to be transparent."
  (let ((win nil))
    (setq win (XCreateWindow
               dpy nil
               0 0 1 1
               0 0 X-InputOnly nil
               :override-redirect t
               :event-mask xwem-tray-evmask))

    (X-Win-EventHandler-add-new win 'xwem-tray-handle-client-message
                                100 (list X-ClientMessage))

    ;; Setup various hints
    (XSetWMClass dpy win '("xwem-tray" "xwem-tray"))
    (XSetWMName dpy win "xwem-tray")

    (setf (xwem-tray-xwin xwem-tray) win)
    (setf (xwem-tray-plist xwem-tray) nil)

    ;; TODO: install Selections and properties we will need
    ))

(defun xwem-tray-init (dpy)
  "Initialize xwem tray."
  (setq xwem-tray
        (make-xwem-tray :atoms (make-vector 40 nil)
                        :xwin (xwem-minib-xwin xwem-minibuffer)))

  (let ((xwem-atoms (xwem-tray-atoms xwem-tray)))
    (aset xwem-atoms 0 (XInternAtom dpy "_NET_WM_WINDOW_TYPE"))
    (aset xwem-atoms 1 (XInternAtom dpy "_NET_WM_WINDOW_TYPE_DOCK"))
    (aset xwem-atoms 3 (XInternAtom dpy "_NET_SYSTEM_TRAY_OPCODE"))
    (aset xwem-atoms 4 (XInternAtom dpy "_XEMBED_INFO"))
    (aset xwem-atoms 5 (XInternAtom dpy "_XEMBED"))
    (aset xwem-atoms 6 (XInternAtom dpy "MANAGER"))
    (aset xwem-atoms 9 (XInternAtom dpy "_NET_SYSTEM_TRAY_MESSAGE_DATA"))
    (aset xwem-atoms 10 (XInternAtom dpy "_NET_WM_WINDOW_TYPE_SPLASH"))
    (aset xwem-atoms 15 (XInternAtom dpy "_NET_WM_STRUT"))
    (aset xwem-atoms 18 (XInternAtom dpy "_NET_WM_ICON"))
    (aset xwem-atoms 19 (XInternAtom dpy "_NET_WM_PID"))
    (aset xwem-atoms 20 (XInternAtom dpy "_XROOTPMAP_ID"))

    (aset xwem-atoms 30 (XInternAtom dpy "XWEM_DOCK_ID"))
    (aset xwem-atoms 31 (XInternAtom dpy "XWEM_DOCK_GROUP"))
    (aset xwem-atoms 32 (XInternAtom dpy "XWEM_DOCK_ALIGN"))

    ;; Use emacs pid as tray identificator
    (aset xwem-atoms 2
          (XInternAtom dpy (format "_NET_SYSTEM_TRAY_S%i" xwem-tray-id))))

  (setenv "SYSTEM_TRAY_ID" (format "%i" xwem-tray-id))

  ;; Subscribe on substructure change events for xwem tray window.
  (XSelectInput (xwem-dpy) (xwem-tray-xwin xwem-tray)
                (Xmask-or XM-SubstructureNotify XM-StructureNotify
                          (X-Attr-event-mask
                           (XGetWindowAttributes
                            (xwem-dpy) (xwem-tray-xwin xwem-tray)))))
  (X-Win-EventHandler-add-new (xwem-tray-xwin xwem-tray)
                              'xwem-tray-handle-xevent -1
                              (list X-MapNotify X-UnmapNotify
                                    X-DestroyNotify X-ConfigureNotify))
  (X-Win-EventHandler-add-new (xwem-tray-xwin xwem-tray)
                              'xwem-tray-handle-xevent 100
                              (list X-ClientMessage))

  ;; Configure systray cursor
  (setq xwem-tray-cursor
        (xwem-make-cursor xwem-tray-cursor-shape
                          xwem-tray-cursor-foreground-color
                          xwem-tray-cursor-background-color))
  (XSetWindowCursor (xwem-dpy) (xwem-tray-xwin xwem-tray)
                    xwem-tray-cursor))

(defun xwem-tray-handle-xevent (xdpy xwin xev)
  "X Events handler for xwem systray."
  (xwem-debug 'xwem-tray "TRAY X Event: %S" '(X-Event-name xev))

  (X-Event-CASE xev
    (:X-ClientMessage
     (let* ((mes-type (X-Atom-id (X-Event-xclient-atom xev)))
            (mes-data (X-Event-xclient-msg xev))
            (mes-win (X-Win-find-or-make (xwem-dpy) (car (nth 2 mes-data))))
            (data-type (truncate (car (nth 1 mes-data)))))
       (cond ((= mes-type (X-Atom-id (aref (xwem-tray-atoms xwem-tray) 3)))
              (cond ((= data-type xwem-tc-dock-req)
                     (xwem-debug 'xwem-tray "Creating new dockapp ..")
                     (xwem-tray-new-dapp mes-win))
                    (t (xwem-message
                        'warning "Unknown data-type %d in client message."
                        data-type))))
             (t (xwem-message 'warning "Unknown mes-type %d" mes-type)))))

    (:X-DestroyNotify
     (let ((dapp (xwem-tray-find-dapp (X-Event-xdestroywindow-window xev))))
       (when (xwem-dapp-alive-p dapp)
         (xwem-message 'note "Removing dockapp ..")
         (xwem-tray-remove-dapp dapp))))

    (:X-UnmapNotify
     (let ((dapp (xwem-tray-find-dapp (X-Event-xunmap-window xev))))
       (when (xwem-dapp-alive-p dapp)
         (xwem-tray-hide-dapp dapp t))))

    (:X-MapNotify
     (let ((dapp (xwem-tray-find-dapp (X-Event-xunmap-window xev))))
       (when (xwem-dapp-alive-p dapp)
         (xwem-tray-show-dapp dapp t))))

    (:X-ConfigureNotify
     (when (eq (X-Event-xconfigure-window xev)
               (xwem-tray-xwin xwem-tray))
       (let ((height (X-Event-xconfigure-height xev)))
         (when (not (eql height
                         (xwem-tray-get-prop xwem-tray 'xwem-saved-height)))
           (xwem-tray-put-prop xwem-tray 'xwem-saved-height height)
           (mapc #'(lambda (dapp)
                     (setf (X-Geom-y (xwem-dapp-geom dapp))
                           (/ (- height (X-Geom-height (xwem-dapp-geom dapp))) 2))
                     (xwem-tray-apply-dapp-y-position dapp))
                 xwem-tray-dapp-list)))))))

(defun xwem-tray-startit (&optional dpy)
  "Start xwew tray on display DPY."
  (unless (get 'xwem-tray 'initialized)
    (unless dpy (setq dpy (xwem-dpy)))

    (xwem-tray-init dpy)
    (XSetSelectionOwner dpy (aref (xwem-tray-atoms xwem-tray) 2)
                        (xwem-tray-xwin xwem-tray))
    (XMapWindow dpy (xwem-tray-xwin xwem-tray))

    ;; Add finialization hook
    (add-hook 'xwem-exit-hook 'xwem-tray-fini)
    (put 'xwem-tray 'initialized t)))

(defun xwem-tray-fini ()
  "Finialize xwem-tray."
  (mapc #'(lambda (dapp)
            (XDestroyWindow (xwem-dpy) (xwem-dapp-xwin dapp)))
        xwem-tray-dapp-list)

  (setq xwem-tray-dapp-list nil)
  (setq xwem-tray-cursor nil))

;;;###autoload
(defun xwem-tray-delimeter (&optional w h bgcol)
  "Add delimiter to dockapp.
W and H specifies delimiter width and height.
BGCOL - background color."
  (unless w
    (setq w xwem-tray-delimiter-width))
  (unless h
    (setq h (- (X-Geom-height (xwem-minib-cl-xgeom xwem-minibuffer))
               (* 2 xwem-tray-delimiter-height-reminder))))

  (let* ((bgmode (xwem-tray-background-mode))
         (bgcol (or bgcol (xwem-face-foreground 'xwem-tray-delimiter-face
                                                (list 'background bgmode))))
         (xwin (XCreateWindow
                (xwem-dpy) nil 0 0 w h
                0 nil nil nil
                :override-redirect t
                :cursor (xwem-make-cursor X-XC-sb_h_double_arrow)
                :background-pixel
                (XAllocNamedColor (xwem-dpy) (XDefaultColormap (xwem-dpy))
                                  bgcol)
                :event-mask (Xmask-or XM-ButtonPress
                                      XM-ButtonRelease XM-ButtonMotion))))
    (xwem-XTrayInit (xwem-dpy) xwin)
    xwin))

;;;###xwem-autoload
(defun xwem-tray-background-mode ()
  "Return background mode(`dark' or `light') for system tray."
  (xwem-get-background-mode
   (face-background-instance 'default (xwem-minib-frame xwem-minibuffer))))

;;; System tray managing model
;;;###autoload
(defun xwem-manage-systray (cl)
  "Manage method for systray dockaps."
  (xwem-XTrayInit (xwem-dpy) (xwem-cl-xwin cl) nil "misc" nil))


(provide 'xwem-tray)

;;;; On-load actions:

;; Systray manage type
(define-xwem-manage-model systray
  "Managing model for systray utilities."
  :manage-method 'xwem-manage-systray)

;; - Register xwem system tray
(if xwem-started
    (xwem-tray-startit)
  (add-hook 'xwem-after-init-hook 'xwem-tray-startit))

;;; xwem-tray.el ends here
