;;; xwem-struct.el --- Core XWEM structures.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Tue Aug 24 12:43:45 MSD 2004
;; Keywords: xwem
;; Time-stamp: <22/8/2008 00:22:35 lg@h1.lan>

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

;;; Code:

(ignore-errors
  (require 'xlib-xc))

(eval-when-compile
  ;; Shut up compiler
  (defvar xwem-current-cl)
  (defvar xwem-last-cl)
  (defvar xwem-frames-list)
  (defvar xwem-clients)
  (defvar xwem-client-ev-mask))

(define-error 'xwem-error "XWEM error")


;;;; Root window
(defvar xwem-root-display nil
  "Default X Display for XWEM.
Use `xwem-dpy' to get it.")
(defvar xwem-root-window nil
  "Default root window of `xwem-dpy'.")
(defvar xwem-root-geometry nil
  "Geometry of `xwem-rootwin'.")

;;;###xwem-autoload
(defmacro xwem-dpy ()
  "Return default X display for XWEM."
  'xwem-root-display)
;;;###xwem-autoload
(defmacro xwem-rootwin ()
  "Return default root window of `xwem-dpy'."
  'xwem-root-window)
;;;###xwem-autoload
(defmacro xwem-rootgeom ()
  'xwem-root-geometry)

(defsetf xwem-dpy () (xdpy)
  "Set default X display for XWEM."
  `(progn
     (setq xwem-root-display ,xdpy)
     (unless (X-Dpy-p (xwem-dpy))
       (error 'xwem-error "Can't open display"))
     (setq xwem-root-window (XDefaultRootWindow (xwem-dpy)))
     (setq xwem-root-geometry (XGetGeometry (xwem-dpy) (xwem-rootwin)))))

;;;; Frame structures
(defstruct (xwem-frame
            (:type vector) :named
            (:print-function (lambda (f s pl)
                               (princ (format "#<xwem-frame '%s'>"
                                              (xwem-frame-name f)) s))))
  type                                  ; frame type (desktop, embedded, dedicated, embedded-desktop, etc)
  xwin                                  ; X window
  xgeom                                 ; frame geometry
  (name "default")                      ; frame name
  rootwin                               ; XWEM's root window for frame
  selwin                                ; XWEM's selected window
  link-next                             ; Link to next xwem's frame in linkage
  link-prev                             ; Link to prev xwem's frame in linkage
  state                                 ; 'mapped, 'unmapped, 'destroyed

  plist)                                ; User defined plist

(defstruct (xwem-frame-saved
            (:type vector) :named)
  frame                                 ; nil or `xwem-frame'
  selected-p                            ; non-nil if frame was selected
  type
  name
  xgeom
  state                                 ;
  plist                                 ; as in xwem-frame
  winconfig)

(defsubst xwem-frame-get-prop (frame prop)
  (plist-get (xwem-frame-plist frame) prop))

(defsubst xwem-frame-rem-prop (frame prop)
  "From FRAME's plist remove property PROP."
  (setf (xwem-frame-plist frame)
        (plist-remprop (xwem-frame-plist frame) prop)))

(defsubst xwem-frame-put-prop (frame prop val)
  "Put PROP with VAL to FRAME's properties list."
  (if prop
      (setf (xwem-frame-plist frame)
            (plist-put (xwem-frame-plist frame) prop val))
    (xwem-frame-rem-prop frame prop)))
(put 'xwem-frame-put-prop 'lisp-indent-function 2)

(defvar xwem-current-frame nil
  "Currently selected frame.
Do not access/modify this variable directly, use `xwem-frame-selected'.")

(defmacro xwem-frame-selected ()
  "Return selected frame."
  'xwem-current-frame)
(defsetf xwem-frame-selected () (frame)
  `(setq xwem-current-frame ,frame))

(defsubst xwem-frame-selected-p (frame)
  "Return non-nil if FRAME is selected."
  (eq frame (xwem-frame-selected)))

(defsubst xwem-frame-desktop-p (frame)
  "Return non-nil if FRAME is desktop."
  (memq (xwem-frame-type frame)
        '(desktop embedded-desktop)))

(defsubst xwem-frame-embedded-p (frame)
  "Return non-nil if FRAME is embedded frame."
  (memq (xwem-frame-type frame)
        '(embedded embedded-desktop)))

(defsubst xwem-frame-dedicated-p (frame)
  "Return non-nil if FRAME is dedicated frame."
  (memq (xwem-frame-type frame)
        '(dedicated)))

(defmacro xwem-frame-x (frame)
  `(X-Geom-x (xwem-frame-xgeom ,frame)))
(defsetf xwem-frame-x (frame) (x)
  `(setf (X-Geom-x (xwem-frame-xgeom ,frame)) ,x))

(defmacro xwem-frame-y (frame)
  `(X-Geom-y (xwem-frame-xgeom ,frame)))
(defsetf xwem-frame-y (frame) (y)
  `(setf (X-Geom-y (xwem-frame-xgeom ,frame)) ,y))

(defmacro xwem-frame-width (frame)
  `(X-Geom-width (xwem-frame-xgeom ,frame)))
(defsetf xwem-frame-width (frame) (width)
  `(setf (X-Geom-width (xwem-frame-xgeom ,frame)) ,width))

(defmacro xwem-frame-height (frame)
  `(X-Geom-height (xwem-frame-xgeom ,frame)))
(defsetf xwem-frame-height (frame) (height)
  `(setf (X-Geom-height (xwem-frame-xgeom ,frame)) ,height))

(defmacro xwem-frame-border-width (frame)
  `(X-Geom-border-width (xwem-frame-xgeom ,frame)))
(defsetf xwem-frame-border-width (frame) (height)
  `(setf (X-Geom-border-width (xwem-frame-xgeom ,frame)) ,height))

(defmacro xwem-frame-title-height (frame)
  "Return FRAME's title height."
  `(xwem-frame-property ,frame 'title-height))
(defsetf xwem-frame-title-height (frame) (new-title-height)
  "Set FRAME's title height to NEW-TITLE-HEIGHT."
  `(xwem-frame-set-property ,frame 'title-height ,new-title-height))

(defmacro xwem-frame-inner-border-width (frame)
  "Return FRAME's inner border width."
  `(xwem-frame-property ,frame 'inner-border-width))
(defsetf xwem-frame-inner-border-width (frame) (new-inner-border-width)
  "Set FRAME's inner border width to be NEW-INNER-BORDER-WIDTH."
  `(xwem-frame-set-property ,frame 'inner-border-width ,new-inner-border-width))

(defsubst xwem-frame-alive-p (frame)
  "Return non-nil if FRAME is alive XWEM frame."
  (and (xwem-frame-p frame)
       (memq frame xwem-frames-list)
       (not (eq (xwem-frame-type frame) 'destroyed))))

(defsubst xwem-frame-mapped-p (frame)
  "Return non-nil if xwem FRAME is mapped."
  (and (xwem-frame-p frame)
       (eq (xwem-frame-state frame) 'mapped)))

(defsubst xwem-frame-cl (frame)
  "Return currently active xwem client in FRAME."
  (xwem-win-cl (xwem-frame-selwin frame)))


;;;; Win structures
(defstruct (xwem-win
            (:type vector) :named
            (:print-function (lambda (w s pl)
                               (princ (format "#<xwem-win id=%d>"
                                              (xwem-win-id w)) s))))
  id                                    ; unique window id
  geom                                  ; window geometry (border width is internal window width)
  clients                               ; xwem clients list managed in window
  cl                                    ; Current window's client
  frame                                 ; xwem frame
  dead                                  ; non-nil if window is dead
  deleted                               ; non-nil if window was deleted
  next                                  ; next window in windows chain
  prev                                  ; previous window in windows chain
  hchild                                ; horisontal child (if any)
  vchild                                ; vertical child (if any)
  parent                                ; parent window

  plist)                                ; User defined plist

(defstruct (xwem-win-saved
            (:type vector) :named
            (:predicate xwem-iswinsaved-p))
  id                                    ; saved window id
  geom                                  ; saved window geometry
  clients                               ; clients managed in window
  cl                                    ; Current window's client
  plist                                 ; properties
  selwin-p                              ; non-nil if window is selected in frame
  first-hchild first-vchild
  next prev)

(defstruct (xwem-win-config
            (:type vector) :named
            (:predicate xwem-iswinconfig-p))
  frame                                 ; window's frame
  frame-xgeom                           ; saved frame X-Geom
  frame-properties                      ; saved frame properties
  current-cl                            ; cl in selected window
  min-width min-height
  saved-root-window)

(defsubst xwem-win-alive-p (window)
  "Return non-nil if WINDOW is alive."
  (and (xwem-win-p window)
       (xwem-frame-alive-p (xwem-win-frame window))
       (not (xwem-win-deleted window))
       (not (xwem-win-dead window))))

(defmacro xwem-win-x (win)
  `(X-Geom-x (xwem-win-geom ,win)))
(defsetf xwem-win-x (win) (x)
  `(setf (X-Geom-x (xwem-win-geom ,win)) ,x))

(defmacro xwem-win-y (win)
  `(X-Geom-y (xwem-win-geom ,win)))
(defsetf xwem-win-y (win) (y)
  `(setf (X-Geom-y (xwem-win-geom ,win)) ,y))

(defmacro xwem-win-width (win)
  `(X-Geom-width (xwem-win-geom ,win)))
(defsetf xwem-win-width (win) (width)
  `(setf (X-Geom-width (xwem-win-geom ,win)) ,width))

(defmacro xwem-win-height (win)
  `(X-Geom-height (xwem-win-geom ,win)))
(defsetf xwem-win-height (win) (height)
  `(setf (X-Geom-height (xwem-win-geom ,win)) ,height))

(defmacro xwem-win-border-width (win)
  `(X-Geom-border-width (xwem-win-geom ,win)))
(defsetf xwem-win-border-width (win) (border-width)
  `(setf (X-Geom-border-width (xwem-win-geom ,win)) ,border-width))

(defsubst xwem-win-get-prop (win prop)
  "Get WIN's property PROP."
  (plist-get (xwem-win-plist win) prop))

(defsubst xwem-win-rem-prop (win prop)
  "Remove WIN's property PROP."
  (setf (xwem-win-plist win)
        (plist-remprop (xwem-win-plist win) prop)))

(defsubst xwem-win-put-prop (win prop val)
  "Set WIN's property PROP to VAL."
  (if val
      (setf (xwem-win-plist win)
            (plist-put (xwem-win-plist win) prop val))
    (xwem-win-rem-prop win prop)))
(put 'xwem-win-put-prop 'lisp-indent-function 2)

(defmacro xwem-win-selected ()
  "Return selected window."
  '(and (xwem-frame-alive-p (xwem-frame-selected))
        (xwem-frame-selwin (xwem-frame-selected))))

(defmacro xwem-win-selected-p (win)
  "Return non-nil if WIN is currently selected window."
  `(eq ,win (xwem-win-selected)))

(defsubst xwem-win-selwin-p (win)
  "Return non-nil if WIN is localy selected window in WIN's frame."
  (and (xwem-win-p win)
       (eq win (xwem-frame-selwin (xwem-win-frame win)))))

(defsubst xwem-win-cl-current-p (cl &optional win)
  "Return non-nil if CL is current WIN's client."
  (unless win
    (setq win (xwem-cl-win cl)))
  (when (xwem-win-p win)
    (eq cl (xwem-win-cl win))))


;;;; Client structures
(defstruct xwem-hints
  ;; TODO: add more
  wm-normal-hints
  wm-hints
  wm-class
  wm-role
  wm-command
  wm-name
  wm-icon-name
  wm-transient-for
  wm-protocols)

(defun xwem-print-cl (cl stream pl)
  "Printer for xwem client."
  (when (xwem-cl-p cl)
    (princ (format "#<xwem-cl id=0x%x name=%S>"
                   (X-Win-id (xwem-cl-xwin cl))
                   (xwem-cl-wm-name cl))
           stream)
    t))

(defstruct (xwem-cl (:print-function xwem-print-cl))
  xwin                                  ; CL's X window
  (ev-mask 0)                           ; CL's event maks
  initial-xattrs                        ; X-Attr when CL just initialized
  initial-xgeom                         ; X-Geom when CL just initialized

  xgeom                                 ; Current CL's X-Geom
  new-xgeom                             ; Wishable CL's X-Geom (for refiting)

  hints                                 ; xwem-hints
  transient-for                         ; non-nil if client is transient for window

  manage-spec                           ; MANAGE-SPEC which was used to manage client.
  win                                   ; xwem-win now (only for windowing manage types)
  translist                             ; list of transient-for windows for this client

  (state 'unknown)                      ; state of client, 'active, 'inactive, 'iconified, 'unknown, etc
  start-time                            ; start-time
  recency                               ; last time when CL was active

  local-variables                       ; client local variables list
  sys-plist                             ; system plist
  plist                                 ; user defined plist
  )

(defmacro xwem-cl-destroyed-p (cl)
  "Return non-nil if CL has already destroyed xwin."
  `(eq (xwem-cl-state ,cl) 'destroyed))

(defmacro xwem-cl-win-geom (cl)
  "Get geometry for client CL. Actually return xwem window geometry."
  `(xwem-win-geom (xwem-cl-win ,cl)))

;; User plist
(defsubst xwem-cl-get-prop (cl prop)
  "From CL's property list get property PROP."
  (plist-get (xwem-cl-plist cl) prop))

(defsubst xwem-cl-rem-prop (cl prop)
  "From CL's property list remove property PROP."
  (setf (xwem-cl-plist cl) (plist-remprop (xwem-cl-plist cl) prop)))

(defsubst xwem-cl-put-prop (cl prop val)
  "In CL's property list put property PROP with value VAL.
If VAL is nil - remove property."
  (if val
      (setf (xwem-cl-plist cl) (plist-put (xwem-cl-plist cl) prop val))
    (xwem-cl-rem-prop cl prop)))
(put 'xwem-cl-put-prop 'lisp-indent-function 2)

;; System plist
(defsubst xwem-cl-get-sys-prop (cl prop)
  "From CL's system property list get property PROP."
  (plist-get (xwem-cl-sys-plist cl) prop))

(defsubst xwem-cl-rem-sys-prop (cl prop)
  "From CL's system property list remove property PROP."
  (setf (xwem-cl-sys-plist cl) (plist-remprop (xwem-cl-sys-plist cl) prop)))

(defsubst xwem-cl-put-sys-prop (cl prop val)
  "In CL's system property list put property PROP with value VAL.
If VAL is nil - remove property."
  (if val
      (setf (xwem-cl-sys-plist cl) (plist-put (xwem-cl-sys-plist cl) prop val))
    (xwem-cl-rem-sys-prop cl val)))
(put 'xwem-cl-put-sys-prop 'lisp-indent-function 2)

(defmacro xwem-cl-manage-type (cl)
  "Return CL's manage type name."
  `(car (xwem-cl-manage-spec ,cl)))
(defsetf xwem-cl-manage-type (cl) (new-type)
  `(setf (xwem-cl-manage-spec ,cl) (list ,new-type)))

(defmacro xwem-cl-selected ()
  "Return currently selected Client.
May be nil if no current client."
  'xwem-current-cl)
(defsetf xwem-cl-selected () (cl)
  `(setq xwem-current-cl ,cl))

(defmacro xwem-last-client (&optional num)
  "Return NUMs last selected client."
  `(nth (or ,num 0) xwem-last-clients))
(defsetf xwem-last-client () (cl)
  `(progn
     (push ,cl xwem-last-clients)
     (when (> (length xwem-last-clients) xwem-maximum-last-clients)
       (setq xwem-last-clients (butlast xwem-last-clients)))))

(defsubst xwem-cl-selected-p (cl)
  "Return non-nil if CL is selected client.
If CL is not valid `xwem-cl' structure, nill will be returned."
  (and (xwem-cl-p cl) (eq cl (xwem-cl-selected))))

(defsubst xwem-cl-frame (cl)
  "Return frame where CL."
  (let ((win (xwem-cl-win cl)))
    (and (xwem-win-p win) (xwem-win-frame win))))

(defsubst xwem-cl-alive-p (cl)
  "Return non-nil if CL is alive i.e. not in 'destroyed state."
  (and (xwem-cl-p cl) (not (xwem-cl-destroyed-p cl))))

(defsubst xwem-cl-managed-p (cl &optional states)
  "Return non-nil if CL ins't in withdrawn state."
  (and (xwem-cl-p cl)
       (memq (xwem-cl-state cl) (or states '(active inactive iconified)))))

(defsubst xwem-cl-active-p (cl)
  "Return non-nil if CL is activated."
  (eq (xwem-cl-state cl) 'active))

(defsubst xwem-cl-inactive-p (cl)
  "Return non-nil if CL is inactive."
  (eq (xwem-cl-state cl) 'inactive))

(defsubst xwem-cl-iconified-p (cl)
  "Return non-nil if CL is iconified."
  (eq (xwem-cl-state cl) 'iconified))

;; wm accessors
(defsubst xwem-cl-wm-name (cl)
  "Return cl's WM_NAME."
  (xwem-hints-wm-name (xwem-cl-hints cl)))
(defsetf xwem-cl-wm-name (cl) (name)
  "Set CL's WM_NAME to NAME."
  `(setf (xwem-hints-wm-name (xwem-cl-hints ,cl)) ,name))

(defsubst xwem-cl-wm-icon-name (cl)
  "Return cl's WM_ICON_NAME."
  (xwem-hints-wm-icon-name (xwem-cl-hints cl)))
(defsetf xwem-cl-wm-icon-name (cl) (icon-name)
  "Set CL's WM_ICON_NAME to ICON-NAME."
  `(setf (xwem-hints-wm-icon-name (xwem-cl-hints ,cl)) ,icon-name))

(defsubst xwem-cl-wm-hints (cl)
  "Return cl's WM_HINTS."
  (xwem-hints-wm-hints (xwem-cl-hints cl)))
(defsetf xwem-cl-wm-hints (cl) (hints)
  "Set CL's WM_HINTS to HINTS."
  `(setf (xwem-hints-wm-hints (xwem-cl-hints ,cl)) ,hints))

(defsubst xwem-cl-wm-normal-hints (cl)
  "Return cl's WM_NORMAL_HINTS."
  (xwem-hints-wm-normal-hints (xwem-cl-hints cl)))
(defsetf xwem-cl-wm-normal-hints (cl) (wnh)
  "Set CL's WM_NORMAL_HINTS to WNH."
  `(setf (xwem-hints-wm-normal-hints (xwem-cl-hints ,cl)) ,wnh))

(defsubst xwem-cl-wm-class (cl)
  "Return cl's WM_CLASS."
  (xwem-hints-wm-class (xwem-cl-hints cl)))
(defsetf xwem-cl-wm-class (cl) (class)
  "Set CL's WM_CLASS to CLASS."
  `(setf (xwem-hints-wm-class (xwem-cl-hints ,cl)) ,class))

(defsubst xwem-cl-wm-role (cl)
  "Return cl's WM_WINDOW_ROLE."
  (xwem-hints-wm-role (xwem-cl-hints cl)))
(defsetf xwem-cl-wm-role (cl) (role)
  "Set CL's WM_WINDOW_ROLE to ROLE."
  `(setf (xwem-hints-wm-role (xwem-cl-hints ,cl)) ,role))

(defsubst xwem-cl-wm-command (cl)
  "Return cl's WM_COMMAND."
  (xwem-hints-wm-command (xwem-cl-hints cl)))
(defsetf xwem-cl-wm-command (cl) (command)
  "Set CL's WM_COMMAND to COMMAND."
  `(setf (xwem-hints-wm-command (xwem-cl-hints ,cl)) ,command))

(defsubst xwem-cl-wm-transient-for (cl)
  "Return cl's WM_TRANSIENT_FOR."
  (xwem-hints-wm-transient-for (xwem-cl-hints cl)))
(defsetf xwem-cl-wm-transient-for (cl) (wtf)
  "Set CL's WM_TRANSIENT_FOR to WTF."
  `(setf (xwem-hints-wm-transient-for (xwem-cl-hints ,cl)) ,wtf))

(defsubst xwem-cl-wm-protocols (cl)
  "Return cl's WM_PROTOCOLS."
  (xwem-hints-wm-protocols (xwem-cl-hints cl)))
(defsetf xwem-cl-wm-protocols (cl) (protocols)
  "Set CL's WM_PROTOCOLS to PROTOCOLS."
  `(setf (xwem-hints-wm-protocols (xwem-cl-hints ,cl)) ,protocols))

;; Client configuration
(defstruct (xwem-cl-config
            (:type vector) :named)
  selected-p
  xwin-id
  command
  manage-type
  properties)


;;;; Minibuffer
(defstruct (xwem-minib
            (:type vector) :named)
  frame                                 ; Emacs frame
  cl                                    ; Corresponding xwem client

  xgeom                                 ; parent geometry
  xwin                                  ; parent xwindow

  plist                                 ; User defined plist
  )

(defmacro xwem-minib-get-prop (m prop)
  `(plist-get (xwem-minib-plist ,m) ,prop))

(defmacro xwem-minib-put-prop (m prop val)
  `(setf (xwem-minib-plist ,m)
         (plist-put (xwem-minib-plist ,m) ,prop ,val)))
(put 'xwem-minib-put-prop 'lisp-indent-function 2)

(defmacro xwem-minib-rem-prop (m prop)
  `(setf (xwem-minib-plist ,m)
         (plist-remprop (xwem-minib-plist ,m) ,prop)))

(defmacro xwem-minib-cl-xgeom (m)
  "Return client's X geometry of minibuffer M."
  `(xwem-cl-xgeom (xwem-minib-cl ,m)))
(defsetf xwem-minib-cl-xgeom (m) (xgeom)
  `(setf (xwem-cl-xgeom (xwem-minib-cl ,m)) ,xgeom))

(defmacro xwem-minib-cl-xwin (m)
  "Return clien's X window of minibuffe M."
  `(xwem-cl-xwin (xwem-minib-cl ,m)))
(defsetf xwem-minib-cl-xwin (m) (xwin)
  `(setf (xwem-cl-xwin (xwem-minib-cl ,m)) ,xwin))


;;; Various macros

;; Defining deferred funcalls
;;; Deferring related stuff
(require 'dll)

(defvar xwem-pre-deferring-hook nil
  "*Hooks to run before deferring.")
(defvar xwem-post-deferring-hook nil
  "*Hooks to run after deferring complete.
`xwem-post-deferring-hook' clears every time it runs.")

(defvar xwem-deferred-dll (dll-create)
  "Double linked list of deferred things.")

(defvar xwem-deferring-p nil
  "Non-nil mean we are running deferred function.")

(defun xwem-deferred-push (fun &rest args)
  (let*  ((dummy (dll-get-dummy-node xwem-deferred-dll))
          (node  (elib-node-right dummy))
          (exists nil))
    (while (and (not (eq node dummy))
                (not (and (funcall
                           #'(lambda (e1 e2)
                               (and (eq (car e1) (car e2))
                                    (if (and (listp (cdr e1)) (listp (cdr e2))
                                             (= (length (cdr e1))
                                                (length (cdr e2))))
                                        (not (memq nil (mapcar*
                                                        'eq (cdr e1) (cdr e2))))
                                      (eq (cdr e1) (cdr e2)))))
                           (cons fun args)
                           (dll-element xwem-deferred-dll node))
                          (setq exists t))))
      (setq node (elib-node-right node)))

    (if exists
        (dll-delete xwem-deferred-dll node)
      (enqueue-eval-event 'xwem-deferred-process nil))

    (xwem-debug 'xwem-deferred "---------> IN %S" 'fun)
    (dll-enter-last xwem-deferred-dll (cons fun args))))

(defun xwem-deferred-process (obj-notused)
  "Process deferring commands."
  (declare (special xwem-deferring-p))

  (unless xwem-deferring-p
    (let ((xwem-deferring-p t))
      (run-hooks 'xwem-pre-deferring-hook)
      (setq xwem-pre-deferring-hook nil)))

  (while (not (dll-empty xwem-deferred-dll))
    (let ((el (dll-first xwem-deferred-dll))
          (xwem-deferring-p t))
      (xwem-debug 'xwem-deferred "<--------- OUT %S" '(car el))
      (dll-delete-first xwem-deferred-dll)
      (apply (car el) (cdr el))))

  (unless xwem-deferring-p
    (let ((xwem-deferring-p t))
      (run-hooks 'xwem-post-deferring-hook)
      (setq xwem-post-deferring-hook nil))))

(defun xwem-add-hook-post-deferring (hook &optional append)
  "Add HOOK to `xwem-post-deferring-hook'."
  (add-hook 'xwem-post-deferring-hook hook append)
  ;; Run it to be sure to enter deferring
  (xwem-deferred-push 'ignore))

;; Dont know where to put this macro, so putten here.
(defmacro define-xwem-deferred
  (deff-name normal-name arglist docstring &rest body)
  "Define new deferred function with function name DEFF-NAME.
Deferred function is function which is called when XEmacs is about to became idle.

Another advantage of deferred function is that only one instance of
function will be called with same arguments.  For example if you have
`my-defffun' deferred function and you call twice `(my-defffun 1)',
`(my-defffun 1)' - then when XEmacs will be about idle only one call
occurs to `my-defffun'.  However if you pass different arguments, all
calls with different arguments are called.  Arguments are equal if
they are either `eq' or both are lists, where each element is `eq'.

NAME, ARGLIST, DOCSTRING and BODY argument have same meaning as for `defun'.
If NORMAL-NAME is specified, also define non-deferred variant of DEFF-NAME function.
If NORMAL-NAME is ommited, then normal-name constructed by
concatination of DEFF-NAME and \"-1\"."
  (unless (and (not (null normal-name))
               (symbolp normal-name))
    ;; If NORMAL-NAME ommited
    (setq body (cons docstring body))
    (setq docstring arglist)
    (setq arglist normal-name)
    (setq normal-name (intern (concat (symbol-name deff-name) "-1"))))

  `(progn
     (defun ,normal-name ,arglist
       ,docstring
       ,@body)

     (defun ,deff-name (&rest args)
       ,(concat "Deferred variant of `" (symbol-name normal-name) "'.")
       (apply (quote xwem-deferred-push) (quote ,normal-name) args))))

(defmacro xwem-deferred-funcall (fun &rest args)
  "Call FUN with ARGS, deferring funcall to FUN."
  `(xwem-deferred-push ,fun ,@args))

(defmacro xwem-unwind-protect (body-form &rest unwind-forms)
  "Execute BODY-FORM protecting it in safe more with UNWIND-FORMS.
`xwem-unwind-protect' differs from `unwind-protect' that
`xwem-unwind-protect' executes UNWIND-FORMS even when debugging."
  `(prog1
    (condition-case xwem-unwind-error
        ,body-form
      (t ,@unwind-forms
         (apply 'error (car xwem-unwind-error) (cdr xwem-unwind-error))))
    ,@unwind-forms))
(put 'xwem-unwind-protect 'lisp-indent-function 1)

(defmacro xwem-overriding-local-map (nlm &rest forms)
  "Execute FORMS installing `xwem-overriding-local-map' to NLM.
Do it in safe manner."
  `(xwem-unwind-protect
       (let ((xwem-override-local-map ,nlm))
         ,@forms)))
(put 'xwem-overriding-local-map 'lisp-indent-function 'defun)

;;; X Properties
(defmacro xwem-XProperty-get (xwin prop-atom-string)
  `(ignore-errors (read (decode-coding-string
                         (XGetPropertyString
                          (xwem-dpy) ,xwin
                          (XInternAtom (xwem-dpy) ,prop-atom-string)) 'ctext))))
(defmacro xwem-XProperty-set (xwin prop-atom-string prop-val)
  `(if ,prop-val
       (XSetPropertyString (xwem-dpy) ,xwin
                           (XInternAtom (xwem-dpy) ,prop-atom-string)
                           (encode-coding-string (format "%S" ,prop-val) 'ctext))
     (XDeleteProperty (xwem-dpy) ,xwin
                      (XInternAtom (xwem-dpy) ,prop-atom-string))))

;;; XSendEvent workaround
(defmacro xwem-XSendEvent (dpy win propagate ev-mask event-native event-ffi)
  (if (featurep 'ffi-xlib)
      `(XSendEvent ,dpy ,win ,propagate ,ev-mask ,event-ffi)
    `(XSendEvent ,dpy ,win ,propagate ,ev-mask ,event-native)))

(provide 'xwem-struct)

;;; xwem-struct.el ends here
