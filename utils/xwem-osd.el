;;; xwem-osd.el --- On Screen Display implementation for XWEM.

;; Copyright (C) 2004-2007 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Mon Jan 12 13:14:32 MSK 2004
;; Keywords: xwem
;; Time-stamp: <8/8/2008 05:01:01 lg@h1.lan>

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

;; Support for On Screen Display in XWEM.  xwem-osd can display text,
;; processing bar, other stuff using shaped window.  The main feature
;; of this OSD implementation that it uses OSD instances to display
;; stuff, so it does not need to handle expose events.

;; OSD supports system tray.  It is very easy to write OSD dockapp.
;; Simple example in xwem-framei.el.  You just create osd as usuall,
;; but using `xwem-osd-create-dock', where you can specify width,
;; height and keymap to use, for example:
;; 
;;    (setq myosd (xwem-osd-create-dock
;;                  (xwem-dpy) 24 24 (list 'keymap myosd-keymap)))
;;    (xwem-osd-text myosd "test")
;; 
;; This will create dockapp in system tray, display "test" in it, and
;; will execute commands in `myosd-keymap' if you click on OSD.  To
;; define commands in `myosd-keymap' do something like:
;; 
;;    (define-key myosd-keymap [button1]
;;      (lambda () (interactive) (xwem-message 'info "Myosd Hello world!")))
;;    (define-key myosd-keymap [button3] 'myosd-popup-menu)
;; 

;; New instance type added - dots.  To poly dataset in OSD you can use
;; `xwem-osd-dots-add' function.  TYPE is one of:
;; 
;;    'points      - Little circles.
;;    'lines       - Lines.
;;    'linespoints - Lines with points at ends.
;;    'impulses    - Impulses from 0 to dot's Y.
;;    'dots        - Tiny dots.
;;    'steps       - Steps around points.
;;    'fsteps      - Another 'steps variant.
;;    'histeps     - Yet another 'steps variant (NI)
;;    'boxes       - Boxes arount points (NI)
;; 
;; For example if you have some dataset(CPU load f.i.) which
;; represented in as list of cons cells, which car is X and cdr is Y.
;; 
;;   ds
;;   ==>
;;   ((1 . 3) (3 . 6) (5 . 4) (7 . 10) (9 . 10) (11 . 3))
;;
;;   (xwem-osd-dots-add myosd 'impulses ds :color "red4")
;; 
;; This will draw nice graph.
;; 
;; For full drawing posibilities consider `xwem-diagram.el'.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'xlib-xshape)
(require 'xlib-tray)
(require 'xlib-xpm)

(require 'xwem-misc)
(require 'xwem-diagram)

(defgroup xwem-osd nil
  "Group to customize OSD."
  :prefix "xwem-osd-"
  :group 'xwem-misc)

(defcustom xwem-osd-default-font "fixed"
  "Default font for text drawed in osd."
  :type 'string
  :group 'xwem-osd)

(defcustom xwem-osd-default-color "black"
  "Default color used to draw."
  :type 'color
  :group 'xwem-osd)

(defcustom xwem-osd-default-stack-rank 100
  "Default rank."
  :type 'number
  :group 'xwem-osd)

;;; Internal variables

(defconst xwem-osd-instance-types '(text line dots arc rect icon)
  "List of valid types of osd instance.")


(defstruct xwem-osd-instance
  type					; instance type, see `xwem-osd-instance-types'
  osd					; back reference to osd

  xwin xmask
  xgc                                   ; instance local GC

  plist)                                ; User defined plist

(defsubst xwem-osd-instance-put-prop (osin prop val)
  "In OSD's instance OSIN properties list put property PROP with value VAL."
  (setf (xwem-osd-instance-plist osin)
        (plist-put (xwem-osd-instance-plist osin) prop val)))
(put 'xwem-osd-instance-put-prop 'lisp-indent-function 2)

(defsubst xwem-osd-instance-get-prop (osin prop)
  "Return OSD's instance OSIN property PROP."
  (plist-get (xwem-osd-instance-plist osin) prop))

(defsubst xwem-osd-instance-rem-prop (osin prop)
  "Remove OSD's instance OSIN property PROP."
  (setf (xwem-osd-instance-plist osin)
        (plist-remprop (xwem-osd-instance-plist osin) prop)))

(defmacro xwem-osd-instance-xdpy (osin)
  "Return display of OSIN osd instance."
  `(xwem-osd-xdpy (xwem-osd-instance-osd osin)))

(defstruct xwem-osd
  state					; 'destroyed, 'hided or 'shown
  x y width height

  xdpy
  xwin
  xmask

  gc					; GC used to draw
  mask-gc				; GC used to draw mask

  instances				; list of xwem-osd-instance structs sorted by depth

  plist)				; User defined plist

(defsubst xwem-osd-put-prop (osd prop val)
  "In OSD's properties list put property PROP with value VAL."
  (setf (xwem-osd-plist osd)
        (plist-put (xwem-osd-plist osd) prop val)))
(put 'xwem-osd-put-prop 'lisp-indent-function 2)

(defsubst xwem-osd-get-prop (osd prop)
  "Return OSD's property PROP."
  (plist-get (xwem-osd-plist osd) prop))

(defsubst xwem-osd-rem-prop (osd prop)
  "Remove OSD's property PROP."
  (setf (xwem-osd-plist osd)
        (plist-remprop (xwem-osd-plist osd) prop)))

(defmacro xwem-osd-xwin-copy (osd)
  `(xwem-osd-get-prop ,osd 'xwin-copy))
(defsetf xwem-osd-xwin-copy (osd) (xwin)
  `(xwem-osd-put-prop ,osd 'xwin-copy ,xwin))


;;; Functions
(defun xwem-osd-event-handler (xdpy xwin xev)
  "On X display XDPY and window XWIN handle X Event XEV."
  (let* ((osd (xwem-osd-get-osd xwin))
         (keymap (xwem-osd-get-prop osd 'keymap)))
    (when (xwem-osd-p osd)
      (X-Event-CASE xev
        (:X-DestroyNotify
         (xwem-osd-destroy osd t))

        ((:X-KeyPress :X-ButtonPress :X-ButtonRelease)
         (when (keymapp keymap)
           (xwem-overriding-local-map keymap
             (xwem-dispatch-command-xevent xev)))))
      )))

(defmacro xwem-osd-get-instance (xwin)
  `(X-Win-get-prop ,xwin 'xwem-osd-instance))

(defmacro xwem-osd-set-instance (xwin osin)
  `(X-Win-put-prop ,xwin 'xwem-osd-instance ,osin))

;;; Instances operations
(defun xwem-osd-instance-event-handler (xdpy xwin xev)
  "On X display XDPY and window XWIN handle X Event XEV."
  (let* ((osin (xwem-osd-get-instance xwin))
         (keymap (xwem-osd-instance-get-prop osin 'keymap)))
    (when (xwem-osd-instance-p osin)
      (X-Event-CASE xev
        (:X-DestroyNotify
         (xwem-osd-instance-destroy osin t))

        ((:X-KeyPress :X-ButtonPress :X-ButtonRelease)
         (when (keymapp keymap)
           (xwem-overriding-local-map keymap
             (xwem-dispatch-command-xevent xev)))))
      )))

(defun xwem-osd-instance-destroy (osin &optional ddw)
  "Destroy osd instance OSIN."
  (let ((xdpy (xwem-osd-instance-xdpy osin))
        (osd (xwem-osd-instance-osd osin)))
    ;; Remove from instances list
    (setf (xwem-osd-instances osd)
          (delq osin (xwem-osd-instances osd)))

    ;; Release X resources
    (XFreeGC xdpy (xwem-osd-instance-xgc osin))
    (unless ddw
      (XDestroyWindow xdpy (xwem-osd-instance-xwin osin)))
    (XFreePixmap xdpy (xwem-osd-instance-xmask osin))

    (X-invalidate-cl-struct osin)
    (xwem-osd-redraw osd)))

(defun xwem-osd-instance-clear-xmask (osin)
  "Clear mask area of OSD instance."
  (let ((osd (xwem-osd-instance-osd osin)))
    (XFillRectangle (xwem-osd-xdpy osd) (xwem-osd-instance-xmask osin)
                    xwem-misc-mask-bgc
		    0 0 (xwem-osd-width osd) (xwem-osd-height osd))))

(defun xwem-osd-instance-add (osd type &rest plist)
  "In OSD add osd instance of TYPE and properties PLIST."
  (let* ((depth (or (plist-get plist :depth) 0))
         (color (or (plist-get plist :color)
                    (xwem-osd-get-prop osd 'color)
                    xwem-osd-default-color))
         (xdpy (xwem-osd-xdpy osd))
         (osin (make-xwem-osd-instance
                :type type :osd osd
                :plist (xwem-misc-merge-plists '(:x 0 :y 0) plist))))
    (setf (xwem-osd-instance-xwin osin)
	  (XCreateWindow xdpy (xwem-osd-xwin osd)
			 0 0 (xwem-osd-width osd) (xwem-osd-height osd)
			 0 nil nil nil
			 :override-redirect t
                         :background-pixel
                         (XAllocColor xdpy (XDefaultColormap xdpy)
                                      (xwem-make-color color))
                         :event-mask (when (plist-get plist :keymap)
                                       (Xmask-or XM-ButtonPress XM-ButtonRelease)))
          (xwem-osd-instance-xmask osin)
	  (XCreatePixmap xdpy (xwem-osd-instance-xwin osin) 1
			 (xwem-osd-width osd) (xwem-osd-height osd))
          (xwem-osd-instance-xgc osin)
          (XCreateGC xdpy (xwem-osd-instance-xmask osin)
                     :foreground 1.0 :background 0.0
                     :line-width (plist-get plist :line-width)
                     :font (if (plist-get plist :font)
                               (X-Font-get xdpy (plist-get plist :font))
                             (X-Gc-font (xwem-osd-gc osd)))))
    (xwem-osd-set-instance (xwem-osd-instance-xwin osin) osin)
    (xwem-osd-instance-clear-xmask osin)

    (push osin (xwem-osd-instances osd))
    (xwem-osd-instance-set-depth osin depth)

    (when (plist-get plist :keymap)
      (xwem-osd-instance-put-prop osin 'keymap (plist-get plist :keymap))
      (X-Win-put-prop (xwem-osd-instance-xwin osin) 'osd-instance osin)
      (X-Win-EventHandler-add-new (xwem-osd-instance-xwin osin)
                                  'xwem-osd-instance-event-handler))
    osin))

(put 'xwem-osd-instance-add 'lisp-indent-function 2)

(defun xwem-osd-instance-show (osin)
  "Show osd instance OSIN."
  (XMapWindow (xwem-osd-instance-xdpy osin) (xwem-osd-instance-xwin osin))
  (pushnew osin (xwem-osd-instances (xwem-osd-instance-osd osin)))
  (xwem-osd-instance-set-depth osin (xwem-osd-instance-get-prop osin :depth))
  (xwem-osd-instance-draw osin))

(defun xwem-osd-instance-hide (osin)
  "Hide osd instance OSIN"
  (XUnmapWindow (xwem-osd-instance-xdpy osin) (xwem-osd-instance-xwin osin))
  (let ((osd (xwem-osd-instance-osd osin)))
    (setf (xwem-osd-instances osd)
          (delq osin (xwem-osd-instances osd)))
    (xwem-osd-redraw osd)))

(define-xwem-deferred xwem-osd-instance-apply-xmask (osin)
  "Apply instances OSIN mask to life."
  (let ((xdpy (xwem-osd-instance-xdpy osin)))
    (X-XShapeMask xdpy (xwem-osd-instance-xwin osin)
                  X-XShape-Bounding X-XShapeSet 0 0 (xwem-osd-instance-xmask osin))))

(define-xwem-deferred xwem-osd-instance-draw (osin)
  "Draw OSD instance OSIN."
  (let ((xdpy (xwem-osd-instance-xdpy osin))
        (osd (xwem-osd-instance-osd osin)))
    (xwem-osd-instance-clear-xmask osin)

    (ecase (xwem-osd-instance-type osin)
      (text
       (let* ((x (xwem-osd-instance-get-prop osin :x))
              (y (xwem-osd-instance-get-prop osin :y))
              (string (xwem-osd-instance-get-prop osin :text))
              (fnt (X-Gc-font (xwem-osd-instance-xgc osin)))
              (yoff (- (X-Text-height xdpy fnt string)
                       (X-Text-descent xdpy fnt string))))
         (XDrawString xdpy (xwem-osd-instance-xmask osin)
                      (xwem-osd-instance-xgc osin)
                      x (+ y yoff) string)
         (XDrawString xdpy (xwem-osd-xmask osd) (xwem-osd-instance-xgc osin)
                      x (+ y yoff) string)))

      (line
       (let ((x0 (xwem-osd-instance-get-prop osin :x0))
             (y0 (xwem-osd-instance-get-prop osin :y0))
             (x1 (xwem-osd-instance-get-prop osin :x1))
             (y1 (xwem-osd-instance-get-prop osin :y1)))
         (XDrawLine xdpy (xwem-osd-instance-xmask osin) (xwem-osd-instance-xgc osin)
                    x0 y0 x1 y1)
         (XDrawLine xdpy (xwem-osd-xmask osd) (xwem-osd-instance-xgc osin)
                    x0 y0 x1 y1)))

      (dots
       (let ((type (xwem-osd-instance-get-prop osin :dots-type))
             (dots (xwem-osd-instance-get-prop osin :dots))
             (x (or (xwem-osd-instance-get-prop osin :x) 0))
             (y (or (xwem-osd-instance-get-prop osin :y)
                    (xwem-osd-height osd))))
         (xwem-diag-plot-dots type (xwem-osd-instance-xmask osin)
                              (xwem-osd-instance-xgc osin)
                              x y dots)
         (xwem-diag-plot-dots type (xwem-osd-xmask osd)
                              (xwem-osd-instance-xgc osin)
                              x y dots)
         ))

      (arc
       (let ((xarc (xwem-osd-instance-get-prop osin :xarc)))
         (XDrawArcs xdpy (xwem-osd-instance-xmask osin)
                    (xwem-osd-instance-xgc osin) (list xarc))
         (XDrawArcs xdpy (xwem-osd-xmask osd) (xwem-osd-instance-xgc osin)
                    (list xarc))))

      (rect
       (let ((x (xwem-osd-instance-get-prop osin :x))
             (y (xwem-osd-instance-get-prop osin :y))
             (width (xwem-osd-instance-get-prop osin :width))
             (height (xwem-osd-instance-get-prop osin :height))
             (fill-p (xwem-osd-instance-get-prop osin :fill)))
         (XDrawRectangles xdpy (xwem-osd-instance-xmask osin)
                          (xwem-osd-instance-xgc osin)
                          (list (make-X-Rect :x x :y y :width width :height height))
                          fill-p)
         (XDrawRectangles xdpy (xwem-osd-xmask osd) (xwem-osd-instance-xgc osin)
                          (list (make-X-Rect :x x :y y :width width :height height))
                          fill-p)))

      (pixmap
       (let ((pix (xwem-osd-instance-get-prop osin :pixmap))
             (mask (xwem-osd-instance-get-prop osin :mask))
             (x (xwem-osd-instance-get-prop osin :x))
             (y (xwem-osd-instance-get-prop osin :y)))
         (XCopyArea xdpy mask (xwem-osd-instance-xmask osin)
                    xwem-misc-mask-bgc 0 0
                    (X-Pixmap-width mask) (X-Pixmap-height mask)
                    0 0)
         (X-XShapeMask xdpy (xwem-osd-instance-xwin osin)
                       X-XShape-Bounding X-XShapeSet 0 0 (xwem-osd-instance-xmask osin))
         (XCopyArea xdpy mask (xwem-osd-xmask osd)
                    xwem-misc-mask-bgc 0 0
                    (X-Pixmap-width mask) (X-Pixmap-height mask)
                    x y)
         (XMoveResizeWindow xdpy (xwem-osd-instance-xwin osin) x y
                            (X-Pixmap-width mask) (X-Pixmap-height mask))
         (XSetWindowBackgroundPixmap xdpy (xwem-osd-instance-xwin osin) pix)))
      )

    (XMapWindow xdpy (xwem-osd-instance-xwin osin))
    (xwem-osd-instance-apply-xmask osin)
    (xwem-osd-apply-xmask osd)))

(defun xwem-osd-instance-set-color (osin new-color)
  "Set new color."
  (let ((xdpy (xwem-osd-instance-xdpy osin)))
    (xwem-osd-instance-put-prop osin :color new-color)
    (XSetWindowBackground xdpy (xwem-osd-instance-xwin osin)
                          (XAllocColor xdpy (XDefaultColormap xdpy)
                                       (xwem-make-color new-color)))
    (XClearArea xdpy (xwem-osd-instance-xwin osin)
		0 0 (xwem-osd-width (xwem-osd-instance-osd osin))
		(xwem-osd-height (xwem-osd-instance-osd osin)) nil)))

(defun xwem-osd-instance-set-depth (osin new-depth)
  "For OSD instance OSIN to depth to NEW-DEPTH."
  (xwem-osd-instance-put-prop osin :depth new-depth)

  ;; - Sort instances according to depth
  ;; - Install below sibling
  (setf (xwem-osd-instances (xwem-osd-instance-osd osin))
        (sort (xwem-osd-instances (xwem-osd-instance-osd osin))
              #'(lambda (s1 s2)
                  (< (xwem-osd-instance-get-prop s1 :depth)
                     (xwem-osd-instance-get-prop s2 :depth)))))

  (let ((siblings (xwem-osd-instances (xwem-osd-instance-osd osin)))
        below-sibl)
    (while siblings
      (if (>= (xwem-osd-instance-get-prop (car siblings) :depth) new-depth)
          (setq siblings nil)
        (setq below-sibl (car siblings)))
      (setq siblings (cdr siblings)))

    (when below-sibl
      (XConfigureWindow (xwem-osd-instance-xdpy osin)
                        (xwem-osd-instance-xwin osin)
                        :sibling (xwem-osd-instance-xwin below-sibl)
                        :stackmode X-Below))))

;;; OSD functions
;;;###autoload
(defun xwem-osd-create (xdpy x y width height &optional x-parent properties)
  "On X display XDPY create new xwem osd context with +X+Y/WIDTHxHEIGHT geometry on X-PARENT.
PROPERTIES is a plist for osd.  Supported properties are:
 
 'keymap     - Keymap for OSD.
 'stack-rank - Rank of OSD in windows stack."
  (let ((osd (make-xwem-osd :xdpy xdpy
                            :x x :y y :width width :height height
                            :plist properties))
        (keymap (plist-get properties 'keymap))
        (stack-rank (plist-get properties 'stack-rank)))
    (setf (xwem-osd-xwin osd)
          (XCreateWindow xdpy (or x-parent (XDefaultRootWindow xdpy))
                         x y width height 0 nil nil nil
                         :override-redirect t
                         :background-pixel (XBlackPixel xdpy)
                         :event-mask (Xmask-or XM-StructureNotify
                                               (if keymap
                                                   (Xmask-or XM-KeyPress
                                                             XM-ButtonPress
                                                             XM-ButtonRelease)
                                                 0))))
    ;; Apply STACK-RANK
    (when (numberp stack-rank)
      (setf (xwem-xwin-rank (xwem-osd-xwin osd)) stack-rank))

    ;; Create gc
    (setf (xwem-osd-gc osd)
	  (XCreateGC xdpy (xwem-osd-xwin osd)
                     :foreground (XAllocNamedColor xdpy (XDefaultColormap xdpy)
                                                   xwem-osd-default-color)
                     :font (X-Font-get xdpy xwem-osd-default-font)))

    (X-Win-put-prop (xwem-osd-xwin osd) 'osd-ctx osd)
    (X-Win-EventHandler-add-new (xwem-osd-xwin osd) 'xwem-osd-event-handler)

    (xwem-osd-create-mask osd)
    osd))

;;;###autoload
(defun xwem-osd-create-dock (xdpy width height &optional osd-props dockid dockgroup dockalign)
  "Create docked osd instance.
XDPY - Display.
WIDTH, HEIGHT - OSD Geometry.
DOCKID, DOCKGROUP and DOCKALIGN specifies how dock is placed in tray."
  (let ((osd (xwem-osd-create xdpy 0 0 width height nil osd-props)))
    (xwem-osd-clear osd)
    (xwem-XTrayInit xdpy (xwem-osd-xwin osd) dockid (or dockgroup "desktop") dockalign)
    osd))

(defun xwem-osd-get-osd (xwin)
  "Get osd context associated with XWIN."
  (and (X-Win-p xwin) (X-Win-get-prop xwin 'osd-ctx)))

(defun xwem-osd-clear-mask (osd)
  "Clear mask area of OSD context."
  (XFillRectangle (xwem-osd-xdpy osd) (xwem-osd-xmask osd)
                  xwem-misc-mask-bgc
		  0 0 (xwem-osd-width osd) (xwem-osd-height osd)))
  
(defun xwem-osd-create-mask (osd)
  "For xwem osd context OSD create mask pixmap."
  (let ((xdpy (xwem-osd-xdpy osd)))
    (setf (xwem-osd-xmask osd)
	  (XCreatePixmap xdpy (xwem-osd-xwin osd) 1 (xwem-osd-width osd)
			 (xwem-osd-height osd)))
    (unless (xwem-osd-mask-gc osd)
      (setf (xwem-osd-mask-gc osd)
            (XCreateGC xdpy (xwem-osd-xmask osd)
                       :foreground 1.0
                       :background 0.0
                       :font (X-Font-get xdpy xwem-osd-default-font))))
    (xwem-osd-clear-mask osd)))

(defun xwem-osd-set-height (osd new-height)
  "Set OSD's window height to NEW-HEIGHT."
  (setf (xwem-osd-height osd) new-height)
  (XResizeWindow (xwem-osd-xdpy osd)
		 (xwem-osd-xwin osd)
		 (xwem-osd-width osd)
		 (xwem-osd-height osd))

  (XFreePixmap (xwem-osd-xdpy osd) (xwem-osd-xmask osd))
  (xwem-osd-create-mask osd))

(defun xwem-osd-set-width (osd new-width)
  "Set OSD's window width to NEW-WIDTH."
  (setf (xwem-osd-width osd) new-width)
  (XResizeWindow (xwem-osd-xdpy osd)
		 (xwem-osd-xwin osd)
		 (xwem-osd-width osd)
		 (xwem-osd-height osd))

  (XFreePixmap (xwem-osd-xdpy osd) (xwem-osd-xmask osd))
  (xwem-osd-create-mask osd))

(defun xwem-osd-move (osd new-x new-y)
  "Change OSD's window position to NEW-X, NEW-Y."
  (XMoveWindow (xwem-osd-xdpy osd) (xwem-osd-xwin osd)
	       new-x new-y))

(defun xwem-osd-set-xwin-color (osd color-name)
  "Set background for OSD's window to COLOR-NAME."
  (let ((xdpy (xwem-osd-xdpy osd)))
    (XSetWindowBackground xdpy (xwem-osd-xwin osd)
			  (XAllocNamedColor xdpy (XDefaultColormap xdpy)
					    color-name))))

(defun xwem-osd-set-gc-color (osd color-name)
  "Set OSD's gc foreground color to COLOR-NAME."
  (let ((xdpy (xwem-osd-xdpy osd)))
    (XChangeGC xdpy (xwem-osd-gc osd)
               :foreground (XAllocNamedColor xdpy (XDefaultColormap xdpy) color-name))))

(defun xwem-osd-set-color (osd color-name)
  "Set both OSD's background and OSD's gc foreground color to COLOR-NAME."
  (let* ((xdpy (xwem-osd-xdpy osd))
	 (col (XAllocNamedColor xdpy (XDefaultColormap xdpy)
				color-name)))
    (xwem-osd-put-prop osd 'color color-name)
    (XSetWindowBackground xdpy (xwem-osd-xwin osd) col)
    (xwem-osd-clear-xwin osd)
    (XChangeGC xdpy (xwem-osd-gc osd) :foreground col)))

(defun xwem-osd-show (osd)
  "Show OSD's window."
  (xwem-osd-apply-xmask-1 osd)
  (XMapWindow (xwem-osd-xdpy osd) (xwem-osd-xwin osd))
  (xwem-misc-raise-xwin (xwem-osd-xwin osd))
  (setf (xwem-osd-state osd) 'shown))

(defun xwem-osd-hide (osd)
  "Hide OSD's window."
  (XUnmapWindow (xwem-osd-xdpy osd) (xwem-osd-xwin osd))
  (setf (xwem-osd-state osd) 'hidden))

(define-xwem-deferred xwem-osd-redraw (osd)
  "Redraw all OSD's instances."
  (when (xwem-osd-p osd)
    (xwem-osd-clear-mask osd)
    (mapc #'xwem-osd-instance-draw (xwem-osd-instances osd))))

(defun xwem-osd-destroy-instances (osd)
  "Destroy all instances in OSD."
  (mapc #'xwem-osd-instance-destroy (xwem-osd-instances osd))
  (setf (xwem-osd-instances osd) nil))

(defun xwem-osd-destroy (osd &optional already-destroyed)
  "Destroy OSD context."
  (xwem-osd-destroy-instances osd)

  (X-Win-EventHandler-rem (xwem-osd-xwin osd) 'xwem-osd-event-handler)
  (X-Win-rem-prop (xwem-osd-xwin osd) 'osd-ctx)
  
  (unless already-destroyed
    (XDestroyWindow (xwem-osd-xdpy osd) (xwem-osd-xwin osd)))
  (XFreePixmap (xwem-osd-xdpy osd) (xwem-osd-xmask osd))
  (XFreeGC (xwem-osd-xdpy osd) (xwem-osd-mask-gc osd))
  (XFreeGC (xwem-osd-xdpy osd) (xwem-osd-gc osd))

  (X-invalidate-cl-struct osd))

(defun xwem-osd-set-font (osd font-name)
  "In OSD's context set font to be FONT-NAME."
  (let* ((xdpy (xwem-osd-xdpy osd))
	 (gc (xwem-osd-gc osd))
	 (mgc (xwem-osd-mask-gc osd))
	 (font (X-Font-get xdpy font-name)))
    (XChangeGC xdpy mgc :font font)
    (XChangeGC xdpy gc :font font)))

(defun xwem-osd-char-width (osd)
  "Return width of OSD's window in characters."
  ;; XXX assumes font is width fixed
  (/ (xwem-osd-width osd)
     (X-Text-width (xwem-osd-xdpy osd) (X-Gc-font (xwem-osd-mask-gc osd)) "_")))

(define-xwem-deferred xwem-osd-apply-xmask (osd)
  "Apply OSD's mask to life."
  (X-XShapeMask (xwem-osd-xdpy osd) (xwem-osd-xwin osd)
		X-XShape-Bounding X-XShapeSet 0 0 (xwem-osd-xmask osd)))

(defun xwem-osd-clear-xwin (osd)
  "Clear contents of OSD's window."
  (XClearArea (xwem-osd-xdpy osd) (xwem-osd-xwin osd) 0 0
	      (xwem-osd-width osd) (xwem-osd-height osd) nil))

(defun xwem-osd-clear (osd)
  "Clear OSD window."
  (xwem-osd-destroy-instances osd)
  (xwem-osd-clear-mask osd)
  (xwem-osd-apply-xmask osd))

(defun xwem-osd-text (osd string)
  "In OSD's context show STRING.
If OSD has any instances, they will be destroyed."
  (let* ((xdpy (xwem-osd-xdpy osd))
	 (yoff (- (X-Text-height xdpy (X-Gc-font (xwem-osd-mask-gc osd)) string)
		  (X-Text-descent xdpy (X-Gc-font (xwem-osd-mask-gc osd)) string))))

    (xwem-osd-destroy-instances osd)
    ;; Update window shape
    (xwem-osd-clear-mask osd)
    (let ((fenc (xwem-misc-font-coding-system
                 (X-Font-name xdpy (X-Gc-font (xwem-osd-mask-gc osd))))))
      (setq string (encode-coding-string string fenc)))
    (XDrawString xdpy (xwem-osd-xmask osd) (xwem-osd-mask-gc osd)
		 0 yoff string)
    (xwem-osd-apply-xmask osd)))

(defun xwem-osd-color-text (osd strspec-list)
  "In OSD's win draw colored text specified by STRSPEC-LIST.
STRSPEC is list of cons cell where car is string and cdr is color."
  (xwem-osd-clear osd)
  (let ((curstr ""))
    (mapcar #'(lambda (strspec)
                (let* ((xdpy (xwem-osd-xdpy osd))
                       (str (concat curstr (car strspec)))
                       (yoff (- (X-Text-height xdpy (X-Gc-font (xwem-osd-mask-gc osd)) str)
                                (X-Text-descent xdpy (X-Gc-font (xwem-osd-mask-gc osd)) str))))
                  (xwem-osd-set-xwin-color osd (cdr strspec))

                  ;; Fix encoding of string
                  (let ((fenc (xwem-misc-font-coding-system
                               (X-Font-name
                                xdpy (X-Gc-font (xwem-osd-mask-gc osd))))))
                    (setq string (encode-coding-string string fenc)))

                  (XDrawString xdpy (xwem-osd-xmask osd) (xwem-osd-mask-gc osd)
                               0 yoff str)
                  (xwem-osd-apply-xmask osd)
                  (setq curstr (concat curstr (car strspec)))))
            strspec-list)))

(defun xwem-osd-instance-change (osin &rest props)
  "For OSD instance OSIN change properties specified by PROPS.
And redraw whole OSD."
  ;; Change depth and color in case they are given
  (let ((depth (plist-get props :depth))
        (color (plist-get props :color)))
    (when depth
      (xwem-osd-instance-set-depth osin depth))
    (when color
      (xwem-osd-instance-set-color osin color)))

  ;; Merge in new properties
  (while props
    (xwem-osd-instance-put-prop osin (first props) (second props))
    (setq props (cddr props)))

  ;; Change instance GC
  (let ((xdpy (xwem-osd-instance-xdpy osin))
        (xgc (xwem-osd-instance-xgc osin))
        (font (xwem-osd-instance-get-prop osin :font)))
    (XChangeGC xdpy xgc :line-width (xwem-osd-instance-get-prop osin :line-width))
    (when font
      (XChangeGC xdpy xgc :font (X-Font-get xdpy font))))

  ;; And redraw whole OSD
  (xwem-osd-redraw (xwem-osd-instance-osd osin)))

(defun xwem-osd-text-add (osd x y text &rest props)
  "In OSD's context at X Y coordinates add TEXT.
Supported PROPS are :depth, :color"
  (let ((osin (apply #'xwem-osd-instance-add osd 'text
                     :x x :y y :text text props)))
    (xwem-osd-instance-draw osin)
    osin))

(defun xwem-osd-line-add (osd x0 y0 x1 y1 &rest props)
  "In OSD's window add line.
Supported PROPS are :depth, :color, :line-width"
  (let ((osin (apply #'xwem-osd-instance-add osd 'line
                     :x0 x0 :y0 y0 :x1 x1 :y1 y1 props)))
    (xwem-osd-instance-draw osin)
    osin))

(defun xwem-osd-dots-add (osd dots-type dots &rest props)
  "In OSD's window add DOTS-TYPE DOTS.
Supported PROPS are :x, :y, :depth, :color"
  (let ((osin (apply #'xwem-osd-instance-add osd 'dots
                     :dots-type dots-type :dots dots props)))
    (xwem-osd-instance-draw osin)
    osin))

(defun xwem-osd-arc-add (osd xarc &rest props)
  "In OSD's window draw arc specified by XARC.
Supported PROPS are :depth, :color"
  (let ((osin (apply #'xwem-osd-instance-add osd 'arc
                     :xarc xarc props)))
    (xwem-osd-instance-draw osin)
    osin))

(defun xwem-osd-rect-add (osd x y width height &rest props)
  "In OSD's window add rectangle specified by X Y WIDTH and HEIGHT.
Supported PROPS are :depth, :color, :fill"
  (let ((osin (apply #'xwem-osd-instance-add osd 'rect
                     :x x :y y :width width :height height props)))
    (xwem-osd-instance-draw osin)
    osin))

(defun xwem-osd-pixmap-add (osd pixmap mask &rest props)
  "In OSD's window add pixmap icon.
Supported PROPS are :depth, :x, :y"
  (let ((osin (apply #'xwem-osd-instance-add osd 'pixmap
                     :pixmap pixmap :mask mask props)))
    (xwem-osd-instance-draw osin)
    osin))

(defun xwem-osd-icon-data-add (osd xpm-data &rest props)
  "In OSD's window add icon.
X and Y specifies osd instance location inside OSD(default is 0 0).
DEPTH specifies osd instance depth(default is 0).
XPM-DATA string of xpm image."
  (apply #'xwem-osd-pixmap-add osd
         (X:xpm-pixmap-from-data
          (xwem-osd-xdpy osd) (xwem-osd-xwin osd) xpm-data)
         (X:xpm-pixmap-from-data
          (xwem-osd-xdpy osd) (xwem-osd-xwin osd) xpm-data t)
         props))

(defun xwem-osd-icon-file-add (osd xpm-file &rest props)
  "Same as `xwem-osd-icon-data-add', but takes xpm image from FILE."
  (let (xpm-data)
    (with-temp-buffer
      (insert-file-contents-literally xpm-file)
      (setq xpm-data (buffer-substring)))
    (apply #'xwem-osd-icon-data-add osd xpm-data props)))

(defun xwem-osd-offscreen (osd)
  "Put OSD off the screen, displaying OSD copy.
Usefull to prevent flicking."
  (if (xwem-osd-xwin-copy osd)
      (XResizeWindow (xwem-osd-xdpy osd) (xwem-osd-xwin-copy osd)
                     (xwem-osd-width osd) (xwem-osd-height osd))
    (setf (xwem-osd-xwin-copy osd)
          (XCreateWindow (xwem-osd-xdpy osd) (xwem-osd-xwin osd)
                         0 0 (xwem-osd-width osd) (xwem-osd-height osd)
                         0 nil nil nil
                         :override-redirect t)))

  (XCopyArea (xwem-osd-xdpy osd)
             (xwem-osd-xwin osd) (xwem-osd-xwin-copy osd)
             (xwem-osd-gc osd)
             0 0 (xwem-osd-width osd) (xwem-osd-height osd)
             0 0)
  (XMapWindow (xwem-osd-xdpy osd) (xwem-osd-xwin-copy osd)))

(defun xwem-osd-commit (osd)
  "Commit changes made while OSD was in off screen."
  (XUnmapWindow (xwem-osd-xdpy osd) (xwem-osd-xwin-copy osd)))

;;; You might consider more powerfull `working' package, which is part
;;; of CEDET.
(defun xwem-osd-working-bar-display (tot-len percents)
  "Return a string with a bar-graph end percentile showing percentage.
TOT-LEN is the total length of bar.  PERCENTS is percentage state."
  (let* ((prstr (int-to-string percents))
	 (len (- tot-len (+ 10 (length prstr))))
	 (dcs (truncate (* len (/ percents 100.0))))
	 (tlen (- len dcs)))
    (concat ": ["
	    (make-string (if (> dcs 0) dcs 0) ?#)
	    (make-string (if (> tlen 0) tlen 0) ?.)
	    "] ... " prstr "%")))

(defun xwem-osd-working-percent-bar (osd prompt percents)
  "Display percentage with PERCENTS done bar prompting PROMPT."
  (require 'working)

  (let ((osdcw (xwem-osd-char-width osd)))
    (xwem-osd-text osd (concat prompt (xwem-osd-working-bar-display (- osdcw (length prompt)) percents)))))

;;; OSD notifiers
(require 'font)

(defvar xwem-osd-notifier-geometries
  '((alarm . ((80 . 500) . (2048 . 1024)))
    (default . ((80 . 80) . (2048 . 1024))))
  "List of coordinates for notifiers.")

(defvar xwem-osd-notifier-fonts
  (list (cons 'alarm (font-create-name (make-font :weight "bold" :size 64)))
        (cons 'default (font-create-name (make-font :weight "bold" :size 64))))
  "List of fonts for notifiers.")

(defvar xwem-osd-notifier-colors
  '((alarm . "red4")
    (default . "magenta"))
  "List of colors for notifiers.")

(defun xwem-osd-notifier-create (xdpy type message &rest props)
  "Create notifier of TYPE using MESSAGE.
PROPS is properties list.  Supported properties are:
:keymap, :icon-data, :icon-file, :color, :font."
  (let* ((geom (cdr (or (assq type xwem-osd-notifier-geometries)
                        (assq 'default xwem-osd-notifier-geometries))))
         (osd (xwem-osd-create
               xdpy (car (car geom)) (cdr (car geom))
               (car (cdr geom)) (cdr (cdr geom))
               nil (list 'keymap (or (plist-get props :keymap)
                                     (make-sparse-keymap)))))
         (font (cdr (or (assq type xwem-osd-notifier-fonts)
                        (assq 'default xwem-osd-notifier-fonts)))))
    (xwem-osd-set-font osd (or (plist-get props :font) font))
    (xwem-osd-set-color
     osd (or (plist-get props :color)
             (cdr (or (assq type xwem-osd-notifier-colors)
                      (assq 'default xwem-osd-notifier-colors)))))

    (let* ((icon-data (plist-get props :icon-data))
           (icon-file (plist-get props :icon-file))
           (glyph (and (or (and icon-file (file-exists-p icon-file))
                           icon-data)
                       (if (and icon-file (file-exists-p icon-file))
                           (make-glyph
                            (vector 'xpm :file icon-file))
                         (make-glyph
                          (vector 'xpm :data icon-data)))))
           (fh (font-height (make-font-specifier font)))
           (iy (when glyph
                 (if (> (glyph-height glyph) fh) 0
                   (/ (- fh (glyph-height glyph)) 2))))
           (ty (if (and glyph (> (glyph-height glyph) fh))
                   (/ (- (glyph-height glyph) fh) 2)
                 0))
           (tx (if glyph (+ (glyph-width glyph) 10) 0)))
      (cond ((and icon-file (file-exists-p icon-file))
             (xwem-osd-icon-file-add osd icon-file :y iy))
            (icon-data
             (xwem-osd-icon-data-add osd icon-data :y iy)))
      (xwem-osd-text-add osd tx ty
                         (encode-coding-string
                          message (xwem-misc-font-coding-system font)))
      osd)))


(provide 'xwem-osd)

;;; xwem-osd.el ends here
