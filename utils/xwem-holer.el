;;; xwem-holer.el --- Making holes in xwem frames.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Jan 15 12:39:04 MSK 2004
;; Keywords: xwem
;; Time-stamp: <30/11/2006 00:04:45 lg@h1>

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

;; This XWEM addon allow you to create/manipulate holes in XWEM
;; frames.
;;
;; Add something following to your ~/.xwem/xwemrc.el to start using
;; holer:
;; 
;;   (define-key xwem-global-map (xwem-kbd "H-x h") 'xwem-holer-prefix)
;; 
;; Note: in xwem2.0-rc2 binded by default to `H-x h'

;;; BUGS:
;;
;;  - You can create/manipulate holes only on selected frame.

;;; Code:

(require 'xlib-xshape)

(require 'xwem-load)
(require 'xwem-compat)
(require 'xwem-frame)

(defgroup xwem-holer nil
  "Group to customize xwem holer."
  :prefix "xwem-holer-"
  :group 'xwem)

(defcustom xwem-holer-outline-width 3
  "Width of outliner."
  :type 'number
  :group 'xwem-holer)

(defcustom xwem-holer-outline-color "blue2"
  "*Color of holer outliner."
  :type 'color
  :group 'xwem-holer)

(defcustom xwem-holer-move-cursor-shape 'X-XC-fleur
  "*Shape of cursor when moving holer."
  :type (xwem-cursor-shape-choice)
  :group 'xwem-holer)

(defcustom xwem-holer-move-cursor-foreground "#0000AA"
  "*Cursor's foreground when moving holer."
  :type 'color
  :group 'xwem-holer)

(defcustom xwem-holer-move-cursor-background "#000088"
  "*Cursor's background when moving holer."
  :type 'color
  :group 'xwem-holer)

(defcustom xwem-holer-resize-cursor-foreground "#0000AA"
  "*Cursor's foreground when resizing holer."
  :type 'color
  :group 'xwem-holer)

(defcustom xwem-holer-resize-cursor-background "#000088"
  "*Cursor's background when resizing holer."
  :type 'color
  :group 'xwem-holer)

(defcustom xwem-holer-min-pixels 10
  "*Minimum pixels to change holer geometry.
Set it higher value to speed up moving/resizing."
  :type 'number
  :group 'xwem-holer)

;;; Internal variables


;;; Define holer prefix map
;;;###autoload(autoload 'xwem-holer-prefix "xwem-holer" nil nil 'keymap)
(xwem-define-prefix-command 'xwem-holer-prefix t)
(defvar xwem-holer-map (symbol-function 'xwem-holer-prefix)
  "Keymap for holer (\\<xwem-global-map>\\[xwem-holer-prefix]) commands.
Bindings:
\\{xwem-holer-map}")

(define-key xwem-holer-map [button1] 'xwem-holer-imove-or-create)
(define-key xwem-holer-map [button2] 'xwem-holer-idestroy)
(define-key xwem-holer-map [button3] 'xwem-holer-iresize)
(define-key xwem-holer-map (xwem-kbd "h") 'xwem-holer-ihide)
(define-key xwem-holer-map (xwem-kbd "s") 'xwem-holer-ishow)
(define-key xwem-holer-map (xwem-kbd "d") 'xwem-holer-idestroy-all)


(defvar xwem-holers-list nil "List of all holers.")

(defstruct xwem-holer
  frame
  state				; 'shown or 'hidden
  x y width height

  click-xoff click-yoff		; offset within holer where click occured

  outliner-win			;
  xmask xmask-gc xmask-bgc

  mode				; nil, 'move, 'resize-bl,
				; 'resize-br, 'resize-tl or
				; 'resize-tr, 'hidden

  ;; cursors
  move-cursor
  resize-bl-cursor resize-br-cursor
  resize-tl-cursor resize-tr-cursor
  )

(defmacro xwem-holer-xdpy (holer)
  "Return HOLER's display."
  `(X-Win-dpy (xwem-frame-xwin (xwem-holer-frame holer))))

(defsubst xwem-holer-add (holer)
  "Add HOLER to the `xwem-holers-list'."
  (pushnew holer xwem-holers-list))

(defsubst xwem-holer-del (holer)
  "Remove HOLER from `xwem-holers-list'."
  (setq xwem-holers-list (delete holer xwem-holers-list)))

(defsubst xwem-holer-find-by-frame (frame)
  "Find holer by FRAME.
Return list of holers for FRAME."
  (let (holers)
    (mapc (lambda (h)
            (when (eq (xwem-holer-frame h) frame)
              (setq holers (cons h holers))))
          xwem-holers-list)
    holers))


(defun xwem-holer-clear (holer)
  "Clear HOLER's xmask."
  (let* ((xgeom (xwem-frame-xgeom (xwem-holer-frame holer)))
	 (width (X-Geom-width xgeom))
	 (height (X-Geom-height xgeom)))
    (XFillRectangle (xwem-holer-xdpy holer) (xwem-holer-xmask holer)
		    (xwem-holer-xmask-gc holer) 0 0 width height)))

(defun xwem-holer-show (holer)
  "Show holer on HOLER's frame."
  (setf (xwem-holer-state holer) 'shown)
  (XMapWindow (xwem-holer-xdpy holer) (xwem-holer-outliner-win holer))
  (xwem-holer-frame-redisplay (xwem-holer-frame holer)))

(defun xwem-holer-hide (holer)
  "Hide HOLER."
  (XUnmapWindow (xwem-holer-xdpy holer) (xwem-holer-outliner-win holer))
  (setf (xwem-holer-state holer) 'hidden)
  (xwem-holer-frame-redisplay (xwem-holer-frame holer)))

(define-xwem-deferred xwem-holer-frame-redisplay (frame)
  "Redisplay holes in FRAME."
  (let* ((xwin (xwem-frame-xwin frame))
	 (xdpy (X-Win-dpy xwin)))
    (X-XShapeMask xdpy xwin X-XShape-Bounding X-XShapeSet 0 0 nil)
    (mapc (lambda (h)
	    (when (eq (xwem-holer-state h) 'shown)
	      (X-XShapeMask xdpy xwin
			    X-XShape-Bounding X-XShapeIntersect
			    0 0 (xwem-holer-xmask h))))
	  (xwem-holer-find-by-frame frame))))

(defun xwem-holer-create (frame x y width height)
  "Create new holer for FRAME with geometry +X+Y+WIDTHxHEIGHT."

  (let* ((holer (make-xwem-holer :frame frame
				 :x x :y y
				 :width width :height height))
	 (xdpy (xwem-holer-xdpy holer))
	 (xgeom (xwem-frame-xgeom frame)))
    (setf (xwem-holer-xmask holer)
	  (XCreatePixmap xdpy (xwem-frame-xwin frame) 1
			 (X-Geom-width xgeom) (X-Geom-height xgeom)))
    (setf (xwem-holer-xmask-gc holer)
	  (XCreateGC xdpy (xwem-holer-xmask holer)
                     :foreground 1.0
                     :background 0.0))
    (setf (xwem-holer-xmask-bgc holer)
	  (XCreateGC xdpy (xwem-holer-xmask holer)
                     :foreground 0.0
                     :background 1.0))

    (xwem-holer-clear holer)
    (XFillRectangle xdpy (xwem-holer-xmask holer)
		    (xwem-holer-xmask-bgc holer)
		    x y width height)
    
    ;; Create cursors
    (setf (xwem-holer-move-cursor holer)
	  (xwem-make-cursor (eval xwem-holer-move-cursor-shape)
			    xwem-holer-move-cursor-foreground
			    xwem-holer-move-cursor-background))
    (setf (xwem-holer-resize-bl-cursor holer)
	  (xwem-make-cursor X-XC-bottom_left_corner
			    xwem-holer-resize-cursor-foreground
			    xwem-holer-resize-cursor-background))
    (setf (xwem-holer-resize-br-cursor holer)
	  (xwem-make-cursor X-XC-bottom_right_corner
			    xwem-holer-resize-cursor-foreground
			    xwem-holer-resize-cursor-background))
    (setf (xwem-holer-resize-tl-cursor holer)
	  (xwem-make-cursor X-XC-top_left_corner
			    xwem-holer-resize-cursor-foreground
			    xwem-holer-resize-cursor-background))
    (setf (xwem-holer-resize-tr-cursor holer)
	  (xwem-make-cursor X-XC-top_right_corner
			    xwem-holer-resize-cursor-foreground
			    xwem-holer-resize-cursor-background))

    ;; Outline holer
    (setf (xwem-holer-outliner-win holer)
	  (XCreateWindow xdpy (xwem-frame-xwin frame)
			 (- x xwem-holer-outline-width)
			 (- y xwem-holer-outline-width)
			 width height xwem-holer-outline-width
			 nil nil nil
			 :override-redirect t
                         :border-pixel
                         (XAllocNamedColor
                          xdpy (XDefaultColormap xdpy)
                          xwem-holer-outline-color)))
    (X-Win-put-prop (xwem-holer-outliner-win holer) 'xwem-holer holer)

    (xwem-holer-add holer)
    (xwem-holer-show holer)
    holer))

(defun xwem-holer-move (holer x y)
  "Move HOLER to X Y."
  (let ((xdpy (xwem-holer-xdpy holer)))
    (setf (xwem-holer-x holer) x)
    (setf (xwem-holer-y holer) y)

    (xwem-holer-clear holer)
    (XFillRectangle xdpy (xwem-holer-xmask holer) (xwem-holer-xmask-bgc holer)
		    (xwem-holer-x holer) (xwem-holer-y holer)
		    (xwem-holer-width holer) (xwem-holer-height holer))

    (XMoveWindow xdpy (xwem-holer-outliner-win holer)
		 (- x xwem-holer-outline-width)
		 (- y xwem-holer-outline-width))
    (xwem-holer-frame-redisplay (xwem-holer-frame holer))))

(defun xwem-holer-move-resize (holer x y width height)
  "Move and resize HOLER to X Y WIDTH HEIGHT geometry."
  (let ((xdpy (xwem-holer-xdpy holer)))
    (setf (xwem-holer-x holer) x)
    (setf (xwem-holer-y holer) y)
    (setf (xwem-holer-width holer) width)
    (setf (xwem-holer-height holer) height)

    (xwem-holer-clear holer)
    (XFillRectangle xdpy (xwem-holer-xmask holer) (xwem-holer-xmask-bgc holer)
		    (xwem-holer-x holer) (xwem-holer-y holer)
		    (xwem-holer-width holer) (xwem-holer-height holer))

    (XMoveResizeWindow xdpy (xwem-holer-outliner-win holer)
		       (- x xwem-holer-outline-width)
		       (- y xwem-holer-outline-width)
		       (xwem-holer-width holer)
		       (xwem-holer-height holer))
    (xwem-holer-frame-redisplay (xwem-holer-frame holer))))

(defun xwem-holer-destroy (holer)
  "Destroy HOLER."
  (let ((xdpy (xwem-holer-xdpy holer)))
    (xwem-holer-del holer)              ; remove from `xwem-holers-list'

    (XFreePixmap xdpy (xwem-holer-xmask holer))
    (XFreeGC xdpy (xwem-holer-xmask-gc holer))
    (XFreeGC xdpy (xwem-holer-xmask-bgc holer))

    ;; Finally destroy outliner window
    (X-Win-rem-prop (xwem-holer-outliner-win holer) 'xwem-holer)
    (XDestroyWindow xdpy (xwem-holer-outliner-win holer))

    (xwem-holer-frame-redisplay (xwem-holer-frame holer))))

;;; Subroutines using when resizing
(defun xwem-holer-change-mode-to-opposite (holer &optional width-p)
  "Change HOLER's resize mode to opposite when resizing."
  (let ((ww '(resize-tr resize-br resize-tl resize-bl))
	(hh '(resize-bl resize-tl resize-br resize-tr))
	(c (memq (xwem-holer-mode holer)
		 '(resize-tl resize-bl resize-tr resize-br))))
    (when c
      (setf (xwem-holer-mode holer)
	    (nth (- 4 (length c)) (if width-p ww hh))))))

(defun xwem-holer-calculate-new-geom (hl frx fry)
  "Return list '(x y width height) represented new geometry for holer HL."
  (let ((mode (xwem-holer-mode hl))
	x y w h)
    (cond ((eq mode 'resize-tl)
	   (setq x frx
		 y fry
		 w (+ (- (xwem-holer-x hl) x) (xwem-holer-width hl))
		 h (+ (- (xwem-holer-y hl) y) (xwem-holer-height hl))))
	  ((eq mode 'resize-bl)
	   (setq x frx
		 y (xwem-holer-y hl)	; didnt affected
		 h (- fry (xwem-holer-y hl))
		 w (+ (- (xwem-holer-x hl) x) (xwem-holer-width hl))))
	  ((eq mode 'resize-tr)
	   (setq x (xwem-holer-x hl)
		 y fry
		 w (- frx x)
		 h (+ (- (xwem-holer-y hl) y) (xwem-holer-height hl))))
	  ((eq mode 'resize-br)
	   (setq x (xwem-holer-x hl)
		 y (xwem-holer-y hl)
		 w (- frx (xwem-holer-x hl))
		 h (- fry (xwem-holer-y hl))))
	  )
    (list x y w h)))

(defun xwem-holer-event-handler (xdpy win xev)
  "Handle events come from root window."
  (X-Event-CASE xev
    (:X-MotionNotify
     (let ((hl (X-Win-get-prop win 'xwem-holer))
	   frxgeom frx fry)
       (when (xwem-holer-p hl)
	 (setq frxgeom (xwem-frame-xgeom (xwem-holer-frame hl)))
	 (setq frx (- (X-Event-xmotion-root-x xev) (X-Geom-x frxgeom)))
	 (setq fry (- (X-Event-xmotion-root-y xev) (X-Geom-y frxgeom)))
	 (cond ((eq (xwem-holer-mode hl) 'move)
		;; Translate root coordinates to frame coordinates.
		;; Using XTranslateCoordinates will slow down.
		(let ((x (- frx (xwem-holer-click-xoff hl)))
		      (y (- fry (xwem-holer-click-yoff hl))))
		  (when (or (>= (abs (- x (xwem-holer-x hl)))
				xwem-holer-min-pixels)
			    (>= (abs (- y (xwem-holer-y hl)))
				xwem-holer-min-pixels))
		    (xwem-holer-move hl x y))))

	       ;; Interactively resize holer
	       ((memq (xwem-holer-mode hl)
		      '(resize-bl resize-br resize-tl resize-tr))
		;; Calculate new geometry
		(let* ((ngeom (xwem-holer-calculate-new-geom hl frx fry))
		       (x (nth 0 ngeom))
		       (y (nth 1 ngeom))
		       (w (nth 2 ngeom))
		       (h (nth 3 ngeom)))

		  ;; When width or height is less then zero check is
		  ;; there need to change resize mode.
		  (when (or (< w 0) (< h 0))
		    (when (< w 0)
		      (xwem-holer-change-mode-to-opposite hl t))
		    (when (< h 0)
		      (xwem-holer-change-mode-to-opposite hl))
		    (setq ngeom (xwem-holer-calculate-new-geom hl frx fry)
			  x (nth 0 ngeom)
			  y (nth 1 ngeom)
			  w (nth 2 ngeom)
			  h (nth 3 ngeom)))

		  (when (and x y (> w 0) (> h 0)
			     (or (>= (abs (- w (xwem-holer-width hl)))
				     xwem-holer-min-pixels)
				 (>= (abs (- h (xwem-holer-height hl)))
				     xwem-holer-min-pixels)))
		    (xwem-holer-move-resize hl x y w h))))
	       ))))

    (:X-ButtonRelease
     (XUngrabPointer xdpy)
     (X-Win-EventHandler-rem (X-Event-xmotion-event xev)
			     'xwem-holer-event-handler)
     (let ((hl (X-Win-get-prop win 'xwem-holer)))
       (when (xwem-holer-p hl)
	 (setf (xwem-holer-mode hl) nil))))
    ))

(defun xwem-holer-find-frame (xev)
  "Using ButtonPress XEV find out xwem frame."
  (let* ((srx (X-Event-xbutton-root-x xev))
         (sry (X-Event-xbutton-root-y xev))
         (frame (or (xwem-xwin-frame (X-Event-xbutton-child xev))
                    (xwem-frame-at srx sry t))))
    frame))

(defun xwem-holer-find-holer (xev)
  "Using ButtonPress XEV find out holer under pointer."
  (when (= (X-Event-type xev) X-ButtonPress)
    (let* ((xdpy (X-Event-dpy xev))
	   (frame (xwem-holer-find-frame xev))
	   (chw (and (xwem-frame-p frame)
                     (cdr (XTranslateCoordinates
			   xdpy (XDefaultRootWindow xdpy)
			   (xwem-frame-xwin frame)
			   (X-Event-xbutton-root-x xev)
			   (X-Event-xbutton-root-y xev)))))
	   (hl (and (X-Win-p chw) (X-Win-get-prop chw 'xwem-holer))))
      (when (xwem-holer-p hl)
	(let ((tpnt (car (XTranslateCoordinates
                          xdpy (XDefaultRootWindow xdpy)
                          (xwem-holer-outliner-win hl)
                          (X-Event-xbutton-root-x xev)
			  (X-Event-xbutton-root-y xev)))))
	  (setf (xwem-holer-click-xoff hl) (X-Point-x tpnt))
	  (setf (xwem-holer-click-yoff hl) (X-Point-y tpnt))
	  hl)))))
	
(define-xwem-command xwem-holer-imove ()
  "Move holer."
  (xwem-interactive "_")

  (let* ((xev xwem-last-xevent)
	 (hl (xwem-holer-find-holer xev)))
    (when (xwem-holer-p hl)
      (setf (xwem-holer-mode hl) 'move)
      (XGrabPointer (X-Event-dpy xev)
		    (xwem-holer-outliner-win hl)
		    (Xmask-or XM-ButtonPress
			      XM-ButtonRelease XM-ButtonMotion)
		    (xwem-holer-move-cursor hl))
      (X-Win-EventHandler-add-new (xwem-holer-outliner-win hl)
				  'xwem-holer-event-handler))))

(define-xwem-command xwem-holer-imove-or-create ()
  "Move already existing holer or create new."
  (xwem-interactive "_")

  (let* ((xev xwem-last-xevent)
	 (hl (xwem-holer-find-holer xev)))
    (if (xwem-holer-p hl)
	(call-interactively 'xwem-holer-imove)

      ;; Create new holer
      (let ((frame (xwem-holer-find-frame xev))
	    frxgeom frx fry)
	
	(when (xwem-frame-p frame)
	  (setq frxgeom (xwem-frame-xgeom frame))
	  (setq frx (- (X-Event-xmotion-root-x xev) (X-Geom-x frxgeom)))
	  (setq fry (- (X-Event-xmotion-root-y xev) (X-Geom-y frxgeom)))

	  (xwem-holer-create frame (- frx 1) (- fry 1) 1 1)
	  ;; After this resizing should appear in 'resize-br mode
	  (call-interactively 'xwem-holer-iresize))))))
      
(define-xwem-command xwem-holer-iresize ()
  "Resize holer."
  (xwem-interactive "_")

  (let* ((xev xwem-last-xevent)
	 (hl (xwem-holer-find-holer xev)))
    (when (xwem-holer-p hl)
      (let ((cx (xwem-holer-click-xoff hl))
	    (cy (xwem-holer-click-yoff hl))
	    (hw (/ (xwem-holer-width hl) 2)) ; half of width
	    (hh (/ (xwem-holer-height hl) 2)) ; half of height
	    cursor)
	(cond ((and (> cx hw) (> cy hh))
	       (setf (xwem-holer-mode hl) 'resize-br)
	       (setq cursor (xwem-holer-resize-br-cursor hl)))

	      ((> cx hw)
	       (setf (xwem-holer-mode hl) 'resize-tr)
	       (setq cursor (xwem-holer-resize-tr-cursor hl)))

	      ((and (<= cx hw) (> cy hh))
	       (setf (xwem-holer-mode hl) 'resize-bl)
	       (setq cursor (xwem-holer-resize-bl-cursor hl)))

	      ((<= cx hw)
	       (setf (xwem-holer-mode hl) 'resize-tl)
	       (setq cursor (xwem-holer-resize-tl-cursor hl))))

	(XGrabPointer (X-Event-dpy xev)
		      (xwem-holer-outliner-win hl)
		      (Xmask-or XM-ButtonPress
				XM-ButtonRelease XM-ButtonMotion)
		      cursor)
	(X-Win-EventHandler-add-new (xwem-holer-outliner-win hl)
				    'xwem-holer-event-handler)
        (while (xwem-holer-mode hl)
          (dispatch-event (next-event)))
	))))

(define-xwem-command xwem-holer-idestroy (ev)
  "Destroy holer."
  (xwem-interactive (list xwem-last-event))

  (when (not (button-event-p ev))
    (error 'xwem-error "`xwem-holer-idestroy' must be bound to mouse event"))

  (let* ((xev xwem-last-xevent)
	 (hl (xwem-holer-find-holer xev)))
    (when (xwem-holer-p hl)
      (xwem-holer-destroy hl)

      (xwem-message 'info "Holler at %dx%d destroyed."
		    (X-Event-xbutton-root-x xev)
		    (X-Event-xbutton-root-y xev)))))

(define-xwem-command xwem-holer-ihide (frame)
  "Hide all holers for FRAME.
If FRAME is ommited - `xwem-frame-selected' assumed."
  (xwem-interactive (list (xwem-frame-selected)))
  (mapc 'xwem-holer-hide (xwem-holer-find-by-frame frame)))

(define-xwem-command xwem-holer-ishow (frame)
  "Show all holers for FRAME.
If FRAME is ommited - `xwem-frame-selected' assumed."
  (xwem-interactive (list (xwem-frame-selected)))
  (mapc 'xwem-holer-show (xwem-holer-find-by-frame frame)))

(define-xwem-command xwem-holer-idestroy-all (frame)
  "Destroy all holers for FRAME."
  (xwem-interactive (list (xwem-frame-selected)))
  (mapc 'xwem-holer-destroy (xwem-holer-find-by-frame frame)))


(provide 'xwem-holer)

;;; xwem-holer.el ends here
