;;; xwem-pager.el --- Simple frame pager.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Richard Klinda <ignotus@hixsplit.hu>
;;         Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Aug 18 08:05:09 MSD 2004
;; Keywords: xwem
;; Time-stamp: <24/12/2008 02:22:38 lg@h1.lan>

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

;; Simple dockapp to show xwem frames.  Somekind of extension of
;; xwem-framei.el
;; 
;; To start using it add:
;; 
;;    (add-hook 'xwem-after-init-hook 'xwem-pager)

;;; Code:

(require 'xwem-load)
(require 'xlib-xshape)

(defgroup xwem-pager nil
  "Group to customize xwem pager."
  :prefix "xwem-pager-"
  :group 'xwem-tray)

(defcustom xwem-pager-dim (cons '(2 . 3) '(4 . 4))
  "Minimum and maximum viewports to show at X and Y."
  :type '(cons (cons :tag "Minimum"
                     (number :tag "X")
                     (number :tag "Y"))
               (cons :tag "Maximum"
                     (number :tag "X")
                     (number :tag "Y")))
  :set (lambda (sym val)
         (set sym val)
         (when (xwem-pager-xwin)
           (xwem-pager-redimentionize)))
  :initialize 'custom-initialize-default
  :group 'xwem-pager)

(defcustom xwem-pager-prefer-horizontal t
  "*Non-nil mean pager will prefer horizontal increment when redimentinizing."
  :type 'boolean
  :set (lambda (sym val)
         (set sym val)
         (when (xwem-pager-xwin)
           (xwem-pager-redimentionize)))
  :initialize 'custom-initialize-default
  :group 'xwem-pager)

(defcustom xwem-pager-viewport-size '(12 . 6)
  "\(X . Y\) where X*Y pixel will represent one viewport."
  :type '(cons (number :tag "X")
               (number :tag "Y"))
  :set (lambda (sym val)
         (set sym val)
         (when (xwem-pager-xwin)
           (xwem-pager-redimentionize)))
  :initialize 'custom-initialize-default
  :group 'xwem-pager)

(defcustom xwem-pager-grid-p t
  "*Set to non-nil if you want visible grid."
  :type 'boolean
  :set (lambda (sym val)
         (set sym val)
         (when (xwem-pager-xwin)
           (xwem-pager-redraw (xwem-pager-xwin) t)))
  :initialize 'custom-initialize-default
  :group 'xwem-pager)

(define-xwem-face xwem-pager-face
  `(((selected) (:foreground "cyan4"))
    ((border selected) (:foreground "grey10"))
    ((deselected) (:foreground "grey55"))
    ((border deselected) (:foreground "grey35"))
    ((unavailable) (:foreground "grey80"))
    ((border unavailable) (:foreground "grey100")))
  "Face for pager."
  :group 'xwem-pager)

(defvar xwem-pager-keymap 
  (let ((map (make-sparse-keymap)))
    (define-key map [button1] 'xwem-pager-iswitch)
    (define-key map [button3] 'xwem-pager-popup-menu)
    map)
  "Keymap for pager operations.")


;; Macroses
(defvar xwem-pager-xwin nil
  "XWIN of xwem pager.")

;; Pager xwin
(defmacro xwem-pager-xwin (&optional xwin)
  "Return pager's dockapp X window."
  `(or ,xwin xwem-pager-xwin))
(defsetf xwem-pager-xwin () (xwin)
  `(setq xwem-pager-xwin ,xwin))
;; Pager pixmap
(defmacro xwem-pager-xpix (&optional xwin)
  "Return pager's dockapp X window."
  `(X-Win-get-prop (xwem-pager-xwin ,xwin) 'xwem-pager-xpixmap))
(defsetf xwem-pager-xpix (&optional xwin) (pix)
  `(X-Win-put-prop (xwem-pager-xwin ,xwin) 'xwem-pager-xpixmap ,pix))
;; Pager dimentions (X . Y
(defmacro xwem-pager-dim (&optional xwin)
  `(X-Win-get-prop (xwem-pager-xwin ,xwin) 'xwem-pager-dim))
(defsetf xwem-pager-dim (&optional xwin) (dim)
  `(X-Win-put-prop (xwem-pager-xwin ,xwin) 'xwem-pager-dim ,dim))


;; Functions
(define-xwem-deferred xwem-pager-redraw-for-frame (frame-num &optional xwin)
  "Redraw FRAME."
  (when (< frame-num (apply '* (xwem-pager-dim xwin)))
    ;; FRAME shows in pager
    (let* ((frame (nth frame-num xwem-frames-list))
           (dim (xwem-pager-dim xwin))
           (tags (cond ((not (xwem-frame-alive-p frame))
                        '(unavailable))
                       ((xwem-frame-selected-p frame)
                        '(selected))
                       (t '(deselected))))
           (gc (xwem-face-get-gc 'xwem-pager-face tags))
           (col (% frame-num (car dim)))
           (row (/ frame-num (car dim))))
      (XFillRectangle (xwem-dpy) (xwem-pager-xpix xwin) gc
                      (* col (car xwem-pager-viewport-size))
                      (* row (cdr xwem-pager-viewport-size))
                      (car xwem-pager-viewport-size)
                      (cdr xwem-pager-viewport-size))
      (when xwem-pager-grid-p
        (XDrawRectangle (xwem-dpy) (xwem-pager-xpix xwin)
                        (xwem-face-get-gc 'xwem-pager-face `(border ,@tags))
                        (* col (car xwem-pager-viewport-size))
                        (* row (cdr xwem-pager-viewport-size))
                        (1- (car xwem-pager-viewport-size))
                        (1- (cdr xwem-pager-viewport-size))))
      (xwem-pager-redraw (xwem-pager-xwin xwin)))))

(define-xwem-deferred xwem-pager-redraw (xwin &optional full)
  "Redraw pager XWIN."
  (let* ((geom (XGetGeometry (xwem-dpy) (xwem-pager-xpix xwin)))
         (w (X-Geom-width geom))
         (h (X-Geom-height geom)))
    (if (not full)
        (XCopyArea (xwem-dpy) (xwem-pager-xpix xwin) xwin
                   (XDefaultGC (xwem-dpy)) 0 0 w h 0 0)

      (XFillRectangle (xwem-dpy) (xwem-pager-xpix xwin)
                      (XDefaultGC (xwem-dpy))
                      0 0 w h)
      (loop for fnum from 0 to (apply '* (xwem-pager-dim xwin))
        do (xwem-pager-redraw-for-frame-1 fnum xwin)))))

(defun xwem-pager-redimentionize (&optional non-used-argument xwin)
  "Check is pager need redimentionisation."
  (let ((frames (length xwem-frames-list))
        (min-ddim (list (car (cdr xwem-pager-dim))
                        (cdr (cdr xwem-pager-dim)))))
    ;; Calculate new dimention
    (mapc (lambda (ddim)
            (let ((dval (- (apply '* ddim) frames))
                  (mval (- (apply '* min-ddim) frames)))
              ;; Change min-ddim only if:
              ;;  - DVAL is positive or zero and MVAL is negative
              ;;  - DVAL and MVAL of same sign and DVAL abs is lesser
              ;;  - DVAL and MVAL of same sign and DVAL is equal to MVAL, but
              ;;    DVAL's X and Y components differs lesser.
              (when (or (and (>= dval 0) (< mval 0))
                        (and (or (zerop dval)
                                 (= (signum dval) (signum mval)))
                             (or (< (abs dval) (abs mval))
                                 (and (= (abs dval) (abs mval))
                                      (< (abs (- (car ddim) (cadr ddim)))
                                         (abs (- (car min-ddim)
                                                 (cadr min-ddim)))))
                                 (and xwem-pager-prefer-horizontal
                                      (= (abs dval) (abs mval))
                                      (= (abs (- (car ddim) (cadr ddim)))
                                         (abs (- (car min-ddim)
                                                 (cadr min-ddim))))
                                      (> (car ddim) (car min-ddim))))))
                (setq min-ddim ddim))))
          ;; Create a list of all possible dimentions
          (loop for i from (car (car xwem-pager-dim))
            to (car (cdr xwem-pager-dim))
            nconc (loop for j from (cdr (car xwem-pager-dim))
                    to (cdr (cdr xwem-pager-dim))
                    collect (list i j))))

    (setf (xwem-pager-dim xwin) min-ddim)
    (let ((w (* (car min-ddim) (car xwem-pager-viewport-size)))
          (h (* (cadr min-ddim) (cdr xwem-pager-viewport-size))))
      (XResizeWindow (xwem-dpy) (xwem-pager-xwin xwin) w h)
      ;; Recreate pixmap (if necessary)
      (when (xwem-pager-xpix xwin)
        (XFreePixmap (xwem-dpy) (xwem-pager-xpix xwin)))
      (setf (xwem-pager-xpix xwin)
            (XCreatePixmap (xwem-dpy) (xwem-pager-xwin xwin)
                           (XDefaultDepth (xwem-dpy)) w h))
      (when xwem-misc-turbo-mode
        (XSetWindowBackgroundPixmap (xwem-dpy) (xwem-pager-xwin xwin)
                                    (xwem-pager-xpix xwin))))
    (xwem-pager-redraw (xwem-pager-xwin xwin) t)))

(defun xwem-pager-frame-redraw (&optional frame)
  "Update xwem pager dockapp, because FRAME just selected/deselected."
  (unless frame (setq frame (xwem-frame-selected)))
  (xwem-pager-redraw-for-frame (xwem-frame-num frame)))

(defun xwem-pager-remove (xwin &optional need-destroy)
  "Destroy pager's XWIN."
  (XFreePixmap (xwem-dpy) (xwem-pager-xpix xwin))
  (setf (xwem-pager-xpix xwin) nil)

  ;; Remove pager events handler
  (X-Win-EventHandler-rem xwin 'xwem-pager-event-handler)

  ;; Destroy pager xwin if needed
  (when need-destroy
    (XDestroyWindow (xwem-dpy) xwin))

  ;; Unset default pager xwin
  (when (eq (xwem-pager-xwin) xwin)
    (setf (xwem-pager-xwin) nil))

  (remove-hook 'xwem-frame-select-hook 'xwem-pager-frame-redraw)
  (remove-hook 'xwem-frame-deselect-hook 'xwem-pager-frame-redraw)
  (remove-hook 'xwem-frame-creation-hook 'xwem-pager-redimentionize)
  (remove-hook 'xwem-frame-destroy-hook 'xwem-pager-redimentionize))

(defun xwem-pager-event-handler (xdpy xwin xev)
  "X Events handler for xwem pager dockapp."
  (X-Event-CASE xev
    (:X-Expose
     (xwem-pager-redraw xwin))
    (:X-DestroyNotify
     (xwem-pager-remove xwin))
    ((:X-ButtonPress :X-ButtonRelease)
     (let ((xwem-override-local-map xwem-pager-keymap))
       (xwem-dispatch-command-xevent xev)))))

;;;###autoload
(defun xwem-pager (&optional dockid dockgroup dockalign)
  "Start xwem pager dockapp.
DOCKID, DOCKGROUP and DOCKALIGN specifies pager placement in xwem
tray."
  (interactive)
  (let* ((pwin (XCreateWindow
                (xwem-dpy) nil 0 0 1 1 0
                nil nil nil
                :event-mask
                (Xmask-or XM-Exposure XM-StructureNotify
                          XM-ButtonPress XM-ButtonRelease)
                :override-redirect t)))
    ;; Set default pager window
    (unless (X-Win-p (xwem-pager-xwin))
      (setf (xwem-pager-xwin) pwin))

    ;; Initialize sizes and stuff
    (xwem-pager-redimentionize nil pwin)

    ;; Install events handler
    (X-Win-EventHandler-add pwin 'xwem-pager-event-handler nil
                            (list X-Expose X-DestroyNotify
                                  X-ButtonPress X-ButtonRelease))

    ;; Initialize wd in sys tray
    (xwem-XTrayInit (xwem-dpy) pwin dockid (or dockgroup "desktop") dockalign)

    (add-hook 'xwem-frame-select-hook 'xwem-pager-frame-redraw)
    (add-hook 'xwem-frame-deselect-hook 'xwem-pager-frame-redraw)
    (add-hook 'xwem-frame-creation-hook 'xwem-pager-redimentionize)
    (add-hook 'xwem-frame-destroy-hook 'xwem-pager-redimentionize)

    pwin))

(defun xwem-pager-frame-at (xwin x y)
  "Return frame that is under X Y position in XWIN pager."
  (let* ((dim (xwem-pager-dim xwin))
         (col (/ x (car xwem-pager-viewport-size)))
         (row (/ y (cdr xwem-pager-viewport-size)))
         (num (+ (* row (car dim)) col)))
    (nth num xwem-frames-list)))

;; Commands
(define-xwem-command xwem-pager-iswitch (ev)
  "Switch to frame."
  (xwem-interactive (list xwem-last-event))
  (unless (button-event-p ev)
    (error 'xwem-error
           "`xwem-pager-iswitch-frame' must be bound to mouse event"))
  (let ((frame (xwem-pager-frame-at
                (X-Event-win xwem-last-xevent)
                (X-Event-xbutton-event-x xwem-last-xevent)
                (X-Event-xbutton-event-y xwem-last-xevent))))
    (when (xwem-frame-p frame)
      (xwem-select-frame frame))))

(define-xwem-command xwem-pager-popup-menu (ev)
  "Popup menu."
  (xwem-interactive (list xwem-last-event))
  (unless (button-event-p ev)
    (error 'xwem-error
           "`xwem-pager-popup-menu' must be bound to mouse event"))
  (xwem-popup-menu
   (list "Pager"
         "---"
         (vector "Destroy"
                 `(xwem-pager-remove ,(X-Event-win xwem-last-xevent) t)))))


;;;###autoload
(defun xwem-pager-make-frames ()
  "Make the frames, call from XWEM-AFTER-INIT-HOOK!"
  (dotimes (it (- (* (car (cdr xwem-pager-dim))
                     (cdr (cdr xwem-pager-dim)))
                  (length (xwem-frames-list 'desktop))))
    (xwem-make-frame-1 'desktop :noselect t)))

;;;###autoload(autoload 'xwem-pager-move-up "xwem-pager" nil t)
(define-xwem-command xwem-pager-move-up (&optional arg)
  "Move one viewport up."
  (xwem-interactive "p")
  (xwem-pager-move 'up arg))

;;;###autoload(autoload 'xwem-pager-move-down "xwem-pager" nil t)
(define-xwem-command xwem-pager-move-down (&optional arg)
  "Move one viewport down."
  (xwem-interactive "p")
  (xwem-pager-move 'down arg))

;;;###autoload(autoload 'xwem-pager-move-left "xwem-pager" nil t)
(define-xwem-command xwem-pager-move-left (&optional arg)
  "Move one viewport left."
  (xwem-interactive "p")
  (xwem-pager-move 'left arg))

;;;###autoload(autoload 'xwem-pager-move-right "xwem-pager" nil t)
(define-xwem-command xwem-pager-move-right (&optional arg)
  "Move one viewport right."
  (xwem-interactive "p")
  (xwem-pager-move 'right arg))

(defun xwem-pager-move (dir &optional arg)
  "Generic function to move to frame in DIR direction.
DIR is one of `up', `down', `right' or `left'."
  (unless arg (setq arg 1))
  (xwem-frame-switch-nth
   (case dir
    (up (- (xwem-frame-num (xwem-frame-selected))
           (* arg (car (xwem-pager-dim)))))
    (down (+ (xwem-frame-num (xwem-frame-selected))
             (* arg (car (xwem-pager-dim)))))
    (left (- (xwem-frame-num (xwem-frame-selected)) arg))
    (right (+ (xwem-frame-num (xwem-frame-selected)) arg)))))

;;;###autoload
(defun xwem-pager-install-bindings ()
  "Install default bindings for pager commands."
  (xwem-global-set-key [(super h)] 'xwem-pager-move-left)
  (xwem-global-set-key [(super t)] 'xwem-pager-move-down)
  (xwem-global-set-key [(super n)] 'xwem-pager-move-up)
  (xwem-global-set-key [(super s)] 'xwem-pager-move-right))


(provide 'xwem-pager)

;;; xwem-pager.el ends here
