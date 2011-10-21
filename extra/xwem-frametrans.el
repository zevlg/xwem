;;; xwem-frametrans.el --- Transparency frames support.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Dec  2 10:35:14 MSK 2004
;; Keywords: xwem
;; Time-stamp: <30/11/2006 00:01:24 lg@h1>

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

;; Not yet fully functional.  To start using it do:
;;
;;   (add-hook 'xwem-frame-creation-hook 'xwem-ft-mask-init)
;;
;; It will apply masking to every newly created frame.
;;
;; For masking on already existing frame, do something like:
;;
;;   H-: (xwem-ft-mask-init (xwem-frame-selected)) RET

;;; Code:


(require 'xwem-load)
(require 'xlib-xshape)
(require 'xwem-frame)

(defstruct xwem-frame-ft
  frame
  mask
  gc
  bgc
  saved-height
  saved-width

  plist)

(defmacro xwem-frame-ft-get-prop (xff prop)
  `(plist-get (xwem-frame-ft-plist ,xff) ,prop))
(defmacro xwem-frame-ft-rem-prop (xff prop)
  `(setf (xwem-frame-ft-plist ,xff)
         (plist-remprop (xwem-frame-ft-plist ,xff) ,prop)))
(defmacro xwem-frame-ft-set-prop (xff prop val)
  `(if ,val
       (setf (xwem-frame-ft-plist ,xff)
             (plist-put (xwem-frame-ft-plist ,xff) ,prop ,val))
     (xwem-frame-ft-rem-prop ,xff ,prop)))


;;; Functions
(define-xwem-deferred xwem-ft-fill-mask (frame)
  "Fill the FRAME with mask."
  (let* ((xff (and (xwem-frame-p frame)
                   (xwem-frame-get-prop frame 'xwem-frame-ft)))
         (mask (and xff (xwem-frame-ft-mask xff)))
         (xgc (and xff (xwem-frame-ft-gc xff)))
         (xmgc (and xff (xwem-frame-ft-bgc xff))))
;         (plist (and xff (xwem-frame-ft-plist xff))))
    (when (and (X-Pixmap-p mask)
               (X-Gc-p xgc)
               (X-Gc-p xmgc))
      (XFillRectangle (xwem-dpy) mask xgc 0 0
                      (xwem-frame-width frame) (xwem-frame-height frame))
      (xwem-win-map #'(lambda (w)
                        (XFillRectangle (xwem-dpy) mask xmgc
                                        (+ (xwem-win-x w)
                                           (xwem-win-border-width w))
                                        (+ (xwem-win-y w)
                                           (xwem-win-border-width w))
                                        (- (xwem-win-width w)
                                           (xwem-win-border-width w)
                                           (xwem-win-border-width w))
                                        (- (xwem-win-height w)
                                           (xwem-win-border-width w)
                                           (xwem-win-border-width w)))
                        (let ((cl (xwem-win-cl w))
                              clg)
                          (when (and (xwem-cl-p cl) (xwem-cl-active-p cl))
                            (setq clg (xwem-cl-xgeom cl))
                            (XFillRectangle (xwem-dpy) mask xgc
                                            (X-Geom-x clg)
                                            (X-Geom-y clg)
                                            (X-Geom-width-with-borders clg)
                                            (X-Geom-height-with-borders clg)))))
                    (xwem-frame-selwin frame))
      (X-XShapeMask (xwem-dpy) (xwem-frame-xwin frame)
                    X-XShape-Bounding X-XShapeSet 0 0 mask))))

(defun xwem-ft-mask-init (frame &optional ft-properties)
  "Initialize transparency mask for FRAME."
  (unless (and (xwem-frame-p frame)
               (xwem-frame-get-prop frame 'xwem-frame-ft))
    (let* ((xpx (XCreatePixmap (xwem-dpy) (xwem-frame-xwin frame) 1
                               (xwem-frame-width frame)
                               (xwem-frame-height frame)))
           (gc xwem-misc-mask-fgc)
           (bgc xwem-misc-mask-bgc))

      (XFillRectangle (xwem-dpy) xpx gc 0 0
                      (xwem-frame-width frame)
                      (xwem-frame-height frame))
      (xwem-frame-put-prop frame 'xwem-frame-ft
        (make-xwem-frame-ft :frame frame
                            :mask xpx
                            :gc gc
                            :bgc bgc
                            :saved-height (xwem-frame-height frame)
                            :saved-width (xwem-frame-width frame)
                            :plist ft-properties))
      (xwem-ft-fill-mask frame))))

(define-xwem-deferred xwem-ft-mask-resize (frame)
  "Resize FRAME's transparency mask."
  (let ((xff (and (xwem-frame-p frame)
                  (xwem-frame-get-prop frame 'xwem-frame-ft))))
    (when xff
      (when (or (not (xwem-frame-ft-saved-height xff))
                (not (xwem-frame-ft-saved-width xff))
                (> (xwem-frame-width frame)
                   (xwem-frame-ft-saved-width xff))
                (> (xwem-frame-height frame)
                   (xwem-frame-ft-saved-height xff)))
        ;; Recreate pixmap
        (XFreePixmap (xwem-dpy) (xwem-frame-ft-mask xff))

        (setf (xwem-frame-ft-mask xff)
              (XCreatePixmap
               (xwem-dpy) (xwem-frame-xwin frame) 1 (xwem-frame-width frame)
               (xwem-frame-height frame)))
        (setf (xwem-frame-ft-saved-width xff) (xwem-frame-width frame))
        (setf (xwem-frame-ft-saved-height xff) (xwem-frame-height frame))
        ))))

(defun xwem-ft-mask-deinit (frame)
  "Denitialize transparency mask for FRAME."
  (let* ((xff (and (xwem-frame-p frame)
		   (xwem-frame-get-prop frame 'xwem-frame-ft)))
	 (xpx (and xff (xwem-frame-ft-mask xff))))
    (xwem-frame-rem-prop frame 'xwem-frame-ft)
    (when xpx
      (XFreePixmap (xwem-dpy) xpx))
    (X-XShapeMask (xwem-dpy) (xwem-frame-xwin frame)
                  X-XShape-Bounding X-XShapeSet 0 0 nil)))

(defun xwem-frame-set-transparency (frame prop val)
  "Set frame transparency."
  (xwem-frame-put-prop frame prop val)

  (if val
      (xwem-ft-mask-init frame)
    (xwem-ft-mask-deinit frame)))

(defun xwem-frame-get-transparency (frame prop)
  "Return FRAME's transparency."
  (xwem-frame-get-prop frame prop))

;;;; Commands
;;;###autoload(autoload 'xwem-frame-transparency "xwem-frametrans" nil t)
(define-xwem-command xwem-frame-transparency (frame &optional toggle)
  "Toggle transparency for FRAME.
If TOGGLE is positive number - enable.
If TOGGLE is negative number - disable."
  (xwem-interactive (list (xwem-frame-selected)
                          xwem-prefix-arg))

  (xwem-frame-set-property
   frame 'transparency
   (if (null toggle)
       (not (xwem-frame-property frame 'transparency))
     (> (prefix-numeric-value toggle) 0))))


(provide 'xwem-frametrans)

;; On-load actions:
(add-hook 'xwem-frame-resize-hook 'xwem-ft-mask-resize)

(defadvice xwem-win-set-cl (after trans-frame activate)
  "Fill frame when window changes its client."
  (xwem-ft-fill-mask (xwem-win-frame (ad-get-arg 0))))

(defadvice xwem-win-redraw-win-1 (after trans-frame activate)
  "Fill frame transparency mask."
  (xwem-ft-fill-mask (xwem-win-frame (ad-get-arg 0))))

(define-xwem-frame-property transparency
  "Make frame to be transparent."
  :type 'boolean
  :set 'xwem-frame-set-transparency
  :get 'xwem-frame-get-transparency)

;;; xwem-frametrans.el ends here
