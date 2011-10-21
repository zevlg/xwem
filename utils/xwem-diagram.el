;;; xwem-diagram.el --- Diagrams drawing for XWEM.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sat Mar  6 17:09:58 MSK 2004
;; Keywords: xwem
;; Time-stamp: <30/11/2006 00:04:40 lg@h1>

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

;; Diagrams drawer, supports:
;; 
;;   * Plain bar digrams.
;;   * 3D bar digrams.
;;   * Plain sectors diagrams.
;;   * 3D sectors diagrams.

;;   * Many gnuplot like datasets drawing.

;;; Code:

(require 'xlib-xlib)

(require 'xwem-faces)


(defun xwem-diag-dot-distance (dot1 dot2)
  "Return distance betwean DOT1 and DOT2."
  (let ((w (abs (- (X-Point-x dot1) (X-Point-x dot2))))
        (h (abs (- (X-Point-y dot1) (X-Point-y dot2)))))

    (sqrt (+ (* h h) (* w h)))))

(defun xwem-diag-dot-betwean-p (x-or-y dot dot1 dot2)
  "Return non-nil if X-OR-Y of DOT betwean DOT1 and DOT2.
X-OR-Y can be 'x or 'y."
  (if (eq x-or-y 'x)
      (or (and (>= (X-Point-x dot) (X-Point-x dot1))
               (<= (X-Point-x dot) (X-Point-x dot2)))
          (and (<= (X-Point-x dot) (X-Point-x dot1))
               (>= (X-Point-x dot) (X-Point-x dot2))))

    (or (and (>= (X-Point-y dot) (X-Point-y dot1))
             (<= (X-Point-y dot) (X-Point-y dot2)))
        (and (<= (X-Point-y dot) (X-Point-y dot1))
             (>= (X-Point-y dot) (X-Point-y dot2))))))

(defun xwem-diag-dot-< (x-or-y dot dot1)
  "Return non-nil if X-OR-Y of DOT is < DOT1."
  (if (eq x-or-y 'x)
      (< (X-Point-x dot) (X-Point-x dot1))
    (< (X-Point-y dot) (X-Point-y dot1))))

(defun xwem-diag-dot-<= (x-or-y dot dot1)
  "Return non-nil if X-OR-Y of DOT is <= DOT1."
  (if (eq x-or-y 'x)
      (<= (X-Point-x dot) (X-Point-x dot1))
    (<= (X-Point-y dot) (X-Point-y dot1))))

(defun xwem-diag-dot-> (x-or-y dot dot1)
  "Return non-nil if X-OR-Y of DOT is > DOT1."
  (if (eq x-or-y 'x)
      (> (X-Point-x dot) (X-Point-x dot1))
    (> (X-Point-y dot) (X-Point-y dot1))))

(defun xwem-diag-dot->= (x-or-y dot dot1)
  "Return non-nil if X-OR-Y of DOT is >= DOT1."
  (if (eq x-or-y 'x)
      (>= (X-Point-x dot) (X-Point-x dot1))
    (>= (X-Point-y dot) (X-Point-y dot1))))
    
(defun xwem-diag-calc-arc-dot-at (cnt-dot w h a)
  "Calculte dot position."
  (let* ((ra (/ (* a pi) 180))
         (cra (cos ra))
         (sra (sin ra))
         (rx (* w cra))
         (ry (* h sra)))
    (cons (round (+ (X-Point-x cnt-dot) rx))
          (round (+ (X-Point-y cnt-dot) (- ry))))))

(defun xwem-diag-draw-rect (d gc dot1 dot2 dot3 dot4 &optional fill-gc)
  "Draw parallelogram with vertexes at DOT1 DOT2 DOT3 and DOT4."
  (when (X-Gc-p fill-gc)
    (XFillPoly (X-Drawable-dpy d) d fill-gc (list dot1 dot2 dot3 dot4)))

  (XDrawLines (X-Drawable-dpy d) d gc (list dot1 dot2 dot3 dot4 dot1)))

(defun xwem-diag-calc-sector-dots (x y w h a1 a2)
  (let* ((mcnt (cons (+ x (/ w 2))
                     (+ y (/ h 2))))
         (d1 (xwem-diag-calc-arc-dot-at mcnt (/ w 2) (/ h 2) a1))
         (d2 (xwem-diag-calc-arc-dot-at mcnt (/ w 2) (/ h 2) (+ a2 a1))))
    (list d1 mcnt d2)))

(defun xwem-diag-draw-sector (d gc x y w h a1 a2 &optional fill-gc)
  "Draw sector, return new dots."
  (let ((dots (xwem-diag-calc-sector-dots x y w h a1 a2)))

    (when (X-Gc-p fill-gc)
      (XFillArc (X-Drawable-dpy d) d fill-gc x y w h a1 a2))

    (XDrawLines (X-Drawable-dpy d) d gc dots)
    (XDrawArc (X-Drawable-dpy d) d gc x y w h a1 a2)
    dots))

(defun xwem-diag-calc-butt-center (ds1 ds2)
  "Evil stuff."
  (let* ((h1 (- (X-Point-x (nth 2 ds1))
                (X-Point-x (nth 1 ds1))))
         (h2 (- (X-Point-x (nth 0 ds1))
                (X-Point-x (nth 1 ds1))))
         (l (* (/ (float h1) h2)
               (- (X-Point-y (nth 1 ds2))
                  (X-Point-y (nth 0 ds2)))))
         (L (+ (- (X-Point-y (nth 2 ds1))
                  (X-Point-y (nth 1 ds2)))
               l))
         (a (- (X-Point-y (nth 1 ds2))
               (X-Point-y (nth 1 ds1))))

         (T (xwem-diag-dot-distance
             (nth 1 ds2) (cons (X-Point-x (nth 2 ds1))
                               (+ (X-Point-y (nth 1 ds2))
                                  (truncate l)))))
         (tt (/ (* T a) (+ L a)))
         
         (nx (* h1 (/ tt T)))
         (ny (* l (/ tt T))))

    (cons (truncate (+ (X-Point-x (nth 1 ds2))
                       nx))
          (truncate (+ (X-Point-y (nth 1 ds2))
                       ny)))))
            
(defun xwem-diag-draw-3d-sector (d gc x y w h a1 a2 sector-width &optional fill-gc)
  "Draw 3d sector."
  (let ((ds1 (xwem-diag-calc-sector-dots x y w h a1 a2))
        (ds2 (xwem-diag-calc-sector-dots x (+ y sector-width) w h a1 a2))
        d0-adds1 d0-adds2 d2-adds1 d2-adds2
        buta1 buta2
        d0-buta1 d0-buta2
        d2-buta1 d2-buta2
        center-visible dot0-visible dot2-visible
        cd0-visible cd2-visible)

    ;; Adjust a1 and a2
    (while (< a1 0)
      (setq a1 (+ a1 360)))
    (while (> a1 360)
      (setq a1 (- a1 360)))
    (while (> a2 360)
      (setq a2 (- a2 360)))

    (when (or (and (<= a1 180)
                   (> (+ a1 a2) 180))
              (and (> a1 180)
                   (> (+ a1 a2) 540)))
      (setq d0-adds1 (cons (- (X-Point-x (nth 1 ds1)) (/ w 2))
                           (X-Point-y (nth 1 ds1))))
      (setq d0-adds2 (cons (- (X-Point-x (nth 1 ds2)) (/ w 2))
                           (X-Point-y (nth 1 ds2))))
      (if (and (<= a1 180)
               (> (+ a1 a2) 180))
          (progn
            (setq buta1 180)
            (setq buta2 (- a2 (- buta1 a1))))

        (setq d0-buta1 180)
        (setq d0-buta2 (- (+ a1 a2) 270 270)))
      )

    (when (> (+ (or buta1 a1) (or buta2 a2)) 360)
      (setq d2-adds1 (cons (+ (X-Point-x (nth 1 ds1)) (/ w 2))
                           (X-Point-y (nth 1 ds1))))
      (setq d2-adds2 (cons (+ (X-Point-x (nth 1 ds2)) (/ w 2))
                           (X-Point-y (nth 1 ds2))))
      (if (or (and (xwem-diag-dot-> 'y (nth 0 ds1) (nth 1 ds1))
                   (xwem-diag-dot-> 'y (nth 2 ds1) (nth 1 ds1)))
              (and d0-buta1 d0-buta2))
          (progn
            (setq d0-buta1 (or d0-buta1 buta1))
            (setq d0-buta2 (or d0-buta2 buta2))
            (setq d2-buta1 (or buta1 a1))
            (setq d2-buta2 (- 360 d2-buta1))
            (setq buta1 nil
                  buta2 nil))

        (setq buta1 (or buta1 a1))
        (setq buta2 (- 360 buta1))
        ))

    ;; Setup visibilities
    (unless (and (xwem-diag-dot-< 'x (nth 0 ds1) (nth 1 ds1))
                 (xwem-diag-dot-< 'y (nth 0 ds1) (nth 1 ds1)))
      (setq dot0-visible t))

    (unless (and (xwem-diag-dot-> 'x (nth 2 ds1) (nth 1 ds1))
                 (xwem-diag-dot-< 'y (nth 2 ds1) (nth 1 ds1)))
      (setq dot2-visible t))

    (when (or (and (xwem-diag-dot->= 'x (nth 0 ds1) (nth 1 ds1))
                   (xwem-diag-dot-<= 'y (nth 0 ds1) (nth 1 ds1))
                   (not (and (xwem-diag-dot->= 'x (nth 2 ds1) (nth 1 ds1))
                             (or (xwem-diag-dot->= 'y (nth 2 ds1) (nth 1 ds1))
                                 (xwem-diag-dot->= 'y (nth 2 ds1) (nth 0 ds1))))))
              (and (xwem-diag-dot->= 'x (nth 0 ds1) (nth 1 ds1))
                   (xwem-diag-dot->= 'y (nth 0 ds1) (nth 1 ds1))
                   (not (and (xwem-diag-dot->= 'y (nth 2 ds1) (nth 0 ds1))
                             (xwem-diag-dot->= 'x (nth 2 ds1) (nth 1 ds1))
                             (xwem-diag-dot->= 'y (nth 2 ds1) (nth 1 ds1)))))
              (and (xwem-diag-dot-<= 'x (nth 0 ds1) (nth 1 ds1))
                   (xwem-diag-dot-<= 'y (nth 0 ds1) (nth 1 ds1))
                   (xwem-diag-dot-<= 'x (nth 2 ds1) (nth 1 ds1))
                   (or (xwem-diag-dot->= 'y (nth 2 ds1) (nth 1 ds1))
                       (xwem-diag-dot->= 'y (nth 2 ds1) (nth 0 ds1))))
              (and (xwem-diag-dot-<= 'x (nth 0 ds1) (nth 1 ds1))
                   (xwem-diag-dot->= 'y (nth 0 ds1) (nth 1 ds1))
                   (xwem-diag-dot-<= 'x (nth 2 ds1) (nth 1 ds1))
                   (and (xwem-diag-dot->= 'y (nth 2 ds1) (nth 1 ds1))
                        (xwem-diag-dot->= 'y (nth 2 ds1) (nth 0 ds1)))))
      (setq center-visible t)

      (when dot0-visible
        (unless (and (xwem-diag-dot-> 'y (nth 2 ds2) (nth 1 ds2))
                     (xwem-diag-dot-betwean-p 'x (nth 2 ds2) (nth 1 ds2) (nth 0 ds2)))
          (setq cd0-visible t)))

      (when dot2-visible
        (unless (and (xwem-diag-dot-> 'y (nth 0 ds2) (nth 1 ds2))
                     (xwem-diag-dot-betwean-p 'x (nth 0 ds2) (nth 1 ds2) (nth 2 ds2)))
          (setq cd2-visible t)))
      )

    ;; Draw buttom arc
    (if (and buta1 buta2)
        (progn
          (when fill-gc
            (XFillArc (X-Drawable-dpy d) d fill-gc x (+ y sector-width) w h buta1 buta2))
          (XDrawArc (X-Drawable-dpy d) d gc x (+ y sector-width) w h buta1 buta2))

      (if (or (and d0-buta1 d0-buta2)
              (and d2-buta1 d2-buta2))
          (progn
            (when (and d0-buta1 d0-buta2)
              (when fill-gc
                (XFillArc (X-Drawable-dpy d) d fill-gc x (+ y sector-width) w h d0-buta1 d0-buta2))
              (XDrawArc (X-Drawable-dpy d) d gc x (+ y sector-width) w h d0-buta1 d0-buta2))
            (when (and d2-buta1 d2-buta2)
              (when fill-gc
                (XFillArc (X-Drawable-dpy d) d fill-gc x (+ y sector-width) w h d2-buta1 d2-buta2))
              (XDrawArc (X-Drawable-dpy d) d gc x (+ y sector-width) w h d2-buta1 d2-buta2)))

        (when (and (xwem-diag-dot->= 'y (nth 0 ds1) (nth 1 ds1))
                   (xwem-diag-dot->= 'y (nth 2 ds1) (nth 1 ds1)))
          (when fill-gc
            (XFillArc (X-Drawable-dpy d) d fill-gc x (+ y sector-width) w h a1 a2))
          (XDrawArc (X-Drawable-dpy d) d gc x (+ y sector-width) w h a1 a2))))

    ;; fill other stuff
    (when fill-gc
      ;; main sector
      (XFillArc (X-Drawable-dpy d) d fill-gc x y w h a1 a2)

      (xwem-diag-draw-rect d fill-gc
                           (nth 2 ds2) (nth 2 ds1)
                           (nth 1 ds1) (nth 1 ds2)
                           fill-gc)
      (xwem-diag-draw-rect d fill-gc
                           (nth 0 ds2) (nth 0 ds1)
                           (nth 1 ds1) (nth 1 ds2)
                           fill-gc)

      (when (and d0-adds1 d0-adds2)
        (XFillPoly (X-Drawable-dpy d) d fill-gc
                   (list d0-adds1 d0-adds2 (nth 1 ds2) (nth 1 ds1)
                         d0-adds1)))

      (when (and d2-adds1 d2-adds2)
        (XFillPoly (X-Drawable-dpy d) d fill-gc
                   (list d2-adds1 d2-adds2 (nth 1 ds2) (nth 1 ds1)
                         d2-adds1)))
      )

    ;; Draw main sector
    (XDrawLines (X-Drawable-dpy d) d gc ds1)
    (XDrawArc (X-Drawable-dpy d) d gc x y w h a1 a2)

    (xwem-diag-draw-sector d gc x y w h a1 a2 fill-gc) ; sector always visible

    ;; Draw visibilities
    (when center-visible
      (XDrawLines (X-Drawable-dpy d) d gc (list (nth 1 ds1) (nth 1 ds2))))
    (when cd0-visible
      (XDrawLines (X-Drawable-dpy d) d gc (list (nth 1 ds2) (nth 0 ds2))))
    (when cd2-visible
      (XDrawLines (X-Drawable-dpy d) d gc (list (nth 1 ds2) (nth 2 ds2))))
    (when dot0-visible
      (XDrawLines (X-Drawable-dpy d) d gc (list (nth 0 ds1) (nth 0 ds2))))
    (when dot2-visible
      (XDrawLines (X-Drawable-dpy d) d gc (list (nth 2 ds1) (nth 2 ds2))))
    (when (and d0-adds1 d0-adds2)
      (XDrawLines (X-Drawable-dpy d) d gc (list d0-adds1 d0-adds2)))
    (when (and d2-adds1 d2-adds2)
      (XDrawLines (X-Drawable-dpy d) d gc (list d2-adds1 d2-adds2)))
    ))

(defun xwem-diag-draw-bar (d gc x y w h &optional fill-gc)
  "Draw plain bar."
  (xwem-diag-draw-rect d gc
                       (cons x y) (cons (+ x w) y)
                       (cons (+ x w) (+ y h)) (cons x (+ y h))
                       fill-gc))
                       
(defun xwem-diag-draw-3d-bar (d gc x y w h bar-width &optional fill-gc)
  "Draw 3d bar."
  (let* ((d1 (cons x y))
         (d2 (cons (+ x w) y))
         (d3 (cons (+ x w) (+ y h)))
         (d4 (cons x (+ y h)))
         (x-off (/ bar-width 2))
         (y-off (/ bar-width 2))
         (dd1 (cons (+ x-off (car d1)) (- (cdr d1) y-off)))
         (dd2 (cons (+ x-off (car d2)) (- (cdr d2) y-off)))
         (dd3 (cons (+ x-off (car d3)) (- (cdr d3) y-off))))

    (when fill-gc
      (XFillPoly (X-Drawable-dpy d) d fill-gc
                 (list d1 dd1 dd2 dd3 d3 d4 d1)))
    
    (XDrawLines (X-Drawable-dpy d) d gc
                (list d1 dd1 dd2 dd3 d3 d4 d1 d2 d3 d2 dd2))
  ))

(defun xwem-diag-draw-percentage (type spec d edge-gc x y width height
                                       &optional sector-width label-factor
                                       label-font override-gc)
  "Draw percentage sector of TYPE.
TYPE is one of 'plain or '3d.
SPEC specifies percentage to display, it is an array in form
\[percents sector-label fill-color center-offset x-offset y-offset\]
  perecnts - is number betwean 0 and 100.
  sector-label - either string or t, t mean show percentage.
  fill-color - nil or color to fill sector.
  center-offset - sector's offset from center using bisector vector.
  x-offset - sector's x offset.
  y-offset - sector's y offest.

EDGE-GC used to draw sector edges.
X, Y, WIDTH and HEIGHT specifies sector geometry coordinate inside
drawable D.
Optionally SECTOR-WIDTH may be specified (only for '3d TYPE).
LABEL-FACTOR is float number used, when calculating label placement.
LABEL-FONT is font used to draw label, default is font of EDGE-GC."
  (let ((xdpy (X-Drawable-dpy d))
        (temp-fill-face (make-face 'temp-fill-face))
        (start-angle 0)
        angle-begin curang)
    
    ;; Validate spec
;    (when (> (apply '+ (mapcar (lambda (el) (aref el 0)) spec)) 100)
;      (error "XWEM Invalid spec" spec))

    (let ((draw-sector
           (lambda (sel angbeg angle)
             (xwem-set-face-foreground temp-fill-face (aref sel 2))
             (let ((xint-off 0)
                   (yint-off 0))

               (when (not (zerop (aref sel 3)))
                 (let ((ra (/ (* pi (+ angbeg (/ angle 2))) 180)))
                   (setq xint-off (round (* (aref sel 3) (cos ra))))
                   (setq yint-off (- (round (* (aref sel 3) (sin ra)))))))

               (if (eq type 'plain)
                   (xwem-diag-draw-sector
                    d edge-gc (+ x xint-off (aref sel 4))
                    (+ y yint-off (aref sel 5)) width height
                    angbeg angle
                    (or override-gc (xwem-face-get-gc temp-fill-face)))
                 (xwem-diag-draw-3d-sector
                  d edge-gc (+ x xint-off (aref sel 4))
                  (+ y yint-off (aref sel 5)) width height
                  angbeg angle (or sector-width 10)
                  (or override-gc (xwem-face-get-gc temp-fill-face))))

               ;; Draw label
               (when (aref sel 1)
                 (let* ((k (or label-factor 0.8))
                        (nw (* width k))
                        (nh (* height k))
                        (nx (+ (aref sel 4) x xint-off (/ (- width nw) 2)))
                        (ny (+ (aref sel 5) y yint-off (/ (- height nh) 2)))
                        (cd (xwem-diag-calc-sector-dots nx ny nw nh angbeg (/ angle 2)))
                        (gc edge-gc)
                        (text (if (stringp (aref sel 1)) (aref sel 1) (format "%d%%" (aref sel 0)))))
                   (XDrawString xdpy d gc
                                (- (X-Point-x (nth 2 cd))
                                   (/ (X-Text-width xdpy (X-Gc-font gc) text) 2))
                                (+ (/ (X-Text-height xdpy (X-Gc-font gc) text) 2)
                                   (X-Point-y (nth 2 cd)))
                                text)))
               ))))
      ;; Sort SPEC by percentage
      (setq spec (sort spec (lambda (el1 el2) (> (aref el1 0) (aref el2 0)))))

      ;; Special cases, when first sector is too big or too small
      (when (> (aref (car spec) 0) 75)
        (setq start-angle (* 360.0 (/ (- (aref (car spec) 0) 100) 100.0))))
      (when (< (aref (car spec) 0) 25)
        (setq start-angle (* 360.0 (/ (- 25 (aref (car spec) 0)) 100.0))))

      (setq angle-begin start-angle)
      ;; Draw huge sectors
      (while (and spec (< (+ (* 100 (/ angle-begin 360.0))
                             (aref (car spec) 0))
                          75))
        (setq curang (* 360.0 (/ (aref (car spec) 0) 100.0)))
        (funcall draw-sector (car spec) angle-begin curang)
        (setq angle-begin (+ angle-begin curang))
        (setq spec (cdr spec)))
        
      ;; Draw little sectors
      (setq angle-begin start-angle)
      (mapc (lambda (sss)
              (setq curang (* 360.0 (/ (aref sss 0) 100.0)))
              (setq angle-begin (- angle-begin curang))
              (funcall draw-sector sss angle-begin curang))
            (nreverse spec)))))

(defun xwem-diag-plot-coordinates (d gc x y w h x-step y-step &rest params)
  "Draw coordinates system."
  (let ((notch-len (or (plist-get params :notch-len) 4))
        (with-grid-p (plist-get params :with-grid))
        (grid-dash-even (or (plist-get params :grid-dash-even) 1))
        (grid-dash-odd (or (plist-get params :grid-dash-odd 3)))
        (with-labels-p (plist-get params :with-labels))
        (labels-offset (or (plist-get params :labels-offest) 4))
        (labels-gc (or (plist-get params :labels-gc) gc))
        (center-x (or (plist-get params :center-x) 0))
        (center-y (or (plist-get params :center-y) 0))
        (scale-x (or (plist-get params :scale-x) x-step))
        (scale-y (or (plist-get params :scale-y) y-step))

        (xdpy (X-Drawable-dpy d))
        x-notches y-notches noff sls)

    (setq noff (% center-x x-step))
    (while (< noff w)
      (setq x-notches (cons (cons (cons (+ x noff)
                                        (- y center-y))
                                  (cons (+ x noff) (- y center-y notch-len)))
                            x-notches))
      (setq noff (+ noff x-step)))

    (setq noff (% center-y y-step))
    (while (< noff h)
      (setq y-notches (cons (cons (cons (+ x center-x)
                                        (- y noff))
                                  (cons (+ x center-x notch-len) (- y noff)))
                            y-notches))
      (setq noff (+ noff y-step)))

    ;; Set dashes
    (when with-grid-p
      (setq sls (X-Gc-line-style gc))
      (XChangeGC xdpy gc :line-style X-LineOnOffDash)
      (XSetDashes xdpy gc 0 (list grid-dash-even grid-dash-odd
                                  grid-dash-even grid-dash-odd))
      (unwind-protect
          (progn
            (XDrawSegments xdpy d gc
                           (mapcar (lambda (s)
                                     (cons (cons x (X-Point-y (car s)))
                                           (cons (+ x w) (X-Point-y (cdr s)))))
                                   y-notches))
            (XDrawSegments xdpy d gc
                           (mapcar (lambda (s)
                                     (cons (cons (X-Point-x (car s)) (- y h))
                                           (cons (X-Point-x (car s)) y)))
                                   x-notches)))
        ;; Revert gc
        (XChangeGC xdpy gc :line-style sls)))

    (when with-labels-p
      (mapc (lambda (s)
              (let* ((txt (int-to-string (/ (* scale-x (- (X-Point-x (car s)) x center-x)) x-step)))
                     (tw (X-Text-width xdpy (X-Gc-font labels-gc) txt))
                     (th (X-Text-height xdpy (X-Gc-font labels-gc) txt)))

                (XDrawString xdpy d labels-gc (- (X-Point-x (car s)) (/ tw 2))
                             (+ y th labels-offset) txt)))
            x-notches)
      (mapc (lambda (s)
              (let* ((txt (int-to-string (/ (* scale-y (- y (X-Point-y (car s)) center-y)) y-step)))
                     (tw (X-Text-width xdpy (X-Gc-font labels-gc) txt))
                     (th (X-Text-height xdpy (X-Gc-font labels-gc) txt)))

                (XDrawString xdpy d labels-gc (- x tw labels-offset)
                             (+ (X-Point-y (car s)) (/ th 2)) txt)))
            y-notches)
      )

    (XDrawSegments xdpy d gc (nconc (list (cons (cons x (- y center-y)) (cons (+ x w) (- y center-y)))
                                          (cons (cons (+ x center-x) y) (cons (+ x center-x) (- y h))))
                                    x-notches y-notches))
    ))

(defun xwem-diag-plot-points (point-type d gc dots &optional point-size)
  "Draw points of TYPE."
  (unless point-size
    (setq point-size 2))

  (let ((xdpy (X-Drawable-dpy d)))
    (cond ((eq point-type 0)
           (XDrawArcs xdpy d gc
                      (mapcar (lambda (d)
                                (make-X-Arc :x (- (X-Point-x d) point-size)
                                            :y (- (X-Point-y d) point-size)
                                            :width (+ point-size point-size)
                                            :height (+ point-size point-size)
                                            :angle1 0
                                            :angle2 360)) dots))
           (XDrawPoints xdpy d gc dots))

          ((eq point-type 1)
           (XDrawSegments xdpy d gc
                          (apply 'nconc
                                 (mapcar (lambda (d)
                                           (list (cons (cons (- (X-Point-x d) point-size)
                                                             (X-Point-y d))
                                                       (cons (+ (X-Point-x d) point-size)
                                                             (X-Point-y d)))
                                                 (cons (cons (X-Point-x d)
                                                             (- (X-Point-y d) point-size))
                                                       (cons (X-Point-x d)
                                                             (+ (X-Point-y d) point-size)))))
                                         dots))))

          ((eq point-type 2)
           (XDrawSegments xdpy d gc
                          (apply 'nconc
                                 (mapcar (lambda (d)
                                           (list (cons (cons (- (X-Point-x d) point-size)
                                                             (- (X-Point-y d) point-size))
                                                       (cons (+ (X-Point-x d) point-size)
                                                             (+ (X-Point-y d) point-size)))
                                                 (cons (cons (- (X-Point-x d) point-size)
                                                             (+ (X-Point-y d) point-size))
                                                       (cons (+ (X-Point-x d) point-size)
                                                             (- (X-Point-y d) point-size)))))
                                         dots))))
          )))

(defun xwem-diag-plot-dots (type d gc x y dots &optional point-type point-size)
  "Draw dots in cartesian coordinate system which has 0 at X Y."
  ;; Adjust dots, according to X Y
  (setq dots (mapcar (lambda (dot)
                       (cons (+ x (X-Point-x dot))
                             (- y (X-Point-y dot))))
                     dots))

  ;; Default point/line types
  (unless point-type
    (setq point-type 0))

  (let ((xdpy (X-Drawable-dpy d)))
    (cond ((eq type 'points)
           (xwem-diag-plot-points point-type d gc dots point-size))

          ((eq type 'lines)
           (XDrawLines xdpy d gc dots))

          ((eq type 'linespoints)
           (xwem-diag-plot-points point-type d gc dots point-size)
           (XDrawLines xdpy d gc dots))

          ((eq type 'impulses)
           (XDrawSegments xdpy d gc
                          (mapcar (lambda (d)
                                    (cons (cons (X-Point-x d) y) d))
                                  dots)))

          ((eq type 'dots)
           (XDrawPoints xdpy d gc dots))

          ((eq type 'steps)
           (XDrawLines xdpy d gc
                       (apply 'nconc (mapcar* (lambda (d dn)
                                                (if dn
                                                    (list d (cons (X-Point-x dn) (X-Point-y d)))
                                                  (list d)))
                                              dots (nconc (cdr dots) (list nil))))))

          ((eq type 'fsteps)
           (XDrawLines xdpy d gc
                       (apply 'nconc (mapcar* (lambda (d dn)
                                                (if dn
                                                    (list d (cons (X-Point-x d) (X-Point-y dn)))
                                                  (list d)))
                                              dots (nconc (cdr dots) (list nil))))))

          ((eq type 'histeps)
           ;; TODO: write me
           )

          ((eq type 'boxes)
           ;; TODO: write me
           ))
    ))

(defun xwem-diag-read-data-file (file &optional using x-scale y-scale)
  "Read data FILE and return list of dots lists.
USING is cons cell that specifies which columns to use.
X-SCALE is x coordinates scalling.
Y-SCALE is y coordinates scalling."
  (unless using
    (setq using (cons 1 2)))

  (with-temp-buffer
    (let (cdots dlist)
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((looking-at "^#"))

              ((looking-at "^[ \t]*$")
               ;; dots set delimiter
               (setq dlist (cons cdots dlist))
               (setq cdots nil))

              (t
               (let ((sc (split-string (buffer-substring (point-at-bol) (point-at-eol)) "[ \t]+")))
                 (setq sc (delete "" sc))
                 (setq cdots (cons (cons (* (or x-scale 1) (string-to-int (nth (1- (car using)) sc)))
                                         (* (or y-scale 1) (string-to-int (nth (1- (cdr using)) sc))))
                                   cdots))))
              )
        (forward-line 1))

      ;; Add last cdots, if any
      (when cdots
        (setq dlist (cons cdots dlist)))
      dlist)))

;;; Testing:

;(progn
;  (mapc (lambda (dots)
;          (xwem-diag-plot-dots 'lines (xwem-frame-xwin (nth 3 xwem-frames-list)) (xwem-face-get-gc 'green)
;                               600 600 dots))
;        (xwem-diag-read-data-file "/usr/local/share/doc/gnuplot/world.dat" nil 3 4))

;  (xwem-diag-plot-dots 'points (xwem-frame-xwin (nth 3 xwem-frames-list)) (xwem-face-get-gc 'red)
;                       600 600 (car (xwem-diag-read-data-file "/usr/local/share/doc/gnuplot/world.cor" nil 3 4)) 1 3)

;  (xwem-diag-plot-coordinates (xwem-frame-xwin (nth 3 xwem-frames-list)) (xwem-face-get-gc 'blue)
;                              60 1000 1200 800 50 50
;                              :center-x 600 :center-y 400 :with-grid t :grid-dash-even 1 :grid-dash-odd 4
;                              :with-labels t :labels-gc (xwem-face-get-gc 'bold))
;  )
 

(provide 'xwem-diagram)

;;; xwem-diagram.el ends here
