;;; xwem-xfig.el --- xfig stuff for XWEM.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sat Mar  6 13:32:11 MSK 2004
;; Keywords: xwem
;; Time-stamp: <30/11/2006 00:04:57 lg@h1>

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

;; Some xfig functionality.

;;; Code:


(defconst xwem-xfig-orientations '("Landscape" "Portrait"))
(defconst xwem-xfig-justifications '("Center" "Flush Left"))
(defconst xwem-xfig-units '("Metric" "Inches"))
(defconst xwem-xfig-papersizes '("Letter" "Legal" "Ledger" "Tabloid" "A" "B" "C" "D" "E" "A4" "A3" "A2" "A1" "A0" "B5"))
(defconst xwem-xfig-multiple-pages '("Single" "Multiple"))

(defconst xwem-xfig-class-types
  '((0 . color)
    (1 . ellipse)
    (2 . poly)
    (3 . spline)
    (4 . text)
    (5 . arc)
    (6 . compound)))

(defconst xwem-xfig-colors
  '((-1 . default)
    (0 . "black")
    (1 . "blue")
    (2 . "green")
    (3 . "cyan")
    (4 . "red")
    (5 . "magenta")
    (6 . "yellow")
    (7 . "white")

    ;; 4 blue shades
    (8 . "#000033")
    (9 . "#000066")
    (10 . "#000099")
    (11 . "#0000cc")

    ;; 3 green shades
    (12 . "#004400")
    (13 . "#008800")
    (14 . "#00cc00")

    ;; 3 cyan shades
    (15 . "#004444")
    (16 . "#008888")
    (17 . "#00cccc")

    ;; 3 red shades
    (18 . "#440000")
    (19 . "#880000")
    (20 . "#cc0000")
    
    ;; 3 magenta shades
    (21 . "#440044")
    (22 . "#880088")
    (23 . "#cc00cc")

    ;; 3 brown shades
    (24 . "#ff4444")
    (25 . "#ff8888")
    (26 . "#ffcccc")

    ;; 4 ping shades
    (27 . "#ff3344")
    (28 . "#ff6688")
    (29 . "#ff99cc")
    (30 . "#ffccff")

    ;; gold
    (31 . "#ffd700")))

(defconst xwem-xfig-line-styles
  '((-1 . default)
    (0 . solid)
    (1 . dashed)
    (2 . dotted)
    (3 . dash-dotted)
    (4 . dash-double-dotted)
    (5 . dash-triple-dotted)))

(defconst xwem-xfig-join-styles
  '((0 . X-JoinMiter)
    (1 . X-JoinRound)
    (2 . X-JoinBevel)))

(defconst xwem-xfig-cap-styles
  '((0 . X-CapButt)
    (1 . X-CapRound)
    (2 . X-CapProjecting)))

(defconst xwem-xfig-arrow-types
  '((0 . stick)
    (1 . closed-triangle)
    (2 . closed-indented-butt)
    (3 . closed-pointed-butt)))

(defconst xwem-xfig-arrow-styles
  '((0 . hollow)                        ; filled with white
    (1 . pen)))                         ; filled with pen

(defstruct xwem-xfig-arrow
  type                                  ; defined in `xwem-xfig-arrow-types'
  style                                 ; defined in `xwem-xfig-arrow-styles'
  thickness                             ; float
  width                                 ; float
  height)                               ; float

(defstruct xwem-xfig-picture
  flipped
  filename)

(defconst xwem-xfig-poly-subtypes
  '((1 . polyline)
    (2 . box)
    (3 . polygon)
    (4 . arc-box)
    (5 . picture)))

(defstruct xwem-xfig-poly
  (object 2)
  (sub-type 'polyline)
  picture                               ; only for sub-type == picture
  line-style                            ; defined in `xwem-xfig-line-styles'
  thickness
  pen-color
  fill-color
  depth
  pen-style
  area-fill
  style-val                             ; float (1/80 inch)
  join-style                            ; defined in `xwem-xfig-join-styles'
  cap-style                             ; defined in `xwem-xfig-cap-styles'
  radius                                ; radius of arc-boxes (1/80 inch)
  forward-arrow                         ; nil or xwem-xfig-arrow
  backward-arrow                        ; nil or xwem-xfig-arrow
  points)

(defconst xwem-xfig-spline-subtypes
  '((0 . open-approximated)
    (1 . closed-approximated)
    (2 . open-interpolated)
    (3 . closed-interpolated)
    (4 . open-x-spline)
    (5 . closed-x-spline)))

(defstruct xwem-xfig-spline
  (object 3)
  (sub-type 'open-approximated)
  line-style
  thickness
  pen-color
  fill-color
  depth
  pen-style
  area-fill
  style-val
  cap-style
  forward-arrow
  backward-arrow
  points)

(defstruct xwem-xfig-text
  (object 4)
  (sub-type 'center)
  color
  depth
  pen-style
  font
  font-size
  angle
  font-flags
  height
  length
  x y
  string)

(defconst xwem-xfig-arc-subtypes
  '((1 . open)                          ; open ended arc
    (2 . closed)))                      ; pie-wedge closed

(defconst xwem-xfig-arc-directions
  '((0 . clockwise)
    (1 . counter-clockwise)))

(defstruct xwem-xfig-arc
  (object 5)
  (sub-type 'open)
  line-style
  line-thickness
  pen-color
  fill-color
  depth
  pen-style
  area-fill
  style-val
  cap-style
  direction
  forward-arrow
  backward-arrow
  center-x center-y
  x1 y1
  x2 y2
  x3 y3)

(defstruct xwem-xfig-comound
  (object 6)
  x y x1 y1
  xfig-objects)

;; Define xfig object
(defstruct xwem-xfig
  data                                  ; data lives here
  (colors xwem-xfig-colors)             ; colors definitions
  objects)
  

(defun xwem-xfig-add-color (xfig color)
  "In XFIG's colors add COLOR."
  (push color (xwem-xfig-colors xfig))
  )

;; Drawing routines
(defun xwem-xfig-draw-text (xfig text)
  "In XFIG's window draw xfig TEXT."
  )

(defun xwem-xfig-draw-spline (xfig spline)
  "In XFIG's window draw SPLINE."
  )

(defun xwem-xfig-draw-poly (xfig poly)
  "In XFIG's window draw POLY."
  )

(defun xwem-xfig-draw-ellipse (xfig ellipse)
  "In XFIG's window draw ELLIPSE."
  )

(defun xwem-xfig-draw-arc (xfig arc)
  "In XFIG's window draw ARC."
  )

(defun xwem-xfig-draw-compound (xfig compound)
  "In XFIG's window draw COMPOUND."
  )

;; Parsing xfig file
(defun xwem-xfig-parse-arc (line)
  )

(defun xwem-xfig-parse-ellipse (line)
  )

(defun xwem-xfig-parse-poly (line)
  )

(defun xwem-xfig-parse-spline (line)
  )

(defun xwem-xfig-parse-text (line)
  )

(defun xwem-xfig-parse-compound (line)
  )

(defun xwem-xfig-parse-color (line)
  )

(defun xwem-xfig-parse (xfig line)
  (let ((ob (string-to-int line)))
    (push (cond ((= ob 0) (xwem-xfig-parse-color line))
                ((= ob 1) (xwem-xfig-parse-ellipse line))
                ((= ob 2) (xwem-xfig-parse-poly line))
                ((= ob 3) (xwem-xfig-parse-spline line))
                ((= ob 4) (xwem-xfig-parse-text line))
                ((= ob 5) (xwem-xfig-parse-arc line))
                ((= ob 6) (xwem-xfig-parse-compound line))
                (t (error "XWEM Invalid line format" line)))
          (xwem-xfig-objects xfig))))


(provide 'xwem-xfig)

;;; xwem-drawing.el ends here
