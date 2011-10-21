;;; xwem-mogu.el --- XWEM MOuse Grid Uberness.

;; Copyright (C) 2005 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Mar  3 22:24:27 MSK 2005
;; Keywords: xwem, mouse, grid

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

;;  When xwem-mogu is enabled whole screen is splited by set of
;; horizontal and vertical lines.  Each vertical line has
;; corresponding <number> and each horizontal line has corresponding
;; <character>.  Each intersection of horizontal and vertical lines
;; denotes NODE.  Each node has unique identificator - cons cell in
;; form: (<character> . <number>).  The closest node to pointer
;; location called MOUSE-NODE.  Set of nodes of same <number> called
;; NODE-VLINE.  Set of nodes of same <character> called NODE-HLINE.

;; Here is an ASCII Scheme:

;;    1          2          3          4
;;   a*----------*----------*----------*
;;    |          |          |          |
;;    |          |          |          |
;;    |          |          |          |
;;    |          |          |          |
;;    |          |          |          |
;;    |          |          |          |
;;   b*----------o----------*----------*
;;    |          |          |          |
;;    |          | 7\       |          |
;;    |          |          |          |
;;    |          |          |          |
;;    |          |          |          |
;;    |          |          |          |
;;   c*----------*----------*----------*
;;
;;      7\ - Pointer
;;      *  - node
;;      o  - mouse-node

;;; Code:

(require 'xwem-load)
(require 'xwem-interactive)
(require 'xwem-misc)

(require 'xlib-xshape)

(defgroup xwem-mogu nil
  "Group to customize xwem mouse grid uberness."
  :prefix "xwem-mogu-"
  :group 'xwem-modes)

(defcustom xwem-mogu-keep-pointer-offset nil
  "*Non-nil mean mouse offset is kept when mouse-node is moved."
  :type 'boolean
  :group 'xwem-mogu)

(defcustom xwem-mogu-numbers-characters-visible-p t
  "*Non-nil mean <number>s and <character>s are visible."
  :type 'boolean
  :group 'xwem-mogu)

(defcustom xwem-mogu-identificators-visible-p nil
  "*Non-nil identificators are shown for each node."
  :type 'boolean
  :group 'xwem-mogu)

(defcustom xwem-mogu-numbers 8
  "Maximum number of <number>s."
  :type 'number
  :group 'xwem-mogu)

(defcustom xwem-mogu-characters 6
  "Maximum number of <character>s."
  :type 'number
  :group 'xwem-mogu)

(defcustom xwem-mogu-grid-width 1
  "*Width of grid lines."
  :type 'number
  :group 'xwem-mogu)

(defcustom xwem-mogu-grid-color "gray78"
  "Color for grid."
  :type 'color
  :group 'xwem-mogu)

(defcustom xwem-mogu-node-color "gray50"
  "Color for nodes."
  :type 'color
  :group 'xwem-mogu)

(defcustom xwem-mogu-mouse-node-color "red4"
  "Color for mouse-node."
  :type 'color
  :group 'xwem-mogu)

(defcustom xwem-mogu-stack-rank
  '(((t) . (30 . 30)))
  "Stack ranks specification for xwem mogu."
  :type '(cons (sexp :tag "Client's QUALIFIER")
               (cons (number :tag "Active")
                     (number :tag "Inactive")))
  :group 'xwem-mogu)

(define-xwem-face xwem-mogu-face
  `(((grid) (:foreground ,xwem-mogu-grid-color))
    ((node) (:foreground ,xwem-mogu-node-color))
    ((mouse-node) (:foreground ,xwem-mogu-mouse-node-color)))
  "Face for use by xwem-mogu."
  :group 'xwem-mogu
  :group 'xwem-faces)

(defcustom xwem-mogu-minor-mode-hook nil
  "Hooks to run when mogu mode is enabled or disabled.
When running hooks value of `xwem-mogu-minor-mode' is non-nil when
mogu minor mode is enabling.
When running hooks value of `xwem-mogu-minor-mode' is nil when mogu
minor mode is disabling."
  :type 'hook
  :group 'xwem-hooks
  :group 'xwem-mogu)

(defvar xwem-mogu-minor-mode nil
  "Non-nil mean mogu minor mode is enabled.")



(defvar xwem-mogu-xwin nil)

(defmacro xwem-mogu-xmask (xwin)
  `(X-Win-get-prop ,xwin 'xwem-mogu-xmask))
(defsetf xwem-mogu-xmask (xwin) (xmask)
  `(X-Win-put-prop ,xwin 'xwem-mogu-xmask ,xmask))

(defmacro xwem-mogu-mouse-node (xwin)
  `(X-Win-get-prop ,xwin 'xwem-mogu-mouse-node))
(defsetf xwem-mogu-mouse-node (xwin) (mouse-node)
  `(X-Win-put-prop ,xwin 'xwem-mogu-mouse-node ,mouse-node))

;;; Functions
(defun xwem-mogu-redimentionize-grid ()
  "`xwem-mogu-numbers' or `xwem-mogu-characters' changed, so handle it."
  ;; TODO: write me
  )

(defun xwem-mogu-query-mouse-node ()
  "Return node that is most close to pointer location."
  (let* ((qp (XQueryPointer (xwem-dpy) xwem-mogu-xwin))
         (x (nth 5 qp))
         (y (nth 6 qp))
         (wid-step (+ (/ (X-Geom-width (xwem-rootgeom)) xwem-mogu-numbers)
                      xwem-mogu-grid-width))
         (hei-step (+ (/ (X-Geom-height (xwem-rootgeom)) xwem-mogu-characters)
                      xwem-mogu-grid-width))
         (rx (/ x wid-step))
         (ry (/ y hei-step))
         (wr (% x wid-step))
         (hr (% y hei-step)))
    (when (> wr (- wid-step wr))
      (incf rx))
    (when (> hr (- hei-step hr))
      (incf ry))
    ;(cons rx (int-to-char (+ ry (char-to-int ?a))))
    (cons rx ry)))

(defun xwem-mogu-create-grid ()
  "Create xwem-mogu grid."
  ;; TODO: write me
  (unless xwem-mogu-xwin
    (let* ((wid (X-Geom-width (xwem-rootgeom)))
           (wid-step (/ wid xwem-mogu-numbers))
           (hei (X-Geom-height (xwem-rootgeom)))
           (hei-step (/ hei xwem-mogu-characters)))
      (setq xwem-mogu-xwin
            (XCreateWindow (xwem-dpy) (xwem-rootwin) 0 0
                           wid hei 0
                           nil nil nil
                           :override-redirect t
                           :background-pixel
                           (XAllocColor (xwem-dpy) (XDefaultColormap (xwem-dpy))
                                        (xwem-make-color xwem-mogu-grid-color))))
      ;; XXX Setup stack rank
      (setf (xwem-xwin-rank xwem-mogu-xwin) 100)

      ;; Create mask pixmap
      (setf (xwem-mogu-xmask xwem-mogu-xwin)
            (XCreatePixmap (xwem-dpy) xwem-mogu-xwin 1 wid hei))

      (XFillRectangle (xwem-dpy) (xwem-mogu-xmask xwem-mogu-xwin)
                      xwem-misc-mask-fgc 0 0 wid hei)

      (loop for xo from 0 to wid by (+ wid-step xwem-mogu-grid-width)
        do (loop for yo from 0 to hei by (+ hei-step xwem-mogu-grid-width)
             do (XFillRectangle (xwem-dpy) (xwem-mogu-xmask xwem-mogu-xwin)
                                xwem-misc-mask-bgc
                                (+ xo xwem-mogu-grid-width)
                                (+ yo xwem-mogu-grid-width)
                                wid-step hei-step)))

      (X-XShapeMask (xwem-dpy) xwem-mogu-xwin
                    X-XShape-Bounding X-XShapeSet 0 0
                    (xwem-mogu-xmask xwem-mogu-xwin))

      ;; Initialise mouse-node
      (setf (xwem-mogu-mouse-node xwem-mogu-xwin)
            (xwem-mogu-query-mouse-node))

      ;; Show mouse grid
      (XMapWindow (xwem-dpy) xwem-mogu-xwin)

      xwem-mogu-xwin)))


;;; Commands:
(define-xwem-command xwem-turn-on-mogu ()
  "Enable mogu minor mode."
  (xwem-interactive)
  (unless xwem-mogu-minor-mode
    (if xwem-mogu-xwin
        (XMapWindow (xwem-dpy) xwem-mogu-xwin)
      (xwem-mogu-create-grid))
    (xwem-turn-on-minor-mode nil 'xwem-mogu-minor-mode)
    (run-hooks 'xwem-mogu-minor-mode-hook)))

(define-xwem-command xwem-turn-off-mogu ()
  "Disable mogu minor mode."
  (xwem-interactive)
  (when xwem-mogu-minor-mode
    (XUnmapWindow (xwem-dpy) xwem-mogu-xwin)
    (xwem-turn-off-minor-mode nil 'xwem-mogu-minor-mode)
    (run-hooks 'xwem-mogu-minor-mode-hook)))

(define-xwem-command xwem-mogu-minor-mode (arg)
  "Toggle mogu minor mode.
If ARG is positive number - enable it.
If ARG is negative number - disable it."
  (xwem-interactive "P")
  (if (or (and (numberp arg)
               (> arg 0))
          (not xwem-mogu-minor-mode))
      (xwem-turn-on-mogu)
    (xwem-turn-off-mogu)))

(defun xwem-mogu-goto (id-x id-y)
  "Move to node with ID."
  (let* ((qp (XQueryPointer (xwem-dpy) xwem-mogu-xwin))
         (x (nth 5 qp))
         (y (nth 6 qp))
         (wid-step (+ (/ (X-Geom-width (xwem-rootgeom)) xwem-mogu-numbers)
                      xwem-mogu-grid-width))
         (hei-step (+ (/ (X-Geom-height (xwem-rootgeom)) xwem-mogu-characters)
                      xwem-mogu-grid-width))
         wr hr)

    (if (not xwem-mogu-keep-pointer-offset)
        (setq wr 0 hr 0)

      (setq wr (% x wid-step)
            hr (% y hei-step))
      (when (> wr (- wid-step wr))
        (setq wr (- wr wid-step)))
      (when (> hr (- hei-step hr))
        (setq hr (- hr hei-step))))

    ;; Save new mouse-node
    (setf (xwem-mogu-mouse-node xwem-mogu-xwin)
          (cons id-x id-y))

    (XWarpPointer (xwem-dpy) (xwem-rootwin) xwem-mogu-xwin
                  0 0 0 0
                  (+ (* wid-step id-x) wr)
                  (+ (* hei-step id-y) hr))))

(defun xwem-mogu-move (direction arg)
  "Move in DIRECTION ARG times.
DIRECTION is one of 'left, 'right, 'up or 'down.
ARG is number."
  (let ((m-n (xwem-mogu-mouse-node xwem-mogu-xwin)))
    (ecase direction
      (left
       (if (< arg 0)
           (xwem-mogu-move 'right (- arg))
         (xwem-mogu-goto (decf (car m-n) arg) (cdr m-n))))
      (right
       (if (< arg 0)
           (xwem-mogu-move 'left (- arg))
         (xwem-mogu-goto (incf (car m-n) arg) (cdr m-n))))
      (up
       (if (< arg 0)
           (xwem-mogu-move 'down (- arg))
         (xwem-mogu-goto (car m-n) (decf (cdr m-n) arg))))
      (down
       (if (< arg 0)
           (xwem-mogu-move 'up (- arg))
         (xwem-mogu-goto (car m-n) (incf (cdr m-n) arg))))
      )))

(define-xwem-command xwem-mogu-right (arg)
  "Move forward ARG nodes."
  (xwem-interactive "p")
  (xwem-mogu-move 'right arg))

(define-xwem-command xwem-mogu-left (arg)
  "Move backward ARG nodes."
  (xwem-interactive "p")
  (xwem-mogu-move 'left arg))

(define-xwem-command xwem-mogu-up (arg)
  "Move up ARG nodes."
  (xwem-interactive "p")
  (xwem-mogu-move 'up arg))

(define-xwem-command xwem-mogu-down (arg)
  "Move down ARG nodes."
  (xwem-interactive "p")
  (xwem-mogu-move 'down arg))

(define-xwem-command xwem-mogu-hline-beginning ()
  "Goto beginning of hline."
  (xwem-interactive)
  (xwem-mogu-goto 0 (cdr (xwem-mogu-mouse-node xwem-mogu-xwin))))

(define-xwem-command xwem-mogu-hline-end ()
  "Goto end of hline."
  (xwem-interactive)
  (xwem-mogu-goto xwem-mogu-numbers
                  (cdr (xwem-mogu-mouse-node xwem-mogu-xwin))))

(define-xwem-command xwem-mogu-vline-beginning ()
  "Goto beginning of vline."
  (xwem-interactive)
  (xwem-mogu-goto (car (xwem-mogu-mouse-node xwem-mogu-xwin)) 0))

(define-xwem-command xwem-mogu-vline-end ()
  "Goto end of vline."
  (xwem-interactive)
  (xwem-mogu-goto (car (xwem-mogu-mouse-node xwem-mogu-xwin))
                  xwem-mogu-characters))


(provide 'xwem-mogu)

;;; On-load actians
(xwem-add-minor-mode 'xwem-mogu-minor-mode "Mogu")

;;; xwem-mogu.el ends here
