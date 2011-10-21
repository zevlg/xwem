;;; xwem-vline.el --- Vertical line minor mode.

;; Copyright (C) 2005 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Feb 17 00:07:38 MSK 2005
;; Keywords: xwem, mode

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

;; This is minor mode which enables nifty vertical line in the client
;; application at the specified column.  When vertical line is
;; displayed you can interactively move it to any column using
;; click-and-drag.

;; To enable vline minor mode in current client simply do
;; C-H-x xwem-vline-minor-mode RET

;; By default vertical line is displayed at `fill-column'.  Use prefix
;; argument to `xwem-vline-minor-mode' command to specify particular
;; column.

;; You can configure vertical lines for different clients by altering
;; `xwem-vline-config' custom variable.

;;; Bugs:

;; - Does not track change of clients height

;;; Code:

(require 'xwem-modes)
(require 'xwem-osd)

(defcustom xwem-vline-width 2
  "*Width in pixels of vertical line."
  :type 'number
  :group 'xwem-modes)

(defcustom xwem-vline-config
  `(((application "xemacs")
     :color "red2" :column fill-column
     :column-width (font-width (face-font 'default)))
    ((application "xterm")
     :color "grey70" :column fill-column :column-width 8)

    ;; Default
    ((t)
     :color "grey70" :column fill-column :column-width 8))
  "*Vline configuration for clients."
  :type '(repeat (list sexp
                       (repeat :inline t
                               (list :inline t
                                     (symbol :tag "property")
                                     (sexp :tag "value")))))
  :group 'xwem-modes)

(defvar xwem-vline-minor-mode nil
  "Non-nil mean vline minor mode is enabled.")
(xwem-make-variable-client-local 'xwem-vline-minor-mode)


(defun xwem-vline-set-column (xwin new-column)
  "Set new column."
  (X-Win-put-prop xwin 'vline-column new-column)
  (let* ((xoff (* (X-Win-get-prop xwin 'vline-column-width) new-column))
         (osd (X-Win-get-prop xwin 'vline-osd))
         (clwid (X-Text-width
                 (xwem-dpy) (X-Gc-font (xwem-osd-gc osd))
                 (int-to-string new-column))))
    (XMoveWindow (X-Win-dpy xwin) xwin xoff 0)

    (xwem-osd-move osd (- xoff (/ clwid 2) -1) 0)
    (xwem-osd-text osd (int-to-string new-column))))

(defun xwem-vline-events-handler (xdpy xwin xev)
  "Handle incoming event."
  (X-Event-CASE xev
    (:X-ButtonPress
     (let ((old-x (X-Event-xbutton-root-x xev))
           (col-wid (X-Win-get-prop xwin 'vline-column-width))
           x)
       (XGrabPointer xdpy xwin
                     (Xmask-or XM-ButtonMotion XM-ButtonRelease XM-ButtonPress))
       (XAllowEvents xdpy X-SyncBoth)
       (xwem-unwind-protect
           (while (= (X-Event-type (setq xev (xwem-next-event)))
                     X-MotionNotify)
             (setq x (X-Event-xmotion-root-x xev))
             (when (>= (abs (- x old-x)) col-wid)
               (xwem-vline-set-column
                xwin (+ (X-Win-get-prop xwin 'vline-column)
                        (/ (- x old-x) col-wid)))
               (setq old-x (- x (% (- x old-x) col-wid)))))
         (XUngrabPointer xdpy))))

    (:X-DestroyNotify
     (xwem-osd-destroy (X-Win-get-prop xwin 'vline-osd))
     (X-Win-EventHandler-rem xwin 'xwem-vline-events-handler))
    ))

;;;###autoload(autoload 'xwem-turn-on-vline "xwem-vline" nil t)
(define-xwem-command xwem-turn-on-vline
  (client &optional color column column-width)
  "Enable vertical line minor mode for CLIENT."
  (xwem-interactive (list (xwem-cl-selected) nil
                          (and xwem-prefix-arg
                               (prefix-numeric-value xwem-prefix-arg))
                          nil))

  (let ((vc xwem-vline-config)
        (frame (xwem-misc-find-emacs-frame client)))
    (while (and vc (not (xwem-cl-match-p client (car (car vc)))))
      (setq vc (cdr vc)))
    (setq vc (cdr (car vc)))
    (when vc
      (with-selected-frame (or frame (selected-frame))
        (unless color
          (setq color (eval (plist-get vc :color))))
        (unless column
          (setq column (eval (plist-get vc :column))))
        (unless column-width
          (setq column-width (eval (plist-get vc :column-width)))))

      (let ((vline-xwin (xwem-cl-get-sys-prop client 'vline-xwin))
            (xoff (* column column-width)))
        ;; XXX Adjust XOFF for Emacs frames
        (when (framep frame)
          (incf xoff (frame-property frame 'internal-border-width)))

        (unless vline-xwin
          (setq vline-xwin
                (XCreateWindow
                 (xwem-dpy) (xwem-cl-xwin client)
                 0 0 xwem-vline-width
                 (X-Geom-height (xwem-cl-xgeom client)) 0
                 nil nil nil
                 :background-pixel
                 (XAllocColor (xwem-dpy) (XDefaultColormap (xwem-dpy))
                              (xwem-make-color color))
                 :cursor (xwem-make-cursor X-XC-sb_h_double_arrow)
                 :event-mask (Xmask-or XM-ButtonPress XM-StructureNotify)))
          (X-Win-EventHandler-add-new vline-xwin 'xwem-vline-events-handler 100
                                      (list X-ButtonPress X-DestroyNotify))
          (xwem-cl-put-sys-prop client 'vline-xwin vline-xwin)

          ;; Create OSD displaying current column
          (let ((osd (xwem-osd-create (xwem-dpy) 0 0 100 100
                                      (xwem-cl-xwin client))))
            (xwem-osd-set-color osd color)
            (xwem-osd-set-font osd (or (plist-get vc :font)
                                       (xwem-face-font 'default)))
            (X-Win-put-prop vline-xwin 'vline-osd osd)))

        ;; Setup properties
        (X-Win-put-prop vline-xwin 'vline-column column)
        (X-Win-put-prop vline-xwin 'vline-column-width column-width)

        (xwem-vline-set-column vline-xwin column)

        (XMapWindow (xwem-dpy) vline-xwin)
        (xwem-osd-show (X-Win-get-prop vline-xwin 'vline-osd)))
      (xwem-turn-on-minor-mode client 'xwem-vline-minor-mode))))

;;;###autoload(autoload 'xwem-turn-off-vline "xwem-vline" nil t)
(define-xwem-command xwem-turn-off-vline (client)
  "Disable vline minor mode for CLIENT."
  (xwem-interactive (list (xwem-cl-selected)))

  (let ((vline-xwin (xwem-cl-get-sys-prop client 'vline-xwin)))
    (when (X-Win-p vline-xwin)
      (XDestroyWindow (xwem-dpy) vline-xwin)
      (xwem-cl-rem-sys-prop client 'vline-xwin)))
  (xwem-turn-off-minor-mode client 'xwem-vline-minor-mode))

;;;###autoload(autoload 'xwem-vline-minor-mode "xwem-vline" nil t)
(define-xwem-command xwem-vline-minor-mode (arg &optional client)
  "According to prefix ARG toggle vline minor mode for CLIENT.
If ARG is positive - turn it on.
If ARG is negative - turn it off."
  (xwem-interactive (list xwem-prefix-arg (xwem-cl-selected)))

  (if (or (and (listp arg) (xwem-client-local-variable-value
                            client 'xwem-vline-minor-mode))
          (and (numberp arg) (< arg 0)))
      (xwem-turn-off-vline client)
    (xwem-turn-on-vline client nil (and (numberp arg) arg))))


(define-xwem-deferred xwem-vline-refit (cl)
  "Halde CLIENT refiting."
  (when (xwem-cl-alive-p cl)
    (let ((vline-xwin (xwem-cl-get-sys-prop cl 'vline-xwin)))
      (when (X-Win-p vline-xwin)
        (XResizeWindow (xwem-dpy) vline-xwin xwem-vline-width
                       (X-Geom-height (xwem-cl-xgeom cl)))))))


;;; On-load actions:
(xwem-add-minor-mode 'xwem-vline-minor-mode "Vline")

(add-hook 'xwem-cl-refit-hook 'xwem-vline-refit)


(provide 'xwem-vline)

;;; xwem-vline.el ends here
