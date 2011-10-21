;;; xwem-gamma.el --- XWEM addon used to adjust gamma.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;; Created: Tue Jan 20 13:10:28 MSK 2004
;; Keywords: xwem
;; Time-stamp: <30/11/2006 00:03:30 lg@h1>

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

;; `xwem-gamma' uses uses `xlib-vidmode' Xlib extension and
;; `color-selector' package.

;; To use it do something like:
;;
;;     (xwem-gamma-widget (xwem-dpy))

;;; Code:

(require 'xlib-vidmode)
(require 'color-selector)

(require 'xwem-load)
(require 'xwem-misc)

(defface xwem-gamma-face
  `((t (:foreground "#1a1a1a" :background "black")))
  "Face used to adjust gamma.")

(defvar xwem-gamma-display nil
  "X Display.")

(defvar xwem-gamma-screen 0
  "Screen number.")


;;; Functions
(defun xwem-gamma-corector (selector)
  "Using SELECTOR adjust gamma."
  (let* ((k (/ 9.9 256))
	 (clist (color-selector-get-color selector))
	 (rg (+ 0.1 (* k (car clist))))
	 (gg (+ 0.1 (* k (cadr clist))))
	 (bg (+ 0.1 (* k (caddr clist)))))
    (X-XF86VidModeSetGamma xwem-gamma-display rg gg bg xwem-gamma-screen)
    (XFlush xwem-gamma-display)
    (xwem-message 'nolog "New gamma: r=%f g=%f b=%f" rg gg bg)))

(defun xwem-gamma-widget (xdpy &optional screen-num)
  "Start correcting gama on display XDPY and screen SCREEN-NUM."
  (setq xwem-gamma-display xdpy)
  (setq xwem-gamma-screen (or (and (numberp screen-num) screen-num) 0))
  (color-selector-make-selector 'xwem-gamma-face 'foreground
				'xwem-gamma-corector))

;;;###autoload(autoload 'xwem-gamma "xwem-gamma" nil t)
(define-xwem-command xwem-gamma ()
  "Adjust gamma."
  (xwem-interactive)
  (let ((buf (get-buffer-create "*XWEM gamma*")))
    (with-current-buffer buf
      (xwem-gamma-widget (xwem-dpy))
      (xwem-misc-view-mode))
    (xwem-special-popup-frame buf)))


(provide 'xwem-gamma)

;;; xwem-gamma.el ends here
