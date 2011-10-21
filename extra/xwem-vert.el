;;; xwem-vert.el --- Support for vertical text.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Tue Sep 14 22:51:27 GMT 2004
;; Keywords: xwem
;; Time-stamp: <30/11/2006 00:01:28 lg@h1>

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;;

;;; Code:

(require 'xwem-load)

(defun X-Font-property (xdpy font prop)
  "Return FONT's property PROP."
  (let ((props (append (X-Font-props font) nil))
	(patom (XInternAtom xdpy prop t)))
    (when (X-Atom-p patom)
      (setq patom (X-Atom-id patom))
      (while (and props (not (= (aref (car props) 0) patom)))
	(setq props (cdr props)))
      (aref (car props) 1))))

(defun xwem-vert-draw-text (d gc angel x y str)
  "At drawable D, using GC."
  (let* ((sfont (X-Gc-font gc))
	 (fn (X-Font-name sfont))
	 (fhe (X-Font-heigth sfont))
	 (fps (X-Font-property (xwem-dpy) sfont "PIXEL_SIZE"))
	 (san (sin angel))
	 (can (cos angel))
	 (x-off x)
	 (y-off y)
	 nfont mb1 me1)
    (when (string-match x-font-regexp fn)
      (setq mb1 (match-beginning 5)
            me1 (match-end 5))
      (setq nfont (concat (substring fn 0 mb1)
                          (replace-in-string
                           (format "[%S %S %S %S]"
                                   (truncate (* fps can))
                                   (truncate (* fps san))
                                   (truncate (- (* fps san)))
                                   (truncate (* fps can))
                                   )
                           "-" "~")
                          (substring fn me1)))
      (XChangeGC (xwem-dpy) gc :font (X-Font-get (xwem-dpy) nfont))

      (mapc #'(lambda (chr)
                (XDrawString (xwem-dpy) d gc x-off y-off (char-to-string chr))
                (setq x-off (+ x-off (* (X-Font-char-width chr sfont) can)))
                (setq y-off (+ y-off (* (X-Font-char-width chr sfont) (- san)))))
            str)

      ;; Revert font
      (XChangeGC (xwem-dpy) gc :font sfont))))

;(xwem-vert-draw-text (xwem-frame-xwin (xwem-frame-selected))
;		     (xwem-face-get-gc 'my-test-face) (/ pi 2)
;		     40 700 "abcdefghjklmnopqrstuvwxyz")
;


(provide 'xwem-vert)

;;; xwem-vert.el ends here
