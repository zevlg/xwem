;;; xwem-alarm.el --- Alarms notificator for xwem.

;; Copyright (C) 2007 by Zajcev Evgeny

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Keywords: alarm, osd

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
;; along with XWEM; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; 

;;; Code:

(defcustom xwem-alarm-osd-coordinates '(80 . 800)
  "*Coordinates for alarm OSD."
  :group 'xwem-alarm
  :type '(cons (number :tag "X") (number :tag "Y")))
  
(defcustom xwem-alarm-osd-font
  (font-create-name (make-font :weight "bold" :size 48))
  "Font to be used to display OSD message."
  :group 'xwem-alarm
  :type '(restricted-sexp :match-alternatives (nil try-font-name)))

(defcustom xwem-alarm-osd-color "red2"
  "*Default color for OSD messages."
  :group 'xwem-alarm
  :type 'color)

(defun xwem-alarm-osd-message (msg)
  (let* ((kmap (make-sparse-keymap))
         (mosd (xwem-osd-create (xwem-dpy) (car xwem-alarm-osd-coordinates)
                                (cdr xwem-alarm-osd-coordinates)
                                2048 1024 nil (list 'keymap kmap))))
    (define-key kmap [button1] `(lambda ()
                                  (interactive)
                                  (xwem-osd-destroy ,mosd)))

    (xwem-osd-set-font mosd xwem-alarm-osd-font)
    (xwem-osd-set-color mosd xwem-alarm-osd-color)

     (xwem-osd-icon-file-add
      mosd (xwem-icon-find-file "alarm-clock.xpm"))
     (xwem-osd-text-add
      mosd 125 0
      (encode-coding-string
       msg (xwem-misc-font-coding-system xwem-alarm-osd-font)))
    (xwem-osd-show mosd)

    (xwem-play-sound 'alarm)
    mosd))

(setq mosd (xwem-alarm-osd-message "23:05 Покер. (Мировая серия)"))
(xwem-osd-show mosd)
(setq ds (loop for i from 1 to 200 by 2
           collect (cons i (random 50))))

(setq ds '((1 . 3) (3 . 6) (5 . 4) (7 . 10) (9 . 10) (11 . 3)))
(xwem-osd-clear mosd)
(xwem-osd-dots-add mosd 'linespoints ds :color "green4" :x 120 :y 150)
(xwem-osd-set-height mosd 120)
(xwem-diag-plot-dots 'impulses (xwem-rootwin)
                     (xwem-face-get-gc 'xwem-face-white)
                     0 40 ds)

(provide 'xwem-alarm)

;;; xwem-alarm.el ends here
