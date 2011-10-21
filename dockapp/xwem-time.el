;;; xwem-time.el --- Time Date Load and Mail display in tray.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;; Created: Mon Dec  8 09:53:42 MSK 2003
;; Keywords: xwem, xlib
;; Time-stamp: <11/9/2007 16:29:11 lg@h1>

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

;; `display-time' like application, which starts in system tray and
;; shows current time, load average and mail status.
;;
;; To start using it add:
;;
;;    (add-hook 'xwem-after-init-hook 'xwem-time)
;;
;; to your xwemrc.

;;; Code:

(eval-when-compile
  (require 'itimer))

(require 'time)

(require 'xlib-xshape)
(require 'xlib-xpm)
(ignore-errors (require 'xlib-img))
(require 'xlib-tray)

(require 'xwem-load)

;;; Customisation
(defgroup xwem-time nil
  "Group to customize XWEM time display."
  :prefix "xwem-time-"
  :group 'xwem)

(defcustom xwem-time-format '(time load mail)
  "*Format to display time/load/mail.
List of keywords, where each keyword is either:

  time - Display time
  load - Display load average
  mail - Display mail status."
  :type '(repeat (choice (const :tag "Time" time)
                         (const :tag "Load average" load)
                         (const :tag "Mail status" mail)))
  :set (lambda (sym val)
         (set sym val)
         (when xwem-time-win
           (xwem-time-reformat xwem-time-win)))
  :initialize 'custom-initialize-default
  :group 'xwem-time)

(defcustom xwem-time-format-distance 3
  "*Distance in pixels between time/load/mail items."
  :type 'number
  :group 'xwem-time)

;; Time
(defcustom xwem-time-time-color "#CA1E1C"
  "Foreground color to display time."
  :type 'color
  :group 'xwem-time)

(defcustom xwem-time-update-interval 1
  "*Seconds between updates of xwem time window."
  :type 'integer
  :group 'xwem-time)

;; Load average
(defcustom xwem-time-load-list
  (list 0.10 0.20 0.30 0.40 0.50 0.60 0.80 1.0 1.2 1.5 1.8)
  "*A list giving 11 thresholds for the load which correspond to the 11
different icons to be displayed as a load indicator."
  :type '(list (number :tag "Threshold 1")
	       (number :tag "Threshold 2")
	       (number :tag "Threshold 3")
	       (number :tag "Threshold 4")
	       (number :tag "Threshold 5")
	       (number :tag "Threshold 6")
	       (number :tag "Threshold 7")
	       (number :tag "Threshold 8")
	       (number :tag "Threshold 9")
	       (number :tag "Threshold 10")
	       (number :tag "Threshold 11"))
  :group 'xwem-time)

(defcustom xwem-time-load-interval 5
  "*Seconds between load average updates."
  :type 'integer
  :group 'xwem-time)

;; Mail
(defcustom xwem-time-get-mail-function 'xwem-time-default-get-mail
  "Function to call in order to check mail availability."
  :type 'function
  :group 'xwem-time)

(defvar xwem-time-map
  (let ((map (make-sparse-keymap)))
    (define-key map [button1] 'xwem-time-show-current-time-and-date)
    (define-key map [button3] 'xwem-time-popup-menu)
    map)
  "Keymap used when clicking time dockapp.")


;;; Internal variables
(defvar xwem-time-dockapp-height 13)

(defconst xwem-time-digit-width 9)
(defconst xwem-time-ampm-width 4)
(defconst xwem-time-load-width 10)
(defconst xwem-time-mail-width 18)

;;; Icons
(defvar xwem-time-xpm-empty-digit (concat "/* XPM */\n"
                                          "static char *noname[] = {\n"
                                          "/* width height ncolors chars_per_pixel */\n"
                                          "\"9 13 2 1\",\n"
                                          "/* colors */\n"
                                          "\"`	c None    s ledbg\",\n"
                                          "\"a	c black   s ledfg\",\n"
                                          "/* pixels */\n"
                                          "\"`````````\",\n"
                                          "\"`````````\",\n"
                                          "\"`````````\",\n"
                                          "\"`````````\",\n"
                                          "\"`````````\",\n"
                                          "\"`````````\",\n"
                                          "\"`````````\",\n"
                                          "\"`````````\",\n"
                                          "\"`````````\",\n"
                                          "\"`````````\",\n"
                                          "\"`````````\",\n"
                                          "\"`````````\",\n"
                                          "\"`````````\"\n"
                                          "};\n"))

(defvar xwem-time-xpm-time0 '(concat "/* XPM */\n"
				     "static char *noname[] = {\n"
				     "/* width height ncolors chars_per_pixel */\n"
				     "\"9 13 2 1\",\n"
				     "/* colors */\n"
				     "\"`	c None    s ledbg\",\n"
				     "\"a	c " xwem-time-time-color " s ledfg\",\n"
				     "/* pixels */\n"
				     "\"`````````\",\n"
				     "\"````aaaaa\",\n"
				     "\"```a````a\",\n"
				     "\"```a````a\",\n"
				     "\"``a````a`\",\n"
				     "\"``a````a`\",\n"
				     "\"`````````\",\n"
				     "\"`a````a``\",\n"
				     "\"`a````a``\",\n"
				     "\"a````a```\",\n"
				     "\"a````a```\",\n"
				     "\"aaaaa````\",\n"
				     "\"`````````\"\n"
				     "};\n"))

(defvar xwem-time-xpm-time1 '(concat "/* XPM */\n"
				     "static char *noname[] = {\n"
				     "/* width height ncolors chars_per_pixel */\n"
				     "\"9 13 2 1\",\n"
				     "/* colors */\n"
				     "\"`	c None s ledbg\",\n"
				     "\"a	c " xwem-time-time-color " s ledfg\",\n"
				     "/* pixels */\n"
				     "\"`````````\",\n"
				     "\"`````````\",\n"
				     "\"````````a\",\n"
				     "\"````````a\",\n"
				     "\"```````a`\",\n"
				     "\"```````a`\",\n"
				     "\"`````````\",\n"
				     "\"``````a``\",\n"
				     "\"``````a``\",\n"
				     "\"`````a```\",\n"
				     "\"`````a```\",\n"
				     "\"`````````\",\n"
				     "\"`````````\"\n"
				     "};\n"))

(defconst xwem-time-xpm-time2 '(concat "/* XPM */\n"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"9 13 2 1\",\n"
				       "/* colors */\n"
				       "\"` c None    s ledbg\",\n"
				       "\"a c " xwem-time-time-color " s ledfg\",\n"
				       "/* pixels */\n"
				       "\"`````````\",\n"
				       "\"````aaaaa\",\n"
				       "\"````````a\",\n"
				       "\"````````a\",\n"
				       "\"```````a`\",\n"
				       "\"```````a`\",\n"
				       "\"``aaaaa``\",\n"
				       "\"`a```````\",\n"
				       "\"`a```````\",\n"
				       "\"a````````\",\n"
				       "\"a````````\",\n"
				       "\"aaaaa````\",\n"
				       "\"`````````\"\n"
				       "};\n"))

(defconst xwem-time-xpm-time3 '(concat "/* XPM */\n"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"9 13 2 1\",\n"
				       "/* colors */\n"
				       "\"` c None    s ledbg\",\n"
				       "\"a c " xwem-time-time-color " s ledfg\",\n"
				       "/* pixels */\n"
				       "\"`````````\",\n"
				       "\"````aaaaa\",\n"
				       "\"````````a\",\n"
				       "\"````````a\",\n"
				       "\"```````a`\",\n"
				       "\"```````a`\",\n"
				       "\"``aaaaa``\",\n"
				       "\"``````a``\",\n"
				       "\"``````a``\",\n"
				       "\"`````a```\",\n"
				       "\"`````a```\",\n"
				       "\"aaaaa````\",\n"
				       "\"`````````\"\n"
				       "};\n"))

(defconst xwem-time-xpm-time4 '(concat "/* XPM */\n"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"9 13 2 1\",\n"
				       "/* colors */\n"
				       "\"` c None    s ledbg\",\n"
				       "\"a c " xwem-time-time-color " s ledfg\",\n"
				       "/* pixels */\n"
				       "\"`````````\",\n"
				       "\"`````````\",\n"
				       "\"```a````a\",\n"
				       "\"```a````a\",\n"
				       "\"``a````a`\",\n"
				       "\"``a````a`\",\n"
				       "\"``aaaaa``\",\n"
				       "\"``````a``\",\n"
				       "\"``````a``\",\n"
				       "\"`````a```\",\n"
				       "\"`````a```\",\n"
				       "\"`````````\",\n"
				       "\"`````````\"\n"
				       "};\n"))


(defconst xwem-time-xpm-time5 '(concat "/* XPM */\n"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"9 13 2 1\",\n"
				       "/* colors */\n"
				       "\"` c None    s ledbg\",\n"
				       "\"a c " xwem-time-time-color " s ledfg\",\n"
				       "/* pixels */\n"
				       "\"`````````\",\n"
				       "\"````aaaaa\",\n"
				       "\"```a`````\",\n"
				       "\"```a`````\",\n"
				       "\"``a``````\",\n"
				       "\"``a``````\",\n"
				       "\"``aaaaa``\",\n"
				       "\"``````a``\",\n"
				       "\"``````a``\",\n"
				       "\"`````a```\",\n"
				       "\"`````a```\",\n"
				       "\"aaaaa````\",\n"
				       "\"`````````\"\n"
				       "};\n"))

(defconst xwem-time-xpm-time6 '(concat "/* XPM */\n"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"9 13 2 1\",\n"
				       "/* colors */\n"
				       "\"` c None    s ledbg\",\n"
				       "\"a c " xwem-time-time-color " s ledfg\",\n"
				       "/* pixels */\n"
				       "\"`````````\",\n"
				       "\"````aaaaa\",\n"
				       "\"```a`````\",\n"
				       "\"```a`````\",\n"
				       "\"``a``````\",\n"
				       "\"``a``````\",\n"
				       "\"``aaaaa``\",\n"
				       "\"`a````a``\",\n"
				       "\"`a````a``\",\n"
				       "\"a````a```\",\n"
				       "\"a````a```\",\n"
				       "\"aaaaa````\",\n"
				       "\"`````````\"\n"
				       "};\n"))

(defconst xwem-time-xpm-time7 '(concat "/* XPM */\n"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"9 13 2 1\",\n"
				       "/* colors */\n"
				       "\"` c None    s ledbg\",\n"
				       "\"a c " xwem-time-time-color " s ledfg\",\n"
				       "/* pixels */\n"
				       "\"`````````\",\n"
				       "\"````aaaaa\",\n"
				       "\"````````a\",\n"
				       "\"````````a\",\n"
				       "\"```````a`\",\n"
				       "\"```````a`\",\n"
				       "\"`````````\",\n"
				       "\"``````a``\",\n"
				       "\"``````a``\",\n"
				       "\"`````a```\",\n"
				       "\"`````a```\",\n"
				       "\"`````````\",\n"
				       "\"`````````\"\n"
				       "};\n"))

(defconst xwem-time-xpm-time8 '(concat "/* XPM */\n"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"9 13 2 1\",\n"
				       "/* colors */\n"
				       "\"` c None    s ledbg\",\n"
				       "\"a c " xwem-time-time-color " s ledfg\",\n"
				       "/* pixels */\n"
				       "\"`````````\",\n"
				       "\"````aaaaa\",\n"
				       "\"```a````a\",\n"
				       "\"```a````a\",\n"
				       "\"``a````a`\",\n"
				       "\"``a````a`\",\n"
				       "\"``aaaaa``\",\n"
				       "\"`a````a``\",\n"
				       "\"`a````a``\",\n"
				       "\"a````a```\",\n"
				       "\"a````a```\",\n"
				       "\"aaaaa````\",\n"
				       "\"`````````\"\n"
				       "};\n"))

(defconst xwem-time-xpm-time9 '(concat "/* XPM */\n"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"9 13 2 1\",\n"
				       "/* colors */\n"
				       "\"` c None    s ledbg\",\n"
				       "\"a c " xwem-time-time-color " s ledfg\",\n"
				       "/* pixels */\n"
				       "\"`````````\",\n"
				       "\"````aaaaa\",\n"
				       "\"```a````a\",\n"
				       "\"```a````a\",\n"
				       "\"``a````a`\",\n"
				       "\"``a````a`\",\n"
				       "\"``aaaaa``\",\n"
				       "\"``````a``\",\n"
				       "\"``````a``\",\n"
				       "\"`````a```\",\n"
				       "\"`````a```\",\n"
				       "\"aaaaa````\",\n"
				       "\"`````````\"\n"
				       "};\n"))

(defconst xwem-time-xpm-am '(concat "/* XPM */\n"
				    "static char *noname[] = {\n"
				    "/* width height ncolors chars_per_pixel */\n"
				    "\"4 13 2 1\",\n"
				    "/* colors */\n"
				    "\"` c None    s ledbg\",\n"
				    "\"a c " xwem-time-time-color " s ledfg\",\n"
				    "/* pixels */\n"
				    "\"````\",\n"
				    "\"``aa\",\n"
				    "\"``aa\",\n"
				    "\"````\",\n"
				    "\"````\",\n"
				    "\"````\",\n"
				    "\"````\",\n"
				    "\"````\",\n"
				    "\"````\",\n"
				    "\"````\",\n"
				    "\"````\",\n"
				    "\"````\",\n"
				    "\"````\"\n"
				    "};\n"))

(defconst xwem-time-xpm-dp '(concat "/* XPM */\n"
				    "static char *noname[] = {\n"
				    "/* width height ncolors chars_per_pixel */\n"
				    "\"9 13 2 1\",\n"
				    "/* colors */\n"
				    "\"` c None    s ledbg\",\n"
				    "\"a c " xwem-time-time-color " s ledfg\",\n"
				    "/* pixels */\n"
				    "\"`````````\",\n"
				    "\"`````````\",\n"
				    "\"`````````\",\n"
				    "\"`````````\",\n"
				    "\"````a````\",\n"
				    "\"````a````\",\n"
				    "\"`````````\",\n"
				    "\"```a`````\",\n"
				    "\"```a`````\",\n"
				    "\"`````````\",\n"
				    "\"`````````\",\n"
				    "\"`````````\",\n"
				    "\"`````````\"\n"
				    "};\n"))

(defconst xwem-time-xpm-pm '(concat "/* XPM */\n"
				    "static char *noname[] = {\n"
				    "/* width height ncolors chars_per_pixel */\n"
				    "\"4 13 2 1\",\n"
				    "/* colors */\n"
				    "\"` c None    s ledbg\",\n"
				    "\"a c " xwem-time-time-color " s ledfg\",\n"
				    "/* pixels */\n"
				    "\"````\",\n"
				    "\"````\",\n"
				    "\"````\",\n"
				    "\"````\",\n"
				    "\"````\",\n"
				    "\"````\",\n"
				    "\"````\",\n"
				    "\"````\",\n"
				    "\"````\",\n"
				    "\"````\",\n"
				    "\"aa``\",\n"
				    "\"aa``\",\n"
				    "\"````\"\n"
				    "};\n"))
;; load icons
(defconst xwem-time-xpm-load00 (concat "/* XPM */\n"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"10 13 2 1\",\n"
				       "/* colors */\n"
				       "\"` s None c None\",\n"
				       "\". s pad-color c #606060\",\n"
				       "/* pixels */\n"
				       "\"``````....\",\n"
				       "\"``````....\",\n"
				       "\"`````.....\",\n"
				       "\"`````.....\",\n"
				       "\"````......\",\n"
				       "\"````......\",\n"
				       "\"```.......\",\n"
				       "\"```.......\",\n"
				       "\"``........\",\n"
				       "\"``........\",\n"
				       "\"`.........\",\n"
				       "\"`.........\",\n"
				       "\"..........\"\n"
				       "};\n"))

(defconst xwem-time-xpm-load05 (concat "/* XPM */"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"10 13 3 1\",\n"
				       "\/* colors \*/\n"
				       "\"` s None c None\",\n"
				       "\"a c #0AB224\",\n"
				       "\". s pad-color c #606060\",\n"
				       "/* pixels */\n"
				       "\"``````....\",\n"
				       "\"``````....\",\n"
				       "\"`````.....\",\n"
				       "\"`````.....\",\n"
				       "\"````......\",\n"
				       "\"````......\",\n"
				       "\"```.......\",\n"
				       "\"```.......\",\n"
				       "\"``........\",\n"
				       "\"``........\",\n"
				       "\"`.........\",\n"
				       "\"`aaaaaaaaa\",\n"
				       "\"..........\"\n"
				       "};\n"))

(defconst xwem-time-xpm-load10 (concat "/* XPM */"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"10 13 3 1\",\n"
				       "\/* colors \*/\n"
				       "\"` s None c None\",\n"
				       "\"a c #0AB224\",\n"
				       "\". s pad-color c #606060\",\n"
				       "/* pixels */\n"
				       "\"``````....\",\n"
				       "\"``````....\",\n"
				       "\"`````.....\",\n"
				       "\"`````.....\",\n"
				       "\"````......\",\n"
				       "\"````......\",\n"
				       "\"```.......\",\n"
				       "\"```.......\",\n"
				       "\"``........\",\n"
				       "\"``........\",\n"
				       "\"`aaaaaaaaa\",\n"
				       "\"`aaaaaaaaa\",\n"
				       "\"..........\"\n"
				       "};\n"))

(defconst xwem-time-xpm-load15 (concat "/* XPM */"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"10 13 3 1\",\n"
				       "\/* colors \*/\n"
				       "\"` s None c None\",\n"
				       "\"a c #0AB224\",\n"
				       "\". s pad-color c #606060\",\n"
				       "/* pixels */\n"
				       "\"``````....\",\n"
				       "\"``````....\",\n"
				       "\"`````.....\",\n"
				       "\"`````.....\",\n"
				       "\"````......\",\n"
				       "\"````......\",\n"
				       "\"```.......\",\n"
				       "\"```.......\",\n"
				       "\"``........\",\n"
				       "\"``aaaaaaaa\",\n"
				       "\"`aaaaaaaaa\",\n"
				       "\"`aaaaaaaaa\",\n"
				       "\"..........\"\n"
				       "};\n"))

(defconst xwem-time-xpm-load20 (concat "/* XPM */\n"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"10 13 4 1\",\n"
				       "/* colors */\n"
				       "\"` s None c None\",\n"
				       "\"a c #2AD244\",\n"
				       "\"b c #DEE614\",\n"
				       "\". s pad-color c #606060\",\n"
				       "/* pixels */\n"
				       "\"``````....\",\n"
				       "\"``````....\",\n"
				       "\"`````.....\",\n"
				       "\"`````.....\",\n"
				       "\"````......\",\n"
				       "\"````......\",\n"
				       "\"```.......\",\n"
				       "\"```.......\",\n"
				       "\"``aaaaaaaa\",\n"
				       "\"``aaaaaaaa\",\n"
				       "\"`aaaaaaaaa\",\n"
				       "\"`aaaaaaaaa\",\n"
				       "\"..........\"\n"
				       "};\n"))

(defconst xwem-time-xpm-load25 (concat "/* XPM */\n"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"10 13 4 1\",\n"
				       "/* colors */\n"
				       "\"` s None c None\",\n"
				       "\"a c #2AD244\",\n"
				       "\"b c #DEE614\",\n"
				       "\". s pad-color c #606060\",\n"
				       "/* pixels */\n"
				       "\"``````....\",\n"
				       "\"``````....\",\n"
				       "\"`````.....\",\n"
				       "\"`````.....\",\n"
				       "\"````......\",\n"
				       "\"````......\",\n"
				       "\"```.......\",\n"
				       "\"```bbbbbbb\",\n"
				       "\"``aaaaaaaa\",\n"
				       "\"``aaaaaaaa\",\n"
				       "\"`aaaaaaaaa\",\n"
				       "\"`aaaaaaaaa\",\n"
				       "\"..........\"\n"
				       "};\n"))

(defconst xwem-time-xpm-load30 (concat "/* XPM */\n"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"10 13 4 1\",\n"
				       "/* colors */\n"
				       "\"` s None c None\",\n"
				       "\"a c #0AB224\",\n"
				       "\"b c #DEE614\",\n"
				       "\". s pad-color c #606060\",\n"
				       "/* pixels */\n"
				       "\"``````....\",\n"
				       "\"``````....\",\n"
				       "\"`````.....\",\n"
				       "\"`````.....\",\n"
				       "\"````......\",\n"
				       "\"````......\",\n"
				       "\"```bbbbbbb\",\n"
				       "\"```bbbbbbb\",\n"
				       "\"``aaaaaaaa\",\n"
				       "\"``aaaaaaaa\",\n"
				       "\"`aaaaaaaaa\",\n"
				       "\"`aaaaaaaaa\",\n"
				       "\"..........\"\n"
				       "};\n"))

(defconst xwem-time-xpm-load35 (concat "/* XPM */\n"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"10 13 4 1\",\n"
				       "/* colors */\n"
				       "\"` s None c None\",\n"
				       "\"a c #0AB224\",\n"
				       "\"b c #DEE614\",\n"
				       "\". s pad-color c #606060\",\n"
				       "/* pixels */\n"
				       "\"``````....\",\n"
				       "\"``````....\",\n"
				       "\"`````.....\",\n"
				       "\"`````.....\",\n"
				       "\"````......\",\n"
				       "\"````bbbbbb\",\n"
				       "\"```bbbbbbb\",\n"
				       "\"```bbbbbbb\",\n"
				       "\"``aaaaaaaa\",\n"
				       "\"``aaaaaaaa\",\n"
				       "\"`aaaaaaaaa\",\n"
				       "\"`aaaaaaaaa\",\n"
				       "\"..........\"\n"
				       "};\n"))

(defconst xwem-time-xpm-load40 (concat "/* XPM */\n"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"10 13 5 1\",\n"
				       "/* colors */\n"
				       "\"a s None c None\",\n"
				       "\"` c #FE0204\",\n"
				       "\"b c #0AB224\",\n"
				       "\"c c #DEE614\",\n"
				       "\". s pad-color c #606060\",\n"
				       "/* pixels */\n"
				       "\"aaaaaa....\",\n"
				       "\"aaaaaa....\",\n"
				       "\"aaaaa.....\",\n"
				       "\"aaaaa.....\",\n"
				       "\"aaaacccccc\",\n"
				       "\"aaaacccccc\",\n"
				       "\"aaaccccccc\",\n"
				       "\"aaaccccccc\",\n"
				       "\"aabbbbbbbb\",\n"
				       "\"aabbbbbbbb\",\n"
				       "\"abbbbbbbbb\",\n"
				       "\"abbbbbbbbb\",\n"
				       "\"..........\"\n"
				       "};\n"))

(defconst xwem-time-xpm-load45 (concat "/* XPM */\n"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"10 13 5 1\",\n"
				       "/* colors */\n"
				       "\"a s None c None\",\n"
				       "\"` c #FE0204\",\n"
				       "\"b c #0AB224\",\n"
				       "\"c c #DEE614\",\n"
				       "\". s pad-color c #606060\",\n"
				       "/* pixels */\n"
				       "\"aaaaaa....\",\n"
				       "\"aaaaaa....\",\n"
				       "\"aaaaa.....\",\n"
				       "\"aaaaa`````\",\n"
				       "\"aaaacccccc\",\n"
				       "\"aaaacccccc\",\n"
				       "\"aaaccccccc\",\n"
				       "\"aaaccccccc\",\n"
				       "\"aabbbbbbbb\",\n"
				       "\"aabbbbbbbb\",\n"
				       "\"abbbbbbbbb\",\n"
				       "\"abbbbbbbbb\",\n"
				       "\"..........\"\n"
				       "};\n"))

(defconst xwem-time-xpm-load50 (concat "/* XPM */\n"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"10 13 5 1\",\n"
				       "/* colors */\n"
				       "\"a s None c None\",\n"
				       "\"` c #FE0204\",\n"
				       "\"b c #0AB224\",\n"
				       "\"c c #DEE614\",\n"
				       "\". s pad-color c #606060\",\n"
				       "/* pixels */\n"
				       "\"aaaaaa....\",\n"
				       "\"aaaaaa....\",\n"
				       "\"aaaaa`````\",\n"
				       "\"aaaaa`````\",\n"
				       "\"aaaacccccc\",\n"
				       "\"aaaacccccc\",\n"
				       "\"aaaccccccc\",\n"
				       "\"aaaccccccc\",\n"
				       "\"aabbbbbbbb\",\n"
				       "\"aabbbbbbbb\",\n"
				       "\"abbbbbbbbb\",\n"
				       "\"abbbbbbbbb\",\n"
				       "\"..........\"\n"
				       "};\n"))

(defconst xwem-time-xpm-load55 (concat "/* XPM */\n"
				       "static char *noname[] = {\n"
				       "/* width height ncolors chars_per_pixel */\n"
				       "\"10 13 5 1\",\n"
				       "/* colors */\n"
				       "\"a s None c None\",\n"
				       "\"` c #FE0204\",\n"
				       "\"b c #0AB224\",\n"
				       "\"c c #DEE614\",\n"
				       "\". s pad-color c #606060\",\n"
				       "/* pixels */\n"
				       "\"aaaaaa....\",\n"
				       "\"aaaaaa````\",\n"
				       "\"aaaaa`````\",\n"
				       "\"aaaaa`````\",\n"
				       "\"aaaacccccc\",\n"
				       "\"aaaacccccc\",\n"
				       "\"aaaccccccc\",\n"
				       "\"aaaccccccc\",\n"
				       "\"aabbbbbbbb\",\n"
				       "\"aabbbbbbbb\",\n"
				       "\"abbbbbbbbb\",\n"
				       "\"abbbbbbbbb\",\n"
				       "\"..........\"\n"
				       "};\n"))

;; Mail icons
(defconst xwem-time-xpm-letter (concat "/* XPM */\n"
				       "static char * jmail_xpm[] = {\n"
				       "\"18 13 4 1\",\n"
				       "\" 	s None c None\",\n"
				       "\".	c gray85\",\n"
				       "\"X	c yellow\",\n"
				       "\"o	c black\",\n"
				       "\"                  \",\n"
				       "\"                  \",\n"
				       "\"   .XXXXXXXXXXX.  \",\n"
				       "\"   XoXXXXXXXXXoXoo\",\n"
				       "\"   XXoXXXXXXXoXXoo\",\n"
				       "\"   XXXoXXXXXoXXXoo\",\n"
				       "\"   XXX.oXXXo.XXXoo\",\n"
				       "\"   XXXo.oXo.oXXXoo\",\n"
				       "\"   XXoXXXoXXXoXXoo\",\n"
				       "\"   XoXXXXXXXXXoXoo\",\n"
				       "\"   .XXXXXXXXXXX.oo\",\n"
				       "\"     ooooooooooooo\",\n"
				       "\"     ooooooooooooo\"\n"
                                       "};\n"))

(defconst xwem-time-xpm-no-letter (concat "/* XPM */\n"
					  "static char * jmail_xpm[] = {\n"
					  "\"18 13 4 1\",\n"
					  "\" 	s None	c None\",\n"
					  "\".	c gray55\",\n"
					  "\"o	c black\",\n"
					  "\"x	c gray95\",\n"
					  "\"                  \",\n"
					  "\"                  \",\n"
					  "\"   ooooooooooooox \",\n"
					  "\"   o.xxxxxxxxx.ox \",\n"
					  "\"   oxox      oxox \",\n"
					  "\"   ox ox    ox ox \",\n"
					  "\"   ox  ox  ox  ox \",\n"
					  "\"   ox oxoxoxox ox \",\n"
					  "\"   oxox  ox  oxox \",\n"
					  "\"   o.x        .ox \",\n"
					  "\"   ooooooooooooox \",\n"
					  "\"   xxxxxxxxxxxxxx \",\n"
					  "\"                  \"\n"
                                          "};\n"))


;;; Huge amount of macroses
(defvar xwem-time-win nil)

(defmacro xwem-time-win (&optional win)
  `(or ,win xwem-time-win))
(defsetf xwem-time-win () (win)
  `(setq xwem-time-win ,win))
(defmacro xwem-time-get-prop (win prop)
  `(X-Win-get-prop (xwem-time-win ,win) ,prop))
(defmacro xwem-time-set-prop (win prop val)
  `(X-Win-put-prop (xwem-time-win ,win) ,prop ,val))

(defmacro xwem-time-mask (&optional win)
  `(xwem-time-get-prop ,win 'time-mask))
(defsetf xwem-time-mask (&optional win) (mask)
  `(xwem-time-set-prop ,win 'time-mask ,mask))

(defmacro xwem-time-pixmap (&optional win)
  `(xwem-time-get-prop ,win 'time-pixmap))
(defsetf xwem-time-pixmap (&optional win) (pixmap)
  `(xwem-time-set-prop ,win 'time-pixmap ,pixmap))

;; Digits operations
(defmacro xwem-time-digits-pixmaps (&optional win)
  `(xwem-time-get-prop ,win 'time-digits-pixmaps))
(defsetf xwem-time-digits-pixmaps (&optional win) (pixs)
  `(xwem-time-set-prop ,win 'time-digits-pixmaps ,pixs))
(defmacro xwem-time-digit-add (win digit pix pix-mask)
  `(setf (xwem-time-digits-pixmaps ,win)
         (cons (cons ,digit (cons ,pix ,pix-mask))
               (xwem-time-digits-pixmaps ,win))))
(defmacro xwem-time-digit-get-pix (win digit)
  `(car (cdr (assq ,digit (xwem-time-digits-pixmaps ,win)))))
(defmacro xwem-time-digit-get-mask (win digit)
  `(cdr (cdr (assq ,digit (xwem-time-digits-pixmaps ,win)))))

;; Load operations
(defmacro xwem-time-load-pixmaps (&optional win)
  `(xwem-time-get-prop ,win 'time-load-pixmaps))
(defsetf xwem-time-load-pixmaps (&optional win) (pixs)
  `(xwem-time-set-prop ,win 'time-load-pixmaps ,pixs))
(defmacro xwem-time-load-add (win load pix pix-mask)
  `(setf (xwem-time-load-pixmaps ,win)
         (cons (cons ,load (cons ,pix ,pix-mask))
               (xwem-time-load-pixmaps ,win))))
(defmacro xwem-time-load-get-pix (win load)
  `(car (cdr (assq ,load (xwem-time-load-pixmaps ,win)))))
(defmacro xwem-time-load-get-mask (win load)
  `(cdr (cdr (assq ,load (xwem-time-load-pixmaps ,win)))))

;; Mail operations
(defmacro xwem-time-mail-pixmaps (&optional win)
  `(xwem-time-get-prop ,win 'time-mail-pixmaps))
(defsetf xwem-time-mail-pixmaps (&optional win) (pixs)
  `(xwem-time-set-prop ,win 'time-mail-pixmaps ,pixs))
(defmacro xwem-time-mail-add (win mail pix pix-mask)
  `(setf (xwem-time-mail-pixmaps ,win)
         (cons (cons ,mail (cons ,pix ,pix-mask))
               (xwem-time-mail-pixmaps ,win))))
(defmacro xwem-time-mail-get-pix (win mail)
  `(car (cdr (assq ,mail (xwem-time-mail-pixmaps ,win)))))
(defmacro xwem-time-mail-get-mask (win mail)
  `(cdr (cdr (assq ,mail (xwem-time-mail-pixmaps ,win)))))

;; General macroses
(defmacro xwem-time-saved-state (&optional win)
  `(xwem-time-get-prop (xwem-time-win ,win) 'time-saved-state))
(defsetf xwem-time-saved-state (&optional win) (state)
  `(xwem-time-set-prop (xwem-time-win ,win) 'time-saved-state ,state))
(defmacro xwem-time-get-state (win state)
  `(plist-get (xwem-time-saved-state ,win) ,state))
(defmacro xwem-time-set-state (win state val)
  `(setf (xwem-time-saved-state ,win)
         (plist-put (xwem-time-saved-state ,win) ,state ,val)))

(defmacro xwem-time-itimer (&optional win)
  `(xwem-time-get-prop ,win 'time-itimer))
(defsetf xwem-time-itimer (&optional win) (itimer)
  `(xwem-time-set-prop ,win 'time-itimer ,itimer))

;; Format related stuff
(defsubst xwem-time-format-tag-width (tag)
  (ecase tag
    (time (* 5 xwem-time-digit-width))
    (load xwem-time-load-width)
    (mail xwem-time-mail-width)))
(defmacro xwem-time-format-offset (tag)
  `(let ((fmt xwem-time-format)
         (off 0))
     (while (and fmt (not (eq (car fmt) ,tag)))
       (incf off (xwem-time-format-tag-width (car fmt)))
       (incf off xwem-time-format-distance)
       (setq fmt (cdr fmt)))
     off))
(defmacro xwem-time-format-width ()
  `(+ (apply '+ (mapcar 'xwem-time-format-tag-width xwem-time-format))
      (* (1- (length xwem-time-format)) xwem-time-format-distance)))
(defmacro xwem-time-format-height ()
  'xwem-time-dockapp-height)

;;; Functions
(defun xwem-time-get-time ()
  "Return current time in format acceptable by `xwem-time-update-time'."
  (mapcar 'identity (substring (current-time-string) 11 16)))

(defun xwem-time-get-load ()
  "Return load average in format acceptable by `xwem-time-update-load'."
  (let ((alist (list (cons 0 0.0)
		     (cons 5 (nth 0 xwem-time-load-list))
		     (cons 10 (nth 1 xwem-time-load-list))
		     (cons 15 (nth 2 xwem-time-load-list))
		     (cons 20 (nth 3 xwem-time-load-list))
		     (cons 25 (nth 4 xwem-time-load-list))
		     (cons 30 (nth 5 xwem-time-load-list))
		     (cons 35 (nth 6 xwem-time-load-list))
		     (cons 40 (nth 7 xwem-time-load-list))
		     (cons 45 (nth 8 xwem-time-load-list))
		     (cons 50 (nth 9 xwem-time-load-list))
		     (cons 55 (nth 10 xwem-time-load-list))
		     (cons 100000 100000)))
        (load-number (car (load-average t)))
	elem load-elem)
    (while (>= load-number (cdr (setq elem (pop alist))))
      (setq load-elem elem))
    (car load-elem)))

(defun xwem-time-default-get-mail ()
  "Default function to search for new mail."
  (let* ((now (current-time))
         (nowhigh (* (- (nth 0 now) (* (/ (nth 0 now) 10) 10)) 65536))
         (mail-spool-file (or display-time-mail-file
                              (getenv "MAIL")
                              (concat rmail-spool-directory
                                      (user-login-name))))
         (mail (and (stringp mail-spool-file)
                    (or (null display-time-server-down-time)
                        ;; If have been down for 20 min, try again.
                        (> (- (+ (nth 1 now) nowhigh)
                              display-time-server-down-time)
                           1200))
                    (let ((start-time (current-time)))
                      (prog1
                          (display-time-file-nonempty-p mail-spool-file)
                        (setq now (current-time)
                              nowhigh (* (- (nth 0 now) (* (/ (nth 0 now) 10) 10)) 65536))
                        (if (> (- (+ (nth 1 now) nowhigh)
                                  (+ (nth 1 start-time)
                                     (* (- (nth 0 start-time)
                                           (* (/ (nth 0 start-time) 10) 10))
                                        65536)))
                               20)
                            ;; Record that mail file is not accessible.
                            (setq display-time-server-down-time 
                                  (+ (nth 1 now) nowhigh))
                          ;; Record that mail file is accessible.
                          (setq display-time-server-down-time nil)))))))
    mail))
  
(defun xwem-time-get-mail ()
  "Return mail status in format acceptable by `xwem-time-update-mail'."
  (if xwem-time-get-mail-function
      (if (funcall xwem-time-get-mail-function)
          'letter
        'no-letter)
    (xwem-time-default-get-mail)))

(define-xwem-deferred xwem-time-update (win)
  "Update time window WIN."
  (X-XShapeMask (xwem-dpy) win X-XShape-Bounding X-XShapeSet 0 0
                (xwem-time-mask win))
  (XCopyArea (xwem-dpy) (xwem-time-pixmap win) win
             (XDefaultGC (xwem-dpy)) 0 0
             (xwem-time-format-width) (xwem-time-format-height)
             0 0))

(defun xwem-time-update-digit (win digit-position digit)
  "Update WIN's DIGIT-POSITION to display DIGIT."
  (let ((off (+ (xwem-time-format-offset 'time)
                (* digit-position xwem-time-digit-width))))
    (XCopyArea (xwem-dpy) (xwem-time-digit-get-mask win digit)
               (xwem-time-mask win) xwem-misc-mask-fgc 0 0
               xwem-time-digit-width xwem-time-dockapp-height
               off 0)
    (XCopyArea (xwem-dpy) (xwem-time-digit-get-pix win digit)
               (xwem-time-pixmap win) (XDefaultGC (xwem-dpy)) 0 0
               xwem-time-digit-width xwem-time-dockapp-height
               off 0)
    (xwem-time-update win)))
    
(defun xwem-time-update-time (win new-time)
  "Update WIN to display NEW-TIME."
  (let ((st (or (xwem-time-get-state win 'time) '(-1 -1 -1 -1 -1)))
        (dpos 0))
    (mapc (lambda (t1 t2)
            (unless (= t1 t2)
              (xwem-time-update-digit win dpos t2))
            (incf dpos))
          st new-time)
    (xwem-time-set-state win 'time new-time)))

(defun xwem-time-update-load (win new-load)
  "Update WIN to display NEW-LOAD."
  (let ((sl (or (xwem-time-get-state win 'load) -1)))
    (unless (= sl new-load)
      (let ((off (xwem-time-format-offset 'load)))
        (XCopyArea (xwem-dpy) (xwem-time-load-get-mask win new-load)
                   (xwem-time-mask win) xwem-misc-mask-fgc 0 0
                   xwem-time-load-width xwem-time-dockapp-height
                   off 0)
        (XCopyArea (xwem-dpy) (xwem-time-load-get-pix win new-load)
                   (xwem-time-pixmap win) (XDefaultGC (xwem-dpy)) 0 0
                   xwem-time-load-width xwem-time-dockapp-height
                   off 0)
        (xwem-time-update win)))
    (xwem-time-set-state win 'load new-load)))

(defun xwem-time-update-mail (win new-mail)
  "Update WIN to display NEW-MAIL."
  (let ((sl (xwem-time-get-state win 'mail)))
    (unless (eq sl new-mail)
      (let ((off (xwem-time-format-offset 'mail)))
        (XCopyArea (xwem-dpy) (xwem-time-mail-get-mask win new-mail)
                   (xwem-time-mask win) xwem-misc-mask-fgc 0 0
                   xwem-time-mail-width xwem-time-dockapp-height
                   off 0)
        (XCopyArea (xwem-dpy) (xwem-time-mail-get-pix win new-mail)
                   (xwem-time-pixmap win) (XDefaultGC (xwem-dpy)) 0 0
                   xwem-time-mail-width xwem-time-dockapp-height
                   off 0)
        (xwem-time-update win)))
    (xwem-time-set-state win 'mail new-mail)))

(defun xwem-time-create-win (xdpy)
  "On display XDPY create time dockapp window."
  (let ((gc-cons-threshold most-positive-fixnum) ; inhibit gc
        (win (XCreateWindow xdpy (XDefaultRootWindow xdpy)
                            0 0 (xwem-time-format-width)
                            (xwem-time-format-height) 0
                            nil nil nil
                            :event-mask (Xmask-or XM-Exposure XM-StructureNotify
                                                  XM-ButtonPress XM-ButtonRelease)
                            :override-redirect t)))
    ;; Create mask pixmap and bs pixmap
    (setf (xwem-time-mask win)
          (XCreatePixmap (xwem-dpy) win 1 (xwem-time-format-width) (xwem-time-format-height)))
    (XFillRectangle (xwem-dpy) (xwem-time-mask win)
                    xwem-misc-mask-bgc 0 0 (xwem-time-format-width) (xwem-time-format-height))

    (setf (xwem-time-pixmap win)
          (XCreatePixmap (xwem-dpy) win (XDefaultDepth (xwem-dpy))
                         (xwem-time-format-width) (xwem-time-format-height)))
    (when xwem-misc-turbo-mode
      (XSetWindowBackgroundPixmap (xwem-dpy) win (xwem-time-pixmap win)))

;    (XFillRectangle (xwem-dpy) (xwem-time-pixmap win)
;                    (XDefaultGC (xwem-dpy)) 0 0
;                    (xwem-time-format-width) (xwem-time-format-height))

    ;; Load digits pixmaps
    (mapc (lambda (digit)
            (let* ((sym (intern (format "xwem-time-xpm-time%c" digit)))
                   (sval (symbol-value sym)))
              (xwem-time-digit-add
               win digit (X:xpm-pixmap-from-data xdpy win (eval sval))
               (X:xpm-pixmap-from-data xdpy win (eval sval) t))))
          "0123456789")
    ;; Empty digit and Colon
    (xwem-time-digit-add win ?\x20 (X:xpm-pixmap-from-data xdpy win xwem-time-xpm-empty-digit)
                         (X:xpm-pixmap-from-data xdpy win xwem-time-xpm-empty-digit t))
    (xwem-time-digit-add win ?: (X:xpm-pixmap-from-data xdpy win (eval xwem-time-xpm-dp))
                         (X:xpm-pixmap-from-data xdpy win (eval xwem-time-xpm-dp) t))
    
    ;; Load load pixmaps
    (mapc (lambda (load)
            (let* ((sym (intern (format "xwem-time-xpm-load%.2d" load)))
                   (sval (symbol-value sym)))
              (xwem-time-load-add
               win load (X:xpm-pixmap-from-data xdpy win sval)
               (X:xpm-pixmap-from-data xdpy win sval t))))
          (loop for i from 0 to 55 by 5 collect i))

    ;; Load mail pixmaps
    (xwem-time-mail-add win 'letter (X:xpm-pixmap-from-data xdpy win xwem-time-xpm-letter)
                        (X:xpm-pixmap-from-data xdpy win xwem-time-xpm-letter t))
    (xwem-time-mail-add win 'no-letter (X:xpm-pixmap-from-data xdpy win xwem-time-xpm-no-letter)
                        (X:xpm-pixmap-from-data xdpy win xwem-time-xpm-no-letter t))

    ;; Install event handler
    (X-Win-EventHandler-add win 'xwem-time-event-handler nil
                            (list X-Expose X-MapNotify X-DestroyNotify
                                  X-ButtonPress X-ButtonRelease))

    ;; Set default time window
    (unless xwem-time-win
      (setq xwem-time-win win))

    ;; Initial mask
    (XFillRectangle xdpy (xwem-time-mask win) xwem-misc-mask-bgc 0 0
                    (xwem-time-format-width) (xwem-time-format-height))
    (X-XShapeMask (xwem-dpy) win X-XShape-Bounding X-XShapeSet 0 0
                  (xwem-time-mask win))

    win))

(defun xwem-time-maybe-update (win)
  "Update WIN."
  (when (memq 'time xwem-time-format)
    (xwem-time-update-time win (xwem-time-get-time)))
  (when (memq 'load xwem-time-format)
    (xwem-time-update-load win (xwem-time-get-load)))
  (when (memq 'mail xwem-time-format)
    (xwem-time-update-mail win (xwem-time-get-mail)))

  (unless (xwem-time-itimer win)
    (setf (xwem-time-itimer win)
          (start-itimer "xwem-time-update" 'xwem-time-maybe-update
                        xwem-time-update-interval xwem-time-update-interval
                        nil t win))))

(defun xwem-time-remove (win &optional no-destroy)
  "Destroy win."
  (when (xwem-time-itimer win)
    (delete-itimer (xwem-time-itimer win))
    (setf (xwem-time-itimer win) nil))
     
  (mapc (lambda (pp)
          (XFreePixmap (xwem-dpy) (car (cdr pp)))
          (XFreePixmap (xwem-dpy) (cdr (cdr pp))))
        (append (xwem-time-digits-pixmaps win)
                (xwem-time-load-pixmaps win)
                (xwem-time-mail-pixmaps win)))

  (XFreePixmap (xwem-dpy) (xwem-time-mask win))
  (XFreePixmap (xwem-dpy) (xwem-time-pixmap win))

  (setf (xwem-time-digits-pixmaps win) nil
        (xwem-time-load-pixmaps win) nil
        (xwem-time-mail-pixmaps win) nil
        (xwem-time-mask win) nil
        (xwem-time-pixmap win) nil
        (xwem-time-saved-state win) nil)

  ;; Remove event handler
  (X-Win-EventHandler-rem win 'xwem-time-event-handler)

  ;; Unset default xwem-time-win
  (when (eq xwem-time-win win)
    (setq xwem-time-win nil))

  (unless no-destroy
    (XDestroyWindow (xwem-dpy) win)))

(defun xwem-time-event-handler (xdpy win xev)
  "On display XDPY and window WIN handle event XEV."
  (X-Event-CASE xev
    (:X-MapNotify (xwem-time-maybe-update win))
    (:X-Expose (xwem-time-update win))
    (:X-DestroyNotify (xwem-time-remove win t))
    ((:X-ButtonPress :X-ButtonRelease)
     (let ((xwem-override-local-map xwem-time-map))
       (xwem-dispatch-command-xevent xev)))))

;;;###autoload
(defun xwem-time (&optional dockid dockgroup dockalign)
  "Start xwem time window in system tray."
  (interactive)
  (xwem-XTrayInit (xwem-dpy) (xwem-time-create-win (xwem-dpy))
                  dockid dockgroup dockalign)
  'started)

(define-xwem-command xwem-time-show-current-time-and-date ()
  "Display current time and date in the minibuffer."
  (xwem-interactive)
  (xwem-message 'info "Time: %s, Load: %S"
                (current-time-string) (load-average)))

(define-xwem-command xwem-time-popup-menu ()
  "Popup menu for time dockapp."
  (xwem-interactive)

  (unless (button-event-p xwem-last-event)
    (error 'xwem-error "`xwem-time-popup-menu' must be bound to mouse event"))

  ;; XXX
  (xwem-popup-menu
   (list "Time"
         (vector "Show Time" 'xwem-time-show-current-time-and-date)
         "---"
         (vector "Destroy" `(XDestroyWindow (xwem-dpy) ,(X-Event-win xwem-last-xevent))))))


(provide 'xwem-time)

;;; xwem-time.el ends here
