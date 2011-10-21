;;; xwem-misc.el --- Misc stuff for XWEM.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: 21 Mar 2003
;; Keywords: xlib, xwem
;; Time-stamp: <21/10/2011 21:30:26 lg@localhost>

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
;;
;; This file used for misc purposes.
;;
;; If you have troubles with C-g key in Emacs, try to eval:
;;
;;    (set-input-mode nil nil nil ?\xff)
;;
;; I dont know where is bug, but sometimes my XEmacs behaves very
;; strange.  Especially after M-x C-h k.

;;; Code:
;;

(require 'xlib-xlib)

(require 'xwem-load)
(require 'xwem-sound)
(require 'advice)

(eval-and-compile
  (require 'cl)
  (ignore-errors (require 'xlib-img))

  (defvar iswitchb-buflist nil)         ; shutup compiler
  (defvar x-emacs-application-class nil)
  (autoload 'iswitchb-read-buffer "iswitchb") ; shutup compiler

  (defvar elp-function-list nil)        ; shut up compiler
  (autoload 'elp-instrument-list "elp" nil t)
  (autoload 'elp-results "elp" nil t)

  (autoload 'calc-eval "calc"))

(defmacro xwem-gc-function-choice ()
  "Return choice dialog to select GC function."
  `(`(choice (const :tag "None" nil)
             (const :tag "Clear" X-GXClear)
             (const :tag "And" X-GXAnd)
             (const :tag "Reverse And" X-GXAndReverse)
             (const :tag "Inverted And" X-GXAndInverted)
             (const :tag "Xor" X-GXXor)
             (const :tag "Or" X-GXOr)
             (const :tag "Reverse Or" X-GXOrReverse)
             (const :tag "Inverted Or" X-GXOrInverted)
             (const :tag "Nor" X-GXNor)
             (const :tag "Equive" X-GXEquiv)
             (const :tag "Invert" X-GXInvert)
             (const :tag "Copy" X-GXCopy)
             (const :tag "Inverted Copy" X-GXCopyInverted)
             (const :tag "Set" X-GXSet))))

(defmacro xwem-cursor-shape-choice ()
  "Return choice dialog to select cursor shape."
  `(`(choice (const :tag "Left" X-XC-left_ptr)
             (const :tag "Left w/mask" (X-XC-left_ptr))
             (const :tag "Right" X-XC-right_ptr )
             (const :tag "Right w/mask" (X-XC-right_ptr ))
             (const :tag "Cross" X-XC-cross)
             (const :tag "Cross w/mask" (X-XC-cross))
             (const :tag "Reverse Cross" X-XC-cross_reverse)
             (const :tag "Reverse Cross w/mask" (X-XC-cross_reverse))
             (const :tag "Crosshair" X-XC-crosshair)
             (const :tag "Crosshair w/mask" (X-XC-crosshair))
             (const :tag "Daimond cross" X-XC-diamond_cross)
             (const :tag "Daimond cross w/mask" (X-XC-diamond_cross))
             ;; TODO: add more, take a look at Cursors section in
             ;; xlib-const.el
             (const :tag "Dot" X-XC-dot)
             (const :tag "Dot w/mask" (X-XC-dot))
             (const :tag "Square Icon" X-XC-icon)
             (const :tag "Square Icon w/mask" (X-XC-icon))
             (const :tag "Fluer" X-XC-fleur)
             (const :tag "Fluer w/mask" (X-XC-fleur))

             ;; Arrows
             (const :tag "Down Arrow" X-XC-sb_down_arrow)
             (const :tag "Down Arrow w/mask" (X-XC-sb_down_arrow))
             (const :tag "Question Arrow" X-XC-question_arrow)
             (const :tag "Question Arrow w/mask" (X-XC-question_arrow))

             (const :tag "TopLeft Arrow" X-XC-top_left_arrow)
             (const :tag "TopLeft Arrow w/mask" (X-XC-top_left_arrow))
             (const :tag "Draft large" X-XC-draft_large)
             (const :tag "Draft large w/mask" (X-XC-draft_large))
             (const :tag "Draft small" X-XC-draft_small)
             (const :tag "Draft small w/mask" (X-XC-draft_small))

             ;; Corners
             (const :tag "Bottom Left corner" X-XC-bottom_left_corner)
             (const :tag "Bottom Left corner w/mask" (X-XC-bottom_left_corner))
             (const :tag "Bottom Right corner" X-XC-bottom_right_corner)
             (const :tag "Bottom Right corner w/mask" (X-XC-bottom_right_corner))
             (const :tag "Top Left corner" X-XC-top_left_corner)
             (const :tag "Top Left corner w/mask" (X-XC-top_left_corner))
             (const :tag "Top Right corner" X-XC-top_right_corner)
             (const :tag "Top Right corner w/mask" (X-XC-top_right_corner))

             (const :tag "Gumby guy" X-XC-gumby)
             (const :tag "Gumby guy w/mask" (X-XC-gumby))
             )))

(defmacro xwem-cus-set-cursor-foreground (cursor)
  "Generate :set function to change CURSOR's foreground"
  `(lambda (sym val)
     (set sym val)
     (when ,cursor
       (xwem-cursor-recolorize ,cursor val))))

(defmacro xwem-cus-set-cursor-background (cursor)
  "Generate :set function to change CURSOR's background"
  `(lambda (sym val)
     (set sym val)
     (when ,cursor
       (xwem-cursor-recolorize ,cursor nil val))))

(defmacro xwem-cus-set-cursor-shape (cursor &optional xwin)
  "Generate :set function to change CURSOR's background"
  `(lambda (sym val)
     (set sym val)
     (when ,cursor
       (let ((ncur (copy-X-Cursor ,cursor))
             src-char msk-char)
         (cond ((listp val)
                (setq src-char (eval (car val))
                      msk-char (1+ src-char)))
               (t (setq src-char (eval val)
                        msk-char src-char)))
         (setf (X-Cursor-id ncur) (X-Dpy-get-id (X-Cursor-dpy ,cursor)))
         (setf (X-Cursor-src-char ncur) src-char)
         (setf (X-Cursor-msk-char ncur) msk-char)
         (XFreeCursor (X-Cursor-dpy ,cursor) ,cursor)
         (XCreateGlyphCursor (X-Cursor-dpy ncur) ncur)
         (setq ,cursor ncur)))
     (if (listp ,xwin)
         (mapc #'(lambda (xw)
                   (when (and xw (X-Win-p xw))
                     (XSetWindowCursor (X-Win-dpy xw) xw ,cursor)))
               ,xwin)
       (when (and ,xwin (X-Win-p ,xwin))
         (XSetWindowCursor (X-Win-dpy ,xwin) ,xwin ,cursor)))))

(define-error 'xwem-internal-error
  "Internal XWEM error.")

(defgroup xwem-misc nil
  "Group to customize miscellaneous options."
  :prefix "xwem-"
  :group 'xwem)

(defcustom xwem-messages-buffer-name " *xwem-messages*"
  "*Buffer name for xwem messages."
  :type 'string
  :group 'xwem-misc)

(defcustom xwem-messages-buffer-lines 1000
  "*Maximum lines in xwem messages buffer."
  :type 'number
  :group 'xwem-misc)

(defconst xwem-messages-builtin-labels
  '(info note error warning alarm todo prompt progress nolog asis)
  "List of builtin labels.")

(defconst xwem-misc-font-x-registry-and-encoding-regexp
  "[-?]\\([^-]*\\)[-?]\\([^-]+\\)\\'")

;;;###autoload
(defcustom xwem-messages-ignore-log-labels
  '(prompt progress nolog)
  "*List of message labels to ignore putting them into xwem message log buffer."
  :type '(repeat (choice (symbol :tag "Custom label")
                         (const :tag "Alarm" alarm)
                         (const :tag "Error" error)
                         (const :tag "Warning" warning)
                         (const :tag "Info" info)
                         (const :tag "Note" note)
                         (const :tag "TODO" todo)
                         (const :tag "Prompt" prompt)
                         (const :tag "Progress" progress)
                         (const :tag "NoLog" nolog)))
  :group 'xwem-misc)

;;;###autoload
(defcustom xwem-messages-ignore-display-labels
  nil
  "*List of message labels to ignore displaying them in xwem minibuffer."
  :type '(repeat (choice (symbol :tag "Custom label")
                         (const :tag "Alarm" alarm)
                         (const :tag "Error" error)
                         (const :tag "Warning" warning)
                         (const :tag "Info" info)
                         (const :tag "Note" note)
                         (const :tag "TODO" todo)
                         (const :tag "Prompt" prompt)
                         (const :tag "Progress" progress)
                         (const :tag "NoLog" nolog)))
  :group 'xwem-misc)

;;;###autoload
(defcustom xwem-messages-ignore-log-regexps nil
  "*List of regexps matching messages should not be logged.
Customize this variable only if you know what you are doing.
Setting this variable will slow down messaging a bit."
  :type '(repeat regexp)
  :group 'xwem-misc)

;;;###autoload
(defcustom xwem-messages-ignore-display-regexps nil
  "*List of regexps matching messages should not be displayed in the minibuffer.
Customize this variable only if you know what you are doing.
Setting this variable will slow down messaging a bit."
  :type '(repeat regexp)
  :group 'xwem-misc)

(defcustom xwem-messages-beeps-alist
  '((warning . warning)
    (error . error)
    (alarm . alarm))
  "*Alist in form (MSG-LABEL . SOUND).
Where SOUND is element of `xwem-sound-alist'."
  :type `(repeat (cons (symbol :tag "Message Label")
                       ,(nconc '(choice)
                               (mapcar #'(lambda (ss)
                                           (list 'const :tag (symbol-name (car ss)) (car ss)))
                                       xwem-sound-alist)
                               '((symbol :tag "Sound type")))))
  :group 'xwem-misc)

(defcustom xwem-messages-label-prefixes
  '((warning "Warning" (red))
    (error "Error" (red bold))
    (alarm "Alarm" (red bold italic))
    (note "Note" (yellow))
    (info "Info" (default))
    (todo "TODO" (bold)))
  "List of prefixes for certain labels.
CAR is label."
  :type '(repeat (list (symbol :tag "Message label")
                       string
                       (repeat face)))
  :group 'xwem-misc)

;;;###xwem-autoload
(defcustom xwem-misc-turbo-mode nil
  "*Non-nil mean xwem will work as fast as it can.
In this case, some color related customizations may not apply on fly.
So on-fly theming will not work, etc.
However seting its value to non-nil is most convinient for most users."
  :type 'boolean
  :group 'xwem-misc)

(defcustom xwem-misc-functions-to-profile nil
  "List of functions to profile using xwem profiler."
  :type (list
         'repeat
         (cons 'choice
               (delq nil
                     (mapcar
                      #'(lambda (fun)
                          (and (symbolp fun)
                               (fboundp fun)
                               (> (length (symbol-name fun)) 4)
                               (string= "xwem" (substring (symbol-name fun) 0 4))
                               (list 'function-item fun)))
                      obarray))))
  :group 'xwem-misc)

;;; Cursors
(defgroup xwem-cursor nil
  "Group to customize cursors in XWEM."
  :prefix "xwem-cursor-"
  :group 'xwem)

;; Default cursor
(defcustom xwem-cursor-default-shape 'X-XC-left_ptr
  "*Shape of default xwem cursor."
  :type (xwem-cursor-shape-choice)
  :set (xwem-cus-set-cursor-shape xwem-cursor-default)
  :initialize 'custom-initialize-default
  :group 'xwem-cursor)

(defcustom xwem-cursor-default-foreground-color "#002800"
  "*Default cursor's foreground color."
  :type 'color
  :set (xwem-cus-set-cursor-foreground xwem-cursor-default)
  :initialize 'custom-initialize-default
  :group 'xwem-cursor)

(defcustom xwem-cursor-default-background-color "#000000"
  "*Default cursor's background color."
  :type 'color
  :set (xwem-cus-set-cursor-background xwem-cursor-default)
  :initialize 'custom-initialize-default
  :group 'xwem-cursor)

;; wait cursor
(defcustom xwem-cursor-wait-shape 'X-XC-icon
  "*Shape of cursor, when XWEM wait for something."
  :type (xwem-cursor-shape-choice)
  :set (xwem-cus-set-cursor-shape xwem-cursor-wait)
  :initialize 'custom-initialize-default
  :group 'xwem-cursor)

(defcustom xwem-cursor-wait-foreground-color "#ea0000"
  "*Cursor's foreground color when XWEM wait for something."
  :type 'color
  :set (xwem-cus-set-cursor-foreground xwem-cursor-wait)
  :initialize 'custom-initialize-default
  :group 'xwem-cursor)

(defcustom xwem-cursor-wait-background-color "#280000"
  "*Cursor's background color when XWEM waiit for something."
  :type 'color
  :set (xwem-cus-set-cursor-background xwem-cursor-wait)
  :initialize 'custom-initialize-default
  :group 'xwem-cursor)

;; move cursor
(defcustom xwem-cursor-move-shape 'X-XC-fleur
  "*Shape of cursor, when moving something."
  :type (xwem-cursor-shape-choice)
  :set (xwem-cus-set-cursor-shape xwem-cursor-move)
  :initialize 'custom-initialize-default
  :group 'xwem-cursor)

(defcustom xwem-cursor-move-foreground-color "#777777"
  "*Cursor's foreground color when moving something."
  :type 'color
  :set (xwem-cus-set-cursor-foreground xwem-cursor-move)
  :initialize 'custom-initialize-default
  :group 'xwem-cursor)

(defcustom xwem-cursor-move-background-color "#280000"
  "*Cursor's background color when moving something."
  :type 'color
  :set (xwem-cus-set-cursor-background xwem-cursor-move)
  :initialize 'custom-initialize-default
  :group 'xwem-cursor)

;; Resize cursor
(defcustom xwem-cursor-resize-shape 'X-XC-sizing
  "*Shape of cursor, when resizing something."
  :type (xwem-cursor-shape-choice)
  :set (xwem-cus-set-cursor-shape xwem-cursor-resize)
  :initialize 'custom-initialize-default
  :group 'xwem-cursor)

(defcustom xwem-cursor-resize-foreground-color "#777777"
  "*Cursor's foreground color when resizing something."
  :type 'color
  :set (xwem-cus-set-cursor-foreground xwem-cursor-resize)
  :initialize 'custom-initialize-default
  :group 'xwem-cursor)

(defcustom xwem-cursor-resize-background-color "#280000"
  "*Cursor's background color when resizing something."
  :type 'color
  :set (xwem-cus-set-cursor-background xwem-cursor-resize)
  :initialize 'custom-initialize-default
  :group 'xwem-cursor)

;; quote cursor
(defcustom xwem-cursor-quote-shape 'X-XC-sb_down_arrow
  "*Shape of cursor, when XWEM quoting keyboard or mouse."
  :type (xwem-cursor-shape-choice)
  :set (xwem-cus-set-cursor-shape xwem-cursor-quote)
  :initialize 'custom-initialize-default
  :group 'xwem-cursor)

(defcustom xwem-cursor-quote-foreground-color "#0000BB"
  "*Cursor's foreground color when XWEM quoting keyboard/mouse."
  :type 'color
  :set (xwem-cus-set-cursor-foreground xwem-cursor-quote)
  :initialize 'custom-initialize-default
  :group 'xwem-cursor)

(defcustom xwem-cursor-quote-background-color "#000099"
  "*Cursor's background color when XWEM quoting keyboard/mouse."
  :type 'color
  :set (xwem-cus-set-cursor-background xwem-cursor-quote)
  :initialize 'custom-initialize-default
  :group 'xwem-cursor)

;; help cursor
(defcustom xwem-cursor-help-shape '(X-XC-question_arrow)
  "*Shape of cursor, when getting help with XWEM."
  :type (xwem-cursor-shape-choice)
  :set (xwem-cus-set-cursor-shape xwem-cursor-help)
  :initialize 'custom-initialize-default
  :group 'xwem-cursor)

(defcustom xwem-cursor-help-foreground-color "#00BB00"
  "*Cursor's foreground color when quering XWEM for help."
  :type 'color
  :set (xwem-cus-set-cursor-foreground xwem-cursor-help)
  :initialize 'custom-initialize-default
  :group 'xwem-cursor)

(defcustom xwem-cursor-help-background-color "#009900"
  "*Cursor's background color when quering XWEM for help."
  :type 'color
  :set (xwem-cus-set-cursor-background xwem-cursor-help)
  :initialize 'custom-initialize-default
  :group 'xwem-cursor)

;;; Internal variables

;; cursor storages
(defvar xwem-cursor-fnt nil "Font for \"cursor\" series.")

(defvar xwem-cursor-default nil "Default cursor.")
(defvar xwem-cursor-left nil "Left cursor.")
(defvar xwem-cursor-right nil "Right cursor.")
(defvar xwem-cursor-wait nil "Cursor when we are wait.")
(defvar xwem-cursor-drag nil "Cursor when we drag.  Drug is a bad idea.")
(defvar xwem-cursor-move nil "Cursor when we move something.")
(defvar xwem-cursor-rsz-vert nil)
(defvar xwem-cursor-rsz-horz nil)
(defvar xwem-cursor-resize nil "Cursor when we resize.")
(defvar xwem-cursor-quote nil "Cursor when quoting key.")
(defvar xwem-cursor-help nil "Cursor when in help mode.")

(defvar xwem-misc-mask-pixmap nil "Pixmap with depth 1.")
;;;###xwem-autoload
(defvar xwem-misc-mask-fgc nil
  "X-Gc with foreground 1.0 destination drawable has depth 1.")
;;;###xwem-autoload
(defvar xwem-misc-mask-bgc nil
  "X-Gc with foreground 1.0 destination drawable has depth 1.")

;;; Macros
(defmacro xwem-xwin-frame (xwin)
  "Return XWEM frame, which X window is XWIN."
  `(X-Win-get-prop ,xwin 'xwem-frame))

;;;###xwem-autoload
(defmacro xwem-xwin-cl (xwin)
  "Return CL, which X window is XWIN."
  `(X-Win-get-prop ,xwin 'xwem-cl))
(defsetf xwem-xwin-cl (xwin) (cl)
  `(if (not ,cl)
       (X-Win-rem-prop ,xwin 'xwem-cl)
     (X-Win-put-prop ,xwin 'xwem-cl ,cl)))

(defadvice XCreateWindow (after xwin-created-by-xwem activate)
  "Mark window's created on xwem-dpy as window created by xwem."
  (let ((dpy (ad-get-arg 0))
        (xwin ad-return-value))
    (when (eq dpy (xwem-dpy))
      (X-Win-put-prop xwin 'xwin-created-by-xwem t))))

;;; Functions
;;;###xwem-autoload
(defun xwem-misc-colorspec->rgb-vector (colspec)
  "Conver color specification COLSPEC to internal representation.
COLSPEC maybe in form: #RRGGBB or name like 'green4'."
  (let ((col (if (fboundp 'color-instance-rgb-components)
		 (color-instance-rgb-components (make-color-instance colspec (default-x-device)))
	       (x-color-values colspec))))
    (and col (vconcat col))))

;;;###xwem-autoload
(defun xwem-misc-colorspec->rgb-vector-safe (colspec &optional defret)
  "Validate COLSPEC to be color specification in safe manner.
Return DEFRET or [0 0 0] if there was error."
  (condition-case nil
      (xwem-misc-colorspec->rgb-vector colspec)
    (t (or defret [0 0 0]))))

;;;###xwem-autoload
(defun xwem-misc-colorspec-valid-p (colspec)
  "Return non-nil if COLSPEC is valid color specification.
Valid colorspecification is spec in form: #RRGGBB or name like 'green4'."
  (condition-case nil
      (xwem-misc-colorspec->rgb-vector colspec)
    (t nil)))

;;;###xwem-autoload
(defun xwem-make-color (colorspec &optional cmap)
  "Create X-Color according to COLORSPEC."
  (let ((ccol (xwem-misc-colorspec->rgb-vector-safe colorspec [0 0 0])))
    (make-X-Color :red (aref ccol 0)
                  :green (aref ccol 1)
                  :blue (aref ccol 2))))

;;;###xwem-autoload
(defun xwem-make-cursor (type &optional fgcol bgcol)
  "Make new cursor of TYPE and store it in WHERE-STORE.
BGCOL maybe nil, that mean masking will not be done."
  (xwem-cursors-init)                   ; make sure cursor font loaded

  (let ((fgc (xwem-misc-colorspec->rgb-vector-safe fgcol [0 0 0]))
        (bgc (xwem-misc-colorspec->rgb-vector-safe bgcol 'invalid-bgcol))
        src-char msk-char)
    (cond ((listp type)
           (setq src-char (eval (car type))
                 msk-char (1+ src-char)))
          (t (setq src-char (eval type)
                   msk-char src-char)))
    (XCreateGlyphCursor
     (xwem-dpy) :source xwem-cursor-fnt :mask xwem-cursor-fnt
     :src-char src-char :msk-char msk-char
     :fgred (aref fgc 0) :fggreen (aref fgc 1) :fgblue (aref fgc 2)
     :bgred (unless (eq bgc 'invalid-bgcol) (aref bgc 0))
     :bggreen (unless (eq bgc 'invalid-bgcol) (aref bgc 1))
     :bgblue (unless (eq bgc 'invalid-bgcol) (aref bgc 2)))))

;;;###xwem-autoload
(defun xwem-cursor-recolorize (cursor new-fg &optional new-bg)
  "Recolorize CURSOR to use NEW-FG foreground and NEW-BG background."
  (let ((fgc (xwem-misc-colorspec->rgb-vector-safe new-fg))
        (bgc (xwem-misc-colorspec->rgb-vector-safe new-bg)))
    (XRecolorCursor (xwem-dpy) cursor
                    (aref fgc 0) (aref fgc 1) (aref fgc 2)
                    (aref bgc 0) (aref bgc 1) (aref bgc 2))))

(defun xwem-cursors-init ()
  "Initialize cursors."
  (unless xwem-cursor-fnt
    ;; Make cursors
    (xwem-message 'init "Initializing cursors ...")

    (setq xwem-cursor-fnt (XOpenFont (xwem-dpy) "cursor"))
    (setq xwem-cursor-default (xwem-make-cursor
                               xwem-cursor-default-shape
                               xwem-cursor-default-foreground-color
                               xwem-cursor-default-background-color)
          xwem-cursor-left (xwem-make-cursor
                            X-XC-left_ptr
                            xwem-cursor-default-foreground-color
                            xwem-cursor-default-background-color)
          xwem-cursor-right (xwem-make-cursor
                             X-XC-right_ptr
                             xwem-cursor-default-foreground-color
                             xwem-cursor-default-background-color)
          xwem-cursor-wait (xwem-make-cursor
                            xwem-cursor-wait-shape
                            xwem-cursor-wait-foreground-color
                            xwem-cursor-wait-background-color)
          xwem-cursor-move (xwem-make-cursor
                            xwem-cursor-move-shape
                            xwem-cursor-move-foreground-color
                            xwem-cursor-move-background-color)
          xwem-cursor-resize (xwem-make-cursor
                              xwem-cursor-resize-shape
                              xwem-cursor-resize-foreground-color
                              xwem-cursor-resize-background-color)
          xwem-cursor-quote (xwem-make-cursor
                             xwem-cursor-quote-shape
                             xwem-cursor-quote-foreground-color
                             xwem-cursor-quote-background-color)
          xwem-cursor-help (xwem-make-cursor
                            xwem-cursor-help-shape
                            xwem-cursor-help-foreground-color
                            xwem-cursor-help-background-color))

    (xwem-message 'init "Initializing cursors ... done")))

;;; Misc drawing
;;;###xwem-autoload
(defun xwem-misc-draw-shadow (dpy win gc1 gc2 x y w h thick)
  "Draw shadow."
  (let ((offset 0)
        s1 s2)
    (if (or (> (* thick 2) h) (> (* thick 2) w))
        nil                             ; undrawable
      (while (not (= thick offset))
        (setq s1 (cons (cons (make-X-Point :xx (+ x offset)
                                           :yy (+ y offset))
                             (make-X-Point :xx (+ x offset)
                                           :yy (- (+ y h) offset 1)))
                       s1))
        (setq s1 (cons (cons (make-X-Point :xx (+ x offset)
                                           :yy (+ y offset))
                             (make-X-Point :xx (- (+ x w) offset 1)
                                           :yy (+ y offset)))
                       s1))

        (setq s2 (cons (cons (make-X-Point :xx (+ x offset)
                                           :yy (- (+ y h) offset 1))
                             (make-X-Point :xx (- (+ x w) offset 1)
                                           :yy (- (+ y h) offset 1)))
                       s2))
        (setq s2 (cons (cons (make-X-Point :xx (- (+ x w) offset 1)
                                           :yy (+ y offset 1))
                             (make-X-Point :xx (- (+ x w) offset 1)
                                           :yy (- (+ y h) offset 1)))
                       s2))

        (setq offset (+ offset 1)))

      (when s1
        (XDrawSegments dpy win gc1 s1))
      (when s2
        (XDrawSegments dpy win gc2 s2))
      )))

;;;###xwem-autoload
(defun xwem-misc-draw-bar (dpy win gc1 gc2 gc3 x y w h th)
  "Draw shadowed bar.
Bar filled with GC1.
Shadow thickness is TH and it is drawed with GC2 and GC3."
  (xwem-debug 'xwem-misc "Draw bar .. x=%d y=%d w=%d h=%d" 'x 'y 'w 'h)

  (xwem-misc-draw-shadow dpy win gc2 gc3 x y w h th)
  (XFillRectangle dpy win gc1 (+ x th) (+ y th)
                  (- w (* th 2)) (- h (* th 2))))

;;;###xwem-autoload
(defun xwem-misc-find-frame (name &optional frames-list)
  "Find Emacs frame by its NAME."
  (let ((fl (or frames-list (frame-list)))
        (rf nil))

    (while fl
      (when (string= (frame-name (car fl)) name)
        ;; Found
        (setq rf (car fl))
        (setq fl nil))
      (setq fl (cdr fl)))
    rf))

;;;###xwem-autoload
(defun xwem-misc-find-emacs-frame (cl)
  "Return Emacs frame that corresponds CL.
HACK, DO NOT USE."
  (let ((fr (xwem-cl-get-sys-prop cl 'emacs-frame)))
    (if fr
        (and (framep fr) fr)
      ;; Not yet in cl's plist
      (let ((frames (frame-list)))
        (while (and frames
                    (not (= (- (string-to-int
                                (or (frame-property
                                     (car frames) 'window-id) "0")) 3)
                            (X-Win-id (xwem-cl-xwin cl)))))
          (setq frames (cdr frames)))
;            (xtr (XQueryTree (xwem-dpy) (xwem-cl-xwin cl))))
;        (if (car xtr)
;            (setq xtr (mapcar 'X-Win-id (cdr (cdr (cdr (cdr xtr))))))
;          (setq xtr nil))
;        (while (and frames
;                    (not (member*
;                          (- (string-to-int
;                              (or (frame-property
;                                   (car frames) 'window-id) "0")) 2)
;                          xtr :test #'=)))
;          (setq frames (cdr frames)))

        (if (framep (car frames))
            (xwem-cl-put-sys-prop cl 'emacs-frame (car frames))
          (xwem-cl-put-sys-prop cl 'emacs-frame 'no-emacs-frame))
        (car frames)))))

(defun xwem-misc-find-cl-by-emacs-frame (frame)
  "Return xwem client that holds Emacs FRAME."
  (let ((fid (float (- (string-to-int
                        (or (frame-property frame 'window-id) "0"))
                       2)))
        (clients xwem-clients)
        wm-class xtr rcl)
    (while clients
      (setq wm-class (xwem-hints-wm-class (xwem-cl-hints (car clients))))
      (when (string= (cdr wm-class) x-emacs-application-class)
        (setq xtr (XQueryTree (xwem-dpy) (xwem-cl-xwin (car clients))))
        (if (car xtr)
            (setq xtr (mapcar 'X-Win-id (cdr (cdr (cdr (cdr xtr))))))
          (setq xtr nil))

        (when (find fid xtr :test '=)
          (setq rcl (car clients))
          (setq clients nil)))

      (setq clients (cdr clients)))
    rcl))

;;;###xwem-autoload
(defun xwem-misc-xwin-valid-p (xwin)
  "Return non-nil if XWIN is valid X window.
Do it in safe manner."
  (let (attrs)
    (X-Dpy-put-property (X-Win-dpy xwin) 'xwem-ignore-bad-win t)
    (xwem-unwind-protect
        (setq attrs (XGetWindowAttributes (X-Win-dpy xwin) xwin))
      (X-Dpy-put-property (X-Win-dpy xwin) 'xwem-ignore-bad-win nil))
    attrs))

(defun xwem-misc-xerr-hook (xdpy xev)
  "Display X errors in `xwem-minibuffer'.
Error hook must not performe any interaction with X server!
XDPY - X-Dpy.
XEV  - X-Event of error type."
  (let* ((err (X-Event-xerror-code xev))
         (badth (X-Event-xerror-resourceid xev))
         (seq (X-Event-seq xev))
         (maj (X-Event-xerror-maj-op xev))
         (opfun (cdr (assq maj xlib-opcodes-alist)))
         (min (X-Event-xerror-min-op xev))
         (bstr (cond ((= err 1) "Request")
                     ((= err 2) "Value")
                     ((= err 3) "Window")
                     ((= err 4) "Pixmap")
                     ((= err 5) "Atom")
                     ((= err 6) "Cursor")
                     ((= err 7) "Font")
                     ((= err 8) "Match")
                     ((= err 9) "Drawable")
                     ((= err 10) "Access")
                     ((= err 11) "Alloc")
                     ((= err 12) "Color")
                     ((= err 13) "GC")
                     ((= err 14) "IDChoice")
                     ((= err 15) "Name")
                     ((= err 16) "Length")
                     ((= err 17) "Implementation")
                     ((= err 128) "FirstExtension")
                     ((= err 255) "LastExtension")
                     (t "Unknown"))))
    (unless (and (X-Dpy-get-property xdpy 'xwem-ignore-bad-win)
                 (eq err 3))
      (xwem-message 'error "X error - Bad %s %f seq=%f:%d ops=%d:%d/%S"
                    bstr badth seq (X-Dpy-rseq-id (xwem-dpy)) maj min opfun)
    )))

(defun xwem-misc-init ()
  "Miscellaneous initializations."
  (pushnew 'xwem-misc-xerr-hook (X-Dpy-error-hooks (xwem-dpy)))
  (xwem-cursors-init)

  (xwem-message 'init "Initializing masking ...")
  ;; Depth 1 pixmap, gcs
  (setq xwem-misc-mask-pixmap
        (XCreatePixmap (xwem-dpy) (xwem-rootwin) 1 1 1)
        xwem-misc-mask-fgc
        (XCreateGC (xwem-dpy) xwem-misc-mask-pixmap
                   :foreground 1.0
                   :background 0.0)
        xwem-misc-mask-bgc
        (XCreateGC (xwem-dpy) xwem-misc-mask-pixmap
                   :foreground 0.0
                   :background 1.0))
  (xwem-message 'init "Initializing masking ... done"))

;;; Stuff for debugging
(defun xwem-misc-str2hexstr (str)
  "Convert STR to hexidecimal string representation."
  (substring (mapconcat #'(lambda (el) (format "%x " el)) str "") 0 -1))

;;; Messaging
(defun xwem-str-with-faces (str face-list)
  "Return STR with applied FACE-LIST."
  (let ((ext (make-extent 0 (length str) str)))

    (set-extent-property ext 'duplicable t)
    (set-extent-property ext 'unique t)
    (set-extent-property ext 'start-open t)
    (set-extent-property ext 'end-open t)
    (set-extent-property ext 'face face-list))
  str)

(defun xwem-format-faces (fmt def-face &rest args)
  "Accepts format FMT and ARGS in form `((arg . face) ...)'.
DEF-FACE is default face. Returns string with faces."
  (let ((flst (string-to-list fmt))
        (chr nil)
        (rstr ""))
    (while flst
      (setq chr (car flst))
      (cond ((= chr ?%)
             (setq flst (cdr flst))
             (setq chr (car flst))
             (let ((arg (if (consp (car args)) (caar args) (car args)))
                   (fcs (if (consp (car args)) (cdr (car args)) nil)))
               (cond ((= chr ?s)
                      (setq rstr (concat
                                  rstr
                                  (xwem-str-with-faces
                                   arg
                                   (if fcs
                                       (list fcs def-face)
                                     (list def-face)))))
                      (setq args (cdr args)))

                     ((= chr ?d)
                      (setq rstr (concat
                                  rstr
                                  (xwem-str-with-faces
                                   (int-to-string arg)
                                   (if fcs
                                       (list fcs def-face)
                                     (list def-face)))))
                      (setq args (cdr args)))
                     (t nil))))
            (t
             (setq rstr
                   (concat rstr
                           (xwem-str-with-faces
                            (char-to-string chr) (list def-face))))))
      (setq flst (cdr flst)))
    rstr))

(defun xwem-message-log (label message)
  "Log MESSAGE in `xwem-messages-buffer-name' buffer."
  (with-current-buffer (get-buffer-create xwem-messages-buffer-name)
    (let ((inhibit-read-only t))
      ;; Remove all messages from buffer if it exided maximum value
      (when (> (count-lines (point-min) (point-max))
               xwem-messages-buffer-lines)
        (delete-region (point-min) (point-max)))

      (goto-char (point-max))
      (insert (format-time-string "%D %T: "))
      (insert message)
      (insert "\n"))))

;;;###xwem-autoload
(defun xwem-clear-message (&optional label)
  "Clear xwem minibuffer's buffer."
  (clear-message label (xwem-minib-frame xwem-minibuffer)))

(defun xwem-message-insert (label msg &optional append-p)
  "Insert message MSG into xwem minibuffer's buffer."
  ;; Workaround XEmacs ''feature''.  When minibuffer is activated,
  ;; and someone uses echo are, XEmacs will wait 2 seconds, so we
  ;; will got lag! (see DEFUN `command-loop-1' in cmdloop.c) --lg
  ;;
  ;;   However in newer XEmacsen there is
  ;;   `minibuffer-echo-wait-function' variable, which controls
  ;;   behaviour. --lg
  (when (zerop (minibuffer-depth))
    ;; Activate minibuffer, if not ignoring this label
    (when (and xwem-minibuffer
               (xwem-minib-cl xwem-minibuffer))
      (xwem-activate (xwem-minib-cl xwem-minibuffer)))
    (add-to-list 'log-message-ignore-labels label) ; avoid logging in *Messages*
    (if append-p
        (append-message label msg
                        (and xwem-minibuffer
                             (xwem-minib-frame xwem-minibuffer)))
      (display-message label msg
                       (and xwem-minibuffer
                            (xwem-minib-frame xwem-minibuffer))))))

(defun xwem-message-label-prefix (label)
  "Return prefix string, according to LABEL.
Return nil if no prefix required for label."
  (let ((lp (assq label xwem-messages-label-prefixes)))
    (when lp
        (concat "XWEM" (if (and (stringp (cadr lp))
                                (not (zerop (length (cadr lp)))))
                           "-" "")
                (funcall 'xwem-str-with-faces (cadr lp) (caddr lp))
                ": "))))

(defun xwem-message-maybe-beep (label)
  "If LABEL is beepable, then beep."
  ;; Beep if needed
  (let ((snd (assq label xwem-messages-beeps-alist)))
    (when snd
      (xwem-play-sound (cdr snd)))))

(defun xwem-message-ignore-display-p (label msg)
  "Return non-nil if LABELed MSG should not be displayed."
  (or (memq label xwem-messages-ignore-display-labels)
      (memq t (mapcar (lambda (rx)
                        (when (string-match rx msg) t))
                      xwem-messages-ignore-display-regexps))))

(defun xwem-message-ignore-log-p (label msg)
  "Return non-nil if LABELed message MSG should not be logged."
  (or (memq label xwem-messages-ignore-log-labels)
      (memq t (mapcar (lambda (rx)
                        (when (string-match rx msg) t))
                      xwem-messages-ignore-log-regexps))))

(defun xwem-message-1 (label fmt append-p &rest args)
  (let* ((print-level 3)                ; XXX limit print level
         (msg (if (eq label 'asis) fmt (apply 'format fmt args)))
         (lp (xwem-message-label-prefix label)))
    (when lp
      (setq msg (concat lp msg)))
    (unless (xwem-message-ignore-log-p label msg)
      (xwem-message-log label msg))
    (xwem-message-maybe-beep label)
    (unless (xwem-message-ignore-display-p label msg)
      (xwem-message-insert label msg append-p))))

(defun xwem-message-append (label fmt &rest args)
  "Append message of LABEL type.
Message formatted using FTM and ARGS."
  (apply 'xwem-message-1 label fmt t args))

;;;###xwem-autoload
(defun xwem-message (label fmt &rest args)
  "Display xwem message of TYPE using FMT format."
  (apply 'xwem-message-1 label fmt nil args))

;;;###autoload(autoload 'xwem-show-message-log "xwem-misc" nil t)
(define-xwem-command xwem-show-message-log (arg)
  "Show `xwem-messages-buffer-name'.
If prefix ARG is given, than behaviour is undefined."
  (xwem-interactive "P")

  (let ((mbuf (get-buffer-create xwem-messages-buffer-name)))
    (xwem-special-popup-frame mbuf)
    (with-current-buffer mbuf
      (setq mode-name "XWEM-log")
      (local-set-key (kbd "q") 'delete-frame)
      (xwem-message 'msg "Press `q' to eliminate buffer.")
      )))

(defun xwem-list-to-string (list len)
  "Convert LIST of characterters to string with length LEN."
  (let ((rstr ""))
    (while (and list (> len 0))
      (setq rstr (concat rstr (string (car list))))
      (setq list (cdr list))
      (setq len (1- len)))
    rstr))

;;;; Misc commands.
;;;###autoload(autoload 'xwem-ignore-command "xwem-misc" nil t)
(define-xwem-command xwem-ignore-command (&rest args)
  "Generic ignore command."
  (xwem-interactive))

(defvar xwem-read-expression-history nil
  "*History of expressions evaled using `xwem-eval-expression'.")

;;;###autoload(autoload 'xwem-eval-expression "xwem-misc" nil t)
(define-xwem-command xwem-eval-expression (expr &optional arg)
  "Eval Lisp expression interactively.
When used with prefix ARG, then insert the result into selected client."
  (xwem-interactive
   (list
    (xwem-read-from-minibuffer (if xwem-prefix-arg
                                   "XWEM (insert) Eval: "
                                 "XWEM Eval: ")
                               nil read-expression-map
                               t 'xwem-read-expression-history)
    xwem-prefix-arg))

  (setq values (cons (eval expr) values))
  (if arg
      (xwem-kbd-add-pending-keys (prin1-to-string (car values)))
    (xwem-deferred-funcall
     'xwem-message 'info "%S => %S" expr (car values))))

;;;###autoload(autoload 'xwem-execute-extended-command "xwem-misc" nil t)
(define-xwem-command xwem-execute-extended-command (arg)
  "Execute Emacs command.
Prefix ARG is passed to extended command."
  (xwem-interactive "P")

  (with-xwem-read-from-minibuffer
   (let ((xwem-prefix-arg arg))
     (execute-extended-command arg))))

;;;###autoload(autoload 'xwem-shell-command "xwem-misc" nil t)
(define-xwem-command xwem-shell-command (command arg)
  "Execute shell command, just as `shell-command' do.
If prefix ARG is given insert result to current client.
If output of COMMAND fits to one string it is displayed in
`xwem-minibuffer', if not Emacs special frame will be poped up with
contents of COMMAND output.
If double prefix ARG \(i.e. \\<xwem-global-map>\\[xwem-universal-argument] \\<xwem-global-map>\\[xwem-universal-argument]\) supplied, then last
'\\n' character will be cuted in output to current client."
  (xwem-interactive (list (xwem-read-external-command
                           (if xwem-prefix-arg
                               (if (> (prefix-numeric-value xwem-prefix-arg) 4)
                                   "XWEM (insert-nonl) shell command: "
                                 "XWEM (insert) shell command: ")
                             "XWEM shell command: "))
                          xwem-prefix-arg))

  (with-temp-buffer
    (let ((status (call-process
                   shell-file-name nil (current-buffer) nil
                   shell-command-switch command)))
      (cond (arg
             (xwem-kbd-add-pending-keys
              (buffer-substring
               (point-min) (- (point-max)
                              (if (> (prefix-numeric-value arg) 4) 1 0)))))
            ((= (point-max) (point-min))
             (xwem-message 'info "Process exit status: %d" status))
            ((= 1 (count-lines (point-min) (point-max)))
             (xwem-message
              'info "%s" (buffer-substring (point-min) (point-max))))
            (t
             (let ((scb (current-buffer))
                   (nbuf (get-buffer-create
                          (generate-new-buffer-name
                           (format " *Shell: %s" command)))))
               (with-current-buffer nbuf
                 (insert-buffer scb)
                 (xwem-misc-view-mode))
               (xwem-special-popup-frame nbuf)))))))

;;;###autoload(autoload 'xwem-mini-calc "xwem-misc" nil t)
(define-xwem-command xwem-mini-calc (expr &optional arg)
  "Calculate expression EXPR.
If some region is active or cutbuffer is selected - calculate selected
region.
If prefix ARG is given, insert the result to current client.

BUGS: prefix ARG might not work if region is active or some cutbuffer
is selected."
  (xwem-interactive
   (list (or (xwem-selection 'local)
             (xwem-read-from-minibuffer (if xwem-prefix-arg
                                            "XWEM (insert) Calc: "
                                          "XWEM Calc: ")))
         xwem-prefix-arg))
  (zmacs-deactivate-region)             ; XXX
  (let ((result (calc-eval expr)))
    (if arg
        (xwem-kbd-add-pending-keys result)
      (xwem-message 'info "%s = %s" expr result))))

;;;###autoload(autoload 'xwem-misc-make-screenshot "xwem-misc" nil t)
(define-xwem-command xwem-misc-make-screenshot (file-name &optional arg)
  "Make screen screenshot and save it to file with NAME.
If used with prefix ARG - import screenshot of current client window.

With double ARG (H-u H-u) - import screenshot of current client with
frame included.

NOTE: `xwem-misc-make-screenshot' uses \"import\" utility from
ImageMagic package, which you can obtain at
http://imagemagick.sourceforge.net/."
  (xwem-interactive "FImport screen to file: \nP")

  (flet ((message (fmt &rest args) nil)) ; XXX shutup messaging
    (xwem-message 'info
                  (format "Importing screenshot to %s." file-name))
    (xwem-add-hook-post-deferring
     `(lambda ()
        (xwem-execute-program
         (format "import -window 0x%x %s %s"
                 ,(X-Win-id (if arg
                                (xwem-cl-xwin (xwem-cl-selected))
                              (xwem-rootwin)))
                 ,(if (equal arg '(16)) "-frame" "")
                 ,(expand-file-name file-name)))))))

;;;###autoload(autoload 'xwem-misc-pause "xwem-misc" nil t)
(define-xwem-command xwem-misc-pause (arg)
  "Pause for ARG decaseconds(0.1 sec).
This command is usefull, when recording keyboard macro, and there need
to wait for something, f.e. window mapping."
  (xwem-interactive "p")

  (add-timeout (* 0.1 arg) (lambda (&rest args) (exit-recursive-edit)) nil)
  (recursive-edit))

;;; Some useful operations on lists
(defun xwem-insert-after (list aft-el el)
  "In LIST after AFT-EL insert EL."
  (push el (cdr (member aft-el list)))
  list)

(defun xwem-insert-before (list bef-el el)
  "In LIST before BEF-EL insert EL."
  (nreverse (xwem-insert-after (nreverse list) bef-el el)))

(defun xwem-list-set-element (list old-el new-el)
  "In LIST set OLD-EL to NEW-EL."
  (setcar (memq old-el list) new-el)
  list)

;;;###xwem-autoload
(defun xwem-list-exchange-els (list el1 el2)
  "In LIST exchange places of EL1 and EL2."
  (when (and (memq el1 list)
             (memq el2 list)
             (not (eq el1 el2)))
    (xwem-list-set-element list el1 'this-fake-name1-should-not-be-in-list)
    (xwem-list-set-element list el2 el1)
    (xwem-list-set-element list 'this-fake-name1-should-not-be-in-list el2))
  list)

;;; Profiling support
;;;###autoload
(defun xwem-misc-start-profiling ()
  "Start profiling critical xlib/xwem functions."
  (interactive)

  (setq elp-function-list
        (or xwem-misc-functions-to-profile
            ;; Profile all X-Dpy-XXX functions
            (delq nil (mapcar #'(lambda (el)
                                  (when (and (symbolp el)
                                             (functionp el)
                                             (string-match "X-Dpy-"
                                                           (symbol-name el)))
                                    el))
                              obarray))))
  (elp-instrument-list))

;;;###autoload
(defun xwem-misc-profiling-results ()
  "Show xlib/xwem profiling results."
  (interactive)

  (elp-results))

;;;###xwem-autoload
(defun xwem-recursive-edit ()
  "Enter recursive edit."
  (recursive-edit))

;;;###xwem-autoload
(defun xwem-exit-recursive-edit ()
  "Exit from recursive edit."
  (if (> (recursion-depth) 0)
      (throw 'exit nil))
  (xwem-message 'warning "No recursive edit is in progress"))


;;; Text Specifications operations

;; TextSpec is list of vectors:
;; - vectors elements is cons cells in form (face . "text")
;; - each vector specifies line
;; - empty vector specifies newline

(defun xwem-misc-line->linesp (default-face)
  "Convert current line in selected buffer to element of text spec - line spec.
DEFAULT-FACE is the default face."
  (let (tsp cpnt npnt face str)
    (save-excursion
      (narrow-to-region (point-at-bol) (point-at-eol))
      (goto-char (point-at-bol))
      (while (not (eolp))
        (setq cpnt (point)
              npnt (or (next-single-property-change cpnt 'face) (point-at-eol))
              face (or (get-char-property cpnt 'face) default-face)
              str (buffer-substring cpnt npnt))
        (when (consp face)
          (setq face (car face)))       ; XXX need face merging

        ;; XXX Untabify
        (setq str (replace-in-string str "\t" (make-string tab-width ?\x20)))

        (setq tsp (cons (cons face str) tsp))
        (goto-char npnt))
      (widen))
    (vconcat (nreverse (or tsp (list (cons default-face "")))))))

(defun xwem-misc-buffer->textsp (default-face &optional buffer start end)
  "Convert BUFFER to text specification.
DEFAULT-FACE is the default face.
If BUFFER is omitted, selected buffer assumed."
  (let (rlst)
    (save-excursion
      (when buffer
        (set-buffer buffer))

      (goto-char (or start (point-min)))
      (while (and (not (eobp))
                  (< (point) (or end (point-max))))
        (setq rlst (cons (xwem-misc-line->linesp default-face) rlst))
        (forward-line 1))
      )
    (nreverse rlst)))

(defun xwem-misc-linesp-width (linesp)
  "Return width of line spec LINESP."
  (apply '+ (mapcar #'(lambda (el)
                        (X-Text-width
                         (xwem-dpy) (X-Gc-font (xwem-face-get-gc (car el)))
                         (cdr el)))
                    linesp)))

(defun xwem-misc-linesp-height (linesp)
  "Return height of line spec LINESP."
  (apply 'max (mapcar #'(lambda (el)
                          (X-Text-height
                           (xwem-dpy) (X-Gc-font (xwem-face-get-gc (car el)))
                           (cdr el)))
                      linesp)))

(defun xwem-misc-linesp-show (d x y linesp &optional type default-background)
  "In X drawable D at X and Y coordinates show line spec LINESP.
TYPE is one of XImageString or XDrawString, default is XImageString."
  (let ((cxoff 0))
    (mapc #'(lambda (el)
              (funcall (cond ((and (eq type 'XDrawString)
                                   (stringp default-background)
                                   (not (string= default-background
                                                 (face-background-name (car el)))))
                              'XImageString)
                             ((not (null type)) type)
                             (t 'XImageString))
                       (X-Drawable-dpy d) d
                       (xwem-face-get-gc (car el))
                       (+ x cxoff) y (cdr el))
              (setq cxoff (+ cxoff (X-Text-width
                                    (X-Drawable-dpy d)
                                    (X-Gc-font (xwem-face-get-gc (car el)))
                                    (cdr el)))))
          linesp)))

(defun xwem-misc-textsp-show (xwin x y textsp &optional type default-background)
  "In x window XWIN at X and Y coordinates show text spec TEXTSP.
TYPE is one of XImageString or XDrawString, default is XImageString.
If TYPE is XDrawString and DEFAULT-BACKGROUND is specifed, characters
that have different than DEFAULT-BACKGROUND baground color are drawed
using XImageString."
  (let ((yoff 0))
    (mapc #'(lambda (el)
              (xwem-misc-linesp-show xwin x (+ y yoff) el type default-background)
              (setq yoff (+ yoff (xwem-misc-linesp-height el))))
          textsp)
    ))

;;; Outlining
(define-xwem-face xwem-misc-outline-face1
  `((t (:foreground "white" :background "black"
        :function X-GXXor :subwindow-mode X-IncludeInferiors
        :line-width 4)))
  "Face used to outline something."
  :group 'xwem-misc
  :group 'xwem-faces)

(define-xwem-face xwem-misc-outline-face2
  `((t (:foreground "white" :background "black"
        :function X-GXXor :subwindow-mode X-IncludeInferiors
        :line-width 2)))
  "Face used to outline something."
  :group 'xwem-misc
  :group 'xwem-faces)

(defun xwem-misc-outline (xrect how &optional xwin)
  "Outline XRECT using HOW method in XWIN.
Valid HOW is 'normal, ...
If XWIN is not specified, X root window is used."
  (unless xwin
    (setq xwin (xwem-rootwin)))

  (let ((x (X-Rect-x xrect))
        (y (X-Rect-y xrect))
        (w (X-Rect-width xrect))
        (h (X-Rect-height xrect)))
    (cond ((eq how 'normal)
           (XDrawRectangles
            (xwem-dpy) xwin (xwem-face-get-gc 'xwem-misc-outline-face1)
            (list xrect)))

          ((eq how 'contiguous)
           (xwem-misc-outline xrect 'normal)
           (XDrawSegments
            (xwem-dpy) xwin (xwem-face-get-gc 'xwem-misc-outline-face2)
            (list (cons (cons x 0)
                        (cons x (X-Geom-height (xwem-rootgeom))))
                  (cons (cons (+ x w) 0)
                        (cons (+ x w) (X-Geom-height (xwem-rootgeom))))
                  (cons (cons 0 y)
                        (cons (X-Geom-width (xwem-rootgeom)) y))
                  (cons (cons 0 (+ y h))
                        (cons (X-Geom-width (xwem-rootgeom)) (+ y h)))
                  )))

          ((eq how 'corners)
           (let* ((cornw (/ w 8))
                  (cornh (/ h 8))
                  (crw (/ (+ cornh cornw) 2)))
             (XDrawSegments
              (xwem-dpy) xwin (xwem-face-get-gc 'xwem-misc-outline-face1)
              (list
               ;; Top left
               (cons (cons x y) (cons (+ x cornw) y))
               (cons (cons x y) (cons x (+ y cornh)))

               ;; Top right
               (cons (cons (+ x w) y) (cons (+ x w (- cornw)) y))
               (cons (cons (+ x w) y) (cons (+ x w) (+ y cornh)))

               ;; Bottom left
               (cons (cons x (+ y h)) (cons (+ x cornw) (+ y h)))
               (cons (cons x (+ y h)) (cons x (+ y h (- cornh))))

               ;; Bottom right
               (cons (cons (+ x w) (+ y h)) (cons (+ x w (- cornw)) (+ y h)))
               (cons (cons (+ x w) (+ y h)) (cons (+ x w) (+ y h (- cornh))))

               ;; Crosshair
               (cons (cons (+ x (/ (- w crw) 2)) (+ y (/ h 2)))
                     (cons (+ x (/ (+ w crw) 2)) (+ y (/ h 2))))
               (cons (cons (+ x (/ w 2)) (+ y (/ (- h crw) 2)))
                     (cons (+ x (/ w 2)) (+ y (/ (+ h crw) 2))))
               ))))

          ((eq how 'grid)
           (xwem-misc-outline xrect 'normal)
           (XDrawSegments
            (xwem-dpy) xwin (xwem-face-get-gc 'xwem-misc-outline-face2)
            (nconc (funcall
                    #'(lambda ()
                        (let ((off 0) rl)
                          (while (< off (+ x w))
                            (when (> off x)
                              (setq rl (cons (cons (cons off y)
                                                   (cons off (+ y h)))
                                             rl)))
                            (setq off (+ off 64)))
                          rl)))
                   (funcall #'(lambda ()
                                (let ((off 0)
                                      rl)
                                  (while (< off (+ y h))
                                    (when (> off y)
                                      (setq rl (cons (cons (cons x off)
                                                           (cons (+ x w) off))
                                                     rl)))
                                    (setq off (+ off 64)))
                                  rl)))))
           )

          ;; TODO: add others
          )))

(defun xwem-misc-move-outline (rect1 rect2 &optional steps)
  "Move RECT1 to RECT2 by STEPS redraws.
Somekind of animation.  XEmacs will block while moving.
Default STEPS is 40."
  (unless steps
    (setq steps 40))

  (xwem-misc-outline rect1 'normal)
  (let ((crect rect1) tcrect
        factor x-step y-step w-step h-step
        done)

    ;; Setup steps
    (setq x-step (/ (- (X-Rect-x rect2) (X-Rect-x rect1))
                    steps)
          y-step (/ (- (X-Rect-y rect2) (X-Rect-y rect1))
                    steps)
          w-step (/ (- (X-Rect-width rect2) (X-Rect-width rect1))
                    steps)
          h-step (/ (- (X-Rect-height rect2) (X-Rect-height rect1))
                    steps)
          factor (max x-step y-step w-step h-step))

    (while (not done)
      ;; Remember CRECT before modification
      (setq tcrect (copy-X-Rect crect))

      ;; Transforme CRECT
      (incf (X-Rect-x crect) x-step)
      (incf (X-Rect-width crect) w-step)
      (incf (X-Rect-y crect) y-step)
      (incf (X-Rect-height crect) h-step)

      ;; break condition
      (when (and (<= (abs (- (X-Rect-width crect)
                             (X-Rect-width rect2))) factor))
        (setq done t))

      ;; Outline CRECT
      (xwem-misc-outline tcrect 'normal)
      (xwem-misc-outline crect 'normal))
    (xwem-misc-outline crect 'normal)))

;;;###xwem-autoload
(defun xwem-misc-xwin-background-mode (xwin x y &optional width height)
  "Return XWIN's background mode in rectange WIDTHxHEIGHT+X+y.
Background mode is one of `light' or `dark'."
  (condition-case nil
      (progn
        (let ((xgeom (XGetGeometry (xwem-dpy) xwin)))
          ;; Adjust x/y/width/height
          (when (< x 0)
            (setq x 0))
          (when (< y 0)
            (setq y 0))
          (when (and width (< width 0))
            (setq width nil))
          (when (and height (< height 0))
            (setq height nil))

          (unless width
            (setq width 10))
          (unless height
            (setq height 10))
          (when (> width (X-Geom-width xgeom))
            (setq width (X-Geom-width xgeom)))
          (when (> height (X-Geom-height xgeom))
            (setq height (X-Geom-height xgeom)))
          (when (> x (X-Geom-width xgeom))
            (setq x (- (X-Geom-width xgeom) width)))
          (when (> y (X-Geom-height xgeom))
            (setq y (- (X-Geom-height xgeom) height))))

        (let* ((ximg (XImageGet (xwem-dpy) xwin x y width height))
               (cv (nth 3 (XQueryColors
                           (xwem-dpy) (XDefaultColormap (xwem-dpy))
                           (mapcan 'identity
                                   (X-Image-get-prop ximg 'px-layout)))))
               (vv (/ (apply '+ (apply 'nconc cv)) (length cv))))

          (XDestroyImage ximg)
          (if (< vv xwem-background-mode-bound)
              'dark 'light)))
    ;; XXX maybe introduce `xwem-background-mode-default' ?
    (t 'light)))

;;;###xwem-autoload
(defun xwem-misc-completing-read-using-iswitchb
  (prompt table &optional predicate require-match)
  "Read a string in the xwem minibuffer using iswitchb package.
PROMPT is a string to prompt with.
TABLE is a list of strings to select.
PREDICATE is a function that limits completion to a subset of TABLE."
  (require 'iswitchb)

  (flet ((iswitchb-make-buflist
           (default)
           (setq iswitchb-buflist table)))
    (iswitchb-read-buffer prompt)))

;;;###xwem-autoload
(defun xwem-misc-xbutton-cl (xev)
  "Return client where button event XEV occured."
  (or (xwem-xwin-cl (X-Event-xbutton-event xev))
      (and (X-Win-p (X-Event-xbutton-child xwem-last-xevent))
           (xwem-xwin-cl (X-Event-xbutton-child xwem-last-xevent)))))


(defun xwem-misc-iresize-or-imove (xwin xevent function argument)
  "Interactively resize of move window XWIN according to X event XEVENT.
FUNCTION called each time with three
arguments - ARGUMENT, NEW-X, NEW-Y."
  (let* ((nh (XGetWMNormalHints (xwem-dpy) xwin))
         (min-pnt (or (and (X-WMSize-pminsize-p nh)
                           (cons (X-WMSize-min-width nh)
                                 (X-WMSize-min-height nh)))
                      (cons 0 0)))
         (base-pnt (or (and (X-WMSize-pbasesize-p nh)
                            (cons (X-WMSize-base-width nh)
                                  (X-WMSize-base-height nh)))
                       min-pnt))
         (step-pnt (or (and (X-WMSize-presizeinc-p nh)
                            (cons (X-WMSize-width-inc nh)
                                  (X-WMSize-height-inc nh)))
                       (cons 1 1)))

         (srx (X-Event-xbutton-root-x xevent))
         (sry (X-Event-xbutton-root-y xevent))

         (done nil)
         need-call xev
         last-x last-y new-x new-y)

    (xwem-mouse-grab xwem-cursor-move xwin
                     (Xmask-or XM-ButtonRelease XM-ButtonMotion))
    (xwem-unwind-protect
        (while (not done)
          (setq need-call nil)
          (X-Event-CASE (setq xev (xwem-next-event))
            (:X-ButtonRelease (setq done t))

            (:X-MotionNotify
             ;; Update curr-xrect
             (setq new-x (X-Event-xmotion-root-x xev)
                   new-y (X-Event-xmotion-root-y xev))

             (when (zerop (% (- (- new-x srx)
                                (car base-pnt))
                             (car step-pnt)))
               ;; Can resize
               (setq last-x (- new-x srx)
                     need-call t))

             (when (zerop (% (- (- new-y sry)
                                (cdr base-pnt))
                             (cdr step-pnt)))
               (setq last-y (- new-y sry)
                     need-call t))

             ;; Call function
             (when need-call
               (funcall function argument last-x last-y)))))
      (xwem-mouse-ungrab))))

;;;###xwem-autoload
(defun xwem-misc-fixup-string (str &optional max-width)
  "Fixup STR to be no-more than MAX-WIDTH chars."
  (if (and max-width (> (length str) max-width))
      (substring str 0 max-width)
    str))

;;;###xwem-autoload
(defun xwem-misc-merge-plists (plist1 &rest merg-plists)
  "Merge plist entries in MERG-PLISTS to PLIST1 and return resulting plist."
  (flet ((mrg-list (l1 ml)
           (if (null ml)
               l1
             (mrg-list (plist-put l1 (car ml) (cadr ml))
                       (cddr ml)))))
    (reduce #'mrg-list (cons (copy-list plist1) merg-plists))))

;;; Image rotator
(defun xwem-misc-rotidx-left (i w h depth)
  (setq i (/ i depth))
  (let* ((y (/ i w))
         (x (% i w))
         (x1 y)
         (y1 (- w x 1)))
    (* (+ (* y1 h) x1) depth)))

(defun xwem-misc-rotidx-right (i w h depth)
  (setq i (/ i depth))
  (let* ((y (/ i w))
         (x (% i w))
         (x1 (- h y 1))
         (y1 x))
    (* (+ (* y1 h) x1) depth)))

;;;###xwem-autoload
(defun xwem-misc-rotate-data (data w h depth &optional rotate)
  "Rotate DATA obtained from XGetImage for use by XPutImage."
  (unless (member depth '(8 16 24 32))
    (error 'xwem-error (format "Unsupported depth %d, use one of (8 16 24 32)" depth)))
  (setq depth (truncate (/ depth 8)))

  (unless rotate
    (setq rotate 'left))
  (setq rotate
        (if (eq rotate 'left)
            'xwem-misc-rotidx-left
          'xwem-misc-rotidx-right))

  (let* ((gc-cons-threshold most-positive-fixnum) ; inhibit GCing
        (dstr (make-string (* w h depth) ?\x00))
        (dlen (length data))
        (i 0)
        off j)
    (while (< i dlen)
      (setq off (funcall rotate i w h depth))
      (setq j 0)
      (while (< j depth)
        (aset dstr (+ off j) (aref data (+ i j)))
        (incf j))
      (setq i (incf i depth)))
    dstr))

;;;###xwem-autoload
(defun xwem-debug (routine fmt &rest fmt-args)
  (let ((print-level 3))                ; XXX Restrict huge output
    (apply 'X-Dpy-log (xwem-dpy) routine fmt fmt-args)))


;;; Raise/lower stuff
(defvar xwem-misc-always-on-top-stack nil
  "List of always-on-top windows.")

(defmacro xwem-xwin-rank (xwin)
  `(X-Win-get-prop ,xwin 'always-on-top-rank))
(defsetf xwem-xwin-rank (xwin) (rank)
  `(if (numberp ,rank)
       (xwem-misc-set-xwin-always-on-top ,xwin ,rank)
     (xwem-misc-unset-always-on-top ,xwin)))

;;;###xwem-autoload
(defun xwem-misc-set-xwin-always-on-top (xwin rank)
  "Mark xwin as always on top window.
RANK number denotes rank of always on top window.  Higher RANK mean
XWIN is above windows with lower RANK."
  (unless (numberp rank)
    (error 'invalid-argument "RANK must be number" rank))

  (X-Win-put-prop xwin 'always-on-top-rank rank)

  ;; Sort `xwem-misc-always-on-top-stack' by rank after adding XWIN,
  ;; higher rank are at the end.
  (pushnew xwin xwem-misc-always-on-top-stack :test 'X-Win-equal)
  (setq xwem-misc-always-on-top-stack
        (sort xwem-misc-always-on-top-stack
              #'(lambda (xwin1 xwin2)
                  (< (xwem-xwin-rank xwin1)
                     (xwem-xwin-rank xwin2)))))

  ;; Finnaly apply RANK to life
  (xwem-misc-raise-xwin xwin))

;;;###xwem-autoload
(defun xwem-misc-unset-always-on-top (xwin)
  "Unmark XWIN as always on top window."
  (X-Win-rem-prop xwin 'always-on-top-rank)
  (setq xwem-misc-always-on-top-stack
        (delq xwin xwem-misc-always-on-top-stack)))

(defun xwem-misc-find-below-sibling (operation rank)
  "Select appropriate below sibling from `xwem-misc-always-on-top-stack'."
  (let ((sibs xwem-misc-always-on-top-stack)
        (rsib nil))
    (while sibs
      (unless (X-Win-p (car sibs))
        ;; Remove broken sibling
        (setq xwem-misc-always-on-top-stack
              (delq (car sibs) xwem-misc-always-on-top-stack)
              sibs (cdr sibs)))
      (when (funcall operation (xwem-xwin-rank (car sibs)) rank)
        (setq rsib (car sibs)
              sibs nil))
      (setq sibs (cdr sibs)))
    rsib))

;;;###xwem-autoload
(defun xwem-misc-raise-xwin (xwin)
  "Raise XWIN reguarding always on top windows."
  (let* ((rank (or (xwem-xwin-rank xwin) 0))
         (bsib (and rank (xwem-misc-find-below-sibling '> rank))))
    (cond ((not bsib)
           (when (memq xwin xwem-misc-always-on-top-stack)
             (setq xwem-misc-always-on-top-stack
                   (nconc (delq xwin xwem-misc-always-on-top-stack)
                          (list xwin))))
           (XRaiseWindow (xwem-dpy) xwin))
          ((not (X-Win-equal xwin bsib))
           ;; Adjust rank stack in case if ranks are equal
           (when (memq xwin xwem-misc-always-on-top-stack)
             (setq xwem-misc-always-on-top-stack
                   (xwem-insert-before
                    (delq xwin xwem-misc-always-on-top-stack)
                    bsib xwin)))
           (XConfigureWindow (xwem-dpy) xwin
                             :stackmode X-Below
                             :sibling bsib)))))

;;;###xwem-autoload
(defun xwem-misc-lower-xwin (xwin)
  "Lower XWIN according to its always on top rank."
  (let* ((rank (xwem-xwin-rank xwin))
         (bsib (and rank (xwem-misc-find-below-sibling '>= rank))))
    (cond ((not bsib)
           (when (memq xwin xwem-misc-always-on-top-stack)
             (setq xwem-misc-always-on-top-stack
                   (cons xwin (delq xwin xwem-misc-always-on-top-stack))))
           (XLowerWindow (xwem-dpy) xwin))
          ((not (X-Win-equal xwin bsib))
           ;; Adjust rank stack in case if ranks are equal
           (when (and (memq xwin xwem-misc-always-on-top-stack)
                      (= rank (xwem-xwin-rank bsib)))
             (setq xwem-misc-always-on-top-stack
                   (xwem-insert-before
                    (delq xwin xwem-misc-always-on-top-stack)
                    bsib xwin)))
           (XConfigureWindow (xwem-dpy) xwin
                             :stackmode X-Below
                             :sibling bsib)))))

;; Debug
(defvar xwem-debug)

;;;###autoload
(defun xwem-turn-on-debug ()
  "Turn on xwem debugging."
  (interactive)

  (setq xwem-debug t)
  (when (X-Dpy-p (xwem-dpy))
    (setf (X:Dpy-log-buffer (xwem-dpy)) "*xwem-debug*")
    (X-Dpy-set-log-routines (xwem-dpy) xwem-debug-routines)))

;;;###autoload
(defun xwem-turn-off-debug ()
  "Turn off xwem debugging."
  (interactive)

  (setq xwem-debug nil)
  (when (X-Dpy-p (xwem-dpy))
    (setf (X:Dpy-log-buffer (xwem-dpy)) nil)))

;;;###xwem-autoload
(defun xwem-misc-flash-rectangle (x y width height &optional xwin pause)
  "Flash root window or XWIN."
  (unless xwin
    (setq xwin (xwem-rootwin)))
  (unless pause
    (setq pause 0.1))

  (let* ((gc-cons-threshold most-positive-fixnum) ; inhibit gcing
         (dpy (xwem-dpy))
         (gc (XCreateGC dpy xwin
                        :function X-GXInvert
                        :subwindow-mode X-IncludeInferiors)))
    (XGrabServer dpy)
    (xwem-unwind-protect
        (progn
          (XFillRectangle dpy xwin gc x y width height)
          (XFlush dpy)
          ;; XXX we need sleeping, so flashing will be visible
          (sleep-for pause)
          (XFillRectangle dpy xwin gc x y width height))
      (XUngrabServer dpy)
      (XFlush dpy)
      (XFreeGC dpy gc))))

;;;###xwem-autoload
(defun xwem-misc-view-mode (&optional majorp)
  "Enable view mode."
  (funcall (if majorp #'view-major-mode #'view-mode)
           nil #'kill-buffer))

(defun xwem-misc-font-coding-system (font)
  "Return coding system for given FONT."
  (and (string-match xwem-misc-font-x-registry-and-encoding-regexp font)
       (intern (downcase (format "%s-%s" (match-string 1 font)
                                 (match-string 2 font))))))

(defun xwem-misc-locale-coding-system ()
  "Return locale coding system."
  (intern (downcase (nth 1 (split-string (getenv "LANG") "\\.")))))


(provide 'xwem-misc)

;;; On-load actions
(defadvice disabled-command-hook (around xwem-command activate)
  (if (or (xwem-command-p xwem-this-command)
          (xwem-command-p this-command))
      (let ((tframe (xwem-special-popup-frame (get-buffer-create "*Help*"))))
        (xwem-kbd-stop-command-keys-echoing)
        (xwem-under-minibuffer
         (let ((this-command (if (xwem-command-p xwem-this-command)
                                 xwem-this-command
                               this-command)))
           ad-do-it)
         (delete-frame tframe)))
    ad-do-it))

;;; xwem-misc.el ends here
