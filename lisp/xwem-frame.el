;;; xwem-frame.el -- Frames ops for XWEM.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;; Created: 21 Mar 2003
;; Keywords: xlib, xwem
;; Time-stamp: <2/12/2008 13:22:38 lg@h1>

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

;;; Commentary

;; This file contain operations on XWEM frames.
;;

;;; Code

(require 'xlib-xlib)
(require 'xlib-xinerama)

(require 'xwem-load)
(require 'xwem-misc)

;;; Variables
(defgroup xwem-frame nil
  "Group to customize xwem frames."
  :prefix "xwem-frame-"
  :prefix "xwem-face-"
  :group 'xwem)

(defcustom xwem-frame-cursor-shape '(X-XC-top_left_arrow)
  "*Cursors shape which will be used when pointer is over xwem frame."
  :type (xwem-cursor-shape-choice)
  :set (xwem-cus-set-cursor-shape xwem-frame-cursor
         (mapcar 'xwem-frame-xwin (xwem-frames-list)))
  :initialize 'custom-initialize-default
  :group 'xwem-frame)

(defcustom xwem-frame-cursor-foreground-color "#111111"
  "*Cursor's foreground color used when pointer is on xwem's frame."
  :type 'color
  :set (xwem-cus-set-cursor-foreground xwem-frame-cursor)
  :initialize 'custom-initialize-default
  :group 'xwem-frame)

(defcustom xwem-frame-cursor-background-color "#EEEEEE"
  "*Cursor's background color used when pointer is on xwem's frame."
  :type 'color
  :set (xwem-cus-set-cursor-background xwem-frame-cursor)
  :initialize 'custom-initialize-default
  :group 'xwem-frame)

(defconst xwem-frame-builtin-properties
  '(inner-border-width outer-border-width title-height title-thickness)
  "List of valid builtin frame properties.")

;;;###autoload
(defcustom xwem-frame-default-properties
  (list 'inner-border-width 3           ; Internal border of xwem's frame
        'inner-border-thickness 1       ; Internal border thickness
        'outer-border-width 0           ; X border
        'background "gray60"            ; background color
        'title-height 18)
  "*Default properties list for xwem frames."
  :type '(restricted-sexp :match-alternatives (valid-plist-p))
  :group 'xwem-frame)

;;;###autoload
(defcustom xwem-embedded-frame-default-properties
  (list 'inner-border-width 0
        'inner-border-thickness 0
        'outer-border-width 0)
  "*Default properties for embedded xwem frames.
Values in `xwem-embedded-frame-default-properties' overrides values in
`xwem-frame-default-properties'."
  :type '(restricted-sexp :match-alternatives (valid-plist-p))
  :group 'xwem-frame)

;;;###autoload
(defcustom xwem-dedicated-frame-default-properties
  (list 'inner-border-width 2
        'inner-border-thickness 1
        'outer-border-width 0
        'manual-position t)
  "*Default properties for dedicated xwem frames.
Values in `xwem-dedicated-frame-default-properties' overrides values in
`xwem-frame-default-properties'."
  :type '(restricted-sexp :match-alternatives (valid-plist-p))
  :group 'xwem-frame)

(defcustom xwem-frame-on-delim-resize-mode 'normal
  "*Mode to outline current window when doing `xwem-frame-on-delim-resize'."
  :type '(choice (const :tag "Normal border" normal)
                 (const :tag "Opaque" opaque)
                 )
  :group 'xwem-frame)

(defcustom xwem-frame-iresize-mode 'normal
  "*Default type of drawing outlines when resizing frame interactively.
Opaque mode can do unexpected things, such as eat your food from
refrigerator, so set it to Opaque on your own risk."
  :type '(choice (const :tag "Normal border" normal)
                 (const :tag "Contiguous borders" contiguous)
                 (const :tag "Outline Corners" corners)
                 (const :tag "Grid" grid)
                 (const :tag "Opaque" opaque)
                 )
  :group 'xwem-frame)

(defcustom xwem-frame-imove-mode 'normal
  "*Default type of drawing outlines when moving frame interactively."
  :type '(choice (const :tag "Normal border" normal)
                 (const :tag "Contiguous border" contiguous)
                 (const :tag "Outline Corners" corners)
                 (const :tag "Grid" grid)
                 (const :tag "Opaque" opaque)
                 )
  :group 'xwem-frame)

(defcustom xwem-frame-imoveresize-mode-function
  'xwem-frame-imoveresize-mode-function-default
  "Function to call in way to select move or resize mode.
It is passed with two arguments - FRAME and WHAT.
Where FRAME is frame which is about to move/resize and WHAT is one of
'resize or 'move.
It should return one of:
 'normal     - Normal resize/move mode, just outline frame rectangle.
 'contiguous - Butified 'normal mode.
 'corners    - Outline frame corners.
 'grid       - Outline frame and draw grid inside.
 'opaque     - Opaque move/resize mode."
  :type 'function
  :group 'xwem-frame)

(defcustom xwem-frame-imoveresize-use-minibuffer t
  "*If non-nil, frame's geometry will be show while imove/iresize."
  :type 'boolean
  :group 'xwem-frame)

(defun xwem-frame-imoveresize-mode-function-default (frame what)
  "Default value of `xwem-frame-imoveresize-mode-function'.
Return `xwem-frame-iresize-mode' if WHAT is 'resize.
Return `xwem-frame-imove-mode' if WHAT is 'move."
  (if (eq what 'resize)
      xwem-frame-iresize-mode
    xwem-frame-imove-mode))

(defcustom xwem-frame-rolling-switch t
  "*Non-nil mean that \\<xwem-global-map>\\[xwem-frame-next] and
\\<xwem-global-map>\\[xwem-frame-previous] will always switch, even if
there no next or previous frame."
  :type 'boolean
  :group 'xwem-frame)

(defcustom xwem-frame-autoiconify-mode nil
  "Non-nil mean frame possible automatically iconifies when switching.
If switching from OFR to NFR frame values mean next:
 nil       - No frame will be iconified.
 intersect - OFR will be iconified if it intersects with NFR.
 always    - OFR always iconfied."
  :type '(choice (const :tag "Disabled" nil)
                 (conts :tag "Intersect" intersect)
                 (const :tag "Always" always))
  :group 'xwem-frame)

(defcustom xwem-frame-autoselect-embedded t
  "*Non-nil mean if embedded frame selected as client, also select frame."
  :type 'boolean
  :group 'xwem-frame)

(defcustom xwem-frame-keep-number nil
  "*Non-nil mean frames keeps their numbers when intermediate frame destroyed."
  :type 'boolean
  :group 'xwem-frame)

;; Hooks
(defcustom xwem-frame-select-hook nil
  "*Hooks to call when new frame just selected."
  :type 'hook
  :group 'xwem-frame)

(defcustom xwem-frame-deselect-hook nil
  "*Hooks to call when selected frame is about to be deselected."
  :type 'hook
  :group 'xwem-frame)

(defcustom xwem-frame-creation-hook nil
  "Hooks called with one argument - frame, when frame just created."
  :type 'hook
  :group 'xwem-hooks)

(defcustom xwem-frame-change-hook nil
  "Hooks called with one argument - frame, when frame changed."
  :type 'hook
  :group 'xwem-hooks)

(defcustom xwem-frame-destroy-hook nil
  "Hooks called with one argument - frame, when frame destroyed."
  :type 'hook
  :group 'xwem-hooks)

(defcustom xwem-frame-resize-hook nil
  "Hooks called with one argument - frame, when frame resized."
  :type 'hook
  :group 'xwem-hooks)

(defcustom xwem-frame-move-hook nil
  "Hooks called with one argument - frame, when frame moved."
  :type 'hook
  :group 'xwem-hooks)

(defcustom xwem-frame-redraw-hook nil
  "Hooks called with one argument - frame, when frame redrawed."
  :type 'hook
  :group 'xwem-hooks)

(defcustom xwem-frame-configuration-exporting t
  "*Non-nil mean, frame configuration exports after each frame command.
Non-nil value is useful when using xwem-agent.  It allows you restore
frames on (S)XEmacs restart."
  :type 'boolean
  :group 'xwem-frame)

(defcustom xwem-frame-fast-switch-to-linkage t
  "*Non-nil to switch to frame linkage instead of frame."
  :type 'boolean
  :group 'xwem-frame)

;;; Internal variables

(defconst xwem-frame-ev-mask
  (Xmask-or XM-Exposure
            XM-StructureNotify
            XM-SubstructureRedirect
            XM-SubstructureNotify
            XM-KeyPress XM-ButtonPress XM-ButtonRelease
            XM-ResizeRedirect)
  "Events mask for xwem's frame.")

(defvar xwem-frame-cursor nil
  "Cursor used for xwem frame.")

(defvar xwem-frame-types
  '(desktop embedded embedded-desktop dedicated)
  "List of xwem frame types.")

;;;###xwem-autoload
(defvar xwem-frames-list nil
  "List of all xwem frames.")

(defvar xwem-frame-noredisplay nil
  "If this non-nil than frame it reffers should not be redisplayed.
Internal variable, do not use.")

(defvar xwem-frame-dumped-config nil)


;;;###xwem-autoload
(defun xwem-frames-list (&optional type)
  "Return list of xwem frames of TYPE.
If TYPE is ommited - list of frames of any type is returned."
  (remove* nil xwem-frames-list
           :test #'(lambda (f1 f2)
                     (not (and (xwem-frame-p f2)
                               (not (eq (xwem-frame-state f2) 'destroyed))
                               (or (null type)
                                   (eq (xwem-frame-type f2) type)))))))

;;; Frame macros
(defmacro xwem-frame-link-insert-after (frame1 frame2)
  "Make FRAME2 to be after FRAME1."
  `(let ((nf (xwem-frame-link-next ,frame1)))
     (when (xwem-frame-p nf)
       (setf (xwem-frame-link-prev nf) ,frame2))
     (setf (xwem-frame-link-next ,frame1) ,frame2)
     (setf (xwem-frame-link-prev ,frame2) ,frame1)
     (setf (xwem-frame-link-next ,frame2) nf)))

(defmacro xwem-frame-link-insert-before (frame1 frame2)
  "Make FRAME2 to be before FRAME1."
  `(let ((pf (xwem-frame-link-prev ,frame1)))
     (when (xwem-frame-p pf)
       (setf (xwem-frame-link-next pf) ,frame2))
     (setf (xwem-frame-link-prev ,frame1) ,frame2)
     (setf (xwem-frame-link-next ,frame2) ,frame1)
     (setf (xwem-frame-link-prev ,frame2) pf)))

(defmacro xwem-frame-link-remove (frame)
  "Remove FRAME from linkage."
  `(let ((nfr (xwem-frame-link-next ,frame))
         (pfr (xwem-frame-link-prev ,frame)))
     (when (xwem-frame-p pfr)
       (setf (xwem-frame-link-next pfr) nfr))
     (when (xwem-frame-p nfr)
       (setf (xwem-frame-link-prev nfr) pfr))))

(defmacro xwem-frame-link-head (frame)
  "Returns head frame of FRAME's linkage."
  `(let ((fr ,frame))
     (while (xwem-frame-p (xwem-frame-link-prev fr))
       (setq fr (xwem-frame-link-prev fr)))
     fr))

(defmacro xwem-frame-linkage-map (frame fn)
  "Call FN for each frame in FRAME's linkage.
FN called with one argument - frame."
  ;; TODO: avoid infinit recursion
  `(let ((fr (xwem-frame-link-head ,frame)))

     (while (xwem-frame-p fr)
       (funcall ,fn fr)
       (setq fr (xwem-frame-link-next fr)))))

;;; X properties
(defmacro xwem-frame-XProperty-get (frame prop-atom-string)
  `(xwem-XProperty-get (xwem-frame-xwin ,frame) ,prop-atom-string))
(defmacro xwem-frame-XProperty-set (frame prop-atom-string prop-val)
  `(xwem-XProperty-set (xwem-frame-xwin ,frame) ,prop-atom-string ,prop-val))

(define-xwem-deferred xwem-frame-export-frame-configuration ()
  "Export frame configuration to root window.

Profiling results on 9 frames with 32 windows:
Function Name                          Call Count  Elapsed Time  Average Time
=====================================  ==========  ============  ============
xwem-frame-export-frame-configuration  50          0.016508      0.00033016
"
  (xwem-XProperty-set (xwem-rootwin) "XWEM_FRAME_CONFIGURATION"
                      (with-temp-buffer
                        (xwem-frame-config-dump1 (xwem-frame-configuration)
                                                 (current-buffer))
                        (read (buffer-substring (point-min) (point-max))))))

(defun xwem-frame-frame-command-post-hook ()
  "Function to use in `xwem-post-command-hook'.
It exports frame configuration after each xwem command."
  (when xwem-frame-configuration-exporting
    (xwem-frame-export-frame-configuration)))

(defun xwem-frame-import-frame-configuration ()
  "Import frame configuration fram root window."
  (eval (xwem-XProperty-get (xwem-rootwin) "XWEM_FRAME_CONFIGURATION")))

;;; Functions
(define-xwem-deferred xwem-frame-apply-state (frame)
  "Apply FRAME's state to life."
  (cond ((eq (xwem-frame-state frame) 'mapped)
         (XMapWindow (xwem-dpy) (xwem-frame-xwin frame)))
        ((eq (xwem-frame-state frame) 'unmapped)
	 (XUnmapWindow (xwem-dpy) (xwem-frame-xwin frame)))))

(defun xwem-frame-unmap (frame)
  "Unmap frame FRAME."
  (setf (xwem-frame-state frame) 'unmapped)
  (xwem-frame-apply-state frame))

;;;###xwem-autoload
(defun xwem-frame-map (frame)
  "Map frame FRAME."
  (setf (xwem-frame-state frame) 'mapped)
  (xwem-frame-apply-state frame))

(define-xwem-deferred xwem-frame-apply-raise-lower (frame)
  "Apply FRAME's raise/lower state to life."
  (let ((rl (xwem-frame-get-prop frame 'raise-lower-state)))
    (cond ((eq rl 'raise)
           (xwem-misc-raise-xwin (xwem-frame-xwin frame)))
          ((eq rl 'lower)
           (xwem-misc-lower-xwin (xwem-frame-xwin frame))))))

;;;###autoload(autoload 'xwem-frame-lower "xwem-frame" "" t)
(define-xwem-command xwem-frame-lower (frame)
  "Lower FRAME's window."
  (xwem-interactive (list (xwem-frame-selected)))

  (xwem-frame-put-prop frame 'raise-lower-state 'lower)
  (xwem-frame-apply-raise-lower frame))
(put 'xwem-frame-lower 'xwem-frame-command t)

;;;###autoload(autoload 'xwem-frame-raise "xwem-frame" "" t)
(define-xwem-command xwem-frame-raise (frame)
  "Raise FRAME's window."
  (xwem-interactive (list (xwem-frame-selected)))

  (xwem-frame-map frame)                ; make sure frame is mapped
  (xwem-frame-put-prop frame 'raise-lower-state 'raise)
  (xwem-frame-apply-raise-lower frame))
(put 'xwem-frame-raise 'xwem-frame-command t)

(defun xwem-frame-embedded-for-frame (frame)
  "Return XWEM frame for which FRAME is embedded."
  (let* ((cl (and (xwem-frame-p frame)
                  (xwem-frame-get-prop frame 'xwem-embedded-cl)))
         (rv (and (xwem-cl-p cl)
                  (xwem-cl-frame cl))))
    rv))

(defun xwem-frame-unembedd (frame &optional new-type)
  "Unembedd FRAME."
  (when (xwem-frame-embedded-p frame)
    (let* ((cl (xwem-frame-get-prop frame 'xwem-embedded-cl))
           (tpnt (car (XTranslateCoordinates
                       (xwem-dpy) (xwem-frame-xwin frame)
                       (xwem-rootwin) (xwem-frame-x frame)
                       (xwem-frame-y frame)))))
      ;; Remove clients stuff
      (xwem-cl-destroy cl)

      ;; Unmark FRAME as embedded
      (xwem-cl-rem-prop cl 'xwem-embedded-frame)
      (xwem-frame-rem-prop frame 'xwem-embedded-cl)

      ;; Set new frame TYPE
      (setf (xwem-frame-type frame) (or new-type 'desktop))

      (XReparentWindow (xwem-dpy) (xwem-frame-xwin frame) (xwem-rootwin)
                       (X-Point-x tpnt) (X-Point-y tpnt))
      (xwem-frame-set-pos frame (X-Point-x tpnt) (X-Point-y tpnt))
      (xwem-frame-apply-state frame))))

;;;###xwem-autoload
(defun xwem-select-frame (frame)
  "Set FRAME to be selected frame.
Actually all the work done in `xwem-select-window'."
  (or (xwem-frame-alive-p frame)
      (error 'xwem-error "Selecting dead frame"))
  (xwem-select-window (xwem-frame-selwin frame)))

(defun xwem-frame-autoiconify-on-deselect ()
  "Maybe iconify FRAME, when deselecting FRAME."
  (when (eq xwem-frame-autoiconify-mode 'always)
    ;; NOTE: double deferring
    (xwem-deferred-funcall
     (lambda (frame)
       (when (xwem-frame-p frame)
	 (xwem-frame-unmap frame)))
     (xwem-frame-selected))))

(defun xwem-frame-autoiconify-on-select ()
  "Maybe iconify some frames when selecting FRAME."
  (when (and (eq xwem-frame-autoiconify-mode 'intersect)
	     (xwem-frame-desktop-p (xwem-frame-selected)))
    (let ((nfr-rect (X-Geom-to-X-Rect
                     (xwem-frame-xgeom (xwem-frame-selected))))
          (frames (xwem-frames-list 'desktop)))
      (while frames
        (when (and (not (eq (xwem-frame-selected) (car frames)))
                   (xwem-frame-mapped-p (car frames))
                   (X-Rect-intersect-p
                    nfr-rect (X-Geom-to-X-Rect
                              (xwem-frame-xgeom (car frames)))))
          ;; NOTE: double deferring
          (xwem-deferred-funcall
	   (lambda (frame)
	     (when (xwem-frame-p frame)
	       (xwem-frame-unmap frame)))
	   (car frames)))
        (setq frames (cdr frames))))))

;;;###xwem-autoload(autoload 'xwem-make-frame-1 "xwem-frame")
(defun* xwem-make-frame-1 (type &key params props noselect)
  "Create new frame with optional frame properties PROPS.
If EMBEDDED-p is non-nil than create embedded frame.
If NOSELECT is non-nil then do not select newly created frame to be
current."
  (let* ((fplist (copy-list xwem-frame-default-properties))
         (frame (apply 'make-xwem-frame params))
         fwin parwin)

    (setf (xwem-frame-type frame) type)
    (setf (xwem-frame-state frame) 'unmapped)

    ;;; Initialise FRAME's geometry
    (unless (xwem-frame-xgeom frame)
      (setf (xwem-frame-xgeom frame) (make-X-Geom)))

    (unless (xwem-frame-x frame)
      (setf (xwem-frame-x frame) 0))
    (unless (xwem-frame-y frame)
      (setf (xwem-frame-y frame) 0))
    (unless (xwem-frame-width frame)
      (setf (xwem-frame-width frame) (X-Geom-width (xwem-rootgeom))))
    (unless (xwem-frame-height frame)
      (setf (xwem-frame-height frame)
            (- (X-Geom-height (xwem-rootgeom))
               (if (X-Geom-p (xwem-minib-xgeom xwem-minibuffer))
                   (X-Geom-height (xwem-minib-xgeom xwem-minibuffer))
                 0))))

    ;;; Initialize FRAME's X window
    (setq fwin (XCreateWindow
                (xwem-dpy) nil
                (xwem-frame-x frame)
                (xwem-frame-y frame)
                (xwem-frame-width frame)
                (xwem-frame-height frame)
                0 nil nil nil
                :override-redirect (not (xwem-frame-embedded-p frame))
                :backing-store nil
                :background-pixmap (unless (xwem-frame-embedded-p frame)
                                     X-ParentRelative)
                :cursor xwem-frame-cursor
                :event-mask xwem-frame-ev-mask))
    (XLowerWindow (xwem-dpy) fwin)
    (X-Win-put-prop fwin 'xwem-frame frame)
    (setf (xwem-frame-xwin frame) fwin)

    ;; Install events handlers
    (X-Win-EventHandler-add-new
     fwin 'xwem-frame-events-handler 150)
    (X-Win-EventHandler-add-new
     fwin 'xwem-ev-reconfig 40 (list X-ConfigureRequest))

    ;; XXX Setup WM_XXX stuff
    (XSetWMProtocols (xwem-dpy) fwin
                     (list (X-Atom-find-by-name (xwem-dpy) "WM_DELETE_WINDOW")
                           (X-Atom-find-by-name (xwem-dpy) "WM_TAKE_FOCUS")))
    (XSetWMClass (xwem-dpy) fwin
                 (list (symbol-name (xwem-frame-type frame))
                       "xwem-frame"))
    (XSetWMName (xwem-dpy) fwin "xwem-frame")

    ;; Install grabbing
    (xwem-kbd-install-grab 'xwem-frame-prefix fwin)

    ;;; Initialise FRAME properties
    ;; Adjust frame properties in case FRAME is embedded or dedicated
    ;; frame.
    (setq fplist (xwem-misc-merge-plists
                  fplist
                  (cond ((xwem-frame-embedded-p frame)
                         xwem-embedded-frame-default-properties)
                        ((xwem-frame-dedicated-p frame)
                         xwem-dedicated-frame-default-properties))))

    ;;; Initialize FRAME's root window
    (setq parwin (xwem-win-new (list :frame frame) nil))
    (setf (xwem-frame-selwin frame) parwin)
    (setf (xwem-frame-rootwin frame) parwin)

    ;; Set FRAME properties
    (xwem-frame-set-properties frame (xwem-misc-merge-plists fplist props))

    ;; Setup rootwin's geometry
    (xwem-frame-setup-root-win frame)

    ;; Find an empty place in xwem-frames-list or add to the end of
    ;; frames list.
    (let ((allframes xwem-frames-list))
      (while (and allframes (xwem-frame-p (car allframes)))
        (setq allframes (cdr allframes)))
      (if allframes
          (setcar allframes frame)
        (setq xwem-frames-list
              (append xwem-frames-list (list frame)))))

    ;; Handle as client, i.e. make frame to be embedded
    (when (xwem-frame-embedded-p frame)
      (let ((ecl (xwem-xwin-try-to-manage (xwem-frame-xwin frame))))
        (when (xwem-cl-p ecl)
          (xwem-frame-put-prop frame 'xwem-embedded-cl ecl)
          (xwem-cl-put-sys-prop ecl 'xwem-embedded-frame frame))))

    ;; Finally map and maybe select newly created frame
    (unless (xwem-frame-property frame 'initially-unmapped)
      (xwem-frame-map frame))

    (unless noselect
      (xwem-select-frame frame))

    ;; Now run on-create hooks
    (run-hook-with-args 'xwem-frame-creation-hook frame)
    frame))

(defun xwem-init-frame-at-rect (xrect)
  "Create frame to fit in XRECT rectangle."
  (let ((xmrect (make-X-Rect
                 :x 0 :y 0 :width (X-Geom-width (xwem-rootgeom))
                 :height (if (xwem-minib-xgeom xwem-minibuffer)
                             (X-Geom-height (xwem-minib-xgeom xwem-minibuffer))
                           (+ (* 2 xwem-minibuffer-border-width)
                              (frame-pixel-height
                               (xwem-minib-frame xwem-minibuffer))
                              xwem-minibuffer-outer-border-width
                              xwem-minibuffer-outer-border-width)))))

    (when (X-Rect-intersect-p xmrect xrect)
      ;; Take into account this intersection
      (setf (X-Rect-height xrect)
            (- (X-Rect-height xrect)
               (X-Rect-height xmrect))))

    (xwem-make-frame-1 'desktop
                       :params (list :xgeom (X-Rect-to-X-Geom xrect))
                       :noselect t)))

(defun xwem-frame-adjust-geom (frame new-rect &optional ignore-minibuffer)
  "Adjust FRAME geom according to NEW-RECT and xwem-minibuffer geom."
  (let* ((minib-has-xgeom-p (and (not ignore-minibuffer)
                                 (X-Geom-p (xwem-minib-xgeom xwem-minibuffer))))
         (mrect (or (and minib-has-xgeom-p
                         (X-Geom-to-X-Rect (xwem-minib-xgeom xwem-minibuffer)))
                    (make-X-Rect :x 0 :y 0 :width 0 :height 0)))
         (brd (or (and minib-has-xgeom-p
                       (X-Geom-border-width (xwem-minib-xgeom xwem-minibuffer)))
                  0))
         ngeom)
    (when (and (xwem-cl-alive-p (xwem-minib-cl xwem-minibuffer))
               (xwem-cl-active-p (xwem-minib-cl xwem-minibuffer)))
      (when brd
        (incf (X-Rect-width mrect) (+ brd brd))
        (incf (X-Rect-height mrect) (+ brd brd)))

      (when (X-Rect-intersect-p new-rect mrect)
        (setf (X-Rect-height new-rect)
              (- (X-Rect-height new-rect)
                 (X-Rect-height mrect)))))

    (setq ngeom (X-Rect-to-X-Geom new-rect))
    (setf (X-Geom-border-width ngeom)
          (X-Geom-border-width (xwem-frame-xgeom frame)))
    (decf (X-Geom-width ngeom) (* 2 (X-Geom-border-width ngeom)))
    (decf (X-Geom-height ngeom) (* 2 (X-Geom-border-width ngeom)))

    (setf (xwem-frame-xgeom frame) ngeom)))

(defun xwem-frame-default-select-hook ()
  "Do various default things when frame selected.
To be used in `xwem-frame-select-hook'."
  (xwem-frame-autoiconify-on-select)
  (xwem-frame-deferred-redraw-inner-border (xwem-frame-selected)))

(defun xwem-frame-default-deselect-hook ()
  "Do various default things when frame deselected.
To be used in `xwem-frame-deselect-hook'."
  (xwem-frame-autoiconify-on-deselect)
  (xwem-frame-deferred-redraw-inner-border (xwem-frame-selected)))

(defun xwem-frame-create-initial ()
  "Create initial frames."
  ;; Try to import frame configuration from root window
  (xwem-frame-import-frame-configuration)

  (if xwem-frame-dumped-config
      ;; Create frames from saved configuration
      (xwem-frame-config-restore1)

    ;; Xinerama stuff
    (let ((xin (X-XIneramaQueryScreens (xwem-dpy)))
          frame frame-old)
      (if (car xin)
          ;; XInerama enabled, so construct frames linkage
           (while (setq xin (cdr xin))
             (setq frame (xwem-init-frame-at-rect (car xin)))
             (when frame-old
               (xwem-frame-link-insert-after frame-old frame))
             (setq frame-old frame))

        ;; No XInerama, crate just one frame
        (xwem-init-frame-at-rect (X-Geom-to-X-Rect (xwem-rootgeom)))))

    ;; Select very first frame
    (xwem-select-frame (car (xwem-frames-list)))))

;;;###xwem-autoload
(defun xwem-frames-init ()
  "xwem frames initializer."
  (xwem-message 'init "Initializing frames ...")

  (setq xwem-frames-list nil)
  (setq xwem-current-frame nil)

  ;; Create frame cursors
  (setq  xwem-frame-cursor
         (xwem-make-cursor xwem-frame-cursor-shape
                           xwem-frame-cursor-foreground-color
                           xwem-frame-cursor-background-color))

  ;; Add autoiconifier hooks
  (add-hook 'xwem-frame-select-hook 'xwem-frame-default-select-hook)
  (add-hook 'xwem-frame-deselect-hook 'xwem-frame-default-deselect-hook)

  ;; Add post command hook to export frames configuration
  (add-hook 'xwem-post-command-hook 'xwem-frame-frame-command-post-hook)

  ;; Create initial frames
  (xwem-frame-create-initial)

  (xwem-message 'init "Initializing frames ... done"))

;;;###xwem-autoload
(defun xwem-frames-fini ()
  "Finialize frames."
  (mapc 'xwem-frame-destroy (xwem-frames-list)))

;;;###autoload(autoload 'xwem-make-frame "xwem-frame" "" t)
(define-xwem-command xwem-make-frame (type &optional arg)
  "Interactively create new XWEM frame.
With prefix ARG create frame of 'desktop type."
  (xwem-interactive (list (or (and xwem-prefix-arg 'desktop)
                              (xwem-completing-read
                               "XWEM Frame type [desktop]: "
                               (mapcar (lambda (cc) (list (symbol-name cc)))
                                       xwem-frame-types)))
                          xwem-prefix-arg))
  (when (stringp type)
    (if (string= type "")
        (setq type 'desktop)
      (setq type (intern-soft type))))

  (xwem-make-frame-1 type))

(defun xwem-frame-find (how arg)
  "Find frame by ARG. HOW is search type, one of 'xwin 'win 'cl or 'name."
  (let ((flist (xwem-frames-list))
        (rf nil))

    (while flist
      (if (cond ((and (eq how 'xwin)
                      (X-Win-p arg)
                      (= (X-Win-id arg)
                         (X-Win-id (xwem-frame-xwin (car flist)))))
                 t)

                ((and (eq how 'win)
                      (eq (xwem-win-frame arg) (car flist)))
                 t)

                ((and (eq how 'cl)
                      (eq (xwem-cl-frame arg) (car flist)))
                 t)

                ((and (eq how 'name)
                      (string= arg (xwem-frame-name (car flist))))
                 t)

                (t nil))
          (progn
            (setq rf (car flist))
            (setq flist nil))

        (setq flist (cdr flist))))
    rf))

(defun xwem-frame-other-frame (frame)
  "Return other frame for FRAME.
NOTE: not yet implemented"

  ;; Try linkage, and then try closest frame
  (let ((oframe (xwem-frame-link-next frame)))
    (unless (xwem-frame-p oframe)
      (setq oframe (xwem-frame-link-prev frame))
      (unless (xwem-frame-p oframe)
        (setq oframe (cadr (memq frame (xwem-frames-list
                                        (xwem-frame-type frame)))))
        (unless (xwem-frame-p oframe)
          (setq oframe (cadr (memq frame (nreverse
                                          (xwem-frames-list
                                           (xwem-frame-type frame))))))
          (unless (xwem-frame-p oframe)
            (setq oframe (car (xwem-frames-list)))))))
    oframe))

;;;###xwem-autoload
(defun xwem-frame-other (frame &optional type)
  "Same as `xwem-frame-other-frame', but return nil, if no good other frame found.
TYPE is ane of 'any, 'linkage"
  (unless type
    (setq type 'any))

  ;; First try linkaged frames
  (let ((allframes (xwem-frames-list))
        (oframe (xwem-frame-link-next frame)))
    (unless (xwem-frame-mapped-p oframe)
      (setq oframe (xwem-frame-link-prev frame)))

    ;; Now try parent in case when FRAME is embedded frame.
    (unless (xwem-frame-mapped-p oframe)
      (when (xwem-frame-embedded-p frame)
        (let ((cl (xwem-frame-get-prop frame 'xwem-embedded-cl)))
          (when (xwem-cl-p cl)
            (setq oframe (xwem-cl-frame cl))))))

    (when (and (not (xwem-frame-mapped-p oframe))
               (eq type 'any))
      ;; Scan frames list only for 'any TYPE
      (while (and allframes (not (xwem-frame-mapped-p oframe)))
        (when (and (xwem-frame-mapped-p (car allframes))
                   (not (eq frame (car allframes))))
          (setq oframe (car allframes))
          (setq allframes nil))
        (setq allframes (cdr allframes))))
    oframe))

;;;###xwem-autoload
(defun xwem-frame-num (frame)
  "Return FRAME index in `xwem-frames-list'."
  (- (length xwem-frames-list)
     (length (memq frame xwem-frames-list))))

(defun xwem-frame-xy-in-p (x y frame)
  "Return non-nil if point at X Y is in FRAME."
  (and (>= x (xwem-frame-x frame))
       (<= x (+ (xwem-frame-x frame) (xwem-frame-width frame)))
       (>= y (xwem-frame-y frame))
       (<= y (+ (xwem-frame-y frame) (xwem-frame-height frame)))))

(defun xwem-frame-at (x y &optional maped-p)
  "Return frame which contain point at X Y.
If MAPED-P is non-nil - search only mapped frame."
  (loop for frame in (xwem-frames-list)
    if (and (or (null maped-p)
                (eq (xwem-frame-state frame) 'mapped))
            (xwem-frame-xy-in-p x y frame))
    return frame))

;;;###xwem-autoload(autoload 'xwem-frame-apply-xgeom-1 "xwem-frame" nil nil)
;;;###xwem-autoload(autoload 'xwem-frame-apply-xgeom "xwem-frame" nil nil)
(define-xwem-deferred xwem-frame-apply-xgeom (frame)
  "Apply FRAME's geometry to life."
  (XConfigureWindow (xwem-dpy) (xwem-frame-xwin frame)
                    :x (xwem-frame-x frame)
                    :y (xwem-frame-y frame)
                    :width (xwem-frame-width frame)
                    :height (xwem-frame-height frame)
                    :border-width (xwem-frame-border-width frame))

  (xwem-frame-setup-root-win frame)
  (run-hook-with-args 'xwem-frame-resize-hook frame))

(define-xwem-deferred xwem-frame-apply-position (frame)
  "Apply FRAME's position to life."
  (when (xwem-frame-p frame)
    (XMoveWindow (xwem-dpy) (xwem-frame-xwin frame)
                 (xwem-frame-x frame) (xwem-frame-y frame))
    (run-hook-with-args 'xwem-frame-move-hook frame)))

;;;###xwem-autoload
(defun xwem-frame-set-pos (frame new-x new-y)
  "Set FRAME position at NEW-X and NEW-Y."
  (setf (xwem-frame-x frame) new-x)
  (setf (xwem-frame-y frame) new-y)

  (xwem-frame-apply-position frame))

(define-xwem-deferred xwem-frame-apply-size (frame)
  "Apply FRAME's size to life."
  (when (xwem-frame-p frame)
    (xwem-debug 'xwem-frame "Applying size: %dx%d"
		'(xwem-frame-width frame) '(xwem-frame-height frame))
    (XResizeWindow (xwem-dpy) (xwem-frame-xwin frame)
                   (xwem-frame-width frame) (xwem-frame-height frame))

    (run-hook-with-args 'xwem-frame-resize-hook frame)))

;;;###xwem-autoload
(defun xwem-frame-set-size (frame new-width new-height)
  "Resize FRAME to NEW-WIDTH and NEW-HEIGHT."
  (when new-width
    (setf (xwem-frame-width frame) new-width))
  (when new-height
    (setf (xwem-frame-height frame) new-height))

  (xwem-frame-setup-root-win frame)
  (xwem-frame-apply-size frame))

;;; Frame Events handling
(defun xwem-frame-hexpose (frame xev)
  "Expose event handler."
  (xwem-debug 'xwem-frame "Exposure event count: %S"
              '(X-Event-xexpose-count xev))

  (when (zerop (X-Event-xexpose-count xev))
    ;; Redraw only when no other exposure events follow
    (and (xwem-frame-p frame)
         (xwem-frame-draw frame nil))))

(defun xwem-frame-total-remove (frame)
  "Totally remove FRAME."
  ;; Firstly we need to remove FRAME from frames list.
  (let ((embed-cl (xwem-frame-get-prop frame 'xwem-embedded-cl))
        (oframe (xwem-frame-other frame 'any)))

    ;; If FRAME is selected, select some other frame
    (when (xwem-frame-selected-p frame)
      (setq xwem-current-frame nil)
      (when (xwem-frame-p oframe)
        (xwem-select-frame oframe)))

    ;; Block events handling
    (XSelectInput (xwem-dpy) (xwem-frame-xwin frame) 0)
    (X-Win-EventHandler-rem (xwem-frame-xwin frame) 'xwem-frame-events-handler)
    (X-Win-EventHandler-rem (xwem-frame-xwin frame) 'xwem-ev-reconfig)

    (X-Win-rem-prop (xwem-frame-xwin frame) 'xwem-frame)

    ;; Remove clients from FRAME
    (mapc #'(lambda (fcl)
              (XReparentWindow (xwem-dpy) (xwem-cl-xwin fcl) (xwem-rootwin)
                               (X-Geom-width (xwem-rootgeom))
                               (X-Geom-height (xwem-rootgeom)))
              (xwem-withdraw fcl))
	  (xwem-frame-clients frame))

    ;; Destroy any X wins
    (unless (eq (xwem-frame-state frame) 'destroyed)
      (XDestroySubwindows (xwem-dpy) (xwem-frame-xwin frame))
      (XDestroyWindow (xwem-dpy) (xwem-frame-xwin frame))
      (setf (xwem-frame-state frame) 'destroyed))

    ;; If we are embedded frame than emulate our destroing
    (when (xwem-cl-p embed-cl)
      (xwem-cl-destroy embed-cl))

    ;; Remove FRAME from various lists
    (xwem-frame-link-remove frame)
    (unless xwem-frame-keep-number
      (setq xwem-frames-list (delq frame xwem-frames-list)))

    (xwem-unwind-protect
        ;; Now run on-destroy hooks
        (run-hook-with-args 'xwem-frame-destroy-hook frame)
      ;; Mark FRAME as non valid for referencing.
      (X-invalidate-cl-struct frame))))

(defun xwem-frame-hconfigure (frame xev)
  "FRAME just received ConfigureNotify event XEV."
  (let ((owid (xwem-frame-width frame))
        (ohei (xwem-frame-height frame))
        (nwid (X-Event-xconfigure-width xev)) ;new width
        (nhei (X-Event-xconfigure-height xev))) ;new height

    (unless (and (= owid nwid) (= ohei nhei))
      (xwem-frame-set-size frame nwid nhei)
      (run-hook-with-args 'xwem-frame-resize-hook frame))))

(defun xwem-frame-hclient (frame xev)
  "FRAME just received XClientMessage event XEV."
  (xwem-debug 'xwem-frame
              "FRAME[%d] got ClientMessage, Atom=%S(%s).."
              '(xwem-frame-num frame) '(X-Atom-id (X-Event-xclient-atom xev))
              '(X-Atom-name (xwem-dpy) (X-Event-xclient-atom xev)))

  (cond ((string= (X-Atom-name (xwem-dpy) (X-Event-xclient-atom xev)) "WM_PROTOCOLS")
         (let ((wmda (X-Atom-find (xwem-dpy) (caar (X-Event-xclient-msg xev)))))
           (when (X-Atom-p wmda)
             (cond ((string= (X-Atom-name (xwem-dpy) wmda) "WM_DELETE_WINDOW")
                    (xwem-debug 'xwem-frame "Killing frame, because of WM_DELETE_WINDOW client message")
                    (xwem-frame-total-remove frame))
                   ((string= (X-Atom-name (xwem-dpy) wmda) "WM_TAKE_FOCUS")
                    (xwem-debug 'xwem-frame "Frame(%s): Taking focus .."
                                '(xwem-frame-name frame))
                    (xwem-focus-set (xwem-frame-cl frame)))))
           ))))

(defun xwem-frame-hkeybutton (frame xev)
  "On FRAME handle KeyPress, ButtonPress or ButtonRelease event XEV."
  (xwem-overriding-local-map 'xwem-frame-prefix
    (when (eq (xwem-dispatch-command-xevent xev) 'done)
      (signal 'X-Events-stop nil))))

(defun xwem-frame-events-handler (xdpy win xev)
  "Event handler for frame."
  (if (eq (X-Event-name xev) :X-MapRequest)
     (let ((cl (xwem-xwin-cl (X-Event-xmaprequest-window xev))))
       (when (xwem-cl-p cl)
         ;; Existing client explicitely requests mapping
         (xwem-select-client cl)))

    (let ((frame (X-Win-get-prop win 'xwem-frame)))
      (when (xwem-frame-p frame)
        (xwem-debug 'xwem-frame "Got event ev=%S, win=%S, frame: %d"
                    '(X-Event-name xev) '(X-Win-id win) '(xwem-frame-num frame))
        (X-Event-CASE xev
          (:X-Expose
           (xwem-frame-hexpose frame xev))

          ((:X-KeyPress :X-ButtonPress :X-ButtonRelease)
           (xwem-frame-hkeybutton frame xev))

          (:X-DestroyNotify
           (setf (xwem-frame-state frame) 'destroyed)
           (xwem-frame-total-remove frame))

          (:X-ClientMessage (xwem-frame-hclient frame xev))

          ;; For emebedded frames
          (:X-ConfigureNotify
           (when (xwem-frame-embedded-p frame)
             (xwem-frame-hconfigure frame xev)))
          (:X-MapNotify
           (when (xwem-frame-embedded-p frame)
             (xwem-frame-map frame)))
          (:X-UnmapNotify
           (when (xwem-frame-embedded-p frame)
             (xwem-frame-unmap frame)))
          )))))

;;;; Frame Events handling ends here

(defun xwem-frame-draw (frame full)
  "Draw FRAME. If FULL is t then fully redraw it, i.e. ClearWindow first."
  (unless (eq frame xwem-frame-noredisplay)
    (when full
      (XClearArea (xwem-dpy) (xwem-frame-xwin frame) 0 0
                  (xwem-frame-width frame)
                  (xwem-frame-height frame) nil))

    (xwem-frame-draw-inner-border frame)
    (run-hook-with-args 'xwem-frame-redraw-hook frame)))

;;; Frame configuration section
;;;###xwem-autoload
(defun xwem-frame-configuration-p (frame-config)
  "Return non-nil if FRAME-CONFIG is looks like frame configuration."
  (and (listp frame-config)
       (eq 'xwem-frame-configuration
           (car frame-config))))

(defun xwem-frame-config-find-sframe (frame sframe-list)
  "Find saved frame by FRAME."
  (while (and sframe-list
              (not (eq (xwem-frame-saved-frame (car sframe-list)) frame)))
    (setq sframe-list (cdr sframe-list)))
  (car sframe-list))

;;;###xwem-autoload
(defun xwem-frame-configuration ()
  "Return current xwem frame configuration."
  ;; TODO: save domain faces
  (cons 'xwem-frame-configuration
        (mapcar #'(lambda (frame)
                    (make-xwem-frame-saved
                     :frame frame
                     :selected-p (xwem-frame-selected-p frame)
                     :type (xwem-frame-type frame)
                     :name (xwem-frame-name frame)
                     :xgeom (copy-X-Geom (xwem-frame-xgeom frame))
                     :state (xwem-frame-state frame)
                     :plist (append (xwem-frame-properties frame)
                                    (unless (xwem-frame-mapped-p frame)
                                      '(initially-unmapped t)))
                     :winconfig (xwem-window-configuration frame)))
                (xwem-frames-list))))

;;;###xwem-autoload
(defun xwem-set-frame-configuration (frame-config &optional no-delete)
  "Restore the frames to the state described by FRAME-CONFIG."
  (unless (xwem-frame-configuration-p frame-config)
    (signal 'wrong-type-argument
            (list 'xwem-frame-configuration-p frame-config)))

  ;; TODO:
  ;;   - Maybe recreate frames that in config, but already destroyed?
  (let ((conf (cdr frame-config))
        frames-to-delete)
    (mapc #'(lambda (frame)
              (let ((sframe (xwem-frame-config-find-sframe frame conf)))
                (if (xwem-frame-saved-p sframe)
                    (progn
                      (setf (xwem-frame-name frame)
                            (xwem-frame-saved-name sframe)
                            (xwem-frame-state frame)
                            (xwem-frame-saved-state sframe))
                      ;; Restore properties
                      (xwem-frame-set-properties
                       frame (xwem-frame-saved-plist sframe))
                      ;; Restore geometry
                      (xwem-frame-adjust-geom
                       frame (xwem-frame-saved-xgeom sframe))
                      ;; Restore windows configuration
                      (xwem-set-window-configuration
                       (xwem-frame-saved-winconfig sframe))
                      ;; Apply frame state
                      (xwem-frame-apply-state frame)
                      ;; Maybe select fram
                      (when (xwem-frame-saved-selected-p sframe)
                        (xwem-select-frame frame)))

                  (setq frames-to-delete (cons frame frames-to-delete)))))
          (xwem-frames-list))

    (if no-delete
        (mapc 'xwem-frame-hide frames-to-delete)
      (mapc 'xwem-frame-destroy frames-to-delete))
    ))

(defun xwem-frame-config-dump1 (config buffer &optional append)
  "Dump frames CONFIG to BUFFER.
If APPEND is non-nil, do not erase FILE, just append to the end."
  ;; TODO: dump also domain faces
  (let ((ccf (copy-sequence config))
        wcf)
    (with-current-buffer (or buffer (current-buffer))
      (if append
          (progn
            (goto-char (point-max))
            (insert "\n"))
        (erase-buffer))

      (insert "(setq xwem-frame-dumped-config (list 'xwem-frame-configuration\n")
      (mapc #'(lambda (sfr)
                (setf (xwem-frame-saved-frame sfr) nil)

                ;; Adjust win config
                (setq wcf (xwem-frame-saved-winconfig sfr))
                (setf (xwem-win-config-frame wcf) nil)
                (setf (xwem-win-config-current-cl wcf) nil)
                (flet ((clrwin (swin)
                         (setf (xwem-win-saved-clients swin) nil)
                         (setf (xwem-win-saved-cl swin) nil)
                         (when (xwem-win-saved-first-vchild swin)
                           (clrwin (xwem-win-saved-first-vchild swin)))
                         (when (xwem-win-saved-first-hchild swin)
                           (clrwin (xwem-win-saved-first-hchild swin)))
                         (when (xwem-win-saved-next swin)
                           (clrwin (xwem-win-saved-next swin)))
                         (when (xwem-win-saved-prev swin)
                           (clrwin (xwem-win-saved-prev swin)))))
                  (clrwin (xwem-win-config-saved-root-window wcf)))

                (insert (format "%S\n" sfr)))
            (cdr ccf))
      (insert "))\n"))))

(defun xwem-frame-config-dump (config &optional file)
  "Dump frame configuration CONFIG to FILE.
If FILE ommited, than ~/.xwem/xwem-configs.el will be used."
  (unless (xwem-frame-configuration-p config)
    (error 'xwem-error "Not an xwem frame configuration" config))

  (unless file
    (setq file (expand-file-name "xwem-configs.el" xwem-dir)))

  (let* ((find-file-hooks nil)          ; omit autoinsert and others
         (buf (find-file-noselect file)))
    (xwem-frame-config-dump1 config buf)
    (save-buffer buf)))

(defun xwem-frame-config-restore1 ()
  "Internal frame config restorer."
  (let (frame-to-select)
    (mapc #'(lambda (sfr)
              (let ((swin (xwem-frame-saved-winconfig sfr))
                    nframe)
                (setq nframe (xwem-make-frame-1
                              (xwem-frame-saved-type sfr)
                              :params (list :name (xwem-frame-saved-name sfr)
                                            :xgeom (xwem-frame-saved-xgeom sfr))
                              :props (xwem-frame-saved-plist sfr)
                              :noselect t))
                (when (xwem-frame-p nframe)
                  (setf (xwem-win-config-frame swin) nframe)
                  (xwem-set-window-configuration swin)

                  (when (and  (xwem-frame-saved-selected-p sfr)
                              (not frame-to-select))
                    (setq frame-to-select nframe)))))
          (cdr xwem-frame-dumped-config))
    (when frame-to-select
      (xwem-select-frame frame-to-select)))

  ;; dumped config has been restored
  (setq xwem-frame-dumped-config nil))

(defun xwem-frame-config-restore (&optional file)
  "Restore saved frames configuration from FILE.
Default FILE is ~/.xwem/xwem-configs.el"
  (unless file
    (setq file (expand-file-name "xwem-configs.el" xwem-dir)))

  (load-file file)
  (unless (xwem-frame-configuration-p xwem-frame-dumped-config)
    (error 'xwem-error "no frames configuration to restore"))

  (xwem-frame-config-restore1))

;;; Frame configuration section ends here
;;;###autoload(autoload 'xwem-frame-next "xwem-frame" "" t)
(define-xwem-command xwem-frame-next (arg)
  "Switch to ARG next frame."
  (xwem-interactive "p")

  (let ((frame (nth arg (memq (xwem-frame-selected) (xwem-frames-list)))))
    (when (and xwem-frame-rolling-switch
               (not (xwem-frame-p frame)))
      ;; Assume first frame if there no next
      (setq frame (car (xwem-frames-list))))

    (unless (xwem-frame-p frame)
      (error 'xwem-error "Invalid frame"))

    (xwem-select-frame frame)))
(put 'xwem-frame-next 'xwem-frame-command t)

;;;###autoload(autoload 'xwem-frame-previous "xwem-frame" "" t)
(define-xwem-command xwem-frame-previous (arg)
  "Switch to ARG previous frame."
  (xwem-interactive "p")

  (let ((frame (nth arg (memq (xwem-frame-selected)
                              (reverse (xwem-frames-list))))))
    (when (and xwem-frame-rolling-switch
               (not (xwem-frame-p frame)))
      ;; Assume last frame if there no previous
      (setq frame (car (last (xwem-frames-list)))))

    (unless (xwem-frame-p frame)
      (error 'xwem-error "Invalid frame"))

    (xwem-select-frame frame)))
(put 'xwem-frame-previous 'xwem-frame-command t)

;;;###autoload(autoload 'xwem-frame-switch "xwem-frame" "Switch to frame by name." t)
(define-xwem-command xwem-frame-switch (name)
  "Switch to frame by NAME."
  (xwem-interactive
   (list (xwem-completing-read
          "XWEM Frame: "
          (mapcar #'(lambda (fr)
                      (list (xwem-frame-name fr)))
                  (xwem-frames-list)))))

  ;; Find frame by name
  (let ((frms (xwem-frames-list))
        (frame nil))
    (while (and frms (not (string= (xwem-frame-name (car frms)) name)))
      (setq frms (cdr frms)))
    (setq frame (car frms))

    (unless (xwem-frame-alive-p frame)
      (error 'xwem-error "No such frame"))

    (xwem-select-frame frame)))

;;;###autoload(autoload 'xwem-frame-fast-switch "xwem-frame" "Fast way to switch frames." t)
(define-xwem-command xwem-frame-fast-switch (key)
  "Just switches to frame numbered after KEY.
To be used by H-C-<digit> bindings."
  (xwem-interactive (list (event-key xwem-last-event)))
  (funcall (if xwem-frame-fast-switch-to-linkage
               'xwem-frame-switch-nth-linkage
             'xwem-frame-switch-nth)
           (- (char-to-int key) 48)))

;;;###autoload(autoload 'xwem-frame-switch-nth "xwem-frame" "" t)
(define-xwem-command xwem-frame-switch-nth (arg)
  "Switch xwem frame.
If ARG is numeric prefix, then switch to ARG frame.
If ARG ommited, 0 as ARG value will be used.
If ARG is list and selected frame is embedded, than unembedd it."
  (xwem-interactive "P")

  (when (null arg)
    (setq arg 0))
  (if (numberp arg)
      (let ((frame (nth (abs arg) xwem-frames-list)))
        (if (xwem-frame-p frame)
            (xwem-select-frame frame)
          (xwem-message 'warning "No such %S frame." (abs arg))))

    ;; UNEMBED selected frame
    (let ((frame (xwem-frame-selected)))
      (when (xwem-frame-embedded-p frame)
        (xwem-frame-unembedd frame)
        (xwem-select-frame frame)))
    ))

;;;###autoload(autoload 'xwem-frame-switch-nth-linkage "xwem-frame" "" t)
(define-xwem-command xwem-frame-switch-nth-linkage (arg)
  "Raise all frames that in linkage of frame with number NUM."
  (xwem-interactive "P")

  (when (null arg) (setq arg 0))
  (if (numberp arg)
      (let ((frame (nth (abs arg) xwem-frames-list)))
        (if (not (xwem-frame-p frame))
          (xwem-message 'warning "No such %S frame." (abs arg))

          ;; Select linkage
          (xwem-frame-linkage-map frame 'xwem-frame-raise)
          (xwem-select-frame frame)))
    (xwem-message 'warning "Strange arg value: %S" arg)))

;;;###autoload(autoload 'xwem-frame-destroy "xwem-frame" "" t)
(define-xwem-command xwem-frame-destroy (frame &optional arg)
  "Destroy FRAME. If FRAME is not given selected frame assumed.
If prefix ARG is given - close all clients managed in FRAME."
  (xwem-interactive (list (xwem-frame-selected) xwem-prefix-arg))

  ;; In case of prefix ARG - close all clients
  (when arg
    (mapc 'xwem-client-kill
          (xwem-clients-list #'(lambda (cl)
                                 (eq (xwem-cl-frame cl) frame)))))

  (when (xwem-frame-p frame)
    (xwem-frame-total-remove frame)))
(put 'xwem-frame-destroy 'xwem-frame-command t)

(defun xwem-frame-goto (n direction &optional frame)
  "Goto window at DIRECTION on FRAME N times.
DIRECTION is one of 'next, 'prev, 'next-vert, ..."
  (let* ((gframe (or frame (xwem-frame-selected)))
         (cwin (xwem-frame-selwin gframe)))

    ;; Adjust N and DIRECTION if needed
    (when (and (eq direction 'next)
               (< n 0))
      (setq n (- n)
            direction 'prev))

    (while (> n 0)
      (cond ((eq direction 'next)
             (setq cwin (xwem-window-next cwin)))
            ((eq direction 'prev)
             (setq cwin (xwem-window-prev cwin)))
            ((eq direction 'next-vert)
             (setq cwin (xwem-window-next-vertical cwin)))
            (t (error
                'xwem-error "Bad DIRECTION in `xwem-frame-goto'" direction)))
      (setq n (1- n)))

    (xwem-select-window cwin)))

;;;###autoload(autoload 'xwem-frame-goto-next "xwem-frame" "" t)
(define-xwem-command xwem-frame-goto-next (arg)
  "Goto ARG next window in selected frame."
  (xwem-interactive "p")
  (xwem-frame-goto arg 'next))

;;;###autoload(autoload 'xwem-frame-goto-prev "xwem-frame" "" t)
(define-xwem-command xwem-frame-goto-prev (arg)
  "Goto ARG previous window in selected frame."
  (xwem-interactive "p")
  (xwem-frame-goto arg 'prev))

;;;###autoload(autoload 'xwem-frame-goto-next-vert "xwem-frame" "" t)
(define-xwem-command xwem-frame-goto-next-vert (arg)
  "Goto ARG next window in vertical direction in selected frame."
  (xwem-interactive "p")
  (xwem-frame-goto arg 'next-vert))

;;;###autoload(autoload 'xwem-frame-goto-next-hor "xwem-frame" "" t)
(define-xwem-command xwem-frame-goto-next-hor (arg)
  "Goto ARG next window in horizontal direction in selected frame."
  (xwem-interactive "p")
  (xwem-frame-goto arg 'next-hor))

;;;###autoload(autoload 'xwem-frame-goto-prev-vert "xwem-frame" "" t)
(define-xwem-command xwem-frame-goto-prev-vert (arg)
  "Goto ARG previous window in vertical direction in selected frame."
  (xwem-interactive "p")
  (xwem-frame-goto arg 'priv-vert))

;;;###autoload(autoload 'xwem-frame-goto-prev-hor "xwem-frame" "" t)
(define-xwem-command xwem-frame-goto-prev-hor (arg)
  "Goto ARG previous window in horizontal direction in selected frame."
  (xwem-interactive "p")
  (xwem-frame-goto arg 'prev-hor))

;;;###autoload(autoload 'xwem-frame-split-sbs "xwem-frame" "" t)
(define-xwem-command xwem-frame-split-sbs (n &optional frame side)
  "Makes N frames side by size of FRAME.
SIDE is one of 'vertical or 'horizontal, if ommited or not one of
above - 'horizontal will be used.

Example:
SIDE is 'horiz
+--------+                             +----+----+--...--+----+
+--------+                             +----+----+--...--+----+
| 1      | `xwem-frame-split-sbs' |--> | 1  | 2  |       | N  |
|        |                             |    |    |       |    |
+--------+                             +----+----+--...--+----+

Widths sum of all N frames after sbs split is equal to width of frame
before."
  (xwem-interactive "p")

  (let* ((vertp (eq side 'vertical))
         (frm (or frame (xwem-frame-selected)))
         (wi (xwem-frame-width frm))
         (he (xwem-frame-height frm))
         (nwi wi)
         (nhe he)
         (wost 0)
         (host 0)
         xoff yoff)

    (when (xwem-frame-embedded-p frm)
      (error 'xwem-error "Can't split embedded frame"))

    (if vertp
        (progn
          (setq nhe (/ he (1+ n)))
          (setq host (% he (1+ n))))
      (setq nwi (/ wi (1+ n)))
      (setq wost (% wi (1+ n))))

    (setq xoff (+ nwi wost))
    (setq yoff (+ nhe host))

    ;; Resize frame
    (xwem-frame-set-size frm xoff yoff)

    ;; TODO: - inherit same parent in case FRM is embedded
    ;;       - install frames linkage
    (let ((oframe frm)
          (nframe nil)
          (samex (xwem-frame-x frm))
          (samey (xwem-frame-y frm)))
      (while (> n 0)
        (setq nframe
              (xwem-make-frame-1 (xwem-frame-type frm)
                                 :params
                                 (list :xgeom
                                       (make-X-Geom :x (if vertp
                                                           samex
                                                         (+ samex xoff))
                                                    :y (if vertp
                                                           (+ samey yoff)
                                                         samey)
                                                    :width nwi
                                                    :height nhe))
                                 :noselect t))
        ;; Now setup linkage
        (xwem-frame-link-insert-after oframe nframe)
        (setq oframe nframe)

        (if vertp
            (setq yoff (+ yoff nhe))
          (setq xoff (+ xoff nwi)))
        (setq n (1- n))))
    ))
(put 'xwem-frame-iresize 'xwem-frame-command t)

;;;###autoload(autoload 'xwem-frame-sbs-hor-split "xwem-frame" "" t)
(define-xwem-command xwem-frame-sbs-hor-split (n)
  "Make horizontal sbs split N times for selected frame."
  (xwem-interactive "p")
  (xwem-frame-split-sbs n))
(put 'xwem-frame-sbs-hor-split 'xwem-frame-command t)

;;;###autoload(autoload 'xwem-frame-sbs-vert-split "xwem-frame" "" t)
(define-xwem-command xwem-frame-sbs-vert-split (n)
  "Make vertical sbs split N times for selected frame."
  (xwem-interactive "p")
  (xwem-frame-split-sbs n nil 'vertical))
(put 'xwem-frame-sbs-vert-split 'xwem-frame-command t)

;;;###autoload(autoload 'xwem-frame-fit-screen "xwem-frame" "" t)
(define-xwem-command xwem-frame-fit-screen (frame &optional ignore-minibuffer)
  "Fit FRAME to screen sizes.
If prefix argument IGNORE-MINIBUFFER is specified, then fit frame to
whole screen, ignoring minibuffer visibility."
  (xwem-interactive (list (xwem-frame-selected) xwem-prefix-arg))

  ;; Take into acount XInerama layout
  (let ((frect (X-Geom-to-X-Rect (xwem-frame-xgeom frame)))
        (xin (X-XIneramaQueryScreens (xwem-dpy))))
    (if (car xin)
        (progn
          ;; XInerama enabled
          (while (and (setq xin (cdr xin))
                      (not (X-Rect-intersect-p (car xin) frect))))
          (setq xin (car xin)))

      ;; No XInerama, so use root geometry
      (setq xin (X-Geom-to-X-Rect (xwem-rootgeom))))

    (xwem-frame-adjust-geom frame xin ignore-minibuffer)

    (xwem-frame-set-size
     frame (xwem-frame-width frame) (xwem-frame-height frame))
    (xwem-frame-set-pos
     frame (xwem-frame-x frame) (xwem-frame-y frame))

    ;; XXX uninstall linkage if any
    (xwem-frame-link-remove frame)))
(put 'xwem-frame-fit-screen 'xwem-frame-command t)

;;;###autoload(autoload 'xwem-frame-transpose "xwem-frame" "" t)
(define-xwem-command xwem-frame-transpose (arg)
  "Transpose frames ARG times."
  (xwem-interactive "p")
  (error 'xwem-error "`xwem-frame-transpose' not implemented yet."))

;;;###autoload(autoload 'xwem-frame-showroot "xwem-frame" "" t)
(define-xwem-command xwem-frame-showroot ()
  "Show root window, i.e. unmap all xwem frames."
  (xwem-interactive)
  (mapc 'xwem-frame-unmap (xwem-frames-list))
  (xwem-select-client nil))

;;;###autoload(autoload 'xwem-frame-hide "xwem-frame" "" t)
(define-xwem-command xwem-frame-hide (frame &optional no-select)
  "Hide FRAME.
When called interactively, FRAME is selected frame.
With prefix ARG, do not select new frame."
  (xwem-interactive (list (xwem-frame-selected)
                          xwem-prefix-arg))

  (let ((oframe (xwem-frame-other frame))
	em-cl)
    (when (xwem-frame-mapped-p frame)
      ;; We are about to hide FRAME, so select other frame
      (when (and (not no-select)
                 (xwem-frame-p oframe))
        (xwem-select-frame oframe))

      ;; In case FRAME is embedded frame - iconify coressponding
      ;; client.
      (if (xwem-cl-p (setq em-cl (xwem-frame-get-prop
				  frame 'xwem-embedded-cl)))
	  (xwem-iconify em-cl)
	(xwem-frame-unmap frame)))))
(put 'xwem-frame-hide 'xwem-frame-command t)

;;;###autoload(autoload 'xwem-transpose-frames "xwem-frame" "" t)
(define-xwem-command xwem-transpose-frames (arg)
  "Transpose selected frame with ARG other frame."
  (xwem-interactive "p")

  (let* ((sfr (xwem-frame-selected))
         (ofr (xwem-frame-other sfr))
         sg og)

    (when (xwem-frame-p ofr)
      (setq sg (copy-sequence (xwem-frame-xgeom sfr))
            og (copy-sequence (xwem-frame-xgeom ofr)))

      (xwem-frame-set-pos sfr (X-Geom-x og) (X-Geom-y og))
      (xwem-frame-set-pos ofr (X-Geom-x sg) (X-Geom-y sg))

      (xwem-frame-set-size sfr (X-Geom-width og) (X-Geom-height og))
      (xwem-frame-set-size ofr (X-Geom-width sg) (X-Geom-height sg))

      ;; Finally exchange positions (aka frame number) in
      ;; `xwem-frames' list
      (xwem-list-exchange-els xwem-frames-list sfr ofr))))
(put 'xwem-transpose-frames 'xwem-frame-command t)

;;;###autoload(autoload 'xwem-frame-set-name "xwem-frame" "" t)
(define-xwem-command xwem-frame-set-name (name &optional frame)
  "Set FRAME's name to NAME."
  (xwem-interactive (list (xwem-read-from-minibuffer
                           (format "New frame name (old: %s): "
                                   (xwem-frame-name (xwem-frame-selected))))
                          (xwem-frame-selected)))

  (setf (xwem-frame-name frame) name)

  ;; Finally run hooks
  (run-hook-with-args 'xwem-frame-change-hook frame))
(put 'xwem-frame-set-name 'xwem-frame-command t)

;; XXX: what this?
(defun xwem-frame-in-delim-p (win x y)
  "Return non-nil if X Y is inside some delimeter."
  (catch 'found
    (let ((hc (xwem-win-hchild win))
          (vc (xwem-win-vchild win))
          rwin)

      (while (xwem-win-p hc)
        ;; For horizontal split
        (when (and (> x (+ (xwem-win-x hc) (xwem-win-width hc)))
                   (< x (+ (xwem-win-x hc) (xwem-win-width hc)
                           (xwem-win-delim-width hc)))
                   (> y (xwem-win-y hc))
                   (< y (+ (xwem-win-y hc) (xwem-win-height hc))))
          (throw 'found hc))
        (when (setq rwin (xwem-frame-in-delim-p hc x y))
          (throw 'found rwin))
        (setq hc (xwem-win-next hc)))

      (while (xwem-win-p vc)
        (when (and (> x (xwem-win-x vc))
                   (< x (+ (xwem-win-x vc) (xwem-win-width vc)))
                   (> y (+ (xwem-win-y vc) (xwem-win-height vc)))
                   (< y (+ (xwem-win-y vc) (xwem-win-height vc)
                           (xwem-win-delim-width vc))))
          (throw 'found vc))
        (when (setq rwin (xwem-frame-in-delim-p vc x y))
          (throw 'found rwin))
        (setq vc (xwem-win-next vc)))
      nil)))

;;;###autoload(autoload 'xwem-frame-on-delim-resize "xwem-frame" "" t)
(define-xwem-command xwem-frame-on-delim-resize ()
  "Resize window dragging delimiter."
  (xwem-interactive)

  (let* ((x (X-Event-xbutton-event-x xwem-last-xevent))
         (y (X-Event-xbutton-event-y xwem-last-xevent))
         (frame (or (X-Win-get-prop
                     (X-Event-xbutton-event xwem-last-xevent) 'xwem-frame)
                    (xwem-frame-at x y)))
         (in-xy (and (xwem-frame-p frame)
                     (xwem-frame-in-delim-p
                      (xwem-frame-rootwin frame) x y)))
         (win in-xy)
         xrect type cursor done xev)

    (when (xwem-win-p win)
      ;; Findout resize tipe (vertical or horisontal)
      (if (> y (+ (xwem-win-y win)
                  (xwem-win-height win)))
          (setq type 'vert)
        (setq type 'horz))

      ;; Fill xrects
      (setq xrect (make-X-Rect :x (xwem-win-x win)
                               :y (xwem-win-y win)
                               :width (xwem-win-width win)
                               :height (xwem-win-height win)))

      ;; Create cursor
      (setq cursor (xwem-make-cursor
                    (if (eq type 'vert)
                        X-XC-sb_v_double_arrow
                      X-XC-sb_h_double_arrow)))

      (xwem-mouse-grab cursor (xwem-frame-xwin frame)
                       (Xmask-or XM-ButtonRelease XM-ButtonMotion))
      (unless (eq xwem-frame-on-delim-resize-mode 'opaque)
        (xwem-misc-outline xrect 'normal (xwem-frame-xwin frame)))
      (xwem-unwind-protect
          (while (and (not done)        ; still not done?
                      (setq xev (xwem-next-event)) ; deliver event before win check
                      (xwem-win-next win)) ; still has delimiter?
            (X-Event-CASE xev
              (:X-ButtonRelease (setq done t))

              (:X-MotionNotify
               (cond ((and (eq type 'vert)
                           (> (X-Event-xmotion-event-y xev) 0)
                           (> (X-Event-xmotion-event-y xev)
                              (+ (X-Rect-y xrect) xwem-win-min-height))
                           (not (= (X-Event-xmotion-event-y xev)
                                   (+ (X-Rect-y xrect)
                                      (X-Rect-height xrect)))))
                      (unless (eq xwem-frame-on-delim-resize-mode 'opaque)
                        (xwem-misc-outline
                         xrect 'normal (xwem-frame-xwin frame)))
                      (setf (X-Rect-height xrect)
                            (- (X-Event-xmotion-event-y xev) (X-Rect-y xrect)))
                      (if (eq xwem-frame-on-delim-resize-mode 'opaque)
                          (xwem-window-enlarge-vertically
                           (- (X-Rect-height xrect) (xwem-win-height win)) win)
                        (xwem-misc-outline
                         xrect 'normal (xwem-frame-xwin frame))))

                     ((and (eq type 'horz)
                           (> (X-Event-xmotion-event-x xev) 0)
                           (> (X-Event-xmotion-event-x xev)
                              (+ (X-Rect-x xrect) xwem-win-min-width))
                           (not (= (X-Event-xmotion-event-x xev)
                                   (+ (X-Rect-x xrect) (X-Rect-width xrect)))))
                      (unless (eq xwem-frame-on-delim-resize-mode 'opaque)
                        (xwem-misc-outline
                         xrect 'normal (xwem-frame-xwin frame)))
                      (setf (X-Rect-width xrect)
                            (- (X-Event-xmotion-event-x xev) (X-Rect-x xrect)))
                      (if (eq xwem-frame-on-delim-resize-mode 'opaque)
                          (xwem-window-enlarge-horizontally
                           (- (X-Rect-width xrect) (xwem-win-width win)) win)
                        (xwem-misc-outline
                         xrect 'normal (xwem-frame-xwin frame))))))))

        (xwem-mouse-ungrab)
        (XFreeCursor (xwem-dpy) cursor)

        (unless (eq xwem-frame-on-delim-resize-mode 'opaque)
          (xwem-misc-outline xrect 'normal (xwem-frame-xwin frame))

          (if (eq type 'vert)
              (xwem-window-enlarge-vertically (- (X-Rect-height xrect)
                                                 (xwem-win-height win)) win)
            (xwem-window-enlarge-horizontally (- (X-Rect-width xrect)
                                                 (xwem-win-width win)) win)))
        ))))
(put 'xwem-frame-on-delim-resize 'xwem-frame-command t)

;;;###autoload(autoload 'xwem-frame-on-delim-menu "xwem-frame" "" t)
(define-xwem-command xwem-frame-on-delim-menu ()
  "Popup menu when delimiter clicked."
  (xwem-interactive)

  (let* ((x (X-Event-xbutton-event-x xwem-last-xevent))
         (y (X-Event-xbutton-event-y xwem-last-xevent))
         (frame (xwem-frame-at x y))
         (win (and (xwem-frame-p frame)
                   (xwem-frame-in-delim-p
                    (xwem-frame-rootwin frame) x y))))

    (when (xwem-win-p win)
      (xwem-popup-menu (xwem-generate-window-menu "Window" win)))))

;;;###xwem-autoload
(defun xwem-frame-imove-internal (frame sx sy &optional imove-mode)
  "Interactively move FRAME."
  (when (xwem-frame-embedded-p frame)
    (error 'xwem-error "Can't interactively move embedded frame"))

  (unless imove-mode
    (setq imove-mode
          (funcall xwem-frame-imoveresize-mode-function frame 'move)))

  (let ((step 1) (done nil)
        last-xrect curr-xrect xev)
    (setq last-xrect (X-Geom-to-X-Rect (xwem-frame-xgeom frame))
          curr-xrect (copy-X-Rect last-xrect))

    (xwem-mouse-grab xwem-cursor-move nil ;(xwem-frame-xwin frame)
                     (Xmask-or XM-ButtonPress XM-ButtonRelease
                               XM-ButtonMotion XM-PointerMotion))
    (if (eq imove-mode 'opaque)
        (xwem-frame-map frame)
      (xwem-misc-outline last-xrect imove-mode))

    ;; Normally we should do this event loop under GrabServer, but
    ;; grabbing server causes XEmacs to freeze in some
    ;; circumstances. --lg
    (xwem-unwind-protect
        (while (not done)
          (X-Event-CASE (setq xev (xwem-next-event))
            ((:X-ButtonPress :X-ButtonRelease) (setq done t))
            (:X-MotionNotify
             ;; Update curr-xrect
             (setf (X-Rect-x curr-xrect)
                   (+ (X-Rect-x curr-xrect)
                      (- (X-Event-xmotion-root-x xev) sx)))
             (setq sx (X-Event-xmotion-root-x xev))
             (setf (X-Rect-y curr-xrect)
                   (+ (X-Rect-y curr-xrect)
                      (- (X-Event-xmotion-root-y xev) sy)))
             (setq sy (X-Event-xmotion-root-y xev))

             (when (or (> (abs (- (X-Rect-x curr-xrect)
                                  (X-Rect-x last-xrect))) step)
                       (> (abs (- (X-Rect-y curr-xrect)
                                  (X-Rect-y last-xrect))) step))
               (unless (eq imove-mode 'opaque)
                 (xwem-misc-outline last-xrect imove-mode))

               (setf (X-Rect-x last-xrect) (X-Rect-x curr-xrect))
               (setf (X-Rect-y last-xrect) (X-Rect-y curr-xrect))
               (if (eq imove-mode 'opaque)
                   (xwem-frame-set-pos
                    frame (X-Rect-x last-xrect) (X-Rect-y last-xrect))
                 (xwem-misc-outline last-xrect imove-mode))))

            (t (X-Dpy-event-dispatch xev))))

      (xwem-mouse-ungrab)
      (unless (eq imove-mode 'opaque)
        (xwem-misc-outline last-xrect imove-mode))

      ;; Apply changes
      (xwem-frame-set-pos frame (X-Rect-x last-xrect) (X-Rect-y last-xrect)))))

;;;###autoload(autoload 'xwem-frame-imove "xwem-frame" "" t)
(define-xwem-command xwem-frame-imove ()
  "Interactively move FRAME."
  (xwem-interactive)

  (unless (or (interactive-p)
              (= (X-Event-type xwem-last-xevent) X-ButtonPress))
    (error 'xwem-error "`xwem-frame-imove'' must be binded to mouse event"))

  (let* ((srx (X-Event-xbutton-root-x xwem-last-xevent))
         (sry (X-Event-xbutton-root-y xwem-last-xevent))
         (frame (or (xwem-xwin-frame (X-Event-xbutton-event xwem-last-xevent))
                    (xwem-frame-at srx sry))))

    (unless (xwem-frame-p frame)
      (error 'xwem-error "`xwem-frame-imove' on non-frame"))

    (xwem-frame-imove-internal frame srx sry)))
(put 'xwem-frame-imove 'xwem-frame-command t)

;;;###autoload(autoload 'xwem-frame-iresize "xwem-frame" "" t)
(define-xwem-command xwem-frame-iresize ()
  "Interactively resize frame"
  (xwem-interactive)

  (unless (or (interactive-p)
              (= (X-Event-type xwem-last-xevent) X-ButtonPress))
    (error 'xwem-error "`xwem-frame-iresize' must be binded to mouse event"))

  (let* ((srx (X-Event-xbutton-root-x xwem-last-xevent))
         (sry (X-Event-xbutton-root-y xwem-last-xevent))
         (frame (or (xwem-xwin-frame (X-Event-xbutton-event xwem-last-xevent))
                    (xwem-frame-at srx sry)))
         (iresize-mode
          (funcall xwem-frame-imoveresize-mode-function frame 'resize))
         (last-xrect (and (xwem-frame-p frame)
                          (make-X-Rect :x (xwem-frame-x frame)
                                       :y (xwem-frame-y frame)
                                       :width (- srx (xwem-frame-x frame))
                                       :height (- sry (xwem-frame-y frame)))))
         (curr-xrect (copy-X-Rect last-xrect))
         step-x step-y min-height min-width
         done xev)

    (unless (xwem-frame-p frame)
      (error 'xwem-error "`xwem-frame-iresize' on non-frame"))

    ;; Setup steps
    (when (xwem-frame-dedicated-p frame)
      (let ((sg (xwem-cl-step-geom (xwem-frame-cl frame)))
            (mg (xwem-cl-min-geom (xwem-frame-cl frame))))
        (setq step-x (car sg)
              step-y (cdr sg)
              min-width (car mg)
              min-height (cdr mg))))
    (unless step-x (setq step-x 1))
    (unless step-y (setq step-y 1))

    ;; Calculate minimal width/height of frame.
    ;; Simple sum win's min width/height so many times how many
    ;; windows
    (unless min-width
      (setq min-width
            (+ xwem-win-min-width
               (- (xwem-frame-width frame)
                  (xwem-win-width (xwem-frame-rootwin frame)))))
      (xwem-win-map #'(lambda (w)
                        (let ((pwin (xwem-win-parent w)))
                          (when (and (and pwin (xwem-win-hchild pwin)))
                            (incf min-width
                                  (+ xwem-win-min-width
                                     (car xwem-win-horizontal-delim-width))))))
                    (xwem-frame-selwin frame)))
    (unless min-height
      (setq min-height
            (+ xwem-win-min-height
               (- (xwem-frame-height frame)
                  (xwem-win-height (xwem-frame-rootwin frame)))))
      (xwem-win-map #'(lambda (w)
                        (let ((pwin (xwem-win-parent w)))
                          (when (and (and pwin (xwem-win-vchild pwin)))
                            (incf min-height
                                  (+ xwem-win-min-height
                                     (car xwem-win-vertical-delim-width))))))
                    (xwem-frame-selwin frame)))

    ;; Adjust last-xrect according to calculated min-width/height
    (when (< (X-Rect-width last-xrect) min-width)
      (setf (X-Rect-width last-xrect) min-width))
    (when (< (X-Rect-height last-xrect) min-height)
      (setf (X-Rect-height last-xrect) min-height))

    (xwem-mouse-grab xwem-cursor-resize (xwem-frame-xwin frame)
                     (Xmask-or XM-ButtonRelease XM-ButtonMotion))
    (unless (eq iresize-mode 'opaque)
      (xwem-misc-outline last-xrect iresize-mode))
    (xwem-unwind-protect
        (while (not done)
          (X-Event-CASE (setq xev (xwem-next-event))
            (:X-ButtonRelease (setq done t))

            (:X-MotionNotify
             ;; Update curr-xrect
             (setf (X-Rect-width curr-xrect)
                   (- (X-Event-xmotion-root-x xev) (X-Rect-x curr-xrect)))
             (setf (X-Rect-height curr-xrect)
                   (- (X-Event-xmotion-root-y xev) (X-Rect-y curr-xrect)))

             (when (and (or (>= (X-Rect-width curr-xrect) min-width)
                            (>= (X-Rect-height curr-xrect) min-height))
                        (or (> (abs (- (X-Rect-width curr-xrect)
                                       (X-Rect-width last-xrect))) step-x)
                            (> (abs (- (X-Rect-height curr-xrect)
                                       (X-Rect-height last-xrect))) step-y)))
               (unless (eq iresize-mode 'opaque)
                 (xwem-misc-outline last-xrect iresize-mode))
               (when (>= (X-Rect-width curr-xrect) min-width)
                 (setf (X-Rect-width last-xrect) (X-Rect-width curr-xrect)))
               (when (>= (X-Rect-height curr-xrect) min-height)
                 (setf (X-Rect-height last-xrect) (X-Rect-height curr-xrect)))
               (if (eq iresize-mode 'opaque)
                   (xwem-frame-set-size
                    frame (X-Rect-width last-xrect) (X-Rect-height last-xrect))
                 (xwem-misc-outline last-xrect iresize-mode))
               ))))
      (xwem-mouse-ungrab)
      (unless (eq iresize-mode 'opaque)
        (xwem-misc-outline last-xrect iresize-mode))

      ;; Apply changes
      (xwem-frame-set-pos frame (X-Rect-x last-xrect) (X-Rect-y last-xrect))
      (xwem-frame-set-size
       frame (X-Rect-width last-xrect) (X-Rect-height last-xrect)))
    ))
(put 'xwem-frame-iresize 'xwem-frame-command t)

;;;###xwem-autoload
(defun xwem-frame-clients (frame)
  "Make list of all clients FRAME holds."
  (xwem-clients-list `(lambda (cl)
                        (eq (xwem-cl-frame cl) ,frame))))

(define-xwem-face xwem-frame-inner-border-face
  `(((light nonselected) (:foreground "gray80" :background "gray80"))
    ((medium nonselected) (:foreground "gray50" :background "gray50"))
    ((dark nonselected) (:foreground "gray20" :background "gray20"))
    ((light selected) (:foreground "cyan2" :background "cyan2"))
    ((medium selected) (:foreground "royalblue" :background "royalblue"))
    ((dark selected) (:foreground "blue4" :background "blue4")))
  "Face to draw frame's inner border."
  :group 'xwem-frame
  :group 'xwem-faces)

;;; Outline inner border
(defun xwem-frame-draw-inner-border (frame)
  "Draw inner border for FRAME."
  (let* ((bw (xwem-frame-property frame 'inner-border-width))
         (th (or (xwem-frame-property frame 'inner-border-thickness) 1))
         (tag1 (if (xwem-frame-selected-p frame) 'selected 'nonselected))
         (wgc (xwem-face-get-gc 'xwem-frame-inner-border-face
                (list 'light tag1) frame))
         (bgc (xwem-face-get-gc 'xwem-frame-inner-border-face
                (list 'dark tag1) frame))
         (gc (xwem-face-get-gc 'xwem-frame-inner-border-face
               (list 'medium tag1) frame))
         (off th))

    (when (> bw 0)
      (XDrawRectangles
       (xwem-dpy) (xwem-frame-xwin frame)
       gc (mapcar #'(lambda (notused)
                      (prog1
                          (make-X-Rect :x off :y off
                                       :width (- (xwem-frame-width frame)
                                                 (* 2 off) 1)
                                       :height (- (xwem-frame-height frame)
                                                  (* 2 off) 1))
                        (incf off)))
                  (make-list (- bw (* 2 th)) nil)))

      (xwem-misc-draw-shadow (xwem-dpy) (xwem-frame-xwin frame)
                             wgc bgc 0 0 (xwem-frame-width frame)
                             (xwem-frame-height frame) th)
      (xwem-misc-draw-shadow (xwem-dpy) (xwem-frame-xwin frame)
                             bgc wgc (- bw th) (- bw th)
                             (- (xwem-frame-width frame) (* 2 (- bw th)))
                             (- (xwem-frame-height frame) (* 2 (- bw th)))
                             th)
      )))

(define-xwem-deferred xwem-frame-deferred-redraw-inner-border (frame)
  "Redraw inner border for selected frame."
  (and (xwem-frame-p frame)
       (xwem-frame-draw-inner-border frame)))

;;; Showing mode
(defvar xwem-frame-showing-mode-hook nil
  "Hooks to call when entering/leaving showing mode.
Each hook called with two arguments: FRAME and MODE,
where mode is one of:

  `enter' - FRAME enters showing mode.
  `leave' - FRAME leaving showing mode.")

(defun xwem-frame-enter-showing-mode (frame)
  "Enable showing mode for FRAME."
  (unless (eq (xwem-frame-property frame 'showing-mode) 'on)
    (let ((fcls (xwem-frame-clients frame)))
      (mapc 'xwem-iconify fcls)

      (xwem-frame-put-prop frame 'showing-mode 'on)
      (run-hook-with-args 'xwem-frame-showing-mode-hook frame 'enter))))

(defun xwem-frame-leave-showing-mode (frame)
  "Disable showing mode."
  (unless (eq (xwem-frame-property frame 'showing-mode) 'off)
    (let ((fcls (xwem-frame-clients frame)))
      (mapcar 'xwem-manage fcls)

      (xwem-frame-put-prop frame 'showing-mode 'off)
      (run-hook-with-args 'xwem-frame-showing-mode-hook frame 'leave))))

;;; Frame properties
(defvar xwem-frame-supported-properties nil
  "*List of supported frame properties.")

(defmacro define-xwem-frame-property (prop doc &rest keys-val)
  "Define new frame property PROP.
DOC is doc-string for property and KEYS-VAL is a list of keyword-value
pairs.  Supported keywords are:

  :type   - Type of property (same as in `defcustom')
  :set    - Function to set property.
  :get    - Function to get property value.
  :notifiers - Functions to call when new value for property is set.
"
  `(progn
     (add-to-list 'xwem-frame-supported-properties (quote ,prop))
     (put (quote ,prop) 'xwem-property-definition (list :doc ,doc ,@keys-val))))

(defun xwem-frame-get-prop-keyword (prop keyword &optional default)
  "Return PROP's KEYWORD value.
Return DEFAULT is KEYWORD not found."
  (plist-get (get prop 'xwem-property-definition) keyword default))

;;;###xwem-autoload
(defun xwem-frame-add-property-notifier (prop notifier)
  (let ((notifiers (xwem-frame-get-prop-keyword prop :notifiers)))
    (if notifiers
        (setcdr (last notifiers) (cons notifier nil))
      (put prop 'xwem-property-definition
           (plist-put (get prop 'xwem-property-definition)
                      :notifiers (list notifier))))))

;;;###xwem-autoload
(defun xwem-frame-set-property (frame prop val)
  "Set FRAME's propertie PROP to VAL.
If FRAME is nil - selected frame is used."
  (unless frame
    (setq frame (xwem-frame-selected)))
  (unless (equal (xwem-frame-property frame prop) val)
    (funcall (xwem-frame-get-prop-keyword prop :set 'xwem-frame-put-prop)
             frame prop val)
    ;; Call notifiers
    (mapc #'(lambda (notifier) (funcall notifier frame prop val))
          (xwem-frame-get-prop-keyword prop :notifiers))))

;;;###xwem-autoload
(defun xwem-frame-set-properties (frame props-plist)
  "Set FRAME properties PROPS."
  (while props-plist
    (xwem-frame-set-property frame (car props-plist) (cadr props-plist))
    (setq props-plist (cddr props-plist))))

;;;###xwem-autoload
(defun xwem-frame-property (frame prop)
  "Return value for FRAME's property PROP.
If FRAME is nil - selected frame is used."
  (funcall (xwem-frame-get-prop-keyword prop :get 'xwem-frame-get-prop)
           (or frame (xwem-frame-selected)) prop))

;;;###xwem-autoload
(defun xwem-frame-properties (&optional frame)
  "Return a list of FRAME's properties.
List is copied, you are free to modify it for your needs.
Note that only supported properties are returned.
If FRAME is omitted - selected frame is used."
  (let ((fplist (xwem-frame-plist (or frame (xwem-frame-selected))))
        rplist)
    (while fplist
      (when (memq (car fplist) xwem-frame-supported-properties)
        (setq rplist (plist-put rplist (car fplist) (cadr fplist))))
      (setq fplist (cddr fplist)))
    rplist))

;; Define some basic properties
(defun xwem-frame-setup-root-win (frame)
  (let ((tl (xwem-frame-property frame 'title-layout))
        (th (xwem-frame-property frame 'title-height))
        (ibw (xwem-frame-property frame 'inner-border-width))
        (fw (xwem-frame-width frame))
        (fh (xwem-frame-height frame))
        (root-win (xwem-frame-rootwin frame))
        x y w h)
    (cond ((eq tl 'top)
           (setq x ibw
                 y (+ th ibw)
                 w (- fw ibw ibw)
                 h (- fh th ibw ibw)))
          ((eq tl 'bottom)
           (setq x ibw
                 y ibw
                 w (- fw ibw ibw)
                 h (- fh th ibw ibw)))
          ((eq tl 'left)
           (setq x (+ ibw th)
                 y ibw
                 w (- fw th ibw ibw)
                 h (- fh ibw ibw)))
          ((eq tl 'right)
           (setq x ibw
                 y ibw
                 w (- fw th ibw ibw)
                 h (- fh ibw ibw)))

          ((eq tl 'none)
           (setq x ibw
                 y ibw
                 w (- fw ibw ibw)
                 h (- fh ibw ibw))))
    (setf (xwem-win-x root-win) x
          (xwem-win-y root-win) y)
    (xwem-win-set-width root-win w)
    (xwem-win-set-height root-win h)))

(defun xwem-frame-set-title-height (frame prop new-height)
  "Set FRAME's title height property PROP to NEW-HEIGHT."
  (let ((old-height (xwem-frame-property frame prop)))
    (xwem-frame-put-prop frame prop new-height)

    ;; Adjust root window position/size
    (when (and new-height old-height (xwem-frame-rootwin frame))
      (incf (xwem-win-y (xwem-frame-rootwin frame))
            (- new-height old-height))
      (xwem-frame-setup-root-win frame))))

(defun xwem-frame-get-title-height (frame prop)
  "Return FRAME's title height."
  (or (xwem-frame-get-prop frame prop)
      0))

(define-xwem-frame-property title-height
  "Frame's title height in pixels."
  :type 'number
  :set 'xwem-frame-set-title-height
  :get 'xwem-frame-get-title-height)

(defcustom xwem-frame-default-title-layout 'top
  "*Default FRAME's title layout."
  :type '(choice (const :tag "No title" none)
                 (const :tag "Top" top)
                 (const :tag "Bottom" bottom)
                 (const :tag "Left" left)
                 (const :tag "Right" right))
  :group 'xwem-frame)

(defun xwem-frame-get-title-layout (frame prop)
  "Return FRAME's title layout."
  (or (xwem-frame-get-prop frame prop)
      xwem-frame-default-title-layout))

(defun xwem-frame-set-title-layout (frame prop new-layout)
  "Set FRAME's title layout property PROP to NEW-LAYOUT."
  (unless (memq new-layout '(left right top bottom none))
    (error 'xwem-error "Invalid title layout %S" new-layout))

  (xwem-frame-put-prop frame prop new-layout)
  (xwem-frame-setup-root-win frame))

(define-xwem-frame-property title-layout
  "Frame's title layout property."
  :type '(choice (const :tag "Top" top)
                 (const :tag "Bottom" bottom)
                 (const :tag "Left" left)
                 (const :tag "Right" right)
                 (const :tag "No Title" none))
  :get 'xwem-frame-get-title-layout
  :set 'xwem-frame-set-title-layout)

(defun xwem-frame-get-outer-border-width (frame prop)
  "Return FRAME's outer border width."
  (X-Geom-border-width (xwem-frame-xgeom  frame)))

(defun xwem-frame-set-outer-border-width (frame prop val)
  "Return FRAME's outer border width."
  (when (numberp val)
    (setf (X-Geom-border-width (xwem-frame-xgeom  frame)) val)
    (XSetWindowBorderWidth (xwem-dpy) (xwem-frame-xwin frame)
                           (X-Geom-border-width (xwem-frame-xgeom  frame)))))

(define-xwem-frame-property outer-border-width
  "Frame's outer border width in pixels."
  :type 'number
  :get 'xwem-frame-get-outer-border-width
  :set 'xwem-frame-set-outer-border-width)

(defun xwem-frame-set-outer-border-color (frame prop val)
  "Set FRAME's outer border color to VAL."
  (xwem-frame-put-prop frame prop val)

  (when val
    (XSetWindowBorder (xwem-dpy) (xwem-frame-xwin frame)
                      (XAllocColor (xwem-dpy) (XDefaultColormap (xwem-dpy))
                                   (xwem-make-color val)))))

(define-xwem-frame-property outer-border-color
  "Frame's outer border color."
  :type 'color
  :set 'xwem-frame-set-outer-border-color)

(defun xwem-frame-set-inner-border-width (frame prop val)
  "Set FRAME's inner border property PROP to VAL."
  (xwem-frame-put-prop frame prop val)

  ;; Setup windows
  (when (xwem-frame-rootwin frame)
    (xwem-frame-setup-root-win frame)
    (xwem-deferred-funcall 'xwem-frame-draw frame t)))

(defun xwem-frame-get-inner-border-width (frame prop)
  (or (xwem-frame-get-prop frame 'inner-border-width)
      0))

(define-xwem-frame-property inner-border-width
  "Frame's inner border width in pixels."
  :type 'number
  :get 'xwem-frame-get-inner-border-width
  :set 'xwem-frame-set-inner-border-width)

(defun xwem-frame-set-inner-border-thickness (frame prop val)
  "Set FRAME's inner border width PROP to VAL."
  (when (and val (> (* val 2) (xwem-frame-property frame 'inner-border-width)))
    (error 'xwem-error "To large thickness to fill in title-height"))

  (xwem-frame-put-prop frame prop val)
  (when val
    (xwem-deferred-funcall 'xwem-frame-draw frame nil)))

(define-xwem-frame-property inner-border-thickness
  "Frame's inner border thickness."
  :type 'number
  :set 'xwem-frame-set-inner-border-thickness)

(defun xwem-frame-set-background (frame prop val)
  "Set FRAME's background color to VAL."
  (xwem-frame-put-prop frame prop val)

  (when val
    (XSetWindowBackground (xwem-dpy) (xwem-frame-xwin frame)
                          (XAllocColor (xwem-dpy) (XDefaultColormap (xwem-dpy))
                                       (xwem-make-color val)))
    (xwem-deferred-funcall 'xwem-frame-draw frame t)))

(define-xwem-frame-property background
  "Frame's background color."
  :type 'color
  :set 'xwem-frame-set-background)

(defun xwem-frame-set-showing-mode (frame prop val)
  "Set FRAME's showing mode PROP to VAL."
  (xwem-frame-put-prop frame prop val)
  (if val
      (xwem-frame-enter-showing-mode frame)
    (xwem-frame-leave-showing-mode frame)))

(define-xwem-frame-property showing-mode
  "Frame's showing mode."
  :type 'boolean
  :set 'xwem-frame-set-showing-mode)


(provide 'xwem-frame)

;;; xwem-frame.el ends here
