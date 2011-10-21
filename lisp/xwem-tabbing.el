;;; xwem-tabbing.el --- Tabs in XWEM frames.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sun Dec  7 18:35:15 MSK 2003
;; Keywords: xwem, xlib
;; Time-stamp: <29/11/2008 04:30:53 lg@h1.lan>

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

;; Tab format may contain one of escape seqs:
;;   %n - Client's name (WM_NAME)
;;   %c - Client's class instance (WM_CLASS)
;;   %C - Client's class name (WM_CLASS)
;;   %i - Client's icon
;;   %u - Client's uptime
;;   %U - Cilent's Uptime
;;   %s - Client's size in pixels
;;   %S - Client's size in units
;;   %f - Client's Frame number
;;   %F - Client's Frame name
;;   %r - Client's register (if any)
;;   %* - "*" when client marked and "-" when not.
;;   %# - "#" when client support WM_DELETE and "-" when not.
;;   %I - Input model:
;;           "-"  - No Input
;;           "P"  - Passive
;;           "L"  - Locally Active
;;           "G"  - Globally Active

;;   %{ - starts emacs lisp
;;   %} - ends emacs lisp

;;   %0 - begin using default face
;;   %[1-9] - start using additional `xwem-tabber-face[num]' defined
;;            using `define-xwem-face'.

;; Note that while running elisp within %{ and %} symbol `cl' refers
;; to client.

;; Also `X-use-queryfont' is highly recommended to be `t' if you are
;; using this file.


;; Supported clients properties:

;;    `xwem-tab-format' - Tab format for certain client.

;;; Code:

(require 'xlib-xlib)
(require 'xwem-load)

;;; Customisation
(defgroup xwem-tab nil
  "Group to customize tabs."
  :prefix "xwem-tab-"
  :prefix "xwem-face-"
  :group 'xwem)

(defcustom xwem-tab-title-type 'single
  "Type of tabbing."
  :type '(choice (const :tag "Single title" single)
                 (const :tag "Multiple plain titles" multiple)
                 (const :tag "Multiple cascade titles" cascade))
  :group 'xwem-tab)

(defcustom xwem-tab-title-placement 'center
  "Placement of text in tab."
  :type '(choice (const :tag "Center" center)
                 (const :tag "Right" right)
                 (const :tag "Left" left))
  :group 'xwem-tab)

(defcustom xwem-tab-default-format " %i %*%#%1%r%0 %n"
  "*Default format for tab item."
  :type 'string
  :group 'xwem-tab)

(defcustom xwem-tab-empty-format "<empty>"
  "What to show when there no client."
  :type 'string
  :group 'xwem-tab)

(defcustom xwem-tab-delim-interval 2
  "*Number of clients to group, will draw largeer delimeter."
  :type 'number
  :group 'xwem-tab)

(defcustom xwem-tab-show-cl-info-on-click t
  "*Non-nil mean show client info when `xwem-tabber-switch-cl' called."
  :type 'boolean
  :group 'xwem-tab)

(define-xwem-face xwem-tabber-face
  `(((frame-selected tab-selected)
     (:foreground "white" :background "green4" :size 16 :bold t))
    ((delimiter-left frame-selected tab-selected)
     (:foreground "white" :size 16))
    ((delimiter-right frame-selected tab-selected)
     (:foreground "black" :size 16))

    ((frame-selected tab-nonselected)
     (:foreground "black" :background "gray80" :size 16))
    ((delimiter-left frame-selected tab-nonselected)
     (:foreground "white" :size 16))
    ((delimiter-right frame-selected tab-nonselected)
     (:foreground "black" :size 16))

    ((frame-nonselected tab-selected)
     (:foreground "gray80" :background "DarkGreen" :size 16 :bold t))
    ((delimiter-left frame-nonselected tab-selected)
     (:foreground "white" :size 16))
    ((delimiter-right frame-nonselected tab-selected)
     (:foreground "black" :size 16))

    ((frame-nonselected tab-nonselected)
     (:foreground "black" :background "gray40" :size 16))
    ((delimiter-left frame-nonselected tab-nonselected)
     (:foreground "white" :size 16))
    ((delimiter-right frame-nonselected tab-nonselected)
     (:foreground "black" :size 16))

    (t (:foreground "white" :size 16)))
  "Face to draw tabs."
  :group 'xwem-tab
  :group 'xwem-faces)

(define-xwem-face xwem-tabber-face1
  `(((frame-selected tab-selected)
     (:foreground "red" :bold t :size 16))
    ((frame-selected tab-nonselected)
     (:foreground "red2" :size 16))
    ((frame-nonselected tab-selected)
     (:foreground "red3" :bold t :size 16))
    ((frame-nonselected tab-nonselected)
     (:foreground "red4" :size 16))
    (t (:foreground "red" :size 16)))
  "Additional face for tabber.")

;; Another interface to customize tabber fonts
(defcustom xwem-tabber-font:frame.selected-tab.selected nil
  "Font to be used in selected tab of selected frame."
  :type '(restricted-sexp :match-alternatives (nil try-font-name))
  :set (lambda (sym val)
         (set sym val)
         (xwem-set-face-font
          'xwem-tabber-face
          (or val (xwem-face-font 'xwem-tabber-face '(default)))
          '(frame-selected tab-selected)))
  :initialize 'custom-initialize-default
  :group 'xwem-tab)

(defcustom xwem-tabber-font:frame.selected-tab.nonselected nil
  "Font to be used in selected tab of selected frame."
  :type '(restricted-sexp :match-alternatives (nil try-font-name))
  :set (lambda (sym val)
         (set sym val)
         (xwem-set-face-font
          'xwem-tabber-face
          (or val (xwem-face-font 'xwem-tabber-face '(default)))
          '(frame-selected tab-nonselected)))
  :initialize 'custom-initialize-default
  :group 'xwem-tab)

(defcustom xwem-tabber-font:frame.nonselected-tab.selected nil
  "Font to be used in selected tab of selected frame."
  :type '(restricted-sexp :match-alternatives (nil try-font-name))
  :set (lambda (sym val)
         (set sym val)
         (xwem-set-face-font
          'xwem-tabber-face
          (or val (xwem-face-font 'xwem-tabber-face '(default)))
          '(frame-nonselected tab-selected)))
  :initialize 'custom-initialize-default
  :group 'xwem-tab)

(defcustom xwem-tabber-font:frame.nonselected-tab.nonselected nil
  "Font to be used in selected tab of selected frame."
  :type '(restricted-sexp :match-alternatives (nil try-font-name))
  :set (lambda (sym val)
         (set sym val)
         (xwem-set-face-font
          'xwem-tabber-face
          (or val (xwem-face-font 'xwem-tabber-face '(default)))
          '(frame-nonselected tab-nonselected)))
  :initialize 'custom-initialize-default
  :group 'xwem-tab)

;;; Internal variables

(defvar xwem-tabber-map
  (let ((map (make-sparse-keymap)))
    (define-key map [button1] 'xwem-tabber-smart-drag-frame)
    (define-key map [button1up] 'xwem-tabber-switch-cl)
    (define-key map [button3] 'xwem-tabber-popup-cl-menu)
    map)
  "Keymap used when accessing `xwem-tabber'.")

(defvar xwem-tabber-dedicated-map
  (let ((map (make-sparse-keymap)))
    (define-key map [button1] 'xwem-tabber-drag-frame)
    (define-key map [button3] 'xwem-tabber-popup-cl-menu)
    map)
  "Keymap for dedicated clients.")

;;;###autoload
(defvar xwem-tabber-click-frame nil
  "Will be binded to frame when tabber clicked.")
;;;###autoload
(defvar xwem-tabber-click-cl nil
  "Will be binded to cl when tabber clicked.")


;;; Margins
;; Margin is some area in tabber, which is drawed specially.
;; Margin is a list in form (rect format face)

;;; Tabber
(defstruct (xwem-tabber
            (:print-function
             (lambda (tb s pl)
               (princ (format "#<xwem-tabber xgeom=%S margins=%S>"
                              (xwem-tabber-xgeom tb)
                              (xwem-tabber-margins tb)) s))))
  frame                                 ; xwem-frame, our parent
  xgeom                                 ; Tabber's geometry
  clients                               ; clients list

  xwin                                  ; Tabber's X-Win
  xpreparer                             ; Tabber's X-Pixmap to prepare tab items
  xpix-copy                             ; Tabber's X-Pixmap used to copy to xwin

  ;; Margins
  margins                               ; list of margins

  plist                                 ; props list
  )

;;; Macros
(defmacro xwem-frame-tabber (frame)
  `(xwem-frame-get-prop ,frame 'xwem-tabber))
(defsetf xwem-frame-tabber (frame) (tabber)
  `(xwem-frame-put-prop ,frame 'xwem-tabber ,tabber))

(defmacro xwem-cl-tabber (cl)
  `(let ((win (xwem-cl-win ,cl)))
     (and win (xwem-frame-tabber (xwem-win-frame win)))))

(defmacro xwem-cl-tab-rect (cl)
  `(xwem-cl-get-sys-prop ,cl 'xwem-tab-rect))
(defsetf xwem-cl-tab-rect (cl) (tab-rect)
  `(xwem-cl-put-sys-prop ,cl 'xwem-tab-rect ,tab-rect))

(defmacro xwem-cl-tab-face (cl)
  `(or (and cl (xwem-client-property ,cl 'xwem-tab-face))
       'xwem-tabber-face))
(defsetf xwem-cl-tab-face (cl) (tab-face)
  `(xwem-client-set-property ,cl 'xwem-tab-face ,tab-face))

(define-xwem-client-property xwem-tab-format nil
  "Tab format."
  :type 'string
  :get 'xwem-tab-get-xwem-tab-format
  :set 'xwem-tab-set-xwem-tab-format)

(defun xwem-tab-get-xwem-tab-format (cl tprop)
  "Get tab format for CL."
  (or (xwem-cl-get-prop cl 'xwem-tab-format)
      xwem-tab-default-format))

(defun xwem-tab-set-xwem-tab-format (cl tprop tval)
  "Set CL's tab format property TPROP to TVAL."
  (xwem-cl-put-prop cl tprop tval)
  (xwem-tabber-on-cl-change cl))


;;; Functions

(defsubst xwem-tabber-put-prop (tabber prop val)
  (setf (xwem-tabber-plist tabber)
        (plist-put (xwem-tabber-plist tabber) prop val)))
(put 'xwem-tabber-put-prop 'lisp-indent-function 2)

(defsubst xwem-tabber-get-prop (tabber prop)
  (plist-get (xwem-tabber-plist tabber) prop))

(defsubst xwem-tabber-rm-prop (tabber prop)
  (setf (xwem-tabber-plist tabber)
        (plist-remprop (xwem-tabber-plist tabber) prop)))

(defun xwem-tabber-init ()
  "Initialize tabbing."
  (xwem-message 'init "Initializing tabbing ...")

  ;; Frame hooks
  (add-hook 'xwem-frame-creation-hook 'xwem-tabber-on-frame-creation)
  (add-hook 'xwem-frame-resize-hook 'xwem-tabber-on-frame-resize)
  (add-hook 'xwem-frame-select-hook 'xwem-tabber-on-frame-select-deselect)
  (add-hook 'xwem-frame-deselect-hook 'xwem-tabber-on-frame-select-deselect)

  ;; Frame properties notifier
  (xwem-frame-add-property-notifier 'title-height 'xwem-tabber-frame-prop-notifier)
  (xwem-frame-add-property-notifier 'title-layout 'xwem-tabber-frame-prop-notifier)
  (xwem-frame-add-property-notifier 'inner-border-width 'xwem-tabber-frame-prop-notifier)

  ;; Window hooks
  (add-hook 'xwem-win-switch-hook 'xwem-tabber-on-win-switch)
  (add-hook 'xwem-win-clients-change-hook 'xwem-tabber-on-win-change)

  ;; Client hooks
  (add-hook 'xwem-cl-create-hook 'xwem-tabber-on-cl-creation)
  (add-hook 'xwem-cl-state-change-hook 'xwem-tabber-on-cl-change)
  (add-hook 'xwem-cl-change-hook 'xwem-tabber-on-cl-change)

  (xwem-message 'init "Initializing tabbing ... done"))

;;;###autoload(autoload 'xwem-tabber-switch-cl "xwem-tabber" "" t)
(define-xwem-command xwem-tabber-switch-cl ()
  "Switch to client which tab item was clicked."
  (xwem-interactive)

  (when (xwem-cl-alive-p xwem-tabber-click-cl)
    (xwem-activate xwem-tabber-click-cl)
    ;; Select client only if switching in current window.
    (when (eq (xwem-cl-win xwem-tabber-click-cl) (xwem-win-selected))
      (xwem-select-client xwem-tabber-click-cl))

    (when xwem-tab-show-cl-info-on-click
      (xwem-client-info xwem-tabber-click-cl))
    ))

(define-xwem-command xwem-tabber-drag-frame ()
  "Interactively move frame."
  (xwem-interactive)

  (xwem-frame-imove-internal
   xwem-tabber-click-frame
   (X-Event-xbutton-root-x xwem-last-xevent)
   (X-Event-xbutton-root-y xwem-last-xevent)))

(define-xwem-command xwem-tabber-smart-drag-frame ()
  "Interactively move dedicated client.
Move frame.  If no moving occured and button is released, bypass it as
command event."
  (xwem-interactive)

  (let ((xev (xwem-next-event nil (list X-ButtonRelease X-MotionNotify))))
    (X-Event-CASE xev
      (:X-MotionNotify
       (xwem-frame-imove-internal
        xwem-tabber-click-frame
        (X-Event-xmotion-root-x xev)
        (X-Event-xmotion-root-y xev)))
      (:X-ButtonRelease
       (xwem-dispatch-command-xevent xev)))))
(put 'xwem-tabber-smart-drag-frame 'xwem-frame-command t)

;;;###autoload(autoload 'xwem-tabber-popup-cl-menu "xwem-tabber" "" t)
(define-xwem-command xwem-tabber-popup-cl-menu ()
  "Popup clients menu."
  (xwem-interactive)

  (if (xwem-cl-alive-p xwem-tabber-click-cl)
      (xwem-popup-menu (xwem-generate-cl-menu xwem-tabber-click-cl))
    ;; TODO
    (let ((menu nil))
      (when xwem-cl-mark-ring
        (setq menu
              (list
               (vector (format "Attach %s"
                               (xwem-client-name (car xwem-cl-mark-ring)))
                       `(xwem-win-set-cl ,(xwem-frame-selwin
                                           (or xwem-tabber-click-frame
                                               (xwem-frame-selected)))
                                         ,(car xwem-cl-mark-ring)))
               (vector (format "Attach (unmark) %s"
                               (xwem-client-name (car xwem-cl-mark-ring)))
                       `(progn
                          (xwem-win-set-cl ,(xwem-frame-selwin
                                             (or xwem-tabber-click-frame
                                                 (xwem-frame-selected)))
                                           ,(car xwem-cl-mark-ring))
                          (xwem-client-unset-mark ,(car xwem-cl-mark-ring)))))))
      (when menu
        (xwem-popup-menu
         (cons (format "Frame %d: %s"
                       (xwem-frame-num (or xwem-tabber-click-frame
                                           (xwem-frame-selected)))
                       (xwem-frame-name (or xwem-tabber-click-frame
                                            (xwem-frame-selected))))
               menu))))
    ))

(defun xwem-tabber-cl-at (tabber x y)
  "Return client of TABBER which rectangle covers point at X Y."
  (let ((clients (xwem-tabber-clients tabber))
        (tmp-rect (xwem-tabber-rect->xpix-rect
                   tabber (make-X-Rect :x x :y y :width 0 :height 0)))
        rect ret-cl)
    ;; Adjust X and Y
    (setq x (X-Rect-x tmp-rect)
          y (X-Rect-y tmp-rect))
    (while clients
      (setq rect (xwem-cl-tab-rect (car clients)))
      (if (and rect
                 (<= (X-Rect-x rect) x)
                 (>= (+ (X-Rect-x rect) (X-Rect-width rect)) x)
                 (<= (X-Rect-y rect) y)
                 (>= (+ (X-Rect-y rect) (X-Rect-height rect)) y))
        (setq ret-cl (car clients)
              clients nil)
        (setq clients (cdr clients))))
    ret-cl))

(defun xwem-tabber-regeom (tabber)
  "Adjust tab items geometries in TABBER."
  ;; TODO: handle margins
  (let* ((twidth (X-Geom-width (xwem-tabber-xgeom tabber)))
         (theight (X-Geom-height (xwem-tabber-xgeom tabber)))
         (clients (xwem-tabber-clients tabber))
         (clsn (length clients ))       ; number of clients
         (off 0)
         tiw twrem rect)

    (when clients
      (setq tiw (/ twidth clsn)
            twrem (% twidth clsn))

      (while clients
        ;; Setup CL's tab rectangle rectangle
        (unless (xwem-cl-tab-rect (car clients))
          (setf (xwem-cl-tab-rect (car clients))
                (make-X-Rect :x 0 :y 0 :width 0 :height 0)))
        (setq rect (xwem-cl-tab-rect (car clients)))
        (setf (X-Rect-x rect) off)
        (setf (X-Rect-width rect) (+ tiw (if (cdr clients) 0 twrem)))
        (setf (X-Rect-height rect) theight)

        (setq off (+ off (X-Rect-width rect)))
        (setq clients (cdr clients))))))

(defsubst xwem-tabber-safe-regeom (tabber &optional draw-p)
  "Saf variant of `xwem-tabber-regeom'."
  (and (xwem-tabber-p tabber)
       (xwem-tabber-regeom tabber)))

;; Drawings
(defun xwem-tabber-xpix-rect->rect (tabber rect)
  (let ((x0 (X-Rect-x rect))
        (y0 (X-Rect-y rect))
        (w0 (X-Rect-width rect))
        (h0 (X-Rect-height rect))
        x y w h)
    (case (xwem-frame-property (xwem-tabber-frame tabber) 'title-layout)
      (left
       (setq y x0
             x (- (X-Geom-width (xwem-tabber-xgeom tabber)) y0 h0)
             h w0
             w h0))
      (right
       (setq x y0
             y (- (X-Geom-height (xwem-tabber-xgeom tabber)) x0 w0)
             w h0
             h w0))
      (t (setq x x0
               y y0
               w w0
               h h0)))
    (make-X-Rect :x x :y y :width w :height h)))

(defun xwem-tabber-rect->xpix-rect (tabber rect)
  (xwem-tabber-xpix-rect->rect tabber rect))

(define-xwem-deferred xwem-tabber-redraw (tabber &optional  x y width height)
  "Redraw TABBER's rectangle specified by X, Y, WIDTH and HEIGHT.
If one of optional arguments ommited, full redraw."
  (when (xwem-tabber-p tabber)
    (let* ((xgeom (xwem-tabber-xgeom tabber))
           (x (or x 0))
           (y (or y 0))
           (width (or width (X-Geom-width xgeom)))
           (height (or height (X-Geom-height xgeom)))
           (tl (xwem-frame-property (xwem-tabber-frame tabber) 'title-layout)))
      (cond ((memq tl '(top bottom))
             (XCopyArea (xwem-dpy) (xwem-tabber-xpix-copy tabber)
                        (xwem-tabber-xwin tabber)
                        (XDefaultGC (xwem-dpy)) x y width height x y))
            ((memq tl '(left right))
             (let* ((ximg (XGetImage (xwem-dpy) (xwem-tabber-xpix-copy tabber)
                                     x y width height X-AllPlanes X-ZPixmap))
                    (rxd (xwem-misc-rotate-data
                          (nth 4 ximg) width height
                          (XGetDepth (xwem-dpy) (xwem-tabber-xpix-copy tabber))
                          (if (eq tl 'left) 'left 'right)))
                    (dst-height width)
                    (dst-width height)
                    dst-x dst-y)
               (if (eq tl 'left)
                   (setq dst-x y
                         dst-y (- (X-Geom-width xgeom) x width))
                 (setq dst-x  (- (X-Geom-height xgeom) y height)
                       dst-y x))
               (XPutImage (xwem-dpy) (xwem-tabber-xwin tabber)
                          (XDefaultGC (xwem-dpy))
                          (XGetDepth (xwem-dpy) (xwem-tabber-xpix-copy tabber))
                          dst-width dst-height
                          dst-x dst-y nil X-ZPixmap rxd)))))))

(define-xwem-deferred xwem-tabber-redraw-xrect (tabber &optional xrect)
  "Redraw part of TABBER.
XRECT specifies geometry to redraw.
Defaultly full redraw."
  (when (xwem-tabber-p tabber)
    (unless xrect
      (setq xrect (X-Geom-to-X-Rect (xwem-tabber-xgeom tabber))))

    (xwem-tabber-redraw tabber
                        (X-Rect-x xrect) (X-Rect-y xrect)
                        (X-Rect-width xrect) (X-Rect-height xrect))))

(defsubst xwem-tabber-frame-win-clients (tabber)
  "Return list of managed clients in TABBER's window."
  (delq nil (mapcar #'(lambda (cl)
                        (and (xwem-cl-managed-p cl '(active inactive)) cl))
                    (xwem-win-clients
                     (xwem-frame-selwin (xwem-tabber-frame tabber))))))

(defsubst xwem-tabber-clients-equal (cls1 cls2)
  "Return non-nil if each element of CLS1 and CLS2 is `eq'."
  (and (= (length cls1) (length cls2))
       (not (memq nil (or (mapcar* 'eq cls1 cls2))))))

(define-xwem-deferred xwem-tabber-draw-format (cl &optional tabber force-update)
  "Draw CL's tab.
If FORCE-UPDATE is non-nil also copy to TABBER x window."
  (when (or (null cl) (xwem-cl-alive-p cl))
    ;; Either empty or valid client
    (let* ((tabber (or tabber (xwem-cl-tabber cl)))
           (rect (or (and (xwem-cl-p cl) (xwem-cl-tab-rect cl))
                     (let ((xgeom (xwem-tabber-xgeom tabber)))
                       (make-X-Rect :x 0 :y 0 :width (X-Geom-width xgeom)
                                    :height (X-Geom-height xgeom)))))
           (fmt (or (and (xwem-cl-p cl)
                         (xwem-client-property cl 'xwem-tab-format))
                    xwem-tab-empty-format))
           (xprep (xwem-tabber-xpreparer tabber))
           (xpcop (xwem-tabber-xpix-copy tabber))
           (xoff (X-Rect-x rect))
           (yoff (X-Rect-y rect))
           tag-set currgc fi item fmt-index sfg)

      ;; Setup TAG-SET
      (if (xwem-frame-selected-p (xwem-tabber-frame tabber))
          (if (xwem-cl-p cl)
              (if (xwem-win-cl-current-p cl)
                  (setq tag-set (list 'frame-selected 'tab-selected))
                (setq tag-set (list 'frame-selected 'tab-nonselected)))

            ;; Empty tab item
            (setq tag-set (list 'frame-selected 'tab-selected)))

        (if (xwem-cl-p cl)
            (if (xwem-win-cl-current-p cl)
                (setq tag-set (list 'frame-nonselected 'tab-selected))
              (setq tag-set (list 'frame-nonselected 'tab-nonselected)))

          ;; Empty tab item
          (setq tag-set (list 'frame-nonselected 'tab-selected))))

      ;; Setup currgc, xprep, tabxwin
      (setq currgc (xwem-face-get-gc (xwem-cl-tab-face cl) tag-set cl))
      (XSetClipRectangles (xwem-dpy) currgc 0 0 (list rect))

      (setq sfg (X-Gc-foreground currgc))
      (xwem-unwind-protect
          (progn
            (XChangeGC (xwem-dpy) currgc :foreground (X-Gc-background currgc))
            (XFillRectangles (xwem-dpy) xprep currgc (list rect))
            (XFillRectangles (xwem-dpy) xpcop currgc (list rect)))
        (XChangeGC (xwem-dpy) currgc :foreground sfg))

      ;; Process format string
      (setq fmt-index 0)
      (while (and (< xoff (+ (X-Rect-x rect) (X-Rect-width rect)))
                  (< fmt-index (length fmt)))

        ;; Extract ITEM
        (setq fi (aref fmt fmt-index))
        (incf fmt-index)
        (if (eq fi ?%)
            (progn
              (setq fi (aref fmt fmt-index))
              (setq item
                    (cond ((= fi ?n) (xwem-client-name cl))
                          ((= fi ?c) (car (xwem-hints-wm-class
                                           (xwem-cl-hints cl))))
                          ((= fi ?C) (cdr (xwem-hints-wm-class
                                           (xwem-cl-hints cl))))
                          ((= fi ?i) (xwem-icons-cl-icon
                                      cl (and (not (xwem-frame-selected-p
                                                    (xwem-cl-frame cl)))
                                              '(shade))))
                          ((= fi ?s) (xwem-cl-get-psize cl))
                          ((= fi ?S) (xwem-cl-get-usize cl))
                          ((= fi ?u) (xwem-cl-get-uptime cl))
                          ((= fi ?U) (xwem-cl-get-uptime cl))
                          ((= fi ?f) (int-to-string
                                      (xwem-frame-num (xwem-cl-frame cl))))
                          ((= fi ?F) (xwem-frame-name (xwem-cl-frame cl)))
                          ((= fi ?r) (let ((rs (xwem-client-property cl 'register)))
                                       (and rs (char-to-string rs))))
                          ((= fi ?*) (if (xwem-cl-marked-p cl) "*" "-"))
                          ((= fi ?#) (if (XWMProtocol-set-p
                                          (xwem-dpy)
                                          (xwem-hints-wm-protocols
                                           (xwem-cl-hints cl))
                                          "WM_DELETE_WINDOW")
                                         "#" "-"))
                          ((= fi ?I) (let ((ip (and (X-WMHints-input-p
                                                     (xwem-hints-wm-hints
                                                      (xwem-cl-hints cl)))
                                                    (= (X-WMHints-input
                                                        (xwem-hints-wm-hints
                                                         (xwem-cl-hints cl))) 1)))
                                           (tf (XWMProtocol-set-p
                                                (xwem-dpy)
                                                (xwem-hints-wm-protocols
                                                 (xwem-cl-hints cl))
                                                "WM_TAKE_FOCUS")))
                                       (cond ((and ip tf) "L")
                                             (ip "P")
                                             (tf "G")
                                             (t "-"))))
                          ((= fi ?%) "%")

                          ;; Emacs lisp
                          ((= fi ?{)
                           (let ((substr (substring fmt (1+ fmt-index)))
                                 elstr)
                             (unless (string-match
                                      "\\(\\([^%]\\|%[^}]\\)*\\)%}" substr)
                               (signal 'search-failed fmt "%}"))

                             ;; extract lisp code and update fmt indexer
                             (setq elstr (match-string 1 substr))
                             (incf fmt-index (match-end 0))

                             ;; Now time to run emacs lisp.

                             ;; NOTE:
                             ;;
                             ;;  - Due to dynamic scoping, emacs
                             ;;    lisp code that is in ELSTR can
                             ;;    access any locally bounded
                             ;;    variable for example `cl'.
                             ;;
                             ;; - It should return string, cons
                             ;;   cell(image) or nil.
                             (eval (read elstr))))

                          ;; Ditig is number of aditional GC
                          ((and (> (char-to-int fi) 47) (< (char-to-int fi) 57))
                           (let* ((n (string-to-int (char-to-string fi)))
                                  (gc (xwem-face-get-gc
                                          (if (zerop n)
                                              (xwem-cl-tab-face cl)
                                            (intern-soft
                                             (concat "xwem-tabber-face"
                                                     (int-to-string n))))
                                        tag-set cl)))
                             (when (X-Gc-p gc)
                               (setq currgc gc)
                               (XSetClipRectangles (xwem-dpy) currgc
                                                   0 0 (list rect))))
                           'skip)

                          (t (error 'xwem-error "Unknown token in tabi format"))))
              ;; size fix
              (when (and (consp item)
                         (numberp (car item))
                         (numberp (cdr item)))
                (setq item (concat (int-to-string (car item))
                                   "x"
                                   (int-to-string (cdr item)))))
              (incf fmt-index))

          ;; Not %
          (setq item (char-to-string fi))
          (while (and (< fmt-index (length fmt))
                      (not (= (aref fmt fmt-index) ?%)))
            (setq item (concat item (char-to-string (aref fmt fmt-index))))
            (incf fmt-index)))

        ;; Display ITEM
        (cond ((stringp item)
                (let* ((font (X-Gc-font currgc))
                       (ta (X-Text-ascent (xwem-dpy) font item))
                       (td (X-Text-descent (xwem-dpy) font item))
                       (hei (X-Rect-height rect))
                       (ty (+ yoff (/ (- hei (+ ta td)) 2) ta)))

                  ;; XXX Encode ITEM to proper encoding
                  ;; We use `xwem-cl-tab-face', but what if GC is
                  ;; changed with custom gc
                  (let ((fenc (xwem-misc-font-coding-system
                               (X-Font-name (xwem-dpy) font))))
                    (when (find-coding-system fenc)
                      (setq item (encode-coding-string item fenc))))

                  (XDrawString (xwem-dpy) xprep currgc xoff ty item)
                  (setq xoff (+ xoff (X-Text-width
                                      (xwem-dpy) (X-Gc-font currgc) item)))
                  ))

              ((and (consp item)
                    (X-Pixmap-p (car item))
                    (X-Pixmap-p (cdr item)))

               ;; Draw icon
               (let ((ty (/ (- (X-Rect-height rect)
                               (X-Pixmap-height (car item))) 2)))
;                  (ximg-mask (X-Pixmap-get-prop (cdr item) 'ximg)))
                 (xwem-unwind-protect
                     (progn
                       (XChangeGC (xwem-dpy) currgc
                                  :clip-mask (cdr item)
                                  :clip-x-origin xoff
                                  :clip-y-origin (+ yoff ty))
                       (XCopyArea (xwem-dpy) (car item) xprep currgc 0 0
                                  (X-Pixmap-width (car item))
                                  (X-Pixmap-height (car item))
                                  xoff (+ yoff ty)))
                   (XChangeGC (xwem-dpy) currgc
                              :clip-mask X-None
                              :clip-x-origin 0
                              :clip-y-origin 0)
                   (XSetClipRectangles (xwem-dpy) currgc 0 0 (list rect)))

                 (setq xoff (+ xoff (X-Pixmap-width (car item))))))

              ((or (null item)
                   (eq item 'skip)) nil)

              (t (error 'xwem-error "Unknown Item" item)))
        )                               ;  while

      ;; Compose xpreparer
      (when (> xoff (+ (X-Rect-x rect) (X-Rect-width rect)))
        (setq xoff (+ (X-Rect-x rect) (X-Rect-width rect))))

      (XCopyArea (xwem-dpy) xprep xpcop currgc
                 (X-Rect-x rect) (X-Rect-y rect)
                 (X-Rect-width rect) (X-Rect-height rect)
                 (+ (X-Rect-x rect)
                    (ecase xwem-tab-title-placement
                      (left 0)
                      (center
                       (/ (- (+ (X-Rect-x rect)
                                (X-Rect-width rect)) xoff) 2))
                      (right (- (+ (X-Rect-x rect)
                                   (X-Rect-width rect)) xoff))))
                 (X-Rect-y rect))

      ;; Draw tab outliner
      (let ((ldgc (xwem-face-get-gc (xwem-cl-tab-face cl)
                    (cons 'delimiter-left tag-set) cl))
            (rdgc (xwem-face-get-gc (xwem-cl-tab-face cl)
                    (cons 'delimiter-right tag-set) cl)))
        (XDrawSegments (xwem-dpy) xpcop ldgc
                       (list (cons (cons (X-Rect-x rect) (X-Rect-y rect))
                                   (cons (X-Rect-x rect) (1- (+ (X-Rect-y rect) (X-Rect-height rect)))))
                             (cons (cons (X-Rect-x rect) (X-Rect-y rect))
                                   (cons (- (+ (X-Rect-x rect) (X-Rect-width rect)) 1) (X-Rect-y rect)))))
        (XDrawSegments (xwem-dpy) xpcop rdgc
                       (list (cons (cons (X-Rect-x rect)
                                         (1- (+ (X-Rect-y rect) (X-Rect-height rect))))
                                   (cons (- (+ (X-Rect-x rect) (X-Rect-width rect)) 1)
                                         (1- (+ (X-Rect-y rect) (X-Rect-height rect)))))
                             (cons (cons (- (+ (X-Rect-x rect) (X-Rect-width rect)) 1)
                                         (X-Rect-y rect))
                                   (cons (- (+ (X-Rect-x rect) (X-Rect-width rect)) 1)
                                         (1- (+ (X-Rect-y rect) (X-Rect-height rect)))))
                             )))

      ;; Finally apply change to xwin
      (when force-update
        (xwem-tabber-redraw-xrect-1 tabber rect))

      ;; Unmark client as need to be redrawed
      (when (xwem-cl-p cl)
        (xwem-cl-rem-sys-prop cl 'xwem-tab-need-redraw))
      )))

(defun xwem-tabber-regeom-p (tabber)
  "Return non-nil if TABBER is regeomed."
  (let ((old-clients (xwem-tabber-clients tabber))
        (new-clients (xwem-tabber-frame-win-clients tabber)))
    (unless (xwem-tabber-clients-equal old-clients new-clients)
      (setf (xwem-tabber-clients tabber) new-clients)
      (when new-clients
        (xwem-tabber-regeom tabber))
      t)))

(define-xwem-deferred xwem-tabber-draw (tabber &optional force-draw)
  "On TABBER draw client's tabs.
If FORCE-DRAW is non-nil redraw tabber even if it logically does not
need to be redrawed."
  (when (and (xwem-tabber-p tabber)
             (xwem-frame-p (xwem-tabber-frame tabber))
             ;; none layout does not need to be redrawn
             (not (eq (xwem-frame-property
                       (xwem-tabber-frame tabber) 'title-layout)
                      'none)))
    (let ((cls-to-draw nil)
          (need-draw-p nil))

      (unless (eq (xwem-tabber-get-prop tabber 'xwem-frame-selected-p)
                  (xwem-frame-selected-p (xwem-tabber-frame tabber)))
        (xwem-tabber-put-prop tabber 'xwem-frame-selected-p
          (xwem-frame-selected-p (xwem-tabber-frame tabber)))
        (setq force-draw t))

      (if (or (xwem-tabber-regeom-p tabber) force-draw)
          (setq cls-to-draw (or (xwem-tabber-clients tabber) (list nil))
                need-draw-p t)          ; all clients
        (setq cls-to-draw
              (delq nil (mapcar #'(lambda (cl)
                                    (and (xwem-cl-get-sys-prop
                                          cl 'xwem-tab-need-redraw) cl))
                                (xwem-tabber-clients tabber)))
              need-draw-p cls-to-draw))

      (when need-draw-p
        (mapc #'(lambda (cl)
                  (xwem-tabber-draw-format-1 cl tabber))
              cls-to-draw)
        (xwem-tabber-redraw-1 tabber)))))

(defun xwem-tabber-event-handler (xdpy xwin xev)
  "On display XDPY and window XWIN handle event XEV."
  (let ((tabber (X-Win-get-prop xwin 'xwem-tabber)))
    (when (xwem-tabber-p tabber)
      (X-Event-CASE xev
        (:X-Expose
         (xwem-tabber-redraw-xrect
          tabber (xwem-tabber-rect->xpix-rect
                  tabber
                  (make-X-Rect :x (X-Event-xexpose-x xev)
                               :y (X-Event-xexpose-y xev)
                               :width (X-Event-xexpose-width xev)
                               :height (X-Event-xexpose-height xev)))))

        (:X-DestroyNotify
         (X-Win-EventHandler-rem
          (xwem-tabber-xwin tabber) 'xwem-tabber-event-handler)
         (when (xwem-tabber-xpreparer tabber)
           (XFreePixmap (xwem-dpy) (xwem-tabber-xpreparer tabber)))
         (when (xwem-tabber-xpix-copy tabber)
           (XFreePixmap (xwem-dpy) (xwem-tabber-xpix-copy tabber)))
         (X-invalidate-cl-struct tabber))

        ((:X-ButtonPress :X-ButtonRelease)
         ;; Handle button press/release event
         (let* ((x (X-Event-xbutton-event-x xev))
                (y (X-Event-xbutton-event-y xev))
                (xwem-tabber-click-frame (xwem-tabber-frame tabber))
                (xwem-tabber-click-cl (xwem-tabber-cl-at tabber x y))
                (xwem-keyboard-echo-keystrokes nil)) ; XXX
           (xwem-overriding-local-map
             (if (xwem-frame-dedicated-p (xwem-tabber-frame tabber))
                 xwem-tabber-dedicated-map
               xwem-tabber-map)
             (xwem-dispatch-command-xevent xev))))
        ))))

(defun xwem-tabber-create (frame)
  "Create new tabber for FRAME."
  (let* ((xgeom (make-X-Geom :x 0 :y 0
                             :width 1 :height 1
                             :border-width 0)) ; XXX
         (tabber (make-xwem-tabber :frame frame
                                   :xgeom xgeom))
         (xdpy (xwem-dpy))
         (w (XCreateWindow
             xdpy
             (xwem-frame-xwin frame)
             (X-Geom-x xgeom) (X-Geom-y xgeom)
             (X-Geom-width xgeom) (X-Geom-height xgeom)
             (X-Geom-border-width xgeom)
             nil nil nil                ;X-InputOutput nil
             :background-pixel (XWhitePixel (xwem-dpy))
             :bit-gravity X-StaticGravity
             :backing-store X-Always)))

    (setf (xwem-tabber-xwin tabber) w)
    (X-Win-put-prop w 'xwem-tabber tabber)

    (XSelectInput xdpy w (Xmask-or XM-Exposure XM-StructureNotify
                                   XM-ButtonPress XM-ButtonRelease
                                   XM-ButtonMotion))
    (X-Win-EventHandler-add-new w 'xwem-tabber-event-handler 0
                                (list X-Expose X-DestroyNotify X-ButtonPress
                                      X-ButtonRelease X-MotionNotify))

    ;; Adjust XGEOM and create Preparer and xpix-copy
;    (setf (xwem-tabber-xpreparer tabber)
;          (XCreatePixmap xdpy w (XDefaultDepth xdpy) (X-Geom-width xgeom) (X-Geom-height xgeom)))
;    (setf (xwem-tabber-xpix-copy tabber)
;          (XCreatePixmap xdpy w (XDefaultDepth xdpy) (X-Geom-width xgeom) (X-Geom-height xgeom)))
    (xwem-tabber-resize tabber)

    ;; Draw tabber contents and map its window
    (xwem-tabber-draw-1 tabber t)
    (XMapWindow (X-Win-dpy w) w)
    tabber))

(defun xwem-tabber-move-resize (tabber)
  "Move TABBER to its place according to title-layout, etc,"
  (let* ((frame (xwem-tabber-frame tabber))
         (th (xwem-frame-property frame 'title-height))
         (ibw (xwem-frame-property frame 'inner-border-width))
         (xgeom (xwem-tabber-xgeom tabber))
         x y w h)
    (when (and (xwem-frame-p frame)
               (memq (xwem-frame-property frame 'title-layout)
                     '(top bottom left right)))
      (case (xwem-frame-property frame 'title-layout)
        (top
         (setf (X-Geom-width xgeom) (- (xwem-frame-width frame) ibw ibw)
               (X-Geom-height xgeom) th)
         (setq x ibw
               y ibw
               w (X-Geom-width xgeom)
               h (X-Geom-height xgeom)))
        (bottom
         (setf (X-Geom-width xgeom) (- (xwem-frame-width frame) ibw ibw)
               (X-Geom-height xgeom) th)
         (setq x ibw
               y (- (xwem-frame-height frame) th ibw)
               w (X-Geom-width xgeom)
               h (X-Geom-height xgeom)))
        (left
         (setf (X-Geom-width xgeom) (- (xwem-frame-height frame) ibw ibw)
               (X-Geom-height xgeom) th)
         (setq x ibw
               y ibw
               w (X-Geom-height xgeom)
               h (X-Geom-width xgeom)))

        (right
         (setf (X-Geom-width xgeom) (- (xwem-frame-height frame) ibw ibw)
               (X-Geom-height xgeom) th)
         (setq x (- (xwem-frame-width frame) th ibw)
               y ibw
               w (X-Geom-height xgeom)
               h (X-Geom-width xgeom))))

      (XMoveResizeWindow (xwem-dpy) (xwem-tabber-xwin tabber) x y w h))))

(defun xwem-tabber-resize (tabber)
  "Resize TABBER to WIDTH, HEIGHT."
  (let* ((xgeom (xwem-tabber-xgeom tabber))
         (owidth (X-Geom-width xgeom))
         (oheight (X-Geom-height xgeom)))

    (xwem-tabber-move-resize tabber)

    (when (or (> (X-Geom-width xgeom) owidth)
              (> (X-Geom-height xgeom) oheight))
      ;; Recreate xpreparer
      (when (X-Pixmap-p (xwem-tabber-xpreparer tabber))
        (XFreePixmap (xwem-dpy) (xwem-tabber-xpreparer tabber)))
      (when (X-Pixmap-p (xwem-tabber-xpix-copy tabber))
        (XFreePixmap (xwem-dpy) (xwem-tabber-xpix-copy tabber)))
      (setf (xwem-tabber-xpreparer tabber)
            (XCreatePixmap (xwem-dpy)
                           (xwem-tabber-xwin tabber) (XDefaultDepth (xwem-dpy))
                           (X-Geom-width xgeom) (X-Geom-height xgeom)))
      (setf (xwem-tabber-xpix-copy tabber)
            (XCreatePixmap (xwem-dpy)
                           (xwem-tabber-xwin tabber) (XDefaultDepth (xwem-dpy))
                           (X-Geom-width xgeom) (X-Geom-height xgeom)))
          (when xwem-misc-turbo-mode
            (XSetWindowBackgroundPixmap (xwem-dpy) (xwem-tabber-xwin tabber)
                                        (xwem-tabber-xpix-copy tabber))))
    (xwem-tabber-regeom tabber)))

;;; Frame Hooks
(defun xwem-tabber-on-frame-select-deselect ()
  "Redraw tabbers when switching frames."
  (when (xwem-frame-p (xwem-frame-selected))
    (xwem-tabber-draw (xwem-frame-tabber (xwem-frame-selected)))))

(defun xwem-tabber-on-frame-resize (frame)
  "FRAME just resized, apply changes to tabber, if any."
  (let ((tabber (xwem-frame-tabber frame)))
    (when (xwem-tabber-p tabber)
      (xwem-tabber-resize tabber)
      (xwem-tabber-draw tabber t))))

(defun xwem-tabber-on-frame-creation (frame)
  "FRAME just created."
  (setf (xwem-frame-tabber frame) (xwem-tabber-create frame)))

(defun xwem-tabber-frame-prop-notifier (frame prop value)
  "FRAME just changed property PROP to VALUE."
  (let ((tabber (xwem-frame-tabber frame)))
    (when (xwem-tabber-p tabber)
      (case prop
        (title-layout
         (if (eq value 'none)
             (XUnmapWindow (xwem-dpy) (xwem-tabber-xwin tabber))
           (XMapWindow (xwem-dpy) (xwem-tabber-xwin tabber))))
        )
      (xwem-tabber-on-frame-resize frame))))

;; Win hooks
(defun xwem-tabber-on-win-switch (owin nwin)
  "Window switch occured OWIN -> NWIN."
  (and (xwem-win-selwin-p nwin)
       (xwem-tabber-draw (xwem-frame-tabber (xwem-win-frame nwin)))))

(defun xwem-tabber-on-win-change (win)
  "WIN's clients list changed."
  (and (xwem-win-selwin-p win)
       (xwem-tabber-draw (xwem-frame-tabber (xwem-win-frame win)))))

(defun xwem-tabber-on-win-ccl-change (win old-cl new-cl)
  "WIN's current client just changed."
  (when (xwem-cl-p old-cl)
    (xwem-tabber-on-cl-change old-cl))
  (when (xwem-cl-p new-cl)
    (xwem-tabber-on-cl-change new-cl)))

;; CL hooks
(defun xwem-tabber-on-cl-creation (cl)
  "CL just created."
  ;; Make tab rect for CL
  (unless (xwem-cl-tab-rect cl)
    (setf (xwem-cl-tab-rect cl)
          (make-X-Rect :x 0 :y 0 :width 0 :height 0)))
  )

(defun xwem-tabber-on-cl-change (cl &rest args)
  "CL just changed its component."
  (let ((tabber (xwem-cl-tabber cl)))
    (when (and tabber
               (memq cl (xwem-tabber-clients tabber))
               (xwem-win-selwin-p (xwem-cl-win cl)))
      ;; mark cl as need to be redraw
      (xwem-cl-put-sys-prop cl 'xwem-tab-need-redraw t)
      (xwem-tabber-draw tabber))))


;;;; New regeometeres
(defvar xwem-tabber-cascade-coefficient 1.5)

(defun xwem-tabber-cascade-selected-width (n)
  (cond ((= n 1) 1)
        ((= n 2) 0.7)
        (t (/ 1.0 (- n 1)))))

(defun xwem-tabber-cascade-width (k0 n)
  (let ((k1 (- n k0 1)))
    (* (/ (float (- n 2)) (float (- n 1)))
       (/ (float (- (expt xwem-tabber-cascade-coefficient k0) 1))
          (float (+ (expt xwem-tabber-cascade-coefficient k0)
                    (expt xwem-tabber-cascade-coefficient k1)
                    -2))))))

(defun xwem-tabber-cascade-regeom (tabber)
  "Adjust tab items geometries in TABBER."
  ;; TODO: handle margins
  (let* ((twidth (X-Geom-width (xwem-tabber-xgeom tabber)))
         (theight (X-Geom-height (xwem-tabber-xgeom tabber)))
         (clients (copy-list (xwem-tabber-clients tabber)))
         (n (length clients))           ; number of clients
         (tcli (xwem-frame-cl (xwem-tabber-frame tabber)))
         (k0 (- n (length (memq tcli clients))))
         (k1 (- n k0 1))
         (nw (* twidth (xwem-tabber-cascade-selected-width n)))
         (k0w (* twidth (xwem-tabber-cascade-width k0 n)))
         (k1w (- twidth nw k0w))
         k0clis k1clis rect)

    (xwem-message 'debug "n/nw: %d/%d, k0/k0w: %d/%d, k1/k1w: %d/%d" n nw k0 k0w k1 k1w)

    ;; Fill K0CLIS and K1CLIS
    (while (not (eq tcli (car clients)))
      (setq k0clis (cons (car clients) k0clis))
      (setq clients (cdr clients)))
    (setq k1clis (cdr clients))

    ;; Left clients (K0)
    (let ((sk0w k0w))
      (while k0clis
        (let* ((tw (if (cdr k0clis) (truncate (* sk0w 0.6)) (truncate sk0w)))
               (tx (if (cdr k0clis) (truncate (- sk0w tw)) 0)))
          (unless (xwem-cl-tab-rect (car k0clis))
            (setf (xwem-cl-tab-rect (car k0clis))
                  (make-X-Rect :x 0 :y 0 :width 0 :height 0)))
          (setq rect (xwem-cl-tab-rect (car k0clis)))
          (setf (X-Rect-x rect) tx)
          (setf (X-Rect-width rect) tw)
          (setf (X-Rect-height rect) theight)

          (xwem-message 'debug "TAB rect = %S, tw=%d, tx=%d sk0w=%d" rect tw tx sk0w)

          (setq sk0w (- sk0w tw)
                k0clis (cdr k0clis)))))

    ;; Selected client
    (unless (xwem-cl-tab-rect tcli)
      (setf (xwem-cl-tab-rect tcli)
            (make-X-Rect :x 0 :y 0 :width 0 :height 0)))
    (setq rect (xwem-cl-tab-rect tcli))
    (setf (X-Rect-x rect) (truncate k0w))
    (setf (X-Rect-width rect) (truncate nw))
    (setf (X-Rect-height rect) theight)

    ;; Right clients (K1)
    (let ((k1off (truncate (+ k0w nw))))
      (while k1clis
        (let ((tw (if (cdr k1clis) (truncate (* k1w 0.6)) (truncate k1w))))
          (unless (xwem-cl-tab-rect (car k1clis))
            (setf (xwem-cl-tab-rect (car k1clis))
                  (make-X-Rect :x 0 :y 0 :width 0 :height 0)))
          (setq rect (xwem-cl-tab-rect (car k1clis)))
          (setf (X-Rect-x rect) k1off)
          (setf (X-Rect-width rect) tw)
          (setf (X-Rect-height rect) theight)

          (setq k1off (+ k1off tw)
                k1w (- k1w tw)
                k1clis (cdr k1clis)))))))

(defun xwem-tabber-cascade-regeom-p (tabber)
  "Return non-nil if TABBER is regeomed."
  (let ((old-clients (xwem-tabber-clients tabber))
        (new-clients (xwem-tabber-frame-win-clients tabber))
        (ret nil))
    (unless (xwem-tabber-clients-equal old-clients new-clients)
      (setf (xwem-tabber-clients tabber) new-clients)
      (setq ret t))
    (unless ret
      (loop for cl in new-clients
        if (xwem-cl-get-sys-prop cl 'xwem-tab-need-redraw)
        return (setq ret t)))
    (when ret
      (xwem-tabber-cascade-regeom tabber)
      t)))


(provide 'xwem-tabbing)

;;;; On-load actions:
;; - Initialize tabber
(xwem-tabber-init)

;;; xwem-tabbing.el ends here
