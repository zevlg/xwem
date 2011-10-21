;;; xwem-minibuffer.el --- XWEM minibuffer support.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Dec  4 15:13:12 MSK 2003
;; Keywords: xwem, xlib
;; Time-stamp: <17/6/2011 14:20:31 lg@localhost>

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

;; XWEM minibuffer stuff.  XWEM minibuffer is Emacs frame with
;; 'minibuffer property set to 'only.  It is used for various thigs,
;; such as messages displaying, system tray, etc.

;;; Code:

(require 'xwem-load)
(require 'xwem-focus)
(require 'xwem-manage)

(eval-when-compile
  (defvar x-emacs-application-class nil))

;; Customization
(defgroup xwem-minibuffer nil
  "Group to customize XWEM minibuffer."
  :prefix "xwem-minibuffer-"
  :group 'xwem-modes)

(defcustom xwem-minibuffer-name "xwem-minibuffer"
  "*Minibuffer name to be used by XWEM."
  :type 'string
  :group 'xwem-minibuffer)

(defcustom xwem-minibuffer-bgcol "gray80"
  "*Background color to be used in `xwem-minib-frame'."
  :type 'color
  :set (lambda (sym val)
         (set sym val)
         (when (and xwem-minibuffer
                    (X-Win-p (xwem-minib-xwin xwem-minibuffer)))
           (XSetWindowBackground
            (xwem-dpy) (xwem-minib-xwin xwem-minibuffer)
            (XAllocColor (xwem-dpy) (XDefaultColormap (xwem-dpy))
                         (xwem-make-color xwem-minibuffer-bgcol)))
           (XClearArea (xwem-dpy) (xwem-minib-xwin xwem-minibuffer) 0 0
                       (X-Geom-width (xwem-minib-xgeom xwem-minibuffer))
                       (X-Geom-height (xwem-minib-xgeom xwem-minibuffer)) nil)))
  :initialize 'custom-initialize-default
  :group 'xwem-minibuffer)

(defcustom xwem-minibuffer-font (face-font-name 'default)
  "*Font to be used in `xwem-minib-frame'.  May be nil or string."
  :type '(restricted-sexp :match-alternatives ('nil try-font-name))
  :set (lambda (sym val)
         (set sym val)
         (when (and xwem-minibuffer
                    (xwem-minib-frame xwem-minibuffer))
           (set-face-font 'default xwem-minibuffer-font
                          (xwem-minib-frame xwem-minibuffer))))
  :initialize 'custom-initialize-default
  :group 'xwem-minibuffer)

(defcustom xwem-minibuffer-height 1
  "Height of `xwem-minibuffer'."
  :type 'number
  :set (lambda (sym val)
         (set sym val)
         ;; DO NOT RELY on `set-frame-height'
         (let ((frame (and xwem-minibuffer (xwem-minib-frame xwem-minibuffer)))
               (mcl (and xwem-minibuffer (xwem-minib-cl xwem-minibuffer))))
           (when (and frame mcl)
             (setq st (/ (frame-pixel-height frame) (frame-height frame))
                   nsz (* st xwem-minibuffer-height))
             (xwem-client-resize mcl nil nsz))))
  :initialize 'custom-initialize-default
  :group 'xwem-minibuffer)

(defcustom xwem-minibuffer-width 80
  "*Usable width of `xwem-minibuffer' frame."
  :type 'number
  :set (lambda (sym val)
         (set sym val)
         ;; DO NOT RELY on `set-frame-width'
         (let ((frame (and xwem-minibuffer (xwem-minib-frame xwem-minibuffer)))
               (mcl (and xwem-minibuffer (xwem-minib-cl xwem-minibuffer))))
           (when (and frame mcl)
             (setq st (/ (frame-pixel-width frame) (frame-width frame))
                   nsz (* st xwem-minibuffer-width))
             (xwem-client-resize mcl nsz nil))))
  :initialize 'custom-initialize-default
  :group 'xwem-minibuffer)

;;;###xwem-autoload
(defcustom xwem-minibuffer-border-width 2
  "Border width for `xwem-minibuffer'."
  :type 'number
  :set (lambda (sym val)
         (set sym val)
         (let ((cl (and xwem-minibuffer (xwem-minib-cl xwem-minibuffer))))
           (when (xwem-cl-p cl)
             (xwem-client-set-property
              cl 'x-border-width xwem-minibuffer-border-width))))
  :initialize 'custom-initialize-default
  :group 'xwem-minibuffer)

(defcustom xwem-minibuffer-passive-border-color "blue3"
  "Border color for `xwem-minibuffer'."
  :type 'color
  :set (lambda (sym val)
         (set sym val)
         (let ((cl (and xwem-minibuffer (xwem-minib-cl xwem-minibuffer))))
           (when (xwem-cl-p cl)
             (xwem-set-face-foreground
              'x-border-face xwem-minibuffer-passive-border-color nil cl)
             (xwem-client-set-property
              cl 'x-border-color
              (xwem-face-foreground
               'x-border-face (and (xwem-cl-selected-p cl) '(selected)) cl)))))
  :initialize 'custom-initialize-default
  :group 'xwem-minibuffer)

(defcustom xwem-minibuffer-active-border-color "cyan3"
  "Border color for `xwem-minibuffer' when it focused."
  :type 'color
  :set (lambda (sym val)
         (set sym val)
         (let ((cl (and xwem-minibuffer (xwem-minib-cl xwem-minibuffer))))
           (when (xwem-cl-p cl)
             (xwem-set-face-foreground
              'x-border-face xwem-minibuffer-active-border-color
              '(selected) cl)
             (xwem-client-set-property
              cl 'x-border-color
              (xwem-face-foreground
               'x-border-face (and (xwem-cl-selected-p cl)
                                   '(selected)) cl)))))
  :initialize 'custom-initialize-default
  :group 'xwem-minibuffer)

(defcustom xwem-minibuffer-hide-cursor-mode t
  "*Non-nil mean that Emacs cursor will be invisible in `xwem-minibuffer'.
When `xwem-minibuffer' loses focus Emacs cursor hides, and unhides
when it focused."
  :type 'boolean
  :set (lambda (sym val)
         (set sym val)
         (when (and xwem-minibuffer (xwem-cl-p (xwem-minib-cl xwem-minibuffer)))
           (set-frame-property (xwem-minib-frame xwem-minibuffer)
                               'text-cursor-visible-p
                               (or (not xwem-minibuffer-hide-cursor-mode)
                                   (xwem-cl-selected-p
                                    (xwem-minib-cl xwem-minibuffer))))))
  :initialize 'custom-initialize-default
  :group 'xwem-minibuffer)

(defcustom xwem-minibuffer-hide-show-parameter 0
  "*Animation delay parameter, when hiding/showing xwem minibuffer."
  :type 'number
  :group 'xwem-minibuffer)

(defcustom xwem-minibuffer-autohide-timeout nil
  "*Non-nil mean xwem minibuffer will be autohided, after that many seconds."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Seconds"))
  :set (lambda (sym value)
         (set sym value)
         (let ((mcl (xwem-minib-cl xwem-minibuffer)))
           (when mcl
             (if value
                 (xwem-minibuffer-enable-autohide-timer mcl)
               (xwem-activate mcl)
               (xwem-minibuffer-disable-autohide-timer mcl)))))
  :initialize 'custom-initialize-default
  :group 'xwem-minibuffer)

(defcustom xwem-minibiffer-always-on-top-rank 20
  "*Always on top rank or nil."
  :type '(choice (const :tag "No rank" nil)
                 (number :tag "Rank"))
  :set (lambda (sym value)
         (set sym value)
         (let ((xwin (xwem-minib-xwin xwem-minibiffer)))
           (when (X-Win-p xwin)
             (setf (xwem-xwin-rank xwin) value))))
  :initialize 'custom-initialize-default
  :group 'xwem-minibuffer)

(defcustom xwem-minibuffer-raise-when-active t
  "*Non-nil mean xwem minibuffer is raised when activated."
  :type 'boolean
  :group 'xwem-minibuffer)

(defcustom xwem-minib-frame-autoraise t
  "*Non-nil minibuffer's frame is raised when minibuffer is selected.
It is pretty wise to keep it non-nil, because dockapps may lie above
the frame and you wont see what you are typing."
  :type 'boolean
  :group 'xwem-minibuffer)

(defcustom xwem-minibuffer-emacs-frames-has-minibuffer t
  "*Non-nil mean Emacs frames will have their own minibuffers."
  :type 'boolean
  :set (lambda (sym val)
         (set sym val)
         (if val
             (progn
               (setq default-x-frame-plist
                     (plist-remprop default-x-frame-plist 'minibuffer))
               (ad-remove-advice
                'read-from-minibuffer 'around 'xwem-minib-as-emacs-minib))
           (setq default-x-frame-plist
                 (plist-put default-x-frame-plist 'minibuffer nil))
           (defadvice
             read-from-minibuffer (around xwem-minib-as-emacs-minib activate)
             "Make xwem minibuffer be conditionally selected, when its emacs frame
selected.
Used when `xwem-minibuffer-emacs-frames-has-minibuffer' is nil.
xwem minibuffer selected only if reading occurs in frame that has xwem
minibuffer as minibuffer window."
             (if (eq (window-frame (minibuffer-window (selected-frame)))
                     (xwem-minib-frame xwem-minibuffer))
                 (xwem-under-minibuffer
                  ad-do-it)
               ad-do-it))))
  :initialize 'custom-initialize-default
  :group 'xwem-minibuffer)

(defcustom xwem-minibuffer-set-default-minibuffer-frame t
  "*Non-nil to set xwem minibuffer frame as `default-minibuffer-frame'.
Modify this variable only if you know what you are doing."
  :type 'boolean
  :group 'xwem-minibuffer)

;;;###xwem-autoload
(defcustom xwem-minibuffer-outer-border-width 1
  "*Outer border width for xwem minibuffer."
  :type 'number
  :group 'xwem-minibuffer)

(defcustom xwem-minibuffer-outer-border-color "black"
  "*Outer border color for xwem minibuffer."
  :type 'color
  :group 'xwem-minibuffer)

;;;###autoload
(defcustom xwem-minibuffer-focusin-hook nil
  "*Hooks called when xwem minibuffer got focus."
  :type 'hook
  :group 'xwem-minibuffer
  :group 'xwem-hooks)

;;;###autoload
(defcustom xwem-minibuffer-focusout-hook nil
  "*Hooks called when xwem minibuffer lose focus."
  :type 'hook
  :group 'xwem-minibuffer
  :group 'xwem-hooks)

(defcustom xwem-minib-resize-max-height 100
  "*Maximum height of minibuffer, while in `xwem-minib-resize-mode'."
  :type '(choice (const :tag "Unlimited" nil)
                 (number :tag "Limited"))
  :group 'xwem-minibuffer)

(defcustom xwem-minib-resize-exact t
  "*If non-`nil', make minibuffer frame exactly the size needed to display all its contents.
Have no effect if `xwem-minib-resize-mode' is disabled.
Otherwise, the minibuffer frame can temporarily increase in size but
never get smaller while it is active."
  :type 'boolean
  :group 'xwem-minibuffer)

(defcustom xwem-minib-specifiers
  '((default-toolbar-visible-p . nil)

    ;; Gutters
    (default-gutter-visible-p . t)
    (top-gutter . nil)
    (top-gutter-border-width . 1)

    (menubar-visible-p . nil)
    (horizontal-scrollbar-visible-p . nil)
    ((face-font 'default) . xwem-minibuffer-font))
  "*Alist of specifiers to be set for xwem minibuffer."
  :type '(repeat (cons (sexp :tag "Specifier sexp")
                       (sexp :tag "Specifier value sexp")))
  :set (lambda (sym value)
         (set sym value)
         (when (and xwem-minibuffer
                    (frame-live-p (xwem-minib-frame xwem-minibuffer)))
           (xwem-minib-apply-specifiers (xwem-minib-frame xwem-minibuffer))))
  :group 'xwem-minibuffer)

;;; Internal variables

;; Variables
;;;###xwem-autoload
(defvar xwem-minibuffer nil
  "Default xwem minibuffer.")

(defvar xwem-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (xwem-kbd "H-g") 'minibuffer-keyboard-quit)
    (define-key map (xwem-kbd "C-<button1>") 'xwem-client-imove)
    map)
  "Keymap used while in xwem.")


(defun xwem-minib-apply-specifiers (frame)
  "Apply `xwem-minib-specifiers' to FRAME."
  (mapc #'(lambda (spc)
            (ignore-errors
              (set-specifier (eval (car spc)) (eval (cdr spc))
                             frame nil 'remove-locale)))
        xwem-minib-specifiers))

(defun xwem-minib-create ()
  "Create minibuffer that will be used by xwem, or use existen."
  (let ((mframe (or (xwem-misc-find-frame xwem-minibuffer-name
                                          (minibuffer-frame-list))
                    (make-frame minibuffer-frame-plist
                                (default-x-device))))
        (dd default-directory))

    (setf (xwem-minib-frame xwem-minibuffer) mframe)
    ;; Set specifiers values for MFRAME
    (xwem-minib-apply-specifiers mframe)
    (redraw-frame mframe t)             ; KEEP THIS!

    ;; Hack over default-directory for minibuffer buffer
    (with-current-buffer
        (window-buffer (frame-root-window mframe))
      (setq default-directory dd))

    mframe))

(defmacro xwem-cl-minibuffer (cl)
  `(xwem-cl-get-sys-prop ,cl 'xwem-minibuffer))
(defsetf xwem-cl-minibuffer (cl) (minib)
  `(xwem-cl-put-sys-prop ,cl 'xwem-minibuffer ,minib))

;;; Minibuffer focus model
(define-xwem-focus-mode minibuffer (cl action &optional xev)
  "Focus mode for xwem minibuffer"
  (let ((mb (xwem-cl-minibuffer cl)))
    (when (and (xwem-minib-p mb)
               (X-Event-p xev)
               (not (member (X-Event-xfocus-mode xev)
                            (list X-NotifyVirtual X-NotifyNonlinearVirtual))))
      (cond ((eq action 'focus-in)
             ;; XWEM Minibuffer activates
             (run-hook-with-args 'xwem-minibuffer-focusin-hook mb))
            ((eq action 'focus-out)
             ;; XWEM Minibuffer deactivates
             (run-hook-with-args 'xwem-minibuffer-focusout-hook mb))))))

;;;; ---- XWEM Minibuffer manage type ----
(defun xwem-minibuffer-client-p (cl)
  "Return non-nil if CL is minibuffer client."
  (xwem-cl-minibuffer cl))

(defun xwem-manage-minibuffer (cl)
  "Manage method for xwem minibuffers."
  (let* ((xgeom (make-X-Geom :x 0       ; XXX
                             :y (- (X-Geom-height (xwem-rootgeom))
                                   xwem-minibuffer-outer-border-width
                                   xwem-minibuffer-outer-border-width)
                             :width (- (X-Geom-width (xwem-rootgeom))
                                       xwem-minibuffer-outer-border-width
                                       xwem-minibuffer-outer-border-width)
                             :height (X-Geom-height-with-borders
                                      (xwem-cl-xgeom cl))
                             :border-width xwem-minibuffer-outer-border-width))
         (minib (make-xwem-minib
                 :frame (xwem-misc-find-emacs-frame cl)
                 :cl cl
                 :xgeom xgeom)))

    (setf (xwem-minib-xwin minib)
          (XCreateWindow (xwem-dpy) nil
                         (X-Geom-x xgeom)
                         (X-Geom-y xgeom)
                         (X-Geom-width xgeom)
                         (X-Geom-height xgeom)
                         (X-Geom-border-width xgeom)
                         nil nil nil
                         :override-redirect t
                         :background-pixel
                         (XAllocNamedColor
                          (xwem-dpy) (XDefaultColormap (xwem-dpy))
                          xwem-minibuffer-bgcol)))
    ;; Setup window a little
    (when xwem-minibuffer-outer-border-color
      (XSetWindowBorder (xwem-dpy) (xwem-minib-xwin minib)
                        (XAllocNamedColor
                         (xwem-dpy) (XDefaultColormap (xwem-dpy))
                         xwem-minibuffer-outer-border-color)))

    ;; Save CL's minibuffer
    (setf (xwem-cl-minibuffer cl) minib)

    ;; Setup x-border-face for minibuffer
    (xwem-set-face-foreground 'x-border-face xwem-minibuffer-active-border-color
                              '(selected) cl)
    (xwem-set-face-foreground 'x-border-face xwem-minibuffer-passive-border-color
                              nil cl)
    (xwem-client-set-property cl 'x-border-width xwem-minibuffer-border-width)
    (xwem-client-set-property cl 'x-border-color
                              (xwem-face-foreground 'x-border-face
                                                    (and (xwem-cl-selected-p cl)
                                                         '(selected)) cl))

    ;; Reparent xwem minib client to parent
    ;; XXX XXX
    (setf (X-Geom-x (xwem-cl-xgeom cl)) 0)
    (setf (X-Geom-y (xwem-cl-xgeom cl)) 0)
    (XReparentWindow (xwem-dpy) (xwem-minib-cl-xwin minib)
                     (xwem-minib-xwin minib) 0 0)

    ;; Set minibuffer focus model
    (xwem-focus-mode-set cl 'minibuffer)

    ;; Install minibuffer local keymap
    (xwem-use-local-map xwem-minibuffer-map cl)

    ;; Finnally refit cl and map parent
    (xwem-refit cl)
    (XMapWindow (xwem-dpy) (xwem-minib-xwin minib))

    ;; Set always on top rank (if any)
    (setf (xwem-xwin-rank (xwem-minib-xwin minib))
          xwem-minibiffer-always-on-top-rank)

    ;; Set default minibuffer, if not already set
    (unless (xwem-cl-p (xwem-minib-cl xwem-minibuffer))
      (setq xwem-minibuffer minib)
      (when xwem-minibuffer-set-default-minibuffer-frame
        (setq default-minibuffer-frame (xwem-minib-frame xwem-minibuffer))))

    ;; Now activate minibuffer
    (xwem-activate cl)
    (xwem-minib-apply-state-1 minib )))

(define-xwem-deferred xwem-minib-apply-pxgeom (minib)
  "Apply MINIB's parent geometry to life."
  (let ((pxgeom (xwem-minib-xgeom minib)))
    (XMoveResizeWindow (xwem-dpy)
                       (xwem-minib-xwin minib)
                       (X-Geom-x pxgeom)
                       (X-Geom-y pxgeom)
                       (X-Geom-width pxgeom)
                       (X-Geom-height pxgeom))))

;;
;; Some bug here:

;;  Do `(xwem-refit (xwem-minib-cl xwem-minibuffer))' - xwem minib
;;  will change its width. (ONLY when xwem-minib-resize-mode is on)

(defun xwem-refit-minibuffer (cl)
  "Refit xwem minibuffer client CL."
  (xwem-debug 'xwem-misc "Minib: Refiting .. to %S" '(xwem-cl-new-xgeom cl))

  (let ((cl-xgeom (xwem-cl-xgeom cl))
        (cl-nx (and (xwem-cl-new-xgeom cl)
                    (X-Geom-x (xwem-cl-new-xgeom cl))))
        (pxgeom (xwem-minib-xgeom (xwem-cl-minibuffer cl))))
    ;; Adjust geometry a little to fill into xwem-minib-xwin and apply
    ;; changes to life
    (xwem-cl-apply-new-xgeom cl)
    (when cl-nx
      ;; CL has new x location - handle it
      (setf (X-Geom-x pxgeom) (X-Geom-x cl-xgeom))
      (setf (X-Geom-width pxgeom)
            (- (X-Geom-width (xwem-rootgeom))
               (X-Geom-x pxgeom)
               xwem-minibuffer-outer-border-width
               xwem-minibuffer-outer-border-width))
      (xwem-minib-apply-pxgeom (xwem-cl-minibuffer cl)))
    (xwem-cl-correct-size-for-size
     cl
     (make-X-Geom :x 0 :y 0
                  :width (X-Geom-width-with-borders cl-xgeom)
                  :height (X-Geom-height-with-borders cl-xgeom)
                  :border-width (X-Geom-border-width cl-xgeom))
     'left 'top)
    (xwem-cl-apply-xgeom cl)

    ;; Check maybe parent need to be resized/moved?
    (unless (= (X-Geom-height-with-borders cl-xgeom)
               (X-Geom-height pxgeom))
      (when (xwem-cl-active-p cl)
        (decf (X-Geom-y pxgeom)
              (- (X-Geom-height-with-borders cl-xgeom)
                 (X-Geom-height pxgeom))))
      (setf (X-Geom-height pxgeom)
            (X-Geom-height-with-borders cl-xgeom))
      (xwem-minib-apply-pxgeom (xwem-cl-minibuffer cl)))))

(defun xwem-minibuffer-autohide-timer (cl)
  (when (and (numberp xwem-minibuffer-autohide-timeout)
             (not (xwem-cl-selected-p cl)))
    (xwem-deactivate cl))
  (xwem-cl-rem-sys-prop cl 'auto-hide-timer))

(defun xwem-minibuffer-disable-autohide-timer (cl)
  (let ((tmr (xwem-cl-get-sys-prop cl 'auto-hide-timer)))
    (when tmr
      (disable-timeout tmr)
      (xwem-cl-rem-sys-prop cl 'auto-hide-timer))))

(defun xwem-minibuffer-enable-autohide-timer (cl)
  (when (numberp xwem-minibuffer-autohide-timeout)
    (xwem-minibuffer-disable-autohide-timer cl)
    (xwem-cl-put-sys-prop cl 'auto-hide-timer
      (add-timeout xwem-minibuffer-autohide-timeout
                   'xwem-minibuffer-autohide-timer cl))))

(define-xwem-deferred xwem-minib-apply-state (minib)
  "Apply xwem minibuffer MINIB's state to life."
  (let* ((pgeom (xwem-minib-xgeom minib))
         (cyo (X-Geom-height pgeom))
         (state (xwem-cl-state (xwem-minib-cl minib)))
         (param (and (numberp xwem-minibuffer-hide-show-parameter)
                     (not (zerop xwem-minibuffer-hide-show-parameter))
                     xwem-minibuffer-hide-show-parameter))
         (i 0))
    (when (not (eq state (xwem-minib-get-prop minib 'saved-state)))
      (xwem-debug 'xwem-misc "Minib: new state .. %S" 'state)

      (while (< i cyo)
        (setf (X-Geom-y (xwem-minib-xgeom minib))
              (funcall (if (eq state 'active) '- '+)
                       (X-Geom-y (xwem-minib-xgeom minib)) 1))
        (XMoveWindow (xwem-dpy) (xwem-minib-xwin minib)
                     (X-Geom-x (xwem-minib-xgeom minib))
                     (X-Geom-y (xwem-minib-xgeom minib)))
        (XFlush (xwem-dpy))
        (when param
          (sit-for param t))
        (setq i (1+ i)))
      (xwem-minib-put-prop minib 'saved-state state)

      (when (and xwem-minibuffer-raise-when-active
                 (eq state 'active))
        (xwem-misc-raise-xwin (xwem-minib-xwin minib))))

    (when (eq state 'active)
      (xwem-minibuffer-enable-autohide-timer (xwem-minib-cl minib)))))

(defun xwem-activate-minibuffer (cl &optional type)
  "Activate xwem minibuffer CL."
  (cond ((eq type 'activate)
         (xwem-minib-apply-state (xwem-cl-minibuffer cl)))

        ((eq type 'select)
         (when xwem-minibuffer-hide-cursor-mode
           (set-frame-property (xwem-minib-frame (xwem-cl-minibuffer cl))
                               'text-cursor-visible-p t))

         (xwem-minibuffer-disable-autohide-timer cl))))

(defun xwem-deactivate-minibuffer (cl &optional type)
  "Deactivate xwem minibuffer client CL."
  (cond ((eq type 'deactivate)
         (xwem-minib-apply-state (xwem-cl-minibuffer cl)))

        ((eq type 'deselect)
         (when xwem-minibuffer-hide-cursor-mode
           (set-frame-property (xwem-minib-frame (xwem-cl-minibuffer cl))
                               'text-cursor-visible-p nil))
         (when xwem-minibuffer-autohide-timeout
           (xwem-deactivate cl)))))

(defun xwem-iconify-minibuffer (cl)
  "Iconify xwem minibuffer client CL."
  (xwem-deactivate cl))

;; Events handler
(define-xwem-deferred xwem-minib-focusin-autoraise (minib)
  "Mainly for use in `xwem-minibuffer-focusin-hook'."
  (when xwem-minib-frame-autoraise
    (XRaiseWindow (xwem-dpy) (xwem-minib-cl-xwin minib))))

(define-xwem-deferred xwem-minib-focusout-autolower (minib)
  "Mainly for use in `xwem-minibuffer-focusout-hook'."
  (when xwem-minib-frame-autoraise
    (XLowerWindow (xwem-dpy) (xwem-minib-cl-xwin minib))))

;;;###autoload(autoload 'xwem-minibuffer-activate "xwem-minibuffer" "" t)
(define-xwem-command xwem-minibuffer-activate ()
  "Switch to xwem minibuffer if it is in active state."
  (xwem-interactive)
  (if (eq (active-minibuffer-window)
          (frame-selected-window (xwem-minib-frame xwem-minibuffer)))
      (xwem-select-client (xwem-minib-cl xwem-minibuffer))
    (xwem-activate (xwem-minib-cl xwem-minibuffer))))

;;;###autoload
(defun xwem-minibuffer-init ()
  "Initialize xwem minibuffer."
  (xwem-message 'init "Initializing minibuffer ...")

  (setq minibuffer-frame-plist
        (xwem-misc-merge-plists
         minibuffer-frame-plist
         `(name ,xwem-minibuffer-name
                wait-for-wm nil
                height ,xwem-minibuffer-height
                text-cursor-visible-p ,(not xwem-minibuffer-hide-cursor-mode)
                width ,xwem-minibuffer-width
                minibuffer only)))

  ;; Adjust initial frame params
  (unless xwem-minibuffer-emacs-frames-has-minibuffer
    (setq initial-frame-plist
          (plist-put initial-frame-plist 'minibuffer nil)))
  (setq initial-frame-plist
        (plist-put initial-frame-plist 'wait-for-wm nil))

  ;; Adjust default frame params
  (unless xwem-minibuffer-emacs-frames-has-minibuffer
    (setq default-x-frame-plist
          (plist-put default-x-frame-plist 'minibuffer nil)))
  (setq default-x-frame-plist
        (plist-put default-x-frame-plist 'wait-for-wm nil))

  ;; Create XEmacs minibuffer only frame for xwem minibuffer
  (setq xwem-minibuffer (make-xwem-minib))
  (setf (xwem-minib-frame xwem-minibuffer)
        (make-initial-minibuffer-frame nil))

  (add-hook 'xwem-minibuffer-focusin-hook 'xwem-minib-focusin-autoraise)
  (add-hook 'xwem-minibuffer-focusout-hook 'xwem-minib-focusout-autolower)

  (xwem-message 'init "Initializing minibuffer ... done"))

;;; Resize-minibuffer mode
(defcustom xwem-minib-resize-mode nil
  "*Non-nil activates xwem minibuffer automatic resize mode."
  :type 'boolean
  :set (lambda (sym val)
         (xwem-minib-resize-mode (if val 1 -1)))
  :initialize 'custom-initialize-default
  :group 'xwem-minibuffer)

(defun xwem-minib-rsz-count-window-lines (&optional start end)
  "Return number of window lines occupied by text in region.
The number of window lines may be greater than the number of actual lines
in the buffer if any wrap on the display due to their length.

Optional arguments START and END default to point-min and point-max,
respectively."
  (with-current-buffer
      (window-buffer (frame-selected-window (xwem-minib-frame xwem-minibuffer)))
    (save-excursion
      (goto-char (point-min))
      ;; XXX
      (vertical-motion-pixels
       (X-Geom-height (xwem-rootgeom))
       (frame-selected-window (xwem-minib-frame xwem-minibuffer))))))

(defvar xwem-minib-rsz-saved-height nil)

(define-xwem-deferred xwem-minib-rsz-resize (&optional new-height)
  "Resize xwem minibuffer to fit either NEW-HEIGHT.
If NEW-HEIGHT is ommited, current window height considered.
`xwem-minib-rsz-resize' won't ever shrink height to less then original
height stored in `xwem-minib-rsz-saved-height'."
  (when (xwem-cl-selected-p (xwem-minib-cl xwem-minibuffer))
    (let* ((height (window-text-area-pixel-height
                    (frame-selected-window (xwem-minib-frame xwem-minibuffer))))
           (lines (or new-height
                      (+ (face-height
                          'default (xwem-minib-frame xwem-minibuffer))
                         (let ((wl (xwem-minib-rsz-count-window-lines)))
                           (if (and xwem-minib-resize-max-height
                                    (> wl xwem-minib-resize-max-height))
                               xwem-minib-resize-max-height
                             wl)))))
           (mg (xwem-minib-cl-xgeom xwem-minibuffer))
           (rh (or new-height (+ (X-Geom-height mg) (- lines height)))))
      (when (< rh xwem-minib-rsz-saved-height)
        (setq rh xwem-minib-rsz-saved-height))
      (when (or new-height xwem-minib-resize-exact)
        ;; Adjust xwem minibuffer cl height
        (xwem-client-resize (xwem-minib-cl xwem-minibuffer) nil rh)))))

(define-xwem-deferred xwem-minib-rsz-setup ()
  "Setup xwem resize minibuffer mode for xwem minibuffer."
  (when (xwem-cl-selected-p (xwem-minib-cl xwem-minibuffer))
    (setq xwem-minib-rsz-saved-height
          (X-Geom-height (xwem-minib-cl-xgeom xwem-minibuffer)))

    (add-hook 'post-command-hook 'xwem-minib-rsz-resize nil t)

    (unless (and (boundp 'icomplete-mode)
                 (eval 'icomplete-mode)) ; shutup compiler
      (xwem-minib-rsz-resize))))

(defun xwem-minib-rsz-restore ()
  "Restore xwem minibuffer size."
  (when (and (xwem-cl-selected-p (xwem-minib-cl xwem-minibuffer))
             xwem-minib-rsz-saved-height)
    (xwem-client-resize (xwem-minib-cl xwem-minibuffer) nil
                        xwem-minib-rsz-saved-height)

    (remove-hook 'post-command-hook 'xwem-minib-rsz-resize)))

;;;###autoload(autoload 'xwem-minib-resize-mode "xwem-minibuffer" "" t)
(define-xwem-command xwem-minib-resize-mode (arg)
  "Toggle xwem minibuffer auto-resize mode.
If prefix ARG is positive number - start.
If prefix ARG is negative number - stop.
Otherwise toggle."
  (xwem-interactive "P")
  (if (or (and (numberp arg) (> arg 0))
          (not xwem-minib-resize-mode))
      (progn
        (add-hook 'xwem-client-select-hook 'xwem-minib-rsz-setup)
        (add-hook 'xwem-client-deselect-hook 'xwem-minib-rsz-restore)
        (setq xwem-minib-resize-mode t))
    (remove-hook 'xwem-client-select-hook 'xwem-minib-rsz-setup)
    (remove-hook 'xwem-client-deselect-hook 'xwem-minib-rsz-restore)
    (setq xwem-minib-resize-mode nil)))

;;; xwem minibuffer modeline using top gutter
(defvar xwem-modeline-format
  '("--"
    (symbol-name (xwem-cl-manage-type cl))
    ": "
    "["
    (or (car (xwem-client-application cl))
        "unknown")
    "]"

    ;; Some additional info
    " "
    (if (xwem-cl-marked-p cl) "*" "-")
    (let ((reg (xwem-client-property cl 'register)))
      (if reg (list (format "%c" reg) 'bold) "-"))
    " "

    (list (xwem-client-name cl) 'modeline-buffer-id)
    "   "
    ;; Minor modes
    "("
    (mapconcat 'identity
               (delq nil (mapcar #'(lambda (mm)
                                     (and (symbol-value (car mm))
                                          (cadr mm)))
                                 xwem-minor-mode-alist)) " ")
    ")"
    "----"
    (let ((usz (xwem-cl-get-usize cl)))
      (format "%dx%d" (car usz) (cdr usz)))
    "--")
  "Modeline format.
Number of triangular brackets shows current minibuffer depth.")
(xwem-make-variable-client-local 'xwem-modeline-format)

(defun xwem-last-nonminibuffer-client ()
  "Return last non-minibuffer client.
If selected client is non minibuffer - return selected client."
  (let ((lcs (cons (xwem-cl-selected) xwem-last-clients)))
    (while (and lcs (xwem-minibuffer-client-p (car lcs)))
      (setq lcs (cdr lcs)))
    (car lcs)))

(defun xwem-modeline-regenerate (cl)
  "Regenerate modeline string."
  (mapconcat #'(lambda (me)
                 ;; NOTE: CL symbol is bound here
                 (condition-case nil
                     (let ((str "") (faces nil))
                       (setq me (eval me))
                       (if (listp me)
                           (setq str (copy-sequence (car me))
                                 faces (cdr me))
                         (setq str (copy-sequence me)
                               faces nil))
                       (xwem-str-with-faces str (append '(modeline) faces)))
                   (t "<error>")))
             xwem-modeline-format ""))

(define-xwem-deferred xwem-modeline-redraw (&optional cl)
  "Redraw xwem modeline."
  ;; XXX this assumes 'modeline face has fixed width font
  (let* ((lnmc (xwem-last-nonminibuffer-client))
         (cl (or cl lnmc)))
    (when (and (eq cl lnmc)
               (xwem-cl-alive-p cl))
      (let* ((str (xwem-modeline-regenerate cl))
             (mw (/ (gutter-pixel-width 'top (xwem-minib-frame xwem-minibuffer))
                    (face-width 'modeline (xwem-minib-frame xwem-minibuffer)))))
        (set-specifier top-gutter (substring str 0 (and (> (length str) mw) mw))
                       (xwem-minib-frame xwem-minibuffer))))))

;;;###autoload(autoload 'xwem-modeline-enable "xwem-minibuffer" nil t)
(define-xwem-command xwem-modeline-enable ()
  "Enable modeline."
  (xwem-interactive)

  (add-hook 'xwem-cl-change-hook 'xwem-modeline-redraw)
  (add-hook 'xwem-client-select-hook 'xwem-modeline-redraw)

  (set-specifier top-gutter-visible-p t
                 (xwem-minib-frame xwem-minibuffer))
  ;; Start showing gutter
  (xwem-modeline-redraw-1)
  ;; Fix xwem minibuffer height size
  (xwem-client-resize
   (xwem-minib-cl xwem-minibuffer) nil
   (+ (X-Geom-height-with-borders (xwem-minib-cl-xgeom xwem-minibuffer))
      (gutter-pixel-height 'top (xwem-minib-frame xwem-minibuffer))
      (specifier-instance top-gutter-border-width
                          (xwem-minib-frame xwem-minibuffer))
      (specifier-instance top-gutter-border-width
                          (xwem-minib-frame xwem-minibuffer)))))

;;;###autoload(autoload 'xwem-modeline-disable "xwem-minibuffer" nil t)
(define-xwem-command xwem-modeline-disable ()
  "Disable modeline."
  (xwem-interactive)

  (remove-hook 'xwem-cl-change-hook 'xwem-modeline-redraw)
  (remove-hook 'xwem-client-select-hook 'xwem-modeline-redraw)

  ;; Fix xwem minibuffer height size
  (xwem-client-resize
   (xwem-minib-cl xwem-minibuffer) nil
   (- (X-Geom-height-with-borders (xwem-minib-cl-xgeom xwem-minibuffer))
      (gutter-pixel-height 'top (xwem-minib-frame xwem-minibuffer))
      (specifier-instance top-gutter-border-width
                          (xwem-minib-frame xwem-minibuffer))
      (specifier-instance top-gutter-border-width
                          (xwem-minib-frame xwem-minibuffer))))
  ;; Stop showing gutter
  (set-specifier top-gutter-visible-p nil
                 (xwem-minib-frame xwem-minibuffer))
  (set-specifier top-gutter nil
                 (xwem-minib-frame xwem-minibuffer)))


(provide 'xwem-minibuffer)

;;;; On-load actions:
;; Add manage type
(define-xwem-manage-model minibuffer
  "Managing model for xwem minibuffer."
  :cl-properties '(dummy-client-p t
                                  skip-deselect t
                                  override-skip-deselect t)
  :qualifier `(and (class-name ,(concat "^" x-emacs-application-class "$"))
                   (class-inst ,(concat "^" xwem-minibuffer-name "$")))

  :manage-method 'xwem-manage-minibuffer
  :activate-method 'xwem-activate-minibuffer
  :deactivate-method 'xwem-deactivate-minibuffer
  :refit-method 'xwem-refit-minibuffer
  :iconify-method 'xwem-iconify-minibuffer)

;; - Before init hook
(if xwem-started
    (xwem-minib-create)
  (add-hook 'xwem-config-read-hook 'xwem-minibuffer-init)
  (add-hook 'xwem-before-init-hook 'xwem-minib-create))

;;; xwem-minibuffer.el ends here
