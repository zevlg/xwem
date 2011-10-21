;;; xwem-special.el --- Special Emacs frames handling.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Dec  4 15:01:21 MSK 2003
;; Keywords: xwem, xlib
;; Time-stamp: <20/5/2010 21:06:58 lg@localhost>

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

;; Special emacs uses by XWEM to accomplish various tasks.  Such as
;; help system, and others.  Special frames are handled in different
;; way, but remain normal XWEM client.  Usually special frame used by
;; XWEM has dedicated window, i.e. removing window will remove frame,
;; but optionally you can create normal frames.  Take a look at
;; documentation for `xwem-special-popup-frame' function.

;; XEmacs has a bug, when window is dedicated to buffer, after buffer
;; deletion window will be deleted as well and if it is only window in
;; frame frame will be also deleted.  But if there is no visible
;; frames at the moment `replace-buffer-in-windows' will skip value of
;; `allow-deletion-of-last-visible-frame' and does not deletes frame.
;; So we can't use dedicated windows, because almost everywhere we
;; will get such sitiation, for example runnig H-h H-h when there is
;; no active emacs frame.  `xwem-special-popup-frame' will use
;; dedicated frame to display buffer and here is advice for
;; `kill-buffer':

;;    (defadvice kill-buffer (before delete-dedicated-frame activate)
;;      "Work around dedicated frame problem."
;;      (let ((frame (buffer-dedicated-frame (ad-get-arg 0))))
;;      (when (framep frame)
;;        (delete-frame frame))))
;;

;;; Code:

(require 'xwem-load)
(require 'xwem-manage)

(eval-when-compile
    (defvar x-emacs-application-class nil))

;;; Customisation
(defgroup xwem-special nil
  "Group to customize special emacs frames handling."
  :prefix "xwem-special-"
  :group 'xwem-modes)

(defcustom xwem-special-frame-name "xwem-special-frame"
  "*Name for special emacs frames"
  :type 'string
  :group 'xwem-special)

(defcustom xwem-special-auto-hide nil
  "*Non-nill mean that special frames will autohide when loses focus or visibility."
  :type 'boolean
  :group 'xwem-special)

(defcustom xwem-special-display-buffer-names nil
  "*List of buffer names to display using special frame."
  :type '(repeat string)
  :group 'xwem-special)

(defcustom xwem-special-display-buffer-strategy 'half
  "*Strategy to use when display one of `xwem-special-display-buffer-names' buffer in special frame."
  :type '(choice (const :tag "Half screen" half)
                 (const :tag "Fill current client" fill)
                 (const :tag "Center" centre))
  :group 'xwem-special)

(defcustom xwem-special-default-strategy 'half
  "*Default strategy to use when displaying special Emacs frame."
  :type '(choice (const :tag "Half screen" half)
                 (const :tag "Fill current client" fill)
                 (const :tag "Center" centre))
  :group 'xwem-special)

(defcustom xwem-special-fill-border-width 10
  "*Pixels border when using `fill' strategy."
  :type 'number
  :group 'xwem-special)

(defcustom xwem-special-client-properties nil
;  '(skip-deselect t override-skip-deselect t)
  "*Plist of properties for special clients."
  :type '(restricted-sexp :match-alternatives (valid-plist-p))
  :group 'xwem-special)


(defun xwem-special-frame-init ()
  "Initialize stuff to work with special emacs frames."
  (setq special-display-frame-plist
        (plist-put special-display-frame-plist 'minibuffer nil))
  (setq special-display-frame-plist
        (plist-put special-display-frame-plist 'name xwem-special-frame-name))
  (setq special-display-frame-plist
        (plist-put special-display-frame-plist 'wait-for-wm nil))

  ;; Modify `temp-buffer-show-function'
;  (put 'temp-buffer-show-function 'saved-value temp-buffer-show-function)
;  (setq temp-buffer-show-function 'special-display-popup-frame)
  )

;; Functions
;; NOTE:
;;   - setting initially-unmapped to t causes double MapRequest
;;
(defun xwem-special-make-frame ()
  "Make special frame.
NOTE: frame is initially unmapped, use `make-frame-visible' to map it."
  (let ((props special-display-frame-plist))
    (setq props (plist-put props 'name xwem-special-frame-name))
;    (setq props (plist-put props 'initially-unmapped t))

    (make-frame props (default-x-device))))

;;;###xwem-autoload
(defun xwem-special-popup-frame (buf &optional nondedicated-p args)
  "As `special-display-popup-frame', but popup frame for sure.
When NONDEDICATED-P is non-nil then frame will not be dedicated."
  (let ((sfr (xwem-special-make-frame)))
    (set-window-buffer (frame-selected-window sfr) buf)
    (unless nondedicated-p
      (set-window-dedicated-p (frame-selected-window sfr) t))

    (set-buffer-dedicated-frame buf sfr) ; XXX

    ;; Put special frame property, to know that this frame forced to
    ;; be special.
    (set-frame-property sfr 'xwem-forced-special t)

    (make-frame-visible sfr)
    (select-frame sfr)
    sfr))

;;;###xwem-autoload
(defun xwem-special-p (cl)
  "Return non-nil if CL is special client."
  (eq (xwem-cl-manage-type cl) 'emacs-special))

;;;###xwem-autoload
(defun xwem-special-revert-focus (&optional spec-cl)
  "Try to predict who has focus, before SPEC-CL and revert to it."
  (xwem-select-last-or-other-client spec-cl))

;; Events handler
(defun xwem-special-evhandler (xdpy win xev)
  "Event handler for speical emacs frames."
  (xwem-debug 'xwem-misc "XWEM-SPECIAL-EVHANDLER: ev = %S, winid = %S"
              '(X-Event-name xev) '(aref win 2))

  (X-Event-CASE xev
    ((:X-FocusOut :X-VisibilityNotify)
     (when xwem-special-auto-hide
       (xwem-deactivate (xwem-xwin-cl win))))

    ((:X-DestroyNotify :X-UnmapNotify)
     (when (X-Win-p win)
       (X-Win-EventHandler-rem win 'xwem-special-evhandler)
       (xwem-special-revert-focus (xwem-xwin-cl win))))
    ))

;;;; ---- Manage methods for special frame ----
;;;###autoload
(defun xwem-manage-emacs-special (cl)
  "Manage method for special emacs frame client CL."
  (let* ((frame (xwem-misc-find-emacs-frame cl))
         (bname (buffer-name (window-buffer (frame-selected-window frame))))
         (win (xwem-cl-xwin cl))
         (par-win (xwem-rootwin))
         strategy fgeom)

    (cond ((member bname xwem-special-display-buffer-names)
           (setq strategy xwem-special-display-buffer-strategy))
          (t (setq strategy xwem-special-default-strategy)))

    (cond ((eq strategy 'half)
           (setq fgeom
                 (make-X-Geom
                  :x (- (X-Geom-x (xwem-minib-xgeom xwem-minibuffer))
                        (X-Geom-border-width (xwem-minib-xgeom xwem-minibuffer)))
                  :y (/ (X-Geom-y (xwem-minib-xgeom xwem-minibuffer)) 2)
                  :width (X-Geom-width-with-borders
                          (xwem-minib-cl-xgeom xwem-minibuffer))
                  :height (/ (X-Geom-y (xwem-minib-xgeom xwem-minibuffer)) 2)
                  :border-width nil)))

          ((and (eq strategy 'fill)
                (xwem-cl-alive-p (xwem-cl-selected))
                (not (eq cl (xwem-cl-selected))))
           (setq fgeom
                 (make-X-Geom
                  :x xwem-special-fill-border-width
                  :y xwem-special-fill-border-width
                  :width (- (X-Geom-width
                             (xwem-cl-xgeom (xwem-cl-selected)))
                            xwem-special-fill-border-width
                            xwem-special-fill-border-width)
                  :height (- (X-Geom-height
                              (xwem-cl-xgeom (xwem-cl-selected)))
                             xwem-special-fill-border-width
                             xwem-special-fill-border-width)
                  :border-width nil))
           (setq par-win (xwem-cl-xwin (xwem-cl-selected)))))

    ;; Operate on unmapped window
    (XSelectInput (xwem-dpy) win 0)

    (when fgeom
      (xwem-cl-correct-size-for-size cl fgeom))
    (xwem-cl-apply-xgeom-1 cl)

    ;; Reparent client
    (xwem-cl-put-sys-prop cl 'special-parent par-win)
    (XReparentWindow (xwem-dpy) win par-win
                     (X-Geom-x (xwem-cl-xgeom cl))
                     (X-Geom-y (xwem-cl-xgeom cl)))

    ;; Setup events handler for special frames
    (XSelectInput (xwem-dpy) win
                  (Xmask-or XM-FocusChange
                            XM-VisibilityChange XM-StructureNotify))
    (X-Win-EventHandler-add-new win 'xwem-special-evhandler)

    (xwem-select-client cl)))

(define-xwem-deferred xwem-special-apply-state (cl)
  "Apply CL's state to life."
  (case (xwem-cl-state cl)
    (active
     (if (X-Win-equal (xwem-cl-get-sys-prop cl 'special-parent)
                      (xwem-rootwin))
         (xwem-misc-raise-xwin (xwem-cl-xwin cl))
       (XRaiseWindow (xwem-dpy) (xwem-cl-xwin cl)))
     (XMapWindow (xwem-dpy) (xwem-cl-xwin cl)))

    ((inactive iconified)
     (XUnmapWindow (xwem-dpy) (xwem-cl-xwin cl))
     (xwem-special-revert-focus cl))))

(defun xwem-activate-emacs-special (cl &optional type)
  "Activate method for special emacs frame client CL."
  (xwem-special-apply-state cl))

(defun xwem-deactivate-emacs-special (cl &optional type)
  "Demanage specal xwem client CL."
  (cond ((eq type 'deactivate)
         (xwem-special-apply-state cl))))

(defun xwem-iconify-emacs-special (cl)
  "Iconify handler for special frame."
  (xwem-special-apply-state cl))


(provide 'xwem-special)

;;;; On-load actions:
;; Add manage type
(define-xwem-manage-model emacs-special
  "Managing model for special Emacs frames."
  :qualifier `(and (class-name ,(concat "^" x-emacs-application-class "$"))
                   (class-inst ,(concat "^" xwem-special-frame-name "$")))

  :cl-properties xwem-special-client-properties
  :manage-method 'xwem-manage-emacs-special
  :activate-method 'xwem-activate-emacs-special
  :deactivate-method 'xwem-deactivate-emacs-special
  :iconify-method 'xwem-iconify-emacs-special)

;; - Before init hook
(if xwem-started
    (xwem-special-frame-init)
  (add-hook 'xwem-before-init-hook 'xwem-special-frame-init))

;;; xwem-special.el ends here
