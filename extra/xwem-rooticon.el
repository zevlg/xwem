;;; xwem-rooticon.el --- Support Icons on root window.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Date lost, it might be on 2004 winter
;; Keywords: xwem
;; Time-stamp: <8/8/2008 08:29:02 lg@h1.lan>

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

;;; Code:

(require 'xwem-load)
(require 'xwem-misc)

(require 'xlib-xpm)
(require 'xlib-xshape)

(defgroup xwem-rooticon nil
  "Group to customize rooticon behaviour."
  :prefix "xwem-rooticon-"
  :group 'xwem)

(defcustom xwem-rooticon-placing 'behind-minibuffer
  "*Placing behaviour."
  :type '(choice (const :tag "Behind minibuffer" behind-minibuffer)
                 (const :tag "Random" random))
  :group 'xwem-rooticon)

(defcustom xwem-rooticon-show-label nil
  "*Non-nil mean show Icon name in rooticon."
  :type 'boolean
  :group 'xwem-rooticon)

(defcustom xwem-rooticon-default-icon "root-icon.xpm"
  "*Default rooticon to use.
This icon is used which client does not have its own icon."
  :type 'file
  :group 'xwem-rooticon)

(defcustom xwem-rooticon-default-show-label t
  "*Non-nil mean show label, when `xwem-rooticon-default-icon' is used."
  :type 'boolean
  :group 'xwem-rooticon)

(defcustom xwem-rooticon-always-on-top-spec '(((t) . 15))
  "*List of cons cells in format:
\(QUALIFIER . RANK\) for always-on-top icons.
If QUALIFIER matches rooticon's client - than RANK is set as always
on top rank."
  :type 'sexp
  :group 'xwem-rooticon)

(defcustom xwem-rooticon-skip-spec
  '(predicate xwem-minibuffer-client-p)
  "*QUALIFIER for clients that does not have root icons.
By default, xwem minibuffer does not have root icon."
  :type 'sexp
  :group 'xwem-rooticon)

(defcustom xwem-rooticon-notify-p t
  "*Non-nil mean rooticon flashing is enabled.

Rooticon flashes when WM_ICON_NAME changes while client is iconified.
This is useful to track activity of iconified clients.  For example
you started compilation in xterm and iconified it, when compilation
completes rooticon will flash.  However this requires addition
settings in client application, for example in xterm case you should
setup your shell to change xterm's title showing current shell
command.  In case of Z-shell this is done by `precmd' and `preexec'
functions.  For example:

    precmd () { print -Pn \"\e]0;[%?]\a\" }
    preexec () { print -Pn \"\e]0;[$1]\a\" }
"
  :type 'boolean
  :group 'xwem-rooticon)

(defcustom xwem-rooticon-notify-parameters
  '(:flash-rooticon t :flash-pause 0.3 :ready-sound t :update-recency t)
  "*Pause in seconds between rooticon flashes.
Makes sense only if `xwem-rooticon-notify-p' is non-nil."
  :type 'list
  :group 'xwem-rooticon)

;;; Internal variables

(defstruct xwem-rooticon
  cl
  xgeom
  xriwin
  xiconwin
  xpixmap
  xpixmask)

(define-xwem-face xwem-rooticon-face
  `((t (:foreground "black" :background "tan"
        :font "-misc-fixed-medium-r-*-*-10-*-*-*-*-*-*-*")))
  "Face to draw text on root icon.")

(defvar xwem-rooticon-map
  (let ((map (make-sparse-keymap)))
    (define-key map [button1] 'xwem-rooticon-smart-move)
    (define-key map [button1up] 'xwem-rooticon-select-cl)
    (define-key map [button3] 'xwem-rooticon-menu)
    map)
  "Keymap for rooticon windows.")

(defvar xwem-rooticon-default-pixmap nil)

(defun xwem-rooticon-ev-handler (xdpy xwin xev)
  (let ((ri (X-Win-get-prop xwin 'xwem-rooticon)))
    (when (xwem-rooticon-p ri)
      (X-Event-CASE xev
        ((:X-ButtonPress :X-ButtonRelease)
         (xwem-overriding-local-map xwem-rooticon-map
           (let ((xwem-click-rooticon ri))
             (declare (special xwem-click-rooticon))
             (xwem-dispatch-command-xevent xev))))

        (:X-Expose
         (xwem-rooticon-draw
	  ri (X-Event-xexpose-x xev) (X-Event-xexpose-y xev)
	  (X-Event-xexpose-width xev) (X-Event-xexpose-height xev)))

        (:X-DestroyNotify
         (xwem-cl-rem-sys-prop (xwem-rooticon-cl ri) 'xwem-rooticon)
         (xwem-misc-unset-always-on-top xwin)
         (X-invalidate-cl-struct ri))))))

(defun xwem-rooticon-icons ()
  "Return list of root icons sorted by X."
  (sort (delq nil (mapcar (lambda (cl)
                            (xwem-cl-get-sys-prop cl 'xwem-rooticon))
                          xwem-clients))
        (lambda (ri1 ri2)
          (> (X-Geom-x (xwem-rooticon-xgeom ri1))
             (X-Geom-x (xwem-rooticon-xgeom ri2))))))

(defun xwem-rooticon-select-place (cl w h)
  "Select place for rooticon."
  (or (xwem-client-property cl 'rooticon-position)
      (cond ((eq xwem-rooticon-placing 'behind-minibuffer)
             ;; Behind the minibuffer
             (let ((ris
		    (sort
		     (delq
		      nil
		      (mapcar
		       (lambda (cl)
			 (let ((ri (xwem-cl-get-sys-prop cl 'xwem-rooticon)))
			   (and ri (>= (+ (X-Geom-y (xwem-rooticon-xgeom ri))
					  (X-Geom-height
					   (xwem-rooticon-xgeom ri)))
				       (- (X-Geom-height (xwem-rootgeom)) h))
				ri)))
		       xwem-clients))
		     (lambda (ri1 ri2)
		       (< (X-Geom-x (xwem-rooticon-xgeom ri1))
			  (X-Geom-x (xwem-rooticon-xgeom ri2))))))
                   ri1 ri2
                   (x 0))
               (while ris
                 (setq ri1 (car ris)
                       ri2 (cadr ris))
                 (setq x (+ (X-Geom-x (xwem-rooticon-xgeom ri1))
                            (X-Geom-width (xwem-rooticon-xgeom ri1))))
                 (when (and ri1 ri2
                            (>= (- (X-Geom-x (xwem-rooticon-xgeom ri2)) x) w))
                   (setq ris nil))
                 (setq ris (cdr ris)))
	       (cons x (- (X-Geom-height (xwem-rootgeom)) h))))

            ((eq xwem-rooticon-placing 'random)
             (cons (random (- (X-Geom-width (xwem-rootgeom)) w))
                   (random (- (X-Geom-height (xwem-rootgeom)) h)))))))

(defun xwem-rooticon-create (cl)
  "Create rooticon for CL."
  (let ((wmh (xwem-hints-wm-hints (xwem-cl-hints cl)))
        (ri (make-xwem-rooticon :cl cl))
        place)
    ;; Fill pixmap/mask fields
    (cond ((and (X-WMHints-iconpixmap-p wmh)
                (= (XGetDepth (xwem-dpy) (xwem-rootwin))
                   (XGetDepth (xwem-dpy) (make-X-Pixmap
					  :id (X-WMHints-icon-pixmap wmh)))))
           ;; Has a pixmap of same depth as root window
           (setf (xwem-rooticon-xpixmap ri)
                 (make-X-Pixmap :id (X-WMHints-icon-pixmap wmh)))
           (when (X-WMHints-iconmask-p wmh)
             (setf (xwem-rooticon-xpixmask ri)
                   (make-X-Pixmap :id (X-WMHints-icon-mask wmh)))))
          (t (unless xwem-rooticon-default-pixmap
               (setq xwem-rooticon-default-pixmap
                     (cons (X:xpm-pixmap-from-file
                            (xwem-dpy) (xwem-rootwin)
                            (xwem-icon-find-file xwem-rooticon-default-icon))
                           (X:xpm-pixmap-from-file
                            (xwem-dpy) (xwem-rootwin)
                            (xwem-icon-find-file xwem-rooticon-default-icon)
                            t))))
             (setf (xwem-rooticon-xpixmap ri)
		   (car xwem-rooticon-default-pixmap))
             (setf (xwem-rooticon-xpixmask ri)
		   (cdr xwem-rooticon-default-pixmap))))

    (setf (xwem-rooticon-xgeom ri)
          (XGetGeometry (xwem-dpy) (xwem-rooticon-xpixmap ri)))

    (setq place (xwem-rooticon-select-place
                 cl (X-Geom-width (xwem-rooticon-xgeom ri))
                 (X-Geom-height (xwem-rooticon-xgeom ri))))
    (setf (X-Geom-x (xwem-rooticon-xgeom ri)) (car place))
    (setf (X-Geom-y (xwem-rooticon-xgeom ri)) (cdr place))
    (setf (xwem-rooticon-xriwin ri)
          (XCreateWindow (xwem-dpy) (xwem-rootwin)
                         (X-Geom-x (xwem-rooticon-xgeom ri))
                         (X-Geom-y (xwem-rooticon-xgeom ri))
                         (X-Geom-width (xwem-rooticon-xgeom ri))
                         (X-Geom-height (xwem-rooticon-xgeom ri))
                         0 nil nil nil
                         :override-redirect t
                         :event-mask (Xmask-or XM-ButtonPress
                                               XM-ButtonRelease
                                               XM-ButtonMotion
                                               XM-Exposure)))

    ;; Apply mask
    (when (xwem-rooticon-xpixmask ri)
      (X-XShapeMask (xwem-dpy) (xwem-rooticon-xriwin ri)
                    X-XShape-Bounding X-XShapeSet 0 0
                    (xwem-rooticon-xpixmask ri)))

    (X-Win-EventHandler-add (xwem-rooticon-xriwin ri)
                            'xwem-rooticon-ev-handler nil
                            (list X-ButtonPress X-ButtonRelease
                                  X-DestroyNotify X-Expose))
    (X-Win-put-prop (xwem-rooticon-xriwin ri) 'xwem-rooticon ri)
    (xwem-cl-put-sys-prop cl 'xwem-rooticon ri)
    ri))

(defun xwem-rooticon-draw (ri &optional x y w h)
  (XCopyArea (xwem-dpy) (xwem-rooticon-xpixmap ri)
             (xwem-rooticon-xriwin ri) (XDefaultGC (xwem-dpy))
             (or x 0) (or y 0)
             (or w (X-Geom-width (xwem-rooticon-xgeom ri)))
             (or h (X-Geom-height (xwem-rooticon-xgeom ri)))
             (or x 0) (or y 0))

  ;; Icon label
  (when (or xwem-rooticon-show-label
            (and xwem-rooticon-default-show-label
                 (eq (car xwem-rooticon-default-pixmap)
                     (xwem-rooticon-xpixmap ri))
                 (eq (cdr xwem-rooticon-default-pixmap)
                     (xwem-rooticon-xpixmask ri))))
    (let ((nname (or (and (> (length (xwem-cl-wm-icon-name
                                      (xwem-rooticon-cl ri))) 0)
                          (xwem-cl-wm-icon-name (xwem-rooticon-cl ri)))
                     (xwem-cl-wm-name (xwem-rooticon-cl ri)))))
      (XImageString (xwem-dpy) (xwem-rooticon-xriwin ri)
                    (xwem-face-get-gc 'xwem-rooticon-face nil
                                      (xwem-rooticon-cl ri))
                    0 (- (X-Geom-height (xwem-rooticon-xgeom ri))
                         (X-Text-descent (xwem-dpy)
                          (X-Gc-font (xwem-face-get-gc 'xwem-rooticon-face))
                          nname))
                    nname))))

;;; Hooking into clients handling
(define-xwem-deferred xwem-rooticon-apply-state (ri)
  "Show/hide rooticon RI according to RI's client state."
  (when (and (xwem-rooticon-p ri)
	     (xwem-cl-p (xwem-rooticon-cl ri)))
    (case (xwem-cl-state (xwem-rooticon-cl ri))
      (iconified
       (xwem-misc-lower-xwin (xwem-rooticon-xriwin ri))
       (XMapWindow (xwem-dpy) (xwem-rooticon-xriwin ri))
       (xwem-rooticon-draw ri))

      (t (XUnmapWindow (xwem-dpy) (xwem-rooticon-xriwin ri))))))

(defun xwem-rooticon-cl-state-change-hook (cl old-state new-state)
  "Handle CL's state change."
  (let ((ri (xwem-cl-get-sys-prop cl 'xwem-rooticon)))
    ;; Create rooticon if not yet created
    (when (and (eq new-state 'iconified)
               (not ri)
               (not (xwem-cl-match-p cl xwem-rooticon-skip-spec)))
      (setq ri (xwem-rooticon-create cl)))
    (when ri
      ;; Set always on top rank (if any)
      (let ((rank (find cl xwem-rooticon-always-on-top-spec
                        :key 'car :test 'xwem-cl-match-p)))
        (setf (xwem-xwin-rank (xwem-rooticon-xriwin ri)) (cdr rank)))

      (xwem-rooticon-apply-state ri))))

(defun xwem-rooticon-cl-destroy (cl)
  (let ((ri (xwem-cl-get-sys-prop cl 'xwem-rooticon)))
    (when ri
      (xwem-cl-rem-sys-prop cl 'xwem-rooticon)
      (xwem-misc-unset-always-on-top (xwem-rooticon-xriwin ri))
      (XDestroyWindow (xwem-dpy) (xwem-rooticon-xriwin ri))
      (X-invalidate-cl-struct ri))))

(defun xwem-rooticon-cl-change-hook (cl)
  (when (and xwem-rooticon-notify-p
             (xwem-cl-iconified-p cl)
             ;; XXX hack, check that we are running inside
             ;; `xwem-cl-hproperty'
             (boundp 'atom-id)
             (= (symbol-value 'atom-id) (X-Atom-id XA-wm-icon-name)))
    ;; Iconified and icon name changed!
    (let* ((ri (xwem-cl-get-sys-prop cl 'xwem-rooticon))
           (rig (and (xwem-rooticon-p ri)
                     (xwem-rooticon-xgeom ri))))
      ;; XXX update CL's recency so `H-x B' will select it first
      (when (plist-get xwem-rooticon-notify-parameters :update-recency)
        (setf (xwem-cl-recency cl) (current-time)))
      (when (plist-get xwem-rooticon-notify-parameters :ready-sound)
        (xwem-play-sound 'ready))
      (when (plist-get xwem-rooticon-notify-parameters :flash-rooticon)
        (xwem-misc-flash-rectangle
         (X-Geom-x rig) (X-Geom-y rig) (X-Geom-width rig) (X-Geom-height rig)
         nil (or (plist-get xwem-rooticon-notify-parameters :flash-pause) 0.3))))))

(defun xwem-rooticon-init ()
  "Initialize root icons."
  (xwem-message 'init "Initializing root icons ...")
  (add-hook 'xwem-cl-state-change-hook 'xwem-rooticon-cl-state-change-hook)
  (add-hook 'xwem-cl-destroy-hook 'xwem-rooticon-cl-destroy)
  (add-hook 'xwem-cl-change-hook 'xwem-rooticon-cl-change-hook)
  (xwem-message 'init "Initializing root icons ... done"))

;;; Commands
(define-xwem-command xwem-rooticon-smart-move ()
  "Interactively move rooticon.
If only clicked(not moving) bypass button release event."
  (xwem-interactive)

  (unless (button-press-event-p xwem-last-event)
    (error 'xwem-error
	   "`xwem-rooticon-smart-move' must be bound to mouse event"))

  (let ((xev (xwem-next-event nil (list X-ButtonRelease X-MotionNotify))))
    (X-Event-CASE xev
      (:X-ButtonRelease
       (xwem-dispatch-command-xevent xev))

      (:X-MotionNotify
       (declare (special xwem-click-rooticon))
       (let ((sx (- (X-Event-xmotion-root-x xev)
                    (X-Geom-x (xwem-rooticon-xgeom xwem-click-rooticon))))
             (sy (- (X-Event-xmotion-root-y xev)
                    (X-Geom-y (xwem-rooticon-xgeom xwem-click-rooticon))))
             (done nil))
         (xwem-mouse-grab
	  xwem-cursor-move (xwem-rooticon-xriwin xwem-click-rooticon)
	  (Xmask-or XM-ButtonMotion XM-ButtonRelease))
         (xwem-unwind-protect
             (while (not done)
               (X-Event-CASE
                   (setq xev (xwem-next-event
                              nil (list X-MotionNotify X-ButtonRelease)))
                 (:X-ButtonRelease (setq done t))

                 (:X-MotionNotify
                  (setf (X-Geom-x (xwem-rooticon-xgeom xwem-click-rooticon))
                        (- (X-Event-xmotion-root-x xev) sx))
                  (setf (X-Geom-y (xwem-rooticon-xgeom xwem-click-rooticon))
                        (- (X-Event-xmotion-root-y xev) sy))
                  (XMoveWindow
		   (xwem-dpy) (xwem-rooticon-xriwin xwem-click-rooticon)
		   (X-Geom-x (xwem-rooticon-xgeom xwem-click-rooticon))
		   (X-Geom-y (xwem-rooticon-xgeom xwem-click-rooticon))))))
           (xwem-mouse-ungrab)))))))

(define-xwem-command xwem-rooticon-select-cl ()
  "Select roowin client."
  (xwem-interactive)

  (unless (or (button-press-event-p xwem-last-event)
              (button-release-event-p xwem-last-event))
    (error 'xwem-error
	   "`xwem-rooticon-select-cl' must be bound to mouse event"))

  (declare (special xwem-click-rooticon))
  (let ((ricl (xwem-rooticon-cl xwem-click-rooticon)))
    (when (and (xwem-cl-p ricl) (xwem-cl-managed-p ricl))
      (if (xwem-dummy-client-p ricl)
          (xwem-activate ricl)
        (xwem-select-client ricl)))))

(defun xwem-rooticon-genmenu (ri)
  "Generate menu for rooticon RI."
  ;; XXX menu title
  (list (if (> (length (xwem-cl-wm-icon-name (xwem-rooticon-cl ri))) 18)
            (concat (substring (xwem-cl-wm-icon-name (xwem-rooticon-cl ri)) 0 16) "..")
          (xwem-cl-wm-icon-name (xwem-rooticon-cl ri)))
        (vector "Select" `(xwem-select-client ,(xwem-rooticon-cl ri)))
        (vector "Info" `(xwem-client-info ,(xwem-rooticon-cl ri)))
        (vector "Mark" `(if (xwem-cl-marked-p ,(xwem-rooticon-cl ri))
                            (xwem-client-unset-mark ,(xwem-rooticon-cl ri))
                          (xwem-client-set-mark ,(xwem-rooticon-cl ri)))
                :style 'toggle :selected `(xwem-cl-marked-p ,(xwem-rooticon-cl ri)))
        "--"
        (vector "Close" `(xwem-client-kill ,(xwem-rooticon-cl ri)))
        (vector "Kill" `(xwem-client-kill ,(xwem-rooticon-cl ri) '(4)))))

(define-xwem-command xwem-rooticon-menu ()
  "Popup rooticon menu."
  (xwem-interactive)
  (let ((ri (X-Win-get-prop (X-Event-win xwem-last-xevent) 'xwem-rooticon)))
    (when (xwem-rooticon-p ri)
      (xwem-popup-menu (xwem-rooticon-genmenu ri)))))

(defun xwem-rooticon-set-position (cl prop val)
  "Set CL's rooticon position property PROP to VAL."
  (let ((ri (xwem-cl-get-sys-prop cl 'xwem-rooticon)))
    (if (not (xwem-rooticon-p ri))
        (xwem-cl-put-prop cl prop val)

      (setf (X-Geom-x (xwem-rooticon-xgeom ri)) (car val))
      (setf (X-Geom-y (xwem-rooticon-xgeom ri)) (cdr val))
      (XMoveWindow (xwem-dpy) (xwem-rooticon-xriwin ri)
                   (X-Geom-x (xwem-rooticon-xgeom ri))
                   (X-Geom-y (xwem-rooticon-xgeom ri))))))

(defun xwem-rooticon-get-position (cl prop)
  "Return CL's rooticon position property PROP."
  (let ((ri (xwem-cl-get-sys-prop cl 'xwem-rooticon)))
    (if (not (xwem-rooticon-p ri))
        (xwem-cl-get-prop cl prop)
      (cons (X-Geom-x (xwem-rooticon-xgeom ri))
            (X-Geom-y (xwem-rooticon-xgeom ri))))))

(define-xwem-client-property rooticon-position nil
  "Client's rooticon position."
  :type '(cons (number :tag "X")
               (number :tag "Y"))
  :set 'xwem-rooticon-set-position
  :get 'xwem-rooticon-get-position)


(provide 'xwem-rooticon)

;;;; On-load actions:
(if xwem-started
    (xwem-rooticon-init)
  (add-hook 'xwem-before-init-wins-hook 'xwem-rooticon-init))

;;; xwem-rooticon.el ends here
