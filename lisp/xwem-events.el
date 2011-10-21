;;; xwem-events.el --- Events handlers.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;; Created: 21 Mar 2003
;; Keywords: xlib, xwem
;; Time-stamp: <17/6/2011 14:59:32 lg@localhost>

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
;; This file used to work with X events, also includes some events
;; handlers.
;;
;;; Code

(require 'xwem-load)

;;;###xwem-autoload
(defun xwem-ev-reconfig (xdpy win xev)
  "Common ConfigureRequest handler."
  (let* ((win (X-Event-xconfigurerequest-window xev))
         (cl (xwem-xwin-cl win))
         (vmask (X-Event-xconfigurerequest-value-mask xev)))

    (xwem-debug 'xwem-event
                "XWEM-EVENTS: ConfigureRequest event for win=%s vmask=%s, x=%S, y=%S, width=%S, height=%S stack-mode=%d"
                '(X-Win-id win) 'vmask '(X-Event-xconfigurerequest-x xev) '(X-Event-xconfigurerequest-y xev)
                '(X-Event-xconfigurerequest-width xev) '(X-Event-xconfigurerequest-height xev)
                '(X-Event-xconfigurerequest-stackmode xev))

    (if (not (xwem-cl-p cl))
        (when (xwem-misc-xwin-valid-p win)
          ;; Not yet managed client
          (XConfigureWindow (xwem-dpy) win
                            :x (and (Xtest vmask X-CWX)
                                    (X-Event-xconfigurerequest-x xev))
                            :y (and (Xtest vmask X-CWY)
                                    (X-Event-xconfigurerequest-y xev))
                            :width (and (Xtest vmask X-CWWidth)
                                        (X-Event-xconfigurerequest-width xev))
                            :height (and (Xtest vmask X-CWHeight)
                                         (X-Event-xconfigurerequest-height xev))
                            :border-width (and (Xtest vmask X-CWBorderWidth)
                                               (X-Event-xconfigurerequest-border-width xev))
                            :sibling (and (Xtest vmask X-CWSibling)
                                          (X-Event-xconfigurerequest-sibling xev))
                            :stackmode (and (Xtest vmask X-CWStackMode)
                                            (X-Event-xconfigurerequest-stackmode xev))))

      ;; Client window already in air
      (if (not (Xtest vmask (Xmask-or X-CWX X-CWY X-CWWidth X-CWHeight X-CWBorderWidth)))
          (xwem-cl-send-config cl)

        ;; Geometry change
        (setf (xwem-cl-new-xgeom cl)
              (make-X-Geom :x (and (Xtest vmask X-CWX) (X-Event-xconfigurerequest-x xev))
                           :y (and (Xtest vmask X-CWY) (X-Event-xconfigurerequest-y xev))
                           :width (and (Xtest vmask X-CWWidth) (X-Event-xconfigurerequest-width xev))
                           :height (and (Xtest vmask X-CWHeight) (X-Event-xconfigurerequest-height xev))
                           :border-width (and (Xtest vmask X-CWBorderWidth) (X-Event-xconfigurerequest-border-width xev))))
        (xwem-refit cl)))))

;;;###xwem-autoload
(defun xwem-ev-resize (xdpy win xev)
  "Handle ResizeRequest event."
  (let ((cl (xwem-xwin-cl (X-Event-xresizerequest-window xev))))
    (when (xwem-cl-p cl)
      (xwem-client-resize cl (X-Event-xresizerequest-width xev)
                          (X-Event-xresizerequest-height xev)))))

;;;###xwem-autoload
(defun xwem-ev-mapreq (xdpy win xev)
  "Handle MapRequest event."
  (let ((cl (xwem-xwin-cl (X-Event-xmaprequest-window xev))))
    (if (xwem-cl-p cl)
        ;; Transition from Withdrawn->Normal/Iconic state
        (xwem-cl-honour-init-state cl)
      ;; Initial window manage
      (xwem-xwin-try-to-manage (X-Event-xmaprequest-window xev)))))

;;;###xwem-autoload
(defun xwem-ev-unmap (xdpy win xev)
  "Handle UnmapNotify event."
  ;; NOTE: Obsolete X clients which does not send synthetic
  ;; UnmapNotify event (as described in ICCCM 4.1.4) to transit to
  ;; withdraw state, are not supported.
  (let (cl)
    (when (and (X-Event-synth-p xev)
               (not (X-Event-xunmap-from-configure xev))
               (xwem-cl-p (setq cl (xwem-xwin-cl (X-Event-xunmap-window xev))))
               (xwem-cl-active-p cl))
      (xwem-withdraw cl))))

;;;###xwem-autoload
(defun xwem-ev-destroy (xdpy win xev)
  "Handle Destroy event."
  (let ((cl (xwem-xwin-cl (X-Event-xdestroywindow-window xev))))
    (when (xwem-cl-p cl)
      (xwem-cl-destroy cl))))

;;;; -- Events, command events, stuff --
;;;###xwem-autoload
(defun xwem-event-client (xev)
  "Return client where X event XEV occured."
  (let ((ecl (and (X-Event-p xev)
                  (X-Win-p (X-Event-win xev))
                  (xwem-xwin-cl (X-Event-win xev)))))
    (if (or (not (xwem-cl-alive-p ecl))
            (eq ecl (xwem-dummy-client)))
        (xwem-cl-selected)
      ecl)))

;;;###xwem-autoload
(defun xwem-next-event (&optional timeout evt-list)
  "Fetch next Emacs keyboard or mouse event, with corresponding X Event.

If EVT-LIST is given, stop when event of type that in EVT-LIST is
occured.  Default value of EVT-LIST is `(list X-KeyPress X-ButtonPress
X-ButtonRelease X-MotionNotify)'.

Return Emacs event.  To acces corresponding X Event use
`(event-object ev)' form."
  (let ((timo (and timeout (add-timeout timeout nil 'xwem-timeout)))
        (nev (allocate-event))
        (obj nil))

    (while (progn
             (next-event nev)
             (not (cond ((and (timeout-event-p nev)
                              (eq (event-object nev) 'xwem-timeout))
                         (setq timo nil) ; unset it
                         t)

                        ((and (eval-event-p nev)
                              (X-Event-p (event-object nev))
                              (memq (X-Event-type (event-object nev))
                                    (or evt-list
                                        (list X-KeyPress X-ButtonPress
                                              X-ButtonRelease X-MotionNotify))))
                         ;; next-event can fetch only
                         ;; keypress/buttonpress/buttonrelease/motion
                         ;; events
                         (setq obj (event-object nev))))))
      (dispatch-event nev))

    (when timo
      (disable-timeout timo))
    (deallocate-event nev)
    obj))

(defun xwem-xevent-emacs-event (xev)
  "Return Emacs event corresponding to X Event XEV."
  (X-Event-get-property xev 'emacs-event))

(defsetf xwem-xevent-emacs-event (xev) (eev)
  `(X-Event-put-property ,xev 'emacs-event ,eev))


;;;###xwem-autoload
(defun xwem-event-as-command (e-ev &optional x-ev)
  "Interpret event E-EV as command event.
Optional X-EV specifies corresponding X Event."
  ;; Remember some information about command invocation
  (setq xwem-last-xevent x-ev
        xwem-event-client (xwem-event-client x-ev)
        xwem-last-event e-ev
        xwem-this-command-keys (vconcat (and (not (xwem-kbd-global-map-current-p))
                                             xwem-this-command-keys)
                                        (vector e-ev))))


;;;###xwem-autoload
(defun xwem-next-command-event (&optional prompt)
  "Return next command event.
Actually return cons cell where car is Emacs event and cdr is X Event."
  (declare (special xwem-keymacro-keys xwem-keymacro-keys-index))
  (let (eev cev xev)
    ;; Normal
    (when prompt
      (xwem-message 'prompt prompt))

    (if (xwem-keymacro-executing-p)
        (setq cev (aref (xwem-kbd-ekeys->eevents
                         (list (aref xwem-keymacro-keys
                                     (incf xwem-keymacro-keys-index)))) 0))

      ;; Process while interesting event occur
      (while (and (setq eev (next-event))
                  (not (cond ((and (eval-event-p eev)
                                   (X-Event-p (setq xev (event-object eev)))
                                   (memq (X-Event-type xev)
                                         (list X-KeyPress X-ButtonPress
                                               X-ButtonRelease X-MotionNotify))
                                   (setq cev (car (xwem-xevents->emacs-events (list xev) t))))
                              (X-Event-put-property xev 'emacs-event cev)
                              'break)

                             ((and (eval-event-p eev)
                                   (eventp (setq cev (event-object eev)))
                                   (eq (event-function eev) 'xwem-dispatch-command-event))
                              ;; Unread command event
                              (setq xev nil)
                              'break))))
        (dispatch-event eev)))

    (when prompt
      (xwem-clear-message))

    (xwem-event-as-command cev xev)
    (cons cev xev)))

;;;###xwem-autoload
(defun xwem-dispatch-command-event (eev &optional xev)
  "Dispatch command Emacs event EEV."
  (let* ((vev (or (and (vectorp eev) eev) (vector eev)))
         (ecl (xwem-event-client xev))
         (bind (or (xwem-lookup-key ecl vev)
                   ;; Then check for quit key
                   (and (eventp vev)
                        (equal xwem-quit-key
                               (events-to-keys vev))
                        xwem-quit-command)
                   ;; Then accept even default bindings
                   (xwem-lookup-key ecl vev t))))
    ;; If some button press/release does not have binding - ignore it
    (unless (and (null bind) (button-event-p eev))
      (xwem-event-as-command eev xev)
      ;; In case EEV is set of events - adjust command keys
      (when (vectorp eev)
        (setq xwem-this-command-keys eev))
      (xwem-kbd-dispatch-binding bind))))

;;;###xwem-autoload
(defun xwem-dispatch-command-xevent (xev)
  "Dispatch command event XEV."
  (unwind-protect
      ;; If we are grabbing keyboard now and modifier pressed do nothing.
      (unless (or (= (X-Event-type xev) X-KeyRelease)
                  (and (= (X-Event-type xev) X-KeyPress)
                       (xwem-kbd-kcode-modifier-p (X-Event-xkey-keycode xev))))
        (setf (xwem-xevent-emacs-event xev)
              (car (xwem-xevents->emacs-events (list xev) t)))
        (xwem-dispatch-command-event
         (xwem-xevent-emacs-event xev) xev))

    ;; Make sure to allow next event
    (XAllowEvents (X-Event-dpy xev) X-AsyncBoth)))

;;; Unread command events support
;;;###xwem-autoload
(defun xwem-unread-command-event (eev-or-xev)
  "Make event EV to be readed by `xwem-next-command-event' later,
or to be executed by `xwem-dispatch-command-event'.
Event EV can be either Emacs event, or X-Event."
  (enqueue-eval-event (if (X-Event-p eev-or-xev)
                          'xwem-dispatch-command-xevent
                        'xwem-dispatch-command-event)
                      eev-or-xev))


(provide 'xwem-events)

;;; xwem-events.el ends here
