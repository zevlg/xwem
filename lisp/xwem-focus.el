;;; xwem-focus.el --- Controling focus under XWEM.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;; Created: Fri Dec 19 13:25:30 MSK 2003
;; Keywords: xwem, xlib
;; Time-stamp: <14/9/2007 22:20:09 lg@h1>

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

;; Various focus operations.

;;; Code:

(require 'xwem-load)

;;;###autoload
(defcustom xwem-default-focus-mode 'generic
  "*Default CL's focus mode."
  :type '(choice (const :tag "Generic mode" generic)
                 (const :tag "Click to focus" click-focus)
                 (const :tag "Follow mouse" follow-mouse))
  :group 'xwem)

;;; Internal variables

(defvar xwem-focus-stack nil
  "Last thing that has focus.
Internal variable, do not modify.")


;;;###xwem-autoload
(defun xwem-focus-xcurrent ()
  "Return current focus."
  (let ((cf (XGetInputFocus (xwem-dpy))))
    cf))

(defun xwem-focus-push (&optional xwin)
  "Push current focus or XWIN to `xwem-focus-stack'."
  (push (or xwin (xwem-focus-xcurrent)) xwem-focus-stack))

;;;###xwem-autoload
(defun xwem-focus-pop ()
  "Pop value from `xwem-focus-stack'."
  (pop xwem-focus-stack))

;;;###xwem-autoload
(defun xwem-focus-push-set (xwin)
  "Push current focus to `xwem-focus-stack' and set focus to XWIN."
  (xwem-focus-push)
  (XSetInputFocus (xwem-dpy) xwin X-RevertToParent))

;;;###xwem-autoload
(defun xwem-focus-pop-set ()
  "Pop from `xwem-focus-stack' and set focus."
  (let ((xwin (xwem-focus-pop)))
    (when (X-Win-p xwin)
      (XSetInputFocus (xwem-dpy) xwin X-RevertToParent))))

;;;###xwem-autoload
(defun xwem-focus-set (thing &optional push revert)
  "Set input focus to THING.
THING - one of X-Win, xwem-frame, or xwem-client.
PUSH  - Non-nil for pushing thing into `xwem-focus-stack'."
  (cond ((X-Win-p thing)                ; X11 window
         (when push
           (xwem-focus-push))
;         (if (= (X-Attr-mapstate (XGetWindowAttributes (xwem-dpy) thing)) X-Viewable)
         (XSetInputFocus (xwem-dpy) thing (or revert X-RevertToParent))

         ;; XXX Set input focus to root window, because that THING
         ;; is not viewable yet.
;           (xwem-message 'warning "Window is not viewable ... %S" (XGetWMName (xwem-dpy) thing))
;           (xwem-focus-set (xwem-dummy-client))))
         )

        ;; xwem client
        ((xwem-cl-p thing)
         ;; For Passive/Locally Active focus models
         (when (or (xwem-client-property thing 'ignore-has-input-p)
                   (eq xwem-client-focusing 'advanced)
                   (and (X-WMHints-input-p
                         (xwem-hints-wm-hints (xwem-cl-hints thing)))
                        (= (X-WMHints-input
                            (xwem-hints-wm-hints (xwem-cl-hints thing)))
                           1)))
           (xwem-focus-set (xwem-cl-xwin thing) push revert))

         ;; For Locally Active/Globally Active focus models
         (when (XWMProtocol-set-p
                (xwem-dpy) (xwem-hints-wm-protocols (xwem-cl-hints thing))
                "WM_TAKE_FOCUS")
           (xwem-client-sendmsg-atom
            thing
            (X-Atom-find-by-name (xwem-dpy) "WM_TAKE_FOCUS")
            (and (X-Event-p xwem-last-xevent)
                 (or (and (member (X-Event-type xwem-last-xevent)
                                  (list X-ButtonPress X-ButtonRelease))
                          (X-Event-xbutton-time xwem-last-xevent))
                     (and (member (X-Event-type xwem-last-xevent)
                                  (list X-KeyPress X-KeyRelease))
                          (X-Event-xkey-time xwem-last-xevent)))))))

        ;; xwem window
        ((xwem-win-p thing)
         (xwem-focus-set (xwem-win-frame thing) push revert))

        ;; xwem-frame
        ((xwem-frame-p thing)
         (let* ((cl (xwem-win-cl (xwem-frame-selwin thing)))
                ;; maybe cl is embedded frame?
                (embf (and (xwem-cl-p cl)
                           (X-Win-get-prop (xwem-cl-xwin cl) 'xwem-frame))))

           (if (xwem-frame-p embf)
               ;; embedded frame
               (xwem-focus-set embf push revert)

             (if (xwem-cl-p cl)
                 ;; Current client active
                 (xwem-focus-set cl push revert)
               (xwem-focus-set (xwem-frame-xwin thing) push revert)))))

        ;; Normally should not happen
        (t (xwem-focus-set (xwem-dummy-client)))))


;;; Focus modes support
(defvar xwem-focus-mode-names nil
  "List of valid focus modes.")

(define-xwem-client-property xwem-focus-mode nil
  "Client focus model."
  :type '(eval (list 'choice xwem-focus-mode-names))
  :set 'xwem-focus-set-focus-mode)

(defun xwem-focus-set-focus-mode (cl prop mode)
  "Set CL focus mode property PROP to MODE."
  (xwem-focus-mode-invoke cl 'before-mode-change)
  (xwem-cl-put-prop cl prop (or mode xwem-default-focus-mode))
  (xwem-focus-mode-invoke cl 'after-mode-change))

(defmacro define-xwem-focus-mode (name args &optional docstring &rest body)
  "Define new focus mode named by NAME.
FUN specifies function to call when focus changes."
  (let ((fun (or (and (functionp args) `(function ,args))
                 `(lambda ,args
                    ,docstring
                    ,@body))))
    `(progn
       (put (quote ,name) 'xwem-focus-mode ,fun)
       (add-to-list 'xwem-focus-mode-names
                    (cons (list 'const :tag ,docstring (quote ,name))
                          (quote ,fun))))))
(put 'define-xwem-focus-mode 'lisp-indent-function 'defun)

(defvar xwem-focus-last-client-leaved nil)

;;;###xwem-autoload
(defun xwem-focus-mode-invoke (cl &rest args)
  "Invoke CL's focus mode function with ARGS.
Invoke focus mode, car of ARGS normally type of invocation.
Built-in invocation types are:

  'before-mode-change   - Called before focus mode changed.
  'after-mode-change    - Called after focus mode has been changed.
  'focus-in             - When CL receives focus.
  'focus-out            - When CL looses focus.
  'enter                - When CL enters.
  'leave                - When CL leaves.
  'before-keymap-change - Before CL's local map changed.
  'after-keymap-change  - After CL's local map changed.
"
  (when (xwem-cl-p cl)
    (let ((ignore-invoke nil)
          fun)
      (when (eq (car args) 'leave)
        (setq xwem-focus-last-client-leaved cl))
      (when (and (eq (car args) 'enter)
                 (eq cl xwem-focus-last-client-leaved))
        (setq ignore-invoke t))

      (unless ignore-invoke
        (setq fun (get (xwem-client-property cl 'xwem-focus-mode) 'xwem-focus-mode))
        (when fun
          (apply fun cl args))))))

;;;###xwem-autoload
(defun xwem-focus-mode-set (cl &optional mode)
  "For CL window set focus mode to MODE.
If MODE is ommited, `xwem-default-focus-mode' is used."
  (xwem-focus-set-focus-mode cl 'xwem-focus-mode mode))

;; Some built-in focus modes
(define-xwem-focus-mode generic ignore "Generic mode")

(define-xwem-focus-mode follow-mouse (cl action &optional xev)
  "Focus follow mouse"
  (cond ((and (eq action 'enter)
              (= (X-Event-xcrossing-mode xev) X-NotifyNormal))
         (xwem-select-client cl))))

;;; Click to focus model
(defvar xwem-focus-click-to-focus-map
  (let ((map (make-sparse-keymap)))
    (define-key map [button1] 'xwem-focus-click-on)
    (define-key map [button2] 'xwem-focus-click-on)
    (define-key map [button3] 'xwem-focus-click-on)
    map)
  "Keymap used for click to focus model.")

(defvar xwem-focus-click-minor-mode nil
  "*Non-nil mean `xwem-focus-click-to-focus-map' is enabled.")
(xwem-make-variable-client-local 'xwem-focus-click-minor-mode)

(defun xwem-turn-on-focus-click-mode (cl)
  "On CL, turn on click to focus minor mode."
  (unless (xwem-client-local-variable-value cl 'xwem-focus-click-minor-mode)
    (when (xwem-misc-xwin-valid-p (xwem-cl-xwin cl))
      (xwem-kbd-install-grab xwem-focus-click-to-focus-map
                             (xwem-cl-xwin cl) X-GrabModeSync)
      (xwem-client-local-variable-set cl 'xwem-focus-click-minor-mode t))))

(defun xwem-turn-off-focus-click-mode (cl)
  "On CL, turn off click to focus minor mode."
  (when (xwem-client-local-variable-value cl 'xwem-focus-click-minor-mode)
    (when (xwem-misc-xwin-valid-p (xwem-cl-xwin cl))
      (XAllowEvents (xwem-dpy) X-ReplayPointer)
      (xwem-kbd-uninstall-grab xwem-focus-click-to-focus-map (xwem-cl-xwin cl))
      (xwem-client-local-variable-set cl 'xwem-focus-click-minor-mode nil))))

(defun xwem-focus-click-mode (cl)
  "On CL, toggle click to focus minor mode."
  (if (xwem-client-local-variable-value cl 'xwem-focus-click-minor-mode)
      (xwem-turn-off-focus-click-mode cl)
    (xwem-turn-on-focus-click-mode cl)))

;;;###autoload(autoload 'xwem-focus-click-on "xwem-focus" nil t)
(define-xwem-command xwem-focus-click-on ()
  "Command used by `click-focus' focus mode."
  (xwem-interactive)

  (when (xwem-cl-p xwem-event-client)
    (xwem-select-client xwem-event-client))

  ;; Pass the click
  (XAllowEvents (xwem-dpy) X-ReplayPointer))

(define-xwem-focus-mode click-focus (cl action &optional xev)
  "Click to focus"
  (cond ((and (eq action 'focus-in)
              (or (eq (X-Event-xfocus-mode xev) X-NotifyNormal)
                  (eq (X-Event-xfocus-mode xev) X-NotifyWhileGrabbed)))
         ;; Remove button[123] from local keymap and ungrab it
         (xwem-turn-off-focus-click-mode cl))

        ((and (eq action 'focus-out)
              (or (eq (X-Event-xfocus-mode xev) X-NotifyNormal)
                  (eq (X-Event-xfocus-mode xev) X-NotifyWhileGrabbed)))
         ;; Add button[123] to local keymap and grab for it
         (xwem-turn-on-focus-click-mode cl))

        ((memq action '(after-mode-change after-keymap-change))
         ;; Start grabing button1 in sync mode
         (unless (xwem-cl-selected-p cl)
           (xwem-turn-on-focus-click-mode cl)))

        ((memq action '(before-mode-change before-keymap-change))
         ;; Remove button1 from local keymap and ungrab it
         (xwem-turn-off-focus-click-mode cl))))

;; Register minor mode
(xwem-add-minor-mode 'xwem-focus-click-minor-mode "Click"
                     'xwem-focus-click-to-focus-map)


(provide 'xwem-focus)

;;; xwem-focus.el ends here
