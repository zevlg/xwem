;;; xwem-keymacro.el --- Recording/playing keyboard macros.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;; Created: Fri Dec 12 17:18:00 MSK 2003
;; Keywords: xwem, xlib
;; Time-stamp: <21/10/2011 21:23:13 lg@localhost>

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

;; XWEM uses Record X extension to collect keypresses when saving
;; keyboard macros, this method is tranparent to user and allow
;; catching events without need of keyboard grabbing.

;; XWEM keyboard macro is the same as Emacs keyboard macro.  It is a
;; vector of Emacs events.  XWEM uses special keymap -
;; `xwem-user-macros-map' to hold user defined macros.  This keymap
;; used to save/restore keyboard macros across XWEM sessions.

;; XWEM's support for keyboard macros is full-featured.  It includes
;; H-x q command to start recursive edition, command which allow to
;; change keyboard macro execution on fly.  It has workaround Emacs
;; blocking, when Emacs enters minibuffer and block waiting for user
;; input, keyboard macro execution stops (deadlock! Emacs can't send
;; further input, because it waiting for this input), but when Emacs
;; is about to block, i.e. enter `read-from-minibuffer' XWEM installs
;; special itimer which track Emacs state and send next event if need.
;; This feature allow you to create keyboard macros which requires
;; interaction with user, for example 'H-x r xterm RET H-u H-z H-x b
;; emac RET'.  When defining keyboard macro remember that XWEM can't
;; track window operations such as map, configure, destroy and others,
;; so please use `xwem-misc-pause' command(default binded to H-z to
;; sleep for a while).  For example you need to define macro - 1)
;; start new xterm 2) in newly managed xterm enter "test it" text.  It
;; will be something like: H-x ( H-a x H-u H-z test it H-x ), because
;; when you start new xterm application it does not appear on screen
;; immediately, but when executing keyboard macro all keystrokes are
;; sent to X server as fast as possible.

;; To save keyboard macros on on exit add something like:
;;
;;    (add-hook 'xwem-exit-hook 'xwem-keymacro-save-macros)
;;
;; And to restore saved keyboard macros on start do next:
;;
;;    (add-hook 'xwem-after-init-hook 'xwem-keymacro-load-macros)
;;

;;; Code:


(require 'xlib-xtest)
(require 'xlib-xrecord)

(require 'xwem-load)
(require 'xwem-misc)


;; Macros customization
(defcustom xwem-keymacro-minib-bg "gray60"
  "*Background color for xwem's minibuffer while recording KBD macro.
If nil - background will not change."
  :type 'color
  :group 'xwem-keyboard)

(defcustom xwem-keymacro-debug nil
  "*Non-nil mean run keyboard macrosing stuff in debug mode."
  :type 'boolean
  :group 'xwem-keyboard)

(defcustom xwem-keymacro-show-macro nil
  "*Non-nil mean show keyboard macro in minibuffer, while executing."
  :type 'boolean
  :group 'xwem-keyboard)

(defcustom xwem-keymacro-macrofile "xwem-macros.el"
  "*Default filename where keyboard macros stores."
  :type 'file
  :group 'xwem-keyboard)

;;; Internal variables


;;; Macros recording/playing internal variables
;;;###xwem-autoload
(defvar xwem-keymacro-macros-stack nil
  "List of defined keyboard macroses.")

(defvar xwem-keymacro-macros-depth nil
  "Current depth of keyboard macro execution.
INTERNAL VARIABLE.")

(defvar xwem-keymacro-default-command 'xwem-keymacro-default
  "Default command while recording/playing macro.
NOTE: may be changed to play macro.")

(defvar xwem-keymacro-prefix-arg nil
  "Value of `xwem-prefix-arg' when entering macro recording.
Internal variable, do not modify directly.")

(defvar xwem-keymacro-minib-old-bg nil
  "Variable used to hold old backgroup of xwem's minibuffer.
Internal variable, do not modify directly.")

(defvar xwem-keymacro-saving nil
  "Non-nil mean that we saving macro now.
Internal variable, do not modify it directly.")
(defvar xwem-keymacro-dpy nil
  "Display used as data connection.
Internal variable, do not modify it directly.")
(defvar xwem-keymacro-rcontext nil
  "Record context used when keymacrosing.
Internal variable, do not modify it directly.")
(defvar xwem-keymacro-rclient nil
  "Client used when keymacrosing.
Internal variable, do not modify it directly.")
(defvar xwem-keymacro-rranges nil
  "Record ranges used when keymacrosing.
Internal variable, do not modify it directly.")

(defvar xwem-keymacro-initialized nil
  "Non-nil when keyboard macrosing initialized.
Internal variable, do not modify it directly.")

(defun xwem-keymacro-init ()
  "Initialize keyboard macrosing stuff."
  (pushnew '(macro "Macro") xwem-messages-label-prefixes)

  ;; Use xlib-xrecord extension to intercept KeyPress/KeyRelease
  ;; events.
  (let ((xrec-ext (X-XRecordQueryVersion (xwem-dpy)))
        (xtest-ext (XQueryExtension (xwem-dpy) "XTEST")))
    (if (or (null (car xrec-ext)) (null (car xtest-ext)))
        ;; No XRECORD or XTEST extension support
        (xwem-message 'error "RECORD or XTEST extension missing")

      ;; (xwem-dpy) supports XRECORD extension
      (setq xwem-keymacro-rcontext (make-X-RecordContext
                                    :dpy (xwem-dpy)
                                    :id (X-Dpy-get-id (xwem-dpy))))
      (setq xwem-keymacro-rranges
            ;; We are only interested in KeyPrees/KeyRelease events
            (list (make-X-RecordRange
                   :device-events (cons X-KeyPress X-KeyRelease))))

      (setq xwem-keymacro-rclient (float X-XRecordAllClients))
      (setq xwem-keymacro-rcontext (X-XRecordCreateContext
                                    (xwem-dpy) xwem-keymacro-rcontext 0
                                    (list xwem-keymacro-rclient)
                                    xwem-keymacro-rranges))
      (X-XRecordRegisterClients (xwem-dpy) xwem-keymacro-rcontext 0
                                (list xwem-keymacro-rclient)
                                xwem-keymacro-rranges)

      (setq xwem-keymacro-dpy (XOpenDisplay
                               (format "%s:%d" (X-Dpy-name (xwem-dpy))
                                       (X-Dpy-default-screen (xwem-dpy)))))
      (when xwem-keymacro-debug
        (setf (X:Dpy-log-buffer xwem-keymacro-dpy) "XREC.log"))

      (setq xwem-keymacro-initialized t))))

(defun xwem-keymacro-extract (xevs &optional cutlen)
  "Extract keyboard macro from X-Events list XEVS.
Return list of Emacs events.
CUTLEN is how many events cut from the end (default is 1)."
  (let ((evs (butlast (xwem-xevents->emacs-events xevs nil)
                      (or cutlen 1))))
    (key-sequence-list-description (vconcat evs))))

;;;###xwem-autoload
(defun xwem-keymacro-executing-p ()
  "Return non-nil if executing keyboard macro."
  (and (boundp 'xwem-keymacro-keys)
       (boundp 'xwem-keymacro-keys-index)))

(defun xwem-keymacro-check-block-in-read-from-minibuffer ()
  "Check whether SXEmacs has been blocked while reading from minibuffer."
  (declare (special xwem-keymacro-keys xwem-keymacro-keys-index))
  (when (and (xwem-cl-selected-p (xwem-minib-cl xwem-minibuffer))
             (> (minibuffer-depth) 0)
             (xwem-keymacro-executing-p)
             (< (incf xwem-keymacro-keys-index)
                (length xwem-keymacro-keys)))
    ;; We are currently reading from minibuffer, so keep executing
    ;; self-insert commands

    ;; XXX MEGA HACK (check for keys that terminates minibuffer)
    (unless (member (aref xwem-keymacro-keys xwem-keymacro-keys-index)
                    '((return) (control ?g) (control ?G) (hyper ?g)))
      (xwem-deferred-funcall
       'xwem-keymacro-check-block-in-read-from-minibuffer))

    (xwem-dispatch-command-event
     (aref xwem-keymacro-keys xwem-keymacro-keys-index))))

;;;###xwem-autoload
(defun xwem-keymacro-execute-keys (keys)
  "Execute keyboard macro KEYS."
  ;; XXX Adjust KEYS in case KEYS is 'self-insert
  (when (and (> (length keys) 1)
             (eq (aref keys 0) 'self-insert))
    (setq keys (vector keys)))

  (let ((xwem-keymacro-keys keys)
        (xwem-keymacro-keys-index 0))
    (while (< xwem-keymacro-keys-index (length xwem-keymacro-keys))
      (xwem-deferred-funcall 'xwem-keymacro-check-block-in-read-from-minibuffer)
      (xwem-dispatch-command-event
       (aref xwem-keymacro-keys xwem-keymacro-keys-index))
      (incf xwem-keymacro-keys-index))))

(defun xwem-keymacro-start-recording ()
  "Start recording keyboard events."
  ;; Clear events queue and enable context
  (setf (X-Dpy-evq xwem-keymacro-dpy) nil)
  (X-XRecordEnableContext xwem-keymacro-dpy xwem-keymacro-rcontext)
  (setq xwem-keymacro-saving t)

  ;; XXX Change minibuffer background
  (when xwem-keymacro-minib-bg
    (setq xwem-keymacro-minib-old-bg
          (face-background-name 'default (xwem-minib-frame xwem-minibuffer)))
    (set-face-property 'default 'background xwem-keymacro-minib-bg
                       (xwem-minib-frame xwem-minibuffer))))

(defun xwem-keymacro-stop-recording ()
  "Stop recording keyboard events."
  (X-XRecordDisableContext (xwem-dpy) xwem-keymacro-rcontext)
  (XFlush (xwem-dpy))
  (setq xwem-keymacro-saving nil)

  ;; XXX Revert minibuffer background
  (when xwem-keymacro-minib-old-bg
    (set-face-property 'default 'background xwem-keymacro-minib-old-bg
                       (xwem-minib-frame xwem-minibuffer))))

;;;###autoload(autoload 'xwem-keymacro-begin "xwem-keymacro" nil t)
(define-xwem-command xwem-keymacro-begin (arg)
  "Start to record keyboard macro.
If used with prefix ARG, then query for bind after macro define."
  (xwem-interactive "P")

  (when (not xwem-keymacro-initialized)
    (error
     'xwem-error "Keyboard macros not initialized, use `xwem-keymacro-init'"))

  (if xwem-keymacro-saving
      (xwem-message 'warning "Already defining macro...")

    (xwem-message 'macro "Defining KBD macro ...")
    (setq xwem-keymacro-prefix-arg arg)
    (xwem-keymacro-start-recording)))

;;;###autoload(autoload 'xwem-keymacro-end "xwem-keymacro" nil t)
(define-xwem-command xwem-keymacro-end (arg)
  "Stop recording keyboard macro.
If recording done with prefix argument, then query for key to bind."
  (xwem-interactive (list xwem-keymacro-prefix-arg))

  (if (not xwem-keymacro-saving)
      (xwem-message 'warning "Not recording KBD macro.")

    ;; Wait last keyrelease, so `xwem-keymacro-extract' will cut keys
    ;; properly.
    (when (= (X-Event-type xwem-last-xevent) X-KeyPress)
      (xwem-kbd-wait-key-release (X-Event-xkey-keycode xwem-last-xevent)))

    (xwem-keymacro-stop-recording)
    (let ((kmacro (xwem-keymacro-extract (X-Dpy-evq xwem-keymacro-dpy)
                                         (length xwem-this-command-keys))))
      (if (not xwem-keymacro-prefix-arg)
          ;; Save last keyboard macro
          (push kmacro xwem-keymacro-macros-stack)

        (let ((key (xwem-read-key "Enter character to bind: ")))
          (define-key 'xwem-user-macros-prefix (events-to-keys (vector key))
            kmacro))))

    (xwem-message 'macro "KBD macro defined.")))

;;;###xwem-autoload
(defun xwem-keymacro-internal-play (keys &optional times)
  "Play Emacs KEYS TIMES times."
  (unless times (setq times 1))

  ;; Unset some variables
  (setq xwem-this-command-keys [])
  (setq xwem-kbd-now-grabbing nil)
  (setq xwem-prefix-arg nil)

  (when xwem-keymacro-show-macro
    (xwem-message 'macro "Executing macro: '%s'%s"
                  (key-description keys)
                  (if (> times 1)
                      (format " %d times" times)
                    "")))

  ;; Force release of modifiers
  (xwem-kbd-force-mods-release)

  ;; Execute KEYS TIMES times
  (dotimes (i times)
    (xwem-keymacro-execute-keys keys)))

;; Commands to be used in `xwem-keymacro-user-macros'
;;;###autoload(autoload 'xwem-keymacro-undefined "xwem-keymacro" nil t)
(define-xwem-command xwem-keymacro-undefined ()
  "Undefined macro command."
  (xwem-interactive)

  (xwem-message 'warning "Macro key `%s' is not defined"
                (key-description xwem-this-command-keys)))

;;;###autoload(autoload 'xwem-keymacro-play-last "xwem-keymacro")
(define-xwem-command xwem-keymacro-play-last (arg)
  "Play last KBD macro ARG times."
  (xwem-interactive "*_p")

  (when (null xwem-keymacro-macros-stack)
    (error 'xwem-error "No KBD macros defined"))

  (if (null xwem-keymacro-macros-depth)
      (setq xwem-keymacro-macros-depth 0)
    (when (>= xwem-keymacro-macros-depth
              (1- (length xwem-keymacro-macros-stack)))
      (error 'xwem-error (format "Can't play macro of %d depth"
                                 (1+ xwem-keymacro-macros-depth))))
    (incf xwem-keymacro-macros-depth))

  (xwem-unwind-protect
      (xwem-keymacro-internal-play
       (nth xwem-keymacro-macros-depth xwem-keymacro-macros-stack) arg)

    (if (zerop xwem-keymacro-macros-depth)
        (setq xwem-keymacro-macros-depth nil)
      (decf xwem-keymacro-macros-depth))))

;;;###autoload(autoload 'xwem-keymacro-recursive-edit "xwem-keymacro" nil t)
(define-xwem-command xwem-keymacro-recursive-edit (arg)
  "Enter recursive edit.
Using \\<xwem-global-map>\\[xwem-keymacro-recursive-edit] you can
change keyboard macro execution in different way, i.e. you can
temporary suspend macro execution.  Use
\\<xwem-global-map>\\[xwem-exit-recursive-edit] to exit recursive
edit."
  (xwem-interactive "_P")

  (or (xwem-keymacro-executing-p)
      xwem-keymacro-saving
      (error 'xwem-error "Not defining or executing keyboard macro"))

  (let ((xwem-this-command-keys [])
       (xwem-prefix-arg nil))

    (unless (xwem-keymacro-executing-p)
      (X-XRecordDisableContext (xwem-dpy) xwem-keymacro-rcontext))

    (xwem-recursive-edit)

    (unless (xwem-keymacro-executing-p)
      (X-XRecordEnableContext xwem-keymacro-dpy xwem-keymacro-rcontext))
    ))

;;;###autoload(autoload 'xwem-keymacro-exit-recursive-edit "xwem-keymacro" nil t)
(define-xwem-command xwem-keymacro-exit-recursive-edit ()
  "Exit recursive edition."
  (xwem-interactive "*")

  ;; XXX
  (when (xwem-keymacro-executing-p)
    (xwem-kbd-force-mods-release))

  (xwem-exit-recursive-edit))

;;;###autoload
(defun xwem-keymacro-save-macros (&optional file)
  "Save all defined macros to FILE.
Default value for FILE is ~/.xwem/xwem-macros.el."
  (let ((buf (get-buffer-create " * temp keymacro buffer *")))
    (with-current-buffer buf
      (erase-buffer)

      (map-keymap #'(lambda (kseq fbind)
                      (when (vectorp fbind)
                        (insert (format "(define-key 'xwem-user-macros-prefix '%S %S)\n" kseq fbind))))
                  (xwem-kbd-fixup-keymap 'xwem-user-macros-prefix))

      (write-file (or file (expand-file-name xwem-keymacro-macrofile xwem-dir)))
      (kill-buffer buf))))

;;;###autoload
(defun xwem-keymacro-load-macros (&optional file)
  "Load macros saved with `xwem-key-save-macros' from FILE.
Default value for FILE is ~/.xwem/xwem-macros.el"
  (load (or file (expand-file-name xwem-keymacro-macrofile xwem-dir))))


(provide 'xwem-keymacro)

;;; On-load actions
(if xwem-started
    (xwem-keymacro-init)
  (add-hook 'xwem-keyboard-init-hook 'xwem-keymacro-init))

;;; xwem-keymacro.el ends here
