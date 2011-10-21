;;; xwem-main.el --- Main part of xwem.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: 21 Mar 2003
;; Keywords: xlib, xwem
;; Time-stamp: <21/10/2011 21:23:43 lg@localhost>

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
;; This main part of XWEM.
;;
;; I strongly recommend you to raise max-lisp-eval-depth value to say
;; 5000.
;;      (setq max-lisp-eval-depth 5000)
;;
;; Try to avoid to use such evil thing as `mouse-avoidance-mode', but
;; if you really want it, than set it to either 'banish or 'jump.
;;
;; If you want develop some xwem addons or take in touch with xwem, it
;; will be usefull to change `find-function-regexp', because xwem uses
;; its own syntax to define interactive commands.

;;     (setq find-function-regexp
;;           (concat "^\\s-*(\\(def[^cgvW]\\w+\\*?"
;;                "\\|define-function"
;;                "\\|define-obsolete-function-alias"
;;                "\\|define-compatible-function-alias"
;;                "\\|define-derived-mode"
;;                "\\|define-xwem-command"
;;                "\\)\\s-+%s\\(\\s-\\|$\\)"))

;; XWEM core:
;;   List of files XWEM can't live without.
;;
;;   xwem-interactive.el - Interactive stuff
;;   xwem-focus.el       - Focuses
;;   xwem-minibuffer.el  - XWEM Minibuffer.
;;   xwem-manage.el      - Manage database.
;;   xwem-keyboard.el    - Keyboard stuff.
;;   xwem-clients.el     - Clients support.
;;   xwem-win.el         - Windows.
;;   xwem-frames.el      - Frames support.

;;; Code:



(eval-when-compile
  (require 'cl))                        ;last, intersection etc

(require 'xwem-load)
(require 'xwem-minibuffer)
(require 'xwem-version)

(defgroup xwem nil
  "XWEM window manager."
  :prefix "xwem-"
  :group 'applications)

(defgroup xwem-hooks nil
  "Group to customize xwem hooks."
  :prefix "xwem-"
  :group 'xwem)

;;;###autoload
(defcustom xwem-dir (file-name-as-directory
                     (expand-file-name ".xwem" (getenv "HOME")))
  "Directory to store XWEM's files."
  :type 'directory
  :group 'xwem)

(defcustom xwem-inhibit-startup-message nil
  "*Non-nil mean, do not show message after successful XWEM start."
  :type 'boolean
  :group 'xwem)

(defcustom xwem-debug nil
  "*Non-nil mean run xlib and xwem in debugging mode."
  :type 'boolean
  :group 'xwem)

;;;###autoload
(defcustom xwem-debug-routines
  '(xwem-cl xwem-event xwem-frame xwem-misc xwem-root xwem-deferred xwem-tray
            ;; and X routines
            x-misc x-event x-tray x-error x-record)
  "Routines to debug on."
  :type '(set (const :tag "XWEM CLients" xwem-cl)
              (const :tag "XWEM Events" xwem-event)
              (const :tag "XWEM Frames" xwem-frame)
              (const :tag "XWEM Misc" xwem-misc)
              (const :tag "XWEM Root" xwem-root)
              (const :tag "XWEM Deferred calls" xwem-deferred)
              (const :tag "XWEM Tray" xwem-tray)
              (const :tag "X Misc" x-misc)
              (const :tag "X Event" x-event)
              (const :tag "X Tray" x-tray)
              (const :tag "X Error" x-error)
              (const :tag "X RECORD" x-record))
  :group 'xwem)

;;;###xwem-autoload
(defcustom xwem-commands-gc-cons-threshold 5000000
  "*Value of `gc-cons-threshold' to use when executing xwem commands.
Make sense only if `xwem-commands-inhibit-gc' is non-nil."
  :type 'number
  :group 'xwem)

;;;###xwem-autoload
(defcustom xwem-commands-inhibit-gc t
  "*Non-nil mean that xwem interactive commands runs without GCing."
  :type 'boolean
  :group 'xwem)

(defcustom xwem-custom-display nil      ;"127.0.0.1:2"
  "*Custom display, mostly for debugging purposes."
  :type '(choice (const :tag "No custom display" nil)
                 (const "127.0.0.1:2")
                 (string :tag "Custom display"))
  :group 'xwem)

;;;###autoload
(defcustom xwem-load-hook nil
  "*Hooks to call after xwem was load."
  :type 'hook
  :group 'xwem-hooks)

;;;###autoload
(defcustom xwem-config-read-hook nil
  "*Hooks to call after xwem read config file."
  :type 'hook
  :group 'xwem-hooks)

;;;###autoload
(defcustom xwem-before-init-wins-hook nil
  "Hooks called before `xwem-init-wins'."
  :type 'hook
  :group 'xwem-hooks)

;;;###autoload
(defcustom xwem-after-init-wins-hook nil
  "Hooks called after `xwem-init-wins'."
  :type 'hook
  :group 'xwem-hooks)

;;;###autoload
(defcustom xwem-before-init-hook nil
  "Hooks to be run before xwem initialization."
  :type 'hook
  :group 'xwem-hooks)

;;;###autoload
(defcustom xwem-after-init-hook nil
  "Hooks to be runned after xwem initialisation."
  :type 'hook
  :group 'xwem-hooks)

(defcustom xwem-exit-hook nil
  "Hooks called after xwem exit."
  :type 'hook
  :group 'xwem-hooks)


;;; Variables
;;;###autoload
(defvar xwem-started nil
  "Non-nil when xwem started.
Do not modify!")


;;; Functions
(defun xwem-initial-manage ()
  "Manage all visible clients.
Even clients with override-redirect attribute set can be managed."
  (xwem-message 'init "Initializing X windows ...")
  (run-hooks 'xwem-before-init-wins-hook)

  (mapc #'xwem-xwin-try-to-manage
        (filter #'(lambda (w)
                    ;; Filter viewable windows that are not
                    ;; xwem-frames and does not have
                    ;; override-redirect flag
                    (let ((attrs (XGetWindowAttributes (xwem-dpy) w)))
                      (and (= (X-Attr-mapstate attrs) X-Viewable)
                           (not (X-Attr-override-redirect attrs))
                           (not (X-Win-get-prop w 'xwem-frame)))))
                (cddddr (XQueryTree (xwem-dpy) (xwem-rootwin)))))

  (run-hooks 'xwem-after-init-wins-hook)
  (xwem-message 'init "Initializing X windows ... done"))

(defun xwem-after-window-setup ()
  "Function which will be added to `window-setup-hook'.
Called after ~/.emacs file loaded and Emacs X window subsystems
initialized."

  (run-hooks 'xwem-before-init-hook)

  (let ((dfen (or xwem-custom-display (getenv "DISPLAY"))))
    (xwem-init-root
     (if (eq (aref dfen 0) ?\:)
         (if (featurep 'ffi-xlib)
             dfen
           (concat "127.0.0.1" dfen))
       dfen)))

  ;; Debugging? yes
  (when xwem-debug
    (setf (X:Dpy-log-buffer (xwem-dpy)) "*xwem-debug*")
    (X-Dpy-set-log-routines (xwem-dpy) xwem-debug-routines))

  ;; Mark as started
  ;; If the user is being tricky with $XWEM_RUNNING variable, set it
  ;; to "yes"
  (setq xwem-started t)
  (when (getenv "XWEM_RUNNING")
    (setenv "XWEM_RUNNING" "yes"))

  (ignore-errors
    ;; Initialize misc stuff
    (xwem-misc-init))
  ;; Create initial frames
  (ignore-errors
    (xwem-frames-init))

  ;; Handle all X clients
  (xwem-initial-manage)

  ;; Now xwem is fully intialized and it is time to run hooks
  (run-hooks 'xwem-after-init-hook)

  (XSync (xwem-dpy))

  (unless xwem-inhibit-startup-message
    (xwem-message
     'asis (concat (xwem-logo-string)
                   " succesfully started. Start with `"
                   (substitute-command-keys
                    "\\<xwem-global-map>\\[xwem-help-prefix]") "'."))))

(defcustom xwem-use-presetup t
  "*When non-nil, use things that normally should be in xwemrc."
  :type 'boolean
  :group 'xwem)

;;; Internal variables

;;;###autoload
(defun xwem-init ()
  "Initialization of xwem subsystems."
  (setq inhibit-startup-message t)      ; DO NOT REMOVE

  (when xwem-use-presetup
    (setq allow-deletion-of-last-visible-frame t
          auto-lower-frame t)

    ;; Yes, do it --ignotus
    (let ((xwem-max-specdpl 10000)
          (xwem-max-lisp-eval-depth 10000))
      (when (< max-specpdl-size xwem-max-specdpl)
        (setq max-specpdl-size xwem-max-specdpl))
      (when (< max-lisp-eval-depth xwem-max-lisp-eval-depth)
        (setq max-lisp-eval-depth xwem-max-lisp-eval-depth)))

    ;; Destroy XEmacs frame when killing dedicated buffer
    (defadvice kill-buffer (before delete-dedicated-frame activate)
      "Work around dedicated frame problem."
      (let ((frame (buffer-dedicated-frame
                    (get-buffer (or (ad-get-arg 0) (current-buffer))))))
        (when (framep frame)
          (delete-frame frame))))

    ;; When XEmacs frame deselects, select xwem minibuffer
    (add-hook 'deselect-frame-hook
              #'(lambda ()
                  (when (and (xwem-minib-p xwem-minibuffer)
                             (framep (xwem-minib-frame xwem-minibuffer))
                             (not (eq (xwem-minib-frame xwem-minibuffer)
                                      (selected-frame))))
                    (select-frame (xwem-minib-frame xwem-minibuffer)))))

    ;; Raise/lower minibuffer
    (add-hook 'xwem-minibuffer-focusin-hook 'xwem-minib-focusin-autoraise)
    (add-hook 'xwem-minibuffer-focusout-hook 'xwem-minib-focusout-autolower)

    ;; Generic managing model
    (require 'xwem-clgen)
    ;; Use nice tabber for frames
    (require 'xwem-tabbing)
    ;; Transient-for clients support
    (require 'xwem-transient)
    ;; Support netwm stuff
    (require 'xwem-netwm))

  ;; Load default keys definitions
  (require 'xwem-keydefs)

  ;; read configuration
  (let ((cfg (expand-file-name "xwemrc.el" xwem-dir)))
    (if (file-exists-p cfg)
        (load cfg)
      (xwem-message 'warning "Configuration file `%s' does not exists" cfg)))

  ;; Config just readed, so run hooks
  (run-hooks 'xwem-config-read-hook)

  (add-hook 'window-setup-hook 'xwem-after-window-setup)
  (add-hook 'kill-emacs-hook 'xwem-fini t))

;;;###autoload(autoload 'xwem-fini "xwem-main" nil t)
(define-xwem-command xwem-fini ()
  "Fini all subsystems."
  (xwem-interactive)

  (ignore-errors
    ;; Run exit hooks
    (run-hooks 'xwem-exit-hook))

  ;; Remove XWEM_RUNNING environment variable if it is set
  (when (getenv "XWEM_RUNNING")
    (setenv "XWEM_RUNNING" nil 'unset))

  (ignore-errors
    ;; And close display
    (xwem-fini-root)))


(provide 'xwem-main)

;;; xwem-main.el ends here
