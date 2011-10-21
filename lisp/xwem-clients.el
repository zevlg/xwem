;;; xwem-clients.el --- Clients manage.

;; Copyright (C) 2003-2007 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;;         Richard Klinda <ignotus@hixsplit.hu>
;; Created: 2 Mar 2003
;; Keywords: xlib, xwem
;; Time-stamp: <20/4/2010 18:09:23 lg@localhost>

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
;; This file used for generic X clients management.
;;
;; Client state(`xwem-cl-state') is one of:
;;
;;   'active    - Client managed and activated.
;;
;;   'inactive  - CL managed, but not viewable.
;;
;;   'iconified - CL iconified, not viewable.
;;
;;   'destroyed - CL's x window destroyed.
;;
;;   'unknown   - Not any of above.

;;
;; Hooks are:
;;
;;   `xwem-cl-create-hook' - Called when new CL just created.
;;
;;   `xwem-cl-destroy-hook' - Called when CL destroyed.
;;
;;   `xwem-cl-state-change-hook' - Called when CL changes state to one
;;                                 described above.
;;
;;   `xwem-cl-manage-hook' - Called when CL just managed.  This hook
;;                           differs from `xwem-cl-create-hook',
;;                           `xwem-cl-create-hook' called only once
;;                           during client lifetime, but
;;                           `xwem-cl-manage-hook' calls evertime
;;                           client changes its managing model.
;;
;;   `xwem-cl-activate-hook' - Called when CL is activated in its
;;                             context.
;;
;;   `xwem-cl-deactivate-hook' - Called when CL is deactivated in its
;;                               context.
;;
;;   `xwem-cl-withdraw-hook' - Called when CL is about to move to
;;                             withdrawn state.
;;

;; Supported client properties:
;;
;;   `noselect' - Non-nil to make client non-selectable.
;;
;;   `skip-deselect' - Non-nil makes client non-deselectable.
;;
;;   `override-skip-deselect' - Non-nil overrides `skip-deselect'
;;                              property of selected client.
;;
;;   `skip-initial-state' - Non-nil to skip processing ICCCM initial
;;                          client state.
;;
;;   `x-border-width' - Client's border width in pixels.
;;
;;   `x-border-color' - Client's border color.
;;
;;   `ignore-has-input-p' - Non-nil mean force focusing even if client
;;                          does not have ICCCM focus model.
;; 
;;   `activate-hook' - Client local activate hook, called with two
;;                     arguments, CL and TYPE
;; 
;;   `deactivate-hook' - Client local deactivate hook
;; 
;;   `refit-hook' - Client local refit hook, called with three
;;                  arguments - CL, OLD-GEOM, NEW-GEOM

;; Client local hooks mostly for use by minor modes.

;;; Code:

(eval-when-compile
  ;; Shutup compiler
  (defvar xwem-frame-ev-mask)
  (require 'cl)

  ;; Have compiled 21.4 code also work on XEmacs binaries with real
  ;; support for multiple values, by avoiding runtime calls to
  ;; #'values-list:
  (when (eq 'identity (symbol-function 'values-list))
    (define-compiler-macro values-list (arg) arg))
  )

(require 'xwem-load)
(require 'xwem-manage)
(require 'xwem-misc)

(unless (fboundp 'filter)
  (defun filter (pred list)
    "Returns a list of all the elements fulfilling the pred requirement (that
is for which (pred elem) is true)"
    (loop for el in list
      if (funcall pred el) collect el)))

;;; Variables
(defgroup xwem-cl nil
  "Group to customize XWEM Clients handling."
  :prefix "xwem-cl-"
  :group 'xwem)

(defgroup xwem-modes nil
  "Group to customize XWEM managing modes."
  :prefix "xwem-"
  :group 'xwem)

(defcustom xwem-cl-use-parent-xwin t
  "*Non-nil mean that CL will use some X window, where it(CL) lowered.
This is need to fullish some X applications, which accuire clients in
such evil manner."
  :type 'boolean
  :group 'xwem-cl)

(defcustom xwem-cl-noname-name "<noname>"
  "*Name for clients which does not have name."
  :type 'string
  :group 'xwem-cl)

(defcustom xwem-cl-noicon-name "<noname>"
  "*Icon name for clients which does not have their own."
  :type 'string
  :group 'xwem-cl)

(defcustom xwem-cl-other-include-active t
  "*Non-nil mean include active clients in clients list when switching.
If `xwem-clswi-include-active' is function, it must be a function that
accepts one argument - CL and returns non-nil if CL must be included."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (function :tag "Custom function"))
  :group 'xwem-cl)

(defcustom xwem-cl-other-include-iconified nil
  "*Non-nil mean include iconified clients in clients list when switching.
If `xwem-clswi-include-iconified' is function, it must be a function that
accepts one argument - CL and returns non-nil if CL must be included."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (function :tag "Custom function"))
  :group 'xwem-cl)

(defcustom xwem-cl-switch-other-omit-mode-specific nil
  "*Non-nil mean \\<xwem-global-map>\\[xwem-switch-other-client]
wont select client that can be selected with \\<xwem-global-map>\\[xwem-cl-switch-to-other]."
  :type 'boolean
  :group 'xwem-cl)

(defcustom xwem-client-strict-activation nil
  "*Non-nil mean client is checked for aliveness before the activation.
Seting `xwem-client-strict-activation' to non-nil can decrease
activation speed a little, because it requires full interaction with X
server, however it guaranties to prevent X errors on activation."
  :type 'boolean
  :group 'xwem-cl)

(defcustom xwem-imove-visible t
  "*Non-nil mean `xwem-client-imove' will info you about new position."
  :type 'boolean
  :group 'xwem-cl)

(defcustom xwem-iresize-visible t
  "*Non-nil mean `xwem-client-iresize' will info you about new size."
  :type 'boolean
  :group 'xwem-cl)

(define-xwem-face x-border-face
  `(((selected) (:foreground "green"))
    (t (:foreground "gray80")))
  "Face for client's X border."
  :group 'xwem-cl
  :group 'xwem-faces)

;;;###xwem-autoload
(defcustom xwem-client-focusing 'standard
  "Clients focusing type.
One of:

  'advanced -  Any client can be selected, even if it has broken focus model.
  'standard -  Strictly follow ICCCM, hoping everything will be ok.

You should change default value only in very rare circumstances.  You
should fully understand how focusing works in xwem to change default
value."
  :type '(choice (const :tag "Advanced" advanced)
                 (const :tag "Standard" standard))
  :group 'xwem-cl)

;;;###autoload
(defcustom xwem-client-default-properties
  '(x-border-width 2)
  "*Default properties for newly managed clients."
  :type '(repeat (list :inline
                       (symbol :tag "property")
                       (sexp :tag "value")))
  :group 'xwem-cl)

(defcustom xwem-client-properties-exporting t
  "*Non-nil mean, client's properties exports after any property changes.
Non-nil value is useful when using xwem-agent.  It allows you restore
client's properties on (S)XEmacs restart."
  :type 'boolean
  :group 'xwem-cl)

(defcustom xwem-client-properties-exporting-allowed-values
  '(numberp characterp stringp symbolp)
  "List of allowed types of property value to be exported."
  :type '(list symbol)
  :group 'xwem-cl)

(defcustom xwem-client-selected-border-color "green"
  "*Border color when client is selected."
  :type 'color
  :group 'xwem-cl)

(defcustom xwem-client-nonselected-border-color "gray"
  "*Border color when client is not selected."
  :type 'color
  :group 'xwem-cl)

(defcustom xwem-cl-mark-ring-max 16
  "*Maximum size of `xwem-cl-mark-ring'.
Start discarding off end if gets this big."
  :type 'number
  :group 'xwem-cl)

;;;###autoload
(defcustom xwem-cl-create-hook nil
  "Hook to call when creating new client."
  :type 'hook
  :group 'xwem-cl)

;;;###autoload
(defcustom xwem-cl-destroy-hook nil
  "Hooks called with one argument - cl, when cl destroyed.
NOT USED YET."
  :type 'hook
  :group 'xwem-hooks)

;;;###autoload
(defcustom xwem-cl-manage-hook nil
  "*Hooks to be called with just managed CL as argument."
  :type 'hook
  :group 'xwem-hooks)

;;;###autoload
(defcustom xwem-cl-activate-hook nil
  "*Hooks to call when client activates.
Called with two arguments:
 CL   - Activated client
 TYPE - Activation type - either 'activate or 'select"
  :type 'hook
  :group 'xwem-hooks)

;;;###autoload
(defcustom xwem-cl-deactivate-hook nil
  "*Hooks to call when client deactivates.
Called with two arguments:
 CL   - Deactivated client
 TYPE - Deactivation type - either 'deactivate or 'deselect"
  :type 'hook
  :group 'xwem-hooks)

;;;###autoload
(defcustom xwem-cl-refit-hook nil
  "Hooks called when CL just refited."
  :type 'hook
  :group 'xwem-hooks)

;;;###autoload
(defcustom xwem-cl-iconify-hook nil
  "Hooks called when CL just iconified."
  :type 'hook
  :group 'xwem-hooks)

;;;###autoload
(defcustom xwem-cl-withdraw-hook nil
  "*Hooks to call when CL moved to withdrawn state."
  :type 'hook
  :group 'xwem-hooks)

;;;###autoload
(defcustom xwem-cl-change-hook nil
  "Hooks called when something changed in CL."
  :type 'hook
  :group 'xwem-hooks)

;;;###autoload
(defcustom xwem-cl-state-change-hook nil
  "*Hooks to run when state of client changes.
Every hook called with three args - CL OLD-STATE NEW-STATE."
  :type 'hook
  :group 'xwem-hooks)

;;;###autoload
(defcustom xwem-client-select-hook nil
  "*Hooks called when new client just selected.
It is pretty guarantied that `xwem-selected-client' is valid xwem-cl
structure at time of hook execution."
  :type 'hook
  :group 'xwem-hooks)

;;;###autoload
(defcustom xwem-client-deselect-hook nil
  "Hooks called with one arg - cl, when cl deselected.
It is pretty guarantied that `xwem-selected-client' is valid xwem-cl
structure at time of hook execution."
  :type 'hook
  :group 'xwem-hooks)

(defcustom xwem-maximum-last-clients 10
  "Maximum length of `xwem-last-clients' stack."
  :type 'number
  :group 'xwem-cl)

;;; Internal variables


(defconst xwem-client-ev-mask
  (Xmask-or XM-ColormapChange XM-PropertyChange
            XM-FocusChange XM-EnterWindow XM-LeaveWindow
            XM-StructureNotify XM-ResizeRedirect)
  "Event mask for xwem's client.")

;;;###xwem-autoload
(defvar xwem-clients nil
  "List of all managed clients.")

;;;###xwem-autoload
(defvar xwem-current-cl nil
  "Internal variable.
Use `xwem-cl-selected' to get selected client.")

;;;###xwem-autoload
(defvar xwem-last-clients nil
  "Last selected clients LIFO.
Use `(xwem-last-client)' to get last selected client.")

;;;###autoload
(defvar xwem-cl-mark-ring nil
  "The list of marked clients.")

;;;###xwem-autoload
(defmacro xwem-cl-marked-p (cl)
  "Return non-nil if client CL is marked, i.e. in `xwem-cl-mark-ring' list."
  `(memq ,cl xwem-cl-mark-ring))

;;; Client properties
;;;###xwem-autoload
(defvar xwem-supported-client-properties nil
  "List of supported client's properties definitions.
Property definition is list in form:
\(NAME . (MANAGE-TYPE (KEYWORD VAL ...) ...)).

Valid KEYWORD are:

   `:type' - Same as for `defcustom'.  eval composite type added.

   `:set' - Function to call when setting this property. Default is
            `xwem-cl-put-prop'.  Function called with three
            arguments - CL PROP VAL.

   `:get'  - Function to call in order to fetch property value. Default
             is `xwem-cl-get-prop'. Function called with two
             arguments - CL PROP.
")

;;;###xwem-autoload
(defmacro define-xwem-client-property (name manage-type doc &rest keys-vals)
  "Define new xwem client property NAME."
  `(xwem-support-cl-property (quote ,name) (quote ,manage-type)
                             (list :doc ,doc ,@keys-vals)))

;;;###xwem-autoload
(defun xwem-property-supported-p (prop)
  "Return non-nil if client property PROP is supported."
  (assq prop xwem-supported-client-properties))

;;;###xwem-autoload
(defun xwem-support-cl-property (prop-name manage-type keys-val)
  "Add supported client property."
  (let ((pdef (assq prop-name xwem-supported-client-properties)))
    (unless pdef
      (setq pdef (cons prop-name nil))
      (setq xwem-supported-client-properties
            (cons pdef xwem-supported-client-properties)))

    (setcdr pdef (plist-put (cdr pdef) manage-type keys-val))))

;;;###xwem-autoload
(defun xwem-unsupport-cl-property (prop-name manage-type)
  "Remove PROP-NAME from supported property for MANAGE-TYPE."
  (let ((pdef (assq prop-name xwem-supported-client-properties)))
    (when pdef
      (setcdr pdef (plist-remprop (cdr pdef) manage-type))
      (when (null (cdr pdef))
        (setq xwem-supported-client-properties
              (delq pdef xwem-supported-client-properties))))))

(defun xwem-clprop-get-keyword (cl prop keyword &optional default)
  "Return CL's property KEYWORD value.
If no KEYWORD for such CL, return default KEYWORD value.
Return DEFAULT if KEYWORD not found."
  (let ((prop-def (assq prop xwem-supported-client-properties)))
    (or (plist-get (plist-get (cdr prop-def) (xwem-cl-manage-type cl)) keyword)
        (plist-get (plist-get (cdr prop-def) nil) keyword default))))

;;;###xwem-autoload
(defun xwem-client-set-property (cl prop val)
  "Set client property."
  (funcall (xwem-clprop-get-keyword cl prop :set 'xwem-cl-put-prop)
           cl prop val)
  ;; Save new properties in CL's plist XProperty
  (xwem-cl-XProperty-plist-export cl))

;;;###xwem-autoload
(defun xwem-client-set-properties (cl props)
  "To CL's properties import PROPS."
  (while props
    (xwem-client-set-property cl (car props) (cadr props))
    (setq props (cddr props))))

;;;###xwem-autoload
(defun xwem-client-property (cl prop)
  "Return CL's property PROP."
  (funcall (xwem-clprop-get-keyword cl prop :get 'xwem-cl-get-prop)
           cl prop))

(defun xwem-client-properties (cl &optional all-supported)
  "Return list of supported properties from CL's plist.
If ALL-SUPPORTED is non-nil then return all supported properties."
  (let ((props (if all-supported
                   (mapcar #'car xwem-supported-client-properties)
                 (filter #'xwem-property-supported-p
                         (mapcar #'car
                                 (plist-to-alist (xwem-cl-plist cl)))))))
    (apply #'append
           (mapcar #'(lambda (prop)
                       (list prop (xwem-client-property cl prop)))
                   props))))

(defun xwem-client-properties-with-allowed-values (cl &optional all-supported)
  "Return list of supported properties with allowed values.
`xwem-client-properties-exporting-allowed-values' defines allowed values.
ALL-SUPPORTED directly passed to `xwem-client-properties'."
  (alist-to-plist
   (filter #'(lambda (kv)
               (loop for tt in xwem-client-properties-exporting-allowed-values
                 if (funcall tt (cdr kv)) return t))
           (plist-to-alist (xwem-client-properties cl all-supported)))))

(define-xwem-client-property x-border-width nil
  "CL's xwin border width."
  :type 'number
  :set 'xwem-client-set-x-border-width)

(define-xwem-client-property x-border-color nil
  "CL's xwin border color."
  :type 'color
  :set 'xwem-client-set-x-border-color)

(define-xwem-client-property noselect nil
  "Non-nil mean CL can't be selected."
  :type 'boolean)

(define-xwem-client-property skip-deselect nil
  "CL skips deselecting."
  :type 'boolean)

(define-xwem-client-property override-skip-deselect nil
  "CL overrides skip-deselect property of selected client."
  :type 'boolean)

(define-xwem-client-property skip-initial-state nil
  "Non-nil mean skip CL's initial state hint."
  :type 'boolean)

(define-xwem-client-property client-window nil
  "Window id where client is currently managed.
Set only if using windowing managing model."
  :type 'number)

(define-xwem-client-property executed-command nil
  "Command used to start client."
  :type 'string)

(define-xwem-client-property activate-hook nil
  "Client local activate hook."
  :type 'hook)

(define-xwem-client-property deactivate-hook nil
  "Client local deactivate hook."
  :type 'hook)

(define-xwem-client-property refit-hook nil
  "Client local refit hook."
  :type 'hook)

(define-xwem-client-property state nil
  "Current CL's state.
This property is used to track client state when exporting/importing
to/from X."
  :type 'symbol
  :get '(lambda (c p)
          (xwem-cl-state c)))


;; X Properties stuff
(defmacro xwem-cl-XProperty-get (cl prop-atom-string)
  `(xwem-XProperty-get (xwem-cl-xwin ,cl) ,prop-atom-string))
(defmacro xwem-cl-XProperty-set (cl prop-atom-string prop-val)
  `(xwem-XProperty-set (xwem-cl-xwin ,cl) ,prop-atom-string ,prop-val))
(defmacro xwem-cl-XProperty-manage-spec (cl)
  `(xwem-cl-XProperty-get ,cl "XWEM_CLIENT_MANAGE_SPEC"))
(define-xwem-deferred xwem-cl-XProperty-manage-spec-export (cl)
  "Export CL's manage spec into XWEM_CLIENT_PLIST X property."
  (when (xwem-cl-alive-p cl)
    (xwem-cl-XProperty-set cl "XWEM_CLIENT_MANAGE_SPEC"
                           (xwem-cl-manage-spec cl))))
(defmacro xwem-cl-XProperty-plist (cl)
  `(xwem-cl-XProperty-get ,cl "XWEM_CLIENT_PLIST"))
(define-xwem-deferred xwem-cl-XProperty-plist-export (cl)
  "Export CL's plist into XWEM_CLIENT_PLIST X property."
  (when (and xwem-client-properties-exporting
             (xwem-cl-alive-p cl))
    (xwem-cl-XProperty-set
     cl "XWEM_CLIENT_PLIST"
     (xwem-client-properties-with-allowed-values cl t))))


;;; Functions
(defun xwem-client-set-x-border-width (cl bprop width)
  "Change CL's border with to WIDTH.
Default WIDTH is 0."
  (xwem-cl-put-prop cl bprop width)     ; save it in props

  (unless (numberp width)
    (setq width 0))                     ; XXX

  (setf (xwem-cl-new-xgeom cl) (make-X-Geom :border-width width))
  (xwem-refit cl))

(defun xwem-client-set-x-border-color (cl bprop col)
  "Change CL's border color to COL."
  (xwem-cl-put-prop cl bprop col)       ; save it in props

  (unless col
    (setq col "black"))                 ; XXX

  (XSetWindowBorder (xwem-dpy) (xwem-cl-xwin cl)
                    (XAllocColor (xwem-dpy) (XDefaultColormap (xwem-dpy))
                                 (xwem-make-color col))))

(defun xwem-cl-focus-selected ()
  "If CL is selected, set focus on it.
Used in `xwem-post-deferring-hook'."
  (xwem-focus-set (xwem-cl-selected)))

;;;###xwem-autoload
(defun xwem-cl-select (cl)
  "Set CL to be current cl."
  (xwem-client-local-variables-import (xwem-cl-selected))

  ;; Set CL to be current client
  (setf (xwem-last-client) (xwem-cl-selected))
  (setf (xwem-cl-selected) cl)

  (xwem-client-local-variables-export cl))

(define-xwem-client-property ignore-has-input-p nil
  "Ignore No Input/Globally Active input model and force focusing."
  :type 'boolean)

(defun xwem-cl-can-be-selected-p (cl)
  "Return non-nil if CL can be selected.
If `xwem-client-focusing' is not 'advanced `xwem-cl-can-be-selected-p'
can return nil in situations where you expecting t.  When changing
default value of `xwem-client-focusing' be sure your xwem configured
properly to handle applications with broken focus models."
  (and (not (xwem-client-property cl 'noselect))
       (or (xwem-client-property cl 'ignore-has-input-p)
           (eq xwem-client-focusing 'advanced)
           (and (X-WMHints-input-p (xwem-hints-wm-hints (xwem-cl-hints cl)))
                (= (X-WMHints-input (xwem-hints-wm-hints (xwem-cl-hints cl))) 1))
           )))

(define-xwem-deferred xwem-cl-apply-x-border-color (cl)
  "Apply CL's x border color to life."
  (when (xwem-cl-managed-p cl)
    (XSetWindowBorder (xwem-dpy) (xwem-cl-xwin cl)
                      (X-Gc-foreground
                       (xwem-face-get-gc 'x-border-face
                         (and (xwem-cl-selected-p cl) '(selected)) cl)))))

(defun xwem-client-default-select-hook ()
  "Default hook to be added to `xwem-client-select-hook'."
  (xwem-cl-apply-x-border-color (xwem-cl-selected)))

(defun xwem-client-default-deselect-hook ()
  "Default hook to be added to `xwem-client-deselect-hook'."
  (xwem-cl-apply-x-border-color (xwem-cl-selected)))

;;;###xwem-autoload
(defun xwem-select-client (cl)
  "Select new client CL.
CL will not be selected if currently selected client has property
`skip-deselect', unless CL has property `override-skip-deselect'.
If CL is not selected because of above condition, last client is
updated however.
If CL is non-client, dummy client will be selected."
  (unless (xwem-cl-p cl)
    (setq cl (xwem-dummy-client)))

  ;; Update CL's recency
  (setf (xwem-cl-recency cl) (current-time))

  ;; Skip clients that can't be selected
  (if (not (xwem-cl-can-be-selected-p cl))
      (xwem-activate cl)

    (cond ((and (not (xwem-cl-selected-p cl))
                (xwem-cl-active-p (xwem-cl-selected))
                (xwem-client-property (xwem-cl-selected) 'skip-deselect)
                (not (xwem-client-property cl 'override-skip-deselect)))
           ;; If client has such property it mean that no other client
           ;; can be selected untill CL will unset this property.
           ;; Good example of using it is xwem minibuffer.  When it is
           ;; active, no other clients can be selected.
           (setf (xwem-last-client) cl))

          (t
           (unless (xwem-cl-selected-p cl)
             (xwem-deactivate (xwem-cl-selected) 'deselect)
             (run-hooks 'xwem-client-deselect-hook)
             (xwem-cl-select cl)
             (run-hooks 'xwem-client-select-hook))

           (xwem-activate cl 'select)
           (xwem-add-hook-post-deferring 'xwem-cl-focus-selected)))
    ))

(defmacro xwem-with-current-client (cl &rest forms)
  "Make CL to be current and exectute FORMS.
After execution restore current client."
  `(let ((xwem-saved-current-client (xwem-cl-selected))
         (xwem-saved-last-client (xwem-cl-selected)))
     (xwem-cl-select ,cl)
     (prog1
         (progn ,@forms)
       (xwem-cl-select xwem-saved-current-client)
       (setf (xwem-last-client) xwem-saved-last-client))))

;;;###xwem-autoload
(defun xwem-client-name (&optional cl clist)
  "Returns unique name for CL.
By default return name of the current client.
If CLIST is ommited, `xwem-clients' will be used.
If CL is string, return it."
  (unless cl (setq cl (xwem-cl-selected)))
  (if (stringp cl)
      cl
    (let ((cll (or clist xwem-clients))
          (cln (xwem-hints-wm-name (xwem-cl-hints cl)))
          (cnt 0))
      (while cll
        (cond ((string= (xwem-hints-wm-name (xwem-cl-hints (car cll))) cln)
               (if (eq cl (car cll))
                   (setq cll nil)
                 (setq cnt (1+ cnt))))
              (t nil))
        (setq cll (cdr cll)))
      (if (> cnt 0)
          (format "%s<%d>" cln cnt)
        cln))))

;;;###xwem-autoload
(defun xwem-find-client (win-or-id)
  "Find WIN-OR-ID in client windows list.
WIN-OR-ID can be X-Win or id of window."
  (let ((winid (if (X-Win-p win-or-id) (X-Win-id win-or-id) win-or-id))
        (cl xwem-clients)
        (rc nil))
    (while cl
      (if (= (X-Win-id (xwem-cl-xwin (car cl))) winid)
          (progn
            (setq rc (car cl))
            (setq cl nil))
        (setq cl (cdr cl))))
    rc))

(defun xwem-remove-client (cl)
  "Delete WIN from clients list."
  (setq xwem-clients (delete cl xwem-clients)))

(defun xwem-cl-do-gravity (cl step)
  "Change CL's gravity.
NOT USED"
  )

;;;###xwem-autoload(autoload 'xwem-cl-send-config "xwem-clients")
(define-xwem-deferred xwem-cl-send-config (cl)
  "Send config info to window."
  (let ((clgeom (xwem-cl-xgeom cl))
        (win (xwem-cl-xwin cl)))
    (xwem-XSendEvent
     (xwem-dpy) win nil XM-StructureNotify
     (X-Create-message
      (list [1 X-ConfigureNotify]       ;type
            [1 nil]                     ;detail
            [2 2806]                    ;seq
            [4 (X-Win-id win)]          ;event
            [4 (X-Win-id win)]          ;window
            [4 X-None]                  ;above sibling
            (vector 2 (X-Geom-x clgeom)) ; shutup compiler
            [2 (X-Geom-y clgeom)]
            [2 (X-Geom-width clgeom)]
            [2 (X-Geom-height clgeom)]
            [2 (X-Geom-border-width clgeom)] ;XXX
            [1 nil]                     ;pad override
            [1 nil]))
     (let ((xev (make-ffi-object 'XConfigureEvent)))
       (setf (XConfigureEvent->type xev) X-ConfigureNotify
             (XConfigureEvent->event xev) (X-Win-id win)
             (XConfigureEvent->window xev) (X-Win-id win)
             (XConfigureEvent->above xev) X-None
             (XConfigureEvent->x xev) (X-Geom-x clgeom)
             (XConfigureEvent->y xev) (X-Geom-y clgeom)
             (XConfigureEvent->width xev) (X-Geom-width clgeom)
             (XConfigureEvent->height xev) (X-Geom-height clgeom)
             (XConfigureEvent->border_width xev) (X-Geom-width clgeom))
       (ffi-address-of xev)))))


;;;###xwem-autoload
(defun xwem-clients-list (&optional predict include-dummy states)
  "Return xwem clients list.

PREDICT is function passed with one argument - CL and must return
non-nil if CL must be included in resulting list.  If PREDICT not
specified - all clients are included.

If INCLUDE-DUMMY is non-nil also include dummy clients(clients for
which `xwem-dummy-client-p' returns non-nil).

STATES is a list of valid states for client or 'any."
  (delq nil
        (mapcar #'(lambda (cl)
                    (and (or (eq states 'any)
                             (xwem-cl-managed-p cl states))
                         (or include-dummy (not (xwem-dummy-client-p cl)))
                         (or (not predict) (funcall predict cl))
                         cl))
                xwem-clients)))

;;;###xwem-autoload
(defun xwem-cl-list-sort-by-recency (cl-list)
  "Sort clients in CL-LIST by their recency."
  (sort (copy-list cl-list)
        #'(lambda (cl1 cl2)
            (let ((rc1 (xwem-cl-recency cl1))
                  (rc2 (xwem-cl-recency cl2)))

              (if (or (not rc1) (not rc2))
                  (not (null rc1))

                (cond ((> (nth 0 rc1) (nth 0 rc2))
                       t)
                      ((< (nth 0 rc1) (nth 0 rc2))
                       nil)
                      (t (cond ((> (nth 1 rc1) (nth 1 rc2))
                                t)
                               ((< (nth 1 rc1) (nth 1 rc2))
                                nil)
                               (t (cond ((> (nth 2 rc1) (nth 2 rc2))
                                         t)
                                        ((< (nth 2 rc1) (nth 2 rc2))
                                         nil)
                                        (t t)))))))))))

;;;###xwem-autoload(autoload 'xwem-cl-other "xwem-clients")
(defun* xwem-cl-other (cl &key clients no-sort also-active also-iconified)
  "Return xwem client other then CL selecting from CLIENTS.
Default CLIENTS is what is returned by `xwem-clients-list'.
Deactivated clients are preferred to activated, unless ALSO-ACTIVE
is non-nil. Special clients excluded.
Unless ALSO-ICONIFIED is non-nil iconified clients will be excluded.

Use `(xwem-cl-other cl :no-sort t)' form to fetch most recent
client, other then CL.

CLIENTS is sorted by recency unless NO-SORT is non-nil."
  (unless clients
    (setq clients (xwem-clients-list)))

  (unless no-sort
    (setq clients
          (xwem-cl-list-sort-by-recency clients)))

  (let ((rcl nil)
        (notgoodcl nil))                ;not so good candidate as rcl

    (while clients
      (when (and (xwem-cl-p (car clients)) ; skip non-clients
                 (not (eq (car clients) cl)) ; skip ourself
                 ;; skip iconified
                 (or also-iconified
                     (not (xwem-cl-iconified-p (car clients)))
                     (if (functionp xwem-cl-other-include-iconified)
                         (funcall xwem-cl-other-include-iconified
                                  (car clients))
                       xwem-cl-other-include-iconified)))
        (if (or also-active
                (not (xwem-cl-active-p (car clients)))
                (if (functionp xwem-cl-other-include-active)
                    (funcall xwem-cl-other-include-active
                             (car clients))
                  xwem-cl-other-include-active))
            ;; Found pretty good candidate
            (setq rcl (car clients)
                  clients nil)

          (when (and also-active (null notgoodcl))
            (setq notgoodcl (car clients)))))
      (setq clients (cdr clients)))

    ;; Return
    (or rcl notgoodcl)))

;;;###xwem-autoload
(defun xwem-select-last-or-other-client (cl &optional force allow-dummy)
  "Select last or other client according to CL.
New client selected only if CL is current selected or FORCE is non-nil
or dummy client currently selected.

`xwem-select-last-or-other-client' tries to avoid selecting dummy
clients, unless ALLOW-DUMMY is non-nil."
  (when (or force
            (xwem-cl-selected-p cl)
            (xwem-cl-selected-p (xwem-dummy-client)))
    (let ((lcl (find cl xwem-last-clients
                     :test #'(lambda (cl lcl)
                               (and (xwem-cl-alive-p lcl)
                                    (not (eq lcl cl))
                                    (or allow-dummy
                                        (not (xwem-dummy-client-p lcl))))))))
      (xwem-select-client (or lcl (xwem-cl-other cl))))))

;;;###xwem-autoload
(defun xwem-select-some-client (&optional cl)
  (if (xwem-cl-alive-p cl)
      (xwem-select-last-or-other-client cl nil t)
    (unless (and (xwem-cl-alive-p (xwem-cl-selected))
                 (xwem-cl-active-p (xwem-cl-selected)))
      (xwem-select-client nil))))


;;;###xwem-autoload
(defun xwem-cl-correct-size-for-size (cl new-geom &optional x-type y-type)
  "Make CL's geometry as close to NEW-GEOM as possible.
X-TYPE is one of 'center 'left or 'right, default is 'center.
Y-TYPE is one of 'center 'top or 'bottom, default is 'center."
  (let* ((hthi (or (X-Geom-border-width new-geom) 0))
         (he (X-Geom-height new-geom))
         (wi (X-Geom-width new-geom))
         (clgmt (xwem-cl-xgeom cl))
         (wmnh (xwem-cl-wm-normal-hints cl))
         (wi-rmd 0) (he-rmd 0)
         (bw 0) (bh 0) wi-st he-st)

    (when wmnh
      (when (X-WMSize-pbasesize-p wmnh)
        (setq bw (X-WMSize-base-width wmnh))
        (setq bh (X-WMSize-base-height wmnh)))

      (when (X-WMSize-presizeinc-p wmnh)
        (setq wi-st (X-WMSize-width-inc wmnh)) ; width step
        (setq he-st (X-WMSize-height-inc wmnh))) ; height step

      ;; - Calculate size reminders
      ;; - Adjust wi-rmd and he-rmd if needed
      (when wi-st
        (setq wi-rmd (% (abs (- wi bw (* 2 hthi))) wi-st))
        (when (> bw wi)
          (setq wi-rmd (- wi-st wi-rmd))))

      (when he-st
        (setq he-rmd (% (abs (- he bh (* 2 hthi))) he-st))
        (when (> bh he)
          (setq he-rmd (- he-st he-rmd))))

      ;; Check aspect ratio, note: major mode may have
      ;; 'omit-aspect-ratio property, in this case no aspect ratio
      ;; checking is performed.
      ;; NOTE: not full support of aspect ratios, only minimal aspect
      ;;       ratio is used in geometry calculation.
      (when (and (X-WMSize-paspect-p wmnh)
                 (not (zerop (X-WMSize-min-aspect-y wmnh)))
                 (not (xwem-manage-property
                       (xwem-cl-manage-type cl) 'omit-aspect-ratio)))
        (let* ((mia-x (X-WMSize-min-aspect-x wmnh))
               (mia-y (X-WMSize-min-aspect-y wmnh))
;               (maa-x (X-WMSize-max-aspect-x wmnh))
;               (maa-y (X-WMSize-max-aspect-y wmnh))
               (client-ratio (/ (float mia-x) mia-y))
               (window-ratio (/ (float wi) he))
               nw nh)
          ;;  +-------------+
          ;;  |             | <---- window aspect ratio <
          ;;  |MMMMMMMMMMMMM|           client aspect ratio
          ;;  |MMMMMMMMMMMMM|        (width = window width)
          ;;  |             |
          ;;  +-------------+              +-------------+
          ;;                               |   MMMMMMM   |
          ;; window aspect ratio >   ----> |   MMMMMMM   |
          ;;     client aspect ratio       |   MMMMMMM   |
          ;;  (height = window height)     |   MMMMMMM   |
          ;;                               +-------------+

          ;;         width        width = ratio * height
          ;; ratio = ------  =>
          ;;         height       height = width / ratio
          ;;
          ;; --ignotus
          (cond ((< window-ratio client-ratio)
                 (setq nw (- wi bw)
                       nh (floor (/ nw client-ratio))))
                ((> window-ratio client-ratio)
                 (setq nh (- he bh)
                       nw (floor (* client-ratio nh)))))

          ;; Set reminders according to calculated NW/NH
          (when (and nw nh)
            (setq wi-rmd (- wi nw)
                  he-rmd (- he nh))))))

    (unless (X-Geom-p clgmt)
      (setf (xwem-cl-xgeom cl) (make-X-Geom))
      (setq clgmt (xwem-cl-xgeom cl)))

    ;; Now adjust geometry according to X-TYPE and Y-TYPE layout
    (cond ((eq x-type 'left)
           (setf (X-Geom-x clgmt) (X-Geom-x new-geom)))
          ((eq x-type 'right)
           (setf (X-Geom-x clgmt) (+ (X-Geom-x new-geom) wi-rmd)))
          (t                            ; 'center or any other
           (setf (X-Geom-x clgmt) (+ (X-Geom-x new-geom) (/ wi-rmd 2)))))
    (cond ((eq y-type 'top)
           (setf (X-Geom-y clgmt) (X-Geom-y new-geom)))
          ((eq y-type 'bottom)
           (setf (X-Geom-y clgmt) (+ (X-Geom-y new-geom) he-rmd)))
          (t
           (setf (X-Geom-y clgmt) (+ (X-Geom-y new-geom) (/ he-rmd 2)))))

    (setf (X-Geom-width clgmt) (- wi (* 2 hthi) wi-rmd))
    (setf (X-Geom-height clgmt) (- he (* 2 hthi) he-rmd))))

;;;###autoload(autoload 'xwem-client-iconify "xwem-clients" "" t)
(define-xwem-command xwem-client-iconify (cl &optional arg)
  "Iconify selected client CL.
If prefix ARG is given - iconify only clients of same major mode as
selected client."
  (xwem-interactive (list (xwem-cl-selected) xwem-prefix-arg))

  (unless (xwem-cl-alive-p cl)
    (error 'xwem-error "Invalid client"))

  (if arg
      (mapc #'xwem-iconify
            (xwem-clients-list
             #'(lambda (ocl)
                 (eq (xwem-cl-manage-type ocl)
                     (xwem-cl-manage-type cl)))))
    (xwem-iconify cl)))

;;;###xwem-autoload(autoload 'xwem-client-iconify-every "xwem-clients" "" t)
(define-xwem-command xwem-client-iconify-every (arg)
  "Iconify every client.
If prefix ARG is specified - iconify also dummy clients."
  (xwem-interactive "P")

  (mapc #'xwem-client-iconify (xwem-clients-list nil arg)))

;;;###autoload(autoload 'xwem-switch-client "xwem-clients" "" t)
(define-xwem-command xwem-switch-client (arg)
  "Interactively switch to client.
When used with prefix ARG, then filter to clients of same managing
model as selected client."
  (xwem-interactive "P")

  (let* ((pred (when arg
                 (let ((mt (and (xwem-cl-p (xwem-cl-selected))
                                (xwem-cl-manage-type (xwem-cl-selected)))))
                   `(lambda (cl)
                      (eq (xwem-cl-manage-type cl) (quote ,mt))))))
         (clients (xwem-cl-list-sort-by-recency
                   (xwem-clients-list pred arg)))
         (cl (xwem-read-client
              (if arg (format "XWEM-CL (%S): "
                              (and (xwem-cl-p (xwem-cl-selected))
                                   (xwem-cl-manage-type (xwem-cl-selected))))
                "XWEM-CL: ") clients)))

    (unless (xwem-cl-alive-p cl)
      (error 'xwem-error "Invalid client"))

    (xwem-select-client cl)))

;;;###autoload(autoload 'xwem-switch-iconified-client "xwem-clients" "" t)
(define-xwem-command xwem-switch-iconified-client (arg)
  "Interactively switch to iconified client."
  (xwem-interactive "P")
  (let* ((clients (xwem-cl-list-sort-by-recency
                   (xwem-clients-list #'xwem-cl-iconified-p arg)))
         (cl (xwem-read-client
              "XWEM-CL: " (or clients (error "No iconified clients")))))
    (unless (xwem-cl-alive-p cl)
      (error 'xwem-error "Invalid client"))
    (xwem-select-client cl)))

;;;###autoload(autoload 'xwem-switch-other-client "xwem-clients" "" t)
(define-xwem-command xwem-switch-other-client (arg)
  "Switch to client other then selected.
This command differs from `xwem-cl-switch-to-other' command,
'other-client' method is not examined, but most recent client is
selected, so pressing `\\<xwem-global-map>\\[xwem-switch-other-client]'
twice will select selected client, what is not guarantied by
`xwem-cl-switch-to-other'."
  (xwem-interactive "p")

  (let* ((cl (xwem-cl-selected))
         (ocl (and xwem-cl-switch-other-omit-mode-specific
                   (xwem-method-other-client cl))))
    (while (> arg 0)
      (setq cl (xwem-cl-other
                cl :clients (xwem-clients-list
                             `(lambda (cl)
                                (not (eq cl ,ocl))))))
      (decf arg))
    (unless (xwem-cl-p cl)
      (setq cl ocl))
    (unless (xwem-cl-p cl)
      (error 'xwem-error "No other client"))

    (xwem-select-client cl)))

;;;###autoload(autoload 'xwem-attach-client "xwem-clients" "" t)
(define-xwem-command xwem-attach-client (arg)
  "Attach client to current window.
When used with prefix ARG, select also clients that are already in
current window."
  (xwem-interactive "P")

  (let* ((pred #'(lambda (cl)
                   (and (xwem-manage-property
                         (xwem-cl-manage-type cl) 'win-support)
                        (or arg
                            (not (xwem-win-selected-p
                                  (xwem-cl-win cl)))))))
         (cl (xwem-read-client (if arg "XWEM-CL any: " "XWEM-CL: ")
                               (xwem-cl-list-sort-by-recency
                                (xwem-clients-list pred)))))

    (unless (xwem-cl-alive-p cl)
      (error 'xwem-error "Invalid client"))

    (xwem-win-set-cl (xwem-win-selected) cl)
    (xwem-select-client cl)))

;;;###autoload(autoload 'xwem-cl-switch-other-win "xwem-clients" "" t)
(define-xwem-command xwem-cl-switch-other-win (cl &optional arg)
  "Display client CL in other window.
If prefix ARG is given - select CL as well."
  (xwem-interactive "cXWEM-CL Other: \nP")

  (let (nwin)
    (when (xwem-win-only-one-p (xwem-win-selected))
      (xwem-window-split-vertically nil)) ; XXX
    (setq nwin (xwem-win-next (xwem-win-selected)))

    (xwem-win-set-cl nwin cl)
    (when arg
      (xwem-select-window nwin))))

;;;###autoload(autoload 'xwem-cl-switch-other-frame "xwem-clients" "" t)
(define-xwem-command xwem-cl-switch-other-frame (cl &optional arg)
  "Switch to CL in other XWEM frame.
When used with prefix ARG, then create embedded frame, if creation is
needed at all."
  (xwem-interactive
   (list (xwem-read-client
          "XWEM-CL Other frame: "
          (xwem-cl-list-sort-by-recency
           (xwem-clients-list
            #'(lambda (cl)
                (xwem-manage-property (xwem-cl-manage-type cl) 'win-support)))))
         xwem-prefix-arg))

  (let ((ofr (or (xwem-frame-other (xwem-frame-selected))
                 (xwem-make-frame-1 (or (and arg 'embedded) 'desktop)
                                    :noselect t))))

    (xwem-cl-change-window cl (xwem-frame-selwin ofr))
    (xwem-select-client cl)))

;;;###autoload(autoload 'xwem-cl-switch-to-other "xwem-clients" "" t)
(define-xwem-command xwem-cl-switch-to-other (cl &optional n)
  "Switch to xwem client other then CL.
Default CL is selected client.
If prefix argument N is specified, switch to N's other client."
  (xwem-interactive (list (xwem-cl-selected)
                          (prefix-numeric-value xwem-prefix-arg)))

  (while (> n 0)
    (setq cl (xwem-method-other-client cl))
    (decf n))

  (unless (xwem-cl-alive-p cl)
    (error 'xwem-error "No other client available"))

  (xwem-select-client cl))

;;;###autoload(autoload 'xwem-cl-switch-to-other-in-other-win "xwem-clients" "" t)
(define-xwem-command xwem-cl-switch-to-other-in-other-win (n)
  "Switch to other client in other window then selected,"
  (xwem-interactive "p")

  (when (xwem-win-only-one-p)
    (error 'xwem-error "Only one window"))

  (let* ((win (xwem-window-other 1))
         (cl (xwem-win-cl win)))
    (while (> n 0)
      (setq cl (xwem-cl-other cl :clients (xwem-win-clients win)))
      (decf n))

    (unless (xwem-cl-alive-p cl)
      (error 'xwem-error "Invalid client"))

    (xwem-activate cl)))

;;;###autoload(autoload 'xwem-kill-cl-and-window "xwem-clients" "" t)
(define-xwem-command xwem-kill-cl-and-window (window &optional arg)
  "Kill WINDOW an client currently activated in it.
If used with prefix ARG then kill client in other window and other
window (not implemented)."
  (xwem-interactive (list (xwem-win-selected)
                          xwem-prefix-arg))

  (let* ((win (if arg (xwem-window-other 1 window) window))
         (cl (xwem-win-cl win)))

    ;; kill client
    (when (xwem-cl-p cl)
      (xwem-client-kill cl))

    ;; kill window
    (xwem-window-delete win)))

;;;###autoload(autoload 'xwem-cl-transpose "xwem-clients" "" t)
(define-xwem-command xwem-cl-transpose (cl &optional arg)
  "Transpose CL with client at right in CL's WIN.
If ARG is non-nil transpose with left client.
If CL is ommited than selected client will be used."
  (xwem-interactive (list (xwem-cl-selected)
                          xwem-prefix-arg))

  (when (xwem-cl-p cl)
    (let* ((sw (xwem-cl-win cl))
           (tai (cadr (memq cl (funcall (if arg 'reverse 'identity)
                                        (xwem-win-clients sw))))))
      (unless (xwem-cl-p tai)
        (setq tai (cadr (memq cl (funcall (if arg 'identity 'reverse)
                                          (xwem-win-clients sw))))))

      (when (xwem-cl-p tai)
        (xwem-list-exchange-els (xwem-win-clients sw) cl tai)
        (run-hook-with-args 'xwem-win-clients-change-hook sw))
      )))
(put 'xwem-cl-transpose 'xwem-frame-command t)

(defun xwem-clients-init ()
  "Clients part initializer"
  (xwem-message 'init "Initializing clients ...")

  (setq xwem-clients nil)

  ;; Add default select/deselect hooks
  (add-hook 'xwem-client-select-hook 'xwem-client-default-select-hook t)
  (add-hook 'xwem-client-deselect-hook 'xwem-client-default-deselect-hook t)

  (add-hook 'xwem-cl-state-change-hook
            (lambda (cl &rest nu)
              (xwem-cl-XProperty-plist-export cl)))

  ;; Initialise dummy client
  (xwem-dummy-client-init)

  (xwem-message 'init "Initializing clients ... done"))

;;;###xwem-autoload
(defun xwem-client-sendmsg-atom (cl atom &optional time)
  "Send Client message to client CL."
  (xwem-XSendEvent
   (xwem-dpy) (xwem-cl-xwin cl) nil 0
   (X-Create-message
    (list [1 X-ClientMessage]           ; type
          [1 X-format-32]               ; format
          [2 1000]                      ; XXX seq
          [4 (X-Win-id (xwem-cl-xwin cl))] ; window
          [4 (X-Atom-id (X-Atom-find-by-name (xwem-dpy) "WM_PROTOCOLS"))]
          [4 (X-Atom-id atom)]
          [4 (or time X-CurrentTime)]
          [4 nil]))
   (let ((xev (make-ffi-object 'XClientMessageEvent)))
     (setf (XClientMessageEvent->type xev) X-ClientMessage
           (XClientMessageEvent->format xev) X-format-32
           (XClientMessageEvent->window xev) (X-Win-id (xwem-cl-xwin cl))
           (XClientMessageEvent->message_type xev)
           (X-Atom-id (X-Atom-find-by-name (xwem-dpy) "WM_PROTOCOLS")))
     (ffi-store xev (ffi-slot-offset 'XClientMessageEvent 'data)
                'long (X-Atom-id atom))
     (ffi-store xev (+ (ffi-slot-offset 'XClientMessageEvent 'data) 4)
                'long (or time X-CurrentTime))
     (ffi-address-of xev))))

(defun xwem-client-delete-window (cl)
  "Close xwem client CL in safe manner.
Return non-nil, if CL supports WM_DELETE_WINDOW."
  (when (XWMProtocol-set-p
         (xwem-dpy) (xwem-hints-wm-protocols (xwem-cl-hints cl)) "WM_DELETE_WINDOW")
    (xwem-client-sendmsg-atom
     cl (X-Atom-find-by-name (xwem-dpy) "WM_DELETE_WINDOW"))
    t))

(define-xwem-deferred xwem-client-apply-state (cl)
  "Apply CL's state to life."
  (when (xwem-cl-alive-p cl)
    (cond ((memq (xwem-cl-state cl) '(active iconified inactive))
           (XSetWMState (xwem-dpy) (xwem-cl-xwin cl)
                        (if (xwem-cl-active-p cl)
                            X-NormalState
                          X-IconicState)))
          ((xwem-misc-xwin-valid-p (xwem-cl-xwin cl))
           (XSetWMState (xwem-dpy) (xwem-cl-xwin cl) X-WithdrawnState)))))

;;;###xwem-autoload
(defun xwem-client-change-state (cl state)
  "Change WM_STATE for CL, to STATE.
STATE is one of 'active, 'inactive, 'iconified, 'withdrawn, etc."
  (let ((old-state (xwem-cl-state cl)))
    (setf (xwem-cl-state cl) state)
    (xwem-client-apply-state cl)
    (run-hook-with-args 'xwem-cl-state-change-hook cl old-state state)))

(defun xwem-client-withdraw (cl)
  "Put CL in withdraw state.
I.e. after `xwem-client-withdraw' CL will not have manage entry."
  (xwem-client-change-state cl 'withdrawn)
  (xwem-method-withdraw cl)

  ;; If CL was current client, uset current client
  (when (xwem-cl-selected-p cl)
    (xwem-select-client nil)))

;;; Creating new client
(defun xwem-cl-presetup (cl)
  "Reparent client CL. CL should be already setuped.
NOTE: specials clients presetuped as any other."
  (let ((xwin (xwem-cl-xwin cl)))
    ;; Select for events
    (XSelectInput (xwem-dpy) xwin (xwem-cl-ev-mask cl))
    (X-Win-EventHandler-add-new xwin 'xwem-cl-events-handler 100)

    ;; Fetch some window properties
    (setf (xwem-cl-hints cl)
          (make-xwem-hints
           :wm-normal-hints (or (XGetWMNormalHints (xwem-dpy) xwin)
                                (make-X-WMSize :flags 0))
           :wm-hints (or (XGetWMHints (xwem-dpy) xwin)
                         (make-X-WMHints :flags 0))
           :wm-class (multiple-value-bind (ci cn)
                         (values-list (XGetWMClass (xwem-dpy) xwin))
                       (cons ci cn))
           :wm-role (XGetPropertyString
                     (xwem-dpy) xwin
                     (XInternAtom (xwem-dpy) "WM_WINDOW_ROLE" nil))
           :wm-command (XGetWMCommand (xwem-dpy) xwin)
           :wm-name (XGetWMName (xwem-dpy) xwin)
           :wm-icon-name (XGetPropertyString (xwem-dpy)
                                             xwin XA-wm-icon-name)
           :wm-transient-for (XGetWMTransientFor (xwem-dpy) xwin)
           :wm-protocols (XGetWMProtocols (xwem-dpy) xwin)))

    (when (zerop (length (xwem-hints-wm-name (xwem-cl-hints cl))))
      ;; WMNAME is empty
      (setf (xwem-hints-wm-name (xwem-cl-hints cl)) xwem-cl-noname-name))

    ;; In case CL is transient for window
    (setf (xwem-cl-transient-for cl)
          (xwem-hints-wm-transient-for (xwem-cl-hints cl)))

    ;; TODO: skip clients created by xwem (for example embedded frame)
    (unless (X-Win-get-prop (xwem-cl-xwin cl) 'xwin-created-by-xwem)
      (XChangeSaveSet (xwem-dpy) (xwem-cl-xwin cl) X-SetModeInsert))

    ;; Install keyboard grabs, (ARGUSED)
    (xwem-kbd-install-grab xwem-global-map xwin)

    ;; Install keyboard grabs for global minor modes
    (mapc #'(lambda (mm-entry)
              (when (and (not (xwem-client-local-variable-p (car mm-entry)))
                         (not (xwem-client-local-variable-p (cdr mm-entry)))
                         (symbol-value (car mm-entry)))
                (xwem-kbd-install-grab (symbol-value (cdr mm-entry)) xwin)))
          xwem-minor-mode-map-alist)))

;;;###xwem-autoload
(defun xwem-make-client (xwin &optional props)
  "Make new client window.
XWIN   - X Window.
PROPS - Properties list for new client.
XWEM-WIN - xwem window where new client should be managed(if possible)."
  (let ((new-cl (make-xwem-cl
                 :xwin xwin
                 :ev-mask (Xmask-or xwem-client-ev-mask
                                    (if (xwem-frame-p (xwem-xwin-frame xwin))
                                        xwem-frame-ev-mask
                                      0)))))
    (when (xwem-misc-xwin-valid-p xwin) ; just to check that XWIN still alive
      (xwem-debug 'xwem-cl "New Client making name=%s, class=%S"
                  '(XGetWMName (xwem-dpy) xwin) '(XGetWMClass (xwem-dpy) xwin))

      (X-Win-put-prop xwin 'xwem-cl new-cl)
      (xwem-cl-presetup new-cl)

      (setf (xwem-cl-initial-xattrs new-cl)
            (XGetWindowAttributes (xwem-dpy) xwin))
      (setf (xwem-cl-initial-xgeom new-cl)
            (XGetGeometry (xwem-dpy) xwin))
      (setf (xwem-cl-xgeom new-cl)
            (copy-X-Geom (xwem-cl-initial-xgeom new-cl)))
      (setf (xwem-cl-start-time new-cl)
            (current-time))

      ;; Apply properties
      (xwem-cl-apply-plist new-cl props)

      new-cl)))

(defun xwem-unmake-client (cl)
  "Unmake client CL."
  ;; If unmaking selected client, then select dummy client
  (when (xwem-cl-selected-p cl)
    (xwem-select-client nil))

  (X-Win-rem-prop (xwem-cl-xwin cl) 'xwem-cl)
  (X-Win-EventHandler-rem (xwem-cl-xwin cl) 'xwem-cl-events-handler)

  ;; Remove from mark ring
  (setq xwem-cl-mark-ring (delq cl xwem-cl-mark-ring))

  ;; Make sure CL not in xwem-clients
  (setq xwem-clients (delq cl xwem-clients))

  (X-invalidate-cl-struct cl))

(defun xwem-client-first-manage (cl &optional props)
  "Manage client CL for the first time.
Return non-nil if CL successfully managed."
  ;; Find match spec for CL
  (let ((mspec (or (xwem-cl-XProperty-manage-spec cl)
                   (xwem-manda-find-match cl))))
    (when mspec
      ;; Add CL to clients list
      (pushnew cl xwem-clients :test 'eq)

      ;; Collaborate all the properties for client from different
      ;; sources and apply them to client.
      ;;
      ;; Sources are:
      ;;   - Specified PROPS
      ;;   - Specified in manage specification
      ;;   - Import from X property
      (xwem-cl-apply-plist
       cl (append props (cadr mspec) (xwem-cl-XProperty-plist cl)))

      ;; Set expectance if 'client-window property is set
      (let ((ewin (xwem-client-property cl 'client-window)))
        (when ewin
          (xwem-cl-was-expected cl t)
          (xwem-client-set-property cl 'expect-win ewin)))

      ;; Setup focus mode if not already setuped
      (unless (xwem-client-property cl 'xwem-focus-mode)
        (xwem-focus-mode-set cl))
      (xwem-debug 'xwem-cl "Focus model: %S selected"
                  '(xwem-client-property cl 'xwem-focus-mode))

      ;; Apply plist before setting managing model, becase seting
      ;; properties may depend on it and will fail because client is
      ;; not yet managed by this managing model.

      ;; On the other hand not seting managing model before applying
      ;; properties may cause seting property to fail, for example
      ;; 'expect-win property, which only set for 'generic managing
      ;; model.
      (setf (xwem-cl-manage-spec cl) mspec)
      (xwem-debug 'xwem-cl "Managing model: %S selected"
                  '(xwem-cl-manage-type cl))

      ;; Run manage method
      (xwem-manage cl)

      ;; Unmark CL as it was expected in case `xwem-manda-find-match'
      ;; marked it.
      (xwem-cl-was-expected cl nil)

      ;; Honour initial client state
      (unless (xwem-client-property cl 'skip-initial-state)
        (xwem-cl-honour-init-state cl))

      ;; Run new client hook
      (when (xwem-cl-alive-p cl)
        (run-hook-with-args 'xwem-cl-create-hook cl))
      cl)))

;;;###xwem-autoload
(defun xwem-xwin-try-to-manage (xwin)
  "Try to manage X window XWIN.
Return managed client, or nil if client wasnt managed."
  (unless (xwem-xwin-cl xwin)
    (let ((cl (xwem-make-client xwin)))
      (when (xwem-cl-p cl)
        (unless (xwem-client-first-manage cl xwem-client-default-properties)
          (xwem-unmake-client cl)))
      (and (xwem-cl-p cl) cl))))

;;;###xwem-autoload
(defun xwem-cl-destroy (cl)
  "Tottally destroy CL."
  (setf (xwem-cl-state cl) 'destroyed)

  ;; If client's window was having always on top rank - unset it
  (xwem-misc-unset-always-on-top (xwem-cl-xwin cl))

  (xwem-method-on-kill cl)
  (xwem-unwind-protect
      (run-hook-with-args 'xwem-cl-destroy-hook cl)

    (xwem-select-some-client cl)
    (xwem-deferred-funcall 'xwem-unmake-client cl)))

;;;###xwem-autoload
(defun xwem-cl-apply-plist (cl nplist)
  "Set plist's properties in CL."
  (while nplist
    (unless (eq (cadr nplist) (xwem-client-property cl (car nplist)))
      (xwem-client-set-property cl (car nplist) (cadr nplist)))
    (setq nplist (cdr (cdr nplist)))))

;;;###xwem-autoload
(defun xwem-cl-apply-new-xgeom (cl &optional correct-including-border
                                   hold-size)
  "Apply entries in `xwem-cl-new-xgeom' to CL's x geometry.
When CORRECT-INCLUDING-BORDER is non-nil, then
`xwem-cl-correct-size-for-size' will correct size reguarding new
border width, otherwise it will correct size as if border width is 0.
If HOLD-SIZE is non-nil no size corrections are performed."
  (let ((nxgeom (xwem-cl-new-xgeom cl))
        (xgeom (xwem-cl-xgeom cl)))
    (when nxgeom
      ;; Adjust CL's border width
      (when (X-Geom-border-width nxgeom)
        (setf (X-Geom-border-width xgeom) (X-Geom-border-width nxgeom))
        (unless correct-including-border
          (setf (X-Geom-border-width nxgeom) nil)))

      ;; Fix NXGEOM before correcting size
      (unless (X-Geom-x nxgeom)
        (setf (X-Geom-x nxgeom) (X-Geom-x xgeom)))
      (unless (X-Geom-y nxgeom)
        (setf (X-Geom-y nxgeom) (X-Geom-y xgeom)))
      (unless (X-Geom-width nxgeom)
        (setf (X-Geom-width nxgeom) (X-Geom-width xgeom)))
      (unless (X-Geom-height nxgeom)
        (setf (X-Geom-height nxgeom) (X-Geom-height xgeom)))

      (unless hold-size
        (xwem-cl-correct-size-for-size cl nxgeom 'left 'top))

      ;; Flush new geometry
      (setf (xwem-cl-new-xgeom cl) nil))))

;;;###xwem-autoload(autoload 'xwem-cl-apply-xgeom "xwem-clients")
;;;###xwem-autoload(autoload 'xwem-cl-apply-xgeom-1 "xwem-clients")
(define-xwem-deferred xwem-cl-apply-xgeom (cl)
  "Apply CL's geometry to CL's X window."
  (let ((clxg (xwem-cl-xgeom cl)))
    (XConfigureWindow (xwem-dpy) (xwem-cl-xwin cl)
                      :x (X-Geom-x clxg)
                      :y (X-Geom-y clxg)
                      :width (X-Geom-width clxg)
                      :height (X-Geom-height clxg)
                      :border-width (X-Geom-border-width clxg))
    (xwem-cl-send-config-1 cl)))

;;;###xwem-autoload
(defun xwem-client-move (cl new-x new-y)
  "Move client CL to NEW-X, NEW-Y."
  (setf (xwem-cl-new-xgeom cl)
        (make-X-Geom :x new-x :y new-y :border-width nil))
  (xwem-refit cl))

;;;###xwem-autoload
(defun xwem-client-resize (cl new-width new-height)
  "Resize client CL to NEW-WIDTH, NEW-HEIGHT."
  (setf (xwem-cl-new-xgeom cl)
        (make-X-Geom :width new-width :height new-height :border-width nil))
  (xwem-refit cl))

;;;###xwem-autoload
(defun xwem-client-move-resize (cl new-x new-y new-width new-height)
  "Move CL to NEW-X, NEW-Y and resize to NEW-WIDTH, NEW-HEIGHT."
  (setf (xwem-cl-new-xgeom cl)
        (make-X-Geom :x new-x :y new-y
                     :width new-width :height new-height
                     :border-width nil))
  (xwem-refit cl))


;;;###xwem-autoload
(defun xwem-cl-change-window (cl &optional new-win)
  "Change CL's window to NEW-WIN.
If NEW-WIN is nil, NEW-WIN will be other window to CL's win window."
  (unless new-win
    (setq new-win (xwem-window-other 1 (xwem-cl-win cl))))

  (unless (eq (xwem-cl-win cl) new-win)
    ;; CL's xwin should be deactivated when changing window
    (xwem-deactivate cl)
    (xwem-cl-set-win cl new-win)))


;; Commands
;;;###autoload(autoload 'xwem-client-kill "xwem-clients" "" t)
(define-xwem-command xwem-client-kill (cl &optional arg)
  "Kill xwem client CL. Also destroys CL's X window.
If used with prefix ARG is given, force CL to die, by default
WM_DELETE_WINDOW will be probed.
If CL is `nil' than client in `xwem-win-selected' assumed."
  ;; TODO: - take icccm stuff(like WM_DELETE_WINDOW) into account
  ;;       - switch to other client
  (xwem-interactive (list (xwem-cl-selected) xwem-prefix-arg))

  (if (xwem-cl-alive-p cl)
      (if arg
          ;; Force killing
          (XKillClient (xwem-dpy) (X-Win-id (xwem-cl-xwin cl)))

        ;; else try soft killing
        (or (xwem-client-delete-window cl)
            (xwem-client-kill cl t)))

    (xwem-message 'warning "Invalid client to kill.")))

;;;###autoload(autoload 'xwem-client-query-kill "xwem-clients" "" t)
(define-xwem-command xwem-client-query-kill (cl &optional arg)
  "Interactively kill client CL.
Prefix ARG directly passed to `xwem-client-kill'."
  (xwem-interactive (list
                     ;; Sort clients, so selected client will be first
                     (xwem-read-client "XWEM Kill client: "
                                       (xwem-cl-list-sort-by-recency xwem-clients)
                                       (xwem-cl-selected))
                     xwem-prefix-arg))

  (xwem-client-kill cl arg))

;;;###autoload(autoload 'xwem-client-demanage-others "xwem-clients" "" t)
(define-xwem-command xwem-client-iconify-others (window &optional arg)
  "Iconify others then selected client in WINDOW.
If ARG given, remain ARG most recent clients."
  (xwem-interactive (list (xwem-win-selected)
                          (prefix-numeric-value xwem-prefix-arg)))

  (let ((wclients (xwem-win-clients window)))
    (while (and wclients (> (length (xwem-win-clients window)) arg))
      (unless (eq (xwem-win-cl window) (car wclients))
        (xwem-iconify (car wclients)))
      (setq wclients (cdr wclients)))))

;;;###autoload(autoload 'xwem-client-run-copy "xwem-clients" "" t)
(define-xwem-command xwem-client-run-copy (cl &optional arg)
  "Run the same command as selected CL.
Prefix ARG specifies how many copies to run."
  (xwem-interactive (list (xwem-cl-selected)
                          (prefix-numeric-value xwem-prefix-arg)))
  (xwem-kbd-stop-grabbing)
  (unless (xwem-cl-alive-p cl)
    (error 'xwem-error "Invalid client"))

  (let ((cmd (xwem-client-command cl))
        (mspec (xwem-cl-manage-spec cl)))
    ;; Check command for validity
    (when (or (not (stringp cmd)) (string= cmd ""))
      (error 'xwem-error "Invalid command: " cmd))

    (while (> arg 0)
      (xwem-execute-program-expecting-1
       cmd :manage-type (car mspec)
       :cl-plist (append (cadr mspec)
                         (when (xwem-cl-win cl)
                           `(expect-win ,(xwem-win-id (xwem-cl-win cl)))))
       :qualifier (xwem-client-guess-qualifier cl))
      (decf arg))))

;;;###autoload(autoload 'xwem-client-run-copy-other-win "xwem-clients" "" t)
(define-xwem-command xwem-client-run-copy-other-win (arg &optional cl)
  "Run copy of selected client in other window.
With prefix ARG, make horizontal split instead of vertical if split
really needed."
  (xwem-interactive "P")
  (unless cl (setq cl (xwem-cl-selected)))
  (when (or (not (xwem-cl-p cl))
            (not (xwem-win-p (xwem-cl-win cl))))
    (error 'xwem-error "Can't run copy of invalid client"))

  (let ((cmd (xwem-client-command cl))
        (own (xwem-window-other 1 (xwem-cl-win cl))))
    (when cmd
      ;; Check is there split needed
      (when (xwem-win-selected-p own)
        (if arg
            (xwem-window-split-horizontally 0)
          (xwem-window-split-vertically 0))
        (setq own (xwem-win-next (xwem-win-selected))))

      (xwem-execute-program-expecting-1
       cmd :manage-type nil
       :cl-plist `(expect-win ,(xwem-win-id own))
       :qualifier (xwem-client-guess-qualifier cl)))))

;;;###autoload(autoload 'xwem-client-run-copy-other-frame "xwem-clients" "" t)
(define-xwem-command xwem-client-run-copy-other-frame (arg &optional cl)
  "Run copy of selected client in other frame.
If prefix ARG is specified, create embedded frame, if creation is
needed at all."
  (xwem-interactive "P")
  (unless cl (setq cl (xwem-cl-selected)))
  (when (or (not (xwem-cl-p cl))
            (not (xwem-frame-p (xwem-cl-frame cl))))
    (error 'xwem-error "Can't run copy of invalid client"))
  (let ((cmd (xwem-client-command cl))
        (ofr (xwem-frame-other (xwem-cl-frame cl))))
    (when cmd
      (unless (xwem-frame-p ofr)
        (setq ofr (xwem-make-frame-1 (or (and arg 'embedded) 'desktop)
                                     :noselect t)))
      (let ((cl (xwem-execute-program-expecting-1
                 cmd :manage-type nil
                 :cl-plist `(expect-win ,(xwem-win-id (xwem-frame-selwin ofr)))
                 :qualifier (xwem-client-guess-qualifier cl))))
        (when (xwem-cl-alive-p cl)
          (xwem-select-client cl))))))

(defun xwem-cl-prog-geom (cl)
  "Get program specified geometry for CL."
  (let ((wmnh (xwem-hints-wm-normal-hints (xwem-cl-hints cl))))
    (when (and wmnh (X-WMSize-psize-p wmnh))
      (cons (X-WMSize-width wmnh)
            (X-WMSize-height wmnh)))))

(defun xwem-cl-base-geom (cl)
  "Return base width and height for CL."
  (let ((wmnh (xwem-hints-wm-normal-hints (xwem-cl-hints cl))))
    (cond ((and wmnh (X-WMSize-pbasesize-p wmnh))
           (cons (X-WMSize-base-width wmnh) ;base width
                 (X-WMSize-base-height wmnh))) ;base height
          ((and wmnh (X-WMSize-pminsize-p wmnh))
           (cons (X-WMSize-min-width wmnh)
                 (X-WMSize-min-height wmnh))))))

;;;###xwem-autoload
(defun xwem-cl-min-geom (cl)
  "Returns minimal geometry for CL."
  (let ((wmnh (xwem-hints-wm-normal-hints (xwem-cl-hints cl))))
    (when (and wmnh (X-WMSize-pminsize-p wmnh))
      (cons (X-WMSize-min-width wmnh)
            (X-WMSize-min-height wmnh)))))

;;;###xwem-autoload
(defun xwem-cl-step-geom (cl)
  "Returns vertical and horisontal step's width for CL."
  (let ((wmnh (xwem-hints-wm-normal-hints (xwem-cl-hints cl))))
    (when (and wmnh (X-WMSize-presizeinc-p wmnh))
      (cons (X-WMSize-width-inc wmnh)
            (X-WMSize-height-inc wmnh)))))

;;;###xwem-autoload
(defun xwem-cl-get-init-state (cl)
  "Return CL's initial state.
Initial state is one of `X-IconicState', `X-NormalState',
or nil if initial state did not specified by client."
  (let ((wm-hints (xwem-hints-wm-hints (xwem-cl-hints cl))))
    (and (X-WMHints-state-p wm-hints) (X-WMHints-initial-state wm-hints))))

;;;###xwem-autoload
(defun xwem-cl-get-usize (cl)
  "Returns cons cell (uwi . uhe) - CL's size in steps."
  (let* ((clgmt (xwem-cl-xgeom cl))
         (wmnh (xwem-hints-wm-normal-hints (xwem-cl-hints cl)))
         (steps (xwem-cl-step-geom cl))
         (base (xwem-cl-base-geom cl))
         (minsize (xwem-cl-min-geom cl))
         (uwi (X-Geom-width clgmt))
         (uhe (X-Geom-height clgmt)))

    (if (and steps
             (not (= (car steps) 0))
             (not (= (cdr steps) 0)))
        (when (or base minsize)
          (progn
            (setq uwi (- uwi (if (X-WMSize-pbasesize-p wmnh)
                                 (car base)
                               (car minsize))))
            (setq uhe (- uhe (if (X-WMSize-pbasesize-p wmnh)
                                 (cdr base)
                               (cdr minsize))))))
      (setq steps '(1 . 1)))

    (cons (/ uwi (car steps))
          (/ uhe (cdr steps)))))

;;;###xwem-autoload
(defun xwem-cl-get-psize (cl)
  "Return pixel size of CL (pwi . phe)."
  (let ((gmt (xwem-cl-xgeom cl)))
    (cons (X-Geom-width gmt) (X-Geom-height gmt))))

(defun xwem-cl-get-uptime-1 (cl)
  (let ((ctime (current-time))
        (stime (xwem-cl-start-time cl)))
    (list (- (nth 0 ctime) (nth 0 stime))
          (- (nth 1 ctime) (nth 1 stime))
          (- (nth 2 ctime) (nth 2 stime)))))

;;;###xwem-autoload
(defun xwem-cl-get-uptime (cl &optional format)
  "Return CL's uptime.
FORMAT can contain %-sequences to substitute parts of the uptime.
%d is replaced by the days
%h is replaced by the hours
%m is replaced by the minutes
%s is replaced by the seconds
%A is replaced by autogenerated format."
  (let* ((upt (xwem-cl-get-uptime-1 cl))
         (upt (+ (* (nth 0 upt) 65536) (mod (nth 1 upt) 65536)))
         (days (/ upt 86400))
         (hours (/ (mod upt 86400) 3600))
         (minutes (/ (mod upt 3600) 60))
         (seconds (mod upt 60))
         (fmt (or format "%A"))
         (rup ""))

    (let ((flst (string-to-list fmt))
          chr)
      (while flst
        (setq chr (car flst))
        (cond ((= chr ?%)
               (setq flst (cdr flst))
               (setq chr (car flst))
               (cond ((= chr ?d)
                      (setq rup (concat rup (format "%d" days))))
                     ((= chr ?h)
                      (setq rup (concat rup (format "%d" hours))))
                     ((= chr ?m)
                      (setq rup (concat rup (format "%d" minutes))))
                     ((= chr ?s)
                      (setq rup (concat rup (format "%d" seconds))))
                     ((= chr ?A)
                      (setq rup (concat
                                 (cond ((> days 0) (format "%dd %dh %dm %ds" days hours minutes seconds))
                                       ((> hours 0) (format "%dh %dm %ds" hours minutes seconds))
                                       ((> minutes 0) (format "%dm %ds" minutes seconds))
                                       ((> seconds 0) (format "%d seconds" seconds))
                                       (t "")))))
                     (t (error "Invalid format"))))

              (t (setq rup (concat rup (char-to-string chr)))))
        (setq flst (cdr flst))))
    rup))

;;;###autoload(autoload 'xwem-client-info "xwem-clients" "" t)
(define-xwem-command xwem-client-info (cl &optional arg)
  "Display info about xwem client CL.
If used with prefix ARG, insert to current client."
  (xwem-interactive (list (xwem-cl-selected)
                          xwem-prefix-arg))

  (unless (xwem-cl-alive-p cl)
    (error 'xwem-error "invalid client"))

  (let* ((usz (xwem-cl-get-usize cl))
         (psz (xwem-cl-get-psize cl))
         (msg (format "%s | 0x%d | %dx%d/%dx%d | Uptime: %s | app: %s | %s"
                      (upcase (symbol-name (xwem-cl-manage-type cl)))
                      (X-Win-id (xwem-cl-xwin cl))
                      (car usz) (cdr usz)
                      (car psz) (cdr psz)
                      (xwem-cl-get-uptime cl)
                      (or (car (xwem-client-application cl)) "UNKNOWN")
                      (xwem-client-name cl))))
    (if arg
        (progn
          (xwem-kbd-stop-grabbing)
          (xwem-kbd-force-mods-release)
          (xwem-kbd-add-pending-keys msg))

      (xwem-message 'info "%s" msg))))

;;;###autoload(autoload 'xwem-cl-pop-to-client "xwem-clients" "" t)
(define-xwem-command xwem-cl-pop-to-client (cl &optional warp)
  "Pop to client CL, i.e. switch to its frame and manage it.
If CL have no frame, popup it in current frame."
  (xwem-interactive "cXWEM-CL Pop: \nP")

  (unless (xwem-cl-alive-p cl)
    (error 'xwem-error "Invalid client"))

  (xwem-select-client cl)
  (when warp
    (XWarpPointer (xwem-dpy) X-None (xwem-cl-xwin cl) 0 0 0 0 10 10)))

;;;###autoload(autoload 'xwem-cl-set-title "xwem-clients" "" t)
(define-xwem-command xwem-client-set-title (title &optional cl)
  "Set new TITLE for client CL."
  (xwem-interactive
   (list (xwem-read-from-minibuffer
          "XWEM Set title: " (xwem-client-name (xwem-cl-selected)))
         (xwem-cl-selected)))

  (unless (xwem-cl-p cl)
    (error 'xwem-error "Invalid client" cl))

  (XSetWMName (xwem-dpy) (xwem-cl-xwin cl) title))

;;;###autoload(autoload 'xwem-client-unset-mark "xwem-clients" "" t)
(define-xwem-command xwem-client-unset-mark (cl)
  "Uset mark on CL."
  (xwem-interactive (list (xwem-cl-selected)))

  (when (xwem-cl-marked-p cl)
    (setq xwem-cl-mark-ring (delq cl xwem-cl-mark-ring))

    ;; Now run cl-change hooks
    (run-hook-with-args 'xwem-cl-change-hook cl)))

;;;###autoload(autoload 'xwem-client-set-mark "xwem-clients" "" t)
(define-xwem-command xwem-client-set-mark (cl &optional arg)
  "Mark selected client CL.
If CL is ommited or `xwem-client-set-mark' called interactively then
selected client assumed.
With no prefix ARG push selected client to `xwem-cl-mark-ring'.
With positive prefix ARG, jump to ARG's client in `xwem-cl-mark-ring'.
With negative prefix ARG, unset mark from CL."
  (xwem-interactive
   (list (if (and (not (null xwem-prefix-arg))
                  (> (prefix-numeric-value xwem-prefix-arg) 0))
             (nth (1- (prefix-numeric-value xwem-prefix-arg))
                  xwem-cl-mark-ring)
           (xwem-cl-selected))
         xwem-prefix-arg))

  (unless (xwem-cl-p cl)
    (error 'xwem-error "Invalid client"))

  (cond ((< (prefix-numeric-value arg) 0)
         (xwem-client-unset-mark cl))
        ((null arg)
         ;; Push cl and adjust `xwem-cl-mark-ring' according to
         ;; `xwem-cl-mark-ring-max' value if needed.
         (when (= (length xwem-cl-mark-ring) xwem-cl-mark-ring-max)
           ;; Remove last item
           (xwem-client-unset-mark (car (last xwem-cl-mark-ring))))

         (push cl xwem-cl-mark-ring)

         ;; Now run cl-change hooks
         (run-hook-with-args 'xwem-cl-change-hook cl)

         (xwem-message 'info "Marking selected client, total ring size %d"
                       (length xwem-cl-mark-ring)))
        ((> (prefix-numeric-value arg) 0)
         (xwem-cl-pop-to-client cl))))

;;;###autoload(autoload 'xwem-client-exchange-selected-and-mark "xwem-clients" "" t)
(define-xwem-command xwem-client-exchange-selected-and-mark (cl)
  "Exchange marked client with selected client CL.
I.e. marked client will be selected and unmarked, and selected client
will be deselected and marked."
  (xwem-interactive (list (xwem-cl-selected)))

  (let ((mcl (car xwem-cl-mark-ring)))
    (unless (xwem-cl-p mcl)
      (error 'xwem-error "No marked client"))
    (xwem-client-unset-mark mcl)
    (xwem-client-set-mark cl)
    (xwem-select-client mcl)))

;;;###xwem-autoload
(defun xwem-fini-clients ()
  "Fini all clients."
  (while xwem-clients
    ;; Make CL to be child of root window
    (let* ((cl (car xwem-clients))
           (geom (xwem-cl-xgeom cl)))
      (XReparentWindow (xwem-dpy) (xwem-cl-xwin cl)
                       (xwem-rootwin) (X-Geom-x geom) (X-Geom-y geom))
      (setf (xwem-cl-state cl) 'unknown))
    (setq xwem-clients (cdr xwem-clients))))


;;; Events handling for client windows
(defun xwem-cl-hproperty (cl xev)
  "PropertyNotify."
  (let ((xwin (xwem-cl-xwin cl))
        (atom-id (X-Atom-id (X-Event-xproperty-atom xev)))
        (state (X-Event-xproperty-state xev))
        rhook)

    ;; Some CL's property changed
    (xwem-debug 'xwem-cl "CLIENT .. PropertyNotify: Atom-id = %d" 'atom-id)

    (cond ((and (= atom-id (X-Atom-id XA-wm-normal-hints))
                (= state X-PropertyNewValue))
           ;; WM_NORMAL_HINTS changed
           (setf (xwem-cl-wm-normal-hints cl)
                 (XGetWMNormalHints (xwem-dpy) xwin))
           (setq rhook t))

          ((and (= atom-id (X-Atom-id XA-wm-hints))
                (= state X-PropertyNewValue))
           ;; WM_HINTS changed
           (setf (xwem-cl-wm-hints cl) (XGetWMHints (xwem-dpy) xwin))
           (setq rhook t))

          ((and (= atom-id (X-Atom-id XA-wm-class))
                (= state X-PropertyNewValue))
           ;; WM_CLASS changed
           (multiple-value-bind (ci cn)
               (values-list (XGetWMClass (xwem-dpy) xwin))
             (setf (xwem-cl-wm-class cl) (cons ci cn)))
           (setq rhook t))

          ((and (= atom-id (X-Atom-id (XInternAtom (xwem-dpy) "WM_WINDOW_ROLE" nil)))
                (= state X-PropertyNewValue))
           ;; WM_WINDOW_ROLE changed
           (setf (xwem-cl-wm-role cl)
                 (XGetPropertyString (xwem-dpy) (xwem-cl-xwin cl)
                                     (XInternAtom (xwem-dpy) "WM_WINDOW_ROLE" nil)))
           (setq rhook t))

          ((and (= atom-id (X-Atom-id XA-wm-command))
                (= state X-PropertyNewValue))
           ;; WM_COMMAND changed
           (setf (xwem-cl-wm-command cl) (XGetWMCommand (xwem-dpy) xwin))
           (setq rhook t))

          ((and (= atom-id (X-Atom-id XA-wm-name))
                (= state X-PropertyNewValue))
           ;; WM_NAME changed
           (xwem-cl-put-sys-prop cl 'saved-name (xwem-cl-wm-name cl))
           (setf (xwem-cl-wm-name cl) (XGetWMName (xwem-dpy) xwin))
           (when (string= "" (xwem-cl-wm-name cl))
             (setf (xwem-cl-wm-name cl) xwem-cl-noname-name))
           (unless (and (stringp (xwem-cl-get-sys-prop cl 'saved-name))
                        (string= (xwem-cl-wm-name cl)
                                 (xwem-cl-get-sys-prop cl 'saved-name)))
             (setq rhook t)))

          ((and (= atom-id (X-Atom-id XA-wm-icon-name))
                (= state X-PropertyNewValue))
           ;; WM_ICON_NAME changed
           (xwem-cl-put-sys-prop cl 'saved-icon-name (xwem-cl-wm-icon-name cl))
           (setf (xwem-cl-wm-icon-name cl)
                 (XGetPropertyString (xwem-dpy) (xwem-cl-xwin cl)
                                     XA-wm-icon-name))
           (when (string= "" (xwem-cl-wm-icon-name cl))
             (setf (xwem-cl-wm-icon-name cl) xwem-cl-noicon-name))
           (unless (and (stringp (xwem-cl-get-sys-prop cl 'saved-icon-name))
                        (string= (xwem-cl-wm-icon-name cl)
                                 (xwem-cl-get-sys-prop cl 'saved-icon-name)))
             (setq rhook t)))
          )

    (when rhook
      (run-hook-with-args 'xwem-cl-change-hook cl))
    ))

(defun xwem-cl-hclient-message (cl xev)
  "ClientMessage event XEV."
  (let ((type (length (X-Event-xclient-msg xev)))) ; 5 -> 32, 10 -> 16, 20 -> 8

    (xwem-debug 'xwem-cl "XWEM-CLIENT: in ClientMessage: type=%S msg(0)=%S"
                'type  '(car (nth 0 (X-Event-xclient-msg xev))))

    (cond ((and (= type 5)
                (= (car (nth 0 (X-Event-xclient-msg xev))) X-IconicState))
           ;; Iconify request from client
           (xwem-iconify cl))
          )))

(defun xwem-cl-events-handler (xdpy xwin xev)
  "Events handler for root window."
  (xwem-debug 'xwem-cl "CLIENT HANDLER: xev type=%S, win=%S"
              '(X-Event-name xev) '(X-Win-id (X-Event-win xev)))

  (X-Event-CASE xev
    ((:X-KeyPress :X-KeyRelease :X-ButtonPress :X-ButtonRelease)
     ;; Normal command event proccessing
     (xwem-dispatch-command-xevent xev))

    ;; Focusing mode
    (:X-FocusIn (xwem-focus-mode-invoke (xwem-xwin-cl xwin) 'focus-in xev))
    (:X-FocusOut (xwem-focus-mode-invoke (xwem-xwin-cl xwin) 'focus-out xev))
    (:X-EnterNotify (xwem-focus-mode-invoke (xwem-xwin-cl xwin) 'enter xev))
    (:X-LeaveNotify (xwem-focus-mode-invoke (xwem-xwin-cl xwin) 'leave xev))

    ;; Various events
    (:X-MapRequest (xwem-ev-mapreq xdpy xwin xev))
    (:X-ResizeRequest (xwem-ev-resize xdpy xwin xev))
    (:X-ConfigureRequest (xwem-ev-reconfig xdpy xwin xev))
    (:X-DestroyNotify (xwem-ev-destroy xdpy xwin xev))
    (:X-UnmapNotify (xwem-ev-unmap xdpy xwin xev))

    (:X-PropertyNotify
     (when (xwem-cl-p (xwem-xwin-cl xwin))
       (xwem-cl-hproperty (xwem-xwin-cl xwin) xev)))
    (:X-ClientMessage
     (when (xwem-cl-p (xwem-xwin-cl xwin))
       (xwem-cl-hclient-message (xwem-xwin-cl xwin) xev)))
    ))

;;;###autoload(autoload 'xwem-client-imove "xwem-clients" nil t)
(define-xwem-command xwem-client-imove ()
  "Interactively move client clicked by button."
  (xwem-interactive "_")

  (let ((cl (xwem-misc-xbutton-cl xwem-last-xevent))
        (x-clic (X-Event-xbutton-root-x xwem-last-xevent))
        (y-clic (X-Event-xbutton-root-y xwem-last-xevent))
        tpnt cl-xwin xev done)

    ;; Check CL
    (unless (xwem-cl-alive-p cl)
      (error 'xwem-error "Invalid client .."))

    (setq cl-xwin (xwem-cl-xwin cl)
          tpnt (car (XTranslateCoordinates
                     (xwem-dpy)
                     (X-Event-xbutton-root xwem-last-xevent)
                     cl-xwin x-clic y-clic))
          x-clic (+ (X-Point-x tpnt) (X-Geom-border-width (xwem-cl-xgeom cl)))
          y-clic (+ (X-Point-y tpnt) (X-Geom-border-width (xwem-cl-xgeom cl))))

    (XGrabPointer (xwem-dpy) cl-xwin
                  (Xmask-or XM-ButtonMotion XM-ButtonRelease XM-ButtonPress)
                  xwem-cursor-move)
    (xwem-unwind-protect
        (while (not done)
          (X-Event-CASE (setq xev (xwem-next-event))
            (:X-ButtonRelease (setq done t))

            (:X-MotionNotify
             (xwem-client-move cl (- (X-Event-xmotion-root-x xev) x-clic)
                               (- (X-Event-xmotion-root-y xev) y-clic))
             (when xwem-imove-visible
               (xwem-message 'nolog "New position %+d%+d"
                             (X-Geom-x (xwem-cl-xgeom cl))
                             (X-Geom-y (xwem-cl-xgeom cl)))))))
      (XUngrabPointer (xwem-dpy)))))

;;;###autoload(autoload 'xwem-client-iresize "xwem-clients" nil t)
(define-xwem-command xwem-client-iresize ()
  "Interactively resize clicked client."
  (xwem-interactive "_")

  (let ((cl (xwem-misc-xbutton-cl xwem-last-xevent))
        (x-clic (X-Event-xbutton-root-x xwem-last-xevent))
        (y-clic (X-Event-xbutton-root-y xwem-last-xevent))
        tpnt cl-xwin done xev)

    (unless (xwem-cl-alive-p cl)
      (error 'xwem-error "Invalid client"))

    (setq cl-xwin (xwem-cl-xwin cl)
          tpnt (car (XTranslateCoordinates
                     (xwem-dpy) (X-Event-xbutton-root xwem-last-xevent)
                     cl-xwin x-clic y-clic))
          x-clic (+ (X-Point-x tpnt) (X-Geom-border-width (xwem-cl-xgeom cl)))
          y-clic (+ (X-Point-y tpnt) (X-Geom-border-width (xwem-cl-xgeom cl))))

    (XGrabPointer (xwem-dpy) cl-xwin
                  (Xmask-or XM-ButtonMotion XM-ButtonRelease XM-ButtonPress)
                  xwem-cursor-resize)
    (xwem-unwind-protect
        (progn
          (xwem-client-resize cl x-clic y-clic)
          (when xwem-iresize-visible
            (let ((psize (xwem-cl-get-psize cl))
                  (usize (xwem-cl-get-usize cl)))
              (xwem-message 'nolog "New size %dx%d/%dx%d"
                            (car psize) (cdr psize) (car usize) (cdr usize))))
          (while (not done)
            (X-Event-CASE (setq xev (xwem-next-event))
              (:X-ButtonRelease (setq done t))
              (:X-MotionNotify
               (let ((xoff (X-Event-xmotion-event-x xev))
                     (yoff (X-Event-xmotion-event-y xev)))
                 (when (and (< xoff 40000) (< yoff 40000))
                   (xwem-client-resize cl xoff yoff)
                   (when xwem-iresize-visible
                     (let ((psize (xwem-cl-get-psize cl))
                           (usize (xwem-cl-get-usize cl)))
                       (xwem-message 'nolog "New size %dx%d/%dx%d"
                                     (car psize) (cdr psize)
                                     (car usize) (cdr usize))))
                   ))))))
      (XUngrabPointer (xwem-dpy)))))

;;;###autoload(autoload 'xwem-client-idestroy "xwem-clients" nil t)
(define-xwem-command xwem-client-idestroy (arg)
  "Interactively destroy clicked client.
If used with prefix ARG, force destroyence."
  (xwem-interactive "_P")

  (let ((cl (xwem-misc-xbutton-cl xwem-last-xevent)))
    (unless (xwem-cl-alive-p cl)
      (error 'xwem-error "Invalid client" cl))

    (xwem-client-kill cl arg)))

;;; Applications navigation
;;;###xwem-autoload
(defun xwem-client-command (&optional cl)
  "Return CL's command."
  (unless cl
    (setq cl (xwem-cl-selected)))
  (or (xwem-client-property cl 'executed-command)
      (xwem-cl-wm-command cl)))

(defun xwem-client-class-qualifier (&optional cl)
  "Return CL's qualifier based on WM_CLASS."
  (let ((ic (xwem-cl-wm-class (or cl (xwem-cl-selected)))))
    `(and (class-inst ,(format "^%s$" (car ic)))
          (class-name ,(format "^%s$" (cdr ic))))))

;;;###xwem-autoload
(defun xwem-client-application (&optional cl)
  "Retun CL's application entry from `xwem-applications-alist'."
  (let ((cl (or cl (xwem-cl-selected))))
    (or
     (xwem-manda-find-match-1 cl xwem-applications-alist 'cadr)

     ;; If no application defined in `xwem-applications-alist', then
     ;; simple return class instance
     (let ((class (xwem-cl-wm-class cl)))
       (cons (car class)
             `((and (class-inst ,(concat "^" (car class) "$"))
                    (class-name ,(concat "^" (cdr class) "$")))))))))

;;;###xwem-autoload
(defun xwem-client-guess-qualifier (&optional cl)
  "Try to guess qualifier to match CL."
  (let ((a (xwem-client-application cl)))
    (if a
        `(application ,(car a))
      (xwem-client-class-qualifier cl))))

;;;###autoload(autoload 'xwem-forward-application "xwem-clients" "Select ARGs next application." t)
(define-xwem-command xwem-forward-application (arg)
  "Select ARGs next client of same application as selected client.
Iconified clients are ignored."
  (xwem-interactive "p")

  (let ((cl (xwem-cl-selected))
        app-clients)
    (setq app-clients
          (xwem-clients-list
           `(lambda (client)
              (and (xwem-cl-can-be-selected-p client)
                   (not (xwem-cl-iconified-p client)) ; ignore iconified clients
                   (xwem-cl-match-p client (xwem-client-guess-qualifier ,cl))))))
    (when (< arg 0)
      (setq app-clients (nreverse app-clients))
      (setq arg (- arg)))
    (while (> arg 0)
      (setq cl (or (cadr (memq cl app-clients))
                   (car app-clients)))
      (decf arg))

    (when (xwem-cl-alive-p cl)
      (xwem-select-client cl))))

;;;###autoload(autoload 'xwem-backward-application "xwem-clients" "Select previous application." t)
(define-xwem-command xwem-backward-application (arg)
  "Select ARGs previous client of same application as selected client.
Iconified clients are ignored."
  (xwem-interactive "p")
  (xwem-forward-application (- arg)))

(defun xwem-current-applications-list (cls)
  "List all currently applications running by clients CLS."
  (let ((al (delq nil (mapcar #'xwem-client-application cls)))
        (all nil))
    ;; Remove duplicates, we can't use
    ;; `delete/remove-duplicates' because of elements order
    (mapcar (lambda (el)
              (unless (member el all)
                (push el all)))
            al)
    (setq all (nreverse all))

    (if (equal (xwem-client-application) (car all))
        ;; Put current application to the end of list
        (append (cdr all) (list (car all)))
      all)))

;;;###autoload(autoload 'xwem-select-application "xwem-clients" "Select client with application APP.")
(define-xwem-command xwem-select-application (&optional app cls)
  "Select client with application APP.
Iconified clients are ignored.
Return non-nil if client has been found."
  (xwem-interactive)

  (let* ((cls (or cls (xwem-cl-list-sort-by-recency
                       (xwem-clients-list
                        #'(lambda (cl)
                            (not (xwem-cl-iconified-p cl)))))))
         (apps (xwem-current-applications-list cls))
         (app (or app (xwem-completing-read "Application: " apps)))
         (app-matcher (cadr (assoc app apps)))
         (av-cls (filter #'(lambda (c)
                             (and (xwem-cl-can-be-selected-p c)
                                  (xwem-cl-match-p c app-matcher)))
                         (or cls (xwem-cl-list-sort-by-recency
                                  (xwem-clients-list
                                   #'(lambda (cl)
                                       (not (xwem-cl-iconified-p cl))))))))
         (cl (if (> (length av-cls) 1)
                 (xwem-read-client "Client: " av-cls)
               (car av-cls))))
    (when (xwem-cl-alive-p cl)
      (xwem-select-client cl)
      t)))

;;;###xwem-autoload
(defun xwem-client-change-manage-type (cl manage-spec)
  "Change CL's manage type to type specified in MANAGE-SPEC."
  (let ((state (xwem-cl-state cl))
        (selp (xwem-cl-selected-p cl)))

    ;; If client's window was always on top rank - unset it
    (xwem-misc-unset-always-on-top (xwem-cl-xwin cl))

    ;; Reparent client to root window.  Reparenting leaves unchanged
    ;; the absolute coordinates (with respect to the root window) of
    ;; the upper-left outer corner.
    (let ((tpnt (car (XTranslateCoordinates (xwem-dpy) (xwem-cl-xwin cl)
                                            (xwem-rootwin) 0 0))))
      (XUnmapWindow (xwem-dpy) (xwem-cl-xwin cl))
      (XReparentWindow (xwem-dpy) (xwem-cl-xwin cl)
                       (xwem-rootwin) (X-Point-x tpnt) (X-Point-y tpnt)))
    (xwem-withdraw cl)
    (xwem-method-on-type-change cl manage-spec)

    ;; Set new manage spec and update client properties
    (setf (xwem-cl-manage-spec cl) manage-spec)
    (xwem-cl-apply-plist cl (cadr manage-spec))

    (xwem-manage cl)

    ;; Keep CL's state
    (when (eq state 'active)
      (xwem-activate cl))
    (when selp
      (xwem-select-client cl))))

;;;; --- Frontends to methods ---

;;;###xwem-autoload
(defun xwem-cl-honour-init-state (cl)
  ;; Honour CL's initial-state (ICCM 4.1.4)
  (let ((istate (xwem-cl-get-init-state cl)))
    (cond ((or (and (eq (xwem-cl-state cl) 'unknown)
                    (eql istate X-IconicState))
               (eq (xwem-cl-get-prop cl 'state) 'iconified))
           (xwem-iconify cl))
          ((or (eql istate X-NormalState)
               t)                       ; default
           (xwem-activate cl)))))

;;;###xwem-autoload
(defun xwem-manage (cl)
  "Manage client CL."
  (xwem-method-manage cl)

  ;; Save new manage spec into X property
  (xwem-cl-XProperty-manage-spec-export cl)

  (run-hook-with-args 'xwem-cl-manage-hook cl))

;;;###xwem-autoload
(defun xwem-activate (cl &optional type)
  "Activate client CL in its context.
TYPE is one of:

  `activate' - Client is non-active and activation required.

  `select' - Client is about to be selected, so activation required.
             Client may be already activated.

Default TYPE is `activate'."
  (if (and xwem-client-strict-activation
           (not (xwem-misc-xwin-valid-p (xwem-cl-xwin cl))))
      ;; CL suddenly died
      (xwem-cl-destroy cl)

    (unless type (setq type 'activate))

    (unless (xwem-cl-active-p cl)
      (xwem-client-change-state cl 'active)
      (xwem-method-activate cl 'activate))

    (unless (eq type 'activate)
      (xwem-method-activate cl type))

    (run-hook-with-args 'xwem-cl-activate-hook cl type)))

;;;###xwem-autoload
(defun xwem-deactivate (cl &optional type)
  "Deactivate client CL in its context.
NOTE that only active client can be deactivated!
TYPE is one of:

   `deactivate' - Client must be deactivate.

   `deselect' - Client is deselecting.

Default TYPE is `deactivate'."
  (unless type (setq type 'deactivate))

  (cond ((eq type 'deselect)
         (xwem-method-deactivate cl type))

        ((xwem-cl-active-p cl)
         (xwem-client-change-state cl 'inactive)
         (xwem-method-deactivate cl type)
         (xwem-select-some-client))

        ((not (xwem-cl-inactive-p cl))
         (xwem-method-deactivate cl type)
         (xwem-select-some-client)))

  (run-hook-with-args 'xwem-cl-deactivate-hook cl type))

;;;###xwem-autoload
(defun xwem-iconify (cl)
  "Function to iconify client CL.
ARGS - arguments."
  (unless (xwem-cl-iconified-p cl)
    (xwem-client-change-state cl 'iconified)
    (xwem-method-iconify cl))

  (xwem-select-some-client cl))

;;;###xwem-autoload
(defun xwem-refit (cl)
  "Function to refit client CL.
ARGS - arguments."
  ;; Reguard  border width change
  (when (and (xwem-cl-new-xgeom cl)
             (xwem-manage-property (xwem-cl-manage-type cl)
                                   'reguard-x-border-width)
             (X-Geom-border-width (xwem-cl-new-xgeom cl)))
    (setf (X-Geom-border-width (xwem-cl-xgeom cl))
          (X-Geom-border-width (xwem-cl-new-xgeom cl))))

  (xwem-method-refit cl)

  ;; Apply (new) CL geometry to life
  (xwem-cl-apply-xgeom cl)

  ;; Finally run hooks
  (run-hook-with-args 'xwem-cl-refit-hook cl))

;;;###xwem-autoload
(defun xwem-withdraw (cl)
  "Withdraw client CL."
  (xwem-client-change-state cl 'withdrawn)
  (xwem-method-withdraw cl)

  (xwem-select-some-client cl)
  (run-hook-with-args 'xwem-cl-withdraw-hook cl))

;;;; ---- Default manage methods ----

(define-xwem-deferred xwem-default-apply-state (cl)
  "Apply CL's state to life."
  (cond ((xwem-cl-active-p cl)
         (XMapWindow (xwem-dpy) (xwem-cl-xwin cl)))
        ((memq (xwem-cl-state cl) '(inactive iconified))
         (XUnmapWindow (xwem-dpy) (xwem-cl-xwin cl)))))

(define-xwem-method activate default (cl &optional type)
  "Default method to activate client CL."
  (cond ((eq type 'select)
         (xwem-deferred-funcall 'xwem-misc-raise-xwin (xwem-cl-xwin cl)))
        ((eq type 'activate)
         (xwem-default-apply-state cl))))

(define-xwem-method deactivate default (cl &optional type)
  "Default method to deactivate client CL."
  (cond ((eq type 'deactivate)
         (xwem-default-apply-state cl))))

(define-xwem-method iconify default (cl)
  "Default method to iconify CL."
  (xwem-default-apply-state cl))

(define-xwem-method refit default (cl)
  "Default method to refit CL."
  (xwem-cl-apply-new-xgeom cl)
  (xwem-cl-apply-xgeom cl))

;; New method to select other client
(defun xwem-method-other-client (cl)
  "Return xwem client other then CL."
  (xwem-execute-method 'other-client (xwem-cl-manage-type cl) cl))

(define-xwem-method other-client default (cl)
  "Default other-client method.
By default no other client available."
  nil)

(define-xwem-method on-kill default (cl)
  "Default on-kill method."
  (xwem-select-last-or-other-client cl))


;;; Dummy client, used, when selecting `nil' client.
(defvar xwem-dummy-client nil
  "Internal variable.")

(define-xwem-client-property dummy-client-p nil
  "Non-nil for dummy clients."
  :type 'boolean)

;;;###xwem-autoload
(defun xwem-dummy-client-p (cl)
  "Return non-nil if CL is dummy client."
  (xwem-client-property cl 'dummy-client-p))

;;;###xwem-autoload
(defun xwem-non-dummy-client-p (cl)
  "Opposit to `xwem-dummy-client-p'."
  (not (xwem-dummy-client-p cl)))

;;;###xwem-autoload
(defun xwem-dummy-client ()
  "Return dummy client."
  (or xwem-dummy-client
      (progn (xwem-dummy-client-init) xwem-dummy-client)))

(defun xwem-dummy-client-init ()
  "Create dummy client"
  (unless xwem-dummy-client
    ;; Set `xwem-dummy-client' to some garbage before creating, to
    ;; avoid infinite loop if `xwem-make-client' will use
    ;; `xwem-dummy-client' somehow --lg
    (setq xwem-dummy-client 'invalid-dummy-client
          xwem-dummy-client
          (xwem-make-client
           (XCreateWindow (xwem-dpy) nil 0 0 1 1 0 nil nil nil
                          :override-redirect t)
           '(dummy-client-p t ignore-has-input-p t)))
    (setf (xwem-cl-manage-type xwem-dummy-client) 'dummy)

    (XSelectInput (xwem-dpy) (xwem-cl-xwin xwem-dummy-client)
                  (Xmask-or XM-KeyPress XM-KeyRelease
                            XM-ButtonPress XM-ButtonRelease))
    (XMapWindow (xwem-dpy) (xwem-cl-xwin xwem-dummy-client))

    ;; First selected client is dummy client
    (setf (xwem-cl-selected) xwem-dummy-client)))

;;;###xwem-autoload
(defun xwem-dummy-selected-p ()
  "Return non-nil if dummy client is currently selected."
  (xwem-cl-selected-p xwem-dummy-client))

(define-xwem-method manage dummy (cl))
(define-xwem-method activate dummy (cl &optional type))
(define-xwem-method deactivate dummy (cl &optional type))
(define-xwem-method withdraw dummy (cl))
(define-xwem-method refit dummy (cl))
(define-xwem-method iconify dummy (cl))

;;; Dedicated managing model
(defcustom xwem-dedicated-client-properties nil
  "*Plist of properties for clients managed in dedicated frames."
  :type '(restricted-sexp :match-alternatives (valid-plist-p))
  :group 'xwem-dedicated)

(defun xwem-manage-dedicated (cl)
  "Manage method for dedicated CL."
  (let* ((frame (xwem-make-frame-1 'dedicated
                                   :props '(initially-unmapped t)
                                   :noselect t))
         (sw (xwem-frame-selwin frame))
         (qp (XQueryPointer (xwem-dpy) (xwem-rootwin))))

    (xwem-cl-set-win cl sw)
    (xwem-win-set-cl sw cl)

    (xwem-frame-set-pos frame (nth 5 qp) (nth 6 qp))
    (setf (X-Geom-x (xwem-cl-xgeom cl)) (xwem-win-x sw))
    (setf (X-Geom-y (xwem-cl-xgeom cl)) (xwem-win-y sw))
    (xwem-window-set-size sw (X-Geom-width-with-borders (xwem-cl-xgeom cl))
                          (X-Geom-height-with-borders (xwem-cl-xgeom cl)))

    (XReparentWindow (xwem-dpy) (xwem-cl-xwin cl)
                     (xwem-frame-xwin frame)
                     (xwem-win-x sw) (xwem-win-y sw))
    (XMapWindow (xwem-dpy) (xwem-cl-xwin cl))

    ;; Setup client & select client
    (xwem-select-client cl)

    (if (xwem-frame-property frame 'manual-position)
        (xwem-frame-imove-internal
         frame (xwem-frame-x frame) (xwem-frame-y frame))
      (xwem-frame-set-pos frame 0 0))
    (xwem-frame-map frame)))

(defun xwem-refit-dedicated (cl)
  "Refit method for dedicated CL."
  (cond ((xwem-cl-new-xgeom cl)
         (xwem-cl-apply-new-xgeom cl)
         (xwem-cl-apply-xgeom cl)
         (xwem-window-set-size
          (xwem-cl-win cl)
          (X-Geom-width-with-borders (xwem-cl-xgeom cl))
          (X-Geom-height-with-borders (xwem-cl-xgeom cl))))

        (t
         (let ((win (xwem-cl-win cl))
               (clg (xwem-cl-xgeom cl)))
           (when (or (not (= (X-Geom-width-with-borders clg)
                             (xwem-win-width win)))
                     (not (= (X-Geom-height-with-borders clg)
                             (xwem-win-height win))))
             (xwem-cl-correct-size-for-size
              cl
              (make-X-Geom :x (xwem-win-x win)
                           :y (xwem-win-y win)
                           :width (xwem-win-width win)
                           :height (xwem-win-height win)
                           :border-width (X-Geom-border-width clg))
              'left 'top)
             (xwem-window-set-size
              (xwem-cl-win cl)
              (X-Geom-width-with-borders (xwem-cl-xgeom cl))
              (X-Geom-height-with-borders (xwem-cl-xgeom cl)))
             (xwem-cl-apply-xgeom cl))))))

(define-xwem-deferred xwem-cldedic-apply-state (cl)
  "Apply CL's state to life."
  ;; Nothing actually to do here
  nil)

(defun xwem-activate-dedicated (cl &optional type)
  "Activate method for dedicated CL."
  (cond  ((eq type 'select)
          (xwem-select-window (xwem-cl-win cl)))

         ((eq type 'activate)
          (xwem-cldedic-apply-state cl))))

(defun xwem-deactivate-dedicated (cl &optional type)
  "Deactivate method for dedicated CL."
  (cond ((eq type 'deselect)
         )
        ((eq type 'deactivate)
         )))

(defun xwem-iconify-dedicated (cl)
  "Iconify method for dedicated CL."
  (xwem-frame-hide (xwem-cl-frame cl)))

(defun xwem-clded-disassociate-frame (cl)
  "Dissassociate dedicated frame from CL."
  (let ((frm (xwem-cl-frame cl)))
    (when (xwem-win-p (xwem-cl-win cl))
      (setf (xwem-win-cl (xwem-cl-win cl)) nil))
    (setf (xwem-cl-win cl) nil)

    (xwem-frame-destroy frm)))

(defun xwem-withdraw-dedicated (cl)
  "Withdraw method for dedicated CL."
  (setf (xwem-frame-state (xwem-cl-frame cl)) 'noselect)
  (XUnmapWindow (xwem-dpy) (xwem-frame-xwin (xwem-cl-frame cl))))

;; Additional methods
(define-xwem-method on-type-change dedicated (cl &optional new)
  (xwem-clded-disassociate-frame cl))

(define-xwem-method on-kill dedicated (cl)
  (xwem-select-last-or-other-client cl)
  (xwem-clded-disassociate-frame cl))

(define-xwem-manage-model dedicated
  "Dedicated managing model.
Manage clients in dedicated frame."
  :qualifier '(nil)                     ; never match

  :cl-properties xwem-dedicated-client-properties
  :manage-properties '(win-support t)
  :manage-method 'xwem-manage-dedicated
  :activate-method 'xwem-activate-dedicated
  :deactivate-method 'xwem-deactivate-dedicated
  :refit-method 'xwem-refit-dedicated
  :iconify-method 'xwem-iconify-dedicated
  :withdraw-method 'xwem-withdraw-dedicated)

;;; Testing:
;(progn
;  (xwem-manda-add-expectance '(dedicated nil (t)))
;  (xwem-launch-xterm 4)
;  nil)

;;; Client local variables
(defvar xwem-client-local-variables nil
  "List of client local variables.")

;;;###autoload
(defun xwem-set-default (var val)
  "Set default VAR's value to VAL.
If client does not have its own value, default value will be used."
  (put var 'xwem-default-value val))

;;;###autoload
(defun xwem-make-variable-client-local (var)
  "Make variable symbol VAR to be client local."
  (setq xwem-client-local-variables
        (cons var xwem-client-local-variables))

  (when (boundp var)
    (xwem-set-default var (symbol-value var))))

;;;###xwem-autoload
(defun xwem-client-local-variable-p (var)
  "Return non-nil if VAL is client local variable."
  (memq var xwem-client-local-variables))

;;;###xwem-autoload
(defun xwem-client-local-variable-set (client variable value)
  "Set CLIENT local VARIABLE to VALUE."
  (setf (xwem-cl-local-variables client)
        (put-alist variable value (xwem-cl-local-variables client)))
  (when (xwem-cl-selected-p client)
    (set variable value)))

;;;###xwem-autoload
(defun xwem-client-local-variable-value (client variable)
  "Return CLINEN's local VARIABLE value.
Or global VARIABLE value if CLIENT does not have local value."
  (let ((lval (assq variable (xwem-cl-local-variables client))))
    (if lval
        (cdr lval)
      (get variable 'xwem-default-value))))

(defun xwem-client-local-variables-import (cl)
  "Set client local variables in CL."
  (when (xwem-cl-p cl)
    (setf (xwem-cl-local-variables cl)
          (mapcar #'(lambda (var)
                      (cons var (symbol-value var)))
                  xwem-client-local-variables))))

(defun xwem-client-local-variables-export (cl)
  "Set variables using CL's client local variables."
  (when (xwem-cl-p cl)
    (mapc #'(lambda (var)
              (let ((val (assq var (xwem-cl-local-variables cl))))
                (if val
                    (set var (cdr val))
                  (set var (get var 'xwem-default-value)))))
          xwem-client-local-variables)))

;;; Clients configurations
;;;###xwem-autoload
(defun xwem-client-config-match (config cl)
  "Return non-nil if CONFIG represents CL."
  (and (= (X-Win-id (xwem-cl-xwin cl)) (xwem-cl-config-xwin-id config))
       (string= (xwem-client-command cl) (xwem-cl-config-command config))))

;;;###xwem-autoload
(defun xwem-client-configuration ()
  "Return current clients configuration."
  (let (seen-groups)
    (delete*
     nil (mapcar #'(lambda (cl)
                     (unless (or (xwem-dummy-client-p cl)
                                 (string= (xwem-client-command cl) "")
                                 (and (X-WMHints-wingroup-p
                                       (xwem-cl-wm-hints cl))
                                      (memq (X-WMHints-window-group
                                             (xwem-cl-wm-hints cl))
                                            seen-groups)))
                       ;; Put CL's group leader to SEEN-GRUOPS
                       (when (X-WMHints-wingroup-p
                              (xwem-cl-wm-hints cl))
                         (setq seen-groups
                               (cons (X-WMHints-window-group
                                      (xwem-cl-wm-hints cl)) seen-groups)))
                       (make-xwem-cl-config
                        :selected-p (xwem-cl-selected-p cl)
                        :xwin-id (X-Win-id (xwem-cl-xwin cl))
                        :command (xwem-client-command cl)
                        :manage-type (xwem-cl-manage-type cl)
                        :properties (xwem-client-properties cl))))
                 (copy-list xwem-clients)))))

;;;###xwem-autoload
(defun xwem-set-client-configuration (config)
  "Set client configuration to CONFIG."
  (let ((cl-to-select (xwem-cl-selected)))
    (mapc #'(lambda (clconf)
              (unless (find clconf xwem-clients :test #'xwem-client-config-match)
                ;; Start application represented by CLCONF
                (let ((ncl (xwem-execute-program-expecting-1
                            (xwem-cl-config-command clconf)
                            :manage-type (xwem-cl-config-manage-type clconf)
                            :cl-plist (xwem-cl-config-properties clconf))))
                  (when (xwem-cl-config-selected-p clconf)
                    (setq cl-to-select ncl))
                  (when (xwem-cl-alive-p ncl)
                    ;; XXX fix CONFIG's xwin-id
                    (setf (xwem-cl-config-xwin-id clconf)
                          (X-Win-id (xwem-cl-xwin ncl)))
                    ))))
          config)
    (when (xwem-cl-p cl-to-select)
      (xwem-select-client cl-to-select))))


(provide 'xwem-clients)

;;;; On-load actions:
;; Define dummy manage type
(define-xwem-manage-model dummy
  "Managing model for dummy clients.
Dummy client is client which can't do anything."
  :qualifier '(predicate xwem-dummy-client-p))

(if xwem-started
    (xwem-clients-init)
  (add-hook 'xwem-before-init-wins-hook 'xwem-clients-init))

;;; xwem-clients.el ends here
