;;; xwem-clgen.el --- Generic model to manage clients.

;; Copyright (C) 2004-2007 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;; Created: Sat Aug 28 14:31:39 MSD 2004
;; Keywords: xwem
;; Time-stamp: <7/8/2008 23:28:14 lg@h1.lan>

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

;; Generic managing model.

;;; Code:

(require 'xwem-load)
(require 'xwem-manage)
(require 'xwem-misc)

;;; Customisation
(defgroup xwem-clgen nil
  "Group to customise management of generic clients."
  :prefix "xwem-clgen-"
  :group 'xwem-modes)

(defcustom xwem-clgen-other-strategy 'samewin
  "*Strategy used when searching for other client in window.
Possible values are:

  `samewin'   - Search for client managed in window.

  `sameframe-nonactive' - Search for nonactive client managed
                          in window's frame.

  `sameframe-any'       - Search for any client managed in window's
                          frame.

  `samemanda-nonactive' - Search for any nonactive client with same
                          manage entry as other client.

  `any-nonactive'       - Search for any nonactive client."
  :type '(choice (const :tag "Same Window" samewin)
                 (const :tag "Inactive in same frame" sameframe-nonactive)
                 (const :tag "Any in same frame" sameframe-any)
                 (const :tag "Inactive with same manda" samemanda-nonactive)
                 (const :tag "Any inactive" any-nonactive))
  :group 'xwem-clgen)

(defcustom xwem-clgen-other-on-split t
  "*Non-nil mean activate client in other window when doing window split."
  :type 'boolean
  :group 'xwem-clgen)

(defcustom xwem-clgen-other-split-type 'vertical
  "*Split type."
  :type '(choice (const :tag "Vertical" vertical)
                 (const :tag "Horizontal" horizontal))
  :group 'xwem-clgen)

(defcustom xwem-clgen-activate-new t
  "*Non-nil mean newly managed generic clients are activated in their windows."
  :type 'boolean
  :group 'xwem-clgen)

(defcustom xwem-clgen-select-new t
  "*Non-nil mean, select new clients managed in selected window.
This value overrides `xwem-clgen-activate-new' if window is selected."
  :type 'boolean
  :group 'xwem-clgen)

(defcustom xwem-clgen-allow-make-frame t
  "*Non-nil mean, clgen permited to create new frames."
  :type 'boolean
  :group 'xwem-clgen)

(defcustom xwem-generic-client-properties
  '(skip-initial-state t)
  "*Plist of properties for generic clients.
`skip-initial-state' is added by default, because clgen can handle initial state."
  :type '(restricted-sexp :match-alternatives (valid-plist-p))
  :group 'xwem-clgen)

;;; Internal variables
(defvar xwem-clgen-map
  (let ((map (make-sparse-keymap)))
    (define-key map (xwem-kbd "H-c H-o") 'xwem-clgen-toggle-other-on-split)
    map)
  "Local keymap for generic clients.")

;;; Macros
(defmacro xwem-cl-xparent (cl)
  `(xwem-cl-get-sys-prop ,cl 'parent-xwin))
(defsetf xwem-cl-xparent (cl) (parent)
  `(xwem-cl-put-sys-prop ,cl 'parent-xwin ,parent))

(defun xwem-clgen-other-client (cl &optional clients-list visible)
  "Search client other then CL in CLIENTS-LIST.
Default CLIENTS-LIST is win's clients where CL managed.
Note that at least one CL or CLIENTS-LIST should be non-nil, otherwise
nil will be returned."
  (when (or (and (xwem-cl-p cl) (xwem-win-p (xwem-cl-win cl)))
            clients-list)
    ;; XXX sort clients by recency
    (setq clients-list
          (xwem-cl-list-sort-by-recency
           (or clients-list (xwem-win-clients (xwem-cl-win cl)))))

    (let (rcl notgoodcl)
      (while clients-list
        (when (and (not (eq (car clients-list) cl)) ; skip ourself
                   ;; skip iconified
                   (not (xwem-cl-iconified-p (car clients-list))))
          ;; exclude dummy clients
          (if (and (not (xwem-dummy-client-p (car clients-list)))
                   (or visible
                       (not (xwem-win-cl-current-p (car clients-list)))))
              (progn
                (setq rcl (car clients-list))
                (setq clients-list nil))

            (when (and visible (null notgoodcl))
              (setq notgoodcl (car clients-list)))))
        (setq clients-list (cdr clients-list)))

      (or rcl notgoodcl))))

(define-xwem-deferred xwem-clgen-activate-other (cl win)
  "Activate other client in WIN.
Clients list is either WIN's clients list or CL's win clients list if
WIN's clients list is empty and CL managed in frame where WIN."
  (when (and (xwem-win-alive-p win)
             (not (xwem-cl-alive-p (xwem-win-cl win))))
    ;; WIN is valid and no clients yet managed in WIN
    (let ((ocl (or (and (xwem-win-clients win) ; There clients to
                        (xwem-clgen-other-client cl (xwem-win-clients win)))
                   (and (xwem-win-alive-p (xwem-cl-win cl))
                        (eq (xwem-win-frame win)
                            (xwem-win-frame (xwem-cl-win cl)))
                        (xwem-win-clients (xwem-cl-win cl))
                        (xwem-clgen-other-client
                         cl (xwem-win-clients (xwem-cl-win cl))))))
          (need-select (or (and (xwem-cl-selected-p cl)
                                (not (xwem-cl-active-p cl)))
                           (xwem-dummy-client-p (xwem-cl-selected)))))
      (when (xwem-cl-alive-p ocl)
        (xwem-win-set-cl win ocl)
        (when (and (xwem-win-selected-p win) need-select)
          (xwem-select-client ocl)))

      ;; If OCL wasnt selected, try last or other client
      (when (and (xwem-win-selected-p win)
                 (not (xwem-cl-selected-p ocl))
                 need-select)
        (setq ocl (xwem-clgen-other-client
                   cl (xwem-frame-clients (xwem-win-frame win)) t))
        (if (xwem-cl-alive-p ocl)
            (xwem-select-client ocl)
          (xwem-select-some-client cl))))))

(defun xwem-clgen-other-on-split (sp-win nwin)
  "NWIN has been created as a result of split.
Probably we want to manage some client in newly created window."
  (xwem-clgen-activate-other (xwem-cl-selected) nwin))

;;; Othen-on-Split commands
(define-xwem-command xwem-clgen-turn-on-other-on-split ()
  "Turn on `xwem-clgen-other-on-split' minor mode."
  (xwem-interactive)

  (setq xwem-clgen-other-on-split t)
  (add-hook 'xwem-win-split-hook 'xwem-clgen-other-on-split)

  (xwem-message 'info "Other on split minor mode is ON."))

(define-xwem-command xwem-clgen-turn-off-other-on-split ()
  "Turn off `xwem-clgen-other-on-split' minor mode."
  (xwem-interactive)

  (setq xwem-clgen-other-on-split nil)
  (remove-hook 'xwem-win-split-hook 'xwem-clgen-other-on-split)

  (xwem-message 'info "Other on split minor mode is OFF."))

(define-xwem-command xwem-clgen-toggle-other-on-split (arg)
  "Toggle `xwem-clgen-other-on-split' minor mode.
Negative ARG turns it off, positive turns it on."
  (xwem-interactive "P")

  (if (numberp arg)
      (if (> arg 0)
          (setq xwem-clgen-other-on-split nil)
        (setq xwem-clgen-other-on-split t)))

  (if xwem-clgen-other-on-split
      (xwem-clgen-turn-off-other-on-split)
    (xwem-clgen-turn-on-other-on-split)))

;;; Initialisation stuff
(defun xwem-clgen-init ()
  "Initialise clgen stuff."
  (xwem-message 'init "Initializing generic clients ...")

  (add-hook 'xwem-win-split-hook 'xwem-clgen-other-on-split)

  (xwem-message 'init "Initializing generic clients ... done"))

;;;; ---- Generic methods ----
(define-xwem-client-property expect-win nil
  "Expectance window."
  :type 'window
  :get 'xwem-cl-get-sys-prop
  :set 'xwem-cl-put-sys-prop)

(defun xwem-manage-generic (cl)
  "Manage method for generic clients."
  (let ((dwin (and (xwem-cl-was-expected-p cl)
                   (xwem-client-property cl 'expect-win))))
    (when (numberp dwin)
      (setq dwin (xwem-win-find-by-id dwin)))

    (if dwin
        (xwem-client-set-property cl 'expect-win nil)
      (if (xwem-frame-alive-p (xwem-frame-selected))
          (setq dwin (xwem-win-selected))
        (when xwem-clgen-allow-make-frame
          ;; Selected window is kinda dead
          (xwem-frame-fit-screen (xwem-make-frame-1 'desktop))
          (setq dwin (xwem-win-selected)))))

    (unless (xwem-win-alive-p dwin)
      (error 'xwem-error "Can't manage in dead window"))

    ;; Create parent window.
    ;; NOTE:
    ;;   Some applications, such as mozilla, when running with
    ;;   -remote tries to find another mozilla instance to run
    ;;   in it, it seaches lowerest client, but it is not
    ;;   guarantied, because xwem frame holds many clients.
    (unless (xwem-cl-xparent cl)
      (setf (xwem-cl-xparent cl)
            (XCreateWindow (xwem-dpy) nil 0 0 1 1 0 nil nil nil
                           :override-redirect t :event-mask 0.0)))

    ;; Setup generic client CL
    (xwem-cl-set-win cl dwin)
    (xwem-use-local-map xwem-clgen-map cl)

    ;; Select newly managed client, if needed
    (if (or (and (eq (xwem-cl-state cl) 'unknown)
                 (eql (xwem-cl-get-init-state cl) X-IconicState))
            (eq (xwem-cl-get-prop cl 'state) 'iconified))
        (xwem-iconify cl)

      ;; Activate client in window or put it in `inactive' state.
      (when xwem-clgen-activate-new
        (if (and (xwem-win-selected-p dwin)
                 (xwem-win-cl dwin))
            (xwem-client-change-state cl 'inactive)
        (xwem-win-set-cl (xwem-cl-win cl) cl)))

      ;; Select client
      (when (and (xwem-win-selected-p dwin)
                 (or xwem-clgen-select-new
                     (eq (xwem-win-cl dwin) cl)))
        (xwem-win-set-cl (xwem-cl-win cl) cl)
        (xwem-select-client cl)))))

(defun xwem-clgen-refit (cl)
  "Refit generic client CL."
  (let ((xwem-win (xwem-cl-win cl))
        hthi)
    (when (xwem-win-alive-p xwem-win)
      (setq hthi (xwem-win-border-width xwem-win))
      (when (and (xwem-cl-new-xgeom cl)
                 (X-Geom-border-width (xwem-cl-new-xgeom cl)))
        ;; Border width changed
        (setf (X-Geom-border-width (xwem-cl-xgeom cl))
              (X-Geom-border-width (xwem-cl-new-xgeom cl))))

      (xwem-cl-correct-size-for-size
       cl
       (make-X-Geom :x (+ (xwem-win-x xwem-win) hthi)
                    :y (+ (xwem-win-y xwem-win) hthi)
                    :width (- (xwem-win-width xwem-win) (* 2 hthi))
                    :height (- (xwem-win-height xwem-win) (* 2 hthi))
                    :border-width (X-Geom-border-width (xwem-cl-xgeom cl)))))))

(defun xwem-refit-generic (cl)
  "Refit method for generic client CL.
Correct CL geometry to fit into CL's window."
  (xwem-clgen-refit cl)
  (xwem-cl-apply-xgeom cl))

(defun xwem-clgen-has-xparent (cl xparent)
  "Return non-nil if generic client CL has XPARENT last-parent."
  (let ((clxp (xwem-cl-get-sys-prop cl 'last-xparent)))
    (and (X-Win-p clxp)
         (X-Win-equal clxp xparent))))

(defun xwem-clgen-xreparent (cl xparwin x y)
  (XReparentWindow (xwem-dpy) (xwem-cl-xwin cl) xparwin x y)
  (xwem-cl-put-sys-prop cl 'last-xparent xparwin))

(define-xwem-deferred xwem-clgen-apply-state (cl)
  "Apply CL's state to life for generic client CL."
  (when (and (xwem-cl-p cl)
             (eq (xwem-cl-manage-type cl) 'generic))
    (cond ((xwem-cl-active-p cl)
           (xwem-clgen-refit cl)
           (when (and (xwem-cl-frame cl)
                      (not (xwem-clgen-has-xparent
                            cl (xwem-frame-xwin (xwem-cl-frame cl)))))
             (xwem-clgen-xreparent cl (xwem-frame-xwin (xwem-cl-frame cl))
                                   (X-Geom-x (xwem-cl-xgeom cl))
                                   (X-Geom-y (xwem-cl-xgeom cl))))
           (xwem-cl-apply-xgeom-1 cl)
           (XLowerWindow (xwem-dpy) (xwem-cl-xwin cl))
           (XMapWindow (xwem-dpy) (xwem-cl-xwin cl)))

          ((xwem-cl-inactive-p cl)
           (unless (xwem-clgen-has-xparent
                    cl (xwem-cl-xparent cl))
             (xwem-clgen-xreparent cl (xwem-cl-xparent cl) 0 0)
             (XLowerWindow (xwem-dpy) (xwem-cl-xwin cl))
             (XUnmapWindow (xwem-dpy) (xwem-cl-xwin cl))))

          ((xwem-cl-iconified-p cl)
           (unless (xwem-clgen-has-xparent
                    cl (xwem-cl-xparent cl))
             (xwem-clgen-xreparent cl (xwem-cl-xparent cl) 0 0)
             (XLowerWindow (xwem-dpy) (xwem-cl-xwin cl))
             (XUnmapWindow (xwem-dpy) (xwem-cl-xwin cl)))))
    ))

(defun xwem-activate-generic (cl &optional type)
  "Activate method for CL."
  (cond ((eq type 'select)
	 (unless (xwem-win-alive-p (xwem-cl-win cl))
	   (xwem-win-set-cl (xwem-win-selected) cl))
	 (xwem-select-window (xwem-cl-win cl)))

        ((eq type 'activate)
         (cond ((xwem-win-cl-current-p cl)
                (xwem-clgen-apply-state cl))

               ((xwem-win-alive-p (xwem-cl-win cl))
                (xwem-client-change-state cl 'inactive)
                (xwem-win-set-cl (xwem-cl-win cl) cl))))))

(defun xwem-deactivate-generic (cl &optional type)
  "Deactivate method for generic client CL."
  (cond ((eq type 'deactivate)
         (xwem-clgen-activate-other cl (xwem-cl-win cl))
         (xwem-clgen-apply-state cl))))

(defun xwem-iconify-generic (cl)
  "Iconify method for generic client CL."
  (xwem-win-rem-cl (xwem-cl-win cl) cl))

(defun xwem-withdraw-generic (cl)
  "Withdraw method for generic client CL."
  (let ((win (xwem-cl-win cl)))
    (when win
      (xwem-win-rem-cl win cl)
      (xwem-cl-set-win cl nil)

      ;; Activate other client in CL's window WIN
      ;; NOTE: Deferred
      (xwem-clgen-activate-other nil win))))

;;; Additional methods
(define-xwem-method on-type-change generic (cl &optional new)
  "Called when CL is about to change manda from generic."
  (xwem-cl-set-win cl nil)
  (xwem-cl-rem-sys-prop cl 'last-xparent))

(define-xwem-method on-kill generic (cl)
  "Called when CL is killed."
  (let ((win (xwem-cl-win cl)))
    ;; Destroy parent window
    (when (xwem-cl-xparent cl)
;; This XDestroyWindow causes some problems
;      (XDestroyWindow (xwem-dpy) (xwem-cl-xparent cl))
      (setf (xwem-cl-xparent cl) nil))

    ;; Activate other client in WIN
    (xwem-clgen-activate-other cl win)

    ;; Remove CL from WIN's clients list
    (when (xwem-win-alive-p win)
      (xwem-win-rem-cl win cl))
    ))

(define-xwem-method other-client generic (cl)
  "Method to return xwem generic client other then CL."
  (xwem-clgen-other-client cl))


(provide 'xwem-clgen)

;;;; On-load actions:
;; Register generic manage type.  Use APPEND because 'generic manage
;; type is most non-privileged and matches any client.
(define-xwem-manage-model generic
  "Generic manage model.
Manage clients that no-one elso wants to manage."
  :qualifier '(t)                       ; always match
  :append t

  :cl-properties xwem-generic-client-properties
  :manage-properties '(win-support t)
  :manage-method 'xwem-manage-generic
  :activate-method 'xwem-activate-generic
  :deactivate-method 'xwem-deactivate-generic
  :refit-method 'xwem-refit-generic
  :iconify-method 'xwem-iconify-generic
  :withdraw-method 'xwem-withdraw-generic)

(if xwem-started
    (xwem-clgen-init)
  (add-hook 'xwem-before-init-wins-hook 'xwem-clgen-init))

;;; xwem-clgen.el ends here
