;;; xwem-transient.el --- Transient for clients support.

;; Copyright (C) 2004-2007 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;; Created: Sat Jun  5 01:33:25 MSD 2004
;; Keywords: xwem
;; Time-stamp: <16/4/2010 17:02:18 lg@localhost>

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
(require 'xwem-focus)
(require 'xwem-manage)

;;; Customisation
(defgroup xwem-transient nil
  "Group to customize transient clients support."
  :prefix "xwem-transient-"
  :group 'xwem-cl)

(defcustom xwem-transient-client-properties
  '(x-border-width 2 x-border-color "blue4" xwem-focus-model click-focus)
  "*Client properties to use when managing transient clients."
  :type '(repeat (list :inline t
                       (symbol :tag "property")
                       (sexp :tag "value")))
  :group 'xwem-transient)

(defcustom xwem-transient-switch-back t
  "*Non-nil mean when transient client dies, switch to client who created it."
  :type 'boolean
  :group 'xwem-transient)

;;; Internal variables

(defvar xwem-transient-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (xwem-kbd "<ESC>") 'xwem-client-kill)
    (define-key map (xwem-kbd "C-<button1>") 'xwem-client-imove)
    (define-key map (xwem-kbd "C-<button2>") 'xwem-client-idestroy)
    (define-key map (xwem-kbd "C-<button3>") 'xwem-client-iresize)
    map)
  "Local keymap for transient-for clients.")

(define-xwem-deferred xwem-transient-on-select-cl (&optional cl)
  "CL just selected, check if it has transient-for windows.
If so, popup them."
  (unless cl
    (setq cl (xwem-cl-selected)))

  (when (xwem-cl-p cl)
    (let ((trfcls (xwem-cl-list-sort-by-recency (xwem-cl-translist cl))))
      (mapc #'xwem-transient-apply-state (nreverse trfcls)))))

;;;; ---- Transient for manage methods ----

;; NOTE: Uses default refit
(defun xwem-cl-transient-for-p (cl)
  "Return non-nil if CL is transient for client."
  (xwem-hints-wm-transient-for (xwem-cl-hints cl)))

(defun xwem-manage-transient-for (cl)
  "Manage CL that have transient-for flag."
  ;; Map window for witch CL is transient and just map and raise CL
  ;; over it
  (let ((xwin (xwem-cl-xwin cl))
        (trc (xwem-xwin-cl (xwem-cl-transient-for cl))))

    ;; If have no suitable X/Y position, try to put client at the center
    ;; of TRC
    (when (and (xwem-cl-p trc)
               (zerop (X-Geom-x (xwem-cl-xgeom cl)))
               (zerop (X-Geom-y (xwem-cl-xgeom cl))))
      (let* ((nx (/ (- (X-Geom-width (xwem-cl-xgeom trc))
                       (X-Geom-width (xwem-cl-xgeom cl))) 2))
             (ny (/ (- (X-Geom-height (xwem-cl-xgeom trc))
                       (X-Geom-height (xwem-cl-xgeom cl))) 2))
             (tpnt (car (XTranslateCoordinates
                         (xwem-dpy) (xwem-cl-xwin trc)
                         (xwem-rootwin) nx ny))))
        (setf (X-Geom-x (xwem-cl-xgeom cl)) nx
              (X-Geom-y (xwem-cl-xgeom cl)) ny)))

    ;; XXX make root window be parent
    (XReparentWindow (xwem-dpy) xwin (xwem-rootwin)
                     (X-Geom-x (xwem-cl-xgeom cl))
                     (X-Geom-y (xwem-cl-xgeom cl)))

    (when (xwem-cl-p trc)
      (setf (xwem-cl-translist trc)
            (cons cl (xwem-cl-translist trc)))
      ;; And inherit always on top rank
      (setf (xwem-xwin-rank (xwem-cl-xwin cl))
            (xwem-xwin-rank (xwem-cl-xwin trc))))

    ;; Install transient local keymap
    (xwem-use-local-map xwem-transient-keymap cl)

    ;; Select it if needed
    (when (or (null trc)
              (xwem-cl-selected-p trc))
      (xwem-activate cl))))

(define-xwem-deferred xwem-transient-apply-state (cl)
  "Apply CL's state to life."
  (cond ((xwem-cl-active-p cl)
         (xwem-misc-raise-xwin (xwem-cl-xwin cl))
         (XMapWindow (xwem-dpy) (xwem-cl-xwin cl))
         (xwem-select-client cl))
;         (xwem-focus-set cl))

        ((xwem-cl-inactive-p cl)
         (xwem-misc-lower-xwin (xwem-cl-xwin cl))
         (XUnmapWindow (xwem-dpy) (xwem-cl-xwin cl)))
        ((xwem-cl-iconified-p cl)
         (XUnmapWindow (xwem-dpy) (xwem-cl-xwin cl)))))

(defun xwem-activate-transient-for (cl &optional type)
  "Activate method for transient-for client CL."
  (cond ((eq type 'select)
         (let ((trc (xwem-xwin-cl (xwem-cl-transient-for cl))))
           (when (xwem-cl-p trc)
             (xwem-activate trc)))
         (xwem-deferred-funcall 'xwem-misc-raise-xwin (xwem-cl-xwin cl)))

        ((eq type 'activate)
         (xwem-transient-apply-state cl))))

(defun xwem-deactivate-transient-for (cl &optional type)
  "Deactivate method for transient-for client CL."
  (cond ((eq type 'deactivate)
         (xwem-transient-apply-state cl))))

(defun xwem-iconify-transient-for (cl &optional type)
  "Iconify method for transient-for client CL."
  (xwem-transient-apply-state cl))

(defun xwem-withdraw-transient-for (cl)
  "Withdraw method for transient-for CL."
  (let ((trc (xwem-xwin-cl (xwem-cl-transient-for cl))))
    (when (and (xwem-cl-selected-p cl)
               (xwem-cl-p trc) (xwem-cl-active-p trc))
      (xwem-select-client trc))))

;;; Additional methods
(define-xwem-method on-kill transient-for (cl)
  "On-kill method for transient-for clients."
  (let ((trc (xwem-xwin-cl (xwem-cl-transient-for cl))))
    (when (xwem-cl-p trc)
      (setf (xwem-cl-translist trc)
            (delq cl (xwem-cl-translist trc)))

      (when (and (xwem-cl-selected-p cl)
                 (xwem-cl-active-p trc))
        (xwem-select-client trc)))))


(provide 'xwem-transient)

;;; On-load actions
(define-xwem-manage-model transient-for
  "Manage models for clients with TRANSIENT_FOR property."
  :qualifier '(predicate xwem-cl-transient-for-p)

  :cl-properties xwem-transient-client-properties
  :manage-properties '(override-manage-list t)
  :manage-method 'xwem-manage-transient-for
  :activate-method 'xwem-activate-transient-for
  :deactivate-method 'xwem-deactivate-transient-for
  :iconify-method 'xwem-iconify-transient-for
  :withdraw-method 'xwem-withdraw-transient-for)

(add-hook 'xwem-client-select-hook 'xwem-transient-on-select-cl)

;;; xwem-transient.el ends here
