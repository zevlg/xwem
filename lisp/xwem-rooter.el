;;; xwem-rooter.el --- OnRoot clients support.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sat Feb 21 03:41:02 MSK 2004
;; Keywords: xwem
;; Time-stamp: <20/4/2010 18:09:30 lg@localhost>

;; This file is part of XWEM.

;; XWEM is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XWEM is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;;

;;; Code:

(require 'xwem-load)
(require 'xwem-manage)
(require 'xwem-focus)

;;; Customisation
(defgroup xwem-rooter nil
  "Group to customize rooter apps."
  :prefix "xwem-rooter-"
  :group 'xwem-modes)

(defcustom xwem-rooter-always-on-top-spec '(((t) . (nil . 30)))
  "*List of conscells in form:
\(QUALIFIER . \(INACTIVE-RANK . ACTIVE-RANK)) for always-on-top icons.
If QUALIFIER matches rooticon's client - than RANK is set as always
on top rank."
  :type 'sexp
  :group 'xwem-rooter)

(defcustom xwem-rooter-client-properties
  '(noselect t xwem-focus-mode nil)
  "*Plist of properties for rooter clients."
  :type '(restricted-sexp :match-alternatives (valid-plist-p))
  :group 'xwem-rooter)

;;; Internal variables

(defvar xwem-rooter-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (xwem-kbd "C-<button1>") 'xwem-client-imove)
    (define-key map (xwem-kbd "C-<button3>") 'xwem-client-iresize)
    (define-key map (xwem-kbd "C-<button2>") 'xwem-client-idestroy)
    (define-key map (xwem-kbd "C-Sh-<button1>") 'xwem-rooter-raise)
    (define-key map (xwem-kbd "C-Sh-<button3>") 'xwem-rooter-lower)
    map)
  "Keymap for rooter clients.")

(defun xwem-cl-rooter-p (cl)
  "Return non-nil if CL is rooter client."
  (eq (xwem-cl-manage-type cl) 'rooter))

(define-xwem-command xwem-rooter-raise ()
  "Raise rooter window."
  (xwem-interactive "_")
  (xwem-misc-raise-xwin (xwem-cl-xwin xwem-event-client)))

(define-xwem-command xwem-rooter-lower ()
  "Lower rooter window."
  (xwem-interactive "_")
  (xwem-misc-lower-xwin (xwem-cl-xwin xwem-event-client)))


(defun xwem-rooter-set-rank (cl)
  "Set always on top rank for CL, according to its state."
  ;; Set apropriate always on top rank
  (let ((rank (find cl xwem-rooter-always-on-top-spec
                    :key 'car :test 'xwem-cl-match-p)))
    (case (xwem-cl-state cl)
      (inactive
       (setf (xwem-xwin-rank (xwem-cl-xwin cl)) (cadr rank))
       (xwem-misc-lower-xwin (xwem-cl-xwin cl)))
      (active
       (setf (xwem-xwin-rank (xwem-cl-xwin cl)) (cddr rank))
       (xwem-misc-raise-xwin (xwem-cl-xwin cl))))))

;;;; Manage methods
;;;###autoload
(defun xwem-manage-rooter (cl)
  "Manage rooter client CL."
  ;; Root window is parent for rooters, that is why rooter major mode
  ;; called rooter :)
  (XReparentWindow (xwem-dpy) (xwem-cl-xwin cl) (xwem-rootwin)
                   (X-Geom-x (xwem-cl-xgeom cl))
                   (X-Geom-y (xwem-cl-xgeom cl)))

  (XMapWindow (xwem-dpy) (xwem-cl-xwin cl))

  ;; Setup and select client client
  (xwem-use-local-map xwem-rooter-mode-map cl)
  (xwem-select-client cl))

;;;###autoload(put 'manage 'rooter 'xwem-manage-rooter)

(defun xwem-activate-rooter (cl &optional type)
  "Activate method for rooter clients."
  (xwem-rooter-set-rank cl)
  (xwem-misc-raise-xwin (xwem-cl-xwin cl))
  (XMapWindow (xwem-dpy) (xwem-cl-xwin cl)))

(defun xwem-deactivate-rooter (cl &optional type)
  "Deactivate method for rooter clients."
  (xwem-rooter-set-rank cl)
  (xwem-misc-lower-xwin (xwem-cl-xwin cl)))


(provide 'xwem-rooter)

;;;; On-load actions
;; Rooter manage type
(define-xwem-manage-model rooter
  "Managing model to show client on root window."
  :cl-properties xwem-rooter-client-properties
  :manage-method 'xwem-manage-rooter
  :activate-method 'xwem-activate-rooter
  :deactivate-method 'xwem-deactivate-rooter)

;;; xwem-rooter.el ends here
