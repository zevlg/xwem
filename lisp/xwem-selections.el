;;; xwem-selections.el --- Support for X selections.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed May  5 17:06:41 MSD 2004
;; Keywords: xwem
;; Time-stamp: <26/7/2007 18:29:30 lg@h1>

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
(require 'xwem-help)

(defgroup xwem-selections nil
  "Group to customize `xwem-selections'."
  :prefix "xwem-selections-"
  :group 'xwem)

(defcustom xwem-selections-maximum 20
  "Maximum number of saved selections."
  :type 'number
  :group 'xwem-selections)

(defcustom xwem-selections-no-remove t
  "*Non-nil for reverse meaning of prefix arg for `xwem-paste-cutbuffer' command.
Without prefix arg, keep currently pasted cutbuffer.
With prefix arg, remove it."
  :type 'boolean
  :group 'xwem-selections)

;;; Internal variables


(defvar xwem-selections nil
  "Ring of saved selections.
Actually alist")

(defvar xwem-selection-xwin nil
  "X-Win used to operate on selections.")

(defun xwem-init-selections ()
  "Initialize selections mechanism."
  (setq xwem-selection-xwin
        (XCreateWindow (xwem-dpy) (xwem-rootwin)
                       0 0 1 1 0 nil nil nil
                       :override-redirect t
                       :event-mask (Xmask-or XM-StructureNotify)))

  (X-Win-EventHandler-add-new xwem-selection-xwin 'xwem-selection-get nil
                              (list X-SelectionNotify)))

(defun xwem-selection-req (sel &optional targ prop)
  "Issue XConvertSelection."
  (unless targ
    (setq targ XA-string))

  (XConvertSelection (xwem-dpy)
                     (if (X-Atom-p sel) sel (XInternAtom (xwem-dpy) sel t))
                     (if (X-Atom-p targ) targ (XInternAtom (xwem-dpy) targ t))
                     (XInternAtom
                      (xwem-dpy) (or prop "XWEM_SELECTION_PROPERTY") t)
                     xwem-selection-xwin)
  )

(defun xwem-selection-get (xdpy xwin xev)
  "On display XDPY and window XWIN process SelectionNotify event XEV."
  (xwem-debug 'xwem-misc "here prop=%d"
              '(X-Atom-id (X-Event-xselection-property xev)))
  (let (target prov)
    (if (not (= (X-Atom-id (X-Event-xselection-property xev)) X-None))
        (progn
          (if (X-Atom-equal (X-Event-xselection-target xev)
                            (XInternAtom xdpy "XA_TARGETS" t))
              (setq target XA-atom)
            (setq target (X-Event-xselection-target xev)))

          (setq prov (XGetWindowProperty
                      xdpy (X-Event-xselection-requestor xev)
                      (X-Event-xselection-property xev) nil nil nil target))

          (xwem-debug 'xwem-misc "Got prov=%S, prop=%S target=%S"
                      'prov '(X-Atom-id (X-Event-xselection-property xev))
                      '(X-Atom-id target)))
      )))

;;;###xwem-autoload
(defun xwem-selection (&optional type)
  "Return text selection of given TYPE.
TYPE is one of:
  'local - Active Emacs region.
  'x     - PRIMARY X selection.
  'any   - Either active region or PRIMARY X selection.

Default TYPE is 'local."
  (unless type (setq type 'local))
  (ecase type
    (local (when (region-active-p)
             (with-current-buffer (zmacs-region-buffer)
               (buffer-substring (region-beginning) (region-end)))))
    (x (or (get-selection-no-error 'PRIMARY 'UTF8_STRING)
           (get-selection-no-error 'PRIMARY 'STRING)
           (get-selection-no-error 'CLIPBOARD 'UTF8_STRING)
           (get-selection-no-error 'CLIPBOARD 'STRING)))
    (any (or (xwem-selection 'local)
             (xwem-selection 'x)))))

;;;###autoload(autoload 'xwem-help-cutbuffers "xwem-selections" "Display help about cutbuffers." t)
(define-xwem-command xwem-help-cutbuffers ()
  "Show help buffer about cutbuffers."
  (xwem-interactive)

  (xwem-help-display "cutbuffers"
    (insert "X cutbuffers:\n\n")
    (insert "NUMBER   VALUE\n")
    (insert "------   -----\n")
    (insert (format "%-9s%S\n" 'PRIMARY (xwem-selection 'x)))
    (mapc #'(lambda (n)
              (let ((cbval (x-get-cutbuffer n)))
                (when cbval
                  (insert (format "%-9d%S\n" n cbval)))))
          '(0 1 2 3 4 5 6 7))

    (insert "\n")

    (insert "XWEM selections:\n\n")
    (insert "NUMBER   VALUE\n")
    (insert "------   -----\n")
    (let ((nsel 0))
      (mapc #'(lambda (s)
                (insert (format "%-9d%S\n" nsel s))
                (incf nsel))
            xwem-selections))))

;;;###autoload(autoload 'xwem-copy-cutbuffer "xwem-selections" "Copy CUTBUFFER0 to `xwem-selections'." t)
(define-xwem-command xwem-copy-cutbuffer (&optional which-one)
  "Copy WHICH-ONE cutbuffer to `xwem-selections'.
However if Emacs region activated, region is copied instead of
cutbuffer."
  (xwem-interactive "p")

  (let ((sel (or (xwem-selection 'any)
                 (x-get-cutbuffer which-one))))
    (zmacs-deactivate-region)             ; XXX
    (if (not sel)
        (xwem-message 'note "No active selection")
      (push sel xwem-selections)
      (xwem-message 'info "Copying %S" sel))))

;;;###autoload(autoload 'xwem-paste-cutbuffer "xwem-selections" "Paste CUTBUFFER0 to `xwem-selections'." t)
(define-xwem-command xwem-paste-cutbuffer (&optional no-remove)
  "Paste's most recent cutbuffer from `xwem-selections' to selected client.
cutbuffer is removed from `xwem-selections', unless NO-REMOVE is non-nil.
However if `xwem-selections-no-remove' is non-nil, NO-REMOVE have
opposite meaning."
  (xwem-interactive "_P")

  (let ((sidx (or (and (numberp no-remove)
                       no-remove)
                  0))
        sel)
    (when (> sidx (1- (length xwem-selections)))
      (error 'xwem-error (format "No %d selection" sidx)))

    (setq sel (nth sidx xwem-selections))
    (xwem-kbd-force-mods-release)
    (mapc 'xwem-unread-command-event sel)

    ;; Remove SEL from `xwem-selections'?
    (setq no-remove (and no-remove (listp no-remove)))
    (unless (or (and xwem-selections-no-remove
                     (not no-remove))
                (and (not xwem-selections-no-remove)
                     no-remove))
      (setq xwem-selections (delq sel xwem-selections)))))


(provide 'xwem-selections)

;;; xwem-selections.el ends here
