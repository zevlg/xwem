;;; xwem-clswi.el --- Simple clients switching.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;; Keywords: xwem
;; Time-stamp: <7/10/2007 22:20:31 lg@h1>

;; This file is NOT part of XEmacs.

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

;; Included as default bindings, `H-[', `H-]', `H-{', `H-}'.

;; If current client supports windowing, than next/prev client in its
;; window is selected.  If client does not support windowing (for
;; example fullscreen client) next/prev client of same manage type is
;; selected.

;;; Code:

(require 'xwem-clients)

(defgroup xwem-clswi nil
  "Group to customize clients switcher."
  :prefix "xwem-clswi-"
  :group 'xwem)

(defcustom xwem-clswi-beep-on-error t
  "*Non-nil mean beep on any error."
  :type 'boolean
  :group 'xwem-clswi)

(defcustom xwem-clswi-show-info nil
  "*Non-nil mean show info about client in xwem minibuffer after switch.
It also can be a function which accepts one argument - client and
return non-nil to show info."
  :type '(restricted-sexp :match-alternatives (functionp boolean-p))
  :group 'xwem-clswi)

;;; Internal variables

(defun xwem-clswi-windowing (window arg)
  "Switch to ARG's next client in window.
Return client that was activated."
  (let ((ccl (xwem-win-cl window))
        (cls (xwem-win-clients window))
        cclinx num scl)
    (unless cls
      (error 'xwem-error "No client to switch"))

    (setq cclinx (- (length cls) (length (memq ccl cls))))
    (setq num (% (+ cclinx arg) (length cls)))
    (setq scl (nth (if (natnump num) num (+ (length cls) num)) cls))

    (xwem-win-set-cl window scl)
    (if (xwem-win-selected-p window)
        (xwem-select-client scl)
      (xwem-activate scl))
    scl))

(defun xwem-clswi-non-windowing (cl arg)
  "Switch to ARG's other client of same client type as CL.
Return client that was activated."
  (let* ((mtype (xwem-cl-manage-type cl))
         (ncls (xwem-cl-list-sort-by-recency
                (xwem-clients-list
                 #'(lambda (ccl)
                     (eq (xwem-cl-manage-type ccl) mtype))
                 t))))
    (when (< arg 0)
      (setq ncls (nreverse ncls))
      (setq arg (- arg)))

    (while (> arg 0)
      (setq cl (xwem-cl-other cl :clients ncls))
      (decf arg))

    (unless (xwem-cl-p cl)
      (error 'xwem-error (format "No other %s client"
				 (upcase (symbol-name mtype)))))

    (xwem-select-client cl)
    cl))

;;;###autoload(autoload 'xwem-clswi-next "xwem-clswi" nil t)
(define-xwem-command xwem-clswi-next (cl &optional arg)
  "According to CL, switch to ARG's next client.
If WIN is ommited then in selected window."
  (xwem-interactive (list (xwem-cl-selected)
                          (prefix-numeric-value xwem-prefix-arg)))

  (let ((window (xwem-cl-win cl)))
    (setq cl (if window
                 (xwem-clswi-windowing window arg)
                (let* ((twin (xwem-cl-transient-for cl))
                       (tcl (and (X-Win-p twin)
                                 (xwem-xwin-cl twin))))
                  (if (xwem-cl-p tcl)
                      (xwem-clswi-next tcl arg)
                   (xwem-clswi-non-windowing cl arg)))))

    (when (or (and (functionp xwem-clswi-show-info)
                   (funcall xwem-clswi-show-info cl))
              (and xwem-clswi-show-info
                   (not (functionp xwem-clswi-show-info))))
      (xwem-client-info cl))
    cl))

;;;###autoload(autoload 'xwem-clswi-prev "xwem-clswi" nil t)
(define-xwem-command xwem-clswi-prev (cl &optional arg)
  "According to CL, switch to ARG's previous client.
If WIN is ommited then in selected window."
  (xwem-interactive (list (xwem-cl-selected)
                          (prefix-numeric-value xwem-prefix-arg)))

  (xwem-clswi-next cl (- arg)))

;;;###autoload(autoload 'xwem-clswi-next-other-window "xwem-clswi" nil t)
(define-xwem-command xwem-clswi-next-other-window (arg)
  "Switch next ARG client in other window."
  (xwem-interactive "p")

  (let ((win (xwem-window-other 1 (xwem-win-selected))))
    (when (and (xwem-win-p win)
               (not (eq win (xwem-win-selected))))
      (xwem-clswi-next (xwem-win-cl win) arg))))

;;;###autoload(autoload 'xwem-clswi-prev-other-window "xwem-clswi" nil t)
(define-xwem-command xwem-clswi-prev-other-window (arg)
  "Switch previous ARG client in other window."
  (xwem-interactive "p")
  (xwem-clswi-next-other-window (- arg)))


(provide 'xwem-clswi)

;;; xwem-clswi.el ends here
