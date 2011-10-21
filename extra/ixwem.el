;;; ixwem.el --- Interactive xwem.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sat Sep 11 21:38:13 GMT 2004
;; Keywords: xwem
;; Time-stamp: <30/11/2006 00:01:03 lg@h1>

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


;; Highlighting.
;; TODO:
;;   - font lock faces
;;   -
(require 'xwem-load)

(defgroup ixwem nil
  "Group to customize IXWEM."
  :prefix "ixwem-"
  :group 'xwem)

(defcustom ixwem-name-length 32
  "*Maximum length of xwem client name to display."
  :type 'number
  :group 'ixwem)

;;; Internal variables

(defvar ixwem-header-line
  (concat " IMD Manda     Client                   Size     Frame    Uptime\n"
          " --- -----     ------                   ----     -----    ------\n"))

(defvar ixwem-local-map 
  (let ((map (make-sparse-keymap)))
    (define-key map ?q 'ixwem-quit)
    map)
  "Keymap used when in ixwem mode.")


(defun ixwem-client-format (cl)
  (format (format "    %%.%ds\n" ixwem-name-length)
	  (xwem-client-name cl)))

(defun ixwem-draw-tree (buf)
  "Draw clients tree in buffer BUF."
  (with-current-buffer buf
    (erase-buffer buf)

    (insert (concat "    Name" (make-string (- ixwem-name-length 3) ?\x20)
		    "Size      " "Uptime      " "Recency\n"))
    (insert (concat "    ----" (make-string (- ixwem-name-length 3) ?\x20)
		    "----      " "------      " "-------\n"))
    (mapc (lambda (ma)
	    (insert (format "[%S]\n" (xwem-manda-name ma)))
	    (cond ((eq (xwem-manda-name ma) 'generic)
		   (mapc (lambda (fr)
			   (insert (format "  {F%d: %s}\n"
					   (xwem-frame-num fr) (xwem-frame-name fr)))
			   (mapc (lambda (cl)
				   (when (and (eq (xwem-cl-frame cl) fr)
					      (eq (xwem-cl-manda cl) ma))
				     (insert (ixwem-client-format cl))))
				 xwem-clients))
			 xwem-frames-list))
		  (t (mapc (lambda (cl)
			     (when (eq (xwem-cl-manda cl) ma)
			       (insert (ixwem-client-format cl))))
			   xwem-clients))))
	  xwem-manda-list)
    ))

(defun ixwem-list-clients (buf)
  "List xwem clients in BUF buffer."
  (with-current-buffer buf
    (erase-buffer)

    ;; Display header
    (insert ixwem-header-line)

    ;; Display clients tree
    (mapcar (lambda (cl)
	      (let ((bstr (make-string 40 ?\x20)))
		(insert
		 (format "  *# %.9s %.24s %.8s %.8s %.20s\n"
			 (concat (symbol-name (xwem-manda-name (xwem-cl-manda cl))) bstr)
			 (concat (xwem-client-name cl) bstr)
			 (concat (let ((gg (xwem-cl-get-usize cl)))
				   (format "%dx%d" (car gg) (cdr gg)))
				 bstr)
			 (if (xwem-cl-frame cl)
			     (concat (xwem-frame-name (xwem-cl-frame cl)) bstr)
			   bstr)
			 (concat (xwem-cl-get-uptime cl) bstr)))))
	    xwem-clients)
    ))

(defun ixwem-mode ()
  "Enter ixwem mode."
  (setq major-mode 'ixwem)
  (setq mode-name "IXWEM")
  (use-local-map ixwem-local-map)
  )

;;;###autoload(autoload 'ixwem "ixwem" "" t)
(define-xwem-command ixwem ()
  "Interactive xwem clients browsing."
  (xwem-interactive)

  (let ((buf (get-buffer-create "*ixwem*")))
    (switch-to-buffer buf)
    (ixwem-draw-tree buf)
;    (ixwem-list-clients buf)
    (ixwem-mode)
  ))

(defun ixwem-quit ()
  "Quit IXWEM."
  (interactive)

  (bury-buffer))


(provide 'ixwem)

;;; ixwem.el ends here
