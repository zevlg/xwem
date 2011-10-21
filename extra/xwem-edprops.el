;;; xwem-edprops.el --- Interactively edit xwem client's properties.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Oct 27 11:15:39 MSD 2004
;; Keywords: xwem, edit
;; Time-stamp: <23/2/2007 23:00:40 lg@h1>

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

;; Mode to edit xwem client's properties.

;;; Code:
(require 'xwem-load)


;; Various stuff
(defvar xwem-edprops-mode-hook nil
  "*Hooks to call when entering xwem edprops mode.")

(defvar xwem-edprops-allowed-values
  xwem-client-properties-exporting-allowed-values
  "List of allowed types of property value.")

(defvar xwem-edprops-client nil)
(make-variable-buffer-local 'xwem-client-edprops-client)

(defvar xwem-edprops-mode nil
  "Non-nil mean xwem edprops mode is enabled.")
(make-variable-buffer-local 'xwem-edprops-mode)
(set-default 'xwem-edprops-mode nil)

(defvar xwem-edprops-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'xwem-edprops-finish)
    (define-key map "\C-c\C-q" 'xwem-edprops-quit)
    map)
  "Keymap when editing client properties.")

(or (assq 'xwem-edprops-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons (list 'xwem-edprops-mode
                      " XWEM-edprops")
                minor-mode-alist)))

(or (assq 'xwem-edprops-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'xwem-edprops-mode
                      xwem-edprops-map)
                minor-mode-map-alist)))


(defun xwem-edprops-quit (cl)
  "Quit editing properties for CL discarding changes."
  (interactive (list xwem-edprops-client))

  (set-buffer-modified-p nil)
  (kill-buffer (current-buffer)))

(defun xwem-edprops-finish (cl)
  "Finish editing properties for CL, saving changes."
  (interactive (list xwem-edprops-client))

  (set-buffer-modified-p nil)
  (let ((nplist (read (buffer-string)))
        (oplist (xwem-cl-plist cl)))
    (kill-buffer (current-buffer))

    ;; Remove all supported properties that not in NPLIST
    (while oplist
      (when (and (xwem-property-supported-p (car oplist))
                 (not (plist-get nplist (car oplist))))
        (xwem-message 'info "Removing property %S ..\n" (car oplist))
        (xwem-client-set-property cl (car oplist) nil))
      (setq oplist (cddr oplist)))

    (xwem-cl-apply-plist cl nplist)))

(defun xwem-edprops-mode ()
  "Enable xwem-edprops mode in current buffer."
  (setq xwem-edprops-mode t)

  (run-hooks 'xwem-edprops-mode-hook))

;;;###autoload(autoload 'xwem-edit-client-properties "xwem-edprops" "Interactively edit client's properties." t)
(define-xwem-command xwem-edit-client-properties (cl)
  "Interactive edit CL's properties."
  (xwem-interactive (list (xwem-cl-selected)))

  (when (eq cl (xwem-dummy-client))
    (error "XWEM Can't edit properties for dummy client"))

  (with-current-buffer (get-buffer-create " *CL-PROPS*")
    (kill-all-local-variables)
    (setq xwem-edprops-client cl)

    (emacs-lisp-mode)
    (setq xwem-edprops-mode t)          ; enable edprops mode

    (erase-buffer)
    (insert ";; Bindings:\n")
    (insert ";;   ") (where-is 'xwem-edprops-finish t) (insert "\n")
    (insert ";;   ") (where-is 'xwem-edprops-quit t) (insert "\n")

    (insert
     "\n"
     ";; XWEM Client\n\n"
     (format ";;   Manage mode: %s\n"
             (upcase (symbol-name (xwem-cl-manage-type cl))))
     (format ";;   Name: %s\n" (xwem-client-name cl))
     (format ";;   Class: %S\n" (xwem-hints-wm-class (xwem-cl-hints cl)))
     (format ";;   Command: %S\n" (xwem-hints-wm-command (xwem-cl-hints cl)))
     "(\n\n")
    (save-excursion
      (mapc (lambda (kv)
              (when (loop for tt in xwem-edprops-allowed-values
                      if (funcall tt (cdr kv)) return t)
                (insert (format "%S %S\n" (car kv) (cdr kv)))))
            (plist-to-alist (xwem-cl-plist cl)))

      (insert "\n\n;;; Supported properties:\n")
      (let ((print-level 4))            ; Restrict huge output
        (mapc (lambda (sp)
                (unless (memq (car sp) (xwem-cl-plist cl))
                  (insert (format "; %S %S\n" (car sp)
                                  (xwem-client-property cl (car sp))))))
              xwem-supported-client-properties))
      (insert "\n)"))

    ;; Enter editing properties mode
    (xwem-edprops-mode)

    (xwem-special-popup-frame (current-buffer))
    ))


(provide 'xwem-edprops)

;;; xwem-edprops.el ends here
