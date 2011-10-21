;;; xwem-ratanot.el --- Rotate and note xwem addon.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Tue Jun 22 16:58:42 MSD 2004
;; Keywords: xwem
;; Time-stamp: <28/8/2008 06:12:09 lg@h1.lan>

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

(defgroup xwem-ratanot nil
  "Group to customize ratanot."
  :prefix "xwem-ratanot-"
  :group 'xwem)

(defcustom xwem-ratanot-file "rnotes.txt"
  "*File to use for notes."
  :type 'file
  :group 'xwem-ratanot)

(defcustom xwem-ratanot-special-display-strategy 'fill
  "*Strategy to use when poping up special emacs frame."
  :type '(choice (const :tag "Half screen" half)
                 (const :tag "Fill current client" fill)
                 (const :tag "Center" centre))
  :group 'xwem-ratanot)

(defcustom xwem-ratanot-template
  '(date "::  " "[ " (name . 24) " ]"  " (" application ") " "\n\n"
         "   * ")
  "Template to insert on new RataNot entry."
  :type 'list
  :group 'xwem-ratanot)

(defvar xwem-ratanot-minor-mode nil
  "Non-nil if ratanot minor mode is enabled.")
(make-variable-buffer-local 'xwem-ratanot-minor-mode)

(defvar xwem-ratanot-minor-mode-string " RataNot"
  "String to display in modeline, when ratanot minor mode is enabled.")

(defvar xwem-ratanot-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-i <RET>") 'xwem-ratanot-insert-template)
    (define-key map (kbd "C-i d") 'xwem-ratanot-insert-date)
    (define-key map (kbd "C-i n") 'xwem-ratanot-insert-name)
    (define-key map (kbd "C-i c") 'xwem-ratanot-insert-class)
    (define-key map (kbd "C-i s") 'xwem-ratanot-insert-size)
    (define-key map (kbd "C-i m") 'xwem-ratanot-insert-mode)
    (define-key map (kbd "C-i a") 'xwem-ratanot-insert-application)
    (define-key map (kbd "C-i i") 'xwem-ratanot-insert-custom-field)
    map)
  "Keymap used when ratanot minor mode is enabled.")


(defvar xwem-ratanot-last-client nil)
(defun xwem-ratanot-client ()
  "Return client."
  (or xwem-ratanot-last-client
      (xwem-last-client)))

(defvar xwem-ratanot-fields-alist nil
  "Alist of fields that can be inserted by ratanot.")

(defvar xwem-ratanot-last-insertion "name")

(defun xwem-ratanot-insert-custom-field (field limit)
  "Insert FIELD limiting output with LIMIT."
  (interactive (list (completing-read
                      (if xwem-ratanot-last-insertion
                          (format "RataNot field [%s]: "
                                  xwem-ratanot-last-insertion)
                        "RataNot field: ")
                      (mapcar #'(lambda (el)
                                  (list (symbol-name (car el))))
                              xwem-ratanot-fields-alist)
                      nil t nil nil xwem-ratanot-last-insertion)
                     prefix-arg))

  ;; Save last insterted field
  (when (stringp field)
    (setq xwem-ratanot-last-insertion field))

  (let* ((sexp (cdr (assq (or (and (symbolp field) field)
                              (intern field))
                          xwem-ratanot-fields-alist)))
         (rstr (eval sexp)))
    (unless (stringp rstr)
      (error 'xwem-error "Invalid RataNot field definition" field))
    (insert (if (and (numberp limit)
                     (> (length rstr) limit))
                (concat (substring rstr 0 (- limit 2)) "..")
              rstr))))

(defun xwem-ratanot-insert-template (limit)
  "Insert the `xwem-ratanot-template'."
  (interactive "P")

  (mapc #'(lambda (ten)
            (if (stringp ten)
                (insert ten)
              (xwem-ratanot-insert-custom-field
               (or (and (consp ten) (car ten))
                   ten) (and (consp ten) (cdr ten)))))
        xwem-ratanot-template))

;; Define dedicated commands to insert fields
(defmacro define-xwem-ratanot-inserter (name doc sexp)
  "Define new inserter macro for ratanot minor mode."
  (let ((ifun (intern (format "xwem-ratanot-insert-%S" name))))
    `(progn
       (setq xwem-ratanot-fields-alist
             (put-alist ',name ',sexp xwem-ratanot-fields-alist))
       (defun ,ifun (limit)
         ,doc
         (interactive "P")
         (xwem-ratanot-insert-custom-field ',name limit)))))

(define-xwem-ratanot-inserter date
  "Insert current date."
  (current-time-string))

(define-xwem-ratanot-inserter name
  "Insert client's name.
If numeric prefix argument LIMIT is specified, limit output to LIMIT
characters."
  (xwem-client-name (xwem-ratanot-client)))

(define-xwem-ratanot-inserter class
  "Insert client's class."
  (format "%S" (xwem-cl-wm-class (xwem-ratanot-client))))

(define-xwem-ratanot-inserter size
  "Insert client's size."
  (let ((usz (xwem-cl-get-usize (xwem-ratanot-client))))
    (format "%dx%d" (car usz) (cdr usz))))

(define-xwem-ratanot-inserter uptime
  "Insert client's uptime."
  (xwem-cl-get-uptime (xwem-ratanot-client)))

(define-xwem-ratanot-inserter mode
  "Insert client's major mode (managing mode) name."
  (symbol-name (xwem-cl-manage-type (xwem-ratanot-client))))

(define-xwem-ratanot-inserter application
  "Insert client's application name."
  (or (car (xwem-client-application (xwem-ratanot-client)))
      "Unknown"))


;; Minor mode
(defun xwem-ratanot-minor-mode (&optional arg)
  "Enable or disable ratanot minor mode.
If prefix ARG is positive number - enable it.
If prefix ARG is negative number - disable it.
Otherwise toggle."
  (interactive "P")

  (if (or (and (numberp arg)
               (> arg 0))
          (not xwem-ratanot-minor-mode))
      (setq xwem-ratanot-minor-mode t)
    (setq xwem-ratanot-minor-mode nil)))

;;;###autoload(autoload 'xwem-ratanot "xwem-ratanot" nil t)
(define-xwem-command xwem-ratanot ()
  "Make a note."
  (xwem-interactive)

  (let ((xwem-special-default-strategy xwem-ratanot-special-display-strategy)
        (buf (find-file-noselect (expand-file-name xwem-ratanot-file xwem-dir))))
    (declare (special xwem-special-default-strategy))
    (set-symbol-value-in-buffer 'xwem-ratanot-minor-mode t buf)

    (setq xwem-ratanot-last-client (xwem-cl-selected))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert "\n")
      (xwem-ratanot-insert-template nil))
    
    (let* ((sf (xwem-special-popup-frame buf))
           (scl (xwem-misc-find-cl-by-emacs-frame sf)))
      )
    ))


(provide 'xwem-ratanot)

;;;; On-load actions:

;; Register ratanot minor mode
(add-minor-mode 'xwem-ratanot-minor-mode
                'xwem-ratanot-minor-mode-string
                xwem-ratanot-keymap)

;;; xwem-ratanot.el ends here
