;;; xwem-desktop.el --- Support for xwem sessions.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;; Created: Wed Jul 14 10:16:20 MSD 2004
;; Keywords: xwem, desktop
;; Time-stamp: <29/11/2006 23:51:38 lg@h1>

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

;; To start using, add something like:

;;    (add-hook 'xwem-exit-hook 'xwem-desktop-save)
;;    (xwem-desktop-load)

;; to your xwemrc.

;;; TODO:

;;   * Save registers betwean sessions, maybe by using expectances.

;;; Code:

(require 'xwem-load)
(require 'xwem-frame)

;;; Customisation
(defgroup xwem-desktop nil
  "Group to customize xwem desktop."
  :prefix "xwem-desktop-"
  :group 'xwem)

(defcustom xwem-desktop-file (expand-file-name ".desktop" xwem-dir)
  "File used to store xwem desktop information."
  :type 'file
  :group 'xwem)

(defcustom xwem-desktop-goals
  '((keymap . xwem-user-macros-prefix)
    (xwem-read-filename-history . 100)
    (xwem-launcher-history . 100)
    (xwem-read-expression-history . 100)
    (xwem-open-file-commands-alist . 1024)

    frames-config
    clients-config)
  "*List of variables to save.
Each element is eather symbol or cons cell in form.
\(symbol . maxsize\)."
  :type '(repeat
          (choice (const :tag "Frames configuration" frames-config)
                  (cons :tag "Keymap goal"
                        (const :tag "Keymap" keymap)
                        (symbol :tag "Keymap prefix"))
                  (cons :tag "History"
                        (choice (const :tag "Launcher history"
                                       xwem-launcher-history)
                                (const :tag "Expression history"
                                       xwem-read-expression-history)
                                (symbol :tag "Custom history"))
                        (number :tag "Max Size"))))
  :group 'xwem-desktop)

(defcustom xwem-desktop-onetime-goals
  '((frames-config . ".desktop.frames"))
  "One time goals to save."
  :type '(repeat (choice (const :tag "Frames config" frames-config)
                         (cons :tag "Variable"
                               (symbol :tag "Custom Symbol")
                               (number :tag "Max Size"))))
  :group 'xwem-desktop)

;;; Internal variables

(defun xwem-desktop-save-element (el &optional buffer)
  "Save element EL.
EL is one of that occurs in `xwem-desktop-goals'."
  (unless buffer
    (setq buffer (current-buffer)))
  (cond ((eq el 'frames-config)
         ;; Store frames configuration here
         (xwem-frame-config-dump1 (xwem-frame-configuration) buffer t))

        ((eq el 'clients-config)
         ;; Store clients configuration
         (princ (format "(add-hook 'xwem-after-init-wins-hook (lambda () (xwem-set-client-configuration (quote %S))))"
                        (xwem-client-configuration)) buffer))

        ((symbolp el)
         (princ "\n;; Symbol value\n" buffer)
         (princ (concat "(setq " (symbol-name el) " "
                        (if (listp (symbol-value el))
                            (concat "(quote "
                                    (prin1-to-string (symbol-value el)) ")")
                          (prin1-to-string (symbol-value el)))
                        ")\n")
                buffer))

        ((and (consp el) (numberp (cdr el)))
         (princ "\n;; List\n" buffer)
         (let ((result nil)
               (clist (symbol-value (car el)))
               (limit (cdr el)))
           (while (and clist (> limit 0))
             (unless (member (car clist) result)
               (setq result (cons (car clist) result))
               (decf limit))
             (setq clist (cdr clist)))
           (setq result (nreverse result))
           (princ (concat "(setq " (symbol-name (car el)) " "
                          "(quote " (prin1-to-string result) ")"
                          ")\n")
                  buffer)))

        ((and (consp el) (eq (car el) 'keymap))
         (let* ((kmap (xwem-kbd-fixup-keymap (cdr el)))
                (kmap-name (keymap-name kmap)))
           (princ (format "\n;; Keymap (%s)\n" kmap-name) buffer)
           (map-keymap #'(lambda (kseq fbind)
                           (princ (concat "(define-key (quote "
                                          (prin1-to-string kmap-name) ") "
                                          "(quote " (prin1-to-string kseq) ") "
                                          "(quote " (prin1-to-string fbind) ")"
                                          ")\n")
                                  buffer))
                       kmap)))

        (t (xwem-message 'warning "Strange el: `%S', skiping .." el))))

;;;###autoload(autoload 'xwem-desktop-save "xwem-desktop" nil t)
(define-xwem-command xwem-desktop-save (&optional file)
  "Save things described in `xwem-desktop-goals' into FILE.
If FILE is ommited, `xwem-desktop-file' is used."
  (xwem-interactive "FSave xwem desktop to: ")
  (unless file (setq file xwem-desktop-file))
  (with-temp-buffer
    (erase-buffer)
    (insert
     (format ";;; %s --- Desktop configuration for XWEM.\n"
             (file-name-nondirectory file))
     "\n;; NOTE: This file is automatically generated by xwem-desktop\n\n")

    ;; Set print-XX to nil to make full printing of objects
    (let ((print-length nil)
          (print-level nil))
      (mapc 'xwem-desktop-save-element xwem-desktop-goals))

    (insert (format "\n;;; %s ends here\n" (file-name-nondirectory file)))
    (write-region (point-min) (point-max) file)))

;;;###autoload(autoload 'xwem-desktop-load "xwem-desktop" nil t)
(define-xwem-command xwem-desktop-load (&optional file)
  "Load saved desktop from FILE.
If FILE is ommited, `xwem-desktop-file' is used."
  (xwem-interactive "FLoad xwem desktop from: ")
  (unless file (setq file xwem-desktop-file))
  (when (file-exists-p file)
    (load-file file)))

;;;###autoload(autoload 'xwem-desktop-save-onetime "xwem-desktop" nil t)
(define-xwem-command xwem-desktop-save-onetime ()
  "Save onetime goals if necessary."
  (xwem-interactive)
  (mapc #'(lambda (gg)
            (let ((file (expand-file-name (cdr gg) xwem-dir)))
              (unless (file-exists-p file)
                (let ((xwem-desktop-goals (list (car gg))))
                  (xwem-desktop-save file)))))
        xwem-desktop-onetime-goals))

;;;###autoload(autoload 'xwem-desktop-load-onetime "xwem-desktop" nil t)
(define-xwem-command xwem-desktop-load-onetime ()
  "Load onetime goals."
  (xwem-interactive)
  (mapc #'(lambda (gg)
            (let ((file (expand-file-name (cdr gg) xwem-dir)))
              (when (file-exists-p file)
                (load-file file))))
        xwem-desktop-onetime-goals))


(provide 'xwem-desktop)

;;; xwem-desktop.el ends here
