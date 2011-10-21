;;; xwem-report.el --- Generate a bug report   -*-Emacs-Lisp-*-

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Steve Youngs <steve@xwem.org>
;; Created: 2004-12-05
;; Keywords: xwem
;; Time-stamp: <29/11/2006 23:55:28 lg@h1>

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
;;  Bug reporter.

;;; Code:

(require 'sendmail)
(require 'shadow)

;; To keep the byte-compiler from spewing out warnings.
(eval-when-compile
  (defvar after-sep-pos)
  (defvar final-resting-place)
  (require 'xwem-version)
  (require 'xlib-xlib)
  (require 'xlib-version)
  (require 'font-lock)
  (require 'pp))


;;; Variables

(defcustom xwem-report-bug-send-init nil
  "*If non-nil, include the user's init.el file in the bug report."
  :group 'xwem-misc
  :type 'boolean)

;;; Internal variables

(defconst xwem-report-salutations
  ["Dear bug team:"
   "Ciao bug team:"
   "Salut bug team:"
   "Guten Tag bug team:"
   "To whom it may concern:"
   "Fellow XWEM'ers:"
   "Yo bug team:"
   "G'day bug team:"
   "Greetings Earthlings:"]
  "A list of salutations used for `xwem-report-bug'.")

(defvar xwem-bug-address
  "XWEM Bugs <xwem-bugs@xwem.org>"
  "The address used for submitting bug reports.")

;;; Functions

(defun xwem-report-pre-hook ()
  "Pre hook run by report-submit-bug-report."
  (mail-subject)
  (insert "[XWEM Bug] ")
  (mail-text))

(defun xwem-report-post-hook ()
  "Post hook run by report-submit-bug-report."
  (save-excursion
    (mail-subject)
    (font-lock-fontify-buffer)
    (let ((subj (read-string "Subject header: ")))
      (insert subj))))

;; Stolen from Gnus.
(defun xwem-report-debug ()
  "Go through the Xwem source files and report what variables have been changed.
The source file has to be in the load path."
  (let ((files '("xwem-smartmods.el" "xwem-recover.el" "ixwem.el"
                 "xwem-battery.el" "xwem-time.el" "xwem-weather.el"
                 "xwem-framei.el" "xwem-worklog.el" "xwem-holer.el"
                 "xwem-osd.el" "xwem-tabbing.el" "xwem-selections.el"
                 "xwem-root.el" "xwem-report.el" "xwem-desktop.el"
                 "xwem-faces.el" "xwem-tray.el" "xwem-main.el" "xwem-sound.el"
                 "xwem-strokes.el" "xwem-edmacro.el" "xwem-clgen.el"
                 "xwem-launcher.el" "xwem-frame.el" "xwem-special.el"
                 "xwem-interactive.el" "xwem-transient.el" "xwem-keyboard.el"
                 "xwem-mouse.el" "xwem-misc.el" "xwem-focus.el"
                 "xwem-keymacro.el" "xwem-rooter.el" "xwem-rooticon.el"
                 "xwem-clients.el" "xwem-minibuffer.el" "xwem-register.el"
                 "xwem-clswi.el" "xwem-icons.el" "xwem-manage.el"
                 "xwem-netwm.el" "xwem-win.el" "xwem-theme.el"))
        (print-level 4)                 ; XXX
        (point (point))
        file expr olist sym)
    (message "Please wait while we snoop your variables...")
    (sit-for 0)
    ;; Go through all the files looking for non-default values for variables.
    (save-excursion
      (set-buffer (get-buffer-create " *xwem bug info*"))
      (while files
        (erase-buffer)
        (when (and (setq file (locate-library (pop files)))
                   (file-exists-p file))
          (insert-file-contents file)
          (goto-char (point-min))
          (if (not (re-search-forward "^;;* *Internal variables" nil t))
              (message "Malformed sources in file %s" file)
            (narrow-to-region (point-min) (point))
            (goto-char (point-min))
            (while (setq expr (ignore-errors (read (current-buffer))))
              (ignore-errors
                (and (or (eq (car expr) 'defvar)
                         (eq (car expr) 'defcustom))
                     (stringp (nth 3 expr))
                     (or (not (boundp (nth 1 expr)))
                         (not (equal (eval (nth 2 expr))
                                     (symbol-value (nth 1 expr)))))
                     (push (nth 1 expr) olist)))))))
      (kill-buffer (current-buffer)))
    (when (setq olist (nreverse olist))
      (insert "\n"))
    (while olist
      (when (boundp (car olist))
        (condition-case ()
            (pp `(setq ,(car olist)
                       ,(if (or (consp (setq sym (symbol-value (car olist))))
                                (and (symbolp sym)
                                     (not (or (eq sym nil)
                                              (eq sym t)))))
                            (list 'quote (symbol-value (car olist)))
                          (symbol-value (car olist))))
                (current-buffer))
          (error
           (format "(setq %s 'whatever)\n" (car olist)))))
        ;(insert ";; (makeunbound '" (symbol-name (car olist)) ")\n"))
      (setq olist (cdr olist)))
    ;; Remove any control chars - they seem to cause trouble for some
    ;; mailers.  (Byte-compiled output from the stuff above.)
    (goto-char point)
    (while (re-search-forward "[\000-\010\013-\037\200-\237]" nil t)
      (replace-match (format "\\%03o" (string-to-char (match-string 0)))
                     t t))))

(defun xwem-bug-packages-list ()
  "Insert into the current buffer a list of installed packages."
  (let ((pkgs packages-package-list))
    (while pkgs
      (insert
       (format "(%s ver: %s upstream: %s)\n"
               (nth 0 (car pkgs))
               (nth 2 (car pkgs))
               (nth 4 (car pkgs))))
      (setq pkgs (cdr pkgs)))))

(eval-when-compile
  (autoload 'xwem-dpy "xwem-struct" nil nil 'macro)
  (ignore-errors (require 'xlib-xc)))

(defun xwem-prepare-report ()
  "Grabs the variables, features to include in bug report.
Then put it all into a mail buffer, nicely formatted."
  (mail-to)
  (insert xwem-bug-address)
  (mail-text)
  (forward-line 1)
  (setq after-sep-pos (point))
  (setq final-resting-place (point-marker))
  (insert
   "\n\n"
   "===============================================================\n"
   "System info to help the XWEM boys and girls try to fix your bug:\n"
   "==============================================================="
   "\n\n")
  (insert (format "%s" xwem-version) "\n"
          (format "%s" xlib-version) "\n\n")
  ;; xdpyinfo
  (insert "Output from xdpyinfo:\n--------------------\n\n"
          (shell-command-to-string (concat "xdpyinfo -display "
                                           (X-Dpy-name (xwem-dpy))))
          "\n")
  ;; backtrace & messages buffers
  (let ((lisptrace (get-buffer "*Backtrace*"))
        (ctrace (get-buffer "*gdb-xemacs*"))
        (debug (get-buffer "*xwem-debug*"))
        (msgs (get-buffer " *xwem-messages*")))
    (when lisptrace
      (insert "Lisp Backtrace:\n--------------\n\n")
      (insert-buffer-substring lisptrace)
      (insert "\n\n"))
    (when ctrace
      (insert "C Backtrace:\n-----------\n\n")
      (insert-buffer-substring ctrace)
      (insert "\n\n"))
    (when debug
      (insert "xwem-debug buffer:\n-----------------\n\n")
      (insert-buffer-substring debug)
      (insert "\n\n"))
    (when msgs
      (insert "xwem-messages buffer:\n--------------------\n\n")
      (insert-buffer-substring msgs)
      (insert "\n\n")))
  ;; Insert all the XWEM vars that have been changed from default.
  ;; The actual work for this is done in `xwem-bug-debug', but it
  ;; needs to be called toward the end of this function.
  (insert "\n\nXWEM variables of note:\n----------------------\n")
  (when window-setup-hook
    (insert "\n\nwindow-setup-hook:")
    (cl-prettyprint (symbol-value 'window-setup-hook)))
  (when kill-emacs-hook
    (insert "\n\nkill-emacs-hook:")
    (cl-prettyprint (symbol-value 'kill-emacs-hook)))
  ;; Insert the output of 'describe-installation'.
  (insert "\n\n"
          (symbol-value 'Installation-string))
  ;; Load-path shadows can cause some grief.
  (flet ((append-message
           (&rest args) ())
         (clear-message
           (&optional label frame stdout-p no-restore)
           ()))
    (insert "\n\nLoad-Path Lisp Shadows:\n"
            "----------------------\n")
    (let ((before-shadows (point)))
      (insert
        (format "%s"
                (find-emacs-lisp-shadows load-path)))
      (save-restriction
        (narrow-to-region before-shadows (point))
        (fill-paragraph t)
        (insert "\n"))))
  ;; Insert a list of installed packages.
  (insert "\n\nInstalled XEmacs Packages:\n"
          "-------------------------\n")
  (xwem-bug-packages-list)
  (insert "\n")
  ;; Insert a list of installed modules.
  (if (fboundp 'list-modules)
      (progn
        (insert "\n\nInstalled Modules:\n"
                "-----------------\n")
        (let* ((mods (list-modules)))
          (while mods
            (cl-prettyprint (cdr (car mods)))
            (setq mods (cdr mods))))))
  ;; Insert a list of loaded features
  (let ((before-features (point)))
    (insert
     (format "\n\nFeatures:\n--------\n\n%s" (symbol-value 'features)))
    (save-restriction
      (narrow-to-region before-features (point))
      (fill-paragraph t)
      (insert "\n\n")))
  ;; Insert the contents of the user's init file if it exists
  ;; and the user wants it sent.
  (if xwem-report-bug-send-init
      (if (file-readable-p user-init-file)
          (save-excursion
            (goto-char (point-max))
            (forward-line -3)
            (beginning-of-line)
            (insert "\n\nUser Init File:\n--------------\n\n")
            (insert-file-contents user-init-file))))
  (xwem-report-pre-hook)
  (xwem-report-post-hook)
  (mail-text)
  (insert
   (aref xwem-report-salutations
         (% (+ (% (random) 1000) 1000)
            (length xwem-report-salutations))) "\n")
  (re-search-forward "XWEM variables of note:" nil t)
  (forward-line 2)
  (xwem-report-debug)
  (goto-char final-resting-place)
  (forward-line 2)
  (set-marker final-resting-place nil)
  (message "Please enter your report.  Type C-c C-c to send, C-x k to abort."))

;;;###autoload
(defun xwem-report-bug (&optional no-confirm)
  "Submit a bug report for XWEM.
Optional argument BLURB is a string that adds a preamble to the bug report.
Optional argument NO-CONFIRM if 't' will not ask for confirmation."
  (interactive)
  (if (or no-confirm
          (y-or-n-p "Do you want to submit a bug report on XWEM? "))
      (progn
        (mail)
        (xwem-prepare-report))))

(provide 'xwem-report)

;;; xwem-report.el ends here
