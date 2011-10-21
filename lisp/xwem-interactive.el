;;; xwem-interactive.el --- XWEM interactive interface.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;; Created: Thu Dec 18 05:49:52 MSK 2003
;; Keywords: xwem, xlib
;; Time-stamp: <1/12/2008 23:18:24 lg@h1>

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

(eval-when-compile
  ;; Shutup compiler
  (defvar iswitchb-buflist nil)
  (autoload 'iswitchb-read-buffer "iswitchb")
  )

(require 'xlib-xlib)

(require 'xwem-struct)
(require 'xwem-loaddefs)


(defcustom xwem-completing-read-type 'iswitchb
  "*Type of interactive client reading.
Possible values are `iswitchb', requires iswitchb package, or
`complete' uses standard `completing-read'."
  :type '(choice (const :tag "Iswitchb" iswitchb)
                 (const :tag "Standard" complete))
  :group 'xwem-misc)

;;; Internal variables

(defvar xwem-interactively nil
  "Non-nil when xwem in interactive mode.
Internal variabel, do not modify.")

;; Save read-from-minibuffer for further use
(eval-and-compile
  (fset 'read-from-minibuffer-for-xwem
        (symbol-function 'read-from-minibuffer)))

(defmacro xwem-interactive-1 (allow-events-mode &rest ispec)
  "Same as `xwem-interactive', but allows you control `XAllowEvents'.
If ALLOW-EVENTS-MODE is nil then no `XAllowEvents' issued, otherwise
call `XAllowEvents' with mode set to ALLOW-EVENTS-MODE."
  (let ((is (cond ((and (= (length ispec) 1)
                        (stringp (car ispec)))
                   (setq ispec (car ispec))
                   (split-string ispec "\n"))

                  (t ispec))))

    (if (not (stringp ispec))
        `(interactive (let ((xwem-interactively t))
                        (when ,allow-events-mode
                          (XAllowEvents (xwem-dpy) ,allow-events-mode))
                        (prog1 (progn ,@ispec)
                          (setq xwem-prefix-arg nil))))

      `(interactive (prog1 (progn
                             (when ,allow-events-mode
                               (XAllowEvents (xwem-dpy) ,allow-events-mode))
                             (xwem-interactive-ilist (quote ,is)))
                      (setq xwem-prefix-arg nil))))
    ))
  
(defmacro xwem-interactive (&rest ispec)
  "Just like `interactive', but accepts xwem specific arguments.
Code letters available are:
  s -- String.
  k -- Single key.
  K -- Key sequence that executes command.
  c -- Client.
  f -- Existing file.
  F -- Possible non-existing file.
  p -- Prefix argument as number.
  P -- Prefix argument in raw form.
  C -- Command.
  e -- External command.
  
  * -- Wait key release
  _ -- Ungrab keyboard, must be first."
  `(xwem-interactive-1 nil ,@ispec))

(defmacro define-xwem-command (funsym args docstring inter &rest body)
  "Same as `xwem-defun', but make FUNSYM to be interactive command.
INTER is actually a for of `xwem-interactive'."
  (list 'progn
        `(put (quote ,funsym) 'xwem-command t)
        (list 'defun funsym args
              (if (stringp docstring)
                  docstring
                (setq body (cons inter body)
                      inter docstring)
                "No docstring.")
              (if (eq (car inter) 'xwem-interactive)
                  (macroexpand inter)
                (error "invalid interactive form: %S" inter))
              ;; Maybe run command without GCing at all
              `(let ((gc-cons-threshold (if xwem-commands-inhibit-gc
                                            xwem-commands-gc-cons-threshold
                                          gc-cons-threshold)))
                 ,@body))))
(put 'define-xwem-command 'lisp-indent-function 'defun)

(defmacro xwem-under-minibuffer (&rest forms)
  "Evaluate FORM under XWEM's minibuffer focus."
  `(if (xwem-cl-selected-p (xwem-minib-cl xwem-minibuffer))
       (progn ,@forms)
     (xwem-client-set-property
      (xwem-minib-cl xwem-minibuffer) 'skip-deselect t)
     (xwem-select-client (xwem-minib-cl xwem-minibuffer))
     (xwem-unwind-protect
         ;; XXX We *MUST NOT* enter debugger here, otherwise
         ;; `xwem-unwind-protect' wont execute uwind forms and xwem
         ;; minibuffer will be selected forever --lg
         (let ((debug-on-error nil)
               (debug-on-signal nil)
               (debug-on-quit nil)
               (debug-on-next-call nil)
               (debug-function-list nil))
           (setq debug-function-list debug-function-list) ; shutup compiler
           (progn ,@forms))
       (xwem-client-set-property
        (xwem-minib-cl xwem-minibuffer) 'skip-deselect nil)
       (xwem-select-last-or-other-client
        (xwem-minib-cl xwem-minibuffer) nil t))))


(defun xwem-command-p (cmd)
  "Return non-nil if CMD is xwem command, defined with `define-xwem-command'."
  (get cmd 'xwem-command))

(defun xwem-interactive-p ()
  "Return non-nil when xwem in interactive mode."
  xwem-interactively)

;; `read-from-minibuffer' variant for use by XWEM.
(defun xwem-read-from-minibuffer (prompt &optional initial-contents keymap
                                         readp history abbrev-table
                                         &rest notused)
  "Read data from xwem minibuffer.
Arguments PROMPT, INITIAL-CONTENTS, KEYMAP, READP, HISTORY and
ABBREV-TABLE are same as for `read-from-minibuffer'."
  (xwem-kbd-stop-grabbing)

  (xwem-under-minibuffer
   (prog1 (let ((special-display-buffer-names
                 (and (boundp 'xwem-special-display-buffer-names)
                      (symbol-value 'xwem-special-display-buffer-names)))
                (xwem-override-map keymap))
            (read-from-minibuffer-for-xwem prompt initial-contents keymap
                                           readp history abbrev-table))
     (xwem-clear-message))))

(defmacro with-xwem-read-from-minibuffer (&rest forms)
  "Execute FORMS using xwem `read-from-minibuffer.'"
  `(let ((saved-read-frome-minibuffer
          (symbol-function 'read-from-minibuffer-for-xwem))
         (ad-redefinition-action 'accept)) ; in case read-from-minibuffer is adviced
     (setq ad-redefinition-action ad-redefinition-action) ; shutup compiler
     (xwem-unwind-protect
         (progn
           (fset 'read-from-minibuffer
                 (symbol-function 'xwem-read-from-minibuffer))
           ,@forms)
       (fset 'read-from-minibuffer saved-read-frome-minibuffer))))

(defun xwem-completing-read (prompt table &optional predicate require-match
                                    initial-contents history)
  "XWEM awared varian of `completing-read'."
  (with-xwem-read-from-minibuffer
   (cond ((eq xwem-completing-read-type 'iswitchb)
          (xwem-misc-completing-read-using-iswitchb
           prompt (mapcar #'car table) predicate require-match))
         ((eq xwem-completing-read-type 'complete)
          (completing-read prompt table predicate require-match
                           initial-contents history))
         (t (error 'xwem-error
                   "Invalid `xwem-completing-read-type'"
                   xwem-completing-read-type)))))

(defun xwem-read-command (prompt)
  "Just like `read-command', but for XWEM.
Argument PROMPT is same as for `read-command'."
  (with-xwem-read-from-minibuffer
   (read-command prompt)))

(defcustom xwem-default-directory-function nil
  "*Non-nil means this function used to determine current directory.
`xwem-default-directory' uses this."
  :type 'function
  :group 'xwem-misc)

(defvar xwem-read-filename-history nil
  "Default history for reading filenames.")

(defun xwem-default-directory ()
  "Return default directory.
If `xwem-default-directory-function' is set, then it used."
  (if (functionp xwem-default-directory-function)
      (or (funcall xwem-default-directory-function)
          (default-directory))
    (default-directory)))

(defun xwem-read-filename (prompt &optional dir default must-match
                                  initial-contents history)
  "Just like `read-file-name', but for XWEM.
PROMPT, DIR, DEFAULT, MUST-MATCH, INITIAL-CONTENTS and HISTORY are
same as for `read-file-name'."
  (with-xwem-read-from-minibuffer
   (let ((use-dialog-box nil))		; block dialogs
     (read-file-name
      prompt (or dir (xwem-default-directory))
      default must-match initial-contents
      (or history 'xwem-read-filename-history)))))

(defun xwem-read-external-command (prompt)
  "Read for external command using PROMPT."
  (xwem-launcher-query prompt))

(defun xwem-read-client (prompt &optional clients initial-client)
  "Read for client name prompting PROMPT and return xwem client.
CLIENTS specifies list of clients to select from, default is list
returned by `xwem-clients-list'.  Selected client is put to the end of
the list.
If INITIAL-CLIENT is given, it is put to the beginning of the clients
list."
  (unless clients
    (setq clients (xwem-clients-list)))

  ;; Put selected client to the end of list
  (when (memq (xwem-cl-selected) clients)
    (setq clients (append (remove* (xwem-cl-selected) clients :test 'eq)
                          (list (xwem-cl-selected)))))

  ;; Put INITIAL-CLIENT to the beginning of the list
  (when (and initial-client
             (memq initial-client clients))
    (setq clients
          (cons initial-client
                (remove* initial-client clients :test 'eq))))

  (let* ((clns (mapcar #'(lambda (cl)
                           (cons (xwem-client-name cl clients) cl))
                       clients))
         (name (xwem-completing-read prompt clns)))
    (cdr (assoc name clns))))

(defun xwem-read-frame (prompt &optional frames)
  "Read for frame prompting PROMPT and return xwem frame.
FRAMES is a list of frames to select from, default is `xwem-frames-list'."
  (unless frames
    (setq frames (xwem-frames-list)))

  (let* ((frms (mapcar #'(lambda (frm)
                           (cons (xwem-frame-name frm) frm))
                       frames))
         (name (xwem-completing-read prompt frms)))
    (cdr (assoc name frms))))

(defun xwem-y-or-n-p (prompt)
  "Ask user a \"y or n\" question."
  (xwem-under-minibuffer (y-or-n-p-minibuf prompt)))

;; For use by `xwem-interactive'
(defun xwem-interactive-ilist (spec)
  "Return list valid for `interactive'.
SPEC is specification of list items."
  (let ((xwem-interactively t))
    (declare (special xwem-interactively))

    ;; ?* mean wait keyrelease
    (when (and spec (eq (aref (car spec) 0) ?*))
      (when (and xwem-last-xevent
                 (= (X-Event-type xwem-last-xevent) X-KeyPress))
        (xwem-kbd-wait-key-release (X-Event-xkey-keycode xwem-last-xevent)))

      ;; Remove ?* from first element in SPEC
      (if (= (length (car spec)) 1)
          (setq spec (cdr spec))
        (setq spec (cons (substring (car spec) 1) (cdr spec)))))

    ;; If ?_ is first than command need to run with ungrabbed
    ;; keyboard.
    (when (and spec (eq (aref (car spec) 0) ?_))
      (xwem-kbd-stop-grabbing)

      ;; Remove ?_ from first element in SPEC
      (if (= (length (car spec)) 1)
          (setq spec (cdr spec))
        (setq spec (cons (substring (car spec) 1) (cdr spec)))))

    (mapcar #'(lambda (el)
                (let ((code (aref el 0))
                      (prompt (substring el 1)))
                  (cond ((eq code ?P) (or xwem-prefix-arg current-prefix-arg))
                        ((eq code ?p) (prefix-numeric-value
                                       (or xwem-prefix-arg current-prefix-arg)))

                        ((eq code ?k) (xwem-read-key prompt))
                        ((eq code ?K) (xwem-read-key-sequence prompt))
                        ((eq code ?f) (xwem-read-filename prompt nil nil t))
                        ((eq code ?F) (xwem-read-filename prompt))
                        ((eq code ?s) (xwem-read-from-minibuffer prompt))
                        ((eq code ?C) (xwem-read-command prompt))
                        ((eq code ?c) (xwem-read-client prompt))
                        ((eq code ?e) (xwem-read-external-command prompt))
                        )))
            spec)))


(provide 'xwem-interactive)

;;; xwem-interactive.el ends here
