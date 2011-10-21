;;; xwem-register.el --- Registers support for XWEM.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Feb  6 08:04:24 MSK 2004
;; Keywords: xwem
;; Time-stamp: <17/6/2011 14:24:15 lg@localhost>

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

;; Just like `register' packege for Emacs, but for XWEM.
;;
;; To start using it add:
;;
;;    (require 'xwem-register)
;;    (xwem-register-install-bindings)
;;
;; to your xwemrc.  That will add bonus bindings to `xwem-global-map',
;; such as `H-x 6' to store current window configuration to register,
;; `H-x /' to store current client to register and `H-x j' to jump to
;; register, i.e. set saved window config or pop to saved client, and
;; others.

;; Idea about automatic registers belongs to Steve Youngs
;; <steve@youngs.au.com>.

;;; Code:

(require 'xwem-load)
(require 'xwem-help)


;;; Customisation, note: uses `xwem-misc' group
(defgroup xwem-registers nil
  "Group to customize xwem registers behaviour."
  :prefix "xwem-registers-"
  :group 'xwem-misc)

(defcustom xwem-registers-frame-config-no-delete t
  "This valued passed as NO-DELETE argument to `xwem-set-frame-configuration'."
  :type 'boolean
  :group 'xwem-registers)

(defcustom xwem-registers-win-config-select-frame t
  "*Non-nil mean, when jumping to window configuration also select
frame for which config was generated.  Directly passed as
SELECT-FRAME-P to `xwem-set-window-configuration'."
  :type 'boolean
  :group 'xwem-registers)

(defcustom xwem-registers-auto-registers
  '((?x (application "xemacs")))
  "*List of automatic registers.

Sample configuration:

  (setq xwem-registers-auto-registers
    '((?x (application \"xemacs\"))
      (?o (application \"opera\"))
      (?m (application \"mozilla\"))))
"
  :type '(repeat (cons (character :tag "Register")
                       (sexp :tag "Match expression")))
  :group 'xwem-registers)

(defcustom xwem-registers-auto-override nil
  "*Non-nil mean when autoregister matches and such register already
exists it would override it with new value."
  :type 'boolean
  :group 'xwem-registers)

;;; Internal variables

(defvar xwem-registers nil
  "XWEM registers alist.
Each element in form (NAME . VALUE), one for each register.
NAME is a character. VALUE is a string, number, client or a list.
A list in form (XWEM-WIN-CONFIG CONFIG) represent a window
configuration.")

(defun xwem-register-set (register value)
  "Set contents of XWEM register named REGISTER to VALUE.
Return VALUE, see documentation for `xwem-registers' for possible VALUE."
  (setq xwem-registers
        (put-alist register value xwem-registers)))

(defun xwem-register-get (reg)
  "Return contents of XWEM register named REG, or nil if none."
  (cdr (assq reg xwem-registers)))

(defun xwem-register-del (reg)
  "Delete REG from registers list."
  (setq xwem-registers (remassq reg xwem-registers)))

(defun xwem-register-del-by-value (type value)
  "Remove all register of TYPE which has VALUE."
  (mapc #'(lambda (r)
            (when (and (eq (car (cdr r)) type)
                       (eq (cadr (cdr r)) value))
              (xwem-register-del (car r))))
        xwem-registers))

;;;###autoload(autoload 'xwem-register-client "xwem-register" "" t)
(define-xwem-command xwem-register-client (register)
  "Store selected client to REGISTER."
  (xwem-interactive "kClient to register: ")
  (xwem-client-set-property (xwem-cl-selected) 'register (event-key register)))

;;;###autoload(autoload 'xwem-register-win-config "xwem-register" "" t)
(define-xwem-command xwem-register-win-config (register)
  "Store window configuration in REGISTER."
  (xwem-interactive "kWindow Configuration to register: ")

  (let ((reg (event-key register)))
    (xwem-register-set reg (list 'XWEM-WIN-CONFIG
                                 (xwem-window-configuration)))))

;;;###autoload(autoload 'xwem-register-frame-config "xwem-register" "" t)
(define-xwem-command xwem-register-frame-config (register)
  "Store frame configuration to REGISTER."
  (xwem-interactive "kFrame Configuration to register: ")

  (let ((reg (event-key register)))
    (xwem-register-set reg (list 'XWEM-FRAME-CONFIG
                                 (xwem-frame-configuration)))))

;;;###autoload(autoload 'xwem-register-client-config "xwem-register" "" t)
(define-xwem-command xwem-register-client-config (register)
  "Store clients configuration to REGISTER."
  (xwem-interactive "kClients Configuration to register: ")

  (let ((reg (event-key register)))
    (xwem-register-set reg (list 'XWEM-CLIENT-CONFIG
                                 (xwem-client-configuration)))))

;;;###autoload(autoload 'xwem-register-jump "xwem-register" "" t)
(define-xwem-command xwem-register-jump (register &optional arg)
  "Jump to REGISTER.
If prefix ARG is supplied remove REGISTER from `xwem-registers' alist."
  (xwem-interactive "kJump to register: \nP")

  (let ((reg (event-key register))
        rval)
    (if arg
        (xwem-register-del reg)

      ;; Jump to REGISTER value
      (setq rval (xwem-register-get reg))
      (cond ((and (listp rval)
                  (eq 'XWEM-CLIENT (car rval))
                  (xwem-cl-p (cadr rval)))
             (xwem-cl-pop-to-client (cadr rval)))

            ((and (listp rval) (eq 'XWEM-WIN-CONFIG (car rval)))
             (xwem-set-window-configuration
              (cadr rval) xwem-registers-win-config-select-frame))

            ((and (listp rval) (eq 'XWEM-FRAME-CONFIG (car rval)))
             (xwem-set-frame-configuration
              (cadr rval) xwem-registers-frame-config-no-delete))

            ((and (listp rval) (eq 'XWEM-CLIENT-CONFIG (car rval)))
             (xwem-set-client-configuration (cadr rval)))

            (t (xwem-message 'todo "Handle register value: %S" rval)))
      )))

(defun xwem-registers-list-switch ()
  "Jump to register while listing registers with `xwem-registers-list'."
  (interactive)
  (help-mode-quit)
  (xwem-register-jump
   (first (last (append (this-command-keys) nil)))))

;;;###xwem-autoload(autoload 'xwem-registers-help "xwem-registers" nil "Show info about registers.")
(define-xwem-command xwem-registers-list ()
  "Show info about registers."
  (xwem-interactive)

  (xwem-help-display "registers"
    ;; Create title
    (add-text-properties (point) (progn (insert "Register") (point))
                         '(face underline))
    (insert "    ")
    (add-text-properties (point) (progn (insert "Type") (point))
                         '(face underline))
    (insert "           ")
    (add-text-properties (point) (progn (insert "Value") (point))
                         '(face underline))
    (insert "\n\n")

    ;; Insert registers information
    (mapc #'(lambda (r)
              (insert (format "  '%c'  " (car r)))
              (let ((rval (cdr r)))
                (cond ((and (listp rval) (eq 'XWEM-CLIENT (car rval)))
                       (insert "    CLIENT  ")
                       (let ((cl (cadr rval)))
                         (insert (format "    %s/%s"
                                         (or (car (xwem-client-application cl))
                                             "unknown")
                                         (xwem-client-name cl)))))
                      ((and (listp rval) (eq 'XWEM-WIN-CONFIG (car rval)))
                       (insert "  WIN-CONFIG")
                       (let ((frame (xwem-win-config-frame (cadr rval))))
                         (if (xwem-frame-alive-p frame)
                             (insert (format "    alive/ [%d] %s"
                                             (xwem-frame-num frame)
                                             (xwem-frame-name frame)))
                           (insert (format "    dead")))))
                      ((and (listp rval) (eq 'XWEM-FRAME-CONFIG (car rval)))
                       (insert " FRAME-CONFIG"))
                      ((and (listp rval) (eq 'XWEM-CLIENT-CONFIG (car rval)))
                       (insert (format " CLIENT-CONFIG  %d clients/ %d alive"
                                       (length (cadr rval))
                                       (length (delete* nil (mapcar #'(lambda (conf)
                                                                        (memq t (mapcar #'(lambda (cl)
                                                                                            (xwem-client-config-match conf cl))
                                                                                        xwem-clients)))
                                                                    (cadr rval)))))))))
              (insert "\n"))
          xwem-registers))

  (let ((map (copy-keymap help-mode-map)))
    (mapc #'(lambda (r)
              (define-key map (vector (car r)) 'xwem-registers-list-switch))
          xwem-registers)
    (use-local-map map)))

(defun xwem-registers-remove-client (cl)
  "CL is dead, so remove it frome registers."
  (xwem-register-del-by-value 'XWEM-CLIENT cl)
  (xwem-client-set-property cl 'register nil))

(defun xwem-registers-auto-register (cl)
  "Put CL to register according to "
  (let ((r (car (xwem-manda-find-match-1
                 cl xwem-registers-auto-registers 'cadr))))
    (when (and r (or xwem-registers-auto-override
                     (not (xwem-register-get r))))
      (xwem-client-set-property cl 'register r))))

;;; Register as client property
(defun xwem-client-set-register (cl rprop reg)
  "Set CL's register property RPROP to REG."
  ;; Remove REG from any other clients
  (mapc #'(lambda (ocl)
            (when (eq reg (xwem-cl-get-prop ocl rprop))
              (xwem-cl-rem-prop ocl rprop)))
        (xwem-clients-list))

  ;; Remove register REG from registers list as well
  (xwem-register-del reg)

  ;; Save it in CL's plist
  (xwem-cl-put-prop cl rprop reg)

  ;; And finally register REG
  (when reg
    (xwem-register-set reg (list 'XWEM-CLIENT cl))))

(define-xwem-client-property register nil
  "CL's register."
  :type 'char
  :set 'xwem-client-set-register)


(provide 'xwem-register)

;;; On-load actions
(add-hook 'xwem-cl-create-hook 'xwem-registers-auto-register)
(add-hook 'xwem-cl-destroy-hook 'xwem-registers-remove-client)

;;; xwem-register.el ends here
