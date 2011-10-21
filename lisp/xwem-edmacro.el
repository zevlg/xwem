;;; xwem-edmacro.el --- Keyboard macro editor for XWEM.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;; Created: Fri Dec 12 11:19:50 MSK 2003
;; Keywords: xwem, xlib
;; Time-stamp: <29/11/2006 23:51:51 lg@h1>

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

;; Macro editing package.  Allow you to edit XWEM keyboard macros like
;; `edmacro' allow you to edit Emacs keyboard macros.  Uses xwem
;; special frame, so you can edit keyboard macro from any point.

;; You might change `xwem-edmacro-can-edit-unbinded' to non-nil to
;; allow edition of non-macro keys.

;;; Code:

(eval-and-compile
  (require 'edmacro))

(require 'xwem-load)

;;; Customization
(defgroup xwem-edmacro nil
  "Group to customize `xwem-edmacro' addon."
  :prefix "xwem-edmacro-"
  :group 'xwem-keyboard)

(defcustom xwem-edmacro-can-edit-unbinded t
  "*Non-nil mean that `xwem-edmacro' allows edit unbinded keys."
  :type 'boolean
  :group 'xwem-edmacro)

(defcustom xwem-edmacro-can-edit-nonmacro nil
  "*Non-nil allow you to edit keys binded to non-macro commands.
USE WITH CAUTION, if this variable is non-nil you can override any
binding in `xwem-global-map'!"
  :type 'boolean
  :group 'xwem-edmacro)

;;; Internal variables

;; Variables
(defvar xwem-edmacro-prefix-arg nil
  "Value of prefix argument.
Internal variable, do not modify.")

(defvar xwem-edmacro-store-place nil
  "Place where to store new keyboard macro after editing.
Internal variable, do not use.")


;; Functions
(defun xwem-edmacro-store (mac)
  "Store new keyboard macro MAC."
  (setq mac (key-sequence-list-description mac))
  (cond ((eq xwem-edmacro-store-place 'xwem-keymacro-macros-stack)
         (if xwem-keymacro-macros-stack
             (setcar xwem-keymacro-macros-stack mac)
           (setq xwem-keymacro-macros-stack (list mac)))
         (xwem-message 'info "New keymacro stored to `%S'"
                       xwem-edmacro-store-place))

        (t
         ;; Redefine key in user macros map
         ;; TODO: Do not use `xwem-global-map', use real keymap where
         ;; key has been bound to. --lg
         (define-key (or (car xwem-edmacro-store-place)
                         xwem-global-map)
           (cdr xwem-edmacro-store-place) mac)))

  (setq xwem-edmacro-store-place nil))

(defun xwem-edmacro-finish (frame)
  "Called when edmacro finishes.
FRAME is special emacs frame where macro editing occurs.
Keep selected buffer to be selected even after FRAME deleted."
  (let ((buf (current-buffer)))
    (if (frame-live-p frame)
        (delete-frame frame t)
      (xwem-special-revert-focus nil))
    (set-buffer buf)))

;;;###autoload(autoload 'xwem-edmacro-edit-kbd-macro "xwem-edmacro" "" t)
(define-xwem-command xwem-edmacro-edit-kbd-macro (xwem-keys &optional arg)
  "Edit XWEM keyboard macro specified by XWEM-KEYS.
With a prefix ARG, format the macro in a more concise way."
  (xwem-interactive
   (list
    (xwem-read-key-sequence
     (substitute-command-keys
      (concat "Enter `\\<xwem-global-map>\\[xwem-keymacro-play-last]' "
              "or one of `\\<xwem-global-map>\\[xwem-user-macros-prefix] XXX': ")))
    (prefix-numeric-value xwem-prefix-arg)))

  (xwem-kbd-stop-grabbing)

  (let ((xwem-cmd (xwem-kbd-get-binding xwem-keys nil t))
        (xwem-keymap (xwem-lookup-map (xwem-cl-selected) xwem-keys))
        xwem-evs frame)

    (setq xwem-evs (cond ((eq xwem-cmd 'xwem-keymacro-play-last)
                          (setq xwem-edmacro-store-place
                                'xwem-keymacro-macros-stack)
                          (or (car xwem-keymacro-macros-stack) []))

                         ((vectorp xwem-cmd)
                          (setq xwem-edmacro-store-place
                                (cons xwem-keymap xwem-keys))
                          xwem-cmd)

                         ((and xwem-edmacro-can-edit-unbinded (null xwem-cmd))
                          (setq xwem-edmacro-store-place
                                (cons xwem-keymap xwem-keys))
                          [])

                         (xwem-edmacro-can-edit-nonmacro
                          (setq xwem-edmacro-store-place
                                (cons xwem-keymap xwem-keys))
                          [])

                         (t nil)))

    (if (null xwem-evs)
        (cond ((and (not xwem-edmacro-can-edit-unbinded) (null xwem-cmd))
               (xwem-message 'warning
                             (concat "Dissalowed to edit unbinded key "
                                     (key-description xwem-keys)
                                     " by `xwem-edmacro-can-edit-unbinded'")))
              ((not xwem-edmacro-can-edit-nonmacro)
               (xwem-message 'warning
                             (concat "Dissalowed to edit non-macro key "
                                     (key-description xwem-keys)
                                     " by `xwem-edmacro-can-edit-nonmacro'.")))
              (t (xwem-message 'warning "Invalid keyboard macro given.")))

      ;; XXX
      (when xwem-edmacro-store-place
        (setq frame (xwem-special-popup-frame
                     (get-buffer-create "*Edit Macro*") t))

        ;; Add some info in *Edit Macro* buffer
        (let ((edmacro-format-hook
               #'(lambda ()
                   (save-excursion
                     (re-search-backward "Macro:")
                     (previous-line 1)
                     (insert (format "\n;; Key: %s\n"
                                     (key-description xwem-keys)))
                     (insert (format ";; Type: %s\n"
                                     (if xwem-cmd
                                         (if (vectorp xwem-cmd)
                                             "binded macro"
                                           "binded command")
                                       "unbinded")))
                     (when (not (vectorp xwem-cmd))
                       (insert (format ";; Cmd: %S\n" xwem-cmd)))))))

          ;; edmacro.el has a bug, it dos not understand self-insert
          ;; in list form, i.e. does not recognize space in [(?a)
          ;; (space) (?b)] as special("SPC") form.  Problem in
          ;; `edmacro-format-keys'.
          ;;
          ;; We adjust xwem-evs to avoid such things.
          (setq xwem-evs
                (mapvector #'(lambda (k)
                               (if (and (listp k)
                                        (= (length k) 1))
                                   (car k)
                                 k))
                           xwem-evs))

          ;; Start edmacro
          (edit-kbd-macro xwem-evs arg
                          `(lambda () (xwem-edmacro-finish ,frame))
                          'xwem-edmacro-store)))
      )))


(provide 'xwem-edmacro)

;;; xwem-edmacro.el ends here
