;;; xwem-modes.el --- Minor modes support for XWEM.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Mon Oct 18 22:36:23 MSD 2004
;; Keywords: xwem
;; Time-stamp: <29/11/2006 23:55:04 lg@h1>

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

;;;###xwem-autoload
(defvar xwem-minor-mode-alist nil
  "Alist saying how to show minor modes.
Each element is a list which looks like (VAR STRING).
STRING is shown when VAR is non-nil.")

;;;###xwem-autoload
(defvar xwem-minor-mode-map-alist nil
  "Alist of keymaps for use of minor modes.
Each element looks like (VAR . KEYMAP).")

;;;###xwem-autoload(autoload 'xwem-add-minor-mode "xwem-modes")
(defun xwem-add-minor-mode (toggle name &optional keymap)
  "Add a minor mode to `xwem-minor-mode-alist'.
For TOGGLE, NAME, KEYMAP, usage take a look at `add-minor-mode'."
  (setq xwem-minor-mode-alist
        (put-alist toggle (list name) xwem-minor-mode-alist))

  (when keymap
    (setq xwem-minor-mode-map-alist
          (put-alist toggle keymap xwem-minor-mode-map-alist))))

;;;###xwem-autoload
(defun xwem-turn-on-minor-mode (cl mm-toggle)
  "On CL, turn on minor mode MM-TOGGLE."
  (unless (if (xwem-client-local-variable-p mm-toggle)
              (xwem-client-local-variable-value cl mm-toggle)
            (symbol-value mm-toggle))
    (let* ((km-sym (cdr (assq mm-toggle xwem-minor-mode-map-alist)))
           (kmap (if (xwem-client-local-variable-p km-sym)
                     (xwem-client-local-variable-value cl km-sym)
                   (symbol-value km-sym))))
      (when (keymapp kmap)
        (xwem-kbd-install-grab kmap (xwem-cl-xwin cl))))

    (if (xwem-client-local-variable-p mm-toggle)
        (xwem-client-local-variable-set cl mm-toggle t)
      (set-variable mm-toggle t))))

;;;###xwem-autoload
(defun xwem-turn-off-minor-mode (cl mm-toggle)
  "On CL, turn off minor mode MM-TOGGLE."
  (when (if (xwem-client-local-variable-p mm-toggle)
            (xwem-client-local-variable-value cl mm-toggle)
          (symbol-value mm-toggle))
    (let* ((km-sym (cdr (assq mm-toggle xwem-minor-mode-map-alist)))
           (kmap (if (xwem-client-local-variable-p km-sym)
                     (xwem-client-local-variable-value cl km-sym)
                   (symbol-value km-sym))))
      (when (keymapp kmap)
        (xwem-kbd-uninstall-grab kmap (xwem-cl-xwin cl))))
    (if (xwem-client-local-variable-p mm-toggle)
        (xwem-client-local-variable-set cl mm-toggle nil)
      (set-variable mm-toggle nil))))


(provide 'xwem-modes)

;;; xwem-modes.el ends here
