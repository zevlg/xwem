;;; xwem-keytt.el --- Keyboard translate table minor mode.

;; Copyright (C) 2005 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Feb 16 02:15:32 MSK 2005
;; Keywords: xwem, mode

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

;; Add this:

;;    (xwem-keytt-global-mode)

;; to your ~/.xwem/xwemrc.el to enable keytt minor mode at manage time
;; for clients that have support.

;;; Code:


(require 'xwem-modes)

(defcustom xwem-keytt-synth-p nil
  "*Non-nil mean use synthetic X events."
  :type 'boolean)

(defvar xwem-keytt-minor-mode nil
  "*Non-nil mean keytt minor mode is enabled.")
(xwem-make-variable-client-local 'xwem-keytt-minor-mode)

(defvar xwem-keytt-keymap nil
  "Keymap for keytt minor mode.")
(xwem-make-variable-client-local 'xwem-keytt-keymap)

(defvar xwem-keytt-translation-keymaps nil
  "Alist of keytt keymaps.
Form of entries is - \(QUALIFIER . \(KEYMAP . SYNTH-P\)\).")

(defun xwem-keytt-add-keymap (qualifier keymap &optional use-synth-p)
  "Add new keymap for a QUALIFIER clients.
If USE-SYNTH-P is non-nil, then set `xwem-keyboard-use-synth-events'
to non-nil for clients that matches QUALIFIER."
  (push (cons qualifier (cons keymap use-synth-p))
        xwem-keytt-translation-keymaps))

(defun xwem-keytt-find-keymap (client)
  "Find an appropriate keymap to use with CLIENT."
  (let ((ktk xwem-keytt-translation-keymaps))
    (while (and ktk (not (xwem-cl-match-p client (caar ktk))))
      (setq ktk (cdr ktk)))
    (cdr (car ktk))))

;;;###autoload(autoload 'xwem-turn-on-keytt "xwem-keytt" nil t)
(define-xwem-command xwem-turn-on-keytt (&optional client)
  "Enable keytt minor mode."
  (xwem-interactive (list (xwem-cl-selected)))

  ;; Establish a grab
  (let ((kmap (xwem-keytt-find-keymap client)))
    (when kmap
      (xwem-client-local-variable-set client 'xwem-keytt-keymap (car kmap))
      (xwem-client-local-variable-set client 'xwem-keyboard-use-synth-events (cdr kmap))
      (xwem-turn-on-minor-mode client 'xwem-keytt-minor-mode))))

;;;###autoload(autoload 'xwem-turn-off-keytt "xwem-keytt" nil t)
(define-xwem-command xwem-turn-off-keytt (&optional client)
  "Disable keytt minor mode."
  (xwem-interactive (list (xwem-cl-selected)))

  (xwem-turn-off-minor-mode client 'xwem-keytt-minor-mode)
  (xwem-client-local-variable-set client 'xwem-keytt-keymap nil))

;;;###autoload(autoload 'xwem-keytt-minor-mode "xwem-keytt" nil t)
(define-xwem-command xwem-keytt-minor-mode (arg &optional client)
  "Toggle keytt minor mode.
If ARG is positive - turn it on.
If ARG is negative - turn it off."
  (xwem-interactive (list xwem-prefix-arg (xwem-cl-selected)))

  (if (not (numberp arg))
      (if (xwem-client-local-variable-value client 'xwem-keytt-minor-mode)
          (xwem-turn-off-keytt client)
        (xwem-turn-on-keytt client))
    (if (> arg 0)
        (xwem-turn-on-keytt client)
      (xwem-turn-off-keytt client))))

;;;###autoload
(defun xwem-keytt-global-mode ()
  "Enable global mode.
For each client that have support for keytt minor mode,
xwem-keytt-minor-mode will be enabled at manage time."
  (add-hook 'xwem-cl-manage-hook 'xwem-turn-on-keytt))


;;;; Add some predefined keymaps:

(defmacro xwem-keytt-define-universal-argument-commands (keymap)
  "Define command universal prefix argument commands for KEYMAP."
  `(progn
     (define-key ,keymap (kbd "C-u") 'xwem-universal-argument)
     (define-key ,keymap (kbd "C-0") 'xwem-universal-digit)
     (define-key ,keymap (kbd "C-1") 'xwem-universal-digit)
     (define-key ,keymap (kbd "C-2") 'xwem-universal-digit)
     (define-key ,keymap (kbd "C-3") 'xwem-universal-digit)
     (define-key ,keymap (kbd "C-4") 'xwem-universal-digit)
     (define-key ,keymap (kbd "C-5") 'xwem-universal-digit)
     (define-key ,keymap (kbd "C-6") 'xwem-universal-digit)
     (define-key ,keymap (kbd "C-7") 'xwem-universal-digit)
     (define-key ,keymap (kbd "C-8") 'xwem-universal-digit)
     (define-key ,keymap (kbd "C-9") 'xwem-universal-digit)))


;; GV
(defvar xwem-keytt-gv-keymap (make-sparse-keymap 'xwem-keytt-gv-keymap)
  "Keytt keymap for GV commands.")
(xwem-keytt-add-keymap '(and (application "gv")
                             (not (predicate xwem-cl-transient-for-p)))
                       xwem-keytt-gv-keymap t)

;; Universal argument, so `C-5 C-n' for exampl will emulate five `C-n'
(xwem-keytt-define-universal-argument-commands xwem-keytt-gv-keymap)
(define-key xwem-keytt-gv-keymap (kbd "C-x C-f") (kbd "<self-insert> o")) ; open
(define-key xwem-keytt-gv-keymap (kbd "C-x C-c") (kbd "<self-insert> q")) ; exit
(define-key xwem-keytt-gv-keymap (kbd "C-x C-w") (kbd "<self-insert> s")) ; save marked
(define-key xwem-keytt-gv-keymap (kbd "C-x C-s") (kbd "<self-insert> S")) ; save
(define-key xwem-keytt-gv-keymap (kbd "M-<") (kbd "<self-insert> <home>")) ; goto first page
(define-key xwem-keytt-gv-keymap (kbd "M->") (kbd "<self-insert> <end>")) ; goto last page
(define-key xwem-keytt-gv-keymap (kbd "C-l") (kbd "<self-insert> V")) ; center page
(define-key xwem-keytt-gv-keymap (kbd "C-n") (kbd "<self-insert> Sh-<down>")) ; scroll down
(define-key xwem-keytt-gv-keymap (kbd "C-p") (kbd "<self-insert> Sh-<up>")) ; scroll up
(define-key xwem-keytt-gv-keymap (kbd "C-v") (kbd "<self-insert> <next>"))
(define-key xwem-keytt-gv-keymap (kbd "M-v") (kbd "<self-insert> <prior>"))
(define-key xwem-keytt-gv-keymap (kbd "C-<SPC>") (kbd "<self-insert> M")) ; mark page
(define-key xwem-keytt-gv-keymap (kbd "C-g") (kbd "<self-insert> N")) ; unmark page


;; FIREFOX and MOZILLA
(defvar xwem-keytt-firefox-keymap (make-sparse-keymap 'xwem-keytt-firefox-keymap)
  "Keytt keymap for firefox commands.")
(xwem-keytt-add-keymap '(and (or (application "firefox")
                                 (application "mozilla"))
                             (not (predicate xwem-cl-transient-for-p)))
                       xwem-keytt-firefox-keymap t)

(xwem-keytt-define-universal-argument-commands xwem-keytt-firefox-keymap)
(define-key xwem-keytt-firefox-keymap (kbd "C-x 5 2") (kbd "<self-insert> C-n")) ; new window
(define-key xwem-keytt-firefox-keymap (kbd "C-x 5 0") (kbd "<self-insert> C-W")) ; Close window
(define-key xwem-keytt-firefox-keymap (kbd "C-x C-c") (kbd "<self-insert> M-f q")) ; quit
(define-key xwem-keytt-firefox-keymap (kbd "C-x C-f") (kbd "<self-insert> C-l")) ; open url
(define-key xwem-keytt-firefox-keymap (kbd "C-x C-s") (kbd "<self-insert> C-s")) ; save url
(define-key xwem-keytt-firefox-keymap (kbd "C-x k") (kbd "<self-insert> C-w")) ; close
(define-key xwem-keytt-firefox-keymap (kbd "C-n") (kbd "<self-insert> <down>")) ; scroll down
(define-key xwem-keytt-firefox-keymap (kbd "C-p") (kbd "<self-insert> <up>")) ; scroll up
(define-key xwem-keytt-firefox-keymap (kbd "C-f") (kbd "<self-insert> <right>")) ; scroll right
(define-key xwem-keytt-firefox-keymap (kbd "C-b") (kbd "<self-insert> <left>")) ; scroll left
(define-key xwem-keytt-firefox-keymap (kbd "C-/") (kbd "<self-insert> C-z")) ; undo
(define-key xwem-keytt-firefox-keymap (kbd "C-v") (kbd "<self-insert> <next>")) ; scroll page down
(define-key xwem-keytt-firefox-keymap (kbd "M-v") (kbd "<self-insert> <prior>")) ; scroll page up
(define-key xwem-keytt-firefox-keymap (kbd "M->") (kbd "<self-insert> <end>")) ; goto end
(define-key xwem-keytt-firefox-keymap (kbd "M-<") (kbd "<self-insert> <home>")) ; goto home
(define-key xwem-keytt-firefox-keymap (kbd "C-s") (kbd "<self-insert> /")) ; search
(define-key xwem-keytt-firefox-keymap (kbd "M-C-s") (kbd "<self-insert> <F3>")) ; next search
(define-key xwem-keytt-firefox-keymap (kbd "M-C-r") (kbd "<self-insert> Sh-<F3>")) ; prev search
(define-key xwem-keytt-firefox-keymap (kbd "C-g") (kbd "<self-insert> <ESC>")) ; cancel search
(define-key xwem-keytt-firefox-keymap (kbd "C-x [") (kbd "<self-insert> M-<left>")) ; back
(define-key xwem-keytt-firefox-keymap (kbd "C-x ]") (kbd "<self-insert> M-<right>")) ; forward
;(define-key xwem-keytt-firefox-keymap (kbd "+") (kbd "<self-insert> C-+")) ; zoom in
;(define-key xwem-keytt-firefox-keymap (kbd "-") (kbd "<self-insert> C--")) ; zoom out
;(define-key xwem-keytt-firefox-keymap (kbd "=") (kbd "<self-insert> C-0")) ; normal size
(define-key xwem-keytt-firefox-keymap (kbd "M-\\") (kbd "<self-insert> C-i")) ; page info
;(define-key xwem-keytt-firefox-keymap (kbd "\\") (kbd "<self-insert> C-u")) ; page source
(define-key xwem-keytt-firefox-keymap (kbd "C-x r m") (kbd "<self-insert> C-d")) ; Add bookmark
(define-key xwem-keytt-firefox-keymap (kbd "M-BS") (kbd "<self-insert> C-BS")) ; kill word backwand

;;; Opera keys
(defvar xwem-keytt-opera-keymap (make-sparse-keymap 'xwem-keytt-opera-keymap)
  "Keytt keymap for opera commands.")
(xwem-keytt-add-keymap '(and (application "opera")
                             (not (predicate xwem-cl-transient-for-p)))
                       xwem-keytt-opera-keymap t)

(xwem-keytt-define-universal-argument-commands xwem-keytt-opera-keymap)
(define-key xwem-keytt-opera-keymap (kbd "C-x C-c") (kbd "<self-insert> C-q")) ; quit
(define-key xwem-keytt-opera-keymap (kbd "C-x C-f") (kbd "<self-insert> C-l")) ; open url
(define-key xwem-keytt-opera-keymap (kbd "C-x C-s") (kbd "<self-insert> C-s")) ; save url
(define-key xwem-keytt-opera-keymap (kbd "C-t") (kbd "<self-insert> C-N")) ; new tab
(define-key xwem-keytt-opera-keymap (kbd "C-x k") (kbd "<self-insert> C-w")) ; close
(define-key xwem-keytt-opera-keymap (kbd "C-n") (kbd "<self-insert> <down>")) ; scroll down
(define-key xwem-keytt-opera-keymap (kbd "C-p") (kbd "<self-insert> <up>")) ; scroll up
(define-key xwem-keytt-opera-keymap (kbd "C-f") (kbd "<self-insert> <right>")) ; scroll right
(define-key xwem-keytt-opera-keymap (kbd "C-b") (kbd "<self-insert> <left>")) ; scroll left
(define-key xwem-keytt-opera-keymap (kbd "C-/") (kbd "<self-insert> C-z")) ; undo
(define-key xwem-keytt-opera-keymap (kbd "C-v") (kbd "<self-insert> <next>")) ; scroll page down
(define-key xwem-keytt-opera-keymap (kbd "M-v") (kbd "<self-insert> <prior>")) ; scroll page up
(define-key xwem-keytt-opera-keymap (kbd "M->") (kbd "<self-insert> <end>")) ; goto end
(define-key xwem-keytt-opera-keymap (kbd "M-<") (kbd "<self-insert> <home>")) ; goto home
(define-key xwem-keytt-opera-keymap (kbd "C-s") (kbd "<self-insert> /")) ; search
(define-key xwem-keytt-opera-keymap (kbd "M-C-s") (kbd "<self-insert> <F3>")) ; next search
(define-key xwem-keytt-opera-keymap (kbd "M-C-r") (kbd "<self-insert> Sh-<F3>")) ; prev search
(define-key xwem-keytt-opera-keymap (kbd "C-g") (kbd "<self-insert> <ESC>")) ; cancel search
(define-key xwem-keytt-opera-keymap (kbd "C-x [") (kbd "<self-insert> M-<left>")) ; back
(define-key xwem-keytt-opera-keymap (kbd "C-x ]") (kbd "<self-insert> M-<right>")) ; forward
(define-key xwem-keytt-opera-keymap (kbd "C-x r m") (kbd "<self-insert> C-t")) ; Add bookmark
(define-key xwem-keytt-opera-keymap (kbd "C-x 5 2") (kbd "<self-insert> C-N")) ; New window
(define-key xwem-keytt-opera-keymap (kbd "C-x 5 0") (kbd "<self-insert> C-W")) ; Close window


;; ACROREAD
(defvar xwem-keytt-acroread-keymap (make-sparse-keymap 'xwem-keytt-acroread-keymap)
  "Keytt keymap for acroread commands.")
(xwem-keytt-add-keymap '(and (application "acroread")
                             (not (predicate xwem-cl-transient-for-p)))
                       xwem-keytt-acroread-keymap t)

(xwem-keytt-define-universal-argument-commands xwem-keytt-acroread-keymap)
(define-key xwem-keytt-acroread-keymap (kbd "C-x C-f") (kbd "<self-insert> C-o")) ; open file
(define-key xwem-keytt-acroread-keymap (kbd "C-x C-c") (kbd "<self-insert> C-q")) ; quit
(define-key xwem-keytt-acroread-keymap (kbd "C-x C-s") (kbd "<self-insert> C-S")) ; save copy
(define-key xwem-keytt-acroread-keymap (kbd "C-x k") (kbd "<self-insert> C-w")) ; close file
(define-key xwem-keytt-acroread-keymap (kbd "C-n") (kbd "<self-insert> <down>")) ; scroll down
(define-key xwem-keytt-acroread-keymap (kbd "C-p") (kbd "<self-insert> <up>")) ; scroll up
(define-key xwem-keytt-acroread-keymap (kbd "C-v") (kbd "<self-insert> <next>")) ; next page
(define-key xwem-keytt-acroread-keymap (kbd "M-v") (kbd "<self-insert> <prior>")) ; prev page
(define-key xwem-keytt-acroread-keymap (kbd "M->") (kbd "<self-insert> C-Sh-<next>")) ; last page
(define-key xwem-keytt-acroread-keymap (kbd "M-<") (kbd "<self-insert> C-Sh-<prior>")) ; first page
(define-key xwem-keytt-acroread-keymap (kbd "C-M-l") (kbd "<self-insert> C-<left>")) ; first page
(define-key xwem-keytt-acroread-keymap (kbd "C-x h") (kbd "<self-insert> C-a")) ; select all
(define-key xwem-keytt-acroread-keymap (kbd "C-s") (kbd "<self-insert> C-f")) ; search
(define-key xwem-keytt-acroread-keymap (kbd "+") (kbd "<self-insert> C-=")) ; zoom in
(define-key xwem-keytt-acroread-keymap (kbd "-") (kbd "<self-insert> C--")) ; zoom out


;; XPDF
(defvar xwem-keytt-xpdf-keymap (make-sparse-keymap 'xwem-keytt-xpdf-keymap)
  "Keytt keymap for xpdf commands.")
(xwem-keytt-add-keymap '(and (application "xpdf")
                             (not (predicate xwem-cl-transient-for-p)))
                       xwem-keytt-xpdf-keymap t)

(xwem-keytt-define-universal-argument-commands xwem-keytt-xpdf-keymap)
(define-key xwem-keytt-xpdf-keymap (kbd "C-x C-f") (kbd "<self-insert> o")) ; open
(define-key xwem-keytt-xpdf-keymap (kbd "C-x C-c") (kbd "<self-insert> q")) ; exit
(define-key xwem-keytt-xpdf-keymap (kbd "C-s") (kbd "<self-insert> f")) ; find text
(define-key xwem-keytt-xpdf-keymap (kbd "C-g") (kbd "<self-insert> <ESC>")) ; stop searching
(define-key xwem-keytt-xpdf-keymap (kbd "M-<") (kbd "<self-insert> <home>")) ; goto first page
(define-key xwem-keytt-xpdf-keymap (kbd "M->") (kbd "<self-insert> <end>")) ; goto last page
(define-key xwem-keytt-xpdf-keymap (kbd "C-n") (kbd "<self-insert> <down>")) ; scroll down
(define-key xwem-keytt-xpdf-keymap (kbd "C-p") (kbd "<self-insert> <up>")) ; scroll up
(define-key xwem-keytt-xpdf-keymap (kbd "C-v") (kbd "<self-insert> n"))
(define-key xwem-keytt-xpdf-keymap (kbd "M-v") (kbd "<self-insert> p"))

;; XDVI
(defvar xwem-keytt-xdvi-keymap (make-sparse-keymap 'xwem-keytt-xdvi-keymap)
  "Keytt keymap for xdvi commands.")
(xwem-keytt-add-keymap '(and (application "xdvi")
                             (not (predicate xwem-cl-transient-for-p)))
                       xwem-keytt-xdvi-keymap t)

(xwem-keytt-define-universal-argument-commands xwem-keytt-xdvi-keymap)
(define-key xwem-keytt-xdvi-keymap (kbd "C-x C-f") (kbd "<self-insert> C-F")) ; open
(define-key xwem-keytt-xdvi-keymap (kbd "C-x C-c") (kbd "<self-insert> q")) ; exit
(define-key xwem-keytt-xdvi-keymap (kbd "C-s")   (kbd "<self-insert> f")); find text
(define-key xwem-keytt-xdvi-keymap (kbd "M-<") (kbd "<self-insert> <home>")) ; goto first page
(define-key xwem-keytt-xdvi-keymap (kbd "M->") (kbd "<self-insert> <end>")) ; goto last page
(define-key xwem-keytt-xdvi-keymap (kbd "C-n") (kbd "<self-insert> <down>")) ; scroll down
(define-key xwem-keytt-xdvi-keymap (kbd "C-p") (kbd "<self-insert> <up>")) ; scroll up
(define-key xwem-keytt-xdvi-keymap (kbd "C-v") (kbd "<self-insert> n"))
(define-key xwem-keytt-xdvi-keymap (kbd "M-v") (kbd "<self-insert> p"))
(define-key xwem-keytt-xdvi-keymap (kbd "C-l") (kbd "<self-insert> R")) ; reread dvi


;; DJVIEW
;;  Note: djvu has trouble with the file menu having two of the entries liked
;;        to o so not likely to work untill its fixed.
(defvar xwem-keytt-djvu-keymap (make-sparse-keymap 'xwem-keytt-djvu-keymap)
  "Keytt keymap for djvu commands.")
(xwem-keytt-add-keymap '(and (application "djview")
                             (not (predicate xwem-cl-transient-for-p)))
                       xwem-keytt-djvu-keymap t)

(xwem-keytt-define-universal-argument-commands xwem-keytt-djvu-keymap)
(define-key xwem-keytt-djvu-keymap (kbd "C-x C-f") (kbd "<self-insert> A-f O")) ; open
(define-key xwem-keytt-djvu-keymap (kbd "C-x C-c") (kbd "<self-insert> A-f x")) ; exit
(define-key xwem-keytt-djvu-keymap (kbd "C-x C-w") (kbd "<self-insert> A-f a")) ; save current page
(define-key xwem-keytt-djvu-keymap (kbd "C-x C-s") (kbd "<self-insert> A-f o")) ; save

(define-key xwem-keytt-djvu-keymap (kbd "C-s") (kbd "<self-insert> f")); find text
(define-key xwem-keytt-djvu-keymap (kbd "M-<") (kbd "<self-insert> <home>")) ; goto first page
(define-key xwem-keytt-djvu-keymap (kbd "M->") (kbd "<self-insert> <end>")) ; goto last page
(define-key xwem-keytt-djvu-keymap (kbd "C-n") (kbd "<self-insert> <down>")) ; scroll down
(define-key xwem-keytt-djvu-keymap (kbd "C-p") (kbd "<self-insert> <up>")) ; scroll up
(define-key xwem-keytt-djvu-keymap (kbd "C-v") (kbd "<self-insert> <next>"))
(define-key xwem-keytt-djvu-keymap (kbd "M-v") (kbd "<self-insert> <prior>"))


;; XCHM
;;  Note: Must send mail to the xchm developer with patches to xchm and make
;;        it have more stuff it menus attached to keymaps
(defvar xwem-keytt-xchm-keymap (make-sparse-keymap 'xwem-keytt-xchm-keymap)
  "Keytt keymap for xchm commands.")
(xwem-keytt-add-keymap '(and (application "xchm")
                             (not (predicate xwem-cl-transient-for-p)))
                       xwem-keytt-xchm-keymap t)

(xwem-keytt-define-universal-argument-commands xwem-keytt-xchm-keymap)
(define-key xwem-keytt-xchm-keymap (kbd "C-x C-f") (kbd "<self-insert> C-o")) ; open
(define-key xwem-keytt-xchm-keymap (kbd "C-x C-c") (kbd "<self-insert> C-x")) ; exit
(define-key xwem-keytt-xchm-keymap (kbd "C-s")   (kbd "<self-insert> C-s ")); find show tree

;; OpenOffice
(defvar xwem-keytt-oo-keymap (make-sparse-keymap 'xwem-keytt-oo-keymap)
  "Keytt keymap for OpenOffice commands.")
(xwem-keytt-add-keymap '(and (application "openoffice")
                             (not (predicate xwem-cl-transient-for-p)))
                       xwem-keytt-oo-keymap t)

(xwem-keytt-define-universal-argument-commands xwem-keytt-oo-keymap)
(define-key xwem-keytt-oo-keymap (kbd "C-f") (kbd "<self-insert> <right>"))
(define-key xwem-keytt-oo-keymap (kbd "C-b") (kbd "<self-insert> <left>"))
(define-key xwem-keytt-oo-keymap (kbd "C-p") (kbd "<self-insert> <up>"))
(define-key xwem-keytt-oo-keymap (kbd "C-n") (kbd "<self-insert> <down>"))
(define-key xwem-keytt-oo-keymap (kbd "C-a") (kbd "<self-insert> <home>")) ; goto beginning of line
(define-key xwem-keytt-oo-keymap (kbd "C-e") (kbd "<self-insert> <end>")) ; goto end of line
(define-key xwem-keytt-oo-keymap (kbd "M-f") (kbd "<self-insert> C-<right>"))
(define-key xwem-keytt-oo-keymap (kbd "M-b") (kbd "<self-insert> C-<left>"))
(define-key xwem-keytt-oo-keymap (kbd "C-v") (kbd "<self-insert> <next>"))
(define-key xwem-keytt-oo-keymap (kbd "M-v") (kbd "<self-insert> <prior>"))
(define-key xwem-keytt-oo-keymap (kbd "C-d") (kbd "<self-insert> <DEL>"))
(define-key xwem-keytt-oo-keymap (kbd "C-/") (kbd "<self-insert> C-z")) ; undo
(define-key xwem-keytt-oo-keymap (kbd "C-x C-/") (kbd "<self-insert> C-y")) ; redo
(define-key xwem-keytt-oo-keymap (kbd "C-w") (kbd "<self-insert> C-x")) ; cut
(define-key xwem-keytt-oo-keymap (kbd "C-y") (kbd "<self-insert> C-v")) ; paste
(define-key xwem-keytt-oo-keymap (kbd "C-s") (kbd "<self-insert> C-f")) ; find
(define-key xwem-keytt-oo-keymap (kbd "M-C-s") (kbd "<self-insert> C-F")) ; find next
(define-key xwem-keytt-oo-keymap (kbd "C-x C-s") (kbd "<self-insert> C-s")) ; save
(define-key xwem-keytt-oo-keymap (kbd "C-x C-c") (kbd "<self-insert> C-q")) ; quit
(define-key xwem-keytt-oo-keymap (kbd "C-x C-f") (kbd "<self-insert> C-o")) ; open
(define-key xwem-keytt-oo-keymap (kbd "M-q") (kbd "<self-insert> C-j")) ; justify
(define-key xwem-keytt-oo-keymap (kbd "M-d") (kbd "<self-insert> C-<DEL>")) ; kill word
(define-key xwem-keytt-oo-keymap (kbd "M-<backspace>") (kbd "<self-insert> C-<backspace>")) ; kill word
(define-key xwem-keytt-oo-keymap (kbd "M-k") (kbd "<self-insert> C-Sh-<DEL>")) ; kill sentence
(define-key xwem-keytt-oo-keymap (kbd "M-<tab>") (kbd "<self-insert> C-<tab>")) ; next completion
(define-key xwem-keytt-oo-keymap (kbd "M-Sh-<tab>") (kbd "<self-insert> C-Sh-<tab>")) ; previous completion
(define-key xwem-keytt-oo-keymap (kbd "M->") (kbd "<self-insert> C-<end>")) ; goto end of document
(define-key xwem-keytt-oo-keymap (kbd "M-<") (kbd "<self-insert> C-<home>")) ; goto start of document

;; TODO: install right/left/up/down/words operations to do selections
(define-key xwem-keytt-oo-keymap (kbd "C-<SPC>") 'xwem-keytt-enter-selection-mode)


;;; On-load actions:
(xwem-add-minor-mode 'xwem-keytt-minor-mode "KeyTT" 'xwem-keytt-keymap)


(provide 'xwem-keytt)

;;; xwem-keytt.el ends here
