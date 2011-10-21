;;; xwem-icons.el --- Icons handling routines.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;; Created: Sat Dec 27 15:38:24 MSK 2003
;; Keywords: xwem
;; Time-stamp: <8/8/2008 06:48:04 lg@h1.lan>

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

;; Icons support.

;; Supports client properties:

;;    `xwem-tab-face' - Face to draw tabber item (overrides `xwem-tabber-face'
;;    `xwem-icon-name' - Name of icon to use (overrides `xwem-icons-list')

;;; Code:

(require 'xlib-xpm)

(require 'xwem-load)

;;;###autoload
(defcustom xwem-icon-dirs (list (locate-data-directory "xwem"))
  "Directory where icons for use by XWEM lies."
  :type '(list directory)
  :group 'xwem)

;;;###autoload
(defcustom xwem-icons-list
  '(("mini-display.xpm"
     (and (class-inst "^Terminal$")
          (class-name "^Terminal$")))
    ("mini-clock.xpm" (application "xclock"))
    ("mini-measure.xpm" (application "xload"))
    ("mini-calc.xpm"
     (or (buffer-major-mode calc-mode)
         (class-name "[cC]alc")))

    ("mini-inkscape.xpm" (application "inkscape"))
    ("mini-xkeycaps.xpm" (application "xkeycaps"))
    ("mini-xv.xpm" (application "xv"))
    ("mini-imagemagic.xpm" (application "display"))

    ("mini-xdvi.xpm" (application "xdvi"))
    ("mini-acroread.xpm" (application "PdfViewer"))
    ("mini-info.xpm" (class-name "Xman"))
    ("mini-gimp.xpm" (application "gimp"))
    ("mini-djvu.xpm" (application "djview"))

    ;; EMACS
    ("mini-xemacstex.xpm"
     (or (buffer-major-mode plain-tex-mode)
         (buffer-major-mode texinfo-mode)
         (and (application "xemacs")
              (name  "\\.tex"))))
    ("mini-xemacsinfo.xpm"
     (or (buffer-major-mode Info-mode)
	 (and (application "xemacs")
	      (name "\\*info\\*"))))
    ("mini-xemacsirc.xpm"
     (or (buffer-major-mode erc-mode)
	 (buffer-major-mode riece-channel-list-mode)
	 (buffer-major-mode riece-channel-mode)
	 (buffer-major-mode riece-command-mode)
	 (buffer-major-mode riece-dialogue-mode)
	 (buffer-major-mode riece-others-mode)
	 (buffer-major-mode riece-user-list-mode)))
    ("mini-xemacsC.xpm"
     (or (buffer-major-mode c-mode)
         (and (application "xemacs")
              (name  "\\.[ch]"))))
    ("mini-xemacslisp.xpm"
     (or (buffer-major-mode lisp-mode)
         (buffer-major-mode emacs-lisp-mode)
         (buffer-major-mode lisp-interaction-mode)
         (and (application "xemacs")
              (name "\\.\\(lisp\\|el\\|\\*scratch\\*\\)"))))
    ("mini-xemacsgnus.xpm"
     (or (buffer-major-mode gnus-group-mode)
         (buffer-major-mode gnus-article-mode)
         (buffer-major-mode gnus-summary-mode)
         (and (application "xemacs")
              (name "\\(Group\\|Summary\\|Article\\)"))))
    ("mini-xemacspy.xpm"
     (or (buffer-major-mode python-mode)
         (and (application "xemacs")
              (name "\\.py"))))

    ("mini-python.xpm"
     (or (buffer-name "\\*Python\\*")   ; Python in Emacs
         (name "^Python Shell$")))      ; IDLE

    ;; MISC
    ("mini-links.xpm" (application "links"))
    ("mini-ddd.xpm" (application "ddd"))
    ("mini-vnc.xpm" (application "vncviewer"))
    ("mini-firefox.xpm" (application "firefox"))
    ("mini-mozilla.xpm" (application "mozilla"))
    ("mini-opera.xpm" (application "opera"))
    ("mini-gv.xpm" (or (application "gv") (application "ghostview")))
    ("mini-xfig.xpm" (application "xfig"))
    ("mini-ethereal.xpm" (application "ethereal"))
    ("mini-font.xpm" (or (application "xfd")
                         (application "xfontsel")))

    ("mini-sh1.xpm"
     (or (buffer-major-mode eshell-mode)
         (buffer-major-mode shell-mode)
         (buffer-major-mode term-mode)
         (buffer-major-mode terminal-mode)
         (and (class-inst "x?console")
              (class-name "[Xx]?[Cc]onsole"))))
    ("mini-colors.xpm"
     (or (buffer-name "\\*Colors\\*")
         (and (class-inst "x?colors?")
              (class-name "[Xx]?colors?"))
         (name "^xcmap$")))
    ("mini-xchat.xpm" (application "xchat"))
    ("mini-diag.xpm" (application "gnumeric"))


    ("mini-zoom.xpm" (or (class-inst "^Xmag$")
                         (name "^Lupe$")))
    ("mini-graph.xpm" (application "gnuplot"))

    ("mini-quake.xpm" (or (and (class-inst "^xqf$")
                               (class-name "^Xqf$"))
                          (name "^FuhQuake$")))

    ;; ICQ
    ("mini-icq.xpm"
     (or (buffer-major-mode eicq-buddy-mode)
         (buffer-major-mode eicq-log-mode)
         (and (application "xemacs")
              (name "\\*Status\\*"))
         (application "licq")
         (name "[LlMmVv][Ii][Cc][Qq]")))

    ;; xterm
    ("mini-term.xpm" (and (class-name "^.[tT]erm$")
                          (or (class-inst "^.term$")
                              (class-inst "^rxvt$"))))

    ("mini-xemacs.xpm" (application "xemacs"))

    ("mini-x2.xpm" (t)))                ; any other
  "Icons matching list in `xwem-manage-list' format."
  :type '(repeat
          (cons :tag "Icon specifier"
                (string :tag "Icon name")
                (repeat
                 (cons
                  (choice (const :tag "Application" application)
                          (const :tag "Class name" class-name)
                          (const :tag "Class instance" class-inst)
                          (const :tag "Name" name)
                          (const :tag "Buffer Major Mode" buffer-major-mode)
                          (const :tag "Buffer Name" buffer-name)
                          (const :tag "Buffer Name" buffer-name)
                          (const :tag "Sexp for evaluation" eval)
                          (const :tag "Function" function)
                          (const :tag "Or operation" or)
                          (const :tag "And operation" and))
                  (repeat sexp)))))
  :group 'xwem)

;;; Internal variables

(defvar xwem-icons-specifiers nil
  "List of icons specifiers.")

(defvar xwem-icons-loaded-list nil
  "List of already loaded icons.")


;;; Faces
(define-xwem-face xwem-icon-red-face
  `(((shade) (:foreground "red3"))
    (t (:foreground "red")))
  "Red face to be used by icons.")

(define-xwem-face xwem-icon-green-face
  `(((shade) (:foreground "green3"))
    (t (:foreground "green")))
  "Green face to be used by icons.")

(define-xwem-face xwem-icon-blue-face
  `(((shade) (:foreground "blue3"))
    (t (:foreground "blue")))
  "Blue face to be used by icons.")

(define-xwem-face xwem-icon-cyan-face
  `(((shade) (:foreground "cyan3"))
    (t (:foreground "cyan")))
  "Cyan face to be used by icons.")

(define-xwem-face xwem-icon-magenta-face
  `(((shade) (:foreground "magenta3"))
    (t (:foreground "magenta")))
  "Magenta face to be used by icons.")

(define-xwem-face xwem-icon-yellow-face
  `(((shade) (:foreground "yellow3"))
    (t (:foreground "yellow")))
  "Yellow face to be used by icons.")

(define-xwem-face xwem-icon-brown-face
  `(((shade) (:foreground "brown3"))
    (t (:foreground "brown")))
  "Brown face to be used by icons.")


(define-xwem-client-property xwem-icon-name nil
  "Icon to use for client."
  :type 'string)

;;; Functions
(defun xwem-icons-cl-icon-name (cl)
  "Return icon name for CL."
  (or (xwem-client-property cl 'xwem-icon-name)
      (car (xwem-manda-find-match-1 cl xwem-icons-list 'cadr))))

;;;###xwem-autoload
(defun xwem-icon-find-file (iname)
  "Return expanded icon filename for icon named INAME."
  (expand-file-name
   iname (find iname xwem-icon-dirs
               :test #'(lambda (in dir)
                         (file-exists-p (expand-file-name in dir))))))

(defun xwem-icons-cl-buildin-icon (cl &optional tag-set)
  "Return build in icon for CL."
  (let ((iname (xwem-icons-cl-icon-name cl))
        ximg-spec fname ximg ximg-mask-pixmap)
    (when iname
      (setq ximg-spec (plist-get xwem-icons-specifiers iname))

      (unless ximg-spec
        (setq ximg-spec (make-specifier 'generic))
        (setq xwem-icons-specifiers
              (plist-put xwem-icons-specifiers iname ximg-spec)))

      (setq ximg (plist-get xwem-icons-loaded-list
                   (cdar (cdar (specifier-spec-list ximg-spec nil tag-set t)))))
      (unless ximg
        ;; No image in TAG-SET environ
        (setq fname (xwem-icon-find-file iname))
        (setq ximg (X:xpm-pixmap-from-file
                    (xwem-dpy) (XDefaultRootWindow (xwem-dpy))
                    fname nil tag-set))
        (setq ximg-mask-pixmap
              (X:xpm-pixmap-from-file
               (xwem-dpy) (XDefaultRootWindow (xwem-dpy)) fname t tag-set))

        (setq ximg (cons ximg ximg-mask-pixmap))
        (let ((sym (gensym "*xwem-icon-")))
          (add-spec-list-to-specifier ximg-spec
             `((global ,(cons tag-set sym))) 'remove-tag-set-prepend)

          (setq xwem-icons-loaded-list
                (plist-put xwem-icons-loaded-list sym ximg))))
      ximg)))

(defun xwem-icons-cl-kwm-win-icon (cl &optional tag-set)
  "Return CL's KWM_WIN_ICON if specified."
  (let ((kwi (xwem-cl-get-sys-prop cl 'kwm-win-icon)))
    (cond ((eq kwi 'no-kwm-win-icon) nil)
          ((null kwi)
           ;; KWM_WIN_ICON not yet checked
           (let* ((kw (XGetWindowProperty
                       (xwem-dpy) (xwem-cl-xwin cl)
                       (XInternAtom (xwem-dpy) "KWM_WIN_ICON")))
                  (pp (and (nth 2 kw) (make-X-Pixmap :dpy (xwem-dpy)
                                                     :id (nth 2 kw))))
                  (pm (and (nth 3 kw) (make-X-Pixmap :dpy (xwem-dpy)
                                                     :id (nth 3 kw))))
                  (gg nil))
             (if (not (and pp pm))
                 (xwem-cl-put-sys-prop cl 'kwm-win-icon 'no-kwm-win-icon)

               (setq gg (XGetGeometry (xwem-dpy) pp))
               (setf (X-Pixmap-width pp) (X-Geom-width gg))
               (setf (X-Pixmap-height pp) (X-Geom-height gg))

               (setq gg (XGetGeometry (xwem-dpy) pm))
               (setf (X-Pixmap-width pm) (X-Geom-width gg))
               (setf (X-Pixmap-height pm) (X-Geom-height gg))

               (xwem-cl-put-sys-prop cl 'kwm-win-icon (setq gg (cons pp pm))))
             gg))
          (t kwi))))

;;;###xwem-autoload
(defun xwem-icons-cl-icon (cl &optional tag-set)
  "Get X-Image of CL's icon.
Return cons cell where car is X-Pixmap of icon and cdr is X-Pixmap
where mask for icon is stored.

TAG-SET specifies environment list for which icon is created."
  (or (xwem-icons-cl-kwm-win-icon cl tag-set)
      (xwem-icons-cl-buildin-icon cl tag-set)))


(provide 'xwem-icons)

;;; xwem-icons.el ends here
