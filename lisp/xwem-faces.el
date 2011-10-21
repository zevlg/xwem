;;; xwem-faces.el --- XWEM can handle Emacs faces.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;; Created: Mon Dec 29 12:04:19 MSK 2003
;; Keywords: xwem
;; Time-stamp: <29/11/2006 23:52:42 lg@h1>

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

;; Faces support.  Main purpose of `xwem-faces' is easify interface to
;; X-Gc.

;;; Code:

(require 'cus-face)                     ; for `custom-face-get-spec'
(require 'xlib-xlib)

(require 'xwem-struct)
(require 'xwem-loaddefs)

;;; Customisation
(defgroup xwem-faces nil
  "*Group to customize faces used by XWEM."
  :prefix "xwem-face-"
  :group 'xwem)

(defcustom xwem-background-mode-bound 72000
  "Number which is used to determine whether color has `dark' or `light' background mode.
Normally you should'nt modify it."
  :type '(choice (const :tag "Default" 65536)
                 (const :tag "Low" 50000)
                 (const :tag "Medium" 72000)
                 (const :tag "High" 94000))
  :group 'xwem-faces)

;;; Internal variables

;;;###xwem-autoload
(defun xwem-get-background-mode (color)
  "Return `dark' or `light' according to COLOR.
COLOR is color instance, color name or list of color components."
  (if (< (apply '+ (or (and (listp color) color)
                       (color-instance-rgb-components
                        (or (and (color-instance-p color) color)
                            (make-color-instance color (default-x-device))))))
         xwem-background-mode-bound)
      'dark 'light))

;;; TODO:
;;   - Create xwem-face widget for customisation.
(define-specifier-tag 'default)

(defsubst xwem-face-foreground-specifier (face)
  (get face 'xwem-face-foreground-specifier))
(defsubst xwem-face-background-specifier (face)
  (get face 'xwem-face-background-specifier))
(defsubst xwem-face-font-specifier (face)
  (get face 'xwem-face-font-specifier))
(defsubst xwem-face-line-style-specifier (face)
  (get face 'xwem-face-line-style-specifier))
(defsubst xwem-face-line-width-specifier (face)
  (get face 'xwem-face-line-width-specifier))
(defsubst xwem-face-cap-style-specifier (face)
  (get face 'xwem-face-cap-style-specifier))
(defsubst xwem-face-join-style-specifier (face)
  (get face 'xwem-face-join-style-specifier))
(defsubst xwem-face-function-specifier (face)
  (get face 'xwem-face-function-specifier))
(defsubst xwem-face-subwindow-mode-specifier (face)
  (get face 'xwem-face-subwindow-mode-specifier))
(defsubst xwem-face-graphics-exposures-specifier (face)
  (get face 'xwem-face-graphics-exposures-specifier))

(defsubst xwem-face-x-gc-specifier (face)
  (get face 'xwem-face-x-gc-specifier))

(defun xwem-face-sort-tag-set (tag-set)
  (sort tag-set #'(lambda (t1 t2)
                    (string-lessp (symbol-name t1) (symbol-name t2)))))

(defun xwem-custom-declare-face (face spec doc &rest args)
  "Define new FACE to be used in xwem.
SPEC is face specification, it should be an alist in form
\\((DISPLAY ATTRS) ...\\)."
  (unless (get face 'xwem-face-spec)
    (put face 'xwem-face-spec spec)
    (unless (find-face face)
      (make-empty-face face)
;      (face-display-set face spec nil '(custom))

      (put face 'xwem-face-font-specifier
           (make-specifier-and-init 'generic `((global ((default) . ,(face-font-name 'default)))) t))
      (put face 'xwem-face-background-specifier
           (make-specifier-and-init 'generic `((global ((default) . "black"))) t))
      (put face 'xwem-face-foreground-specifier
           (make-specifier-and-init 'generic `((global ((default) . "white"))) t))
      (put face 'xwem-face-line-style-specifier
           (make-specifier-and-init 'generic `((global ((default) . ,X-LineSolid))) t))
      (put face 'xwem-face-line-width-specifier
           (make-specifier-and-init 'generic `((global ((default) . 0))) t))
      (put face 'xwem-face-cap-style-specifier
           (make-specifier-and-init 'generic `((global ((default) . ,X-CapButt))) t))
      (put face 'xwem-face-join-style-specifier
           (make-specifier-and-init 'generic `((global ((default) . ,X-JoinMiter))) t))
      (put face 'xwem-face-function-specifier
           (make-specifier-and-init 'generic `((global ((default) . ,X-GXCopy))) t))
      (put face 'xwem-face-subwindow-mode-specifier
           (make-specifier-and-init 'generic `((global ((default) . ,X-ClipByChildren))) t))
      (put face 'xwem-face-graphics-exposures-specifier
           (make-specifier-and-init 'generic `((global ((default) . ,X-False))) t))

      (put face 'xwem-face-x-gc-specifier
           (make-specifier 'generic))

      ;; Setup face
      (let (tag-set tag-set-plist tval font)
        (while spec
          (setq tag-set (caar spec))
          (setq tag-set-plist (cadr (car spec)))
          (cond ((eq tag-set t)
                 (setq tag-set '(default)))
                ((listp tag-set)
                 ;; Define new tags
                 (setq tag-set (xwem-face-sort-tag-set tag-set))
                 (mapc 'define-specifier-tag tag-set))
                (t (error 'xwem-error "Inavild TAG-SET specification")))

          ;; Setup face params
          (when (setq tval (plist-get tag-set-plist :foreground))
            (add-spec-list-to-specifier (xwem-face-foreground-specifier face)
                                        `((global ,(cons tag-set tval))) 'remove-tag-set-prepend))
          (when (setq tval (plist-get tag-set-plist :background))
            (add-spec-list-to-specifier (xwem-face-background-specifier face)
                                        `((global ,(cons tag-set tval))) 'remove-tag-set-prepend))

          ;; Play with font
          ;;; Old stuff commented out, because of limitations
;          (unless (setq font (plist-get tag-set-plist :font))
;            (setq font (face-font-name 'default)))
;          (if (plist-get tag-set-plist :bold)
;              (setq font (or (x-make-font-bold font) font))
;            (setq font (or (x-make-font-unbold font) font)))
;          (if (plist-get tag-set-plist :italic)
;              (setq font (or (x-make-font-italic font) font))
;            (setq font (or (x-make-font-unitalic font) font)))

          (unless (setq font (plist-get tag-set-plist :font))
            (setq font (face-font-name 'default)))
          (when font
            (set-face-font face font))
          (flet ((booleanp (arg) (or (eq arg t) (null arg))))
            (mapc #'(lambda (kw-pre)
                      (let* ((kw (car kw-pre))
                             (kv (assq kw custom-face-attributes))
                             (rv (plist-get tag-set-plist kw)))
                        (when (and kv (funcall (cadr kw-pre) rv))
                          (funcall (caddr kv) face rv))))
                  '((:size numberp) (:family stringp) (:dim booleanp)
                    (:bold booleanp) (:italic booleanp) (:underline booleanp))))
          (setq font (face-font-name face))
          (add-spec-list-to-specifier (xwem-face-font-specifier face)
                                      `((global ,(cons tag-set font))) 'remove-tag-set-prepend)

          ;; Other params
          (when (setq tval (plist-get tag-set-plist :line-style))
            (add-spec-list-to-specifier (xwem-face-line-style-specifier face)
                                        `((global ,(cons tag-set tval))) 'remove-tag-set-prepend))
          (when (setq tval (plist-get tag-set-plist :line-width))
            (add-spec-list-to-specifier (xwem-face-line-width-specifier face)
                                        `((global ,(cons tag-set tval))) 'remove-tag-set-prepend))
          (when (setq tval (plist-get tag-set-plist :cap-style))
            (add-spec-list-to-specifier (xwem-face-cap-style-specifier face)
                                        `((global ,(cons tag-set tval))) 'remove-tag-set-prepend))
          (when (setq tval (plist-get tag-set-plist :join-style))
            (add-spec-list-to-specifier (xwem-face-join-style-specifier face)
                                        `((global ,(cons tag-set tval))) 'remove-tag-set-prepend))
          (when (setq tval (plist-get tag-set-plist :function))
            (add-spec-list-to-specifier (xwem-face-function-specifier face)
                                        `((global ,(cons tag-set tval))) 'remove-tag-set-prepend))
          (when (setq tval (plist-get tag-set-plist :subwindow-mode))
            (add-spec-list-to-specifier (xwem-face-subwindow-mode-specifier face)
                                        `((global ,(cons tag-set tval))) 'remove-tag-set-prepend))
          (when (setq tval (plist-get tag-set-plist :graphics-exposures))
            (add-spec-list-to-specifier (xwem-face-graphics-exposures-specifier face)
                                        `((global ,(cons tag-set tval))) 'remove-tag-set-prepend))

          (setq spec (cdr spec))))

      (unless (and doc (not (face-doc-string face)))
        (set-face-doc-string face doc))
      (custom-handle-all-keywords face args 'custom-face)
      face)))

(defmacro define-xwem-face (face spec doc &rest args)
  "Define new xwem face."
  `(xwem-custom-declare-face (quote ,face) ,spec ,doc ,@args))

(defun xwem-copy-face (old-face new-face)
  "Create NEW-FACE as copy of OLD-FACE."
  (xwem-custom-declare-face new-face (get old-face 'xwem-face-spec)
    (format "Copy of `%S' face." (face-name old-face)))
  )

(define-xwem-face xwem-face-white
  `((t (:foreground "white" :background "black")))
  "White color face."
  :group 'xwem-faces)

(define-xwem-face xwem-face-black
  `((t (:foreground "black" :background "black")))
  "Black color face."
  :group 'xwem-faces)

(define-xwem-face xwem-face-red
  `((t (:foreground "red" :background "black")))
  "Red color face."
  :group 'xwem-faces)

(define-xwem-face xwem-face-green
  `((t (:foreground "green" :background "black")))
  "Green color face."
  :group 'xwem-faces)

(define-xwem-face xwem-face-blue
  `((t (:foreground "blue" :background "black")))
  "Blue color face."
  :group 'xwem-faces)

(define-xwem-face xwem-face-yellow
  `((t (:foreground "yellow" :background "black")))
  "Yellow color face."
  :group 'xwem-faces)

(define-xwem-face xwem-face-cyan
  `((t (:foreground "cyan" :background "black")))
  "Cyan color face."
  :group 'xwem-faces)

(define-xwem-face xwem-face-magenta
  `((t (:foreground "magenta" :background "black")))
  "Magenta color face."
  :group 'xwem-faces)

(define-xwem-face xwem-face-darkgray
  `((t (:foreground "gray20" :background "black")))
  "DarkGray color face."
  :group 'xwem-faces)

(define-xwem-face xwem-face-lightgray
  `((t (:foreground "gray80" :background "black")))
  "LightGray color face."
  :group 'xwem-faces)

(define-xwem-face xwem-face-gray
  `((t (:foreground "gray50" :background "black")))
  "Gray color face."
  :group 'xwem-faces)

(define-xwem-face xwem-face-outline1
  `((t (:foreground "red" :background "black")))
  "Face used to outline something."
  :group 'xwem-faces)

(define-xwem-face xwem-face-outline2
  `((t (:foreground "blue" :background "black")))
  "Face used to outline something."
  :group 'xwem-faces)

(define-xwem-face xwem-face-outline3
  `((t (:foreground "cyan" :background "black")))
  "Face used to outline something."
  :group 'xwem-faces)


(defun xwem-face-get-domain-face (face &optional domain)
  "Adjust FACE using DOMAIN."
  (cond ((null domain)
         (or (and (xwem-cl-selected)
                  (xwem-face-get-domain-face face (xwem-cl-selected)))
             face))
        ((xwem-cl-p domain)
         (or (cdr (assq face (xwem-cl-get-sys-prop domain 'domain-faces)))
             (and (xwem-cl-win domain)
                  (xwem-face-get-domain-face face (xwem-cl-win domain)))))
        ((xwem-win-p domain)
         (or (cdr (assq face (xwem-win-get-prop domain 'domain-faces)))
             (xwem-face-get-domain-face face (xwem-win-frame domain))))
        ((xwem-frame-p domain)
         (cdr (assq face (xwem-frame-get-prop domain 'domain-faces))))
        ((eq domain 'nodomain) face)
        (t (error 'xwem-error "Invalid domain" domain))))

;;; Functions
(defun xwem-face-generic-specifier (face spcname &optional tag-set domain)
  "Return FACE's SPCNAME specifier value in TAG-SET.
SPCNAME is one of 'foreground, 'background, 'font, 'line-style,
'line-width, 'cap-style, 'join-style, 'function, 'subwindow-mode,
'graphics-exposures or 'x-gc.
DOMAIN is either xwem window, xwem client or xwem frame."
  (if (and tag-set (listp tag-set))
      (setq tag-set (xwem-face-sort-tag-set tag-set))
    (setq tag-set '(default)))

  ;; Adjust face according to DOMAIN, for get operation
  (setq face (or (xwem-face-get-domain-face face domain)
                 face))

  (let* ((fn (cond ((eq spcname 'foreground) 'xwem-face-foreground-specifier)
                   ((eq spcname 'background) 'xwem-face-background-specifier)
                   ((eq spcname 'font) 'xwem-face-font-specifier)
                   ((eq spcname 'line-style) 'xwem-face-line-style-specifier)
                   ((eq spcname 'line-width) 'xwem-face-line-width-specifier)
                   ((eq spcname 'cap-style) 'xwem-face-cap-style-specifier)
                   ((eq spcname 'join-style) 'xwem-face-join-style-specifier)
                   ((eq spcname 'function) 'xwem-face-function-specifier)
                   ((eq spcname 'subwindow-mode) 'xwem-face-subwindow-mode-specifier)
                   ((eq spcname 'graphics-exposures) 'xwem-face-graphics-exposures-specifier)
                   ((eq spcname 'x-gc) 'xwem-face-x-gc-specifier)
                   (t (error 'xwem-error "Invalid SPCNAME" spcname))))
         (spec (funcall fn face)))
    (when (specifierp spec)
      (setq spec (specifier-spec-list spec nil tag-set t))
      (cdr (car (cdr (car spec)))))))

;; Getters
(defsubst xwem-face-foreground (face &optional tag-set domain)
  "Return FACE foreground color in TAG-SET."
  (or (xwem-face-generic-specifier face 'foreground tag-set domain)
      (xwem-face-generic-specifier face 'foreground)
      (face-foreground-name face)))
(defsubst xwem-face-background (face &optional tag-set domain)
  "Return FACE background color in TAG-SET."
  (or (xwem-face-generic-specifier face 'background tag-set domain)
      (xwem-face-generic-specifier face 'background)
      (face-background-name face)))
(defsubst xwem-face-font (face &optional tag-set domain)
  "Return FACE font in TAG-SET."
  (or (xwem-face-generic-specifier face 'font tag-set domain)
      (xwem-face-generic-specifier face 'font)
      (face-font-name face)))
(defsubst xwem-face-line-style (face &optional tag-set domain)
  "Return FACE's line style in TAG-SET."
  (or (xwem-face-generic-specifier face 'line-style tag-set domain)
      (xwem-face-generic-specifier face 'line-style)))
(defsubst xwem-face-line-width (face &optional tag-set domain)
  "Return FACE's line width TAG-SET."
  (or (xwem-face-generic-specifier face 'line-width tag-set domain)
      (xwem-face-generic-specifier face 'line-width)))
(defsubst xwem-face-cap-style (face &optional tag-set domain)
  "Return FACE's cap style in TAG-SET."
  (or (xwem-face-generic-specifier face 'cap-style tag-set domain)
      (xwem-face-generic-specifier face 'cap-style)))
(defsubst xwem-face-join-style (face &optional tag-set domain)
  "Return FACE's join style in TAG-SET."
  (or (xwem-face-generic-specifier face 'join-style tag-set domain)
      (xwem-face-generic-specifier face 'join-style)))
(defsubst xwem-face-function (face &optional tag-set domain)
  "Return FACE's function in TAG-SET."
  (or (xwem-face-generic-specifier face 'function tag-set domain)
      (xwem-face-generic-specifier face 'function)))
(defsubst xwem-face-subwindow-mode (face &optional tag-set domain)
  "Return FACE's cap style in TAG-SET."
  (or (xwem-face-generic-specifier face 'subwindow-mode tag-set domain)
      (xwem-face-generic-specifier face 'subwindow-mode)))
(defsubst xwem-face-graphics-exposures (face &optional tag-set domain)
  "Return FACE's graphics exposures in TAG-SET."
  (or (xwem-face-generic-specifier face 'graphics-exposures tag-set domain)
      (xwem-face-generic-specifier face 'graphics-exposures)
      X-False))

(defsubst xwem-face-x-gc (face &optional tag-set domain)
  "Return FACE's X-Gc in TAG-SET."
  (or (face-property face (xwem-face-generic-specifier face 'x-gc tag-set domain))
      (face-property face 'xwem-x-gc)))

;; Setters
(defun xwem-face-set-domain-face (face &optional domain)
  (cond ((null domain) (xwem-face-get-domain-face face))
        ((xwem-cl-p domain)
         (or (cdr (assq face (xwem-cl-get-sys-prop domain 'domain-faces)))
             (let ((nface (intern (symbol-name (gensym "xwem-cl-domain-face")))))
               (xwem-cl-put-sys-prop domain 'domain-faces
                 (cons (cons face (xwem-copy-face face nface))
                       (xwem-cl-get-sys-prop domain 'domain-faces)))
               nface)))
        ((xwem-win-p domain)
         (or (cdr (assq face (xwem-win-get-prop domain 'domain-faces)))
             (let ((nface (intern (symbol-name (gensym "xwem-win-domain-face")))))
               (xwem-win-put-prop domain 'domain-faces
                 (cons (cons face (xwem-copy-face face nface))
                       (xwem-win-get-prop domain 'domain-faces)))
               nface)))
        ((xwem-frame-p domain)
         (or (cdr (assq face (xwem-frame-get-prop domain 'domain-faces)))
             (let ((nface (intern (symbol-name (gensym "xwem-frame-domain-face")))))
               (xwem-frame-put-prop domain 'domain-faces
                 (cons (cons face (xwem-copy-face face nface))
                       (xwem-frame-get-prop domain 'domain-faces)))
               nface)))
        ((eq domain 'nodomain) face)
        (t (error 'xwem-error "Invalid domain" domain))))

(defun xwem-face-set-generic-specifier (face spcname value &optional tag-set domain)
  "Set FACE's SPCNAME specifier to VALUE in TAG-SET."
  (if (and tag-set (listp tag-set))
      (setq tag-set (xwem-face-sort-tag-set tag-set))
    (setq tag-set '(default)))

  ;; Adjust face according to DOMAIN, for set operation
  (setq face (or (xwem-face-set-domain-face face domain) face))

  (let* ((fn (cond ((eq spcname 'foreground) 'xwem-face-foreground-specifier)
                  ((eq spcname 'background) 'xwem-face-background-specifier)
                  ((eq spcname 'font) 'xwem-face-font-specifier)
                  ((eq spcname 'line-style) 'xwem-face-line-style-specifier)
                  ((eq spcname 'line-width) 'xwem-face-line-width-specifier)
                  ((eq spcname 'cap-style) 'xwem-face-cap-style-specifier)
                  ((eq spcname 'join-style) 'xwem-face-join-style-specifier)
                  ((eq spcname 'function) 'xwem-face-function-specifier)
                  ((eq spcname 'subwindow-mode) 'xwem-face-subwindow-mode-specifier)
                  ((eq spcname 'graphics-exposures) 'xwem-face-graphics-exposures-specifier)
                  ((eq spcname 'x-gc) 'xwem-face-x-gc-specifier)
                  (t (error 'xwem-error "Invalid SPCNAME" spcname))))
        (spec (funcall fn face))
        xgc)

    (if (specifierp spec)
        ;; Update specifier
        (add-spec-list-to-specifier spec `((global ,(cons tag-set value)))
                                    'remove-tag-set-prepend)

      ;; Or update face directly (non xwem face)
      (cond ((eq spcname 'foreground)
             (set-face-foreground face value))
            ((eq spcname 'background)
             (set-face-background face value))
            ((eq spcname 'font)
             (set-face-font face value))))

    ;; Update X Gc if needed
    (when (and (not (eq spcname 'x-gc))
               (setq xgc (xwem-face-x-gc face tag-set 'nodomain)))
      (cond ((eq spcname 'foreground)
             (setq spcname :foreground
                   value (XAllocColor (xwem-dpy) (XDefaultColormap (xwem-dpy))
                                      (xwem-make-color value))))
            ((eq spcname 'background)
             (setq spcname :background
                   value (XAllocColor (xwem-dpy) (XDefaultColormap (xwem-dpy))
                                      (xwem-make-color value))))
            ((eq spcname 'font)
             (setq spcname :font
                   value (X-Font-get (xwem-dpy) value)))
            (t (setq spcname (intern (concat ":" (symbol-name spcname))))))
      (XChangeGC (xwem-dpy) xgc spcname value))))

(defsubst xwem-set-face-foreground (face new-fg &optional tag-set domain)
  "Set FACE's foreground color to NEW-FG in TAG-SET."
  (xwem-face-set-generic-specifier face 'foreground new-fg tag-set domain))
(defsubst xwem-set-face-background (face new-bg &optional tag-set domain)
  "Set FACE's background color to NEW-FG in TAG-SET."
  (xwem-face-set-generic-specifier face 'background new-bg tag-set domain))
(defsubst xwem-set-face-font (face new-font &optional tag-set domain)
  "Set FACE's font to NEW-FG in TAG-SET."
  (xwem-face-set-generic-specifier face 'font new-font tag-set domain))
(defsubst xwem-set-face-line-style (face new-ls &optional tag-set domain)
  "Set FACE's line style to NEW-FG in TAG-SET."
  (xwem-face-set-generic-specifier face 'line-style new-ls tag-set domain))
(defsubst xwem-set-face-line-width (face new-lw &optional tag-set domain)
  "Set FACE's line width to NEW-FG in TAG-SET."
  (xwem-face-set-generic-specifier face 'line-width new-lw tag-set domain))
(defsubst xwem-set-face-cap-style (face new-cs &optional tag-set domain)
  "Set FACE's cap style to NEW-FG in TAG-SET."
  (xwem-face-set-generic-specifier face 'cap-style new-cs tag-set domain))
(defsubst xwem-set-face-join-style (face new-js &optional tag-set domain)
  "Set FACE's join style to NEW-FG in TAG-SET."
  (xwem-face-set-generic-specifier face 'join-style new-js tag-set domain))
(defsubst xwem-set-face-function (face new-fun &optional tag-set domain)
  "Set FACE's function to NEW-FG in TAG-SET."
  (xwem-face-set-generic-specifier face 'function new-fun tag-set domain))
(defsubst xwem-set-face-subwindow-mode (face new-sm &optional tag-set domain)
  "Set FACE's subwindow mode to NEW-FG in TAG-SET."
  (xwem-face-set-generic-specifier face 'subwindow-mode new-sm tag-set domain))
(defsubst xwem-set-face-graphics-exposures (face new-ge &optional tag-set domain)
  "Set FACE's graphics exposures to NEW-FG in TAG-SET."
  (xwem-face-set-generic-specifier face 'graphics-exposures new-ge tag-set domain))

;; Font attributes
(defun xwem-set-face-font-attribute (face attr-fun value &optional tag-set domain)
  (let ((font (xwem-face-font face tag-set domain)))
    (set-face-font face font)
    (funcall attr-fun face value)
    (xwem-set-face-font face (face-font-name face) tag-set domain)))
(defsubst xwem-set-face-font-family (face font-family &optional tag-set domain)
  "Set FACE's font famility to FONT-FAMILY in TAG-SET of DOMAIN."
  (xwem-set-face-font-attribute face 'custom-set-face-font-family font-family
                                tag-set domain))
(defsubst xwem-set-face-font-size (face font-size &optional tag-set domain)
  "Set FACE's font famility to FONT-FAMILY in TAG-SET of DOMAIN."
  (xwem-set-face-font-attribute face 'custom-set-face-font-size font-size
                                tag-set domain))
(defsubst xwem-set-face-bold (face bold-p &optional tag-set domain)
  "Set bold FACE's attribute to BOLD-P in TAG-SET of DOMAIN."
  (xwem-set-face-font-attribute face 'custom-set-face-bold bold-p
                                tag-set domain))
(defsubst xwem-set-face-italic (face italic-p &optional tag-set domain)
  "Set italic FACE's attribute to ITALIC-P in TAG-SET of DOMAIN."
  (xwem-set-face-font-attribute face 'custom-set-face-italic italic-p
                                tag-set domain))

(defsubst xwem-set-face-x-gc (face new-gc &optional tag-set domain)
  "Set FACE's X-Gc to NEW-FG in TAG-SET."
  ;; NOTE: Ugly thing, we can't store NEW-GC directly in specifier,
  ;; because it uses `copy-tree' to ensure to not share sequence.  So
  ;; workaround is to create unique FACE property, which will store
  ;; our NEW-GC and specifier will store property name.
  (if (specifierp (xwem-face-x-gc-specifier face))
      (let ((sym (gensym "*xwem-x-gc-")))
        (set-face-property face sym new-gc)
        (xwem-face-set-generic-specifier face 'x-gc sym tag-set domain))

    ;; No, specifier, it mean that FACE is normal emacs face, not
    ;; defined by `define-xwem-face'.
    (set-face-property face 'xwem-x-gc new-gc)))

;;;###xwem-autoload
(defun xwem-face-get-gc (face &optional tag-set domain d)
  "Return X-Gc for FACE in TAG-SET.
D    - X drawable default is `XDefaultRootWindow'."
  (setq face (or (xwem-face-get-domain-face face domain) face))

  (or (xwem-face-x-gc face tag-set 'nodomain)
      (let* ((xdpy (xwem-dpy))
             (cmap (XDefaultColormap xdpy))
             (gc (XCreateGC
                  xdpy (or (and (X-Drawable-p d) d)
                           (XDefaultRootWindow xdpy))
                  :line-style (xwem-face-line-style face tag-set 'nodomain)
                  :line-width (xwem-face-line-width face tag-set 'nodomain)
                  :cap-style (xwem-face-cap-style face tag-set 'nodomain)
                  :join-style (xwem-face-join-style face tag-set 'nodomain)
                  :function (xwem-face-function face tag-set 'nodomain)
                  :subwindow-mode (xwem-face-subwindow-mode face tag-set 'nodomain)
                  :graphics-exposures (xwem-face-graphics-exposures face tag-set 'nodomain)
                  :foreground (let ((fc (xwem-face-foreground face tag-set 'nodomain)))
                                (if (stringp fc)
                                    (XAllocColor xdpy cmap (xwem-make-color fc))
                                  fc))
                  :background (let ((bc (xwem-face-background face tag-set 'nodomain)))
                                (if (stringp bc)
                                    (XAllocColor xdpy cmap (xwem-make-color bc))
                                  bc))
                  :font (X-Font-get xdpy (xwem-face-font face tag-set 'nodomain)))))
        (xwem-set-face-x-gc face gc tag-set 'nodomain)
        gc)))

(put 'xwem-face-get-gc 'lisp-indent-function 1)


(provide 'xwem-faces)

;;; xwem-faces.el ends here
