;;; xwem-manage.el --- Manage stuff for xwem.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Richard Klinda <ignotus@hixsplit.hu>
;; Created: 21 Mar 2003
;; Keywords: xlib, xwem
;; Time-stamp: <24/11/2008 19:57:26 lg@h1.lan>

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

;; Manage database.  Manage database is list of manda entries, which
;; are used to decide how to manage certain client.  Every manda entry
;; has methods to operate on client.

;;; Customization:

;; Only one customisable variable is `xwem-manage-list' is a list
;; where each element is a list in form:

;;   \(MANAGE-TYPE CLIENT-PLIST QUALIFIER\)

;; Configuration looks like this:

;;    (setq xwem-manage-list
;;          '((fullscreen (ignore-has-input-p t fs-real-size t
;;                         x-border-width 2 x-border-color "brown4"
;;                         xwem-focus-mode follow-mouse)
;;                        (application "rdesktop"))
;;            (rooter (dummy-client-p t)
;;                    (or (application "xclock")
;;                        (application "gkrellm")
;;                        (application "gdesklets")
;;                        (application "gdeskcal")))
;;            ))

;;; Code

(require 'xwem-load)

;;;; Variables
(defcustom xwem-manage-default-expectance-expire-timeout 30
  "*Default expire timeout for expectance entries."
  :type 'number
  :group 'xwem)

;;;###autoload
(defcustom xwem-manage-default-properties
  '(reguard-x-border-width t)
  "*Default managing properties.
These properties are always set in any managing model.
Supported properties are:

  `reguard-x-border-width' - Reguard border width.
  `win-support'  - Managing model uses window operations.
."
  :type '(repeat (list :inline t
                       (symbol :tag "property")
                       (sexp :tag "value")))
  :group 'xwem)

;;;###autoload
(defcustom xwem-manage-list nil
  "List where each element in form:

\(MANAGE-TYPE CLIENT-PLIST QUALIFIER\)

MANAGE-TYPE is a symbol.

CLIENT-PLIST is a list of client properties to set when client manages
and unset when client changes manage type.  In core supported
properties are:

  `noselect' - Non-nil mean client can't be selected, usefull for
               `rooter' clients.

  `no-minib-overlap' - Non-nil to not overlap xwem minibuffer, usefull
                       for `fullscreen' clients.

  `xwem-icon-name'   - Icon to use for this client.

  `xwem-focus-mode'  - Specifies client's focus mode.

  `xwem-tab-format'  - Format to use in tabber.

  `xwem-tab-face'    - Face to use in tabber.

MATCH-TYPE is a list of match entries, where each entry TODO:
describe me."
  :type '(repeat (list symbol sexp sexp))
  :group 'xwem)

;;;###autoload
(defcustom xwem-applications-alist
  '(("inkscape" (and (class-inst "^inkscape$")
                   (class-name "^Inkscape$$")))
    ("thunar" (and (class-inst "^thunar$")
                   (class-name "^Thunar$")))
    ("lastfm" (and (class-inst "^lfmplayer$")
                   (class-name "^Lfmplayer$")))
    ("xemacs" (class-name "^Emacs$"))
    ("xterm" (and (class-inst "^xterm$")
                  (class-name "^XTerm$")))
    ("display" (and (class-inst "^display$")
                    (class-name "^[dD]isplay$")))
    ("xzgv" (and (class-inst "^xzgv$")
                 (class-name "^Xzgv$")))
    ("xv" (and (class-inst "^xv$")
               (class-name "^XV")))
    ("xcalc" (and (class-inst "^xcalc$")
                  (class-name "^XCalc$")))
    ("xclock" (and (class-inst "^xclock$")
                   (class-name "^[Xx][cC]lock$")))
    ("xload" (and (class-inst "^xload$")
                  (class-name "^XLoad$")))
    ("xkeycaps" (and (class-inst "^xkeycaps$")
                     (class-name "^XKeyCaps$")))
    ("gimp_startup" (and (class-inst "^gimp_startup$")
                         (class-name "^Gimp$")))
    ("gv" (and (class-inst "^gv$")
               (class-name "^GV$")))
    ("ghostview" (and (class-inst "^ghostview$")
                      (class-name "^Ghostview$")))
    ("xfd" (and (class-inst "^xfd$")
                (class-name "^Xfd$")))
    ("xfontsel" (and (class-inst "^xfontsel$")
                     (class-name "^XFontSel$")))
    ("gnumeric" (and (class-inst "^gnumeric$")
                     (class-name "^Gnumeric$")))
    ("ethereal" (and (class-inst "^ethereal$")
                     (class-name "^Ethereal$")))

    ("xsensors" (and (class-inst "^xsensors$")
                     (class-name "^Xsensors$")))
    ("gkrellm" (and (class-inst "^[gG]krellm")
                    (class-name "^Gkrellm$")))

    ;; Gdesklets stuff
    ("gdesklets" (and (class-inst "^gDesklets$")
                      (class-name "^Gdesklets$")))
    ("gdeskcal" (and (class-inst "^gdeskcal$")
                     (class-name "^Gdeskcal$")))

    ("links" (and (class-inst "^Links$")
                  (class-name "^Links$")))
    ("licq" (and (class-inst "^licq$")
                 (class-name "^Licq$")))

    ("pinentry" (and (class-inst "^pinentry")
                     (class-name "^Pinentry")))

    ("glade" (and (class-inst "^glade")
                  (class-name "^Glade")))

    ;; CLASS-NAME only
    ("mozilla" (or (class-name "^[mM]ozilla")
                   (class-inst "^[mM]ozilla")))
    ("openoffice" (class-name "^OpenOffice"))
    ("xmms" (class-name "^[Xx]mms$"))
    ("xine" (class-name "^xine$"))
    ("mplayer" (class-name "^MPlayer$"))
    ("xchat" (or (class-name "^X-Chat$")
                 (class-name "^Xchat$")))
    ("gimp" (class-name "^Gimp$"))
    ("ddd" (class-name "^Ddd$"))
    ("firefox" (class-name "^Firefox"))
    ("thunderbird" (class-name "^Thunderbird"))
    ("opera" (class-name "^Opera$"))
    ("xpdf" (class-name "^Xpdf$"))
    ("acroread" (class-name "^AcroRead$"))
    ("ftp" (class-name "^gFTP$"))

    ;; NAME only
    ("gnuplot" (name "^Gnuplot$"))
    ("xchm" (name "^xCHM"))

    ;; generalized applications
    ("PdfViewer" (or (application "xpd")
                     (application "acroread")))
    ("WebBrowser" (or (application "firefox")
                      (application "opera")
                      (application "mozilla")
                      (application "links")))
    ("PictureViewer" (or (application "display")
                         (application "xzgv")
                         (application "xv")))
    ("DocumentViewer" (or (application "xpdf")
                          (application "acroread")
                          (application "gv")
                          (application "xchm")))
    )
  "alist of known applications.
CAR is appllication name, CDR is qualifier.

Use `xwem-appcollect' to create `xwem-applications-alist'."
  :type '(cons string sexp)
  :group 'xwem)

(defvar xwem-manage-internal-list nil
  "Internal manage list in `xwem-manage-list' format.")

;;;###autoload
(defvar xwem-manage-expectances nil
  "List of expectances in `xwem-manage-list' format.
The difference from `xwem-manage-list' is that, when matching occurs
in `xwem-manage-expectances', matched entry removed from
`xwem-manage-expectances' list.")

;;; Internal variables


;;; Qualification

(defvar xwem-client-qualifiers nil "List of qualifiers.")

(defmacro define-xwem-qualifier (name doc predicate)
  "Define new qualifier named by NAME.
DOC - Documentation for matcher.
PREDICATE - Function to call with two arguments - CL and PARAM."
  `(setq xwem-client-qualifiers
         (put-alist (quote ,name) (list ,doc ,predicate)
                    xwem-client-qualifiers)))

;;;###xwem-autoload
(defun xwem-cl-match-p (cl qualifier)
  "Check whether CL matches QUALIFIER.
Here is QUALIFIER format in EBNF:

  QUALIFIER ::= <SIMPLE-QUALIFIER> | ( {or|and} <QUALIFIER>* )
  SIMPLE-QUALIFIER ::= ( <TYPE> <PARAM> )
  TYPE ::= one of defined in `xwem-client-qualifiers'
  PARAM := any data

Here is special TYPE exists - `override-redirect'. If
`override-redirect' is used, also match client with override-redirect
attribute, otherwise clients with override-redirect are skiped, even
if others specs matches."
  (let ((case-fold-search nil)          ; case sensivity searching
        (ao-type nil))                  ; and-or matching

    ;; Setup global matching, default to 'and
    (when (memq (car qualifier) '(or and))
      (setq ao-type (car qualifier)
            qualifier (cdr qualifier)))

    (if (not ao-type)
        ;; Check simple qualifier for matching
        (let* ((type (car qualifier))
               (param (cadr qualifier))
               (dq (assq type xwem-client-qualifiers)))
          (unless dq
            (error 'xwem-error (format "Unknown qualifier type: %S" type)))
          (funcall (third dq) cl param))

      (let (ao-res or)
        ;; Scan QUALIFIER for matching
        (while qualifier
          (setq ao-res (cond ((eq (car (car qualifier)) 'override-redirect)
                              (and (xwem-cl-p cl)
                                   (X-Attr-override-redirect
                                    (xwem-cl-initial-xattrs cl))
                                   (setq or t)))
                             (t (xwem-cl-match-p cl (car qualifier)))))
          (if (or (and (eq ao-type 'and) (null ao-res))
                  (and (eq ao-type 'or) ao-res))
              ;; Break conditions, not-match or already matches
              (setq qualifier nil)
            ;; Continue traversing
            (setq qualifier (cdr qualifier))))

        (if (and (xwem-cl-p cl)
                 (X-Attr-override-redirect (xwem-cl-initial-xattrs cl)))
            (and or ao-res)
          ao-res)))))

(define-xwem-qualifier not
  "Return non-nil if PARAM is false."
  #'(lambda (cl param)
      (not (xwem-cl-match-p cl param))))

(define-xwem-qualifier class-inst
  "Return non-nil if PARAM matches CL's class instance."
  #'(lambda (cl param)
      (and (xwem-cl-p cl)
           (or (null param)
               (string-match
                param (or (car (xwem-hints-wm-class (xwem-cl-hints cl)))
                          ""))))))

(define-xwem-qualifier class-name
  "Return non-nil if PARAM matches CL's class name."
  #'(lambda (cl param)
      (and (xwem-cl-p cl)
           (or (null param)
               (string-match
                param (or (cdr (xwem-hints-wm-class (xwem-cl-hints cl)))
                          ""))))))

(define-xwem-qualifier role
  "Return non-nil if PARAM matches CL's WM_WINDOW_ROLE property."
  #'(lambda (cl param)
      (and (xwem-cl-p cl)
           (or (null param)
               (string-match
                param (or (xwem-cl-wm-role cl) ""))))))

(define-xwem-qualifier name
  "Return non-nil if PARAM matches CL's WM_NAME."
  #'(lambda (cl param)
      (and (xwem-cl-p cl)
           (or (null param)
               (string-match
                param (or (xwem-hints-wm-name (xwem-cl-hints cl)) ""))))))

(define-xwem-qualifier command
  "Return non-nil if PARAM matches CL's COMMAND."
  #'(lambda (cl param)
      (and (xwem-cl-p cl)
           (or (null param)
               (string-match param (xwem-client-command cl))))))

(define-xwem-qualifier property
  "Return non-nil if CL's property PARAM is set.
PARAM could be either symbol or cons cell.
If PARAM is symbol then return non-nil if PARAM property is set.
If PARAM is cons cell then return non-nil if property named by PARAM's
car is set and `equal' to PARAM's cdr."
  #'(lambda (cl param)
      (and (xwem-cl-p cl)
           (cond ((symbolp param)
                  (xwem-client-property cl param))
                 ((and (consp param)
                       (symbolp (car param)))
                  (equal (xwem-client-property cl (car param))
                         (cdr param)))))))

(define-xwem-qualifier application
  "Return non-nil if CL's application is PARAM.
First, lookup in `xwem-applications-alist', then look at class instance."
  #'(lambda (cl param)
      (and (xwem-cl-p cl)
           (let ((aspec (cadr (assoc param xwem-applications-alist))))
             (if aspec
                 (xwem-cl-match-p cl aspec)
               (string= (car (xwem-cl-wm-class cl)) param))))))

(define-xwem-qualifier predicate
  "Return non-nil if PARAM predicate function return non-nil.
CL is argument to PARAM predicate."
  #'(lambda (cl param)
      (funcall param cl)))

(define-xwem-qualifier t
  "Always return non-nil."
  #'(lambda (cl param) t))

(define-xwem-qualifier nil
  "Always return nil."
  #'(lambda (cl param) nil))

(defmacro define-xwem-buffer-qualifier (name doc &rest forms)
  `(define-xwem-qualifier ,name ,doc
     #'(lambda (cl param)
         (let* ((fr (xwem-misc-find-emacs-frame cl))
                (buf (and (frame-live-p fr)
                          (window-buffer (frame-selected-window fr)))))
           (when (bufferp buf)
               ,@forms)))))

(define-xwem-buffer-qualifier buffer-major-mode
  "Return non-nil if CL is Emacs frame and its buffer's major-mode is PARAM."
  (eq param (symbol-value-in-buffer 'major-mode buf)))

(define-xwem-buffer-qualifier buffer-name
  "Return non-nil if CL is Emacs frame and its buffer's name matches PARAM."
  (string-match param (buffer-name buf)))

(define-xwem-buffer-qualifier buffer-filename
  "Return non-nil if CL is Emacs frame and its buffer's filename matches PARAM."
  (and (buffer-file-name buf)
       (string-match param (buffer-file-name buf))))

;;;###xwem-autoload
(defun* xwem-manda-find-match-1 (cl manage-list &optional (extractor 'caddr))
  "Search for CL match in MANAGE-LIST.
EXTRACTOR is where to obtain qualifier from MANAGE-LIST elements,
default is 'caddr."
  (find cl manage-list
        :test #'(lambda (c mc)
                  (xwem-cl-match-p c (funcall extractor mc)))))

(defun xwem-manda-find-expectance (cl)
  "Search in `xwem-manage-expectances' to match CL.
If match occurs, matching entry removed from `xwem-manage-expectances'."
  (let ((ee (find cl xwem-manage-expectances
                  :test #'(lambda (c e)
                            (xwem-cl-match-p c (caddr e))))))
    (setq xwem-manage-expectances
          (delq ee xwem-manage-expectances))
    ee))

;;;###xwem-autoload
(defun xwem-cl-was-expected (cl new)
  "Mark/unmark CL as it was expected according to NEW value.
If NEW is non-nil mark CL as was expected.
If NEW is nil then unmark."
  (xwem-cl-put-sys-prop cl 'cl-was-expected new))

;;;###xwem-autoload
(defun xwem-cl-was-expected-p (cl)
  "Return non-nil if CL was expected for managing."
  (xwem-cl-get-sys-prop cl 'cl-was-expected))

(defun xwem-manda-find-match (cl)
  "Find match for CL in manage database.
Search `xwem-manage-internal-list' with 'override-manage-list non-nil
property, then search for match in `xwem-manage-list' and
`xwem-manage-internal-list' respectively.
`xwem-manda-find-match' also checks expectances.  Only expectances of
same manage type as normal match is used."
  (let ((mspec (or (xwem-manda-find-match-1
                    cl (delq nil (mapcar #'(lambda (mm)
                                             (and (xwem-manage-property
                                                   (car mm) 'override-manage-list)
                                                  mm))
                                         xwem-manage-internal-list)))
                   (xwem-manda-find-match-1 cl xwem-manage-list)
                   (xwem-manda-find-match-1 cl xwem-manage-internal-list)))
        (expt-spec (xwem-manda-find-expectance cl)))
    ;; Check that EXPT-SPEC is the same manage type as MSPEC
    (if (and expt-spec (car expt-spec))
        ;; Mark CL as it was expected
        (progn
          (xwem-cl-was-expected cl t)
          expt-spec)

      ;; Merge default client properties and properties specified by
      ;; user into single plist, default properties are overwritten
      ;; Also merge in expectance properties
      (let ((mplist (xwem-misc-merge-plists
                     (cadr (assq (car mspec) xwem-manage-internal-list))
                     (cadr mspec)
                     (and mspec expt-spec (null (car expt-spec))
                          (cadr expt-spec)))))
        ;; Mark CL as it was expected
        (xwem-cl-was-expected cl t)
        (list (car mspec) mplist (caddr mspec))))))

(defun xwem-manda-del-expectance (expt)
  "Remove EXPT from `xwem-manage-expectances' list."
  (setq xwem-manage-expectances
        (delq expt xwem-manage-expectances)))

;;;###xwem-autoload
(defun xwem-manda-add-expectance (expectance &optional expire-timeout)
  "Install new EXPECTANCE in `xwem-manage-expectances' list.
EXPIRE-TIMEOUT specifies time-to-live for new entry in seconds
\(default is `xwem-manage-default-expectance-expire-timeout'\)."
  (push expectance xwem-manage-expectances)

  ;; Install expectance timeout handler
  (start-itimer "xwem-expectance" 'xwem-manda-del-expectance
                (or expire-timeout xwem-manage-default-expectance-expire-timeout)
                nil nil t expectance))

;;;###xwem-autoload
(defun xwem-manage-property (manage-type prop)
  "For MANAGE-TYPE, return manage property PROP."
  (plist-get (get manage-type 'xwem-manage-properties) prop))

;;;###xwem-autoload
(defun xwem-manage-rem-property (manage-type prop)
  "For MANAGE-TYPE, remove property PROP."
  (put manage-type 'xwem-manage-properties
       (plist-remprop (get manage-type 'xwem-manage-properties) prop)))

;;;###xwem-autoload
(defun xwem-manage-set-property (manage-type prop val)
  "For MANAGE-TYPE, set manage property PROP to VAL."
  (if val
      (put manage-type 'xwem-manage-properties
           (plist-put (get manage-type 'xwem-manage-properties) prop val))
    (xwem-manage-rem-property manage-type prop)))

;;;###xwem-autoload
(defun* define-xwem-manage-model-1
  (manage-name docstring &key manage-properties cl-properties qualifier append
               manage-method activate-method deactivate-method refit-method
               iconify-method withdraw-method)
  "Define new managing model of MANAGE-NAME.
DOCSTRING is documentation for managing model.
MANAGE-PROPERTIES - Some manage properties used when managing clients
of this managing model.
CL-PROPERTIES - Client properties to import into client when client
managing using this managing model.
QUALIFIER - Client matching specification, see `xwem-cl-match-p'.
APPEND - Non-nil mean append to the end of managing models list.  By
default managing models are prepended to list."
  (put manage-name :docstring docstring)

  (add-to-list 'xwem-manage-internal-list
               (list manage-name cl-properties qualifier) append)

  ;; Set manage properties
  (setq manage-properties
        (xwem-misc-merge-plists
         xwem-manage-default-properties manage-properties))
  (while manage-properties
    (xwem-manage-set-property
     manage-name (car manage-properties) (cadr manage-properties))
    (setq manage-properties (cddr manage-properties)))

  ;; Register methods
  (when manage-method
    (put 'manage manage-name manage-method))
  (when activate-method
    (put 'activate manage-name activate-method))
  (when deactivate-method
    (put 'deactivate manage-name deactivate-method))
  (when refit-method
    (put 'refit manage-name refit-method))
  (when iconify-method
    (put 'iconify manage-name iconify-method))
  (when withdraw-method
    (put 'withdraw manage-name withdraw-method))
  )

(defmacro define-xwem-manage-model (manage-name docstring &rest args)
  "Define new managing model."
  `(funcall 'define-xwem-manage-model-1 (quote ,manage-name) ,docstring ,@args))


;;; New concept, generic functions and methods (like CLOS)
(defsubst xwem-execute-method (method-name manda-type &rest args)
  "Execute METHOD-NAME  passing ARGS.
If no method METHOD-NAME found for MANDA-TYPE, use 'default type."
  (let ((fun (get method-name manda-type)))
    (when (or fun (setq fun (get method-name 'default)))
      (apply fun args))))

(defsubst xwem-method-manage (cl)
  (xwem-execute-method 'manage (xwem-cl-manage-type cl) cl))

(defsubst xwem-method-activate (cl &optional type)
  "Activation method for CL.
For TYPE, see documentation for `xwem-activate'."
  (xwem-execute-method 'activate (xwem-cl-manage-type cl) cl type))

(defsubst xwem-method-deactivate (cl &optional type)
  (xwem-execute-method 'deactivate (xwem-cl-manage-type cl) cl type))

(defsubst xwem-method-refit (cl)
  (xwem-execute-method 'refit (xwem-cl-manage-type cl) cl))

(defsubst xwem-method-iconify (cl)
  (xwem-execute-method 'iconify (xwem-cl-manage-type cl) cl))

(defsubst xwem-method-withdraw (cl)
  (xwem-execute-method 'withdraw (xwem-cl-manage-type cl) cl))

(defsubst xwem-method-on-kill (cl)
  (xwem-execute-method 'on-kill (xwem-cl-manage-type cl) cl))

(defsubst xwem-method-on-type-change (cl &optional new-type)
  (xwem-execute-method 'on-type-change (xwem-cl-manage-type cl) cl new-type))

(defmacro define-xwem-method (method-name manda-type arg-list
                                          &optional doc-string &rest forms)
  "Define new method METHOD-NAME for MANDA-TYPE.
DOC-STRING is documentation.
FORMS - elisp forms to eval."
  (let ((sym (intern (format "xwem:-%s-%s" manda-type method-name))))
    `(eval-and-compile
       (defun ,sym ,arg-list
         ,doc-string
         ,@forms)
       (put (quote ,method-name) (quote ,manda-type) (quote ,sym)))))


(provide 'xwem-manage)

;;; xwem-manage.el ends here
