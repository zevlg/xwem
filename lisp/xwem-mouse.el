;;; xwem-mouse.el --- Mouse support for XWEM.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: 21 Mar 2003
;; Keywords: xlib, xwem
;; Time-stamp: <22/9/2007 03:19:58 lg@h1>

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
;; XWEM supports mouse as well as keyboard.

;;; Code:

(require 'xwem-load)
(require 'xwem-manage)

;;; Customisation
(defcustom xwem-popup-menu-function 'popup-menu
  "*Function used to popup menus.
It is created for case when you change default `popup-menu' function,
for example if you are using tpum.el."
  :type 'function
  :group 'xwem)

;;; Internal variables


(defun xwem-mouse-change-cursor (cursor)
  "XXX.
CURSOR - Dunno."
  (XChangeActivePointerGrab (xwem-dpy) cursor
                            (Xmask-or XM-ButtonPress XM-ButtonRelease)))

;;;###autoload
(defun xwem-mouse-grab (cursor &optional win mask)
  "Begin to grab mouse, showing CURSOR in WIN using event mask MASK.
Default WIN is root window.
Default MASK is capturing ButtonPress or ButtonRelease events."
  ;; TODO: install custom events handlers?
  (XGrabPointer (xwem-dpy)
                (or win (xwem-rootwin))
                (or mask (Xmask-or XM-ButtonPress XM-ButtonRelease))
                cursor))

;;;###autoload
(defun xwem-mouse-ungrab (&optional flush-p)
  "Stop grabing mouse.
If FLUSH-P is non-nil, mouse is ungrabbed imediately."
  (XAllowEvents (xwem-dpy) X-ReplayPointer)
  (XUngrabPointer (xwem-dpy))
  (when flush-p
    ;; XX flush data to server and wait a little
    (XFlush (xwem-dpy))
    (sit-for 0)))

;;; Menus
;;;###autoload
(defun xwem-popup-menu (menu)
  "Popup MENU at pointer position."
  (xwem-mouse-ungrab t)
  (funcall xwem-popup-menu-function menu))

(defvar xwem-applications-submenu
  '("Applications"
    ("XEmacs"
     ["New frame" (make-frame nil (default-x-device))]
     ["*scratch* frame" (with-selected-frame
                            (make-frame nil (default-x-device))
                          (switch-to-buffer "*scratch*"))])
    ("XTerm"
     ["Default xterm" (xwem-launch-xterm 1)]
     ["2 xterm" (xwem-launch-xterm 2)]
     ["3 xterm" (xwem-launch-xterm 3)]
     ["4 xterm" (xwem-launch-xterm 4)])
    ["Mozilla" (xwem-execute-program "mozilla")]
    "--"
    ["GhostView" (xwem-execute-program "gv")]
    ["xfontsel" (xwem-execute-program "xfontsel")]
    ["Lupe" (xwem-launch-lupe nil)]
    )
  "Submenu with applications.")

;;;###xwem-autoload
(defun xwem-generate-window-menu (title &optional win)
  "Generate menu for WIN."
  (unless title
    (setq title "Window"))
  (list title
        (vector "Vertical Split" `(xwem-window-split-vertically nil ,win))
        (vector "Horizontal Split" `(xwem-window-split-horizontally nil ,win))
        (vector "Delete Window" `(xwem-window-delete ,win))
        (vector "Delete Others" `(xwem-window-delete-others ,win))
        (vector "Balance" `(xwem-balance-windows ,win))))

(defun xwem-generate-iconified-cl-menu (title &optional max-mwidth)
  "Generate menu for iconified clients with TITLE.
MAX-MWIDTH specifies maximum menu width."
  (list (xwem-misc-fixup-string title max-mwidth)
        :filter
        `(lambda (not-used)
           (delq nil
                 (mapcar #'(lambda (cl)
                             (when (xwem-cl-iconified-p cl)
                               (vector (xwem-misc-fixup-string
                                        (xwem-client-name cl) ,max-mwidth)
                                       `(xwem-select-client ,cl)
                                       :active (xwem-non-dummy-client-p cl))))
                         xwem-clients)))))

(defun xwem-generate-applications-cl-menu (title &optional max-mwidth)
  "Generate menu for applications."
  (list (xwem-misc-fixup-string title max-mwidth)
        :filter
        `(lambda (not-used)
           (mapcar
            #'(lambda (app-spec)
                (list (xwem-misc-fixup-string (car app-spec) ,max-mwidth)
                      :filter
                      `(lambda (not-used)
                         (delq nil
                               (mapcar
                                #'(lambda (cl)
                                    (when (xwem-cl-match-p
                                           cl (cadr (quote ,app-spec)))
                                      (vector (xwem-misc-fixup-string
                                               (xwem-client-name cl) ,,max-mwidth)
                                              `(xwem-select-client ,cl)
                                              :active
                                              (xwem-non-dummy-client-p cl))))
                                xwem-clients)))))
            xwem-applications-alist))))

;;;###xwem-autoload
(defun xwem-generate-clients-menu (title &optional max-mwidth)
  "Generate clients menu.
TITLE is menu title.
Optional MAX-MWIDTH argument specifies maximum width for menu items,
default is 42."
  (unless max-mwidth
    (setq max-mwidth 42))

  (let (malist)
    (mapc #'(lambda (cl)
              (let ((kv (assq (xwem-cl-manage-type cl) malist)))
                (if kv
                    (setcdr kv (cons cl (cdr kv)))
                  (setq malist (put-alist (xwem-cl-manage-type cl)
                                          (list cl) malist)))))
          xwem-clients)

    (nconc (list (xwem-misc-fixup-string title max-mwidth))
           (mapcar #'(lambda (mc)
                       (list (xwem-misc-fixup-string
                              (symbol-name (car mc)) max-mwidth)
                             :filter
                             `(lambda (not-used)
                                (mapcar #'(lambda (mccl)
                                            (vector (xwem-misc-fixup-string
                                                     (xwem-client-name mccl)
                                                     ,max-mwidth)
                                                    `(xwem-select-client ,mccl)
                                                    :active (xwem-non-dummy-client-p mccl)))
                                        (cdr (quote ,mc))))))
                   malist)

           ;; Iconified
           (list "==")
           (list (xwem-generate-iconified-cl-menu "Iconified" max-mwidth))

           ;; Applications
           (list "--")
           (list (xwem-generate-applications-cl-menu "Applications" max-mwidth))
           )))

(defun xwem-generate-recent-files (&optional title limit)
  "Generate recent files menu."
  (unless title (setq title "Recent Files"))
  (unless limit (setq limit 10))
  (list title
        :filter `(lambda (not-used)
                   (mapcar #'(lambda (file)
                               (vector file `(xwem-open-file ,file)))
                           (let ((files xwem-read-filename-history)
                                 (ret-files nil)
                                 (ci 0))
                             (while (and files (< ci ,limit))
                               (setq ret-files (cons (car files) ret-files)
                                     files (cdr files))
                               (incf ci))
                             (nreverse ret-files))))))

;;;###xwem-autoload
(defun xwem-generate-menu ()
  "Generate xwem menu on fly."
  (list "XWEM Menu"
        (list "Minibuffer"
              ["Hide" (xwem-iconify (xwem-minib-cl xwem-minibuffer))
               :active (xwem-cl-active-p (xwem-minib-cl xwem-minibuffer)) ]
              ["Show" (xwem-activate (xwem-minib-cl xwem-minibuffer))
               :active (not (xwem-cl-active-p (xwem-minib-cl xwem-minibuffer))) ]
              ["Restore size" (xwem-minib-rsz-resize 1)])
        (xwem-generate-window-menu "Window" (xwem-win-selected))
        "--"
        (list "Frames" :filter
              #'(lambda (not-used)
                  (nconc
                   (list (list "Operations"
                               ["New Frame" (xwem-make-frame 'desktop)]
                               ["Next" (xwem-frame-next 1)]
                               ["Previous" (xwem-frame-previous 1)]
                               ["Iconify" (xwem-frame-hide (xwem-frame-selected))]
                               ["Transpose" (xwem-transpose-frames 1)]
                               "--"
                               ["Destroy" (xwem-frame-destroy (xwem-frame-selected))])
                         (list "Side-by-side"
                               ["Vertical" (xwem-frame-sbs-vert-split 1)]
                               ["Horizontal" (xwem-frame-sbs-hor-split 1)])
                         "--"
                         ["Show Root" (xwem-frame-showroot)]
                         ["Lower" (xwem-frame-lower (xwem-frame-selected))]
                         ["Raise" (xwem-frame-raise (xwem-frame-selected))]
                         )
                   (list "==")
                   (mapcar #'(lambda (el)
                               (let ((fn (xwem-frame-num el)))
                                 (vector
                                  (concat "Frame " (int-to-string fn) ": "
                                          (xwem-frame-name el))
                                  `(xwem-frame-switch-nth ,fn))))
                           (xwem-frames-list)))))

        (list "Clients" :filter
              #'(lambda (not-used)
                  (nconc
                   (cdr (xwem-generate-clients-menu nil))
                   (list "==")
                   (and (xwem-cl-selected)
                        (cdr (xwem-generate-cl-menu (xwem-cl-selected) 32))))))
        "--"
        xwem-applications-submenu

        (xwem-generate-recent-files)
        ;; XXX - it is just demo of popup menus
        ))

;;;###autoload(autoload 'xwem-popup-clients-menu "xwem-mouse" nil t)
(define-xwem-command xwem-popup-clients-menu ()
  "Popup clients menu."
  (xwem-interactive)

  (xwem-popup-menu (xwem-generate-clients-menu "XWEM Clients")))

;;;###xwem-autoload
(defun xwem-generate-cl-menu (cl &optional maxnlen)
  "Generate menu for CL.
MAXNLEN - maximum menu width in characters."
  (unless maxnlen
    (setq maxnlen 20))

  (delq nil
	(list (let ((name (xwem-client-name cl)))
		(when (> (length name) maxnlen)
		  (setq name (concat (substring name 0 (- maxnlen 2)) "..")))
		name)
	      "--"
	      (vector "Focus client" `(xwem-cl-pop-to-client ,cl))
	      (vector "Info" `(xwem-client-info ,cl))
	      (vector "Iconify" `(xwem-client-iconify ,cl))
	      "--:singleDashedLine"
	      (vector "Transpose ==>" `(xwem-cl-transpose ,cl))
	      (vector "Transpose <==" `(xwem-cl-transpose ,cl '(4)))
	      "--:singleDashedLine"
	      (vector "Mark client" `(if (xwem-cl-marked-p ,cl)
					 (xwem-client-unset-mark ,cl)
				       (xwem-client-set-mark ,cl))
		      :style 'toggle :selected `(xwem-cl-marked-p ,cl))
	      (when (and xwem-cl-mark-ring
			 (not (eq (xwem-cl-frame (car xwem-cl-mark-ring))
				  (if (and (boundp 'xwem-tabber-click-frame)
					   (xwem-frame-p xwem-tabber-click-frame))
				      xwem-tabber-click-frame
				    (xwem-frame-selected)))))
		(vector "Attach"
			`(xwem-win-set-cl ,(xwem-frame-selwin
					    (if (and (boundp 'xwem-tabber-click-frame)
						     (xwem-frame-p xwem-tabber-click-frame))
						xwem-tabber-click-frame
					      (xwem-frame-selected)))
					  ,(car xwem-cl-mark-ring))))
	      (when (and xwem-cl-mark-ring
			 (not (eq (xwem-cl-frame (car xwem-cl-mark-ring))
				  (if (and (boundp 'xwem-tabber-click-frame)
					   (xwem-frame-p xwem-tabber-click-frame))
				      xwem-tabber-click-frame
				    (xwem-frame-selected)))))
		(vector "Attach (unmark)"
			`(progn
			   (xwem-win-set-cl ,(xwem-frame-selwin
					      (if (and (boundp 'xwem-tabber-click-frame)
						       (xwem-frame-p xwem-tabber-click-frame))
						  xwem-tabber-click-frame
						(xwem-frame-selected)))
					    ,(car xwem-cl-mark-ring))
			   (xwem-client-unset-mark ,(car xwem-cl-mark-ring)))))
	      "--:singleDashedLine"
	      (vector "Run Copy" `(xwem-client-run-copy nil ,cl))
	      (vector "Run Copy other win" `(xwem-client-run-copy-other-win nil ,cl))
	      (vector "Run Copy other frame" `(xwem-client-run-copy-other-frame nil ,cl))
	      "--:doubleLine"
	      (when (XWMProtocol-set-p
		     (xwem-dpy) (xwem-hints-wm-protocols (xwem-cl-hints cl))
                     "WM_DELETE_WINDOW")
		(vector "Close" `(xwem-client-kill ,cl)))
	      (vector "Kill" `(xwem-client-kill ,cl '(4))))))

;;;###autoload(autoload 'xwem-popup-auto-menu "xwem-mouse" nil t)
(define-xwem-command xwem-popup-auto-menu (arg)
  "Popup menu generated by `xwem-generate-menu'.
ARG - Not used yet."
  (xwem-interactive "_P")

  (xwem-popup-menu (xwem-generate-menu)))


(provide 'xwem-mouse)

;;; xwem-mouse.el ends here
