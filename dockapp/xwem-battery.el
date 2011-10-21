;;; xwem-battery.el --- Dockapp APM battery monitor for XWEM.

;; Copyright (C) 2004-2009 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;; Created: Thu Sep  2 01:14:36 GMT 2004
;; Keywords: xwem
;; Time-stamp: <2/10/2009 23:51:56 00000000@macbook-air-0000-0000.local>

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

;; APM battery status monitor dockapp for use under XWEM.

;; It looks like:

;;   normal    charching
;;
;;    ****        ****  
;;  ********    *******/
;;  *      *    *     //
;;  *      *    *    //*
;;  *      *    *   // *
;;  ********    ***//***
;;  *------*    *-//---*
;;  *------*    *//----*
;;  *------*    //-----*
;;   ******      ****** 

;; To start using it, just add:

;;   (load-module <path-to-apm-battery-ell>)
;;   (add-hook 'xwem-after-init-hook 'xwem-battery)

;; to your xwemrc.el.

;;; Code:

(eval-when-compile
  (require 'cl)
  (autoload 'apm-battery "battery.ell" "Return current battery status.")
  )

(require 'xlib-xlib)
(require 'xlib-xshape)

(require 'xwem-load)

;;; Customisation
(defgroup xwem-batt nil
  "Group to customise APM battery monitor."
  :prefix "xwem-batt-"
  :group 'xwem)

(defcustom xwem-batt-update-interval 5
  "*Apm battery dockapp update interval in seconds."
  :type 'number
  :group 'xwem-batt)

(defcustom xwem-batt-height 24
  "*Height of apm battery dockapp in pixels."
  :type 'number
  :group 'xwem-batt)

(defcustom xwem-batt-width 10
  "*Width of apm battery dockapp in pixels."
  :type 'number
  :group 'xwem-batt)

(defcustom xwem-batt-percentage-colors
  '((10 . "red3")
    (20 . "red2")
    (40 . "orange")
    (60 . "yellow2")
    (80 . "green3")
    (100 . "green2"))
  "*Table to translate percentage to color."
  :type '(repeat (cons (number :tag "Percents")
                       (color :tag "Color")))
  :group 'xwem-batt)

(defcustom xwem-batt-ac-line-width 4
  "*Width of ac-line."
  :type 'number
  :group 'xwem-batt)

(defcustom xwem-batt-ac-line-color "blue"
  "*Color used to display ac-line."
  :type 'color
  :group 'xwem-batt)

;;; Internal variables


(defmacro xwem-batt-itimer (win)
  `(X-Win-get-prop ,win 'xwem-batt-itimer))
(defsetf xwem-batt-itimer (win) (itimer)
  `(X-Win-put-prop ,win 'xwem-batt-itimer ,itimer))
(defmacro xwem-batt-xmask (win)
  `(X-Win-get-prop ,win 'xwem-batt-xmask))
(defsetf xwem-batt-xmask (win) (xmask)
  `(X-Win-put-prop ,win 'xwem-batt-xmask ,xmask))
(defmacro xwem-batt-pixmap (win)
  `(X-Win-get-prop ,win 'xwem-batt-pixmap))
(defsetf xwem-batt-pixmap (win) (pixmap)
  `(X-Win-put-prop ,win 'xwem-batt-pixmap ,pixmap))

(defmacro xwem-batt-old-ac-line-p (win)
  `(X-Win-get-prop ,win 'old-ac-line-p))
(defsetf xwem-batt-old-ac-line-p (win) (oalp)
  `(X-Win-put-prop ,win 'old-ac-line-p ,oalp))
(defmacro xwem-batt-old-dheight (win)
  `(X-Win-get-prop ,win 'old-dheight))
(defsetf xwem-batt-old-dheight (win) (dheight)
  `(X-Win-put-prop ,win 'old-dheight ,dheight))

(defun xwem-batt-init (xdpy)
  "On display XDPY create and return APM battery monitor window."
  (let (xwin xmask xpix)
    (setq xwin (XCreateWindow xdpy (XDefaultRootWindow xdpy)
			      0 0 xwem-batt-width xwem-batt-height 0
			      nil nil nil
			      :override-redirect t
                              :backing-store X-WhenMapped))

    ;; Create mask pixmap for xwin
    (setq xmask (XCreatePixmap xdpy xwin 1 xwem-batt-width xwem-batt-height))
    ;; XXX Draw mask
    (XFillRectangle xdpy xmask xwem-misc-mask-bgc
                    0 0 xwem-batt-width xwem-batt-height)
    (XFillRectangle xdpy xmask xwem-misc-mask-fgc
                    0 2 xwem-batt-width (- xwem-batt-height 3))
    (XDrawSegments xdpy xmask xwem-misc-mask-fgc
		   (list (cons (cons 3 0) (cons (- xwem-batt-width 4) 0))
			 (cons (cons 1 1) (cons (- xwem-batt-width 2) 1))
			 (cons (cons 1 (- xwem-batt-height 1))
			       (cons (- xwem-batt-width 2)
                                     (- xwem-batt-height 1)))))

    ;; Set mask
    (X-XShapeMask xdpy xwin X-XShape-Bounding X-XShapeSet 0 0 xmask)
    (setf (xwem-batt-xmask xwin) xmask)

    ;; Create pixmap for storer
    (setq xpix (XCreatePixmap xdpy xwin (XDefaultDepth xdpy)
                              xwem-batt-width xwem-batt-height))
    (setf (xwem-batt-pixmap xwin) xpix)
    (xwem-batt-win-update xwin t)

    xwin))

(defface xwem-batt-tmp-face
  `((t (:foreground "black")))
  "Temporary face used by apm battery dockapp.")

(define-xwem-deferred xwem-batt-apply-pixmap (xwin)
  "Apply pixmap storer to XWIN."
  (XCopyArea (X-Win-dpy xwin) (xwem-batt-pixmap xwin) xwin
             (XDefaultGC (X-Win-dpy xwin))
             0 0 xwem-batt-width xwem-batt-height 0 0))

(defun xwem-batt-win-update (xwin &optional force)
  "Update contents of XWIN to reflect current APM battery state."
  (let* ((xdpy (X-Win-dpy xwin))
         (xpix (xwem-batt-pixmap xwin))
         (as (apm-battery))
         (ac-line-p (car as))
         (cperc (caddr as))
         (perc-cols xwem-batt-percentage-colors)
         dheight)

    (when (> cperc 100)
      (setq  cperc 100))

    ;; Calculate displayed height
    (setq dheight (round (/ (* cperc (- xwem-batt-height 5)) 100.0)))

    (when (or force (not (eq dheight (xwem-batt-old-dheight xwin)))
              (not (eq ac-line-p (xwem-batt-old-ac-line-p xwin))))
      (XFillRectangle xdpy xpix (xwem-face-get-gc 'xwem-face-white)
                      0 0 xwem-batt-width xwem-batt-height)
      ;; Outline battery
      (XFillRectangle xdpy xpix (xwem-face-get-gc 'xwem-face-white)
                      0 0 xwem-batt-width xwem-batt-height)
      (XDrawRectangle xdpy xpix (xwem-face-get-gc 'xwem-face-black)
                      1 2 (- xwem-batt-width 3) (- xwem-batt-height 4))
      (XDrawLine xdpy xpix (xwem-face-get-gc 'xwem-face-black)
                 3 1 (- xwem-batt-width 4) 1)
      (setq force t))

    ;; Maybe redraw percentage
    (when (or force (not (eq dheight (xwem-batt-old-dheight xwin))))
      ;; Find appopriate color
      (while (and perc-cols (> cperc (caar perc-cols)))
        (setq perc-cols (cdr perc-cols)))
      (setq perc-cols (cdar perc-cols))

      (xwem-set-face-foreground 'xwem-batt-tmp-face perc-cols)
      (XFillRectangle xdpy xpix (xwem-face-get-gc 'xwem-batt-tmp-face)
                      2 (- xwem-batt-height 2 dheight)
                      (- xwem-batt-width 4) dheight)
      (when (< dheight (- xwem-batt-height 5))
        (XDrawLine xdpy xpix (xwem-face-get-gc 'xwem-face-black)
                   2 (- xwem-batt-height 2 dheight)
                   (- xwem-batt-width 2) (- xwem-batt-height 2 dheight)))

      ;; Save DHEIGHT
      (setf (xwem-batt-old-dheight xwin) dheight))

    ;; Maybe redraw ac-line status
    (when (or force (not (eq ac-line-p (xwem-batt-old-ac-line-p xwin))))
      (when ac-line-p
        (xwem-set-face-foreground 'xwem-batt-tmp-face xwem-batt-ac-line-color)
        (let ((acgc (xwem-face-get-gc 'xwem-batt-tmp-face)))
          (XChangeGC xdpy acgc :line-width xwem-batt-ac-line-width)
          (XDrawLine xdpy xpix acgc
                     xwem-batt-width xwem-batt-ac-line-width
                     0 (- xwem-batt-height xwem-batt-ac-line-width))
          (XChangeGC xdpy acgc :line-width 0)))
      (setf (xwem-batt-old-ac-line-p xwin) ac-line-p))

    (xwem-batt-apply-pixmap xwin)))

(defun xwem-batt-win-remove (xwin &optional need-destroy)
  "Remove battery dockapp."
  (when (xwem-batt-itimer xwin)
    (delete-itimer (xwem-batt-itimer xwin)))
  (XFreePixmap (X-Win-dpy xwin) (xwem-batt-xmask xwin))
  (XFreePixmap (X-Win-dpy xwin) (xwem-batt-pixmap xwin))

  (setf (xwem-batt-itimer xwin) nil
        (xwem-batt-xmask xwin) nil
        (xwem-batt-pixmap xwin) nil
        (xwem-batt-old-dheight xwin) nil
        (xwem-batt-old-ac-line-p xwin) nil)

  ;; Remove events handler
  (X-Win-EventHandler-rem xwin 'xwem-batt-event-handler)

  (when need-destroy
    (XDestroyWindow (X-Win-dpy xwin) xwin)))

(defvar xwem-battery-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [button1] 'xwem-battery-status)
    (define-key map [button3] 'xwem-battery-popup-menu)
    map)
  "*Keymap for battery dock.")

(defvar xwem-battery-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [button1] 'xwem-battery-status)
    (define-key map [button3] 'xwem-battery-popup-menu)
    map)
  "*Keymap for battery dock.")

(defun xwem-batt-event-handler (xdpy win xev)
  "Event handler for xwem battery monitor."
  (X-Event-CASE xev
    (:X-MapNotify (xwem-batt-win-update win t))
    (:X-Expose (xwem-batt-apply-pixmap win))
    (:X-DestroyNotify (xwem-batt-win-remove win))
    (:X-ButtonPress
     (xwem-overriding-local-map xwem-battery-keymap
       (xwem-dispatch-command-xevent xev)))))

;;;###autoload
(defun xwem-battery (&optional dockip dockgroup dockalign)
  "Start xwem apm battery monitor in system tray."
  (interactive)
  (unless (fboundp 'apm-battery)
    (error "APM Battery module not loaded"))

  (let ((bxwin (xwem-batt-init (xwem-dpy))))
    ;; Enable turbo mode
    (when xwem-misc-turbo-mode
      (XSetWindowBackgroundPixmap (xwem-dpy) bxwin (xwem-batt-pixmap bxwin)))

    (XSelectInput (xwem-dpy) bxwin
		  (Xmask-or XM-Exposure XM-StructureNotify
                            XM-ButtonPress XM-ButtonRelease))
    (X-Win-EventHandler-add bxwin 'xwem-batt-event-handler nil
			    (list X-Expose X-DestroyNotify
                                  X-ButtonPress X-ButtonRelease))

    (xwem-XTrayInit (xwem-dpy) bxwin dockip dockgroup dockalign)

    (setf (xwem-batt-itimer bxwin)
          (start-itimer "xwem-batt"
                        `(lambda () (xwem-batt-win-update ,bxwin))
                        xwem-batt-update-interval
                        xwem-batt-update-interval))
    'started))

;;;###autoload(autoload 'xwem-battery-status "xwem-battery" nil t)
(define-xwem-command xwem-battery-status ()
  "Show battery status in xwem minibuffer."
  (xwem-interactive)
  (destructuring-bind (ac-line status perc time)
      (or (apm-battery) '(nil nil nil nil))
    (xwem-message
     'info "Battery: %d%%: AC-line: %s, Status: %S"
     perc (if ac-line "on" "off") status)))

;;;###autoload(autoload 'xwem-battery-popup-menu "xwem-battery" nil t)
(define-xwem-command xwem-battery-popup-menu (ev)
  "Popup battery menu."
  (xwem-interactive (list xwem-last-event))
  (unless (button-event-p ev)
    (error 'xwem-error
           "`xwem-battery-popup-menu' must be bound to mouse event"))
  (xwem-popup-menu
   (list "Battery"
         ["Status" xwem-battery-status]
         "---"
         (vector "Destroy"
                 `(xwem-batt-win-remove
                   ,(X-Event-win xwem-last-xevent) t)))))

;;;; In case there is no battery.ell
(unless (fboundp 'apm-battery)
  (defvar apm-program "apm")
  (defvar apm-acline-status-percent-time-arguments "-ablt")
  (defvar apm-status-alist
    '((0 . high) (1 . low) (2 . critical) (3 . charging)))

  (defun apm-battery ()
    "Return battery status."
    (let (acline status percents time)
      (with-temp-buffer
        (call-process apm-program nil (current-buffer)
                      nil apm-acline-status-percent-time-arguments)
        (goto-char (point-min))
        (setq acline (= (string-to-int
                         (buffer-substring (point-at-bol)
                                           (point-at-eol))) 1))
        (forward-line)
        (setq status (cdr (assq (string-to-int
                                 (buffer-substring (point-at-bol)
                                                   (point-at-eol)))
                                apm-status-alist)))
        (forward-line)
        (setq percents (string-to-int
                        (buffer-substring (point-at-bol)
                                          (point-at-eol))))
        (forward-line)
        (setq time (string-to-int
                    (buffer-substring (point-at-bol)
                                      (point-at-eol))))
        )
      (list acline status percents time))))


(provide 'xwem-battery)

;;; xwem-battery.el ends here
