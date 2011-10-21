;;; xwem-framei.el --- Frame indicator.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;; Created: Tue Jan 27 08:01:43 MSK 2004
;; Keywords: xwem
;; Time-stamp: <30/11/2006 00:00:47 lg@h1>

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

;; When switching frames indicate frame number using xwem-osd.
;;
;; Add something like this to your xwemrc.el to start using frame
;; indicator:
;;
;;   (add-hook 'xwem-after-init-hook 'xwem-framei-init)
;;

;; To use framei dockapp add:

;;   (add-hook 'xwem-after-init-hook 'xwem-framei-dockapp)

;;; Code:

(require 'xwem-load)
(require 'xwem-frame)
(require 'xwem-osd)

;;; Customisation
(defgroup xwem-framei nil
  "*Group to customize xwem frame indicator."
  :prefix "xwem-framei-"
  :group 'xwem-frame)

(defcustom xwem-framei-showtime 1
  "Period of time in second while xwem framei is shown."
  :type 'number
  :group 'xwem-framei)

(defcustom xwem-framei-xoffset 20
  "*X Offset from top left frame corner in pixels, where to show framei."
  :type 'number
  :group 'xwem-framei)

(defcustom xwem-framei-yoffset 20
  "*Y Offset from top left frame corner in pixels, where to show framei."
  :type 'number
  :group 'xwem-framei)

(define-xwem-face xwem-framei-face
  `(((background dark desktop)
     (:foreground "magenta" :size 34 :family "helvetica"))
    ((background desktop light)
     (:foreground "darkmagenta" :size 34 :family "helvetica"))
    ((background dark embedded)
     (:foreground "magenta" :size 24 :family "helvetica"))
    ((background embedded light)
     (:foreground "darkmagenta" :size 24 :family "helvetica"))
    ((background dark embedded-desktop)
     (:foreground "magenta" :size 24 :family "helvetica"))
    ((background embedded-desktop light)
     (:foreground "darkmagenta" :size 24 :family "helvetica"))
    ((background dark dedicated)
     (:foreground "magenta" :size 20 :family "fixed"))
    ((background dedicated light)
     (:foreground "darkmagenta" :size 20 :family "fixed"))
    (t (:foreground "green2" :size 24 :family "helvetica")))
  "*Face used to draw frame number."
  :group 'xwem-framei)

(defcustom xwem-framei-select-face-function 'xwem-framei-default-sff
  "*Function called with one arg - frame and should return face, which
will be used.  Function can look like this:

(cond ((string= \"Devel\" (xwem-frame-name frame))
       (get-face 'my-xwem-fi-devel-face))
      ((string= \"Inet\" (xwem-frame-name frame))
       (get-face 'my-xwem-fi-inet-face))
      )

To use different faces for different frames."
  :type 'function
  :group 'xwem-framei)

(defcustom xwem-framei-format-function 'xwem-framei-default-format
  "Function passed with one arg - frame, should return string to display."
  :type 'funtion
  :group 'xwem-framei)

(define-xwem-face xwem-framei-dockapp-face
  `(((desktop) (:foreground "magenta4" :bold t))
    ((embedded-desktop) (:foreground "magenta4"))
    ((dedicated) (:foreground "yellowgreen"))
    (t (:foreground "magenta4" :bold t)))
  "Face used to draw in framei dockapp."
  :group 'xwem-faces)

(defcustom xwem-framei-dockapp-format-function
  'xwem-framei-dockapp-default-format
  "Function passed with one arg - frame, should return string to display."
  :type 'funtion
  :group 'xwem-framei)

;;; Internal variables

(defun xwem-framei-default-format (frame)
  "Default function used in `xwem-framei-format-function'.
Return string in form \"NUM: NAME\""
  (case (xwem-frame-type frame)
    (dedicated
     (if (xwem-frame-cl frame)
         (format "%s" (xwem-client-name (xwem-frame-cl frame)))
       "Unknown"))
    (t
     (format "[%d] %s" (xwem-frame-num frame) (xwem-frame-name frame)))))


;;; Internal variables
(defvar xwem-framei-osd nil
  "OSD for frame indicator.")

(defvar xwem-framei-itimer nil
  "itimer for frame indicator.")

(defun xwem-framei-hidder ()
  (xwem-osd-hide xwem-framei-osd)
  (setq xwem-framei-itimer nil)
  )


(defun xwem-framei-default-sff (frame)
  "Default `xwem-framei-select-face-function'."
  'xwem-framei-face)

;;; Functions
(defun xwem-framei-show-osd ()
  "Show OSD for frame selected frame."
  (let* ((frgeom (xwem-frame-xgeom (xwem-frame-selected)))
         (fface (or (funcall xwem-framei-select-face-function
                            (xwem-frame-selected))
                    'xwem-framei-face))
         (x (+ xwem-framei-xoffset (X-Geom-x frgeom)))
         (y (+ xwem-framei-yoffset (X-Geom-y frgeom)))
         (text (funcall xwem-framei-format-function
                        (xwem-frame-selected)))
         (bgmode (xwem-misc-xwin-background-mode (xwem-rootwin) x y))
         (ftype (xwem-frame-type (xwem-frame-selected))))

    (xwem-osd-move xwem-framei-osd x y)

    ;; Change
    (xwem-osd-set-color
     xwem-framei-osd
     (xwem-face-foreground fface (list 'background bgmode ftype)))
    (xwem-osd-set-font
     xwem-framei-osd
     (xwem-face-font fface (list 'background bgmode ftype)))

    (xwem-osd-text xwem-framei-osd text)
    (xwem-osd-show xwem-framei-osd)

    (when (itimerp xwem-framei-itimer)
      (delete-itimer xwem-framei-itimer)
      (setq xwem-framei-itimer nil))
    (setq xwem-framei-itimer
          (start-itimer "xwem-framei-timer"
                        'xwem-framei-hidder xwem-framei-showtime))
    ))

(defun xwem-framei-on-select ()
  "Handle frame switching."
  (xwem-add-hook-post-deferring 'xwem-framei-show-osd))

(defun xwem-framei-on-change (frame)
  "Handle FRAME change.
To be used in `xwem-frame-change-hook'."
  (when (and (xwem-frame-selected-p frame)
             (eq (xwem-osd-state xwem-framei-osd) 'shown))
    ;; Update only if framei osd is shown on screen
    (xwem-framei-on-select)))

;;;###autoload
(defun xwem-framei-init (&optional xdpy)
  "Initialize xwem frame indicator on display XDPY."
  (unless xdpy
    (setq xdpy (xwem-dpy)))

  (let ((rgeom (XGetGeometry xdpy (XDefaultRootWindow xdpy))))
    (setq xwem-framei-osd
          (xwem-osd-create xdpy 0 0
                           (X-Geom-width rgeom)
                           (X-Geom-height rgeom))))

  (add-hook 'xwem-frame-select-hook 'xwem-framei-on-select)
  (add-hook 'xwem-frame-change-hook 'xwem-framei-on-change))

(defun xwem-framei-stop ()
  "Stop xwem-framei."
  (remove-hook 'xwem-frame-select-hook 'xwem-framei-on-select)
  (remove-hook 'xwem-frame-change-hook 'xwem-framei-on-change)
  (when xwem-framei-itimer
    (delete-itimer xwem-framei-itimer))

  (when (xwem-osd-p xwem-framei-osd)
    (xwem-osd-destroy xwem-framei-osd)))

;;;; Frame indicator dockapp
(defvar xwem-framei-dockapp-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-default-binding map 'ignore)
    (define-key map [button1] 'xwem-framei-dockapp-popup-menu)
    (define-key map [button3] 'xwem-framei-dockapp-popup-alt-menu)
    map)
  "Keymap used for framei osd.")

(defun xwem-framei-dockapp-default-format (frame)
  "Default function used in `xwem-framei-dockapp-format-function'.
Return string in form \"NUM: NAME\""
  (case (xwem-frame-type frame)
    (dedicated
     (if (xwem-frame-cl frame)
         (format "%s" (xwem-client-name (xwem-frame-cl frame)))
       "Unknown"))
    (t
     (format "%d:%s" (xwem-frame-num frame) (xwem-frame-name frame)))))

(defvar xwem-framei-dockapp-osd nil
  "Framei's dockapp osd.")

(defun xwem-framei-dockapp-update (&optional frame)
  "To be used in `xwem-frame-select-hook'."
  (if (not (xwem-osd-p xwem-framei-dockapp-osd))
      ;; Hmm, why this?
      (remove-hook 'xwem-frame-select-hook 'xwem-framei-dockapp-update)

    (unless frame
      (setq frame (xwem-frame-selected)))
    (when (xwem-frame-selected-p frame)
      (xwem-osd-set-font xwem-framei-dockapp-osd
                         (xwem-face-font 'xwem-framei-dockapp-face
                                         (list (xwem-frame-type frame))))
      (xwem-osd-set-color xwem-framei-dockapp-osd
                          (xwem-face-foreground 'xwem-framei-dockapp-face
                                                (list (xwem-frame-type frame))))
      (xwem-osd-text xwem-framei-dockapp-osd
                     (funcall xwem-framei-dockapp-format-function frame)))))

(define-xwem-command xwem-framei-stop-dockapp ()
  "Stop framei dockapp, if any."
  (xwem-interactive)

  (remove-hook 'xwem-frame-select-hook 'xwem-framei-dockapp-on-select)
  (remove-hook 'xwem-frame-change-hook 'xwem-framei-dockapp-on-change)

  (when (xwem-osd-p xwem-framei-dockapp-osd)
    (xwem-osd-destroy xwem-framei-dockapp-osd)))

;;;###autoload
(defun xwem-framei-dockapp (&optional dockid dockgroup dockalign)
  "Start frame indicator dockapp."
  (unless (xwem-osd-p xwem-framei-dockapp-osd)
    (let ((width (* (face-width 'xwem-framei-dockapp-face) 8))
          (height (face-height 'xwem-framei-dockapp-face)))

      (setq xwem-framei-dockapp-osd
            (xwem-osd-create-dock (xwem-dpy) width height
                                  (list 'keymap xwem-framei-dockapp-keymap)))

      ;; Try to display current frame
      (xwem-framei-dockapp-update)

      ;; Add frame hooks
      (add-hook 'xwem-frame-select-hook 'xwem-framei-dockapp-update)
      (add-hook 'xwem-frame-change-hook 'xwem-framei-dockapp-update))))
    
;; Commands
;;;###autoload(autoload 'xwem-framei-dockapp-popup-menu "xwem-framei" nil t)
(define-xwem-command xwem-framei-dockapp-popup-menu ()
  "Popup frames menu."
  (xwem-interactive)

  (xwem-popup-menu
   (list "XWEM Frames" :filter 
         (lambda (not-used)
           (nconc
            (mapcar
             (lambda (frame)
               (vector
                (case (xwem-frame-type frame)
                  (dedicated
                   (if (xwem-frame-cl frame)
                       (format "D[%d]: %s" (xwem-frame-num frame)
                               (xwem-client-name (xwem-frame-cl frame)))
                     (format "D[%d]: <none>" (xwem-frame-num frame))))
                  (t (format "%s %d: %s"
                             (capitalize (symbol-name (xwem-frame-type frame)))
                             (xwem-frame-num frame) (xwem-frame-name frame))))
                `(xwem-select-frame ,frame)))
             xwem-frames-list)
            (list "---"
                  (vector "Destroy" 'xwem-framei-stop-dockapp)))))))

;;;###autoload(autoload 'xwem-framei-dockapp-popup-alt-menu "xwem-framei" nil t)
(define-xwem-command xwem-framei-dockapp-popup-alt-menu ()
  "Popup alternative menu."
  (xwem-interactive)

  (xwem-popup-menu
   (nconc '("XWEM Frames")
          (mapcar
           (lambda (type)
             (nconc
              (list (capitalize (symbol-name type)))
              (mapcar
               (lambda (frame)
                 (vector
                  (format "%d: %s"
                          (xwem-frame-num frame)
                          (case (xwem-frame-type frame)
                            (dedicated
                             (if (xwem-frame-cl frame)
                                 (xwem-client-name (xwem-frame-cl frame))
                               "<none>"))
                            (t (xwem-frame-name frame))))
                  `(xwem-select-frame ,frame)))
               (xwem-frames-list type))))
           xwem-frame-types)
          '("---" ["Destroy" xwem-framei-stop-dockapp]))))


(provide 'xwem-framei)

;;; xwem-framei.el ends here
