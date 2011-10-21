;;; xwem-theme.el --- Themes support for xwem.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Tue Nov 23 14:49:41 MSK 2004
;; Keywords: xwem
;; Time-stamp: <29/11/2008 04:43:33 lg@h1.lan>

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

(defgroup xwem-theme nil
  "Group to customize xwem themes."
  :prefix "xwem-theme-"
  :group 'xwem)

;;;; Face themes
(defconst xwem-face-default-theme
  `((face xwem-tabber-face
          (((frame-selected tab-selected)
            (:foreground "white" :background "green4"))
           ((delimiter-left frame-selected tab-selected)
            (:foreground "white"))
           ((delimiter-right frame-selected tab-selected)
            (:foreground "black"))

           ((frame-selected tab-nonselected)
            (:foreground "black" :background "gray80"))
           ((delimiter-left frame-selected tab-nonselected)
            (:foreground "white"))
           ((delimiter-right frame-selected tab-nonselected)
            (:foreground "black"))

           ((frame-nonselected tab-selected)
            (:foreground "gray80" :background "DarkGreen"))
           ((delimiter-left frame-nonselected tab-selected)
            (:foreground "white"))
           ((delimiter-right frame-nonselected tab-selected)
            (:foreground "black"))

           ((frame-nonselected tab-nonselected)
            (:foreground "black" :background "gray40"))
           ((delimiter-left frame-nonselected tab-nonselected)
            (:foreground "white"))
           ((delimiter-right frame-nonselected tab-nonselected)
            (:foreground "black"))

           (t (:foreground "white"))))

    (face x-border-face
          (((selected) (:foreground "green"))
           (t (:foreground "gray80"))))

    (face xwem-frame-inner-border-face
          (((light nonselected)
            (:foreground "gray80" :background "gray80"))
           ((medium nonselected)
            (:foreground "gray50" :background "gray50"))
           ((dark nonselected)
            (:foreground "gray20" :background "gray20"))
           ((light selected)
            (:foreground "cyan2" :background "cyan2"))
           ((medium selected)
            (:foreground "royalblue" :background "royalblue"))
           ((dark selected)
            (:foreground "blue4" :background "blue4"))))

    (face xwem-launch-dock-face
          (((medium) (:foreground "gray70"))
           ((light) (:foreground "white"))
           ((dark) (:foreground "black"))))

    (face xwem-strokes-face
          (((background light)
            (:foreground "red4" :background "black"))
           ((background dark)
            (:foreground "red" :background "black"))
           ((background begin light)
            (:foreground "magenta4" :background "black"
                         :line-width 12 :cap-style X-CapRound))
           ((background begin dark)
            (:foreground "magenta" :background "black"
                         :line-width 12 :cap-style X-CapRound))))

    (face xwem-tray-delimiter-face
          (((background light)
            (:foreground "gray40"))
           ((background light shadow)
            (:foreground "gray30"))
           ((background dark)
            (:foreground "gray70"))
           ((background dark shadow)
            (:foreground "gray80"))))

    (face xwem-window-outline-face
          (((frame-selected win-selected)
            (:foreground "green" :background "green4" :line-width 4))
           ((frame-selected win-nonselected)
            (:foreground "gray70" :background "gray70" :line-width 4))
           ((frame-nonselected win-selected)
            (:foreground "green3" :background "green4" :line-width 4))
           ((frame-nonselected win-nonselected)
            (:foreground "gray60" :background "gray40" :line-width 4))))

    (face xwem-window-delimiter-face
          (((horizontal)
            (:foreground "royalblue" :background "black"))
           ((horizontal shadow)
            (:foreground "blue4" :background "black"))
           ((horizontal light shadow)
            (:foreground "cyan" :background "black"))
           ((vertical)
            (:foreground "royalblue" :background "black"))
           ((shadow vertical)
            (:foreground "blue4" :background "black"))
           ((light shadow vertical)
            (:foreground "cyan" :background "black"))
           (t (:foreground "gray20" :background "black")))))
  "Default faces theme.")

(defconst xwem-face-extrim-theme
  `((face xwem-tabber-face
     (((frame-selected tab-selected)
       (:foreground "white" :background "gray20"))
      ((delimiter-left frame-selected tab-selected)
       (:foreground "white"))
      ((delimiter-right frame-selected tab-selected)
       (:foreground "black"))

      ((frame-selected tab-nonselected)
       (:foreground "black" :background "gray80"))
      ((delimiter-left frame-selected tab-nonselected)
       (:foreground "white"))
      ((delimiter-right frame-selected tab-nonselected)
       (:foreground "black"))

      ((frame-nonselected tab-selected)
       (:foreground "gray80" :background "gray40"))
      ((delimiter-left frame-nonselected tab-selected)
       (:foreground "white"))
      ((delimiter-right frame-nonselected tab-selected)
       (:foreground "black"))

      ((frame-nonselected tab-nonselected)
       (:foreground "black" :background "gray40"))
      ((delimiter-left frame-nonselected tab-nonselected)
       (:foreground "white"))
      ((delimiter-right frame-nonselected tab-nonselected)
       (:foreground "black"))))

    (face x-border-face
     (((selected) (:foreground "white"))
      (t (:foreground "dark"))))

    (face xwem-frame-inner-border-face
     (((light nonselected)
       (:foreground "gray80"))
      ((medium nonselected)
       (:foreground "gray50"))
      ((dark nonselected)
       (:foreground "gray20"))
      ((light selected)
       (:foreground "white"))
      ((medium selected)
       (:foreground "gray50"))
      ((dark selected)
       (:foreground "dark"))))

    (face xwem-launch-dock-face
     (((medium) (:foreground "gray70"))
      ((light) (:foreground "white"))
      ((dark) (:foreground "black"))))

    (face xwem-strokes-face
     (((background light)
       (:foreground "dimgray" :background "black"))
      ((background dark)
       (:foreground "lightgray" :background "black"))
      ((background begin light)
       (:foreground "darkslategrey" :background "black"
        :line-width 12 :cap-style X-CapRound))
      ((background begin dark)
       (:foreground "gray" :background "black"
        :line-width 12 :cap-style X-CapRound))))

    (face xwem-tray-delimiter-face
     (((background light)
       (:foreground "gray40"))
      ((background light shadow)
       (:foreground "gray30"))
      ((background dark)
       (:foreground "gray70"))
      ((background dark shadow)
       (:foreground "gray80"))))

    (face xwem-window-outline-face
     (((frame-selected win-selected)
       (:foreground "dark" :background "dark"))
      ((frame-selected win-nonselected)
       (:foreground "gray50" :background "gray50"))
      ((frame-nonselected win-selected)
       (:foreground "gray20" :background "gray20"))
      ((frame-nonselected win-nonselected)
       (:foreground "gray60" :background "gray40"))))

    (face xwem-window-delimiter-face
     (((horizontal)
       (:foreground "gray30" :background "black"))
      ((horizontal shadow)
       (:foreground "gray10" :background "black"))
      ((horizontal light shadow)
       (:foreground "gray80" :background "black"))
      ((vertical)
       (:foreground "gray10" :background "black"))
      ((shadow vertical)
       (:foreground "gray40" :background "black"))
      ((light shadow vertical)
       (:foreground "white" :background "black")))))
  "Extrim face theme.")

(defconst xwem-face-ocean-theme
  `((face xwem-tabber-face
          (((frame-selected tab-selected)
            (:foreground "#7DC5C5" :background "#174D4D" :bold t))
           ((delimiter-left frame-selected tab-selected)
            (:foreground "white"))
           ((delimiter-right frame-selected tab-selected)
            (:foreground "black"))

           ((frame-selected tab-nonselected)
            (:foreground "#102A2A" :background "#598080"))
           ((delimiter-left frame-selected tab-nonselected)
            (:foreground "white"))
           ((delimiter-right frame-selected tab-nonselected)
            (:foreground "black"))

           ((frame-nonselected tab-selected)
            (:foreground "#6BA0A0" :background "#406060" :bold t))
           ((delimiter-left frame-nonselected tab-selected)
            (:foreground "white"))
           ((delimiter-right frame-nonselected tab-selected)
            (:foreground "black"))

           ((frame-nonselected tab-nonselected)
            (:foreground "#123D3D" :background "#507070"))
           ((delimiter-left frame-nonselected tab-nonselected)
            (:foreground "white"))
           ((delimiter-right frame-nonselected tab-nonselected)
            (:foreground "black"))))

    (face x-border-face
          (((selected) (:foreground "#10BABA"))
           (t (:foreground "#085580"))))

    (face xwem-frame-inner-border-face
          (((light nonselected)
            (:foreground "gray80" :background "gray80"))
           ((medium nonselected)
            (:foreground "gray50" :background "gray50"))
           ((dark nonselected)
            (:foreground "gray20" :background "gray20"))
           ((light selected)
            (:foreground "#7DB3B3" :background "#7DB3B3"))
           ((medium selected)
            (:foreground "#176D6D" :background "#176D6D"))
           ((dark selected)
            (:foreground "#102A2A" :background "#102A2A"))))

    (face xwem-launch-dock-face
          (((medium) (:foreground "#176D6D"))
           ((light) (:foreground "#7DB3B3"))
           ((dark) (:foreground "#102A2A"))))

    (face xwem-strokes-face
          (((background light)
            (:foreground "#176D6D" :background "black"))
           ((background dark)
            (:foreground "#7DB3B3" :background "black"))
           ((background begin light)
            (:foreground "#154C4C" :background "black"
                         :line-width 12 :cap-style X-CapRound))
           ((background begin dark)
            (:foreground "#6DA0A0" :background "black"
                         :line-width 12 :cap-style X-CapRound))))

    (face xwem-tray-delimiter-face
          (((background light)
            (:foreground "gray40"))
           ((background light shadow)
            (:foreground "gray30"))
           ((background dark)
            (:foreground "gray70"))
           ((background dark shadow)
            (:foreground "gray80"))))

    (face xwem-window-outline-face
          (((frame-selected win-selected)
            (:foreground "#10BABA" :background "green4" :line-width 4))
           ((frame-selected win-nonselected)
            (:foreground "gray70" :background "gray70" :line-width 4))
           ((frame-nonselected win-selected)
            (:foreground "#109898" :background "green4" :line-width 4))
           ((frame-nonselected win-nonselected)
            (:foreground "gray60" :background "gray40" :line-width 4))))

    (face xwem-window-delimiter-face
          (((horizontal)
            (:foreground "#176D6D" :background "#176D6D"))
           ((horizontal shadow)
            (:foreground "#102A2A" :background "#102A2A"))
           ((horizontal light shadow)
            (:foreground "#7DB3B3" :background "#7DB3B3"))
           ((vertical)
            (:foreground "#176D6D" :background "#176D6D"))
           ((shadow vertical)
            (:foreground "#102A2A" :background "#102A2A"))
           ((light shadow vertical)
            (:foreground "#7DB3B3" :background "#7DB3B3")))))
  "Ocean faces theme.")

(defvar xwem-face-themes
  '((default . xwem-face-default-theme)
    (extrim . xwem-face-extrim-theme)
    (ocean . xwem-face-ocean-theme))
  "List of themes for xwem faces.
NOT USED.")

;;;; Fonts theme
(defconst xwem-font-default-theme
  `((face xwem-tabber-face
          (((frame-selected tab-selected)
            (:font ,(face-font-name 'default) :bold t :size 16))
           ((frame-selected tab-nonselected)
            (:font ,(face-font-name 'default) :size 16))
           ((frame-nonselected tab-selected)
            (:font ,(face-font-name 'default) :bold t :size 16))
           ((frame-nonselected tab-nonselected)
            (:font ,(face-font-name 'default) :size 16))))))

(defconst xwem-font-extrim-theme
  `((face xwem-tabber-face
          (((frame-selected tab-selected)
            (:font "-*-fixed-medium-r-*-*-13-*-*-*-*-*-*-*" :bold t))
           ((frame-selected tab-nonselected)
            (:font "-*-fixed-medium-r-*-*-12-*-*-*-*-*-*-*"))
           ((frame-nonselected tab-selected)
            (:font "-*-fixed-medium-r-*-*-13-*-*-*-*-*-*-*" :bold t))
           ((frame-nonselected tab-nonselected)
            (:font "-*-fixed-medium-r-*-*-12-*-*-*-*-*-*-*"))))))

(defvar xwem-font-themes
  '((default . xwem-font-default-theme)
    (extrim . xwem-font-extrim-theme)))

;;;; Cursor themes
(defconst xwem-cursor-default-theme
  '((custom xwem-cursor-help-foreground-color "#00BB00")
    (custom xwem-cursor-help-background-color "#009900")
    (custom xwem-root-cursor-foreground-color "white")
    (custom xwem-root-cursor-background-color "black")
    (custom xwem-frame-cursor-foreground-color "#111111")
    (custom xwem-frame-cursor-background-color "#EEEEEE")
    (custom xwem-tray-cursor-foreground-color "#000075")
    (custom xwem-tray-cursor-background-color "#ffffff")))

(defconst xwem-cursor-extrim-theme
  `((custom xwem-cursor-help-foreground-color "#888888")
    (custom xwem-cursor-help-background-color "#080808")
    (custom xwem-root-cursor-foreground-color "black")
    (custom xwem-root-cursor-background-color "white")
    (custom xwem-frame-cursor-foreground-color "white")
    (custom xwem-frame-cursor-background-color "black")))

(defconst xwem-cursor-ocean-theme
  `((custom xwem-cursor-help-foreground-color "#009999")
    (custom xwem-cursor-help-background-color "#006B6B")
    (custom xwem-root-cursor-foreground-color "#005858")
    (custom xwem-root-cursor-background-color "#AFFFFA")
    (custom xwem-frame-cursor-foreground-color "#BFFFFB")
    (custom xwem-frame-cursor-background-color "#006D66")))

(defconst xwem-cursor-spring-theme
  `((custom xwem-cursor-help-foreground-color "#00B366")
    (custom xwem-cursor-help-background-color "#007D48")
    (custom xwem-root-cursor-foreground-color "#007B3D")
    (custom xwem-root-cursor-background-color "#BFFFD6")
    (custom xwem-frame-cursor-foreground-color "#80FFC9")
    (custom xwem-frame-cursor-background-color "#007D48")))

(defconst xwem-cursor-winter-theme
  `((custom xwem-cursor-help-foreground-color "white")
    (custom xwem-cursor-help-background-color "gray50")
    (custom xwem-root-cursor-foreground-color "white")
    (custom xwem-root-cursor-background-color "gray50")
    (custom xwem-frame-cursor-foreground-color "white")
    (custom xwem-frame-cursor-background-color "gray50")))

(defvar xwem-cursor-themes
  '((default . xwem-cursor-default-theme)
    (extrim . xwem-cursor-extrim-theme)
    (ocean . xwem-cursor-ocean-theme)
    (spring . xwem-cursor-spring-theme)
    (winter . xwem-cursor-winter-theme))
  "Alist of themes for cursors colors.
Alist in form \(THEME-NAME . THEME-VAR\).")

;;;; Frame themes
(defconst xwem-frame-default-theme
  '((frame-property inner-border-width 8)
    (frame-property inner-border-thickness 2)
    (frame-property title-height 18)
    (custom xwem-win-vertical-delim-width (8 . 1))
    (custom xwem-win-horizontal-delim-width (6 . 1))))

(defconst xwem-frame-extrim-theme
  '((frame-property inner-border-width 3)
    (frame-property inner-border-thickness 1)
    (frame-property title-height 14)
    (custom xwem-win-vertical-delim-width (4 . 1))
    (custom xwem-win-horizontal-delim-width (3 . 1))))

(defvar xwem-frame-themes
  '((default . xwem-frame-default-theme)
    (extrim . xwem-frame-extrim-theme))
  "Alist of themes for frame.
Alist in form \(THEME-NAME . THEME-VAR\).")

;;;; Client themes
(defconst xwem-client-default-theme
  '((client-property x-border-width 2)))

(defconst xwem-client-extrim-theme
  '((client-property x-border-width 3)))

(defvar xwem-client-themes
  '((default . xwem-client-default-theme)
    (extrim . xwem-client-extrim-theme))
  "Alist of themes for clients.
Alist in form \(THEME-NAME . THEME-VAR\).")

;;;; Minibuffer themes
(defconst xwem-minibuffer-default-theme
  '((custom xwem-minibuffer-bgcol "gray80")
    (custom xwem-minibuffer-passive-border-color "blue3")
    (custom xwem-minibuffer-active-border-color "cyan3")))

(defconst xwem-minibuffer-extrim-theme
  '((custom xwem-minibuffer-bgcol "black")
    (custom xwem-minibuffer-passive-border-color "gray30")
    (custom xwem-minibuffer-active-border-color "white")))

(defconst xwem-minibuffer-ocean-theme
  '((custom xwem-minibuffer-bgcol "gray80")
    (custom xwem-minibuffer-passive-border-color "#306F6F")
    (custom xwem-minibuffer-active-border-color "cyan")))

(defvar xwem-minibuffer-themes
  '((default . xwem-minibuffer-default-theme)
    (extrim . xwem-minibuffer-extrim-theme)
    (ocean . xwem-minibuffe-ocean-theme))
  "Alist of themes for minibuffer.
Alist in form \(THEME-NAME . THEME-VAR\).")

;;;; TODO: managing themes

;;; Themes definition
(defconst xwem-theme-default
  (append xwem-face-default-theme
          xwem-font-default-theme
          xwem-cursor-default-theme
          xwem-frame-default-theme
          xwem-client-default-theme
          xwem-minibuffer-default-theme)
  "Default xwem theme.")

(defconst xwem-theme-extrim
  (append xwem-face-extrim-theme
          xwem-font-extrim-theme
          xwem-cursor-extrim-theme
          xwem-frame-extrim-theme
          xwem-client-extrim-theme
          xwem-minibuffer-extrim-theme)
  "Extrim theme for xwem.")

(defconst xwem-theme-ocean
  (append xwem-face-ocean-theme
          xwem-cursor-ocean-theme
          xwem-minibuffer-ocean-theme)
  "Ocean theme for xwem.")

(defvar xwem-theme-themes
  '(xwem-theme-default xwem-theme-extrim xwem-theme-ocean)
  "List of themes.")

(defun xwem-theme-apply-face-spec (face spec domain)
  "Apply to FACE it spec."
  (let (tag-set vplist tt tv vfont)
    (while spec
      (setq tag-set (car (car spec))
            vplist (cadr (car spec))
            vfont nil)

      ;; Apply attributes
      (while vplist
        (setq tt (car vplist)
              tv (cadr vplist))
        (cond ((eq tt :foreground)
               (xwem-set-face-foreground face tv tag-set domain))
              ((eq tt :background)
               (xwem-set-face-background face tv tag-set domain))
              ((eq tt :line-style)
               (xwem-set-face-line-style face tv tag-set domain))
              ((eq tt :line-width)
               (xwem-set-face-line-width face tv tag-set domain))
              ((eq tt :cap-style)
               (xwem-set-face-cap-style face tv tag-set domain))
              ((eq tt :join-style)
               (xwem-set-face-join-style face tv tag-set domain))
              ((eq tt :function)
               (xwem-set-face-function face tv tag-set domain))
              ((eq tt :subwindow-mode)
               (xwem-set-face-subwindow-mode face tv tag-set domain))
              ((eq tt :graphics-exposures)
               (xwem-set-face-graphics-exposures face tv tag-set domain))
              ((eq tt :font)
               (setq vfont tv)
               (xwem-set-face-font face tv tag-set domain))
              ((eq tt :size)
               (xwem-set-face-font-size face tv tag-set domain))
              ((eq tt :bold)
               (if tv
                   (setq tv (or (x-make-font-bold (or vfont (face-font-name face)))
                                (or vfont (face-font-name face '(default)))))
                 (setq tv (or (x-make-font-unbold (or vfont (face-font-name face)))
                              (or vfont (face-font-name face '(default))))))
               (setq vfont tv)
               (xwem-set-face-font face tv tag-set domain))
              ((eq tt :italic)
               (if tv
                   (setq tv (or (x-make-font-italic (or vfont (face-font-name face)))
                                (or vfont (face-font-name face '(default)))))
                 (setq tv (or (x-make-font-unitalic (face-font-name face))
                              (or vfont (face-font-name face '(default))))))
               (setq vfont tv)
               (xwem-set-face-font face tv tag-set domain)))
        (setq vplist (cddr vplist)))
      (setq spec (cdr spec)))))

(defun xwem-theme-apply (theme &optional domain)
  "Apply theme to DOMAIN.
If DOMAIN is omitted, apply THEME globally."
  (mapc #'(lambda (tspec)
            (case (car tspec)
              (face
               (when (find-face (cadr tspec))
                 (xwem-theme-apply-face-spec (cadr tspec) (caddr tspec) domain)))

              (frame-property
               (if (and domain (xwem-frame-p domain))
                   (xwem-frame-set-property domain (cadr tspec) (caddr tspec))
                 (mapc #'(lambda (f)
                           (xwem-frame-set-property f (cadr tspec) (caddr tspec)))
                       (xwem-frames-list))))

              (client-property
               (if (and domain (xwem-cl-p domain))
                   (xwem-client-set-property domain (cadr tspec) (caddr tspec))
                 (mapc #'(lambda (c)
                           (xwem-client-set-property c (cadr tspec) (caddr tspec)))
                       (xwem-clients-list))))

              (custom
               (customize-set-variable (cadr tspec) (caddr tspec)))))
        theme))

;;;###autoload(autoload 'xwem-theme-set "xwem-theme" nil t)
(define-xwem-command xwem-theme-set (theme &optional domain)
  "Interactively set theme."
  (xwem-interactive
   (list (xwem-completing-read
          "XWEM Theme [default]: "
          (mapcar #'(lambda (el)
                      (last (split-string (symbol-name el) "-")))
                  xwem-theme-themes)
          nil t)
         (xwem-completing-read
          "XWEM Domain [global]: "
          '(("global") ("client") ("window") ("frame"))
          nil t)))

  ;; Fix theme
  (if (string= "" theme)
      (setq theme xwem-theme-default)
    (setq theme (symbol-value (intern (concat "xwem-theme-" theme)))))

  ;; Fix domain
  (cond ((string= "client" domain)
         (setq domain (xwem-cl-selected)))
        ((string= "window" domain)
         (setq domain (xwem-win-selected)))
        ((string= "frame" domain)
         (setq domain (xwem-frame-selected)))
        (t (setq domain nil)))

  (xwem-theme-apply theme domain))


(provide 'xwem-theme)

;;; xwem-theme.el ends here
