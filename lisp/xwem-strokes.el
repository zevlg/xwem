;;; xwem-strokes.el --- Strokes support by XWEM.

;; Copyright (C) 2003-2007 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Steve Youngs  <steve@youngs.au.com>
;; Created: Thu Dec  4 17:42:12 MSK 2003
;; Keywords: xwem, xlib
;; Time-stamp: <19/10/2007 00:00:00 lg@h1>

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

;; To save strokes on exit:

;;    (add-hook 'xwem-exit-hook 'xwem-strokes-save)

;;; Features:

;; o Various command types
;; o Global and client local strokes maps
;; o Commentary for strokes
;; o Editing possibilities

;;; Code:

(require 'strokes)
(require 'view-less)
(require 'xlib-xtest)
(require 'xwem-load)
(require 'xwem-misc)

;;; Customization
(defgroup xwem-strokes nil
  "Group to customize XWEM strokes."
  :prefix "xwem-strokes-"
  :group 'xwem)

(defcustom xwem-strokes-grid 25
  "*XWEM strokes Grid resolution.
Look at `strokes-grid-resolution' for details."
  :type 'integer
  :group 'xwem-strokes)

(defcustom xwem-strokes-minimum-match-score 2000
  "*Take a look at `strokes-minimum-match-score'."
  :type 'integer
  :group 'xwem-strokes)

(defcustom xwem-strokes-file (expand-file-name ".strokes" xwem-dir)
  "*File contained strokes for xwem stroke mode."
  :type 'file
  :group 'xwem-strokes)

(defcustom xwem-strokes-click-command 'xwem-strokes-default-click-command
  "*Command to execute when stroke is actually a `click'."
  :type 'function
  :group 'xwem-strokes)

(defcustom xwem-strokes-cursor-type '(X-XC-dot)
  "*Type of cursor to use when XWEM enters strokes mode."
  :type (xwem-cursor-shape-choice)
  :set (xwem-cus-set-cursor-shape xwem-strokes-cursor)
  :initialize 'custom-initialize-default
  :group 'xwem-strokes)

(defcustom xwem-strokes-cursor-foreground-color "#00ea00"
  "*Cursor's foreground color when XWEM in strokes mode."
  :type 'color
  :set (xwem-cus-set-cursor-foreground xwem-strokes-cursor)
  :initialize 'custom-initialize-default
  :group 'xwem-strokes)

(defcustom xwem-strokes-cursor-background-color "#002800"
  "*Cursor's background color when XWEM in strokes mode."
  :type 'color
  :set (xwem-cus-set-cursor-background xwem-strokes-cursor)
  :initialize 'custom-initialize-default
  :group 'xwem-strokes)

(defcustom xwem-strokes-gc-function 'X-GXCopy
  "Function applied to draw strokes."
  :type (xwem-gc-function-choice)
  :group 'xwem-strokes)

(define-xwem-face xwem-strokes-face
  `(((background light)
     (:foreground "red4" :background "black"))
    ((background dark)
     (:foreground "red" :background "black"))
    ((background begin light)
     (:foreground "magenta4" :background "black"
      :line-width 12 :cap-style X-CapRound))
    ((background begin dark)
     (:foreground "magenta" :background "black"
      :line-width 12 :cap-style X-CapRound))
    (t (:foreground "red4" :background "black"
        :line-width 8 :cap-style X-CapRound
        :subwindow-mode X-IncludeInferiors
        :function (eval xwem-strokes-gc-function))))
  "Face used to draw strokes."
  :group 'xwem-strokes
  :group 'xwem-faces)

;;; Internal variables

;; Stroke variables
(defvar xwem-strokes-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-default-binding map 'xwem-strokes-nocmd)

    (define-key map [button2] 'xwem-strokes-idescribe)
    (define-key map [button2up] 'xwem-strokes-ibutton2up)
    (define-key map [button1] 'xwem-strokes-ibutton1)
    (define-key map [button1up] 'xwem-strokes-ibutton1up)
    (define-key map [(meta button1up)] 'xwem-strokes-ibutton1up)
    (define-key map [(control button1up)] 'xwem-strokes-ibutton1up)
    (define-key map [(meta control button1up)] 'xwem-strokes-ibutton1up)
    (define-key map [button3] 'xwem-strokes-ibutton3)
    map)
  "Keymap being used while in strokes event loop.")

(defvar xwem-strokes-global-map nil
  "List of global strokes used in any context of XWEM.")

(defsubst xwem-strokes-global-map ()
  "Return global strokes map for xwem.
Makes sure strokes are loaded."
  (unless (get 'xwem-strokes 'loaded)
    (xwem-strokes-load))
  xwem-strokes-global-map)

(defvar xwem-strokes-maps nil
  "Context specific strokes maps.
List of cons cells, where car of each cell is QUALIFIER using in
`xwem-cl-match-p' and cdr is list of defined strokes for clients that
matches QUALIFIER.")

(defsubst xwem-strokes-maps ()
  "Return list of local strokes maps.
Makes sure strokes are loaded."
  (unless (get 'xwem-strokes 'loaded)
    (xwem-strokes-load))
  xwem-strokes-maps)

(defvar xwem-strokes-curr nil
  "Current events list, it passed to `stroke-' routines.")
(defvar xwem-strokes-defining nil
  "Non-nil mean that we defining stroke now.")

(defvar xwem-strokes-background-mode nil
  "Saved background mode.")

(defvar xwem-strokes-cursor nil
  "Cursor used while reading stroke.")

(defvar xwem-strokes-last-stroke nil "Last stroke.")

(defvar xwem-strokes-click-next-allow nil)
;; Functions
(define-xwem-command xwem-strokes-default-click-command ()
  "Default command to execute when strokes detected click.
Click is detected when no mouse moving occurred while doing stroke."
  (xwem-interactive)
  ;; BUG! Hardcoded <button2>!
  (when (and (memq (X-Event-type xwem-last-xevent)
                   (list X-ButtonPress X-ButtonRelease))
             (= (X-Event-xbutton-button xwem-last-xevent) 2))
    (setq xwem-strokes-click-next-allow t)
    (X-XTest-FakeInput (xwem-dpy) X-Xtest-ButtonPress 2)
    (X-XTest-FakeInput (xwem-dpy) X-Xtest-ButtonRelease 2))
  )

(define-xwem-command xwem-strokes-emulate-button2-click ()
  "Emulate clicking <button2>."
  (xwem-interactive)
  (xwem-kbd-add-pending-keys [button2]))

(define-xwem-command xwem-strokes-save (&optional file)
  "Save user defined strokes to FILE.
By default FILE is `xwem-strokes-file'."
  (xwem-interactive (list xwem-strokes-file))
  (when (or (get 'xwem-strokes 'loaded)
            (not (file-exists-p (or file xwem-strokes-file))))
    ;; Save only if strokes has been loaded
    (with-temp-buffer
      (insert-string
       ";;   -*- Syntax: Emacs-Lisp; Mode: emacs-lisp -*-\n")
      (insert-string (format ";;; Saved strokes for %s, as of %s\n\n"
                             (user-full-name)
                             (format-time-string "%B %e, %Y" nil)))
      ;; Save strokes maps
      (mapc #'(lambda (smap)
                (insert-string
                 (format "(setq %S '%S)\n\n" smap (symbol-value smap))))
            '(xwem-strokes-global-map xwem-strokes-maps))
      (write-region (point-min) (point-max) (or file xwem-strokes-file)))))

(define-xwem-command xwem-strokes-load (&optional file)
  "Load user defined strokes from FILE.
By default FILE is `xwem-strokes-file'."
  (xwem-interactive (list xwem-strokes-file))
  (setq file (or file xwem-strokes-file))
  (if (and (file-exists-p file) (file-readable-p file))
      (load-file file))
  (put 'xwem-strokes 'loaded t))

(defun xwem-strokes-cmd-valid-p (cmd)
  "Return non-nil if CMD is valid to be run after stroke."
  ;; NOTE: string/keyboard macro/interactive function/interactive
  ;; lambda are all valid commands.
  (or (commandp cmd)
      (consp cmd)))

(defun xwem-strokes-cmd-type (cmd)
  "Return type of strokes command CMD.
Type is one of \"string\", \"keys\" or \"command\"."
  (cond ((stringp cmd) "string")
        ((vectorp cmd) "keys")
        ((listp cmd) "form")
        ((or (xwem-command-p cmd)
             (commandp cmd)) "command")
        (t "unknown")))

(defun xwem-strokes-cmd-description (cmd)
  "Return description of strokes command CMD."
  (let ((ctype (xwem-strokes-cmd-type cmd)))
    (cond ((string= ctype "string") (concat "\"" cmd "\""))
          ((string= ctype "keys") (key-description cmd))
          ((or (string= ctype "command")
               (string= ctype "form")) (format "%S" cmd))
          (t (format "%S" cmd)))))

(defun xwem-strokes-read-command (&optional prompt)
  "Read command, prompting PROMPT.
Command may be interactive command, key or string."
  (condition-case nil
      (let* ((type (xwem-completing-read
                    "XWEM Stroke type: "
                    '(("command") ("string") ("keys") ("form"))
                    nil t "command"))
             (cmd (cond ((string= type "command")
                         (xwem-read-command "XWEM Stroke command: "))
                        ((string= type "string")
                         (xwem-read-from-minibuffer "XWEM Stroke string: "))
                        ((string= type "keys")
                         (let ((xwem-interactively t))
                           (prog1
                               (key-sequence-list-description
                                (xwem-read-key-sequence "XWEM Stroke key: "))
                             (xwem-kbd-stop-grabbing))))
                        ((string= type "form")
                         (xwem-read-from-minibuffer
                          "XWEM Stroke form: " nil read-expression-map
                          t 'xwem-read-expression-history))
                        (t nil))))
        cmd)
    (t nil)))

(defun xwem-strokes-execute-command (cmd)
  "Execute CMD.  CMD is one of interactive command or keysequence."
  (if (xwem-strokes-cmd-valid-p cmd)
      (cond ((stringp cmd)
             (xwem-kbd-force-mods-release)
             (xwem-kbd-add-pending-keys cmd (xwem-cl-selected)))

            ((vectorp cmd)
             (xwem-kbd-force-mods-release)
             (setq xwem-this-command-keys []) ; XXX
             (xwem-keymacro-execute-keys cmd))
            
            ((listp cmd)
             ;; Execute form
             (eval cmd))

            (t (command-execute cmd)))
    (xwem-message 'error "Invalid strokes command `%S'" cmd)))

;;;###autoload(autoload 'xwem-strokes-execute-last-stroke "xwem-strokes" "" t)
(define-xwem-command xwem-strokes-execute-last-stroke (arg)
  "Execute last stroke."
  (xwem-interactive "_p")
  (xwem-strokes-execute-stroke xwem-strokes-last-stroke arg))

(defun xwem-strokes-lookup-stroke (stroke)
  "Lookup STROKE in local and global maps."
  (when (and (null (xwem-strokes-maps))
             (null (xwem-strokes-global-map)))
    (error 'xwem-error "No strokes defined"))

  ;; First lookup stroke in local maps,
  ;; If not found then lookup in global map
  (flet ((matched-stroke (s m)
           (let* ((match (strokes-match-stroke s m))
                  (score (cdr match)))
             (and match
                  (<= score xwem-strokes-minimum-match-score)
                  match))))
    (let ((lrs (cons (matched-stroke stroke (xwem-strokes-global-map))
                     (mapcar #'(lambda (m)
                                 (when (xwem-cl-match-p (xwem-cl-selected) (car m))
                                   (matched-stroke stroke (cdr m))))
                             (xwem-strokes-maps)))))
      ;; Select matched stroke with best score
      (car (sort (delq nil lrs) #'(lambda (a b)
                                    (> (cdr a) (cdr b))))))))

(defun xwem-strokes-execute-stroke (stroke &optional n)
  "Given STROKE, execute the command corresponds to it.
If optional N is given then execute STROKE N times."
  (setq xwem-strokes-last-stroke stroke)

  (let* ((match (xwem-strokes-lookup-stroke stroke))
         (command (car (car match)))
         (commentary (cdr (car match))))
    (cond ((strokes-click-p stroke)
           (dotimes (i (or n 1))
             (command-execute xwem-strokes-click-command)))

          ((null match)
           (error 'xwem-error "No matches; see `xwem-strokes-minimum-match-score'"))
          (t
           (xwem-message 'info "Executing %sstroke: '%s' ;; %s"
                         (if (numberp n)
                             (format "%d times " n)
                           "")
                         (xwem-strokes-cmd-description command)
                         commentary)
           (xwem-deferred-funcall
            `(lambda ()
               (dotimes (i (or ,n 1))
                 (xwem-strokes-execute-command ',command))))))))

(defun xwem-strokes-define-stroke (st &optional local submap)
  "Define stroke ST.
If LOCAL is non-nil then define local stroke in SUBMAP.
Return non-nil if new stroke was successfully defined."
  (let* ((cmdorstr (xwem-strokes-read-command))
         (commentary  (and cmdorstr (xwem-read-from-minibuffer
                                     "XWEM Stroke commentary: "))))
    (when (xwem-strokes-cmd-valid-p cmdorstr)
      (if (not local)
          (strokes-define-stroke xwem-strokes-global-map st
                                 (cons cmdorstr commentary))
        (let* ((sm (or submap
                       (find (xwem-cl-selected) (xwem-strokes-maps)
                             :test #'(lambda (scl xsm)
                                       (xwem-cl-match-p scl (car xsm))))))
               (ssm (cdr sm)))
          (strokes-define-stroke ssm st (cons cmdorstr commentary))
          (if sm
              (setcdr sm ssm)
            (push (cons (xwem-client-guess-qualifier (xwem-cl-selected)) ssm)
                  xwem-strokes-maps))))
      ;; return non-nil
      'new-stroke-defined)))
  
(defun xwem-strokes-define-or-execute (st)
  "Bind stroke ST to command or execute."
  (if (not xwem-strokes-defining)
      ;; Execute stroke
      (xwem-strokes-execute-stroke st)

    ;; Attach command to stroke
    (setq xwem-strokes-defining nil)
    (when (xwem-strokes-define-stroke
           st (xwem-y-or-n-p "Define as local stroke? "))
      (xwem-message 'info "New stroke defined."))))

(defun xwem-strokes-start-new (x y)
  "Start new stroke or new stick at X Y point."
  (push (cons x y) xwem-strokes-curr)
  (setq xwem-strokes-background-mode
        (xwem-misc-xwin-background-mode (xwem-rootwin) x y))
  (XDrawArc (xwem-dpy) (xwem-rootwin)
            (xwem-face-get-gc 'xwem-strokes-face
              (list 'background 'begin xwem-strokes-background-mode))
            x y 1 1 0 (* 360 64)))

(defun xwem-strokes-continue (x y)
  "Continue stroke at X Y."
  (let* ((xsc xwem-strokes-curr)
         (old-x (if (car xsc) (X-Point-x (car xsc)) x))
         (old-y (if (car xsc) (X-Point-y (car xsc)) y)))

    (push (cons x y) xwem-strokes-curr)
    (XDrawLine (xwem-dpy) (xwem-rootwin)
               (xwem-face-get-gc 'xwem-strokes-face
                 (list 'background xwem-strokes-background-mode))
               old-x old-y x y)))

;;;###autoload(autoload 'xwem-strokes-define "xwem-strokes" "" t)
(define-xwem-command xwem-strokes-define (arg)
  "Define new stroke.
If used with prefix ARG, then define new complex stroke.
See also documentation for the `strokes-define-stroke' function."
  (xwem-interactive "P")

  (if arg
      (xwem-strokes-cmplx-begin '(4))
    (xwem-strokes-begin '(4))))

(defun xwem-strokes-motion (xev)
  "Handles motion notify event XEV."
  (if (strokes-lift-p (car xwem-strokes-curr))
      ;; Before new stick in complex stroke, should not happen because
      ;; we select for ButtonMotion, i.e. report motion events only
      ;; when some button pressed.
      nil
    (xwem-strokes-continue
     (X-Event-xmotion-root-x xev) (X-Event-xmotion-root-y xev))))

;;; Interactive commands

;;;###autoload(autoload 'xwem-strokes-ibutton1 "xwem-strokes" "" t)
(define-xwem-command xwem-strokes-ibutton1 ()
  "On button1 command."
  (xwem-interactive)

  (declare (special xwem-stroke-complexp))
  (declare (special xwem-stroke-done))

  (xwem-strokes-start-new (X-Event-xbutton-root-x xwem-last-xevent)
                          (X-Event-xbutton-root-y xwem-last-xevent)))

;;;###autoload(autoload 'xwem-strokes-idescribe "xwem-strokes" "" t)
(define-xwem-command xwem-strokes-idescribe ()
  "Describe current stroke."
  (xwem-interactive)

  (declare (special xwem-stroke-complexp))
  (declare (special xwem-stroke-done))

  (xwem-strokes-describe-current))

;;;###autoload(autoload 'xwem-strokes-ibutton2up "xwem-strokes" "" t)
(define-xwem-command xwem-strokes-ibutton2up ()
  "On button2 up command."
  (xwem-interactive)
  (declare (special xwem-stroke-complexp))
  (unless xwem-stroke-complexp
    (xwem-strokes-ibutton1up)))

;;;###autoload(autoload 'xwem-strokes-ibutton1up "xwem-strokes" "" t)
(define-xwem-command xwem-strokes-ibutton1up ()
  "On button1 up command."
  (xwem-interactive)

  (declare (special xwem-stroke-complexp))
  (declare (special xwem-stroke-done))

  (if (not xwem-stroke-complexp)
      (progn (setq xwem-strokes-curr (nreverse xwem-strokes-curr))
             (setq xwem-stroke-done t))

    (push strokes-lift xwem-strokes-curr)))

;;;###autoload(autoload 'xwem-strokes-ibutton3 "xwem-strokes" "" t)
(define-xwem-command xwem-strokes-ibutton3 ()
  "On button3 up command."
  (xwem-interactive)

  (declare (special xwem-stroke-complexp))
  (declare (special xwem-stroke-done))

  (setq xwem-strokes-curr (nreverse (cdr xwem-strokes-curr)))
  (when (strokes-lift-p (car xwem-strokes-curr))
    (setq xwem-strokes-curr (cdr xwem-strokes-curr)))
  (setq xwem-stroke-done t))

;;;###autoload(autoload 'xwem-strokes-nocmd "xwem-strokes" "" t)
(define-xwem-command xwem-strokes-nocmd ()
  "Command to do nothing."
  (xwem-interactive))

(defun xwem-strokes-go (complexp)
  "Install stuff needed to handle stroke input.
If COMPLEXP is non-nil than setup all for complex stroke input."
  ;; TODO:
  ;;   - Check whether there already button release event, so we wont
  ;;     start stroke in that case, requires `XPeekIfEvent' to be implemented.
  (if xwem-strokes-click-next-allow
      (progn
        (XAllowEvents (xwem-dpy) X-ReplayPointer)
        (setq xwem-strokes-click-next-allow nil))

    ;; Grab pointer
    (xwem-mouse-grab xwem-strokes-cursor (xwem-rootwin)
                     (Xmask-or XM-ButtonPress XM-ButtonRelease XM-ButtonMotion))

    (setq xwem-strokes-curr nil)
    (xwem-strokes-start-new (X-Event-xbutton-root-x xwem-last-xevent)
                            (X-Event-xbutton-root-y xwem-last-xevent))
    ;; Event loop
    (xwem-unwind-protect
        (let ((gc-cons-threshold most-positive-fixnum) ; inhibit GC'ing
              (xwem-override-local-map xwem-strokes-keymap) ; override local keymap
              (xwem-keyboard-echo-keystrokes nil) ; Do not show
              (xwem-stroke-complexp complexp)
              (xwem-stroke-done nil)
              xev)
          (declare (special xwem-stroke-done))
          (declare (special xwem-stroke-complexp))

          (while (not xwem-stroke-done)
            (X-Event-CASE (setq xev (xwem-next-event))
              (:X-MotionNotify
               (xwem-strokes-motion xev))
              ((:X-KeyPress :X-ButtonPress :X-ButtonRelease)
               (xwem-dispatch-command-xevent xev)))))
      (xwem-strokes-done))

    ;; Execute or define stroke
    (let* ((grid-locs (strokes-renormalize-to-grid
                       xwem-strokes-curr xwem-strokes-grid))
           (st (strokes-fill-stroke
                (strokes-eliminate-consecutive-redundancies grid-locs))))
      (xwem-strokes-define-or-execute st))))

(defun xwem-strokes-done (&optional xev)
  "Uninstall stuff installed by `xwem-strokes-go'.
XEV is last processed X-Event."
  (xwem-mouse-ungrab t)

  (let ((xsc (or xwem-strokes-curr
                 (list (or (and xev (cons (X-Event-xbutton-root-x xev)
                                          (X-Event-xbutton-root-y xev)))
                           (cons 0 0)))))
        (x 10000000) (y 1000000) (xma 0) (yma 0)
        (thi (* 2 (max (xwem-face-line-width 'xwem-strokes-face
                                             '(background begin light))
                       (xwem-face-line-width 'xwem-strokes-face
                                             '(background begin dark))
                       (xwem-face-line-width 'xwem-strokes-face)))))

    (while xsc
      (while (not (consp (car xsc)))
        ;; cut off lifts
        (setq xsc (cdr xsc)))

      (when (< (caar xsc) x)
        (setq x (caar xsc)))
      (when (> (caar xsc) xma)
        (setq xma (caar xsc)))

      (when (< (cdar xsc) y)
        (setq y (cdar xsc)))
      (when (> (cdar xsc) yma)
        (setq yma (cdar xsc)))

      (setq xsc (cdr xsc)))

    (xwem-root-refresh
     (- x thi) (- y thi) (+ (- xma x) (* 2 thi)) (+ (- yma y) (* thi 2)))))

(defun xwem-strokes-describe-current ()
  "Describe current complex stroke."
  (let ((pix-lock (copy-list xwem-strokes-curr)) ; because of nreverse
        (match nil))

    (setq pix-lock (nreverse (cdr pix-lock)))
    (when (strokes-lift-p (car pix-lock))
      (setq pix-lock (cdr pix-lock)))

    (setq match (strokes-match-stroke
                 (strokes-fill-stroke
                  (strokes-eliminate-consecutive-redundancies
                   (strokes-renormalize-to-grid pix-lock xwem-strokes-grid)))
                 (xwem-strokes-global-map)))

    (xwem-message 'info "Current stroke executes: %S" (car match))))

;;;###autoload(autoload 'xwem-strokes-begin "xwem-strokes" "" t)
(define-xwem-command xwem-strokes-begin (arg)
  "Begin to input simple stroke.
If prefix ARG is given start to define simple stroke."
  (xwem-interactive "P")

  (when arg
    (setq xwem-strokes-defining t))

  (xwem-strokes-go nil))

;;;###autoload(autoload 'xwem-strokes-cmplx-begin "xwem-strokes" "" t)
(define-xwem-command xwem-strokes-cmplx-begin (arg)
  "Begin to input complex stroke(i.e. which has more than one strokes).
If prefix ARG is given start to define new complex stroke."
  (xwem-interactive "P")

  (when arg
    (setq xwem-strokes-defining t))

  (xwem-strokes-go t))

;;;###autoload(autoload 'xwem-strokes-unset-last-stroke "xwem-strokes" "" t)
(define-xwem-command xwem-strokes-unset-last-stroke (arg)
  "Removes last stroke from `xwem-strokes-global-map'.
With prefix ARG do not require a confirmation"
  (xwem-interactive
   (list (or xwem-prefix-arg
             (y-or-n-p (format "XWEM: Really delete last stroke (%s)? "
                               (xwem-strokes-cmd-description
                                (cdr (car (xwem-strokes-global-map)))))))))
  (if (not arg)
      (xwem-message 'info "Nothing done.")
    (setq xwem-strokes-global-map
          (cdr (xwem-strokes-global-map)))
    (xwem-message 'info "Last stroke has been deleted.")))

(defun xwem-strokes-sort (smap &optional how)
  "Sort strokes map SMAP.
HOW methods are not implemented."
  (sort (copy-list smap)
        #'(lambda (s1 s2)
            (let ((c1 (cdr s1))
                  (c2 (cdr s2)))
              (cond ((and (stringp c1) (stringp c2))
                     (string-lessp c1 c2))
                    ((and (symbolp c1) (symbolp 2))
                     (string-lessp (symbol-name c1) (symbol-name c2)))
                    ((stringp c1) t)
                    ((and (vectorp c1) (not (stringp c2))) t)
                    ((and (symbolp c1) (not (stringp c2)) (not (vectorp c2))) t)
                    ((and (functionp c1) (not (stringp c2))
                          (not (vectorp c2)) (not (symbolp c2))) t))))))

(defun xwem-stroke-generate-xpm (stroke)
  "Return tiny XPM picture for the STROKE."
  (strokes-xpm-for-stroke stroke " *strokes-xpm*")
  (buffer-substring nil nil " *strokes-xpm*"))

(defvar xwem-strokes-list-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parents map (list view-mode-map))
    (define-key map [?d] 'xwem-strokes-list-delete)
    (define-key map [?e] 'xwem-strokes-list-edit)
    (define-key map [?q] 'xwem-strokes-list-quit)
    (define-key map [?s] 'xwem-strokes-save)
    (define-key map [?w] 'xwem-strokes-copy-strokes)
    (define-key map [?y] 'xwem-strokes-paste-strokes)
    map)
  "Keymap for strokes list mode.")

(defun xwem-strokes-copy-strokes (s e)
  "Copy strokes in region from S to E."
  (interactive "r")
  (declare (special xwem-strokes-copied-strokes))
  (let (sts)
    (loop for sp from s below e
      do
      (let ((st (get-text-property sp 'stroke)))
        (when (and st (not (memq st sts)))
          (push st sts))))
    (setq xwem-strokes-copied-strokes sts)))

(defun xwem-insert-after (list aft-el el)
  "In LIST after AFT-EL insert EL."
  (push el (cdr (member aft-el list)))
  list)

(defun xwem-strokes-paste-strokes ()
  "Paste previously copied with `xwem-strokes-copy-strokes' strokes."
  (interactive)
  (declare (special xwem-strokes-copied-strokes))
  (let ((stroke (get-text-property (point) 'stroke))
        (submap (get-text-property (point) 'strokes-submap)))
    (flet ((sia (l a e)
             ;; Helper function to insert items E int list L after element A
             (setf (cdr (memq a l)) (append e (cdr (memq a l))))
             l))
      (if submap
          (setcdr submap (sia (cdr submap) stroke xwem-strokes-copied-strokes))
      (setq xwem-strokes-global-map
        (sia xwem-strokes-global-map stroke xwem-strokes-copied-strokes)))))
  (xwem-strokes-list nil))

(defun xwem-strokes-delete-stroke (stroke &optional submap)
  "Delete STROKE from SUBMAP or from global map."
  (if submap
      (setcdr submap (delq stroke (cdr submap)))
    (setq xwem-strokes-global-map
          (delq stroke xwem-strokes-global-map))))

(defun xwem-strokes-list-delete ()
  "Delete stroke at current position."
  (interactive)
  (let ((stroke (get-text-property (point) 'stroke))
        (submap (get-text-property (point) 'strokes-submap)))
    (xwem-strokes-delete-stroke stroke submap)
    (xwem-strokes-list nil)))

(defun xwem-strokes-list-edit ()
  "Edit stroke at current position."
  (interactive)
  (let ((stroke (get-text-property (point) 'stroke))
        (submap (get-text-property (point) 'strokes-submap)))
    (unless stroke
      (error "No stroke at point"))
    (and (xwem-strokes-define-stroke (car stroke) submap submap)
         (xwem-strokes-delete-stroke stroke submap))
    (xwem-strokes-list nil)))

(defun xwem-strokes-list-quit ()
  "Quit strokes list."
  (interactive)
  (kill-buffer (current-buffer)))

(defun xwem-strokes-list-insert-stroke (stdef)
  "Insert stroke defined by STDEF."
  (let ((stroke (car stdef))
        (stroke-cmd (car (cdr stdef)))
        (stroke-commentary (cdr (cdr stdef))))
    ;; Stroke
    (insert " ")
    (set-extent-begin-glyph
     (make-extent (point) (point))
     (make-glyph
      (list
       (vector 'xpm :data (xwem-stroke-generate-xpm stroke))
       [string :data "[Image]"])))

    ;; Type
    (insert-face (format "%9s"(xwem-strokes-cmd-type stroke-cmd))
                 (if (xwem-strokes-cmd-valid-p stroke-cmd)
                     nil 'red))

    ;; Command
    (insert-face "  "
                 (if (xwem-strokes-cmd-valid-p stroke-cmd)
                     nil 'red))
    (insert-face (xwem-strokes-cmd-description stroke-cmd)
                 (if (xwem-strokes-cmd-valid-p stroke-cmd)
                     nil 'red))
    (unless (string= stroke-commentary "")
      (when (< (current-column) 42)
        (move-to-column 42 t))
      (insert-face (format " ;; %s" stroke-commentary)
                   (if (xwem-strokes-cmd-valid-p stroke-cmd)
                       nil 'red)))

    (put-text-property (point-at-bol) (point-at-eol)
                       'stroke stdef)
    (insert "\n")
    ))

;;;###autoload(autoload 'xwem-strokes-list "xwem-strokes" "" t)
(define-xwem-command xwem-strokes-list (arg)
  "List strokes defined for XWEM use.
With prefix ARG sort strokes by command (NOT IMPLEMENTED YET)."
  (xwem-interactive "P")

  (let ((stb (get-buffer "*XWEM Strokes*"))
        saved-point)
    (unless stb
      (xwem-special-popup-frame
       (setq stb (get-buffer-create "*XWEM Strokes*"))))
    (with-current-buffer stb
      (setq saved-point (point))
      (setq buffer-read-only nil)
      (erase-buffer)
      ;; Last stroke
      (insert-face "Last stroke: " 'header-line)
      (set-extent-begin-glyph
       (make-extent (point) (point))
       (make-glyph
        (list
         (vector 'xpm :data (xwem-stroke-generate-xpm xwem-strokes-last-stroke))
         [string :data "[Image]"])))
      (insert-face ", " 'header-line)
      ;; Insert keys annotation
      (map-keymap #'(lambda (k b)
                      (insert-face
                       (format "%S: %s, "
                               k (car (split-string
                                       (function-documentation b) " ")))
                       'header-line))
                  xwem-strokes-list-map)
      (backward-delete-char 2)          ; remove trailing ', '
      (insert "\n")
      (insert
       "Stroke  Type  Stroke\n"
       "------  ----  -------\n")
      (insert "== Global ==\n")
      ;; Adjust saved-point
      (when (= saved-point (point-min))
        (setq saved-point (point)))
      (mapc #'xwem-strokes-list-insert-stroke
            (if arg
                (xwem-strokes-sort (xwem-strokes-global-map))
              (xwem-strokes-global-map)))
      (insert "\n")

      ;; Now insert locals
      (mapc #'(lambda (submap)
                (insert (format "-- Local %S --\n" (car submap)))
                (let ((spoint (point)))
                  (mapc #'xwem-strokes-list-insert-stroke
                        (if arg
                            (xwem-strokes-sort (cdr submap))
                          (cdr submap)))
                  (put-text-property spoint (point) 'strokes-submap submap))
                (insert "\n"))
            xwem-strokes-maps)

      (kill-region (min (1+ (point)) (point-max)) (point-max))

      (condition-case nil
          (goto-char saved-point)
        (t (goto-char (point-min))))

      ;; Enable xwem-strokes-list-mode
      (setq major-mode 'xwem-strokes-list-mode)
      (setq buffer-read-only t)
      (use-local-map xwem-strokes-list-map)
      )))

(defun xwem-strokes-init ()
  "Initialize strokes support."
  (xwem-message 'init "Initializing strokes ...")

  (setq xwem-strokes-curr nil)
  (setq  xwem-strokes-cursor
         (xwem-make-cursor xwem-strokes-cursor-type
                           xwem-strokes-cursor-foreground-color
                           xwem-strokes-cursor-background-color))

  (xwem-message 'init "Initializing strokes ... done"))


(if xwem-started
    (xwem-strokes-init)
  (add-hook 'xwem-after-init-hook 'xwem-strokes-init))


(provide 'xwem-strokes)

;;; xwem-strokes.el ends here
