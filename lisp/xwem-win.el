;;; xwem-win.el --- Windows ops for XWEM.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: 21 Mar 2003
;; Keywords: xlib, xwem
;; Time-stamp: <11/12/2006 23:30:31 lg@h1>

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
;; This file contain operations on XWEM windows.  Window is part of
;; Frame, Window holds X client - CL.
;;

;;; Code

(require 'xwem-load)

;;; Customisation
(defgroup xwem-win nil
  "Group to customize XWEM windows."
  :prefix "xwem-win-"
  :group 'xwem)

;;;###autoload
(defcustom xwem-win-min-width 80
  "*Minimal width for window"
  :type 'number
  :group 'xwem-win)

;;;###autoload
(defcustom xwem-win-min-height 80
  "*Minimal height for window"
  :type 'number
  :group 'xwem-win)

;;;###xwem-autoload
(defcustom xwem-win-vertical-delim-width '(8 . 1)
  "*Width in pixels for vertical delimiters.
car is delimiter width, cdr is shadow thickness."
  :type '(cons number number)
  :group 'xwem-win)

;;;###xwem-autoload
(defcustom xwem-win-horizontal-delim-width '(6 . 1)
  "*Width in pixels for horizontal delimiters.
car is delimiter width, cdr is shadow thickness."
  :type '(cons number number)
  :group 'xwem-win)

(defcustom xwem-win-default-border-width 1
  "*Default border width for newly created windows."
  :type 'number
  :group 'xwem-win)

(defcustom xwem-win-default-properties nil
  "*Default properties list for frame windows.

dead       - window that referenced but not workable.
deleted    - deleted window have this equal to t
frame      - frame window attached to.
next       - next window in windows double linked list.
prev       - previous window.
hchild     - child window after horizontal split.
vchild     - child window after vertical split.
parent     - parent window (window in which we do split).
client     - client currently active in window. (index in clients-list)
"
  :type '(restricted-sexp :match-alternatives (valid-plist-p))
  :group 'xwem-win)

(defcustom xwem-win-winmove-allow-jumping t
  "*Non-nil allows jumping to opposite edge, when no window founded."
  :type 'boolean
  :group 'xwem-win)

(defcustom xwem-win-collect-deleted-clients t
  "*Non-nil mean clients managed in window that are deleting now, will
be placed in `xwem-window-other' window."
  :type 'boolean
  :group 'xwem-win)

(defcustom xwem-win-max-clients 32
  "*Maximum number of clients in window.
NOT USED."
  :type 'number
  :group 'xwem-win)


;;; Hooks
(defcustom xwem-win-switch-hook nil
  "Hooks to be called when window switching occurs.
Function will receive two arguments OLD-WIN and NEW-WIN."
  :type 'hook
  :group 'xwem-hooks)

(defcustom xwem-win-delete-hook nil
  "Hooks called with one arg - window when deleting window."
  :type 'hook
  :group 'xwem-hooks)

(defcustom xwem-win-clients-change-hook nil
  "Hooks called when win's clients list changed."
  :type 'hook
  :group 'xwem-hooks)

;;;###autoload
(defcustom xwem-win-split-hook nil
  "Hook to be called after window split.
Functions will be called with two arguments: SPLIT-WIN, NEW-WIN."
  :type 'hook
  :group 'xwem-hooks)

(define-xwem-face xwem-window-outline-face
  `(((frame-selected win-selected)
     (:foreground "green" :background "green4" :line-width 4))
    ((frame-selected win-nonselected)
     (:foreground "gray70" :background "gray70" :line-width 4))
    ((frame-nonselected win-selected)
     (:foreground "green3" :background "green4" :line-width 4))
    ((frame-nonselected win-nonselected)
     (:foreground "gray60" :background "gray40" :line-width 4)))
  "Face to outline frame windows."
  :group 'xwem-win
  :group 'xwem-faces)

(define-xwem-face xwem-window-delimiter-face
  `(((horizontal)
     (:foreground "royalblue"))
    ((horizontal shadow)
     (:foreground "blue4"))
    ((horizontal light shadow)
     (:foreground "cyan"))
    ((vertical)
     (:foreground "royalblue"))
    ((shadow vertical)
     (:foreground "blue4"))
    ((light shadow vertical)
     (:foreground "cyan"))
    (t (:foreground "gray20" :background "black")))
  "Face to draw window delimiter."
  :group 'xwem-win
  :group 'xwem-faces)

;;; Internal variables


;;;; Win macros
(defmacro xwem-win-child (window)
  "Return child of WINDOW, hchild checked first then if not set vchild
  tested."
  `(or (xwem-win-hchild ,window) (xwem-win-vchild ,window)))

(defmacro xwem-win-mark-deleted (win)
  "Mark WIN as deleted window."
  `(setf (xwem-win-deleted ,win) t))

;;;###xwem-autoload
(defun xwem-win-delim-width (window)
  "Return WIN's delimiter width."
  (let ((pwin (xwem-win-parent window)))
    (or (and pwin (xwem-win-hchild pwin) (car xwem-win-horizontal-delim-width))
        (and pwin (xwem-win-vchild pwin) (car xwem-win-vertical-delim-width))
        0)))

;;;###xwem-autoload
(defun xwem-win-delim-shadow-thickness (window)
  "Return WIN's delimiter width."
  (let ((pwin (xwem-win-parent window)))
    (or (and pwin (xwem-win-hchild pwin) (cdr xwem-win-horizontal-delim-width))
        (and pwin (xwem-win-vchild pwin) (cdr xwem-win-vertical-delim-width))
        0)))


;;;; Functions
;;;###xwem-autoload
(defun xwem-win-make-list-by-next (window)
  "Create list of WINDOW and all next windows."
  (let ((wins window)
        rlist)
    (while (xwem-win-p wins)
      (setq rlist (cons wins rlist))
      (setq wins (xwem-win-next wins)))
    (nreverse rlist)))

;;;###xwem-autoload
(defun xwem-win-num (win)
  "Return relative WIN's number in logical window list."
  (let ((ch (xwem-win-child (xwem-frame-rootwin (xwem-win-frame win))))
        (idx 0))
    (when (xwem-win-p ch)
      (while (not (xwem-win-alive-p ch))
        (setq ch (xwem-win-child ch)))
      ;; CH is very first window in frame
      (while (not (eq ch win))
        (setq idx (1+ idx)
              ch (xwem-window-next ch))))
    idx))

;;;###xwem-autoload
(defun xwem-win-find-by-num (frame num)
  "In FRAME find window for which `xwem-win-num' returns NUM."
  (let* ((fch (xwem-win-child (xwem-frame-rootwin frame)))
         (ch fch))
    (if (not (xwem-win-p ch))
        (and (= num 0) (xwem-frame-rootwin frame))

      (while (not (xwem-win-alive-p ch))
        (setq ch (xwem-win-child ch)))
      ;; CH is very first window in frame
      (while (and (> num 0) ch)
        (setq ch (xwem-window-next ch))
        (if (eq ch fch)
            (setq ch nil)
          (decf num)))
      ch)))

;; We do want closures in emacs lisp
(defvar xwem-win-next-id 0)
(defun xwem-win-gen-id ()
  "Generate new window id."
  (truncate (+ (X-Dpy-resource-base (xwem-dpy))
            (X-Dpy-resource-id (xwem-dpy))
            (incf xwem-win-next-id))))

(defun xwem-win-hacked-or (&rest args)
  "Like `or', but can by used under `apply'."
  (while (and args (null (car args)))
    (setq args (cdr args)))
  (car args))

;;;###xwem-autoload
(defun xwem-win-find-by-id (win-id &optional win)
  "Find a window with WIN-ID."
  (cond ((and (xwem-win-p win)
              (= (xwem-win-id win) win-id))
         win)

        ((xwem-win-p win)
         (or (apply 'xwem-win-hacked-or
                    (mapcar #'(lambda (w)
                                (xwem-win-find-by-id win-id w))
                            (xwem-win-make-list-by-next (xwem-win-child win))))
             (apply 'xwem-win-hacked-or
                    (mapcar #'(lambda (w)
                                (xwem-win-find-by-id win-id w))
                            (delq win (xwem-win-make-list-by-next win))))))

        (t (apply 'xwem-win-hacked-or
                  (mapcar #'(lambda (w)
                              (xwem-win-find-by-id win-id w))
                          (mapcar #'xwem-frame-rootwin
                                  xwem-frames-list))))))

;;;###xwem-autoload
(defun xwem-cl-set-win (cl win)
  "Associate CL with WIN.
WIN is valid WIN or nil."
  (unless (eq (xwem-cl-win cl) win)
    (let ((owin (xwem-cl-win cl)))
      ;; Deactivate CL, when changing window
      (xwem-deactivate cl)

      (setf (xwem-cl-win cl) win)

      ;; Set also client property
      (xwem-client-set-property
       cl 'client-window (and (xwem-win-p win) (xwem-win-id win)))

      ;; Remove CL from OWIN's clients list
      (when (xwem-win-p owin)
        (xwem-win-rem-cl owin cl))))

  ;; Add CL to WIN's clients list
  (when (xwem-win-p win)
    (xwem-win-add-cl win cl)))

;;;###xwem-autoload
(defun xwem-win-add-cl (win cl)
  "Into WIN's clients list add new client CL."
  (unless (or (not (xwem-cl-p cl))
              (memq cl (xwem-win-clients win)))

    (when (and (xwem-frame-dedicated-p (xwem-win-frame win))
               (xwem-win-clients win))
      (error 'xwem-error "Window in dedicted frame already has client"))

    ;; Insert CL in WIN's clients list in proper place (as in
    ;; `xwem-clients')
    (let ((wcls (xwem-win-clients win)))
      (while (and wcls (memq cl (memq (car wcls) xwem-clients)))
        (setq wcls (cdr wcls)))
      (if (not wcls)
          (setf (xwem-win-clients win)
                (append (xwem-win-clients win) (list cl)))
        (setcdr wcls (cons (car wcls) (cdr wcls)))
        (setcar wcls cl)))

    (run-hook-with-args 'xwem-win-clients-change-hook win)))

;;;###xwem-autoload
(defun xwem-win-rem-cl (win cl)
  "From WIN's clients list remove client CL."
  (when (and (xwem-cl-p cl)
             (memq cl (xwem-win-clients win)))
    (setf (xwem-win-clients win)
          (delq cl (xwem-win-clients win)))

    ;; If CL is current client in WIN, also unset it
    (when (eq cl (xwem-win-cl win))
      (xwem-win-set-cl win nil))

    (run-hook-with-args 'xwem-win-clients-change-hook win)))

;;;###xwem-autoload
(defun xwem-win-set-cl (win cl)
  "Associate WIN with CL as current client in WIN."
  ;; When CL isnt in WIN's clients list yet, add it.
  (unless (eq (xwem-win-cl win) cl)
    (when (xwem-cl-p cl)
      (xwem-cl-set-win cl win))

    (let ((ocl (xwem-win-cl win)))
      (setf (xwem-win-cl win) cl)

      (when (xwem-cl-alive-p (xwem-win-cl win))
        (xwem-activate (xwem-win-cl win)))
      (when (xwem-cl-alive-p ocl)
        (xwem-deactivate ocl))

      (when (or (null ocl) (null (xwem-win-cl win)))
        (xwem-win-redraw-win win)))))

;;;###autoload
(defun xwem-win-new (&optional params props)
  "Create new window with properties PROPS."
  (let ((nwin (apply 'make-xwem-win :id (xwem-win-gen-id) params))
        (rplist (copy-list xwem-win-default-properties)))

    ;; Prepare window properties
    (setq rplist (xwem-misc-merge-plists rplist props))

    (setf (xwem-win-geom nwin)
          (make-X-Geom :x 0 :y 0 :width 1 :height 1
                       :border-width xwem-win-default-border-width))
    (setf (xwem-win-clients nwin) nil)  ; list of clients
    (setf (xwem-win-cl nwin) nil)       ; no visible client yet
    (setf (xwem-win-plist nwin) rplist) ; window properties

    nwin))

(defun xwem-win-replace (old new)
  "Replace OLD window with contents of NEW window."
  (when (not (and (xwem-win-p old) (xwem-win-p new)))
    (error 'xwem-error "Hmm .. one of OLD or NEW is not a xwem window"))

  (when (eq (xwem-frame-rootwin (xwem-win-frame old)) old)
    (setf (xwem-frame-rootwin (xwem-win-frame old)) new))

  (setf (xwem-win-x new) (xwem-win-x old))
  (setf (xwem-win-y new) (xwem-win-y old))
  (setf (xwem-win-width new) (xwem-win-width old))
  (setf (xwem-win-height new) (xwem-win-height old))

  (let ((tem))
    (setq tem (xwem-win-next old))
    (setf (xwem-win-next new) tem)
    (when (xwem-win-p tem)
      (setf (xwem-win-prev tem) new))

    (setq tem (xwem-win-prev old))
    (setf (xwem-win-prev new) tem)
    (when (xwem-win-p tem)
      (setf (xwem-win-next tem) new))

    (setq tem (xwem-win-parent old))
    (setf (xwem-win-parent new) tem)
    (when (xwem-win-p tem)
      (when (eq (xwem-win-vchild tem) old)
        (setf (xwem-win-vchild tem) new))
      (when (eq (xwem-win-hchild tem) old)
        (setf (xwem-win-hchild tem) new))
      )))

(defun xwem-win-make-parent (window)
  "Make dummy parent for WINDOW."
  (let ((pwin (xwem-win-new
               (list :frame (xwem-win-frame window)
                     :dead t            ; XXX dead mean that window
                                        ; can't contain clients
                     ))))

    (xwem-win-replace window pwin)

    (setf (xwem-win-next window) nil)
    (setf (xwem-win-prev window) nil)
    (setf (xwem-win-hchild window) nil)
    (setf (xwem-win-vchild window) nil)
    (setf (xwem-win-parent window) pwin)
    ))

;;;###xwem-autoload
(defun xwem-window-next (&optional window)
  "Return next window after WINDOW in canonical ordering of windows.
If omitted, WINDOW defaults to the `(xwem-win-selected)'."
  (let ((win (or window (xwem-win-selected)))
        while-exit tem)

    (while (and (not while-exit)
                (null (setq tem (xwem-win-next win))))
      (if (xwem-win-p (setq tem (xwem-win-parent win)))
          (setq win tem)

        ;;else
        (setq tem (xwem-frame-rootwin (xwem-win-frame win)))
        (setq while-exit t)             ;break from loop
        ))

    (setq win tem)

    ;; now if we have a horizontal or vertical combination - find the
    ;; first child
    (while (cond ((xwem-win-p (xwem-win-child win))
                  (progn (setq win (xwem-win-child win)) t))
                 (t nil)))              ;break
    win))

;;;###xwem-autoload
(defun xwem-window-next-vertical (&optional window)
  "Return next window which is vertically after WINDOW.
If WINDOW is not given `(xwem-win-selected)' will be used."
  (let* ((win (or window (xwem-win-selected)))
         (root-win (xwem-frame-rootwin (xwem-win-frame win)))
         (rwin nil))

    (when (eq root-win win)
      (while (cond ((xwem-win-p (xwem-win-hchild win))
                    (progn (setq win (xwem-win-hchild win)) t))
                   ((xwem-win-p (xwem-win-vchild win))
                    (progn (setq win (xwem-win-vchild win)) t))
                   (t (progn (setq rwin win) nil)))))
    (if rwin
        rwin

      ;; TODO: check for root window in frame
      (while (progn
               (if (and (xwem-win-p (xwem-win-parent win))
                        (xwem-win-p (xwem-win-vchild (xwem-win-parent win))))
                     (if (xwem-win-p (xwem-win-next win))
                         (setq rwin (xwem-win-next win))
                       (setq win (xwem-win-parent win)))
                 (setq win (xwem-win-parent win)))
               (and (null rwin) (not (eq win root-win)))))

      (if rwin
          rwin
        (while (progn
                 (if (xwem-win-p (xwem-win-hchild win))
                     (setq win (xwem-win-hchild win))
                   (if (xwem-win-p (xwem-win-vchild win))
                       (setq win (xwem-win-vchild win))
                     (setq rwin win)))
                 (null rwin)))
        rwin))))

;;;###xwem-autoload
(defun xwem-window-prev (&optional window)
  "Retrun previous window before WINDOW in canonical ordering of windows.
If ommitted, WINDOW defaults to the `(xwem-win-selected)'."
  (let* ((win (or window (xwem-win-selected)))
         while-exit tem)

    (while (and (not while-exit)
                (null (setq tem (xwem-win-prev win))))

      (if (xwem-win-p (setq tem (xwem-win-parent win)))
          (setq win tem)

        (setq tem (xwem-frame-rootwin (xwem-win-frame win)))
        (setq while-exit t)             ;break from loop
        ))

    (setq win tem)

    ;; now if we have a horizontal or vertical combination find
    ;; the first child
    (while (and
            (cond ((xwem-win-p (xwem-win-child win))
                   (progn (setq win (xwem-win-child win)) t))
                  (t nil))              ;break
            (progn
              (while (xwem-win-p (xwem-win-next win))
                (setq win (xwem-win-next win)))
              t)))
    win))

;;;###xwem-autoload
(defun xwem-window-other (cnt &optional window)
  "Return CNT's next window for WINDOW if CNT is greater then zero and
  previous if negative."
  (let ((ofn (if (>= cnt 0) 'xwem-window-next 'xwem-window-prev))
        (cnt (abs cnt))
        (win (or window (xwem-win-selected))))
    (while (> cnt 0)
      (setq win (funcall ofn win))
      (setq cnt (1- cnt)))
    win))

(defun xwem-win-xy-in-p (x y win &optional inc-gutter)
  "Returns non-nil if X Y lies in WIN.
If INC-GUTTER is non-nil, than include gutters width as WIN area."
  (let ((edges (xwem-win-pixel-edges win))
        (gw (if inc-gutter (xwem-win-delim-width win) 0)))
    (and (>= x (- (nth 0 edges) gw))
         (<= x (+ (nth 2 edges) gw))
         (>= y (- (nth 1 edges) gw))
         (<= y (+ (nth 3 edges) gw)))))

(defun xwem-window-at (x y &optional frame)
  "Returns window where X and Y lies in it."
  (catch 'found
    (xwem-win-map #'(lambda (win)
                      (when (xwem-win-xy-in-p x y win t)
                        (throw 'found win)))
                  (xwem-frame-selwin (or frame (xwem-frame-selected))))
    ))

;;; -- Moving around windows --
;;
(defun xwem-winmove-distance (&optional win)
  "Returns distance between windows."
  ;; 2 is XXX
  (+ (xwem-win-delim-width (or win (xwem-win-selected))) 2))

(defun xwem-winmove-refloc (&optional arg window)
  "Calculates the reference location for directional window selection.
Returns cons cell in form (hpos . vpos)."
  (let* ((effarg (if (null arg) 0 (prefix-numeric-value arg)))
         (edges (xwem-win-pixel-edges window))
         (top-left (cons (nth 0 edges) (nth 1 edges)))
         (bot-right (cons (1- (nth 2 edges)) (1- (nth 3 edges)))))
    (cond ((> effarg 0) top-left)
          ((< effarg 0) bot-right)
          (t
           ;; As if point in center of WINDOW
           (cons (+ (nth 0 edges) (/ (- (nth 2 edges) (nth 0 edges)) 2))
                 (+ (nth 1 edges) (/ (- (nth 3 edges) (nth 1 edges)) 2)))))
    ))

(defun xwem-winmove-other-window (dir &optional arg window)
  "Calculates location of window to be moved to.
Returns cons cell (x . y)."
  (let ((edges (xwem-win-pixel-edges window))
        (refpoint (xwem-winmove-refloc arg window)))
    (cond ((eq dir 'left)
           (cons (- (nth 0 edges)
                    (xwem-winmove-distance))
                 (cdr refpoint)))
          ((eq dir 'up)
           (cons (car refpoint)
                 (- (nth 1 edges)
                    (xwem-winmove-distance))))
          ((eq dir 'right)
           (cons (+ (nth 2 edges)
                    (xwem-winmove-distance))
                 (cdr refpoint)))
          ((eq dir 'down)
           (cons (car refpoint)
                 (+ (nth 3 edges)
                    (xwem-winmove-distance))))
          (t (error 'xwem-error "`xwem-winmove-other-window': Invalid direction %s" dir)))))

(defun xwem-winmove-select (dir &optional arg window)
  "Moves to the window in DIR direction."
  (let* ((frame (xwem-win-frame (or window (xwem-win-selected))))
         (owin-loc (xwem-winmove-other-window dir arg window))
         (x (car owin-loc))
         (y (cdr owin-loc))
         (owin (xwem-window-at x y frame)))

    (when xwem-win-winmove-allow-jumping
      (let ((rwin (xwem-frame-rootwin
                   (xwem-win-frame (or window (xwem-win-selected))))))
        (when (not (xwem-win-xy-in-p x y rwin))
          ;; we are outside root window
          (setq owin-loc
                (cond ((eq dir 'left)
                       (cons (- (nth 2 (xwem-win-pixel-edges rwin))
                                (abs x)) y))
                      ((eq dir 'right)
                       (cons (- x (nth 2 (xwem-win-pixel-edges rwin)))
                             y))
                      ((eq dir 'up)
                       (cons x (- (nth 3 (xwem-win-pixel-edges rwin))
                                  (abs y))))
                      ((eq dir 'down)
                       (cons x (+ (- y (nth 3 (xwem-win-pixel-edges rwin)))
                                  (nth 1 (xwem-win-pixel-edges rwin)))))
                      (t (error 'xwem-error "Invalid direction"))))
          (setq owin (xwem-window-at (car owin-loc) (cdr owin-loc) frame)))))

    (if (not (xwem-win-p owin))
        (xwem-message 'error "No window at %S" dir)
      (xwem-select-window owin))
    ))

;;;###autoload(autoload 'xwem-other-window "xwem-win" "Switch to other window." t)
(defalias 'xwem-other-window 'xwem-frame-goto-next)

;;;###autoload(autoload 'xwem-winmove-up "xwem-win" "" t)
(define-xwem-command xwem-winmove-up (arg)
  "Selects window that up for selected."
  (xwem-interactive "P")
  (xwem-winmove-select 'up arg))
(put 'xwem-winmove-up 'xwem-frame-command t)

;;;###autoload(autoload 'xwem-winmove-down "xwem-win" "" t)
(define-xwem-command xwem-winmove-down (arg)
  "Selects window that down for selected."
  (xwem-interactive "P")
  (xwem-winmove-select 'down arg))
(put 'xwem-winmove-down 'xwem-frame-command t)

;;;###autoload(autoload 'xwem-winmove-left "xwem-win" "" t)
(define-xwem-command xwem-winmove-left (arg)
  "Selects window that left for selected."
  (xwem-interactive "P")
  (xwem-winmove-select 'left arg))
(put 'xwem-winmove-left 'xwem-frame-command t)

;;;###autoload(autoload 'xwem-winmove-right "xwem-win" "" t)
(define-xwem-command xwem-winmove-right (arg)
  "Selects window that right for selected."
  (xwem-interactive "P")
  (xwem-winmove-select 'right arg))
(put 'xwem-winmove-right 'xwem-frame-command t)

;;; Windows oriented drawers
(define-xwem-deferred xwem-win-redraw-delims (win)
  "Draw delimetrs in window WIN."
  (when (xwem-win-p win)
    (let ((wf (xwem-win-frame win))
          (hc (xwem-win-hchild win))
          (vc (xwem-win-vchild win)))

      (while (xwem-win-p hc)
        ;; For horizontal split
        (when (xwem-win-p (xwem-win-next hc))
          (xwem-misc-draw-bar
           (xwem-dpy) (xwem-frame-xwin wf)
           (xwem-face-get-gc 'xwem-window-delimiter-face
             '(horizontal) hc)
           (xwem-face-get-gc 'xwem-window-delimiter-face
             '(horizontal shadow light) hc)
           (xwem-face-get-gc 'xwem-window-delimiter-face
             '(horizontal shadow) hc)
           (+ (xwem-win-x hc) (xwem-win-width hc))
           (xwem-win-y hc)
           (xwem-win-delim-width hc)
           (xwem-win-height hc)
           (xwem-win-delim-shadow-thickness hc)))

        (xwem-win-redraw-delims-1 hc)
        (setq hc (xwem-win-next hc)))

      (while (xwem-win-p vc)
        ;; For vertical split
        (when (xwem-win-p (xwem-win-next vc))
          (xwem-misc-draw-bar
           (xwem-dpy) (xwem-frame-xwin wf)
           (xwem-face-get-gc 'xwem-window-delimiter-face
             '(vertical) vc)
           (xwem-face-get-gc 'xwem-window-delimiter-face
             '(light shadow vertical) vc)
           (xwem-face-get-gc 'xwem-window-delimiter-face
             '(shadow vertical) vc)
           (xwem-win-x vc)
           (+ (xwem-win-y vc) (xwem-win-height vc))
           (xwem-win-width vc)
           (xwem-win-delim-width vc)
           (xwem-win-delim-shadow-thickness vc)))

        (xwem-win-redraw-delims-1 vc)
        (setq vc (xwem-win-next vc)))
      )))

(defun xwem-win-choose-outline-gc (win)
  "Choose X-Gc according to WIN's current state."
  (xwem-face-get-gc 'xwem-window-outline-face
    (list (if (xwem-frame-selected-p (xwem-win-frame win))
              'frame-selected
            'frame-nonselected)
          (if (xwem-win-selwin-p win)
              'win-selected
            'win-nonselected))
    win))

(define-xwem-deferred xwem-win-redraw-win (win)
  "Redraw only one WIN in WIN's frame."
  (when (and (xwem-win-p win)
             (xwem-frame-p (xwem-win-frame win)))
    (XClearArea (xwem-dpy) (xwem-frame-xwin (xwem-win-frame win))
                (xwem-win-x win) (xwem-win-y win)
                (xwem-win-width win) (xwem-win-height win) nil)
    (if (xwem-win-cl win)
        (XFillRectangles
         (xwem-dpy) (xwem-frame-xwin (xwem-win-frame win))
         (xwem-win-choose-outline-gc win)
         (list (make-X-Rect :x (xwem-win-x win)
                            :y (xwem-win-y win)
                            :width (xwem-win-width win)
                            :height (xwem-win-border-width win))
               (make-X-Rect :x (xwem-win-x win)
                            :y (xwem-win-y win)
                            :width (xwem-win-border-width win)
                            :height (xwem-win-height win))
               (make-X-Rect :x (+ (xwem-win-x win)
                                  (xwem-win-width win)
                                  (- (xwem-win-border-width win)))
                            :y (xwem-win-y win)
                            :width (xwem-win-border-width win)
                            :height (xwem-win-height win))
               (make-X-Rect :x (xwem-win-x win)
                            :y (+ (xwem-win-y win)
                                  (xwem-win-height win)
                                  (- (xwem-win-border-width win)))
                            :width (xwem-win-width win)
                            :height (xwem-win-border-width win))))

      (let ((cgc (xwem-win-choose-outline-gc win)))
        (XSetClipRectangles (xwem-dpy) cgc 0 0
                            (list (X-Geom-to-X-Rect (xwem-win-geom win))))
        (XDrawRectangle (xwem-dpy) (xwem-frame-xwin (xwem-win-frame win))
                        cgc
                        (xwem-win-x win) (xwem-win-y win)
                        (xwem-win-width win) (xwem-win-height win))
        (XChangeGC (xwem-dpy) cgc :clip-mask X-None)))))

(define-xwem-deferred xwem-win-redraw-frame (frame)
  "Outline windows in FRAME if needed."
  (when (and (xwem-frame-alive-p frame)
	     (eq (xwem-frame-state frame) 'mapped))
    (xwem-win-map 'xwem-win-redraw-win (xwem-frame-selwin frame))
    (xwem-win-redraw-delims (xwem-frame-rootwin frame))))

;;;###xwem-autoload
(defun xwem-select-window (window)
  "Set WINDOW to be selected window."
  (when (xwem-win-alive-p window)
    (let* ((wframe (xwem-win-frame window))
           (ow (xwem-frame-selwin wframe))
           (cl (xwem-win-cl window))
           emcl)
      (unless (xwem-frame-selected-p wframe)
        ;; Select client in case WFRAME is embedded client
        (when (xwem-cl-p
               (setq emcl (xwem-frame-get-prop wframe 'xwem-embedded-cl)))
          (xwem-select-client emcl))

        (run-hooks 'xwem-frame-deselect-hook)
        (setq xwem-current-frame wframe)
        (run-hooks 'xwem-frame-select-hook))

      (xwem-frame-raise wframe)
      (unless (eq ow window)
        (setf (xwem-frame-selwin wframe) window)
        (run-hook-with-args 'xwem-win-switch-hook ow window))

      (unless (xwem-cl-selected-p cl)
        (xwem-select-client cl)))))

;;;###xwem-autoload
(defun xwem-window-set-pixsize (window nsize nodelete is-height)
  "Set pixsize for WINDOW."
  (let ((old-pixsize (if is-height
                         (xwem-win-height window)
                       (xwem-win-width window)))
        (min-size (if is-height
                      xwem-win-min-height
                    xwem-win-min-width))
        machild michild pos)

    (if (and (null nodelete)
             (xwem-win-parent window)   ; not top level window
             (< nsize min-size))
        (progn
          (if is-height
              (setf (xwem-win-height window) nsize)
            (setf (xwem-win-width window) nsize))
          ;; If size will be not enought even after everything is
          ;; complete - delete window.
          ;; In case when balancing window, some windows resizes to
          ;; nsize < min-size temporary, but then resizes again to
          ;; better size, so we will not delete them.
          (xwem-deferred-funcall
           `(lambda (window)
              (when (< (if ,is-height
                           (xwem-win-height window)
                         (xwem-win-width window))
                       (if ,is-height
                           xwem-win-min-height
                         xwem-win-min-width))
                (xwem-window-delete window)))
           window))

      (if is-height
          (progn
            (setf (xwem-win-height window) nsize)
            (setq machild (xwem-win-vchild window))
            (setq michild (xwem-win-hchild window)))
        (progn
          (setf (xwem-win-width window) nsize)
          (setq machild (xwem-win-hchild window))
          (setq michild (xwem-win-vchild window))))

      ;; Also refit window's client
      (when (xwem-cl-p (xwem-win-cl window))
        (xwem-deferred-funcall 'xwem-refit (xwem-win-cl window)))

      (cond ((xwem-win-p michild)
             (mapc (lambda (child)
                     (if is-height
                         (setf (xwem-win-y child) (xwem-win-y window))
                       (setf (xwem-win-x child) (xwem-win-x window)))
                     (xwem-window-set-pixsize child nsize nodelete is-height))
                   (xwem-win-make-list-by-next michild)))

            ((xwem-win-p machild)
             ;; TODO: adjust geom for major child
             (let* ((last-pos (if is-height
                                  (xwem-win-y window)
                                (xwem-win-x window)))
                    (first last-pos)
                    (last-old-pos 0)
                    (delims-size 0)
                    (old-pos nil)
                    (mchils (xwem-win-make-list-by-next machild)))

               ;; Calculate width sum of all delimetrs
               (mapc #'(lambda (el)
                         (when (xwem-win-p (xwem-win-next el))
                           (setq delims-size
                                 (+ delims-size (xwem-win-delim-width el)))))
                     mchils)

               (mapc #'(lambda (child)
                         (if is-height
                             (progn
                               (setq old-pos
                                     (+ last-old-pos
                                        (xwem-win-height child)))
                               (setf (xwem-win-y child) last-pos))
                           (progn
                             (setq old-pos (+ last-old-pos
                                              (xwem-win-width child)))
                             (setf (xwem-win-x child) last-pos)))

                         (setq pos (/ (+ (* old-pos
                                            (if (xwem-win-p (xwem-win-next child))
                                                (- nsize delims-size)
                                              nsize)
                                            2)
                                         (- old-pixsize delims-size))
                                      (* 2 (- old-pixsize delims-size))))

                         (xwem-window-set-pixsize
                          child (- (+ pos first) last-pos) t is-height)

                         (setq last-pos
                               (+ pos first (xwem-win-delim-width child))
                               last-old-pos old-pos))
                     mchils))

             ;; Now delete any children that became too small.
             (when (not nodelete)
               (mapc #'(lambda (child)
                         (if is-height
                             (xwem-window-set-pixsize
                              child (xwem-win-height child) nil t)
                           (xwem-window-set-pixsize
                            child (xwem-win-width child) nil nil)))
                     (xwem-win-make-list-by-next machild)))
             )

            ;; Normal window, just outdraw
            (t (xwem-win-redraw-win window)))

      ;; Redraw WINDOW's frame
      (xwem-win-redraw-delims (xwem-frame-rootwin (xwem-win-frame window)))
      )))

;;;###xwem-autoload
(defun xwem-win-set-width (win new-width)
  "Set window's WIN width to NEW-WIDTH."
  (xwem-window-set-pixsize win new-width nil nil))

;;;###xwem-autoload
(defun xwem-win-set-height (win new-height)
  "Set window's WIN height to NEW-HEIGHT."
  (xwem-window-set-pixsize win new-height nil t))

;;;###autoload(autoload 'xwem-window-delete "xwem-win" "Delete selected WINDOW." t)
(define-xwem-command xwem-window-delete (win)
  "Delete WIN."
  (xwem-interactive (list (xwem-win-selected)))

  (unless (xwem-win-p win)
    (error 'xwem-error "Invalid window" win))

  (when (xwem-win-only-one-p win)
    (error 'xwem-error "Can't delete window, because it is only one."))

  (let ((frame (xwem-win-frame win))
        dclients owin par pwin ccl)

    (if (null (xwem-win-parent win))
        (progn
          ;; win is top level window
          ;; TODO: should I delete frame?
          nil)

      ;; Normal window - delete it
      (setq par (xwem-win-parent win))
      (setq pwin (xwem-frame-selwin frame))
      (while (and (xwem-win-p pwin) (not (eq pwin win)))
        (setq pwin (xwem-win-parent pwin)))

      ;; If we're going to delete selected window then we should
      ;; update selected window and demanage client, and maybe
      ;; collect its clients to other window.
      (setq dclients (xwem-win-clients win))
      (setq owin (xwem-window-other 1 win))
      (mapc #'(lambda (cl)
                (xwem-cl-change-window cl owin))
            dclients)

      (when (eq pwin win)
        ;; If there no clients in window manage our client
        (when (and (not (xwem-cl-p (xwem-win-cl owin)))
                   (xwem-cl-p (setq ccl (xwem-win-cl win))))
          (xwem-cl-change-window ccl owin)
          (xwem-activate ccl))
        (xwem-select-window owin))

      ;; close that hole in list
      ;; XXX: check h v h split then delete right win ...
      (when (xwem-win-p (xwem-win-next win))
        (setf (xwem-win-prev (xwem-win-next win)) (xwem-win-prev win)))
      (when (xwem-win-p (xwem-win-prev win))
        (setf (xwem-win-next (xwem-win-prev win)) (xwem-win-next win)))
      (when (eq win (xwem-win-hchild par))
        (setf (xwem-win-hchild par) (xwem-win-next win)))
      (when (eq win (xwem-win-vchild par))
        (setf (xwem-win-vchild par) (xwem-win-next win)))

      ;; TODO: adjust the geometry
      (let ((sib (xwem-win-prev win)))
        (when (null sib)
          (setq sib (xwem-win-next win))
          (setf (xwem-win-x sib) (xwem-win-x win))
          (setf (xwem-win-y sib) (xwem-win-y win)))

        (when (xwem-win-p (xwem-win-vchild par))
          (xwem-window-set-pixsize
           sib
           (+ (xwem-win-height sib)
              (xwem-win-height win)
              (car xwem-win-vertical-delim-width))
           t t))

        (when (xwem-win-p (xwem-win-hchild par))
          (xwem-window-set-pixsize
           sib
           (+ (xwem-win-width sib)
              (xwem-win-width win)
              (car xwem-win-horizontal-delim-width))
           t nil)))

      ;; If parent now has only one child put child into parent
      ;; place
      (when (null (xwem-win-next (xwem-win-child par)))
        (xwem-win-replace par (xwem-win-child par))
        (xwem-win-mark-deleted par))

      ;; Since we deleting combination of windows we should delete
      ;; all childs
      (when (xwem-win-p (xwem-win-child win))
        (xwem-win-delete-subwindows (xwem-win-child win)))

      (xwem-win-mark-deleted win)

      ;; Redraw WIN's frame
      (xwem-win-redraw-frame (xwem-win-frame win))

      ;; Now run on-delete hooks
      (run-hook-with-args 'xwem-win-delete-hook win))
    ))
(put 'xwem-window-delete 'xwem-frame-command t)

;;;###autoload(autoload 'xwem-window-delete-others "xwem-win" nil t)
(define-xwem-command xwem-window-delete-others (window)
  "Delete all xwem windows other then WINDOW."
  (xwem-interactive (list (xwem-win-selected)))

  (unless (xwem-win-p window)
    (error 'xwem-error "Invalid window" window))

  (xwem-win-map #'(lambda (ww)
                    (unless (eq ww window)
                      (xwem-window-delete ww)))
                window))
(put 'xwem-window-delete-others 'xwem-frame-command t)

(defun xwem-win-delete-subwindows (win)
  "Delete all childs of WIN."
  (when (xwem-win-p (xwem-win-next win))
    (xwem-win-delete-subwindows (xwem-win-next win)))
  (when (xwem-win-child win)
    (xwem-win-delete-subwindows (xwem-win-child win)))

  (xwem-win-mark-deleted win))

;;;###xwem-autoload
(defun xwem-win-map (fn &optional window)
  "Apply FN to FRAME-WIN and each subwindow.
FN will be called with a window as argument.
If ommitted, WINDOW defaults to the `(xwem-win-selected)'."
  (let* ((start-win (or window (xwem-win-selected)))
         cur-win res)

    (when start-win
      (setq res (cons (funcall fn start-win) res))
      (setq cur-win (xwem-window-next start-win)))

    (while (not (eq cur-win start-win))
      (setq res (cons (funcall fn cur-win) res))
      (setq cur-win (xwem-window-next cur-win)))
    res))

(defun xwem-win-count (&optional window)
  "Count windows."
  (let ((cnt 0))
    (xwem-win-map #'(lambda (win) (setq cnt (+ cnt 1))) window)
    cnt))

;;;###autoload(autoload 'xwem-balance-windows "xwem-win" "" t)
(define-xwem-command xwem-balance-windows (&optional win)
  "Make all WIN's parent children windows to be same height or width.
If WIN is ommited, selected window is used."
  (xwem-interactive (list (xwem-win-selected)))
  (unless (xwem-win-p win)
    (error 'xwem-error "invalid window" win))

  (when (xwem-win-p (setq win (xwem-win-parent win)))
    (let* ((height-p (xwem-win-p (xwem-win-vchild win)))
           (getsizefn (if height-p
                          'xwem-win-pixel-height
                        'xwem-win-pixel-width))
           (wins (xwem-win-make-list-by-next (xwem-win-child win)))
           size)

      (when wins
        (setq size (/ (funcall getsizefn win) (length wins)))
        (mapc #'(lambda (w)
                  (xwem-window-enlarge
                   (- size (funcall getsizefn w) (xwem-win-delim-width w))
                   height-p w))
              wins)
        (mapc #'(lambda (w)
                  (xwem-window-enlarge
                   (- size (funcall getsizefn w) (xwem-win-delim-width w))
                   height-p w))
              wins)
        ))))
(put 'xwem-balance-windows 'xwem-frame-command t)

;;;###(autoload 'xwem-transpose-windows "xwem-win" nil t)
(define-xwem-command xwem-transpose-windows (arg)
  "Transpose selected window with other window.
Prefix ARG directly passed to `xwem-window-other' to findout other
window."
  (xwem-interactive "p")

  (let ((cw (xwem-win-selected))
        (ow (xwem-window-other arg)))
    (when (or (eq cw ow)
            (not (xwem-win-p cw))
            (not (xwem-win-p ow)))
      (error 'xwem-error "Can't transpose windows"))

    (let ((cwcl (xwem-win-cl cw))
          (cwcls (xwem-win-clients cw))
          (owcl (xwem-win-cl ow))
          (owcls (xwem-win-clients ow)))

      (xwem-win-set-cl cw owcl)
      (mapc #'(lambda (cl) (xwem-cl-set-win cl cw)) (nreverse owcls))

      (xwem-win-set-cl ow cwcl)
      (mapc #'(lambda (cl) (xwem-cl-set-win cl ow)) (nreverse cwcls))

      (xwem-select-window cw))))
(put 'xwem-transpose-windows 'xwem-frame-command t)

;;;###xwem-autoload
(defun xwem-win-only-one-p (&optional window)
  "Return non-nil if WINDOW is only one in chain.
If WINDOW ommitted `(xwem-win-selected)' used."
  (= 1 (xwem-win-count (or window (xwem-win-selected)))))

(defun xwem-window-list (&optional frame)
  "Return list of windows for FRAME.
Default FRAME is selected frame."
  (let* ((wlf (or frame (xwem-frame-selected)))
         (win (xwem-frame-rootwin wlf))
         (rlist nil))
    (xwem-win-map #'(lambda (win)
                      (setq rlist (cons win rlist)))
                  win)
    rlist))

(defun xwem-win-pixel-edges (&optional window)
  "Return a list of the pixel edge coordinates of WINDOW.
\\(LEFT TOP RIGHT BOTTOM\\), all relative to 0, 0 at top left corner of
frame.  The frame title are considered to be outside of this area."
  (unless window
    (setq window (xwem-win-selected)))
  (list (xwem-win-x window)
        (xwem-win-y window)
        (+ (xwem-win-x window) (xwem-win-width window))
        (+ (xwem-win-y window) (xwem-win-height window))))

(defun xwem-win-pixel-width (&optional window)
  "Retrun width in pixels of WINDOW."
  (xwem-win-width (or window (xwem-win-selected))))

(defun xwem-win-pixel-height (&optional window)
  "Return height in pixels of WINDOW."
  (xwem-win-height (or window (xwem-win-selected))))

(defun xwem-win-pixels-steps (&optional window)
  "Convert size in pixels to size in steps for WINDOW."
  (error 'xwem-error "`xwem-win-pixels-steps' not yet implemented."))

(defun xwem-win-steps-pixels (&optional window)
  "Convert size in steps to size in pixels for WINDOW."
  (error 'xwem-error "`xwem-win-steps-pixels' not yet implemented."))

;;; START: Window configurations section

;;;###xwem-autoload
(defun xwem-window-configuration (&optional frame)
  "Return an object representing the current window configuration of xwem FRAME.
If FRAME is nil or ommited, use the sected frame."
  (let ((frm (or frame (xwem-frame-selected))))
    (when (xwem-frame-p frm)
      (make-xwem-win-config
       :frame frm
       :frame-xgeom (copy-X-Geom (xwem-frame-xgeom frm))
       :frame-properties (xwem-frame-properties frm)
       :current-cl (xwem-win-cl (xwem-frame-selwin frm))
       :min-width xwem-win-min-width :min-height xwem-win-min-height
       :saved-root-window (xwem-win-root->saved-win (xwem-frame-rootwin frm))))))

(defun xwem-win-root->saved-win (win)
  "Convert an xwem root WIN into a tree of saved-window structures."
  (let ((saved-win
         (make-xwem-win-saved
          :id (xwem-win-id win)
          :geom (copy-X-Geom (xwem-win-geom win))
          :clients (copy-list (xwem-win-clients win))
          :cl (xwem-win-cl win)
          :plist (xwem-win-properties win)
          :selwin-p (xwem-win-selwin-p win)
          :first-hchild (and (xwem-win-p (xwem-win-hchild win))
                             (xwem-win-root->saved-win (xwem-win-hchild win)))
          :first-vchild (and (xwem-win-p (xwem-win-vchild win))
                             (xwem-win-root->saved-win (xwem-win-vchild win)))

          :next (and (xwem-win-p (xwem-win-next win))
                     (xwem-win-root->saved-win (xwem-win-next win)))
          )))
    saved-win))

(defun xwem-win-config-equal (win-cfg0 win-cfg1)
  "Return non-nil if two window configurations WIN-CFG0 and WIN-CFG1 are equal."
  (equal win-cfg0 win-cfg1))

;;;###xwem-autoload
(defun xwem-set-window-configuration (config &optional select-frame-p)
  "Set window to CONFIG.
If optional argument SELECT-FRAME-P is non-nil also select frame for
which window CONFIG was generated."
  (let ((frame (xwem-win-config-frame config)))
    (when (and (xwem-frame-alive-p frame)
               (not (xwem-win-config-equal
                     config (xwem-window-configuration frame))))

      (xwem-frame-reduce-to-one-window frame)
      (xwem-frame-set-win-config-frame-params config)

      (xwem-win-restore-saved-win
       config (xwem-frame-rootwin frame)
       (xwem-win-config-saved-root-window config) 'vertical)

      ;; XXX what is this?
      (setq xwem-win-min-width (xwem-win-config-min-width config))
      (setq xwem-win-min-height (xwem-win-config-min-height config)))

    ;; Time to select frame
    (when (and select-frame-p (xwem-frame-alive-p frame))
      (xwem-select-frame frame))))

(defun xwem-frame-reduce-to-one-window (frame)
  "Delete all windows except the one."
  (let ((swin (xwem-frame-selwin frame)))
    (xwem-win-map #'(lambda (ww)
                      (when (not (eq ww swin))
                        (xwem-window-delete ww)))
                  swin)))

(defun xwem-frame-set-win-config-frame-params (config)
  "Restore FRAME size of a window configuration CONFIG."
  (setf (xwem-frame-xgeom (xwem-win-config-frame config))
        (copy-X-Geom (xwem-win-config-frame-xgeom config)))
  (xwem-frame-apply-xgeom-1 (xwem-win-config-frame config))
  (xwem-frame-set-properties (xwem-win-config-frame config)
                             (xwem-win-config-frame-properties config)))

(defun xwem-win-restore-saved-win (config win saved-win direction)
  "Within CONFIG, restore WIN to the state of SAVED-WIN."
  (if (xwem-win-saved-next saved-win)
      (progn
        (xwem-win-split win direction)
        (xwem-win-restore-win-params config win saved-win)
        (xwem-win-restore-saved-win config (xwem-win-next win)
           (xwem-win-saved-next saved-win) direction))

    ;; [else] No next saved
    (xwem-win-restore-win-params config win saved-win))

  (when (xwem-win-saved-first-hchild saved-win)
    (xwem-win-restore-saved-win config win
                                (xwem-win-saved-first-hchild saved-win)
                                'horizontal))
  (when (xwem-win-saved-first-vchild saved-win)
    (xwem-win-restore-saved-win config win
                                (xwem-win-saved-first-vchild saved-win)
                                'vertical)))

(defun xwem-win-subr-first-member (l1 l2)
  "Return first element in L1, which also in L2.
Comparison done using `eq'."
  (while (and l1 (not (memq (car l1) l2)))
    (setq l1 (cdr l1)))
  l1)

(defun xwem-reorder-clients (clients clients-list)
  "Reorder CLIENTS to be in same order as in CLIENTS-LIST.
This is destructive function, it will modify CLIENTS list directly."
  (let ((cls clients)
        (scls clients-list)
        t1 t2)
    (while scls
      (when (setq t1 (memq (car scls) cls))
        (setq t2 (xwem-win-subr-first-member cls scls))
        (xwem-list-exchange-els clients (car t1) (car t2))
        (setq scls t1)
        (setq cls t2))

      (setq scls (cdr scls))
      (setq cls (cdr cls)))))

(defun xwem-win-reorder-clients (saved-win)
  "Reorder `xwem-clients' in SAVED-WIN clients order."
  (let ((cls xwem-clients)
        (scls (xwem-win-saved-clients saved-win))
        t1 t2)
    (while scls
      (when (setq t1 (memq (car scls) cls))
        (setq t2 (xwem-win-subr-first-member cls scls))
        (xwem-list-exchange-els xwem-clients (car t1) (car t2))
        (setq scls t1)
        (setq cls t2))

      (setq scls (cdr scls))
      (setq cls (cdr cls)))))

(defun xwem-win-restore-win-params (config win saved-win)
  "Restore the windown parameters stored in SAVED-WIN on WIN."
  (let ((cln (xwem-win-saved-cl saved-win)))
    ;; Restore ID and PLIST
    (setf (xwem-win-id win) (xwem-win-saved-id saved-win)
          (xwem-win-plist win) (copy-list (xwem-win-saved-plist saved-win)))

    ;; Resort clients in WIN's order
    (xwem-win-reorder-clients saved-win)

    ;; Collect saved clients back to WIN
    (mapc #'(lambda (cl)
              (when (xwem-cl-alive-p cl)
                (xwem-cl-set-win cl win)
                (xwem-client-change-state cl 'inactive)))
          (xwem-win-saved-clients saved-win))

    ;; Restore window geometry
    (when (and (not (xwem-win-saved-first-hchild saved-win))
               (not (xwem-win-saved-first-vchild saved-win)))
      (when (not (eq win (xwem-frame-rootwin (xwem-win-frame win))))
        (xwem-window-enlarge (- (X-Geom-width (xwem-win-saved-geom saved-win))
                                (xwem-win-width win)) nil win)
        (xwem-window-enlarge (- (X-Geom-height (xwem-win-saved-geom saved-win))
                                (xwem-win-height win)) t win)))

    ;; Remanage current client in WIN
    (when (xwem-cl-alive-p cln)
      ;; Check manage
      (xwem-cl-change-window cln win)
      (xwem-activate cln))

    (when (xwem-win-saved-selwin-p saved-win)
      (setf (xwem-frame-selwin (xwem-win-frame win)) win))))

;;; END:

;;;###autoload(autoload 'xwem-window-split-horizontally "xwem-win" nil t)
(define-xwem-command xwem-window-split-horizontally (arg &optional window)
  "Split WINDOW horizontally."
  (xwem-interactive (list (prefix-numeric-value xwem-prefix-arg)
                          (xwem-win-selected)))

  (unless window
    (setq window (xwem-win-selected)))
  (unless (xwem-win-p window)
    (error 'xwem-error "Invalid window" window))

  (when (xwem-frame-dedicated-p (xwem-win-frame window))
    (error 'xwem-error "Can't split dedicated frame"))

  (xwem-win-split window 'horizontal arg))
(put 'xwem-window-split-horizontally 'xwem-frame-command t)

;;;###autoload(autoload 'xwem-window-split-vertically "xwem-win" nil t)
(define-xwem-command xwem-window-split-vertically (arg &optional window)
  "Split WINDOW horizontally."
  (xwem-interactive (list (prefix-numeric-value xwem-prefix-arg)
                          (xwem-win-selected)))
  (unless window
    (setq window (xwem-win-selected)))
  (unless (xwem-win-p window)
    (error 'xwem-error "Invalid window" window))

  (when (xwem-frame-dedicated-p (xwem-win-frame window))
    (error 'xwem-error "Can't split dedicated frame."))

  (xwem-win-split window 'vertical arg))
(put 'xwem-window-split-vertically 'xwem-frame-command t)

;;;###autoload(autoload 'xwem-window-enlarge-horizontally "xwem-win" nil t)
(define-xwem-command xwem-window-enlarge-horizontally (n window)
  "Enlarge horizontally WINDOW by N pixels."
  (xwem-interactive (list (prefix-numeric-value xwem-prefix-arg)
                          (xwem-win-selected)))
  (unless (xwem-win-p window)
    (error 'xwem-error "Invalid window" window))

  (xwem-window-enlarge n nil window))
(put 'xwem-window-enlarge-horizontally 'xwem-frame-command t)

;;;###autoload(autoload 'xwem-window-enlarge-vertically "xwem-win" nil t)
(define-xwem-command xwem-window-enlarge-vertically (n window)
  "Enlarge vertically WINDOW by N pixels."
  (xwem-interactive (list (prefix-numeric-value xwem-prefix-arg)
                          (xwem-win-selected)))
  (unless (xwem-win-p window)
    (error 'xwem-error "Invalid window" window))

  (xwem-window-enlarge n t window))
(put 'xwem-window-enlarge-vertically 'xwem-frame-command t)

(defun xwem-win-split (&optional window how new-size)
  "Split WINDOW.
When WINDOW is ommitted `(xwem-win-selected)' used.
HOW is 'vertical of 'horizontal, default is 'horizontal.
If NEW-SIZE is given make WINDOW NEW-SIZE pixels bigger after split."
  (let* ((sp-win (or window (xwem-win-selected)))
         (sp-how (or how 'horizontal))
         (hor (equal sp-how 'horizontal))
         (nwin (xwem-win-new (list :frame (xwem-win-frame sp-win))))
         (nsiz (or new-size 0))
         psize psize1 psize2 remd)

    (if hor
        (progn
          (setq psize (/ (- (xwem-win-width sp-win)
                            (car xwem-win-horizontal-delim-width)) 2))
          (setq remd (% (- (xwem-win-width sp-win)
                           (car xwem-win-horizontal-delim-width)) 2)))
      (progn
          (setq psize (/ (- (xwem-win-height sp-win)
                            (car xwem-win-vertical-delim-width)) 2))
          (setq remd (% (- (xwem-win-height sp-win)
                           (car xwem-win-vertical-delim-width)) 2))))

    (setq psize1 (+ psize nsiz))
    (setq psize2 (+ psize remd (- nsiz)))
    ;; Check that minimal widht or height is not exceeded
    (xwem-debug 'xwem-misc "Doing split hor=%s psize=%d" 'hor 'psize)
    (if hor
        ;; horizontal split
        (if (or (< psize1 xwem-win-min-width)
                (< psize2 xwem-win-min-width))
            (error 'xwem-error "Can't do split.")

          ;; TODO:
          (when (or (null (xwem-win-parent sp-win))
                    (null (xwem-win-hchild (xwem-win-parent sp-win))))
            (xwem-win-make-parent sp-win)
            (setf (xwem-win-hchild (xwem-win-parent sp-win)) sp-win)))

      ;; vertical split
      (if (or (< psize1 xwem-win-min-height)
              (< psize2 xwem-win-min-height))
          (error 'xwem-error "Can't do split.")
        ;; TODO
        (when (or (null (xwem-win-parent sp-win))
                  (null (xwem-win-vchild (xwem-win-parent sp-win))))
          (xwem-win-make-parent sp-win)
          (setf (xwem-win-vchild (xwem-win-parent sp-win)) sp-win))))

    (setf (xwem-win-frame nwin) (xwem-win-frame sp-win))
    (setf (xwem-win-next nwin) (xwem-win-next sp-win))
    (when (xwem-win-p (xwem-win-next nwin))
      (setf (xwem-win-prev (xwem-win-next nwin)) nwin))
    (setf (xwem-win-prev nwin) sp-win)
    (setf (xwem-win-next sp-win) nwin)
    (setf (xwem-win-parent nwin) (xwem-win-parent sp-win))

    ;; TODO: adjust geometry
    (if hor
        (progn
          (setf (xwem-win-width sp-win) psize1)
          (setf (xwem-win-x nwin)
                (+ (xwem-win-delim-width sp-win)
                   (xwem-win-x sp-win)
                   (xwem-win-width sp-win)))
          (setf (xwem-win-y nwin) (xwem-win-y sp-win))
          (setf (xwem-win-width  nwin) psize2)
          (setf (xwem-win-height nwin) (xwem-win-height sp-win)))
      (progn
        (setf (xwem-win-height sp-win) psize1)
        (setf (xwem-win-x nwin) (xwem-win-x sp-win))
        (setf (xwem-win-y nwin)
              (+ (xwem-win-delim-width sp-win)
                 (xwem-win-y sp-win)
                 (xwem-win-height sp-win)))
        (setf (xwem-win-width nwin) (xwem-win-width sp-win))
        (setf (xwem-win-height nwin) psize2)))

    ;; Client refitting if needed
    (when (xwem-cl-p (xwem-win-cl sp-win))
      (xwem-deferred-funcall 'xwem-refit (xwem-win-cl sp-win)))

    ;; NOTE: nwin is (xwem-win-next sp-win)
    (run-hook-with-args 'xwem-win-split-hook sp-win nwin)

    ;; Redraw window delimiters and outline
    (xwem-win-redraw-delims (xwem-win-parent nwin))
    (xwem-win-redraw-win nwin)
    (xwem-win-redraw-win sp-win)

    nwin))

(defvar xwem-allow-resize-frame-when-resizing-root-window t
  "Non-nil mean win's frame can be resized if resizing frame's root window.")

(defun xwem-window-change-size (win delta height-p)
  "Change WIN size to old-size plus DELTA."
  (let ((getsizefn (if height-p 'xwem-win-pixel-height 'xwem-win-pixel-width))
        (setsizefn (if height-p 'xwem-win-set-height 'xwem-win-set-width))
        (par nil)
        wsize minsize maxdelta)

    (if (eq win (xwem-frame-rootwin (xwem-win-frame win)))
        ;; Resize frame
        (if xwem-allow-resize-frame-when-resizing-root-window
            (xwem-frame-set-size (xwem-win-frame win)
                                 (+ (X-Geom-width
                                     (xwem-frame-xgeom (xwem-win-frame win)))
                                    (if height-p 0 delta))
                                 (+ (X-Geom-height
                                     (xwem-frame-xgeom (xwem-win-frame win)))
                                    (if height-p delta 0)))
          (error 'xwem-error "Can't change size of frame's root window"))

      (setq par (xwem-win-parent win))
      (while (and par (not (if height-p
                               (xwem-win-vchild par)
                             (xwem-win-hchild par))))
        (setq win par)
        (setq par (xwem-win-parent win)))

      (setq wsize (funcall getsizefn win))
      (setq minsize (if height-p xwem-win-min-height xwem-win-min-width))
      (when (< (+ wsize delta) minsize)
        (error 'xwem-error
               "Minimum size exceeded while in `xwem-window-change-size'"))

      (setq maxdelta (if par (- (funcall getsizefn par) wsize)
                       (if (xwem-win-next win)
                           (- (funcall getsizefn (xwem-win-next win))
                              minsize)
                         (if (xwem-win-prev win)
                             (- (funcall getsizefn (xwem-win-prev win))
                                minsize)
                           (setq delta 0)))))
      (when (> delta maxdelta)
        (setq delta maxdelta))

      (unless (= delta 0)
        (if (and (xwem-win-p (xwem-win-next win))
                 (>= (- (funcall getsizefn (xwem-win-next win)) delta) minsize))
            (progn
              (xwem-debug
               'xwem-misc "HERE IN xwem-window-change-size: delta=%d" 'delta)
              (if height-p
                  (setf (xwem-win-y (xwem-win-next win))
                        (+ (xwem-win-y (xwem-win-next win)) delta))
                (setf (xwem-win-x (xwem-win-next win))
                      (+ (xwem-win-x (xwem-win-next win)) delta)))
              (funcall setsizefn (xwem-win-next win)
                       (- (funcall getsizefn (xwem-win-next win)) delta))
              (funcall setsizefn win (+ (funcall getsizefn win) delta)))

          (if (and (xwem-win-p (xwem-win-prev win))
                   (>= (- (funcall getsizefn (xwem-win-prev win)) delta)
                       minsize))
              (progn
                (funcall setsizefn (xwem-win-prev win)
                         (- (funcall getsizefn (xwem-win-prev win)) delta))
                (if height-p
                    (setf (xwem-win-y win) (- (xwem-win-y win) delta))
                  (setf (xwem-win-x win) (- (xwem-win-x win) delta)))
                (funcall setsizefn win (+ (funcall getsizefn win) delta)))

            ;; If we are here than changing window size possible
            ;; causes changing parent size.
            (let ((opht (funcall getsizefn par))
                  delta1)
              (if (<= opht (+ delta wsize))
                  (setq delta1 (* opht opht 2))

                (setq delta1 (/ (* delta opht 100)
                                (* (- opht wsize delta) 100))))

              (if height-p
                  (setf (xwem-win-height par) (+ opht delta1))
                (setf (xwem-win-width par) (+ opht delta1)))

              (funcall setsizefn win (+ wsize delta1))
              (funcall setsizefn par opht)
              ))))
      )))

(defun xwem-window-enlarge (n &optional height-p window)
  "Make WINDOW N pixels bigger.
If HEIGHT-P is non-nil then enlarge vertically.
If WINDOW is ommited then selected window will be used."
  (xwem-window-change-size (or window (xwem-win-selected)) n height-p))

(defun xwem-window-shrink (n &optional height-p window)
  "Make WINDON N pixels smaller.
If HEIGHT-P is non-nil then shrink vertically.
If WINDOW is ommited then selected window will be used."
  (xwem-window-change-size (or  window (xwem-win-selected)) (- n) height-p))

;;;###xwem-autoload
(defun xwem-window-set-size (win new-width new-height)
  "Set WIN's size to be NEW-WIDTH, NEW-HEIGHT."
  (xwem-window-change-size win (- new-width (xwem-win-width win)) nil)
  (xwem-window-change-size win (- new-height (xwem-win-height win)) t))

(defsubst xwem-win-redraw-selected-frame ()
  "Redraw selected frame."
  (xwem-win-redraw-frame (xwem-frame-selected)))

(defun xwem-win-init ()
  "Initialize xwem windows."
  (add-hook 'xwem-win-switch-hook
            #'(lambda (owin nwin)
                (when (xwem-win-p owin)
                  (xwem-win-redraw-win owin))
                (when (xwem-win-p nwin)
                  (xwem-win-redraw-win nwin))))

  (add-hook 'xwem-frame-select-hook 'xwem-win-redraw-selected-frame)
  (add-hook 'xwem-frame-deselect-hook 'xwem-win-redraw-selected-frame)
  (add-hook 'xwem-frame-redraw-hook 'xwem-win-redraw-frame)
  )

;;; Win properties
(defvar xwem-win-supported-properties '(domain-faces)
  "List of supported window properties.")

(defun xwem-win-properties (win)
  "Return list of win's properties."
  (let ((wplist (xwem-win-plist win))
        (rplist nil))
    (while wplist
      (when (memq (car wplist) xwem-win-supported-properties)
        (setq rplist (plist-put rplist (car wplist) (cadr wplist))))
      (setq wplist (cddr wplist)))
    rplist))


(provide 'xwem-win)

;;;; On-load actions:
;; - Initialize windows
(xwem-win-init)

;;; xwem-win.el ends here
