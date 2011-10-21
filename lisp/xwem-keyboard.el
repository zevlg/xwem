;;; xwem-keyboard.el --- Keyboard support for XWEM.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Authors: Zajcev Evgeny <zevlg@yandex.ru>
;;          Steve Youngs  <steve@youngs.au.com>
;;          Alex Ott <ottalex@narod.ru>
;; Created: 21 Mar 2003
;; Keywords: xwem, xlib
;; Time-stamp: <30/4/2010 21:21:06 lg@localhost>

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
;; `xwem-global-map' is normal keymap used by xwem.
;;
;; Set `xwem-keyboard-echo-keystrokes' to t if you want echoing of
;; incomplete commands in echo area.

;;; Code:

(require 'xlib-xlib)
(require 'xlib-xtest)
(require 'xlib-keysymdb)

(require 'xwem-load)
(require 'xwem-misc)

;;{{{ [-] Custamizable xwem-keyboard group

;;; Customize variables
(defgroup xwem-keyboard nil
  "Group to customize keyboard in XWEM."
  :prefix "xwem-"
  :group 'xwem)

;;;###autoload
(defcustom xwem-pre-command-hook nil
  "*Hooks to run just before executing command.
This may examine `xwem-this-command' variable to find out which
command is about to be run, or may change it to cause a different
command to run."
  :type 'hook
  :group 'xwem-keyboard
  :group 'xwem-hooks)

;;;###autoload
(defcustom xwem-post-command-hook nil
  "*Hooks to run after command execution."
  :type 'hook
  :group 'xwem-keyboard
  :group 'xwem-hooks)

;;;###autoload
(defcustom xwem-keyboard-echo-keystrokes 1
  "*Echo unfinished commands in echo area after this many seconds of pause."
  :type 'number
  :group 'xwem-keyboard)

(defcustom xwem-hyper-modifier 'hyper
  "*Hack to interpret ``H'' in `xwem-kbd' specification."
  :type '(choice (const :tag "Meta" meta)
                 (const :tag "Control" control)
                 (const :tag "Super" super)
                 (const :tag "Hyper" hyper)
                 (const :tag "Alt" alt))
  :group 'xwem-keyboard)

(defcustom xwem-meta-modifier 'meta
  "*Hack to interpret ``M'' in `xwem-kbd' specification."
  :type '(choice (const :tag "Meta" meta)
                 (const :tag "Control" control)
                 (const :tag "Super" super)
                 (const :tag "Hyper" hyper)
                 (const :tag "Alt" alt))
  :group 'xwem-keyboard)

(defcustom xwem-control-modifier 'control
  "*This is a little trick of how ``C'' interpretted in `xwem-kbd'."
  :type '(choice (const :tag "Meta" meta)
                 (const :tag "Control" control)
                 (const :tag "Super" super)
                 (const :tag "Hyper" hyper)
                 (const :tag "Alt" alt))
  :group 'xwem-keyboard)

(defcustom xwem-kbd-evillocks (list XK-Num-Lock XK-Caps-Lock)
  "List of evil locks."
  :type `(repeat (choice (const :tag "NumLock" ,XK-Num-Lock)
                         (const :tag "CapsLock" ,XK-Caps-Lock)
                         ;; TODO: add others .. which?
                         ))
  :group 'xwem-keyboard)

(defvar xwem-kbd-evilmasks (list 0)
  "List of evil masks.
Internal variable, DO NOT MODIFY.")

;;;###autoload
(defcustom xwem-quit-key [(hyper ?g)]
  "Quit command key."
  :type 'sexp
  :group 'xwem-keyboard)

;;;###autoload
(defcustom xwem-quit-command 'xwem-keyboard-quit
  "Default command to be called when `xwem-quit-key' pressed."
  :type 'function
  :group 'xwem-keyboard)

;;;###autoload
(defcustom xwem-help-key [(hyper ?h)]
  "Help command key."
  :type 'sexp
  :group 'xwem-keyboard)

;;;###autoload
(defcustom xwem-prefix-help-command 'xwem-describe-prefix-bindings
  "Default command to be called when `xwem-help-key' pressed."
  :type 'function
  :group 'xwem-keys)

;;;###autoload
(defcustom xwem-universal-key [(hyper ?u)]
  "Key for universal argument commands."
  :type 'sexp
  :group 'xwem-keyboard)

;;;###autoload
(defcustom xwem-keyboard-init-hook nil
  "*Hooks called after xwem keyboard initialization."
  :type 'hook
  :group 'xwem-keyboard
  :group 'xwem-hooks)

(defcustom xwem-keyboard-use-synth-events nil
  "*Non-nil mean use XSendEvent instead of XTEST's FakeInput.
Useful to set it to non-nil when using sticky modifiers or in any
other case when xwem can't properly force modifiers releasing.
However if having `xwem-keyboard-use-synth-events' to non-nil, make
sure you've configured clients to accept synthetic X events (f.i. set
`x-allow-sendevents' to non-nil to make XEmacs accept synthetic
events).
This is client local variable.  Each client have its own value for it."
  :type 'boolean
  :group 'xwem-keyboard)
(xwem-make-variable-client-local 'xwem-keyboard-use-synth-events)

;;; Internal variables

;;;###autoload
(defvar xwem-override-map nil
  "Keymap that overrides all other keymaps.
And did not considered as global map.  You should control end of
command execution for yourself if using it.
You should *bind* this, not set it.")

;;;###autoload
(defvar xwem-override-local-map nil
  "Keymap that overrides all local keymaps.
You should *bind* it, not set it.
Normally you should use `xwem-overriding-local-map' function.")

(defvar xwem-override-global-map nil
  "Keymap that is lookuped straight after local map.
You should *bind* it, not set it.")

;;;###autoload
(defvar xwem-kbd-now-grabbing nil
  "Non-nil indicates that we now grabbing keyboard.
Internal variable, do not modify.")

(defvar xwem-xkeys-mapping nil
  "X KeyMapping fetched from X server.
Internal variable, do not modify.")

(defvar xwem-xmods-mapping nil
  "List of keycodes for modifiers.
Corresponds to Shift, Lock,Control, Mod1, Mod2, Mod3, Mod4, and Mod5, in order.
INTERNAL VARIABLE, do not modify.")

;;;###xwem-autoload
(defvar xwem-event-client nil "Client where last key/mouse event occured.")
;;;###xwem-autoload
(defvar xwem-last-event nil "Last key/mouse event(Emacs event).")
;;;###xwem-autoload
(defvar xwem-last-xevent nil "Last key/mouse X Event.")
;;;###xwem-autoload
(defvar xwem-this-command-keys []
  "Vector of events that were used to invoke this command.")
;;;###xwem-autoload
(defvar xwem-this-command nil "The command now being executed.")
;;;###xwem-autoload
(defvar xwem-last-command nil "The last command executed.")

;; Private variables
(defvar xwem-kbd-private-prefix-map nil
  "Private variable holds prefix keymap or symbol that holds keymap.
Internal variable, do not modify.")

(defvar xwem-kbd-last-prefix-map nil
  "Last value of `xwem-kbd-private-prefix-map'.")

;; Special modes for reading key or keysequence
(defvar xwem-kbd-reading-key nil
  "Non-nil mean we are reading for key.
Actually references symbol to which save readed key.
Internal variable, do not modify.")

(defvar xwem-kbd-reading-keyseq nil
 "Non-nil mean we are reading key sequence.
Actually references symbol to which save readed key sequence.
Internal variable, do not modify.")

;;}}}


;;{{{ [-] Defining new key

;;;###autoload
(defun xwem-define-key (win keymap key command &optional pgrab-mode kgrab-mode)
  "Add KEY to KEYMAP to execute COMMAND and grab KEY on WIN.
When command is `nil', then undefine KEY in KEYMAP and ungrab KEY.

PGRAB-MODE and KGRAB-MODE specifies grabbing mode for pointer and
keyboard respectively."
  (define-key keymap key command)
  (xwem-kbd-graugra-key key win (if command 'grab 'ungrab)
                        nil pgrab-mode kgrab-mode))

;;;###autoload
(defun xwem-global-set-key (key command &optional pgrab-mode kgrab-mode)
  "Define KEY to call COMMAND, when xwem is already run.
If command is `nil' then undefine KEY in `xwem-global-map' and ungrab KEY."
  (define-key xwem-global-map key command)
  (mapc #'(lambda (cl)
            (xwem-kbd-graugra-key
             key (xwem-cl-xwin cl) (if command 'grab 'ungrab)
             nil pgrab-mode kgrab-mode))
        (xwem-clients-list))
  nil)

;;}}}

;;{{{ [-] Reading key and keysequence

(defvar xwem-saved-this-command-keys nil
  "Saved value of `xwem-this-command-keys'.")

;;;###autoload
(defun xwem-kbd (spec)
  "Just like `kbd' but take into account values of `xwem-hyper-modifier' and `xwem-meta-modifier'."
  (let ((keys (key-sequence-list-description (kbd spec))))
    (mapvector #'(lambda (key)
                   (let ((rkey (last key))
                         (rmods (butlast key)))
                     (when (member 'meta key)
                       (setq rmods (delete 'meta rmods))
                       (setq rkey (cons xwem-meta-modifier rkey)))
                     (when (member 'hyper key)
                       (setq rmods (delete 'hyper rmods))
                       (setq rkey (cons xwem-hyper-modifier rkey)))
                     (when (member 'control key)
                       (setq rmods (delete 'control rmods))
                       (setq rkey (cons xwem-control-modifier rkey)))

                     (when rmods
                       (setq rkey (nconc rmods rkey)))
                     rkey))
               keys)))

(defun xwem-read-keys-start (keyseq-p keyvar)
  "Start reading key or key sequence.
KEYSEQ-P is non-nil when staring reading key sequence.
KEYVAR is place where to store readed key or key sequence."
  (xwem-kbd-start-grabbing)

  (setq xwem-saved-this-command-keys xwem-this-command-keys)
  (setq xwem-this-command-keys [])
  (if keyseq-p
      (setq xwem-kbd-reading-keyseq keyvar)
    (setq xwem-kbd-reading-key keyvar))

  (xwem-recursive-edit))

(defun xwem-read-keys-stop (keyseq-p)
  "Stop reading key or key sequence.
If KEYSEQ-P is non-nil than stop reading key sequence."
  (set (if keyseq-p xwem-kbd-reading-keyseq xwem-kbd-reading-key)
       xwem-this-command-keys)
  (if keyseq-p
    (setq xwem-kbd-reading-keyseq nil)
    (setq xwem-kbd-reading-key nil))

  (xwem-kbd-set-current-prefix-keymap nil)
  (setq xwem-this-command-keys xwem-saved-this-command-keys)

  (xwem-exit-recursive-edit))

;; Read key, install active grab
;;;###autoload
(defun xwem-read-key (&optional prompt no-minib-focus-p)
  "Read single key press, prompting PROMPT in `xwem-minibuffer'.
If NO-MINIB-FOCUS-P is non-nil, focus `xwem-minibuffer' while reading."
  (xwem-kbd-stop-command-keys-echoing)
  (xwem-kbd-start-grabbing)
  (let ((event (car (if no-minib-focus-p
                        (xwem-next-command-event prompt)
                      (xwem-under-minibuffer
                       (xwem-next-command-event prompt))))))
    (unless inhibit-quit
      (when (equal (events-to-keys (vector event)) xwem-quit-key)
        (signal 'quit '(xwem))))
    event))

(defun xwem-read-key-sequence-1 (&optional continue-echo)
  (xwem-kbd-stop-command-keys-echoing)
  (xwem-kbd-start-grabbing)
  (let ((xwem-kbd-reading-keyseq t)
        (done nil)
        xev eev eevs bind)
    (while (not done)
      (XAllowEvents (xwem-dpy) X-SyncBoth)
      (setq xev (xwem-next-event))
      (when (setq eev (xwem-xevents->emacs-events (list xev) t))
        (setq xwem-last-xevent xev
;              xwem-event-client (xwem-event-client xev)
              xwem-last-event (car eev)
              xwem-this-command-keys (vconcat xwem-this-command-keys eev)
              eevs (vconcat eevs eev)
              bind (xwem-kbd-fixup-keymap
                    (xwem-lookup-key
                     xwem-event-client (events-to-keys eevs) t)))
        (xwem-kbd-schedule-command-keys-echoing)
        (unless (keymapp bind)
          (xwem-kbd-stop-command-keys-echoing)
          (setq done t))))
    eevs))

;; Read keysequence which binds to command
;;;###autoload
(defun xwem-read-key-sequence (&optional prompt no-minib-focus-p)
  "Read key sequence that call command prompting PROMPT."
  (when prompt
    (xwem-message 'prompt prompt))

  (let ((keys (if no-minib-focus-p
                  (xwem-read-key-sequence-1)
                (xwem-under-minibuffer
                 (xwem-read-key-sequence-1)))))
    (when prompt
      (xwem-clear-message))
    keys))

;;}}}

;;{{{ [-] local keymap

(defun xwem-client-set-local-keymap (cl prop keymap)
  (let ((nkeymap (xwem-kbd-fixup-keymap keymap))
        (okeymap (xwem-local-map cl)))
    (when (keymapp okeymap)
      (xwem-focus-mode-invoke cl 'before-keymap-change)
      ;; But avoid ungrabbing keymaps!  Because it can use prefix
      ;; keymap of some other command.
      (xwem-kbd-uninstall-grab
       okeymap (xwem-cl-xwin cl)
       #'(lambda (key def)
           (not (keymapp (xwem-kbd-fixup-keymap def))))))

    ;; Install new keymap
    (xwem-cl-put-sys-prop cl prop keymap) ; save it in system props
    (when (keymapp nkeymap)
      (xwem-kbd-install-grab nkeymap (xwem-cl-xwin cl))
      (xwem-focus-mode-invoke cl 'after-keymap-change))))

(defun xwem-client-get-local-keymap (cl prop)
  (xwem-kbd-fixup-keymap (xwem-cl-get-sys-prop cl prop)))

(define-xwem-client-property local-keymap nil
  "CL's local keymap."
  :type 'keymap
  :set 'xwem-client-set-local-keymap
  :get 'xwem-client-get-local-keymap)

;;;###autoload
(defun xwem-local-map (cl)
  "Return CL's local keymap."
  (xwem-client-property cl 'local-keymap))
(defsetf xwem-local-map (cl) (keymap)
  "Set CL's local keymap to KEYMAP."
  `(xwem-client-set-property ,cl 'local-keymap ,keymap))

;;;###autoload
(defun xwem-local-set-key (key command &optional cl pgrab-mode kgrab-mode)
  "Set KEY to call COMMAND in CL's local keymap.
If COMMAND is `nil' then undefine KEY in CL's local map and ungrab KEY.
If CL is ommited `xwem-cl-selected' assumed."
  (unless cl
    (setq cl (xwem-cl-selected)))

  ;; Create new local keymap for client if needed
  (unless (keymapp (xwem-local-map cl))
    (setf (xwem-local-map cl) (make-sparse-keymap)))

  (xwem-define-key
   (xwem-cl-xwin cl) (xwem-local-map cl) key command pgrab-mode kgrab-mode))

;;;###xwem-autoload
(defun xwem-use-local-map (keymap &optional cl)
  "Select KEYMAP as  local CL's keymap."
  (unless cl
    (setq cl (xwem-cl-selected)))

  (setf (xwem-local-map cl) keymap))

;;}}}

;;{{{ [-] Undefined command processing

;;;###autoload(autoload 'xwem-undefined-command "xwem-keyboard" "" t)
(define-xwem-command xwem-undefined-command ()
  "Called when key is not binded."
  (xwem-interactive)
  (signal 'undefined-keystroke-sequence xwem-this-command-keys))

;;;###autoload(autoload 'xwem-self-insert-or-undefined "xwem-keyboard" "" t)
(define-xwem-command xwem-self-insert-or-undefined (arg)
  "Self insert or undefined command.
Prefix ARG specifies how many characters to insert."
  (xwem-interactive "*_P")

  (let ((self-insert-p nil))
    (cond ((and (> (length xwem-this-command-keys) 1)
                (eq (aref xwem-this-command-keys 0) 'self-insert))
           ;; Adjust xwem-this-command-keys by removing leading 'self-insert
           (setq xwem-this-command-keys
                 (vconcat (cdr (append xwem-this-command-keys nil))))
           (setq self-insert-p t))

          ((= (length xwem-this-command-keys) 1)
           (setq self-insert-p t)))

    (if (not self-insert-p)
        (error 'xwem-error (format "%s is undefined"
                                   (key-description xwem-this-command-keys)))

      ;; Self insert command allowed only for normal clients to avoid
      ;; infinite loops.
      (unless (eq (xwem-dummy-client) (xwem-cl-selected))
        (xwem-kbd-add-pending-keys
         (apply 'vconcat (make-list (prefix-numeric-value arg)
                                    xwem-this-command-keys))
         (xwem-cl-selected))))))

;;}}}

;;{{{ [-] Quiting keyboarding

;;;###autoload(autoload 'xwem-keyboard-quit "xwem-keyboard" "" t)
(define-xwem-command xwem-keyboard-quit ()
  "Send quit signal."
  (xwem-interactive)

  (setq xwem-override-map nil)
  (signal 'quit '(xwem))

  ;; NOT REACHED
  (xwem-message 'error "quit."))

;;;###autoload(autoload 'xwem-kbd-quit "xwem-keyboard" "" t)
(define-xwem-command xwem-kbd-quit ()
  "Quit from keyboard haldling."
  (xwem-interactive)

  (xwem-kbd-stop-grabbing)
  (setq xwem-kbd-private-prefix-map nil)
  (XSetInputFocus (xwem-dpy) X-PointerRoot X-RevertToPointerRoot X-CurrentTime)
  (xwem-play-sound 'quit)
  (xwem-message 'note "[kbd-quit] InputFocus set PointerRoot"))

;;}}}

;;{{{ [-] Converters

;; Convertors
(defun xwem-kbd-xksym->emacs (ksym)
  "Convert KSYM to Emacs key symbol."
  (cond
   ;; ksym is list for buttons
   ((listp ksym)
    (let ((kval (car ksym)))
      (cond ((= kval 1) 'button1)
            ((= kval 2) 'button2)
            ((= kval 3) 'button3)
            ((= kval 4) 'button4)
            ((= kval 5) 'button5)
            (t nil))))                  ;can't be!!!

   ((= ksym XK-BackSpace) 'backspace)
   ((= ksym XK-Tab) 'tab)
   ((= ksym XK-ISO-Left-Tab) 'iso-left-tab)
   ((= ksym XK-Linefeed) 'linefeed)
   ((= ksym XK-Return) 'return)
   ((= ksym XK-Escape) 'escape)
   ((= ksym XK-Delete) 'delete)

   ((= ksym XK-Home) 'home)
   ((= ksym XK-End) 'end)
   ((= ksym XK-Left) 'left)
   ((= ksym XK-Right) 'right)
   ((= ksym XK-Up) 'up)
   ((= ksym XK-Down) 'down)

   ((= ksym XK-Insert) 'insert)
   ((= ksym XK-Pause) 'pause)
   ((= ksym XK-Space) 'space)

   ((= ksym XK-Next) 'next)
   ((= ksym XK-Prior) 'prior)
   ;; TODO: add more

   ((= ksym XK-F1) 'f1)
   ((= ksym XK-F2) 'f2)
   ((= ksym XK-F3) 'f3)
   ((= ksym XK-F4) 'f4)
   ((= ksym XK-F5) 'f5)
   ((= ksym XK-F6) 'f6)
   ((= ksym XK-F7) 'f7)
   ((= ksym XK-F8) 'f8)
   ((= ksym XK-F9) 'f9)
   ((= ksym XK-F10) 'f10)
   ((= ksym XK-F11) 'f11)
   ((= ksym XK-F12) 'f12)

   ((= ksym 0) nil)

   (t (or (X-XKeysymDB-keysym->sym ksym)
          (XCharacter ksym))))) ;nil or proper character

(defun xwem-kbd-emacs->xksym (ksym)
  "Convert back from Emacs key symbol KSYM to proper X key symbol."
  (cond ((null ksym) 0)                 ;hmm

        ((symbolp ksym)
         (let ((osymname (symbol-name ksym))
               (symname (downcase (symbol-name ksym))))
           (cond ((string= symname "backspace") XK-BackSpace)
                 ((string= symname "tab") XK-Tab)
                 ((string= symname "iso-left-tab") XK-ISO-Left-Tab)
                 ((string= symname "linefeed") XK-Return) ;XXX
                 ((string= symname "linefeed") XK-Linefeed)
                 ((string= symname "return") XK-Return)
                 ((string= symname "escape") XK-Escape)
                 ((string= symname "delete") XK-Delete)
                 ((string= symname "space") XK-Space)

                 ((string= symname "home") XK-Home)
                 ((string= symname "end") XK-End)
                 ((string= symname "left") XK-Left)
                 ((string= symname "right") XK-Right)
                 ((string= symname "up") XK-Up)
                 ((string= symname "down") XK-Down)

                 ((string= symname "insert") XK-Insert)
                 ((string= symname "pause") XK-Pause)

                 ((string= symname "next") XK-Next)
                 ((string= symname "prior") XK-Prior)

                 ;; Mouse buttons
                 ((string= symname "button1") (list X-XButton1))
                 ((string= symname "button2") (list X-XButton2))
                 ((string= symname "button3") (list X-XButton3))
                 ((string= symname "button4") (list X-XButton4))
                 ((string= symname "button5") (list X-XButton5))

                 ;; Functional keys
                 ((string-match "^[fF]\\([0-9]+\\)$" symname)
                  (symbol-value
                   (intern
                    (concat "XK-F"
                            (substring symname (match-beginning 1)
                                       (match-end 1))))))

                 (t (or (X-XKeysymDB-sym->keysym ksym)
         (char-to-int (string-to-char osymname)))))))

        ((characterp ksym) (char-to-int ksym)) ;Should not be there

        (t 0)))                         ;HMM!

(defun xwem-kbd-emods->xmodmask (emods)
  "Convert Emacs modifiers list EMODS to X modifers mask."
  (apply 'Xmask-or 0 (mapcar #'(lambda (mod)
                                 (or (get mod 'x-mod-mask) 0))
                             emods)))

(defun xwem-kbd-xmodmask->emods (mmask)
  "Convert X modifiers mask MMASK to Emacs modifiers list."
  (delq nil (mapcar #'(lambda (mod)
                        (and (Xtest mmask (or (get mod 'x-mod-mask) 0))
                             mod))
                    '(shift control alt meta hyper super))))

;; keysyms and keycodes converters
(defun xwem-kbd-xksym->xkcode (ksym)
  "Convert keysym KSYM to keycode.
Convert keysym to cons cell where car is keycode and cdr is modifiers
list, using `xwem-xkeys-mapping' list.
NOTE: only 'shift modifier supported."
  (let* ((kcode (X-Dpy-min-keycode (xwem-dpy)))
         (kslist (car (last xwem-xkeys-mapping)))
         (ksyms-per-kcode (length (car kslist)))
         (ksym-off 0)
         (kmods nil))
    (while kslist
      (cond ((= ksym (or (nth ksym-off (car kslist)) -1))
             (setq kslist nil))
            ((= ksym (or (nth (1+ ksym-off) (car kslist)) -1))
             (setq kslist nil
                   kmods (cons 'shift kmods)))
            (t (setq kslist (cdr kslist)
                     kcode (1+ kcode))
               (when (null kslist)
                 (setq ;kcode (X-Dpy-min-keycode (xwem-dpy))
                       ksym-off (+ 2 ksym-off))
                 (when (< ksym-off ksyms-per-kcode)
                   (setq kslist (car (last xwem-xkeys-mapping))))))))
    (cons kcode kmods)))

(defun xwem-kbd-xkcode->xksym (kcode)
  "Convert key code KCODE to keysym.
KeyCode -> KeySyms list in form (base shift caps ShiftCaps)."
  (nth (- kcode (X-Dpy-min-keycode (xwem-dpy))) (car (last xwem-xkeys-mapping)))
  )

;;; Various subroutines
(defun xwem-kbd-emod->kcode (emod &optional any)
  "Convert Emacs modifier EMOD to X keycode.
Behaviour is undefined If ANY argument is supplied.
DO NOT RELY ON THIS FUNCTION."
  (funcall (if any 'identity 'car) (get emod 'x-key-codes)))

(defun xwem-kbd-kcode->emod (kcode)
  "Convert key code KCODE to Emacs modifier, if KCODE is actually a modifier.
See also `xwem-kbd-kcode-modifier-p'."
  (let ((emods '(shift control alt meta super hyper)))
    (while (and emods (not (member kcode (get (car emods) 'x-key-codes))))
      (setq emods (cdr emods)))
    (car emods)))

;;;###xwem-autoload
(defun xwem-kbd-kcode-modifier-p (kcode)
  "Return non-nil if key code KCODE is modifier."
  (member kcode (apply 'append xwem-xmods-mapping)))

(defun xwem-kbd-adjust-keycode (keycode modifiers)
  "Convert KEYCODE to keysym according to MODIFIERS."
  ;; XXX only 'shift modifier supported
  (if (and (member 'shift modifiers)
           (> (cadr (xwem-kbd-xkcode->xksym keycode)) 0))
      (cadr (xwem-kbd-xkcode->xksym keycode))

    (car (xwem-kbd-xkcode->xksym keycode))))

(defun xwem-kbd-adjust-modifiers (keycode modifiers)
  "According to KEYCODE adjust MODIFIERS, i.e. remove 'shift."
  (if (and (member 'shift modifiers)
           keycode (> (cadr (xwem-kbd-xkcode->xksym keycode)) 0))
      ;; 'shift in modifiers and keysym is valid, so we remove 'shift
      (remove 'shift modifiers)

    modifiers))

(defun xwem-kbd-hack-frame ()
  (let ((vf (visible-frame-list))
        (sf (selected-frame)))
    (if (memq sf vf)
        sf
      (car vf))))

(defun xwem-kbd-hack-frame-cl ()
  (let ((hf (xwem-kbd-hack-frame)))
    (if (eq hf (xwem-minib-frame xwem-minibuffer))
        (xwem-minib-cl xwem-minibuffer)
      (xwem-misc-find-cl-by-emacs-frame hf))))

(defun xwem-kbd-hack-mouse (xev)
  "Return (X . Y) to be used in mouse Emacs event."
  (let ((cl (xwem-kbd-hack-frame-cl))
        xpnt x y)
    (if (member (X-Event-type xev) (list X-ButtonPress X-ButtonRelease))
        (setq x (X-Event-xbutton-root-x xev)
              y (X-Event-xbutton-root-y xev))
      (setq x (X-Event-xmotion-root-x xev)
            y (X-Event-xmotion-root-y xev)))
    (when cl
      (setq xpnt (car (XTranslateCoordinates
                       (xwem-dpy) (xwem-cl-xwin cl)
                       (xwem-rootwin) 0 0))))

    (when xpnt
      (setq x (- x (X-Point-x xpnt)))
      (setq y (- y (X-Point-y xpnt))))
    (cons x y)))

;;;###xwem-autoload
(defun xwem-xevents->emacs-events (xevs &optional trust-modbits)
  "Convert X-Events XEVS to Emacs events.
If TRUST-MODBITS is non-nil than we can trust modifier bits in
X-Event, otherwise we are trying to keep track of modifiers presses
and releases.  This is needed because core X events obtained from
RECORD extension does not have valid inforamtion about modifiers
bits."
  ;; NOTE: events grabbed using RECORD extension does not have correct
  ;; state field.
  (let (current-modifiers)
    (flet ((xwem-xevent->emacs-event (xev)
             (let ((xevtype (X-Event-type xev))
                   kcode mbutton)
               (unless (member xevtype
                               (list X-KeyPress X-KeyRelease
                                     X-ButtonPress X-ButtonRelease
                                     X-MotionNotify))
                 (error 'xwem-error "Invalid event type: %s"
                        (X-Event-name xev)))

               (cond ((member xevtype (list X-KeyPress X-KeyRelease))
                      (setq kcode (X-Event-xkey-keycode xev)))
                     ((member xevtype (list X-ButtonPress
                                            X-ButtonRelease))
                      (setq mbutton (X-Event-xbutton-button xev))))

               (if (xwem-kbd-kcode-modifier-p kcode)
                   ;; KCODE is actually modifier
                   (if (= (X-Event-type xev) X-KeyPress)
                       (setq current-modifiers
                             (cons (xwem-kbd-kcode->emod kcode)
                                   current-modifiers))
                     (setq current-modifiers
                           (delete (xwem-kbd-kcode->emod kcode)
                                   current-modifiers)))

                 ;; Skip normal key realese events
                 (unless (eq xevtype X-KeyRelease)

                   ;; If we can trust modifier bits, get
                   ;; information about modifiers currently
                   ;; pressed from state field.
                   (when trust-modbits
                     (setq current-modifiers
                           (xwem-kbd-xmodmask->emods
                            (cond ((eq xevtype X-KeyPress)
                                   (X-Event-xkey-state xev))
                                  ((member xevtype
                                           (list X-ButtonPress
                                                 X-ButtonRelease))
                                   (X-Event-xbutton-state xev))
                                  ((eq xevtype X-MotionNotify)
                                   (X-Event-xmotion-state xev))))))

                   (make-event
                    (cond ((eq xevtype X-KeyPress) 'key-press)
                          ((eq xevtype X-ButtonPress) 'button-press)
                          ((eq xevtype X-ButtonRelease) 'button-release)
                          ((eq xevtype X-MotionNotify) 'motion)
                          (t (error 'xwem-error "Unknown event: %s"
                                    (X-Event-name xev))))
                    (nconc (when (member xevtype (list X-ButtonPress
                                                       X-ButtonRelease
                                                       X-MotionNotify))
                             (let ((xm (xwem-kbd-hack-mouse xev)))
                               (list 'channel (xwem-kbd-hack-frame)
                                     'x (car xm) 'y (cdr xm))))
                           (list 'modifiers (xwem-kbd-adjust-modifiers
                                             kcode current-modifiers))
                           (when (eq xevtype X-KeyPress)
                             (list 'key (xwem-kbd-xksym->emacs
                                         (xwem-kbd-adjust-keycode
                                          kcode current-modifiers))))
                           (when (member xevtype (list X-ButtonPress
                                                       X-ButtonRelease))
                             (list 'button mbutton)))))))))
      ;; Construct list of Emacs events
      (delete* nil (mapcar #'xwem-xevent->emacs-event
                     (cond ((listp xevs) (vconcat xevs))
                           ((vectorp xevs) xevs)
                           (t xevs)))
               :test #'(lambda (el1 el2) (not (eventp el2))))
      )))

(defun xwem-kbd-emacs-events->xevents (events)
  "Convert Emacs EVENTS list to X events list."
  )

;;;###xwem-autoload
(defun xwem-kbd-ekeys->eevents (ekeys)
  "Convert Emacs keys sequence EKEYS to Emacs events vector."
  (mapvector #'(lambda (key)
                 (let (mods keychar)

                   (if (listp key)
                       (progn
                         (setq keychar (car (last key)))
                         (setq mods (butlast key)))
                     (setq keychar key))

                   (make-event 'key-press (list 'modifiers mods
                                                'key keychar))))
             ekeys))

;;}}}

;;{{{ [-] Sending

;; Sending (using XTEST)
(defun xwem-key-send-xtest-internal (&optional keycode-seq)
  "Emulate key presses/releases of KEYCODE-SEQ sequence using XTEST extension."
  (mapc #'(lambda (ksel)
            (let ((ktype (car ksel))
                  (kcode nil)
                  (ktime nil))
              (cond ((vectorp (cdr ksel))
                     (setq kcode (aref (cdr ksel) 0))
                     (setq ktime (aref (cdr ksel) 1)))
                    (t (setq kcode (cdr ksel))
                       (setq ktime X-CurrentTime)))
              (X-XTest-FakeInput (xwem-dpy) ktype kcode X-None 0 0 ktime)))
        keycode-seq))

(defun xwem-key-send-xtest (keys)
  "Send Emacs key sequence KEYS using XTEST extension."
  (let (lseq)
    (mapc #'(lambda (key)
              (let* ((mods (mapcar #'xwem-kbd-emod->kcode (butlast key)))
                     (subcode (xwem-kbd-emacs->xksym (car (last key))))
                     (codtype (if (listp subcode)
                                  (list X-Xtest-ButtonPress X-Xtest-ButtonRelease)
                                (list X-Xtest-KeyPress X-Xtest-KeyRelease)))
                     (code (unless (listp subcode)
                             (xwem-kbd-xksym->xkcode subcode))))
                (if code
                    ;; For key event
                    (setq mods (nconc mods
                                      (mapcar #'xwem-kbd-emod->kcode (cdr code)))
                          code (car code))
                  ;; For button event
                  (setq code (car subcode)))

                (mapc #'(lambda (mod)
                          (setq lseq (cons (cons X-Xtest-KeyPress mod) lseq)))
                      mods)
                (setq lseq (cons (cons (first codtype) code) lseq))
                (setq lseq (cons (cons (second codtype) code) lseq))
                (mapc #'(lambda (mod)
                          (setq lseq (cons (cons X-Xtest-KeyRelease mod) lseq)))
                      mods)))
          (key-sequence-list-description keys))

    (setq lseq (nreverse lseq))
    (xwem-key-send-xtest-internal lseq)))

;; Sending (using XSendEvent)
(defun xwem-key-send-synth (keys &optional client)
  "Send synthesize KEYS to CLIENT.
If CLIENT is ommited, selected client is used."
  (unless client
    (setq client (xwem-cl-selected)))

  (when (xwem-cl-alive-p client)
    (mapc #'(lambda (key)
              (let* ((mods (butlast key))
                     (subcode (xwem-kbd-emacs->xksym (car (last key))))
                     (codtype (if (listp subcode) X-ButtonPress X-KeyPress))
                     (code (unless (listp subcode)
                             (xwem-kbd-xksym->xkcode subcode))))
                (if code
                    (setq mods (nconc mods (cdr code))
                          code (car code))
                  (setq code (car subcode)))

                (xwem-XSendEvent
                 (xwem-dpy) (xwem-cl-xwin client) nil
                 (if (eq codtype X-KeyPress) XM-KeyPress XM-ButtonPress)
                 (X-Create-message
                  (list [1 codtype]     ;type
                        [1 code]        ;detail
                        [2 2806]        ; XXX seq
                        [4 X-CurrentTime] ; time
                        [4 (X-Win-id (xwem-rootwin))] ; root
                        [4 (X-Win-id (xwem-cl-xwin client))] ; event
                        [4 X-None]      ; child
                        [2 0]           ; root-x
                        [2 0]           ; root-y
                        [2 0]           ; event-x
                        [2 0]           ; event-y
                        [2 (xwem-kbd-emods->xmodmask mods)] ; state
                        [1 t]           ; same-screen
                        [1 nil]))
                 (let ((xev (make-ffi-object 'XEvent)))
                   (if (eq codtype X-KeyPress)
                       (setf (XKeyEvent->type xev) codtype
                             (XKeyEvent->keycode xev) code
                             (XKeyEvent->time xev) X-CurrentTime
                             (XKeyEvent->root xev) (X-Win-id (xwem-rootwin))
                             (XKeyEvent->window xev) (X-Win-id (xwem-cl-xwin client))
                             (XKeyEvent->state xev) (xwem-kbd-emods->xmodmask mods)
                             (XKeyEvent->same_screen xev) 1
                             (XKeyEvent->x xev) 0
                             (XKeyEvent->y xev) 0
                             (XKeyEvent->x_root xev) 0
                             (XKeyEvent->y_root xev) 0)
                     (setf (XButtonEvent->type xev) codtype
                           (XButtonEvent->button xev) code
                           (XButtonEvent->time xev) X-CurrentTime
                           (XButtonEvent->root xev) (X-Win-id (xwem-rootwin))
                           (XButtonEvent->window xev) (X-Win-id (xwem-cl-xwin client))
                           (XButtonEvent->state xev) (xwem-kbd-emods->xmodmask mods)
                           (XButtonEvent->same_screen xev) 1
                           (XButtonEvent->x xev) 0
                           (XButtonEvent->y xev) 0
                           (XButtonEvent->x_root xev) 0
                           (XButtonEvent->y_root xev) 0))
                   (ffi-address-of xev)))))
          (key-sequence-list-description keys))))

;;;###xwem-autoload
(defun xwem-kbd-wait-key-release (keycode)
  "Wait for key with KEYCODE for release."
  (unless (xwem-keymacro-executing-p)
    ;; Now wait key release event
    (XAllowEvents (xwem-dpy) X-SyncBoth)
    (XNextEvent (xwem-dpy) nil
                #'(lambda (xev)
                    (and (= (X-Event-type xev) X-KeyRelease)
                         (= (X-Event-xkey-keycode xev) keycode))))))

;;;###xwem-autoload
(defun xwem-kbd-force-mods-release (&optional mods)
  "Force release of MODS modifiers."
  (unless xwem-keyboard-use-synth-events
    ;; Only needed when using XSendEvent for emulating keys
    (xwem-key-send-xtest-internal
     (mapcar #'(lambda (el) (cons X-Xtest-KeyRelease el))
	     (apply 'append
		    (mapcar #'(lambda (mod)
                                (get mod 'x-key-codes))
			    (or mods '(shift control meta super hyper))))))
    (XFlush (xwem-dpy))))

(defun xwem-kbd-wait-button-release (button)
  "Wait for BUTTON for release."
  )

(define-xwem-deferred xwem-cl-apply-pending-keys (cl)
  "Apply pending keys to CL."
  (when (and (xwem-cl-alive-p cl)
             (xwem-cl-get-sys-prop cl 'pending-keys))
    (if (xwem-client-local-variable-value cl 'xwem-keyboard-use-synth-events)
        ;; Using XSendEvent
        (xwem-key-send-synth (xwem-cl-get-sys-prop cl 'pending-keys) cl)

      ;; Using XTEST
      (let ((xattr (XGetWindowAttributes (xwem-dpy) (xwem-cl-xwin cl)))
            qt)
        (unless (= (X-Attr-mapstate xattr) X-Viewable)
          (setq qt (XQueryTree (xwem-dpy) (xwem-cl-xwin cl)))
          ;; Make CL viewable
          (XReparentWindow (xwem-dpy) (xwem-cl-xwin cl)
                           (xwem-cl-xwin (xwem-dummy-client)) 0 0)
          (XMapWindow (xwem-dpy) (xwem-cl-xwin cl)))

        (xwem-focus-push-set (xwem-cl-xwin cl))
        (xwem-key-send-xtest (xwem-cl-get-sys-prop cl 'pending-keys))
        (xwem-focus-pop-set)
        (XFlush (xwem-dpy))

        (unless (= (X-Attr-mapstate xattr) X-Viewable)
          ;; Restore mapping
          (when (= (X-Attr-mapstate xattr) X-Unmapped)
            (XUnmapWindow (xwem-dpy) (xwem-cl-xwin cl)))
          ;; Restore parent back to its parent
          (XReparentWindow (xwem-dpy) (xwem-cl-xwin cl) (nth 3 qt)
                           (X-Geom-x (xwem-cl-xgeom cl))
                           (X-Geom-y (xwem-cl-xgeom cl))))))

    ;; Remove pending keys
    (xwem-cl-rem-sys-prop cl 'pending-keys)))

;;;###xwem-autoload
(defun xwem-kbd-add-pending-keys (keys &optional cl)
  "Add KEYS as pending to be pressed in CL.
If CL is ommited selected client considered."
  (unless cl
    (setq cl (xwem-cl-selected)))
  (xwem-cl-put-sys-prop cl 'pending-keys
    (vconcat (xwem-cl-get-sys-prop cl 'pending-keys)
             keys))
  (xwem-cl-apply-pending-keys cl))

;;}}}

;;{{{ [-] keypress/KeyRelease processing

;;;###xwem-autoload
(defun xwem-kbd-add-default-keymap (keymap)
  "Add KEYMAP to default keymaps.
KEYMAP MUST NOT HAS PARENTS!"
  (unless (memq keymap (keymap-parents xwem-default-parent-map))
    (set-keymap-parents xwem-default-parent-map
                        (cons keymap (keymap-parents xwem-default-parent-map)))))


;;; Keys echoing
(defvar xwem-kbd-echoing-keys nil "Non-nil mean we are echoing command keys.")
(defvar xwem-kbd-scheduled-keys [] "Keys scheduled for echoing.")
(defvar xwem-kbd-scheduled-timer nil)

(defun xwem-kbd-echo-command-keys (&optional skip)
  "Echo keys."
  (xwem-clear-message 'keys-continuator)
  ;; Clear echo area if not yet reading, otherwise remove continuator
  (if (and (not xwem-kbd-echoing-keys)
           (not (eq (current-message-label) 'prompt)))
      (xwem-clear-message)
    (xwem-clear-message 'keys))
  (xwem-message-append 'keys "%s " (key-description xwem-kbd-scheduled-keys))
  (unless skip
    (xwem-message-append 'keys-continuator "-"))

  (setq xwem-kbd-echoing-keys t))

(defun xwem-kbd-echo-keys-timer (&rest not-used)
  "Timer to call when there a need to show command keys."
  (when (> (length xwem-kbd-scheduled-keys) 0)
    (xwem-kbd-echo-command-keys))
  (setq xwem-kbd-scheduled-timer nil))

;;;###xwem-autoload
(defun xwem-kbd-schedule-command-keys-echoing ()
  "Schedule echoing pressed KEY in minibuffer."
  (when (X-Event-p xwem-last-xevent)
    ;; Echo only for events which has corresponding X Event.
    (when xwem-kbd-scheduled-timer
      (disable-timeout xwem-kbd-scheduled-timer)
      (setq xwem-kbd-scheduled-timer nil))
    (when (numberp xwem-keyboard-echo-keystrokes)
      (setq xwem-kbd-scheduled-keys
            (vconcat xwem-kbd-scheduled-keys (list xwem-last-event)))
      (if xwem-kbd-echoing-keys
          (xwem-kbd-echo-command-keys t)
        (xwem-deferred-funcall
         #'(lambda ()
             (when (numberp xwem-keyboard-echo-keystrokes)
               (setq xwem-kbd-scheduled-timer
                     (add-timeout xwem-keyboard-echo-keystrokes
                                  'xwem-kbd-echo-keys-timer nil)))))))))

;;;###xwem-autoload
(defun xwem-kbd-stop-command-keys-echoing ()
  "Stop echoing."
  (when xwem-kbd-scheduled-timer
    (disable-timeout xwem-kbd-scheduled-timer)
    (setq xwem-kbd-scheduled-timer nil))

  (unless xwem-kbd-echoing-keys
    (xwem-clear-message 'keys))
  (xwem-clear-message 'keys-continuator)

  (setq xwem-kbd-scheduled-keys []
        xwem-kbd-echoing-keys nil))

;;;###xwem-autoload
(defun xwem-kbd-fixup-keymap (keymap)
  "Return fixed KEYMAP.
KEYMAP might be valid keymap or symbol, which `symbol-function' is keymap.
If KEYMAP can't be fixed, just return it."
  (unless (null keymap)
    (let (done kmap)
      (while (not done)
        (condition-case nil
            (setq kmap (indirect-function keymap))
          (t (setq kmap nil)))
        (cond ((keymapp kmap)
               (setq done t))

              ;; Handle autoload
              ((and (symbolp keymap)
                    (consp kmap)
                    (eq (car kmap) 'autoload)
                    (eq (car (cdr (cdr (cdr (cdr kmap))))) 'keymap))
               (load (cadr kmap)))

              (t (setq done t))))
      kmap)))

(defun xwem-kbd-keymap-plist (keymap)
  "Return KEYMAP's plist."
  (when (and (symbolp keymap)
             (keymapp (xwem-kbd-fixup-keymap keymap)))
    (symbol-plist keymap)))

;;;###xwem-autoload
(defun xwem-kbd-current-map ()
  "Return current prefix map."
  (let ((cm (or xwem-kbd-private-prefix-map
                xwem-override-map
                xwem-override-local-map
                (and (xwem-cl-p xwem-event-client)
                     (xwem-local-map xwem-event-client))
                xwem-override-global-map
                xwem-global-map)))
    (xwem-kbd-fixup-keymap cm)))

;;;###xwem-autoload
(defun xwem-kbd-global-map-current-p ()
  "Return non-nil if `xwem-global-map' is current prefix map."
  (and (not xwem-kbd-private-prefix-map) (not xwem-override-map)))

;;;###xwem-autoload
(defun xwem-kbd-set-current-prefix-keymap (newkeymap)
  "Set current keymap to NEWKEYMAP.

Use (xwem-kbd-set-current-prefix-keymap nil) to set current keymap
to `xwem-global-keymap' instead of
(xwem-kbd-set-current-prefix-keymap xwem-global-map)."
  (setq xwem-kbd-private-prefix-map newkeymap))

(defun xwem-kbd-handle-keyrelease (xdpy win xev)
  "On XDPY and window WIN handle KeyRelease event XEV."
  ;; TODO:
  ;;  - modifiers tracker
  nil)

;;;###xwem-autoload
(defun xwem-lookup-map (client keys &optional accept-default)
  "Lookup keymap where KEYS command is defined."
  (unless client
    (setq client (xwem-cl-selected)))

  (let (lkmap bind)
    (cond ((setq lkmap (or xwem-kbd-private-prefix-map
                           xwem-override-map
                           xwem-override-local-map))
           (and (lookup-key (xwem-kbd-fixup-keymap lkmap) keys accept-default)
                lkmap))

          ;; Try local keymap
          ((and (setq lkmap (xwem-local-map client))
                (setq bind (lookup-key (xwem-kbd-fixup-keymap lkmap)
                                       keys accept-default)))
           (and bind lkmap))

          ;; Try minor modes keymaps and global keymap
          (t (let* ((mm-bind (xwem-minor-mode-key-binding
                              client keys accept-default))
                    (mm-km-sym (and mm-bind
                                    (cdr (assq (car mm-bind)
                                               xwem-minor-mode-map-alist))))
                    (mm-km (and mm-km-sym
                                (if (xwem-client-local-variable-p mm-km-sym)
                                    (xwem-client-local-variable-value
                                     client mm-km-sym)
                                  (symbol-value mm-km-sym)))))
               ;; If no minor mode keymap, use global map
               (or mm-km
                   (and (lookup-key (xwem-kbd-fixup-keymap xwem-global-map)
                                    keys accept-default)
                        xwem-global-map)))))))

;;;###xwem-autoload
(defun xwem-global-key-binding (cl keys &optional accept-default)
  "Return global binding for KEYS."
  (lookup-key xwem-global-map keys accept-default))

;;;###xwem-autoload
(defun xwem-local-key-binding (cl keys &optional accept-default)
  "Return local binding for KEYS."
  (let ((lkmap (xwem-local-map cl)))
    (and (keymapp lkmap)
         (lookup-key lkmap keys accept-default))))

;;;###xwem-autoload
(defun xwem-minor-mode-key-binding (cl keys &optional accept-default)
  "Return CL's minor mode binding for KEYS.
Retun cons cell in form `(MODENAME . BINDING)'."
  (let ((mlist xwem-minor-mode-map-alist)
        (bind nil))
    ;; Scan minor modes for binding
    (while (and mlist (not bind))
      (let ((mode-sym (car (car mlist)))
            (km-sym (cdr (car mlist)))
            kmap)
        (when (if (xwem-client-local-variable-p mode-sym)
                  (xwem-client-local-variable-value cl mode-sym)
                (symbol-value mode-sym))
          (setq kmap (if (xwem-client-local-variable-p km-sym)
                         (xwem-client-local-variable-value cl km-sym)
                       (symbol-value km-sym)))
          (when (keymapp kmap)
            (setq bind (lookup-key kmap keys accept-default)))))
      (unless bind
        (setq mlist (cdr mlist))))
    (when bind
      (cons (car (car mlist)) bind))))

;;;###xwem-autoload
(defun xwem-lookup-key (client keys &optional accept-default)
  "In CLIENT's context, lookup for KEYS binding.
`xwem-lookup-key' omits default binding unless ACCEPT-DEFAULT is
non-nil."
  (unless client
    (setq client (xwem-cl-selected)))

  (let (lkmap)
    (if (setq lkmap (or (and (= (length keys) 1)
                             xwem-kbd-private-prefix-map)
                        xwem-override-map
                        xwem-override-local-map))
        (lookup-key (xwem-kbd-fixup-keymap lkmap) keys accept-default)

      ;; Try looking up in next order:
      ;;   - Minor modes map
      ;;   - Local map
      ;;   - Default parent map
      ;;   - Global map
      (or (cdr (xwem-minor-mode-key-binding client keys accept-default))
          (xwem-local-key-binding client keys accept-default)
          (xwem-global-key-binding client keys accept-default)))))

;;;###xwem-autoload
(defun xwem-kbd-get-binding (keys &optional client reject-default)
  "Get binding value for KEYS for CLIENT.
By default CLIENT is currently selected client.
If optional REJECT-DEFAULT is non-nil, skip looking up default binding
\(pass `nil' as ACCEPT-DEFAULT to `lookup-key'\)."
  (xwem-lookup-key client keys (not reject-default)))

(defun xwem-kbd-fixate-current-lkm (lkm)
  "In case LKM is not usable, fixate it.
Return fixated LKM."
  (when (null lkm)
    (let ((etk (events-to-keys (vector xwem-last-event))))
      (cond ((equal etk xwem-help-key)
             (setq lkm xwem-prefix-help-command))
            ((equal etk xwem-quit-key)
             (setq lkm xwem-quit-command))

            ;; Last chance in re-lookup command keys
            (t (setq lkm (or (xwem-lookup-key
                              xwem-event-client xwem-this-command-keys)
                             (xwem-lookup-key
                              xwem-event-client xwem-this-command-keys t)))))))
  lkm)

;;;###xwem-autoload
(defun xwem-kbd-dispatch-binding (lkm)
  "Process keymap or command entry LKM after `lookup-key'.
Return non-nil if some action was performed."
  ;; Check for special mode, when we just reading single keystroke
  (when xwem-kbd-reading-key
    (xwem-read-keys-stop nil))

  ;; Fixate LKM, if it is bad
  (setq lkm (xwem-kbd-fixate-current-lkm lkm))

  (xwem-debug 'xwem-event "KBD Dispatcher: %S, lkm = %S"
              'xwem-this-command-keys 'lkm)

  (if (keymapp (xwem-kbd-fixup-keymap lkm))
      (progn
        ;; Subkeymap
        (xwem-kbd-set-current-prefix-keymap lkm)
        (xwem-kbd-start-grabbing
         (eval (plist-get (xwem-kbd-keymap-plist lkm) 'cursor)))

        ;; Show keymap's prompt
        (let ((prompt (or (eval (plist-get (xwem-kbd-keymap-plist lkm) 'prompt))
                          (keymap-prompt lkm))))
          (if prompt
              (xwem-message 'prompt "%s" prompt)
            (xwem-kbd-schedule-command-keys-echoing))))

    ;; Check are we reading keysequence now
    (when xwem-kbd-reading-keyseq
      (xwem-read-keys-stop t))

    ;; Reset privat keymap prefix
    (xwem-kbd-set-current-prefix-keymap nil)

    ;; Now run command or keyboard macro
    (xwem-unwind-protect
        (cond ((or (vectorp lkm) (stringp lkm))
               ;; Keyboard macro.
               ;; Wait for keyrelease, ungrab keyboard, than play it.
               (when (and (X-Event-p xwem-last-xevent)
                          (= (X-Event-type xwem-last-xevent) X-KeyPress))
                 (xwem-kbd-wait-key-release
                  (X-Event-xkey-keycode xwem-last-xevent)))
               (xwem-kbd-schedule-command-keys-echoing)
               (xwem-kbd-stop-grabbing)
               (xwem-keymacro-internal-play
                lkm (prefix-numeric-value xwem-prefix-arg)))

              ((commandp lkm)
               ;; Fix LKM in case it is frame command
               (when (and (symbolp lkm)
                          (get lkm 'xwem-frame-command)
                          (not (or (xwem-frame-p (xwem-cl-frame xwem-event-client))
                                   (eq (xwem-dummy-client) xwem-event-client))))
                 (setq lkm 'ignore))
               (xwem-kbd-schedule-command-keys-echoing)

               ;; Execute LKM command
               (setq xwem-this-command lkm)
               (run-hooks 'xwem-pre-command-hook)
               (command-execute xwem-this-command)
               (setq xwem-last-command xwem-this-command)
               (run-hooks 'xwem-post-command-hook)
               (setq xwem-this-command nil))

              ((null lkm)
               ;; Just echo key
               (xwem-kbd-schedule-command-keys-echoing))

              (t (error 'xwem-error (format "Unknown command: '%S'" lkm))))

      ;; XXX Generic post command processing
      (when (xwem-kbd-global-map-current-p)
        (setq xwem-this-command-keys []))
      ;; Do it deferring, becase there maybe pending command events
      ;; which need to be processed.
      (xwem-deferred-funcall
       #'(lambda ()
           (when (xwem-kbd-global-map-current-p)
             (xwem-kbd-stop-command-keys-echoing)
             (xwem-kbd-stop-grabbing)))))

    (and lkm 'done)))

;;}}}

;;{{{ [-] Grabbing

;;;###xwem-autoload
(defun xwem-kbd-graugra-key (key win mode
                                 &optional button-mask pgrab-mode kgrab-mode)
  "Grab or Ungrab KEY on WIN.
MODE is either 'grab or 'ungrab.
BUTTON-MASK is mask passed to `XGrabButton' if MODE is 'grab and key
is actually a mouse key."
  (xwem-keyboard-init)                  ; make sure keyboard initialised

  (let* ((key (aref (key-sequence-list-description key) 0))
         (kmods (butlast key))
         (ksyko (xwem-kbd-emacs->xksym (car (last key))))
         mouse)

    (if (listp ksyko)
        (setq mouse (car ksyko))

      (setq ksyko (xwem-kbd-xksym->xkcode ksyko))
      (when (eq (cadr ksyko) 'shift)
        (setq kmods (cons 'shift kmods)))
      (setq ksyko (car ksyko)))

    (setq kmods (xwem-kbd-emods->xmodmask kmods))

    ;; Take into account evil masks
    (mapc #'(lambda (m1)
              (mapc #'(lambda (m2)
                        (if mouse
                            (if (eq mode 'grab)
                                (XGrabButton
                                 (xwem-dpy) mouse (Xmask-or kmods m1 m2) win
                                 (or button-mask (Xmask-or XM-ButtonPress
                                                           XM-ButtonRelease))
                                 nil t pgrab-mode kgrab-mode)
                              (XUngrabButton
                               (xwem-dpy) mouse (Xmask-or kmods m1 m2) win))

                          (if (eq mode 'grab)
                              (XGrabKey
                               (xwem-dpy) ksyko (Xmask-or kmods m1 m2) win
                               t pgrab-mode kgrab-mode)
                            (XUngrabKey
                             (xwem-dpy) ksyko (Xmask-or kmods m1 m2) win))))
                    xwem-kbd-evilmasks))
          xwem-kbd-evilmasks)))

(defun xwem-kbd-grab-key (key win &optional pgrab kgrab)
  "Grab KEY on WIN."
  (xwem-kbd-graugra-key key win 'grab nil pgrab kgrab))

(defun xwem-kbd-ungrab-key (key win)
  "Stop grabbing KEY on WIN."
  (xwem-kbd-graugra-key key win 'ungrab))

;;;###xwem-autoload
(defun xwem-kbd-install-grab (keymap win &optional pgrab kgrab)
  "Install KEYMAP grabs on X window WIN."
  (let ((fkeymap (xwem-kbd-fixup-keymap keymap)))
    (flet ((xsafe-get (o p) (ignore-errors (get o p))))
      (map-keymap #'(lambda (key bind)
                      (unless (xsafe-get bind 'xwem-no-grab)
                        (xwem-kbd-grab-key key win (xsafe-get bind 'pgrab-mode)
                                           (xsafe-get bind 'kgrab-mode))))
                  fkeymap))

    ;; Also grab KEYMAP's parents
    (mapc #'(lambda (pkeymap)
              (xwem-kbd-install-grab pkeymap win pgrab kgrab))
          (keymap-parents fkeymap))))

;;;###xwem-autoload
(defun xwem-kbd-uninstall-grab (keymap win &optional predict)
  "Uninstall KEYMAP grabs on X window WIN.

Optionally you can specify PREDICT to decide for which keys grabbing
should be uinstalled.  PREDICT must accept two arguments - KEY and
BINDING, and return non-nil if this KEY must be ungrabbed.

By default all keys are ungrabbed."
  (map-keymap #'(lambda (key bind)
                  (when (or (not predict)
                            (funcall predict key bind))
                    (xwem-kbd-ungrab-key key win)))
              (xwem-kbd-fixup-keymap keymap))

  ;; Also ungrab KEYMAP's parents
  (mapc #'(lambda (pkeymap)
            (xwem-kbd-uninstall-grab pkeymap win predict))
        (keymap-parents (xwem-kbd-fixup-keymap keymap))))

(defun xwem-kbd-apply-grabbing ()
  "Start/stop grabbing according to `xwem-kbd-now-grabbing'."
  (if xwem-kbd-now-grabbing
      (unless (xwem-keymacro-executing-p)
        (XGrabKeyboard (xwem-dpy) (or (xwem-cl-xwin (xwem-dummy-client))
                                      (xwem-rootwin)))
        (xwem-mouse-grab xwem-kbd-now-grabbing))
    (unless (xwem-keymacro-executing-p)
      (XAllowEvents (xwem-dpy) X-AsyncBoth)
      (XUngrabKeyboard (xwem-dpy))
      (xwem-mouse-ungrab))))

;;;###xwem-autoload
(defun xwem-kbd-start-grabbing (&optional cursor)
  "Begin grabbing keyboard (some key-prefix is entered).
Optionally you can specify CURSOR to be used, while grabbing."
  (unless xwem-kbd-now-grabbing
    (setq xwem-kbd-now-grabbing (or cursor xwem-cursor-wait))
    (xwem-kbd-apply-grabbing)))

;;;###xwem-autoload
(defun xwem-kbd-stop-grabbing ()
  "Stop grabbing keyboard."
  (when xwem-kbd-now-grabbing
    (setq xwem-kbd-now-grabbing nil)
    (xwem-kbd-apply-grabbing)))

;;;###autoload(autoload 'xwem-kbd-quote-command "xwem-keyboard" "" t)
(define-xwem-command xwem-kbd-quote-command ()
  "Pass event EV to currently active window.
DOES NOT WORK."
  (xwem-interactive "_")

  (let ((xwin (xwem-focus-xcurrent))
        xev)
    (xwem-message 'debug "xwin = %S" xwin)
    (when (X-Win-p xwin)
      (XGrabKeyboard (xwem-dpy) xwin nil X-GrabModeSync X-GrabModeSync)
      (XGrabPointer (xwem-dpy) xwin (Xmask-or XM-ButtonPress XM-ButtonRelease)
                    xwem-cursor-quote nil X-GrabModeSync X-GrabModeSync)
      (XAllowEvents (xwem-dpy) X-SyncBoth
                    (X-Event-xkey-time xwem-last-xevent))

      (xwem-message 'prompt "[Quote key]")
      (xwem-unwind-protect
          (while (and (setq xev (xwem-next-event
                                 nil (list X-KeyPress X-KeyRelease
                                           X-ButtonPress X-ButtonRelease)))
                      (not (xwem-xevents->emacs-events (list xev) t) ))
            (xwem-message 'debug "EVENT!!!! xev=%S" xev)
            (XAllowEvents (xwem-dpy) X-SyncBoth (X-Event-xkey-time xev)))
        (xwem-message 'debug "HERE!!!! xev=%S" xev)
        (XAllowEvents (xwem-dpy) X-ReplayKeyboard)
        (XAllowEvents (xwem-dpy) X-ReplayPointer)
        (XUngrabKeyboard (xwem-dpy))
        (XUngrabPointer (xwem-dpy))
        (xwem-clear-message)))))

;;}}}

;;{{{ [-] Initializators

(defun xwem-kbd-filter-keycodes (keycodes)
  "Filter valid keycodes from KEYCODES list."
  (delq nil (mapcar #'(lambda (kc)
                        (and (>= kc (X-Dpy-min-keycode (xwem-dpy)))
                             (<= kc (X-Dpy-max-keycode (xwem-dpy)))
                             kc))
		    keycodes)))

;;;###xwem-autoload
(defun xwem-kbd-initialize-modifiers ()
  "Create internal modifier representation to speedup futher work.
Also update `xwem-kbd-evilmasks' if `xwem-kbd-evillocks' is non-nil."
  (setq xwem-xmods-mapping
        (car (last (XGetModifierMapping (xwem-dpy)))))

  (let* ((alts (list (car (xwem-kbd-xksym->xkcode XK-Alt-L))
                     (car (xwem-kbd-xksym->xkcode XK-Alt-R))))
         (metas (list (car (xwem-kbd-xksym->xkcode XK-Meta-L))
                      (car (xwem-kbd-xksym->xkcode XK-Meta-R))))
         (hypers (list (car (xwem-kbd-xksym->xkcode XK-Hyper-L))
                       (car (xwem-kbd-xksym->xkcode XK-Hyper-R))))
         (supers (list (car (xwem-kbd-xksym->xkcode XK-Super-L))
                       (car (xwem-kbd-xksym->xkcode XK-Super-R))))
         (numlocks (list (car (xwem-kbd-xksym->xkcode XK-Num-Lock))))
         (evils (mapcar #'(lambda (ks)
                            (car (xwem-kbd-xksym->xkcode ks)))
                        xwem-kbd-evillocks))
         (mlist (list X-Mod1 X-Mod2 X-Mod3 X-Mod4 X-Mod5))
         (slist (nthcdr 3 xwem-xmods-mapping)))

    ;; Clear modifiers info
    (mapc #'(lambda (mod-sym)
              (put mod-sym 'x-key-codes nil)
              (put mod-sym 'x-mod-mask nil))
          '(shift lock control alt meta hyper super numlock))

    ;; Shift
    (put 'shift 'x-key-codes
	 (xwem-kbd-filter-keycodes (nth 0 xwem-xmods-mapping)))
    (put 'shift 'x-mod-mask X-Shift)
    ;; Lock
    (put 'lock 'x-key-codes
	 (xwem-kbd-filter-keycodes (nth 1 xwem-xmods-mapping)))
    (put 'lock 'x-mod-mask X-Lock)
    ;; Control
    (put 'control 'x-key-codes
	 (xwem-kbd-filter-keycodes (nth 2 xwem-xmods-mapping)))
    (put 'control 'x-mod-mask X-Control)

    (while slist
      ;; Update some private modifier mask
      (mapc #'(lambda (mods mod-sym)
                (let ((ism (xwem-kbd-filter-keycodes
                            (intersection (car slist) mods))))
                  (when ism
                    (put mod-sym 'x-key-codes
                         (nconc ism (get mod-sym 'x-key-codes)))
                    (put mod-sym 'x-mod-mask
                         (Xmask-or (car mlist)
                                   (or (get mod-sym 'x-mod-mask) 0))))))
            (list alts metas hypers supers numlocks)
            (list 'alt 'meta 'hyper 'super 'numlock))

      ;; Update Evil locks
      (when (intersection (car slist) evils)
        (push (car mlist) xwem-kbd-evilmasks))

      (setq slist (cdr slist)
            mlist (cdr mlist)))

    ;; Hack over Alt-Meta problem
    (when (eql (get 'alt 'x-mod-mask) (get 'meta 'x-mod-mask))
      (put 'alt 'x-mod-mask 0))))

(defun xwem-keyboard-init ()
  "Initialize xwem keyboard.
Fetches KeyboardMapping from the X server and stores it in
`xwem-xkeys-mapping'"
  (unless (get 'xwem-keyboard 'initialized)
    (xwem-message 'init "Initializing keyboard ...")

    (xwem-kbd-set-current-prefix-keymap nil)

    ;; Hmm FSFmacs issued "invalid instraction" in `XGetKeyboardMapping'
    (setq xwem-xkeys-mapping
          (XGetKeyboardMapping (xwem-dpy)
                               (X-Dpy-min-keycode (xwem-dpy))
                               (- (X-Dpy-max-keycode (xwem-dpy))
                                  (X-Dpy-min-keycode (xwem-dpy)))))

    ;; Initialize modifiers
    (xwem-kbd-initialize-modifiers)

    ;; Some messaging configuration
    (add-to-list 'xwem-messages-ignore-log-labels 'keys)
    (add-to-list 'xwem-messages-ignore-log-labels 'keys-continuator)

    (run-hooks 'xwem-keyboard-init-hook)
    (put 'xwem-keyboard 'initialized t)
    (xwem-message 'init "Initializing keyboard ... done")))

;;}}}

;;{{{ [-] Universal argument

;;;###autoload
(defvar xwem-prefix-arg nil
"The value of the prefix argument for this editing command.

It may be a number, or the symbol `-' for just a minus sign as arg, or
a list whose car is a number for just one or more
\\<xwem-global-map>\\[xwem-universal-argument] or `nil' if no argument
has been specified.  This is what `\\(xwem-interactive \"P\"\\)'
returns.")

(defconst xwem-universal-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-default-binding map 'xwem-universal-command)
    (define-key map xwem-universal-key 'xwem-universal-more)
    (define-key map [?-] 'xwem-universal-minus)
    (define-key map [?0] 'xwem-universal-digit)
    (define-key map [?1] 'xwem-universal-digit)
    (define-key map [?2] 'xwem-universal-digit)
    (define-key map [?3] 'xwem-universal-digit)
    (define-key map [?4] 'xwem-universal-digit)
    (define-key map [?5] 'xwem-universal-digit)
    (define-key map [?6] 'xwem-universal-digit)
    (define-key map [?7] 'xwem-universal-digit)
    (define-key map [?8] 'xwem-universal-digit)
    (define-key map [?9] 'xwem-universal-digit)
    map)
  "Keymap used while processing \\<xwem-global-map>\\[xwem-universal-argument].")

(defvar xwem-universal-argument-num-events nil
  "Number of argument-specifying events read by
`xwem-universal-argument'.")

(defun xwem-universal-common-begin ()
  "Common begin for universal argument."
  (setq xwem-universal-argument-num-events
        (length xwem-this-command-keys))
  (if xwem-kbd-now-grabbing
      (setq xwem-override-map xwem-universal-map)

    ;; Start grabbing
    (xwem-kbd-start-grabbing)
    (setq xwem-override-map xwem-universal-map)))

;;;###autoload(autoload 'xwem-universal-argument "xwem-keyboard" "" t)
(define-xwem-command xwem-universal-argument ()
  "Begin a numeric argument for the following command.
Digits or minus sign following
\\<xwem-global-map>\\[xwem-universal-argument] make up the numeric
argument.  \\<xwem-global-map>\\[xwem-universal-argument] following the
digits or minus sign ends the argument.
\\<xwem-global-map>\\[xwem-universal-argument] without digits or minus
sign provides 4 as argument.
Repeating \\<xwem-global-map>\\[xwem-universal-argument] without digits or
minus sign multiplies the argument by 4 each time."
  (xwem-interactive)

  (setq xwem-prefix-arg (list 4))
  (xwem-universal-common-begin))

;; A subsequent H-u means to multiply the factor by 4 if we've typed
;; nothing but H-u's otherwise it means to terminate the prefix arg.
;;;###autoload(autoload 'xwem-universal-more "xwem-keyboard" "" t)
(define-xwem-command xwem-universal-more (arg)
  "A subsequent \\<xwem-universal-map>\\[xwem-universal-more] means to
  multiply the factor by 4 if we've typed nothing but
  \\<xwem-universal-map>\\[xwem-universal-more]'s; otherwise it means
  to terminate the prefix arg."
  (xwem-interactive "P")

  (if (consp arg)
      (setq xwem-prefix-arg (list (* 4 (car arg))))

    (setq xwem-prefix-arg arg)
    (setq xwem-override-map nil))
  (setq xwem-universal-argument-num-events (length xwem-this-command-keys)))

;;;###autoload(autoload 'xwem-universal-minus "xwem-keyboard" "" t)
(define-xwem-command xwem-universal-minus (arg)
  "Begin a negative numeric argument for the next command.
\\<xwem-global-map>\\[xwem-universal-argument] following digits or
minus sign ends the argument."
  (xwem-interactive "P")

  (setq xwem-prefix-arg (cond ((integerp arg) (- arg))
                              ((eq arg '-) nil)
                              (t '-)))
  (xwem-universal-common-begin))

;;;###autoload(autoload 'xwem-universal-digit "xwem-keyboard" "" t)
(define-xwem-command xwem-universal-digit (arg)
  "Part of the numeric argument for the next command.
\\<xwem-global-map>\\[xwem-universal-argument] following digits or
minus sign ends the argument."
  (xwem-interactive "P")

  (let* ((num (or (and (key-press-event-p xwem-last-event)
                       (event-key xwem-last-event))
                  (and (listp xwem-last-event)
                       (car (last xwem-last-event)))))
         (digit (- num ?0)))
    (cond ((integerp arg)
           (setq xwem-prefix-arg (+ (* arg 10) (if (< arg 0) (-  digit) digit))))
          ((eq arg '-)
           ;; Treat -0 as just -, so that -01 will work.
           (setq xwem-prefix-arg (if (zerop digit) '- (- digit))))

          (t (setq xwem-prefix-arg digit)))

    (xwem-universal-common-begin)))

;;;###autoload(autoload 'xwem-universal-command "xwem-keyboard" "" t)
(define-xwem-command xwem-universal-command (arg)
  "Handle universal argument functionality."
  (xwem-interactive "P")

  (setq xwem-prefix-arg arg)
  (setq xwem-override-map nil)

  ;; Reprocess last event
  ;; XXX Avoid double keys echoing
  (setq xwem-kbd-scheduled-keys
        (vconcat (butlast (append xwem-kbd-scheduled-keys nil))))
  (xwem-dispatch-command-event xwem-last-event xwem-last-xevent))

;;}}}


(provide 'xwem-keyboard)

;;;; On-load actions:
(if xwem-started
    (xwem-keyboard-init)
  (add-hook 'xwem-before-init-wins-hook 'xwem-keyboard-init))

;;; xwem-keyboard.el ends here
