;;; xwem-launcher.el --- Program launcher for XWEM.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Dec  4 16:32:11 MSK 2003
;; Keywords: xwem, xlib
;; Time-stamp: <1/12/2008 23:14:14 lg@h1>

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

;; External programs launcher for XWEM.

;;; TODO:
;;    * Assume argument in quotas as single argument, i.e. do not make
;;      splitting inside quotas.

;;; Code:

(eval-and-compile
  (require 'completer)
  (require 'comint)
  (autoload 'executable-find "executable")
  )

(require 'xlib-xlib)
(require 'xlib-tray)
(require 'xlib-xpm)

(require 'xwem-load)

(defgroup xwem-launcher nil
  "Group to customize XWEM's programs launcher."
  :prefix "xwem-"
  :group 'xwem)

(defcustom xwem-launcher-beep-done nil
  "*Non-nil mean beep when execution of program done.
Beep performed using \(xwem-play-sound 'ready\)."
  :type 'boolean
  :group 'xwem-launcher)

(defcustom xwem-launcher-function 'xwem-execute-program-expecting
  "Function to be used to execute external program.
If you are having problems with
default\(`xwem-execute-program-expecting'\) value, set it to
`xwem-execute-program'."
  :type '(choice (function :tag "Expecting clients (default)"
                           xwem-execute-program-expecting)
                 (function :tag "Non-expecting clients"
                           xwem-execute-program)
                 (function :tag "User defined launcher"))
  :group 'xwem-launcher)

(defcustom xwem-launcher-use-nohup nil
  "*When non-nil launch apps with nohup\(1\).
Consider setting `xwem-launcher-forced-pwd' if you set this to get rid
of many nohup.out files in different places."
  :type 'boolean
  :group 'xwem-launcher)

(defcustom xwem-launcher-forced-pwd nil
  "*The $PWD to use when launching apps.
By default `xwem-launcher-forced-pwd' is nil, means that $PWD is
current value of `default-directory'."
  :type '(choice (const :tag "No" nil)
                 (directory :tag "Custom directory" :must-match t))
  :group 'xwem-launcher)

(defcustom xwem-launcher-abbrev-table nil
  "Abbrev table used by `xwem-launcher-query'."
  :type 'list
  :group 'xwem-launcher)

(defcustom xwem-launcher-history nil
  "History of `xwem-launcher-query'ies."
  :type 'list
  :group 'xwem-launcher)

(defcustom xwem-launcher-split-type 'xwem-execute-program-other-win-vertical
  "Window split type, when launching program in other window."
  :type '(choice
          (const :tag "Horizontal" xwem-execute-program-other-win-horizontal)
          (const :tag "Vertical" xwem-execute-program-other-win-vertical))
  :group 'xwem-launcher)

(defcustom xwem-launcher-frame-type 'xwem-execute-program-other-frame
  "Type of frame, when launching program in other frame."
  :type '(choice
          (const :tag "Normal frame" xwem-execute-program-other-frame)
          (const :tag "Embedded frame" xwem-execute-program-embedded-frame))
  :group 'xwem-launcher)

(defcustom xwem-xterm-program "xterm"
  "Name of terminal emulator program."
  :type 'string
  :group 'xwem-launcher)

(defcustom xwem-xterm-font-argument "-fn"
  "Program key to specify in order to change font."
  :type 'string
  :group 'xwem-launcher)

(defcustom xwem-xterm-font1 ""
  "Default xterm font."
  :type 'string
  :group 'xwem-launcher)

(defcustom xwem-xterm-font2 "10x20"
  "Second xterm font."
  :type 'string
  :group 'xwem-launcher)

(defcustom xwem-xterm-font3 "9x15"
  "Third xterm font."
  :type 'string
  :group 'xwem-launcher)

(defcustom xwem-xterm-font4 "fixed"
  "Forth xterm font."
  :type 'string
  :group 'xwem-launcher)

(defcustom xwem-lupe-program "lupe"
  "Xmag like mignifier program."
  :type 'string
  :group 'xwem-launcher)

(defcustom xwem-lupe-arguments "-noshape -nohud"
  "Argument to pass to `xwem-lupe-program'."
  :type 'string
  :group 'xwem-launcher)

(defcustom xwem-xlock-program "xlock"
  "Program to run in order to lock X display."
  :type 'string
  :group 'xwem-launcher)

(defcustom xwem-xlock-arguments "-mode image"
  "Arguments to supply `xwem-xlock-program'."
  :type 'string
  :group 'xwem-launcher)

;;; Launcher dockapp button
(defcustom xwem-launch-dock-width 24
  "*Launcher dockapp width."
  :type 'number
  :group 'xwem-launcher)

(defcustom xwem-launch-dock-height 24
  "*Launcher dockapp height."
  :type 'number
  :group 'xwem-launcher)

(defcustom xwem-launch-dock-thick 2
  "*Launcher dockapp border thickness."
  :type 'number
  :group 'xwem-launcher)

;;; Internal variables

(defvar xwem-launcher-shell-completion-table nil
  "*Completion table in used by `xwem-completer'.")

;; Executing program
(defun xwem-launcher-build-shell-completion-table (&optional rehash)
  "Scan `exec-path' to build completion table of executables."
  (if (and xwem-launcher-shell-completion-table (not rehash))
      xwem-launcher-shell-completion-table

    (setq xwem-launcher-shell-completion-table
          (apply 'nconc
                 (mapcar #'(lambda (dir)
                             (mapcar #'list (directory-files dir nil nil t t)))
                         exec-path)))))

(defun xwem-launcher-shell-complete (&optional undo mode)
  "Complete the previous command or display possibilities if done
twice in a row.  If called with a prefix, undo the last completion."
  (interactive "P")
  (if undo
      (completer-undo)

    (setq completer-string nil)
    (if (string-match ".*[ \t].*" (buffer-substring (point-min) (point-max)))
        (comint-dynamic-complete-filename)

      ;; Complete application
      (completer-complete-goto "^ \t\n\"" completer-words
                               (xwem-launcher-build-shell-completion-table)
                               #'(lambda (w)
                                   (executable-find (car w)))))))

(defun xwem-launcher-read-command (prompt &optional initial-input)
  "Read command name prompting PROMPT.
Just linke `read-shell-command', but installs
`xwem-launcher-abbrev-table' as abbrev table, `xwem-launcher-history'
as history of commands and `xwem-launcher-shell-completion-table' as table
to performe completions.
INITIAL-INPUT is self-described."
  (let ((minibuffer-completion-table nil)
        (keymap (copy-keymap read-shell-command-map)))
    (xwem-unwind-protect
        (progn
          (define-key read-shell-command-map
            (kbd "TAB") 'xwem-launcher-shell-complete)
          (xwem-read-from-minibuffer
           prompt initial-input read-shell-command-map
           nil 'xwem-launcher-history xwem-launcher-abbrev-table))
      (setq read-shell-command-map keymap))))

(defun xwem-launcher-parse-arguments (cmd-str &optional keep-quotes-p)
  "Parse CMD-STR by spliting it to arguments list.
If optional KEEP-QUOTES-P is non-nil, quotes in quoted arguments are kept."
  (let ((scmd (split-string cmd-str " "))
        qarg args cmd)
    (while scmd
      (if (string-match "^['\"]" (car scmd))
          (setq qarg (car scmd))
        (when (stringp qarg)
          (setq qarg (concat qarg " " (car scmd)))))
      (if (stringp qarg)
          (when (string-match "['\"]$" (car scmd))
            (setq args (cons (if keep-quotes-p
                                 qarg
                               (substring qarg 1 (1- (length qarg))))
                             args)
                  qarg nil))
        (setq args (cons (car scmd) args)))
      (setq scmd (cdr scmd)))
    ;; Pre-normalise command and its arguments
    (setq args (remove "" (nreverse args))
          cmd (executable-find (car args)))
    (when cmd
      (cons cmd (cdr args)))))

(defun xwem-launcher-normalize-cmd (cmd)
  "Normalize command CMD string.
Return normalized command string, or signal error if CMD can't be
normalized."
  (let ((pargs (xwem-launcher-parse-arguments cmd t)))
    (unless pargs
      (error 'xwem-error (format "Can't normalize command: %S" cmd)))
    (mapconcat #'identity pargs " ")))

;;;###xwem-autoload
(defun xwem-launcher-query (&optional prompt)
  "Query for command to launch using PROMPT."
  (let ((cmd (xwem-launcher-read-command (or prompt "XWEM-Run: "))))
    (xwem-launcher-normalize-cmd cmd)))

;;;###autoload(autoload 'xwem-run-program "xwem-launcher" "" t)
(define-xwem-command xwem-run-program (command)
  "Run COMMAND."
  (xwem-interactive "_eRun command: ")
  (xwem-execute-program command))

(defconst xwem-proc-name-format "xwem-job-%d")

(defun xwem-next-job-number ()
  "Return next job number for use by xwem."
  (let ((job-number 1))
    (while (get-process (format xwem-proc-name-format job-number))
      (setq job-number (1+ job-number)))
    job-number))

(defun xwem-program-sentinel (process msg)
  "Called when PROCESS changed state to MSG."
  (let ((ms (match-data))) ; barf
    (unwind-protect
         (let ((msg (cond ((string= msg "finished\n") "Done")
                          ((string-match "^exited" msg)
                           (concat "Exit " (substring msg 28 -1)))
                          ((zerop (length msg)) "Continuing")
                          (t (concat (upcase (substring msg 0 1))
                                     (substring msg 1 -1))))))
           (when xwem-launcher-beep-done
             (xwem-play-sound 'ready))
           (xwem-message 'note "Job [%s] %s '%s'" (process-name process)
                         msg (mapconcat #'identity
                                        (process-command process) " "))
           (if (null (buffer-name (process-buffer process)))
               (set-process-buffer process nil) ; WHY? Olin.

             (if (memq (process-status process) '(signal exit))
                 (with-current-buffer (process-buffer process)
                   (let ((at-end (eobp)))
                     (save-excursion
                       (goto-char (point-max))
                       (insert ?\n msg ?\x20
                               (substring (current-time-string) 11 19) ?\n))
                     (if at-end (goto-char (point-max))))
                   (set-buffer-modified-p nil)))))
      (store-match-data ms))))

;;;###autoload
(defun xwem-execute-program (command)
  "Execute COMMAND with output directed some buffer.
Much like `background', but do not use shell.
This function can be used as `xwem-launcher-function'."
  (let* ((cmdargs
          ;; Do it under `condition-case', due to split-string
          ;; args-out-of-range bug.
          (condition-case nil
              (xwem-launcher-parse-arguments command)
            (t (list command))))
         (prg (if xwem-launcher-use-nohup
		  "nohup"
		(car cmdargs)))
         (args (if xwem-launcher-use-nohup
		   cmdargs
		 (cdr cmdargs)))
         (emacs-env (getenv "EMACS"))
         (job-number (xwem-next-job-number))
         (job-name (format xwem-proc-name-format job-number))
         (buffer-name (format " *%s*" job-name))
         proc)
    (with-current-buffer (get-buffer-create buffer-name)
      (if xwem-launcher-forced-pwd
	  (setq default-directory
                (file-name-as-directory xwem-launcher-forced-pwd))
        ;; Sometimes `default-directory' became nil for some reason, I
        ;; don't know why.  Maybe XEmacs bug? --lg
	(unless default-directory
	  (setq default-directory
                (file-name-as-directory (user-home-directory)))))

      (erase-buffer)
      (insert (format "--- Working directory: %S\n%% %S\n"
                      default-directory command))

      ;; Set our EMACS environment variable so comint-exec doesn't do it
      ;; for us.  Note that if the environment is already set, we may
      ;; not want to do it again.
      (unless emacs-env
        (setenv "EMACS" "xwem"))

      (setq proc (get-buffer-process
                  (comint-exec buffer-name job-name prg nil args)))
      (process-kill-without-query proc)
      (comint-mode)
      ;; COND because the proc may have died before the G-B-P is called.
      (cond (proc (set-process-sentinel proc 'xwem-program-sentinel)
                  (xwem-message 'note "Job [%d] '%s' PID=%d"
                                job-number command (process-id proc))))
      (setq mode-name "XWEM-Job")

      ;; Restore our Emacs environment variable to its previous state.
      (setenv "EMACS" emacs-env)

      proc)))

;;;###xwem-autoload(autoload 'xwem-execute-program-expecting-1 "xwem-launcher" nil t)
(defun* xwem-execute-program-expecting-1
  (cmd &key manage-type cl-plist (qualifier '(t)) non-block)
  "Execute CMD expecting till client will be managed.
MANAGE-TYPE specifies which manage type to use in expectance.
CL-PLIST is properties for expected client.
Return managed client unless NON-BLOCK is non-nil."
  (setq cmd (xwem-launcher-normalize-cmd cmd))

  (let* ((job-num (xwem-next-job-number))
         (expt (list manage-type
                     (xwem-misc-merge-plists
                      ;; In case no expect-win is specified in
                      ;; CL-PLIST, set it to selected window
                      `(expect-win ,(xwem-win-id (xwem-win-selected)))
                      cl-plist
                      `(job-num ,job-num))
                     qualifier)))
    (xwem-manda-add-expectance expt)
    (xwem-execute-program cmd)
    (unless non-block
      ;; Wait till client gets managed
      (while (memq expt xwem-manage-expectances)
        (dispatch-event (next-event)))

      ;; Find a client and set its `executable-command' property
      ((lambda (cl)
         (when cl
           (xwem-client-set-property cl 'executed-command cmd))
         cl)
       (find job-num xwem-clients :test #'= :key
             #'(lambda (cl) (or (xwem-client-property cl 'job-num) -1)))))))

;;;###xwem-autoload
(defun xwem-execute-program-expecting (command)
  "Execute COMMAND expecting new client to manage.
This function can be used as `xwem-launcher-function'."
  (xwem-execute-program-expecting-1 command))

(defun xwem-execute-program-other-win (cmd type &optional select-p)
  "Execute CMD in other XWEM window, making TYPE split if needed.
TYPE is one of 'horizontal of 'vertical.
If SELECT-P is non-nil - select newly created client.
Return newly created client."
  (let ((own (xwem-window-other 1))
        (cl (xwem-execute-program-expecting-1 cmd :manage-type 'dummy)))
    (when (xwem-cl-alive-p cl)
      (when (xwem-win-only-one-p own)
        (let ((xwem-win-split-hook nil)) ; Omit split hooks
          (if (eq type 'horizontal)
              (xwem-window-split-horizontally 0 own)
            (xwem-window-split-vertically 0 own)))
        (setq own (xwem-win-next own)))
      ;; Manage CL in OWN window using generic mode
      (xwem-client-change-manage-type
       cl `(generic (expect-win ,(xwem-win-id own))))
      ;; And possible select client
      (when select-p
        (xwem-select-client cl)))
    cl))

(defun xwem-execute-program-other-win-horizontal (cmd &optional select-p)
  "Execute CMD in other XWEM window, making horizontal split if needed."
  (xwem-execute-program-other-win cmd 'horizontal select-p))

(defun xwem-execute-program-other-win-vertical (cmd &optional select-p)
  "Execute CMD in other XWEM window, making vertical split if needed."
  (xwem-execute-program-other-win cmd 'vertical select-p))

(defun xwem-execute-program-other-frame (cmd &optional select-p)
  "Execute CMD in other XWEM frame.
If SELECT-P is non-nil select newly managed client."
  (let* ((oframe (or (xwem-frame-other (xwem-frame-selected))
                     (xwem-make-frame-1 'desktop :noselect t)))
         (cl (xwem-execute-program-expecting-1
              cmd :manage-type nil
              :cl-plist `(expect-win ,(xwem-win-id (xwem-frame-selwin oframe))))))
    (when select-p
      (xwem-select-client cl))
    cl))

(defun xwem-execute-program-embedded-frame (cmd &optional select-p)
  "Execute CMD in linkaged frame or in embedded XWEM frame."
  (let* ((oframe (or (xwem-frame-other (xwem-frame-selected) 'linkage)
                     (xwem-make-frame-1 'embedded :noselect t)))
         (cl (xwem-execute-program-expecting-1
              cmd :manage-type nil
              :cl-plist `(expect-win ,(xwem-win-id (xwem-frame-selwin oframe))))))
    (when select-p
      (xwem-select-client cl))
    cl))

;;;; XWEM commands.

;;;###autoload(autoload 'xwem-launcher-turn-on-horizontal-split-type "xwem-launcher" "" t)
(define-xwem-command xwem-launcher-turn-on-horizontal-split-type ()
  "Set `xwem-launcher-split-type' to `xwem-execute-program-other-win-horizontal'."
  (xwem-interactive)

  (setq xwem-launcher-split-type 'xwem-execute-program-other-win-horizontal)
  (xwem-message 'info "Launcher split type HORIZONTAL on."))

;;;###autoload(autoload 'xwem-launcher-turn-on-vertical-split-type "xwem-launcher" "" t)
(define-xwem-command xwem-launcher-turn-on-vertical-split-type ()
  "Set `xwem-launcher-split-type' to `xwem-execute-program-other-win-vertical'."
  (xwem-interactive)

  (setq xwem-launcher-split-type 'xwem-execute-program-other-win-vertical)
  (xwem-message 'info "Launcher split type VERTICAL on."))

;;;###autoload(autoload 'xwem-launcher-toggle-split-type "xwem-launcher" "" t)
(define-xwem-command xwem-launcher-toggle-split-type (arg)
  "Toggle `xwem-launcher-split-type'.
Without prefix ARG set it to vertical, with prefix ARG set it to horizontal."
  (xwem-interactive "P")

  (if arg
      (xwem-launcher-turn-on-horizontal-split-type)
    (xwem-launcher-turn-on-vertical-split-type)))

;;;###autoload(autoload 'xwem-launcher-turn-on-normal-frame-type "xwem-launcher" "" t)
(define-xwem-command xwem-launcher-turn-on-normal-frame-type ()
  "Set `xwem-launcher-frame-type' to `xwem-execute-program-other-frame', aka normal frame type."
  (xwem-interactive)

  (setq xwem-launcher-frame-type 'xwem-execute-program-other-frame)
  (xwem-message 'info "Other frame NORMAL type on."))

;;;###autoload(autoload 'xwem-launcher-turn-on-embedded-frame-type "xwem-launcher" "" t)
(define-xwem-command xwem-launcher-turn-on-embedded-frame-type ()
  "Set `xwem-launcher-frame-type' to `xwem-execute-program-embedded-frame' aka embedded frame type."
  (xwem-interactive)

  (setq xwem-launcher-frame-type 'xwem-execute-program-embedded-frame)
  (xwem-message 'info "Other frame EMBEDDED type on."))

;;;###autoload(autoload 'xwem-launcher-toggle-frame-type "xwem-launcher" "" t)
(define-xwem-command xwem-launcher-toggle-frame-type (arg)
  "Toggle `xwem-launcher-frame-type'.
Without prefix ARG set it to normal, with prefix ARG set it to embedded."
  (xwem-interactive "P")

  (if arg
      (xwem-launcher-turn-on-normal-frame-type)
    (xwem-launcher-turn-on-embedded-frame-type)))

;;;###autoload(autoload 'xwem-launch-program "xwem-launcher" "" t)
(define-xwem-command xwem-launch-program (cmd &optional arg)
  "Run CMD program in background.
If used with prefix ARG (\\<xwem-global-map>\\[xwem-universal-argument]), installs
expectance on selected window (i.e. when client window performe
MapWindow, it will be managed to window where expectance setuped
regardless selected window at map moment.  It is usefull to do so when
you start application with large start time, such as Mozilla or
AcrobatReader, and you want continue doing things not wainting untill
application window maps.
If used with numeric prefix ARG, then that number specifies how many
programs to run."
  (xwem-interactive "_eLaunch program: \nP")

  (cond ((listp arg)
         (xwem-execute-program-expecting-1
          cmd :manage-type nil
          :cl-plist `(expect-win ,(xwem-win-id (xwem-win-selected)))))

        ((numberp arg)
         (while (> arg 0)
           (xwem-execute-program cmd)
           (setq arg (1- arg))))
        (t (error 'xwem-error "Invalid arg: %s" arg))))

;;;###autoload(autoload 'xwem-launch-program-other-win "xwem-launcher" "" t)
(define-xwem-command xwem-launch-program-other-win (cmd &optional arg)
  "Run program in other window.
If prefix ARG is specified - select newly created client.
Window spliting (if needed) is controled by `xwem-launcher-split-type'."
  (xwem-interactive "_eLaunch Other Win: \nP")

  (funcall xwem-launcher-split-type cmd arg))

;;;###autoload(autoload 'xwem-launch-program-other-frame "xwem-launcher" "" t)
(define-xwem-command xwem-launch-program-other-frame (cmd &optional arg)
  "Run programm in other frame.
If prefix ARG is specified - select newly created client."
  (xwem-interactive "_eLaunch Other Frame: \nP")

  (funcall xwem-launcher-frame-type cmd arg))

(defsubst xwem-launch (cmd)
  "Execute CMD using `xwem-launcher'."
  (funcall xwem-launcher-function cmd))

;;;###autoload
(defun xwem-launch-generic-program (cmd sarg)
  "Run generic program CMD with arguments SARG."
  (xwem-launch (mapconcat #'identity (list cmd sarg) " ")))

;; Executing xterm
(defun xwem-xterm-construct-cmd (arg)
  "Construct xterm command according to ARG."
  (let ((fn (symbol-value
             (intern-soft (concat "xwem-xterm-font" (int-to-string arg)))))
        (cmd xwem-xterm-program))

    (when (and (stringp fn) (> (length fn) 0))
      (setq cmd (concat cmd " " xwem-xterm-font-argument " " fn)))
    cmd))

;;;###autoload(autoload 'xwem-launch-xterm "xwem-launcher" "" t)
(define-xwem-command xwem-launch-xterm (arg)
  "Run xterm program `xwem-xterm-program' with ARG as font argument."
  (xwem-interactive "_p")

  (let ((cmd (xwem-xterm-construct-cmd arg)))
    (xwem-launch cmd)))

;;;###autoload(autoload 'xwem-launch-xterm-other-win "xwem-launcher" "" t)
(define-xwem-command xwem-launch-xterm-other-win (arg)
  "Execute xterm in other window, ARG have same meaning as in `xwem-launch-xterm'."
  (xwem-interactive "_p")

  (let ((xwem-launcher-function xwem-launcher-split-type))
    (xwem-launch-xterm arg)))

;;;###autoload(autoload 'xwem-launch-xterm-other-frame "xwem-launcher" "" t)
(define-xwem-command xwem-launch-xterm-other-frame (arg)
  "Execute xterm in other frame, ARG have same meaning as in `xwem-launch-xterm'."
  (xwem-interactive "_p")

  (let ((xwem-launcher-function xwem-launcher-frame-type))
    (xwem-launch-xterm arg)))

;; Executing lupe
;;;###autoload(autoload 'xwem-launch-lupe "xwem-launcher" "" t)
(define-xwem-command xwem-launch-lupe (arg)
  "Run lupe `xwem-lupe-program' with `xwem-lupe-arguments'.
Prefix ARG is actually unused."
  (xwem-interactive "_P")

  (xwem-launch-generic-program xwem-lupe-program xwem-lupe-arguments))

;;;###autoload(autoload 'xwem-launch-lupe-other-win "xwem-launcher" "" t)
(define-xwem-command xwem-launch-lupe-other-win (arg)
  "Run lupe in other window.
Prefix ARG is actually unused."
  (xwem-interactive "_P")

  (let ((xwem-launcher-function xwem-launcher-split-type))
    (xwem-launch-lupe arg)))

;;;###autoload(autoload 'xwem-launch-lupe-other-frame "xwem-launcher" "" t)
(define-xwem-command xwem-launch-lupe-other-frame (arg)
  "Run lupe in other frame.
Prefix ARG is actually unused."
  (xwem-interactive "_P")

  (let ((xwem-launcher-function xwem-launcher-frame-type))
    (xwem-launch-lupe arg)))

;;;###autoload(autoload 'xwem-launch-xlock "xwem-launcher" "" t)
(define-xwem-command xwem-launch-xlock ()
  "Launch `xwem-xlock-program' with `xwem-xlock-arguments'."
  (xwem-interactive "_")

  (xwem-launch-generic-program xwem-xlock-program xwem-xlock-arguments))

;;;; Launcher dockapp
(define-xwem-face xwem-launch-dock-face
  `(((medium) (:foreground "gray70"))
    ((light) (:foreground "white"))
    ((dark) (:foreground "black"))
    (t (:foreground "gray70" :background "black")))
  "Default background face for launcher docks."
  :group 'xwem-launcher
  :group 'xwem-faces)

(defvar xwem-launch-dock-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [button1] 'xwem-launch-dock-down)
    (define-key map [button1up] 'xwem-launch-dock-launch)
    (define-key map [button3] 'xwem-launch-dock-menu)
    map)
  "Keymap for launch docks.")

;; Macroses to access ladock internals
(defmacro xwem-ladock-state (win)
  `(X-Win-get-prop ,win 'ladock-state))
(defsetf xwem-ladock-state (win) (state)
  `(X-Win-put-prop ,win 'ladock-state ,state))
(defmacro xwem-ladock-action (win)
  `(X-Win-get-prop ,win 'ladock-action))
(defsetf xwem-ladock-action (win) (action)
  `(X-Win-put-prop ,win 'ladock-action ,action))
(defmacro xwem-ladock-pixmap (win)
  `(X-Win-get-prop ,win 'ladock-pixmap))
(defsetf xwem-ladock-pixmap (win) (action)
  `(X-Win-put-prop ,win 'ladock-pixmap ,action))

;;;###autoload(autoload 'xwem-launch-dock-down "xwem-launcher" "" t)
(define-xwem-command xwem-launch-dock-down (ev)
  "Default command when button is down."
  (xwem-interactive (list xwem-last-event))
  (unless (button-event-p ev)
    (error 'xwem-error "`xwem-launch-dock-down' must be bound to mouse event"))

  ;; Push the button
  (setf (xwem-ladock-state (X-Event-win xwem-last-xevent)) 'down)
  (xwem-ladock-redraw (X-Event-win xwem-last-xevent)))

;;;###autoload(autoload 'xwem-launch-dock-launch "xwem-launcher" "" t)
(define-xwem-command xwem-launch-dock-launch (ev)
  "Launch command for launch dock."
  (xwem-interactive (list xwem-last-event))
  (unless (button-event-p ev)
    (error 'xwem-error "`xwem-launch-dock-up' must be bound to mouse event"))

  ;; Pop the button
  (setf (xwem-ladock-state (X-Event-win xwem-last-xevent)) 'up)
  (xwem-ladock-redraw (X-Event-win xwem-last-xevent))

  (when (and (< (X-Event-xbutton-event-x xwem-last-xevent)
                xwem-launch-dock-width)
             (< (X-Event-xbutton-event-y xwem-last-xevent)
                xwem-launch-dock-height))
    ;; React on ButtonRelease only if it released within ladock
    ;; window.
    (let* ((action (xwem-ladock-action (X-Event-win xwem-last-xevent)))
           (atype (car action))
           (acmd (cdr action)))
      (ecase atype
        (elisp (eval (car (read-from-string acmd))))
        (cmd (xwem-execute-program acmd))))))

;;;###autoload(autoload 'xwem-launch-dock-menu "xwem-launcher" "" t)
(define-xwem-command xwem-launch-dock-menu (action)
  "Popup menu."
  (xwem-interactive (list (xwem-ladock-action (X-Event-win xwem-last-xevent))))

  (let ((cls (delq nil (mapcar #'(lambda (cl)
                                   (and (string-match (cdr action)
                                                      (xwem-cl-wm-command cl))
                                        cl))
                               xwem-clients)))
        menu)

    ;; Sort clients
    (setq cls (sort cls #'(lambda (cl1 cl2)
                            (and (xwem-frame-p (xwem-cl-frame cl1))
                                 (xwem-frame-p (xwem-cl-frame cl2))
                                 (> (xwem-frame-num (xwem-cl-frame cl1))
                                    (xwem-frame-num (xwem-cl-frame cl2)))))))

    (setq menu
          (list "Clients" :filter
                #'(lambda (not-used)
                    (nconc
                     (mapcar #'(lambda (cl)
                                 (let ((frame (xwem-cl-frame cl))
                                       (name (xwem-cl-wm-name cl)))
                                   (vector
                                    (if (xwem-frame-p frame)
                                        (format "[%d](%s): %s"
                                                (xwem-frame-num (xwem-cl-frame cl))
                                                (xwem-frame-name (xwem-cl-frame cl))
                                                name)
                                      name)
                                    `(xwem-cl-pop-to-client ,cl)
                                    :active (xwem-non-dummy-client-p cl))))
                             cls)
                     (list
                      "---"
                      (vector "Destroy"
                              `(xwem-launch-button-stop
                                ,(X-Event-win xwem-last-xevent) t)))))))
    (xwem-popup-menu menu)))

(define-xwem-deferred xwem-ladock-redraw (xwin)
  "Redraw launcher button dock XWIN."
  (xwem-misc-draw-shadow
   (X-Win-dpy xwin) (xwem-ladock-pixmap xwin)
   (xwem-face-get-gc 'xwem-launch-dock-face
     (if (eq (xwem-ladock-state xwin) 'down) '(dark) '(light)))
   (xwem-face-get-gc 'xwem-launch-dock-face
     (if (eq (xwem-ladock-state xwin) 'down) '(light) '(dark)))
   0 0 xwem-launch-dock-width xwem-launch-dock-height xwem-launch-dock-thick)

  (XCopyArea (xwem-dpy) (xwem-ladock-pixmap xwin) xwin
             (XDefaultGC (xwem-dpy)) 0 0
             xwem-launch-dock-width xwem-launch-dock-height 0 0))

(defun xwem-ladock-evhandler (xdpy xwin xev)
  "On XDPY and launcher dock XWIN handle event XEV."
  (X-Event-CASE xev
    ((:X-Expose :X-MapNotify)
     (xwem-ladock-redraw xwin))

    ((:X-ButtonPress :X-ButtonRelease)
     (xwem-overriding-local-map xwem-launch-dock-keymap
       (xwem-dispatch-command-xevent xev)))

    (:X-DestroyNotify
     (xwem-launch-button-stop
      (X-Event-xdestroywindow-window xev)))))

(defun xwem-launch-button-stop (xwin &optional force)
  "Destoy launch dockapp button XWIN.
If FORCE is non-nil also destroy XWIN."
  (XFreePixmap (X-Win-dpy xwin) (xwem-ladock-pixmap xwin))

  (setf (xwem-ladock-pixmap xwin) nil
        (xwem-ladock-state xwin)nil
        (xwem-ladock-action xwin)nil)

  ;; Remove events handler
  (X-Win-EventHandler-rem xwin 'xwem-ladock-evhandler)

  (when force
    (XDestroyWindow (X-Win-dpy xwin) xwin)))

;;;###autoload
(defun xwem-launch-button-start (xpm-file action &optional dockid
                                          dockgroup dockalign)
  "Create new dockapp button with XPM-FILE image and doing ACTION on click.
ACTION is cons cell wher car is one of 'elisp or 'cmd and cdr is string.
For 'elisp car, cdr is real elisp expression, to evaluate on click.
For 'cmd  car, cdr is cmd to run on click."
  (let ((xwin (XCreateWindow (xwem-dpy) nil 0 0
                             xwem-launch-dock-width
                             xwem-launch-dock-height
                             0 nil nil nil
                             :event-mask (Xmask-or XM-Exposure XM-StructureNotify
                                                   XM-ButtonPress XM-ButtonRelease)
                             :override-redirect t)))

    ;; Create pixmap
    (setf (xwem-ladock-pixmap xwin)
          (XCreatePixmap (xwem-dpy) xwin (XDefaultDepth (xwem-dpy))
                         xwem-launch-dock-width xwem-launch-dock-height)
          (xwem-ladock-state xwin) 'up
          (xwem-ladock-action xwin) action)

    ;; Initial pixmaps setup
    (XFillRectangle (xwem-dpy) (xwem-ladock-pixmap xwin)
                    (xwem-face-get-gc 'xwem-launch-dock-face '(medium)) 0 0
                    (X-Pixmap-width (xwem-ladock-pixmap xwin))
                    (X-Pixmap-height (xwem-ladock-pixmap xwin)))
    (let* ((fname (xwem-icon-find-file xpm-file))
           (ipix (X:xpm-pixmap-from-file (xwem-dpy) xwin fname))
           (imask (X:xpm-pixmap-from-file (xwem-dpy) xwin fname t))
           (x-orig (/ (- xwem-launch-dock-width (X-Pixmap-width ipix)) 2))
           (y-orig (/ (- xwem-launch-dock-height (X-Pixmap-height ipix)) 2))
           (clgc (XCreateGC (xwem-dpy) ipix
                            :clip-x-origin x-orig
                            :clip-y-origin y-orig
                            :clip-mask imask)))
      ;; Copy to our pixmap
      (XCopyArea (xwem-dpy) ipix (xwem-ladock-pixmap xwin) clgc
                 0 0 (X-Pixmap-width ipix) (X-Pixmap-height ipix)
                 x-orig y-orig)
      ;; Release resources
      (XFreeGC (xwem-dpy) clgc)
      (XFreePixmap (xwem-dpy) imask)
      (XFreePixmap (xwem-dpy) ipix))

    (when xwem-misc-turbo-mode
      (XSetWindowBackgroundPixmap (xwem-dpy) xwin (xwem-ladock-pixmap xwin)))

    ;; Install events handler
    (X-Win-EventHandler-add-new xwin 'xwem-ladock-evhandler nil
                                (list X-Expose X-MapNotify X-ButtonPress
                                      X-ButtonRelease X-DestroyNotify))

    (xwem-XTrayInit (xwem-dpy) xwin dockid (or dockgroup "launch") dockalign)
    xwin))


;;;; Open file
;;;###autoload
(defcustom xwem-open-file-commands-alist
  '(("\\.\\(ps\\|ps_pages\\|eps\\)\\'" . "gv")
    ("\\.pdf\\'" . "xpdf")
    ("\\.\\(jpe?g\\|gif\\|png\\)\\'" . "display")
    ("\\.dvi\\'" . "xdvi")
    ("\\.chm\\'" . "xchm")
    ("\\.djvu\\'" . "djview")
    ("\\.txt\\'" . "xterm -e less"))
  "*Alist specifying how to view special types of files.
`xwem-open-file-commands-alist' can be dynamically changed by
`xwemw-open-file' when registering new file extensions, so we
recommend to use `xwem-desktop' package to save/restore
`xwem-open-file-commands-alist' value between sessions."
  :group 'xwem-launcher
  :type '(repeat (cons (string :tag "Regexp")
                       (string :tag "Command"))))

(defcustom xwem-open-file-registration 'query
  "*Defines the behaviour of `xwem-open-file' when file extension is
not defined in `xwem-open-file-commands-alist'.

 nil - Do nothing.
 t - Register new extensions in `xwem-open-file-commands-alist'.
 query - Query about registration."
  :group 'xwem-launcher
  :type '(choice (const :tag "No registration" nil)
                 (const :tag "Auto registration" t)
                 (const :tag "Query for registration" query)))

;;;###xwem-autoload
(defun xwem-file-find-command (filename)
  "Find appropriate command to open FILENAME file."
  (let ((cmds xwem-open-file-commands-alist))
    (while (and cmds (not (string-match (caar cmds) filename)))
      (setq cmds (cdr cmds)))
    (cdr (car cmds))))

;;;###autoload(autoload 'xwem-open-file "xwem-launcher" "Open file with appopriate command" t)
(define-xwem-command xwem-open-file (file &optional command)
  "Open FILE with command specified by COMMAND.
If prefix arg is specified, expicitely query for the COMMAND."
  (xwem-interactive
   (let* ((file (expand-file-name (xwem-read-filename "Find File: ")))
          (cmd (xwem-file-find-command file)))
     (when (or xwem-prefix-arg
               (not cmd))
       (setq cmd (xwem-read-external-command "Command: "))
       ;; Try to register FILE extension in
       ;; `xwem-open-file-commands-alist'.
       (when (and (not xwem-prefix-arg)
                  xwem-open-file-registration)
         (let ((fext (and (string-match "\\.\\([^.]+\\)\\'" file)
                          (match-string 1 file))))
           (when (and fext
                      (or (eq xwem-open-file-registration t)
                          (xwem-under-minibuffer
                           (y-or-n-p (format "Register '%s' extension?: "
                                             fext)))))
             (push (cons (concat "\\." fext "\\'") cmd)
                   xwem-open-file-commands-alist)))))
       (list file cmd)))

  ;; Fixate FILE in case `xwem-open-file' called non-interactively
  (setq file (expand-file-name file))
  (unless command
    (setq command (xwem-file-find-command file)))

  (xwem-launch (format "%s '%s'" command file)))

;;;###autoload(autoload 'xwem-open-recent-file "xwem-launcher" "Open recent file with appopriate command" t)
(define-xwem-command xwem-open-recent-file (filename)
  "Open recent file named FILENAME."
  (xwem-interactive
   (list (xwem-completing-read
          "Open File: " (mapcar 'list (mapcar 'file-name-nondirectory
                                              xwem-read-filename-history)))))
  (let ((fname (find filename xwem-read-filename-history
                     :key 'file-name-nondirectory
                     :test 'string=)))
    (xwem-open-file fname)))


(provide 'xwem-launcher)

;;; xwem-launcher.el ends here
