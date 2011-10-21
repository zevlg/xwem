;;; xwem-worklog.el --- Worklog for XWEM.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;;         Richard Klinda <ignotus@hixsplit.hu>
;; Created: Thu Feb 26 01:00:25 MSK 2004
;; Keywords: xwem
;; Time-stamp: <2/12/2008 13:24:14 lg@h1>

;; This file is part of XWEM.

;; XWEM is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XWEM is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; Inspired by `worklog.el'.

;; Use:

;;   (add-hook 'xwem-after-init-hook 'xwem-worklog-start-dockapp)

;; to start worklog dockapp at start time.

;;; Code:

(eval-when-compile
  (require 'cl)

  ;; Shutup compiler
  (defvar worklog-file nil)
  (autoload 'worklog-make-date-time "worklog")
  (autoload 'worklog-mode "worklog")
  (autoload 'worklog-summarize-tasks "worklog")
  )

(require 'xlib-xlib)
(require 'xlib-xshape)

(require 'xwem-load)
(require 'xwem-misc)
(require 'xwem-compat)
(require 'xwem-diagram)

(defstruct xwem-worklog-pause
  (type 'pause)                         ; type of pause, can be 'pause or 'list
  prefix-arg                            ; prefix arg, when entering pause
  pwin                                  ; xwin that shows pause stuff
  ppix                                  ; pixmap to redraw worklog
  pbuf                                  ; buffer to render
  start-time
  end-time

  itimer                                ; content updater itimer
  )

(defstruct xwem-worklog-task
  name                                  ; task name
  times                                 ; list of cons cells in form (start-time . stop-time)
  (total-time '(0 0))                   ; total time spended on this task
  (today-time '(0 0))                   ; today's time spended on task

  last-comment                          ; last comment to this task
  )

(defgroup xwem-worklog nil
  "Group to customize xwem worklog."
  :prefix "xwem-worklog-"
  :group 'xwem)

(defcustom xwem-worklog-silent nil
  "*Non-nil mean worklog will not print any messages in xwem minibuffer."
  :type 'boolean
  :group 'xwem-worklog)

(defcustom xwem-worklog-file "worklog"
  "*File to store xwem worklogs in worklog format."
  :type 'file
  :group 'xwem-worklog)

(defcustom xwem-worklog-history-mode t
  "*Non-nil mean save tasks to the `xwem-worklog-file'.
Note that `xwem-worklog-task-time-decrease' and
`xwem-worklog-task-time-increase' will not change file entries."
  :type 'boolean
  :group 'xwem-worklog)

(defcustom xwem-worklog-history-lines 2000
  "*Number of lines to read from file, when entering history mode."
  :type 'number
  :group 'xwem-worklog)

(defcustom xwem-worklog-auto-login t
  "*Non-nil mean, automatically login, when worklog started."
  :type 'boolean
  :group 'xwem-worklog)

(defcustom xwem-worklog-auto-continue nil
  "*Non-nil mean auto continue, when task started/stoped while in pause."
  :type 'boolean
  :group 'xwem-worklog)

(defcustom xwem-worklog-pause-dont-stop t
  "*Non-nil mean do not suspend current task when entering pause."
  :type 'boolean
  :group 'xwem-worklog)

(defcustom xwem-worklog-register-new-tasks t
  "*Non-nil mean register new tasks in `xwem-worklog-tasks-description'.
Task not registered if it is already in `xwem-worklog-tasks-description'."
  :type 'boolean
  :group 'xwem-worklog)

(defcustom xwem-worklog-day-start 9
  "*Hour number when your workday starts in 0-23 range."
  :type 'number
  :group 'xwem-worklog)

(defcustom xwem-worklog-day-ends 18
  "*Hour number when your workday ends."
  :type 'number
  :group 'xwem-worklog)

(defcustom xwem-worklog-logout-notify-period 5
  "*Period in minutes to notify, that your workday is end, and you need to logout."
  :type 'number
  :group 'xwem-worklog)

(defcustom xwem-worklog-logout-auto-period 60
  "*Period in minutes to autologout, after your workday is end."
  :type 'number
  :group 'xwem-worklog)

(defcustom xwem-worklog-login-notify-period 5
  "*Period in minutes to notify that your workday started."
  :type 'number
  :group 'xwem-worklog)

(defcustom xwem-worklog-login-stop-period 45
  "*Period in minites when login notifying stops."
  :type 'number
  :group 'xwem-worklog)

(defcustom xwem-worklog-pwin-width 500
  "*Width of pause window."
  :type 'number
  :group 'xwem-worklog)

(defcustom xwem-worklog-pwin-height 700
  "*Height of xwem worklog pause window."
  :type 'number
  :group 'xwem-worklog)

(defcustom xwem-worklog-pwin-border-width 2
  "*Border width for xwem worklog pause window."
  :type 'number
  :group 'xwem-worklog)

(defcustom xwem-worklog-pwin-border-color "red4"
  "*Border color for xwem worklog pause window."
  :type 'color
  :group 'xwem-worklog)

(defcustom xwem-worklog-pause-cursor-shape 'X-XC-gumby
  "*Shape of cursor while in xwem worklog pause."
  :type (xwem-cursor-shape-choice)
  :group 'xwem-worklog)

(defcustom xwem-worklog-pause-cursor-foreground-color "#777777"
  "*Cursor's foreground color, while in xwem worklog pause."
  :type 'color
  :group 'xwem-worklog)

(defcustom xwem-worklog-pause-cursor-background-color nil
  "*Cursor's background color, while in xwem worklog pause."
  :type 'color
  :group 'xwem-worklog)

(defcustom xwem-worklog-use-diagram t
  "*Non-nil mean draw digram for today, when enter list mode."
  :type 'boolean
  :group 'xwem-worklog)

(defcustom xwem-worklog-diagram-type '3d
  "*Type of diagram to draw in list mode."
  :type '(choice (const :tag "3D Sectors" 3d)
                 (const :tag "Plain Sectors" plain))
  :group 'xwem-worklog)

(defcustom xwem-worklog-dockapp-diagram-type 'plain
  "*Type of diagram to draw in worklog dockapp."
  :type '(choice (const :tag "3D Sectors" 3d)
                 (const :tag "Plain Sectors" plain))
  :group 'xwem-worklog)

(defcustom xwem-worklog-dockapp-update-rate 200
  "*Dockapp updates per working day."
  :type 'number
  :group 'xwem-worklog)

(defcustom xwem-worklog-sort-type 'Today-time
  "*Sorting type for `xwem-worklog-sorted-task-list'."
  :type '(choice (const :tag "Sort by name" name)
                 (const :tag "Sort by Name" Name)
                 (const :tag "Sort by today time" today-time)
                 (const :tag "Sort by Today time" Today-time)
                 (const :tag "Sort by total time" total-time)
                 (const :tag "Sort by Total time" Total-time))
  :group 'xwem-worklog)

;;;###autoload
(defcustom xwem-worklog-load-hook nil
  "*Hooks to run when `xwem-worklog' loaded."
  :type 'hook
  :group 'xwem-worklog)

;;;###autoload
(defcustom xwem-worklog-task-start-hook nil
  "*Hooks to run when new task just started."
  :type 'hook
  :group 'xwem-worklog)

;;;###autoload
(defcustom xwem-worklog-task-stop-hook nil
  "*Hooks to run when task just stoped."
  :type 'hook
  :group 'xwem-worklog)

;;;###autoload
(defcustom xwem-worklog-task-pause-hook nil
  "*Hooks to run when task is paused."
  :type 'hook
  :group 'xwem-worklog)

;;;###autoload
(defcustom xwem-worklog-task-resume-hook nil
  "*Hooks to run when task is resumed."
  :type 'hook
  :group 'xwem-worklog)

;;;###autoload
(defcustom xwem-worklog-login-hook nil
  "*Hooks to run when login."
  :type 'hook
  :group 'xwem-worklog)

;;;###autoload
(defcustom xwem-worklog-logout-hook nil
  "*Hooks to run when logout."
  :type 'hook
  :group 'xwem-worklog)

;;;###autoload
(defcustom xwem-worklog-tasks-description
  '(("Watching movies" (:key [(hyper ?v)] :color "violet" :cost 0)
     (application "mplayer"))
    ("Web browsing" (:key [(hyper ?w)] :color "dodgerblue" :cost 0)
     (or (application "mozilla")
         (application "firefox")
         (application "opera")
         (application "links")
         (buffer-major-mode w3m-mode)
         (and (application "xemacs")
              (name "\\*w3m\\*"))))
    ("Mail reading" (:key [(hyper ?m) ?r] :color "blue1" :cost 0)
     (or (buffer-major-mode gnus-group-mode)
         (buffer-major-mode gnus-article-mode)
         (buffer-major-mode gnus-summary-mode)
         ;; For remote emacsen
         (and (application "xemacs")
              (name"\\*\\(Group\\|Summary\\|Article\\)"))))
    ("Mail writing" (:key [(hyper ?m) ?w] :color "blue4" :cost 0)
     (or (buffer-major-mode message-mode)
         (buffer-major-mode bbdb-mode)
         ;; For remote emacsen
         (and (application "xemacs")
              (name"\\*\\(BBDB\\*\\|mail\\*\\|wide reply\\|reply\\)"))))
    ("Chating" (:key [?C] :color "palegreen" :cost 0)
     (or (buffer-major-mode erc-mode)
         (buffer-major-mode eicq-buddy-mode)
	 (buffer-major-mode eicq-log-mode)
	 (and (application "xemacs")
	      (or (name "\\*Status\\*")
                  (name "\\(#eicq\\|#xemacs\\|#sxemacs\\|irc\\.freenode\\.org\\)")))
         (application "licq")
	 (name "[LlMmVv][Ii][Cc][Qq]")))
    ("Info/man reading" (:key [(hyper ?i)] :color "cyan" :cost 10)
     (or (buffer-major-mode Manual-mode)
         (buffer-major-mode Info-mode)
         (buffer-major-mode hyper-apropos-mode)
         (and (application "xemacs")
              (name "\\(\\*info\\*\\|Man: \\|\\*Help\\*\\|\\*Hyper Apropos\\*\\)"))
         (class-name "Xman")))
    ("Emacs lisping" (:key [(hyper ?e)] :color "yellow2" :cost 0)
     (or (buffer-major-mode emacs-lisp-mode)
         (buffer-major-mode lisp-interaction-mode)
         (and (application "xemacs")
              (name "\\(\\*scratch\\*\\|\\.el\\)"))))
    ("C for profit" (:key [(hyper ?p)] :color "magenta" :cost 100)
     (or (buffer-major-mode c-mode)
         (buffer-major-mode gdb-mode)
         (application "ddd")
         (and (or (application "xemacs")
                  (and (class-inst "^.term$")
                       (class-name "^.Term$")))
              (name "\\(gdb\\|\\.[ch]\\)"))))
    ("Docs reading" (:key [(hyper ?d) ?r] :color "forestgreen" :cost 50)
     (or (application "acroread")
         (application "xpdf")
         (application "djview")
         (application "gv")
         (application "xchm")))
    ("Docs writing" (:key [(hyper ?d) ?w] :color "olivedrab" :cost 300)
     (or (buffer-major-mode plain-tex-mode)
         (buffer-major-mode bibtex-mode)
         (buffer-major-mode latex-mode)
         (buffer-major-mode texinfo-mode)
         (and (application "xemacs")
              (name "\\.texi?"))))
    ("Administrativa" (:key [(hyper ?a)] :color "lightblue" :cost 150)
     (or (application "ethereal")
         (and (application "xterm")
              (name "\\(tcpdump\\|ssh\\)"))))
    ("Windows remote" (:color "lightcyan")
     (application "rdesktop"))
    ("Minibuffer Usage" (:color "tan")
     (predicate xwem-minibuffer-client-p))

    ;; Without matchers (can be triggered only with key)
    ("Smoke" (:key [(hyper ?s)] :color "red3" :cost -50))
    ("Nothing" (:key [(hyper ?n)] :color "gray80" :cost 50))
    ("WorkProject" (:key [(hyper ?c)] :color "green2" :cost 200))
    ("C for fun" (:key [(hyper ?f)] :color "tomato" :cost 0))

    ("Unknown" (:color "gray50" :cost 0)
     (t)))
  "List of task descriptions in `xwem-manage-list' format.

Supported properties are:

  :key   - Key to bind task in `xwem-worklog-map'.
  :color - Color to use when drawing some task related info.
  :cost  - Task cost in $/hour. (not implemented)
  :image - Use image instead of :color. (not implemented)

You could define your own properties, which you will handle for
yourself.
"
  :type '(repeat sexp)
  :group 'xwem-worklog)

;;; Worklog dockapp
(defcustom xwem-worklog-dockapp-width 24
  "*Width in pixels for worklog dockapp."
  :type 'number
  :group 'xwem-worklog)

(defcustom xwem-worklog-dockapp-height 24
  "*Height in pixels for worklog dockapp."
  :type 'number
  :group 'xwem-worklog)

(defcustom xwem-worklog-dockapp-sector-width 0
  "*Sector width for worklog dockapps."
  :type 'number
  :group 'xwem-worklog)

;;; Internal variables

;; Worklog keymap
;;;###autoload(autoload 'xwem-worklog-prefix "xwem-worklog" nil nil 'keymap)
(xwem-define-prefix-command 'xwem-worklog-prefix t)
(defvar xwem-worklog-map (symbol-function 'xwem-worklog-prefix)
  "Keymap for xwem worklog (\\<xwem-global-map>\\[xwem-worklog-prefix]) commands.
Bindings:
\\{xwem-worklog-map}")

(define-key xwem-worklog-map (xwem-kbd "?") 'xwem-worklog-task-info)
(define-key xwem-worklog-map (xwem-kbd "<tab>") 'xwem-worklog-login)
(define-key xwem-worklog-map (xwem-kbd "i") 'xwem-worklog-login)
(define-key xwem-worklog-map (xwem-kbd "<backspace>") 'xwem-worklog-logout)
(define-key xwem-worklog-map (xwem-kbd "o") 'xwem-worklog-logout)

(define-key xwem-worklog-map (xwem-kbd "b") 'xwem-worklog-begin-task)
(define-key xwem-worklog-map (xwem-kbd "e") 'xwem-worklog-end-task)
(define-key xwem-worklog-map (xwem-kbd "p") 'xwem-worklog-pause)
(define-key xwem-worklog-map (xwem-kbd "l") 'xwem-worklog-task-list)

;; Sorting
(define-key xwem-worklog-map (xwem-kbd "s n") 'xwem-worklog-sort-by-name)
(define-key xwem-worklog-map (xwem-kbd "s N") 'xwem-worklog-sort-by-Name)
(define-key xwem-worklog-map (xwem-kbd "s t") 'xwem-worklog-sort-by-today-time)
(define-key xwem-worklog-map (xwem-kbd "s T") 'xwem-worklog-sort-by-Today-time)
(define-key xwem-worklog-map (xwem-kbd "s g") 'xwem-worklog-sort-by-total-time)
(define-key xwem-worklog-map (xwem-kbd "s G") 'xwem-worklog-sort-by-Total-time)

(defvar xwem-worklog-pause-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-default-binding map 'xwem-worklog-continue)

    (define-key map (xwem-kbd "-") 'xwem-worklog-task-time-decrease)
    (define-key map (xwem-kbd "+") 'xwem-worklog-task-time-increase)

    (define-key map (xwem-kbd "0") 'xwem-universal-digit)
    (define-key map (xwem-kbd "1") 'xwem-universal-digit)
    (define-key map (xwem-kbd "2") 'xwem-universal-digit)
    (define-key map (xwem-kbd "3") 'xwem-universal-digit)
    (define-key map (xwem-kbd "4") 'xwem-universal-digit)
    (define-key map (xwem-kbd "5") 'xwem-universal-digit)
    (define-key map (xwem-kbd "6") 'xwem-universal-digit)
    (define-key map (xwem-kbd "7") 'xwem-universal-digit)
    (define-key map (xwem-kbd "8") 'xwem-universal-digit)
    (define-key map (xwem-kbd "9") 'xwem-universal-digit)
    (define-key map xwem-universal-key 'xwem-universal-argument)

    (set-keymap-parents map (list xwem-worklog-map))
    map)
  "Keymap used when worklog in pause state.")

(defvar xwem-worklog-pause-cursor nil
  "Cursor used while in xwem worklog pause.")

(defvar xwem-worklog-pause-p nil
  "Non-nil when worklog in pause state.
Internal variable, do not modify!.")

(defvar xwem-worklog-current-task nil
  "Current task.")

(defvar xwem-worklog-task-list nil
  "List of tasks.")

(defvar xwem-worklog-pause-window-update-hook nil
  "Hooks called when updating pause window contents.
For internal usage only.")

(defvar xwem-worklog-logged-in nil
  "Non-nil if we are logged in.")


;;; Define new qualifier `worklog'
(require 'xwem-manage)

(define-xwem-qualifier worklog
  "Return non-nil if PARAM matches current worklog task."
  #'(lambda (cl param)
      (string-match
       param (xwem-worklog-task-name (xwem-worklog-current-task)))))


;;; Functions
(defun xwem-worklog-current-task ()
  "Return current worklog task."
  xwem-worklog-current-task)

(defun xwem-worklog-create-cmd (template)
  "Create symbol from TEMPLATE string."
  (let ((fsym (intern (concat "xwem-worklog-custom-"
                              (replace-in-string template " " "-")))))
    (fset fsym `(lambda ()
                  (interactive)
                  (xwem-worklog-begin-task ,template)))
    fsym))

(defun xwem-worklog-lookup-description (name)
  "Lookup description for task named by NAME."
  (assoc name xwem-worklog-tasks-description))

(defun xwem-worklog-register-task (name &optional no-binding)
  "Register new task with NAME in `xwem-worklog-tasks-description'.
Query for keybinding unless NO-BINDING is non-nil."
  (unless (xwem-worklog-lookup-description name)
    (let ((key (and (not no-binding)
                    (xwem-read-key
                     (format "Key for '%s' task: " name))))
          (col (xwem-read-from-minibuffer
                (format "Color for '%s' task: " name))))
      (when key
        (setq key (events-to-keys (vector key)))
        (define-key xwem-worklog-map key (xwem-worklog-create-cmd name)))

      (setq xwem-worklog-tasks-description
            (cons (list name (list :key key :color col))
                  xwem-worklog-tasks-description)))))

(defun xwem-worklog-find-task (name &optional create)
  "Search for task with NAME in tasks list."
  (let ((tasks xwem-worklog-task-list))
    (while (and tasks (not (string= name (xwem-worklog-task-name (car tasks)))))
      (setq tasks (cdr tasks)))

    (if (or tasks (not create))
        (car tasks)

      (let ((task (make-xwem-worklog-task :name name)))
        (setq xwem-worklog-task-list (cons task xwem-worklog-task-list))
        task))))

(defun xwem-worklog-sorted-task-list ()
  "Return sorted `xwem-worklog-task-list'."
  (sort (copy-list xwem-worklog-task-list)
        #'(lambda (e1 e2)
            (cond ((eq xwem-worklog-sort-type 'name)
                   (string-lessp (xwem-worklog-task-name e1)
                                 (xwem-worklog-task-name e2)))
                  ((eq xwem-worklog-sort-type 'Name)
                   (string-lessp (xwem-worklog-task-name e2)
                                 (xwem-worklog-task-name e1)))

                  ((memq xwem-worklog-sort-type '(today-time Today-time))
                   (let ((tt1 (xwem-worklog-get-today-time e1))
                         (tt2 (xwem-worklog-get-today-time e2))
                         tdiff)
                     (cond ((eq xwem-worklog-sort-type 'today-time)
                            (setq tdiff (xwem-worklog-time-diff tt1 tt2)))
                           ((eq xwem-worklog-sort-type 'Today-time)
                            (setq tdiff (xwem-worklog-time-diff tt2 tt1))))
                     (or (< (car tdiff) 0) (< (cadr tdiff) 0))))

                  ((memq xwem-worklog-sort-type '(total-time Total-time))
                   (let ((tt1 (xwem-worklog-get-total-time e1))
                         (tt2 (xwem-worklog-get-total-time e2))
                         tdiff)
                     (cond ((eq xwem-worklog-sort-type 'total-time)
                            (setq tdiff (xwem-worklog-time-diff tt1 tt2)))
                           ((eq xwem-worklog-sort-type 'Total-time)
                            (setq tdiff (xwem-worklog-time-diff tt2 tt1))))
                     (or (< (car tdiff) 0) (< (cadr tdiff) 0))))
                  ))))

(define-xwem-command xwem-worklog-begin-task (name &optional arg)
  "Start new worklog task named after NAME.
If prefix ARG is specified, and we are in pause, than resume."
  (xwem-interactive (list (xwem-completing-read "Task name: "
                                                xwem-worklog-tasks-description)
                          xwem-prefix-arg))

  (when xwem-worklog-current-task
    ;; Some task runned
    (xwem-worklog-end-task xwem-worklog-current-task))

  ;; Register in `xwem-worklog-tasks-description'
  (when (and (interactive-p)
             xwem-worklog-register-new-tasks)
    (xwem-worklog-register-task name))

  ;; go go go!
  (setq xwem-worklog-current-task
        (xwem-worklog-find-task name t))

  (unless (and xwem-worklog-pause-p
               (not (or xwem-worklog-pause-dont-stop
                        (xwem-worklog-pause-prefix-arg xwem-worklog-pause-p))))
    (xwem-worklog-resume-task xwem-worklog-current-task))

  (when (and (or arg xwem-worklog-auto-continue)
             xwem-worklog-pause-p)
    (xwem-worklog-pause-stop)
    (xwem-worklog-resume-task xwem-worklog-current-task))

  (run-hooks 'xwem-worklog-task-start-hook)

  (unless xwem-worklog-silent
    (xwem-message 'worklog "New task '%s' started." name)))

(define-xwem-command xwem-worklog-end-task (task &optional arg)
  "Stop TASK.
By default `xwem-worklog-current-task' assumed.
If prefix ARG is specified, and we are in pause, than resume."
  (xwem-interactive (list xwem-worklog-current-task xwem-prefix-arg))

  (when (or arg xwem-worklog-auto-continue)
    (xwem-worklog-pause-stop))

  (when task
    (xwem-worklog-pause-task task)
    (xwem-worklog-task-update-total-time task)
    (xwem-worklog-task-update-today-time task)

    (run-hooks 'xwem-worklog-task-stop-hook)

    (unless xwem-worklog-silent
      (xwem-message 'worklog "Task '%s' stoped."
                    (xwem-worklog-task-name xwem-worklog-current-task)))

    (setq xwem-worklog-current-task nil)))

(defun xwem-worklog-task-change-time (how arg)
  "Change runtime of current task by ARG minutes.
HOW is one of '- or '+."
  (when (and xwem-worklog-current-task
             (caar (xwem-worklog-task-times xwem-worklog-current-task)))
    (let ((ctime (decode-time (caar (xwem-worklog-task-times
                                     xwem-worklog-current-task)))))
      (setcar (cdr ctime)
              (funcall how (cadr ctime) arg))
      (setcar (car (xwem-worklog-task-times xwem-worklog-current-task))
              (apply 'encode-time ctime)))))

(define-xwem-command xwem-worklog-task-time-increase (arg)
  "Increase runtime of current task by ARG minutes."
  (xwem-interactive "p")
  (xwem-worklog-task-change-time '- arg))

(define-xwem-command xwem-worklog-task-time-decrease (arg)
  "Decrease runtime of current task by ARG minutes."
  (xwem-interactive "p")
  (xwem-worklog-task-change-time '+ arg))

(defun xwem-worklog-pause-task (task &optional time)
  "Pause task named by NAME."
  (unless (cdr (car (xwem-worklog-task-times task)))
    (setcdr (car (xwem-worklog-task-times task))
            (or time (current-time)))

    (unless time
      (run-hooks 'xwem-worklog-task-pause-hook))))

(defun xwem-worklog-resume-task (task &optional time)
  "Resume TASK."
  (when (or (null (xwem-worklog-task-times task))
            (cdr (car (xwem-worklog-task-times task))))
    (setf (xwem-worklog-task-times task)
          (cons (cons (or time (current-time)) nil)
                (xwem-worklog-task-times task)))

    (unless time
      (run-hooks 'xwem-worklog-task-resume-hook))))

(defun xwem-worklog-task-update-total-time (task)
  "Update total-time entry of TASK."
  (setf (xwem-worklog-task-total-time task)
        (apply 'xwem-worklog-calc-time
               (mapcar #'(lambda (el)
                           (xwem-worklog-time-diff (cdr el) (car el)))
                       (xwem-worklog-task-times task)))))

(defun xwem-worklog-task-update-today-time (task)
  "Update today-time entry of TASK."
  (let ((ttimes (xwem-worklog-task-times task))
        (ctime (decode-time (current-time)))
        stime etime rttimes)
    ;; Set secs, mins, hours to 0
    (setcar ctime 0)
    (setcar (cdr ctime) 0)
    (setcar (cddr ctime) 0)
    (setq stime (apply 'encode-time ctime))

    ;; Set secs=59, mins=59, hours=23
    (setcar ctime 59)
    (setcar (cdr ctime) 59)
    (setcar (cddr ctime) 23)
    (setq etime (apply 'encode-time ctime))

    (while ttimes
      (setq ctime (car ttimes))
      (if (and (car ctime) (cdr ctime)
               (xwem-worklog-time-> (car ctime) stime)
               (xwem-worklog-time-< (cdr ctime) etime))
          (setq rttimes (cons ctime rttimes)))
      (setq ttimes (cdr ttimes)))

    (setf (xwem-worklog-task-today-time task)
          (and rttimes
               (apply 'xwem-worklog-calc-time
                      (mapcar #'(lambda (el)
                                  (xwem-worklog-time-diff (cdr el) (car el)))
                              rttimes))))))

(defun xwem-worklog-calc-time (ct &rest diffs)
  "Calculate total time."
  (cond ((or (null diffs)
             (null (car diffs)))
         ct)

        (t (mapc #'(lambda (el)
                     (setcar ct (+ (nth 0 ct) (nth 0 el)))
                     (setcar (cdr ct) (+ (nth 1 ct) (nth 1 el)))
                     (when (> (cadr ct) 65535)
                       (incf (car ct))
                       (decf (cadr ct) 65536)))
                 diffs)
           ct)))

(defvar xwem-worklog-keymap-functions-skip
  '(xwem-universal-digit xwem-worklog-continue)
  "List of commands, when update is not required.")

(defun xwem-worklog-keymap-functions (keymap)
  (apply 'nconc
         (let ((wfuncs))
           (map-keymap
            #'(lambda (b f)
                (cond ((commandp f)
                       (setq wfuncs (cons f wfuncs)))
                      ((keymapp f)
                       (setq wfuncs (nconc (xwem-worklog-keymap-functions f)
                                           wfuncs)))))
            keymap)
           wfuncs)
         (mapcar #'xwem-worklog-keymap-functions (keymap-parents keymap))))

(defun xwem-worklog-post-command ()
  "Function used in `xwem-post-command-hook'."
  (when xwem-worklog-pause-p
    (let ((wfuncs (xwem-worklog-keymap-functions xwem-worklog-pause-map)))
      (when (and (memq xwem-last-command wfuncs)
                 (not (memq xwem-last-command
                            xwem-worklog-keymap-functions-skip)))
        (xwem-worklog-pause-update)
        (xwem-kbd-stop-command-keys-echoing))))

  (setq xwem-override-map xwem-worklog-pause-map))

(define-xwem-command xwem-worklog-pause (arg &optional type)
  "Pause task counting.
If called with prefix ARG, than do not pause current task, if any."
  (xwem-interactive "P")

  ;; Since we will block here, we need to reset `xwem-prefix-arg'
  (setq xwem-prefix-arg nil)

  (if xwem-worklog-pause-p
      ;; Already paused .. so just change type
      (progn
        (setf (xwem-worklog-pause-prefix-arg xwem-worklog-pause-p) arg)
        (setf (xwem-worklog-pause-type xwem-worklog-pause-p) (or type 'pause)))

    ;; Pause current task, if we are not already in pause
    (when (and xwem-worklog-current-task
               (not (or arg xwem-worklog-pause-dont-stop)))
      (xwem-worklog-pause-task xwem-worklog-current-task)
      (xwem-worklog-task-update-total-time xwem-worklog-current-task)
      (xwem-worklog-task-update-today-time xwem-worklog-current-task))

    (xwem-worklog-pause-start (or type 'pause) arg)

    (let ((xwem-override-map xwem-worklog-pause-map)
          xev)

      ;; Clear minibuffer
      (xwem-kbd-stop-command-keys-echoing)
      (xwem-clear-message 'keys)

      (xwem-kbd-stop-grabbing)
      (xwem-kbd-start-grabbing xwem-worklog-pause-cursor)

      (pushnew 'xwem-worklog-post-command xwem-post-command-hook)
      (xwem-unwind-protect
          (while xwem-worklog-pause-p
            (setq xev (xwem-next-event 1))
            (if (not xev)
                ;; Timeout
                (let ((xwem-worklog-pause-window-update-hook
                       '(xwem-worklog-show-color-bricks)))
                  (xwem-worklog-pause-update))
              ;; Event arrived
              (X-Event-CASE xev
                ((:X-KeyPress :X-KeyRelease :X-ButtonPress :X-ButtonRelease)
                 (xwem-dispatch-command-xevent xev)
                 (xwem-kbd-start-grabbing xwem-worklog-pause-cursor)))
              ))

        (setq xwem-post-command-hook
              (delq 'xwem-worklog-post-command xwem-post-command-hook))
        ;; Ungrab keyboard and stop pauser if any
        (xwem-worklog-pause-stop)
        (xwem-kbd-stop-grabbing)))))

(define-xwem-command xwem-worklog-task-list (arg)
  "Display task list using pause window."
  (xwem-interactive "P")

  (let ((xwem-worklog-pause-window-update-hook
         (if xwem-worklog-use-diagram
             (list 'xwem-worklog-show-color-bricks
                   'xwem-worklog-draw-today-diagram)
           (list 'xwem-worklog-show-color-bricks))))
    (xwem-worklog-pause arg 'list)))

(define-xwem-command xwem-worklog-continue (arg)
  "Continue run current task."
  (xwem-interactive "P")

  (unless (button-release-event-p xwem-last-event)
    (xwem-worklog-pause-stop)
    (when xwem-worklog-current-task
      (xwem-worklog-resume-task xwem-worklog-current-task)
      (unless xwem-worklog-silent
        (xwem-message 'worklog "Continuing '%s' task"
                      (xwem-worklog-task-name xwem-worklog-current-task))))))

(defun xwem-worklog-pause-start (type arg)
  "Start xwem worklog pausing."
  (if xwem-worklog-pause-p
      ;; Already exists, so just change type
      (setf (xwem-worklog-pause-type xwem-worklog-pause-p) type)

    ;; Create new pause window
    (setq xwem-worklog-pause-p
          (make-xwem-worklog-pause
           :type type
           :prefix-arg arg
           :pbuf (get-buffer-create " *worklog-pause*")
           :start-time (current-time)))

    ;; Create window and pixmap
    (setf (xwem-worklog-pause-pwin xwem-worklog-pause-p)
          (xwem-worklog-pause-create-xwin)
          (xwem-worklog-pause-ppix xwem-worklog-pause-p)
          (xwem-worklog-pause-create-xpixmap
           (xwem-worklog-pause-pwin xwem-worklog-pause-p)))

    (xwem-worklog-pause-update)))

(defun xwem-worklog-pause-stop ()
  "Stop xwem worklog pausing."

  (when xwem-worklog-pause-p
    (XFreePixmap (xwem-dpy) (xwem-worklog-pause-ppix xwem-worklog-pause-p))
    (when (X-Win-p (xwem-worklog-pause-pwin xwem-worklog-pause-p))
      (XDestroyWindow
       (xwem-dpy) (xwem-worklog-pause-pwin xwem-worklog-pause-p)))
    (when (bufferp (xwem-worklog-pause-pbuf xwem-worklog-pause-p))
      (kill-buffer (xwem-worklog-pause-pbuf xwem-worklog-pause-p)))

    (setq xwem-worklog-pause-p nil)))

(defun xwem-worklog-pause-create-xwin ()
  "Create pause window at the centre of selected frame."
  ;; Create cursor
  (unless xwem-worklog-pause-cursor
    (setq xwem-worklog-pause-cursor
          (xwem-make-cursor (eval xwem-worklog-pause-cursor-shape)
                            xwem-worklog-pause-cursor-foreground-color
                            xwem-worklog-pause-cursor-background-color)))

  (let* ((xfgeom (xwem-frame-xgeom (xwem-frame-selected)))
         (xwin (XCreateWindow
                (xwem-dpy) nil
                (+ (X-Geom-x xfgeom)
                   (/ (- (X-Geom-width xfgeom) xwem-worklog-pwin-width) 2))
                (+ (X-Geom-y xfgeom)
                   (/ (- (X-Geom-height xfgeom) xwem-worklog-pwin-height) 2))
                xwem-worklog-pwin-width xwem-worklog-pwin-height
                xwem-worklog-pwin-border-width nil nil nil
                :override-redirect t
                :backing-store X-Always
                :background-pixel
                (XAllocNamedColor
                 (xwem-dpy) (XDefaultColormap (xwem-dpy))
                 (face-background-name 'default))
                :border-pixel
                (XAllocNamedColor
                 (xwem-dpy) (XDefaultColormap (xwem-dpy))
                 xwem-worklog-pwin-border-color)
                :event-mask
                (Xmask-or XM-Exposure
                          XM-StructureNotify
                          XM-ButtonPress XM-ButtonRelease))))
    (XMapWindow (xwem-dpy) xwin)
    (XRaiseWindow (xwem-dpy) xwin)
    xwin))

(defun xwem-worklog-pause-create-xpixmap (xwin)
  "Create X Pixmap for pause window."
  (XCreatePixmap (xwem-dpy) xwin (XDefaultDepth (xwem-dpy))
                 xwem-worklog-pwin-width xwem-worklog-pwin-height))

(defun xwem-worklog-time-diff (a b)
  "Return the difference between two times.
This function requires the second argument B to be earlier in time
than the first argument A."
  (cond ((= (nth 0 a) (nth 0 b)) (list 0 (- (nth 1 a) (nth 1  b))))
        ((> (nth 1 b) (nth 1 a)) (list (- (nth 0 a) (nth 0 b) 1)
                                       (- (+ 65536 (nth 1 a)) (nth 1 b))))
        (t (list (- (nth 0 a) (nth 0 b))
                 (- (nth 1 a) (nth 1 b))))))

(defun xwem-worklog-time-sum (&optional a &rest b)
  "Return the sum of two times A and B."
  (unless a
    (setq a '(0 0)))

  (if (or (not b) (not (car b)))
      a

    (let* ((rt1 (+ (nth 0 a) (nth 0 (car b))))
           (rt2 (+ (nth 1 a) (nth 1 (car b)))))
      (when (> rt2 65535)
        (setq rt2 (% rt2 65536))
        (setq rt1 (1+ rt1)))
      (apply 'xwem-worklog-time-sum (cons (list rt1 rt2) (cdr b)))
      )))

(defun xwem-worklog-format-time (time &optional padlen)
  "Return string."
  (let (rawtime days hours minutes seconds lfm)
    ;; XXX can't deal with car
    (if (not (zerop (car time)))
        (setq rawtime (+ (* 65536.0 (car time)) (cadr time)))
      (setq rawtime (float (cadr time))))

    (setq days (truncate (/ rawtime (* 60 60 24))))
    (setq rawtime (- rawtime (* 60 60 24 days)))
    (setq hours (truncate (/ rawtime (* 60 60))))
    (setq rawtime (- rawtime (* 60 60 hours)))
    (setq minutes (truncate (/ rawtime 60)))
    (setq rawtime (- rawtime (* 60 minutes)))
    (setq seconds (truncate rawtime))

    (setq lfm
          (cond ((not (zerop days)) (format "%dd %dh" days hours))
                ((not (zerop hours)) (format "%dh %dm" hours minutes))
                (t (format "%dm %ds" minutes seconds))))
    (if padlen
        (if (> padlen (length lfm))
            (concat lfm (make-string (- padlen (length lfm)) ?\x20))
          (substring lfm 0 padlen))
      lfm)))

(defvar xwem-worklog-bricks-width
  (font-ascent (face-font 'default)))

(defvar xwem-worklog-bricks-offset 6)

(define-xwem-deferred xwem-worklog-pause-update ()
  "Redraw pause win."
  (when xwem-worklog-pause-p
    (with-current-buffer (xwem-worklog-pause-pbuf xwem-worklog-pause-p)
      (erase-buffer)

      (insert "XWEM Worklog mode ")
      (insert-face "PAUSE: " 'red)
      (insert (xwem-worklog-format-time
               (xwem-worklog-time-diff
                (current-time)
                (xwem-worklog-pause-start-time xwem-worklog-pause-p))
               20))
      (insert "\n\n")

      (if (eq (xwem-worklog-pause-type xwem-worklog-pause-p) 'pause)
          (xwem-worklog-insert-current-task)
        (xwem-worklog-insert-task-list))

      (insert "\n")
      (insert "Press any key to continue with current task.\n")
      (if (eq (xwem-worklog-pause-type xwem-worklog-pause-p) 'list)
          (insert (substitute-command-keys
                   "Press ``\\<xwem-worklog-pause-map>\\[xwem-worklog-pause]'' to show pause buffer.\n"))

        (insert "Bindings:\n")
        (insert "Key             Binding\n")
        (insert "---             -------\n")
        (describe-bindings-internal xwem-worklog-pause-map))

      (xwem-set-face-foreground
       'xwem-worklog-temp-face (face-background-name 'default))
      (XFillRectangle (xwem-dpy) (xwem-worklog-pause-ppix xwem-worklog-pause-p)
                      (xwem-face-get-gc 'xwem-worklog-temp-face)
                      0 0 xwem-worklog-pwin-width xwem-worklog-pwin-height)
      (xwem-misc-textsp-show (xwem-worklog-pause-ppix xwem-worklog-pause-p)
                             (+ xwem-worklog-bricks-width
                                xwem-worklog-bricks-offset
                                xwem-worklog-bricks-offset)
                             (face-height 'default)
                             (xwem-misc-buffer->textsp 'default))
      )

    ;; Run update hooks in case there is something to draw more
    (run-hooks 'xwem-worklog-pause-window-update-hook)

    ;; Update PWIN
    (XCopyArea (xwem-dpy) (xwem-worklog-pause-ppix xwem-worklog-pause-p)
               (xwem-worklog-pause-pwin xwem-worklog-pause-p)
               (XDefaultGC (xwem-dpy)) 0 0
               xwem-worklog-pwin-width xwem-worklog-pwin-height
               0 0)))

(defun xwem-worklog-last-time-string (task &optional padlen)
  "Return time string of last time of TASK was runned."
  (let ((ctime (or (cdr (car (xwem-worklog-task-times task))) (current-time))))
    (if (caar (xwem-worklog-task-times xwem-worklog-current-task))
        (xwem-worklog-format-time
         (xwem-worklog-time-diff
          ctime (caar (xwem-worklog-task-times xwem-worklog-current-task)))
         padlen)

      (if padlen
          (concat "---" (make-string (- padlen 3) ?\x20)) ; XXX
        "---"))))

(defun xwem-worklog-get-today-time (task)
  "Return time TASK was runned today."
  (if (cdr (car (xwem-worklog-task-times task)))
      (or (xwem-worklog-task-today-time task) '(0 0))

    ;; Task not paused
    (if (caar (xwem-worklog-task-times task))
         (xwem-worklog-calc-time
          (xwem-worklog-time-diff
           (current-time) (caar (xwem-worklog-task-times task)))
          (xwem-worklog-task-today-time task))
      '(0 0))))

(defun xwem-worklog-get-total-time (task)
  "Return total time TASK was runned."
  (if (cdr (car (xwem-worklog-task-times task)))
      (or (xwem-worklog-task-total-time task) '(0 0))

    ;; Task not paused
    (if (caar (xwem-worklog-task-times task))
         (xwem-worklog-calc-time
          (xwem-worklog-time-diff
           (current-time) (caar (xwem-worklog-task-times task)))
          (xwem-worklog-task-total-time task))
      '(0 0))))

(defun xwem-worklog-today-time-string (task &optional padlen)
  "Return time string for today time of TASK."
  (if (cdr (car (xwem-worklog-task-times task)))
      (if (xwem-worklog-task-today-time task)
          (xwem-worklog-format-time
           (xwem-worklog-task-today-time task) padlen)
        (if padlen
            (concat "---" (make-string (- padlen 3) ?\x20)) ; XXX
          "---"))

    ;; Task not paused
    (if (caar (xwem-worklog-task-times task))
        (xwem-worklog-format-time
         (xwem-worklog-calc-time
          (xwem-worklog-time-diff
           (current-time) (caar (xwem-worklog-task-times task)))
          (xwem-worklog-task-today-time task))
         padlen)

      (if padlen
          (concat "---" (make-string (- padlen 3) ?\x20)) ; XXX
        "---"))))

(defun xwem-worklog-total-time-string (task &optional padlen)
  "Return time string for total time of TASK."
  (if (cdr (car (xwem-worklog-task-times task)))
      (if (xwem-worklog-task-total-time task)
          (xwem-worklog-format-time (xwem-worklog-task-total-time task) padlen)

        (if padlen
            (concat "---" (make-string (- padlen 3) ?\x20)) ; XXX
          "---"))

    ;; Task not paused
    (if (caar (xwem-worklog-task-times task))
        (xwem-worklog-format-time
         (xwem-worklog-calc-time
          (xwem-worklog-time-diff
           (current-time) (caar (xwem-worklog-task-times task)))
          (xwem-worklog-task-total-time task))
         padlen)

      (if padlen
          (concat "---" (make-string (- padlen 3) ?\x20)) ; XXX
        "---"))))

(defun xwem-worklog-insert-task-list ()
  "Insert into pause buffer list of registered tasks and their values."
  (let* ((noff 10)
         (mlen (or (and xwem-worklog-task-list
                        (+ (apply 'max
                                  (mapcar
                                   #'(lambda (el)
                                       (length (xwem-worklog-task-name el)))
                                   xwem-worklog-task-list))
                           noff))
                   20))
         (task-hdr "Task"))
    (insert task-hdr)
    (insert (make-string (- mlen (length task-hdr)) ?\x20))
    (insert "Today Time    Total time\n")
    (insert (make-string (length task-hdr) ?-))
    (insert (make-string (- mlen (length task-hdr)) ?\x20))
    (insert "----- ----    ----- ----\n")

    (mapc #'(lambda (task)
              (if (eq task xwem-worklog-current-task)
                  (insert-face (xwem-worklog-task-name task) 'bold)
                (insert (xwem-worklog-task-name task)))

              (move-to-column mlen t)

              ;; Insert today/total time
              (insert (xwem-worklog-today-time-string task 14))
              (insert (xwem-worklog-total-time-string task 10))
              (insert "\n"))
          (xwem-worklog-sorted-task-list))

    ;; Insert grand total
    (let ((tl (copy-list xwem-worklog-task-list)))
      ;; Remove 'login', 'logout' tasks from task list
      (setq tl (delete* nil tl
                        :test (lambda (e1 e2)
                                (or (string= (xwem-worklog-task-name e2)
                                             "login")
                                    (string= (xwem-worklog-task-name e2)
                                             "logout")))))

      (insert (concat (make-string (+ 14 10 mlen) ?-) "\n"))
      (insert "Grand Total:")
      (move-to-column mlen t)
      (insert (xwem-worklog-format-time
               (apply 'xwem-worklog-time-sum
                      (mapcar 'xwem-worklog-get-today-time tl)) 14))
      (insert (xwem-worklog-format-time
               (apply 'xwem-worklog-time-sum
                      (mapcar 'xwem-worklog-get-total-time tl)) 10))

      (insert "\n"))
    ))

(defun xwem-worklog-insert-current-task ()
  "Insert state of current task into pause buffer."
  (if (not xwem-worklog-current-task)
      (insert "  No active task.\n")

    ;; Got active task
    (insert "  Current task: ")
    (insert-face (xwem-worklog-task-name xwem-worklog-current-task) 'bold)
    (insert "\n")

    (unless (cdr (car (xwem-worklog-task-times xwem-worklog-current-task)))
      ;; Task not paused
      (insert (format "  Task time:    %s\n"
                      (xwem-worklog-last-time-string
                       xwem-worklog-current-task 20)))
      (insert (format "  Today time:   %s\n"
                      (xwem-worklog-today-time-string
                       xwem-worklog-current-task 20)))
      (insert (format "  Total time:   %s\n"
                      (xwem-worklog-total-time-string
                       xwem-worklog-current-task 20)))
      )))

(defun xwem-worklog-time-> (a-time b-time)
  "Return non-nil if A-TIME is >= than B-TIME."
  (or (> (nth 0 a-time) (nth 0 b-time))
      (and (= (nth 0 a-time) (nth 0 b-time))
           (>= (nth 1 a-time) (nth 1 b-time)))))

(defun xwem-worklog-time-< (a-time b-time)
  "Return non-nil if A-TIME is <= than B-TIME."
  (or (< (nth 0 a-time) (nth 0 b-time))
      (and (= (nth 0 a-time) (nth 0 b-time))
           (<= (nth 1 a-time) (nth 1 b-time)))))

;; Sorting
(define-xwem-command xwem-worklog-sort-by-name ()
  "Sort tasks by name."
  (xwem-interactive)
  (setq xwem-worklog-sort-type 'name))

(define-xwem-command xwem-worklog-sort-by-Name ()
  "Sort tasks by name."
  (xwem-interactive)
  (setq xwem-worklog-sort-type 'Name))

(define-xwem-command xwem-worklog-sort-by-today-time ()
  "Sort tasks by today time."
  (xwem-interactive)
  (setq xwem-worklog-sort-type 'today-time))

(define-xwem-command xwem-worklog-sort-by-Today-time ()
  "Sort tasks by Today time."
  (xwem-interactive)
  (setq xwem-worklog-sort-type 'Today-time))

(define-xwem-command xwem-worklog-sort-by-total-time ()
  "Sort tasks by total time."
  (xwem-interactive)
  (setq xwem-worklog-sort-type 'total-time))

(define-xwem-command xwem-worklog-sort-by-Total-time ()
  "Sort tasks by Total time."
  (xwem-interactive)
  (setq xwem-worklog-sort-type 'Total-time))

;; Worklog notification facilities
(defvar xwem-worklog-notifier-timer nil
  "itimer used to notify you.
Internal variable, DO NOT MODIFY.")

(defvar xwem-worklog-auto-timer nil
  "itimer used to autologout or to stop login notifier.
Internal variable, DO NOT MODIFY.")

(defun xwem-worklog-notifier-stop ()
  "Login notifier stopper."
  (when (itimerp xwem-worklog-auto-timer)
    (delete-itimer xwem-worklog-auto-timer)
    (setq xwem-worklog-auto-timer nil))

  (when (itimerp xwem-worklog-notifier-timer)
    (delete-itimer xwem-worklog-notifier-timer)
    (setq xwem-worklog-notifier-timer nil)))

;;;###autoload(autoload 'xwem-worklog-login "xwem-worklog" nil t)
(define-xwem-command xwem-worklog-login ()
  "Stop login notifier, start logout notifier."
  (xwem-interactive)

  (xwem-worklog-notifier-stop)

  ;; Install logout notifier
  (setq xwem-worklog-notifier-timer
        (xwem-worklog-today-start-at
         xwem-worklog-day-ends 0
         'xwem-worklog-logout-notifier
         (* 60 xwem-worklog-logout-notify-period)))

  ;; Recalculate today time for every task
  (mapc #'xwem-worklog-task-update-today-time xwem-worklog-task-list)

  (xwem-worklog-begin-task "login")
  (setq xwem-worklog-logged-in t)

  (run-hooks 'xwem-worklog-login-hook))

;;;###autoload(autoload 'xwem-worklog-logout "xwem-worklog" nil t)
(define-xwem-command xwem-worklog-logout ()
  "Stop logout notifier, start login notifier."
  (xwem-interactive)

  (xwem-worklog-notifier-stop)

  ;; Install login notifier
  (setq xwem-worklog-notifier-timer
        (xwem-worklog-tomorow-start-at
         xwem-worklog-day-start 0
         'xwem-worklog-login-notifier
         (* 60 xwem-worklog-login-notify-period)))

  (xwem-worklog-begin-task "logout")
  (setq xwem-worklog-logged-in nil)

  (run-hooks 'xwem-worklog-logout-hook))

(defun xwem-worklog-login-notifier ()
  "Notify that you must login."
  (unless (itimerp xwem-worklog-auto-timer)
    ;; Start login notify stopper
    (setq xwem-worklog-auto-timer
          (start-itimer "xwem-worklog-login-notify-stopper"
                        'xwem-worklog-notifier-stop
                        (* 60 xwem-worklog-login-stop-period))))

  (xwem-message 'alarm "[WORKLOG] Workday started, but you're not logged in."))

(defun xwem-worklog-logout-notifier ()
  "Notify that you need to logout."
  (unless (itimerp xwem-worklog-auto-timer)
    ;; Start autologouter
    (setq xwem-worklog-auto-timer
          (start-itimer "xwem-worklog-autologout"
                        'xwem-worklog-logout
                        (* 60 xwem-worklog-logout-auto-period))))

  (xwem-message 'alarm "[WORKLOG] Workday ended, but you're still working."))

(defun xwem-worklog-doat (time func &optional restart)
  "Run function FUNC at a given TIME.
Return itimer handler."
  (let ((ctime (current-time))
        rtime)
    (when (xwem-worklog-time-> time ctime)
      (setq rtime (xwem-worklog-time-diff time ctime))
      (setq rtime (+ (* (float (car rtime)) 65536)
                     (cadr rtime)))

      (start-itimer "doat" func rtime restart))
    ))

(defun xwem-worklog-tomorow-start-at (hour min fun &optional restart)
  "Run function FUN tomorow at HOUR and MIN."
  (let ((ctime (decode-time (current-time))))
    (setcar ctime 0)
    (setcar (cdr ctime) min)
    (setcar (cddr ctime) hour)
    (setcar (cdddr ctime) (1+ (nth 3 ctime)))

    (xwem-worklog-doat (apply 'encode-time ctime) fun restart)))

(defun xwem-worklog-today-start-at (hour min fun &optional restart)
  "Run function FUN today at HOUR and MIN."
  (let ((ctime (decode-time (current-time))))
    (setcar ctime 0)
    (setcar (cdr ctime) min)
    (setcar (cddr ctime) hour)

    (xwem-worklog-doat (apply 'encode-time ctime) fun restart)))

;; Diagram drawing
(define-xwem-face xwem-worklog-temp-face
  `((t (:foreground "black")))
  "Temporary face used by worklog.")

(defun xwem-worklog-show-color-bricks ()
  "Show color bricks."
  (when (eq (xwem-worklog-pause-type xwem-worklog-pause-p) 'list)
    ;; Do it only in listing
    (let* ((face-height (face-height 'default))
           (w xwem-worklog-bricks-width)
           (y (- (* 5 face-height) xwem-worklog-bricks-width))
           (x xwem-worklog-bricks-offset))
      (mapc #'(lambda (task)
                (xwem-set-face-foreground
                 'xwem-worklog-temp-face
                 (or (plist-get (cadr (xwem-worklog-lookup-description
                                       (xwem-worklog-task-name task))) :color)
                     "black"))
                (xwem-diag-draw-rect
                 (xwem-worklog-pause-ppix xwem-worklog-pause-p)
                 (xwem-face-get-gc 'default)
                 (cons x y) (cons (+ x w) y)
                 (cons (+ x w) (+ y w)) (cons x (+ y w))
                 (xwem-face-get-gc 'xwem-worklog-temp-face))
                (setq y (+ y face-height)))
            (xwem-worklog-sorted-task-list)))
    (XFlush (xwem-dpy))))

(defun xwem-worklog-generate-percentage-spec
  (sector-width &optional no-labels no-yoff)
  "Generates percentage diagram spec.
If NO-LABELS is non-nil, labels will be avoided."
  (let* ((today-seconds
	  (* 60.0 60
	     (if (> xwem-worklog-day-ends xwem-worklog-day-start)
		 (- xwem-worklog-day-ends xwem-worklog-day-start)
	       (- (+ 24 xwem-worklog-day-ends)
		  xwem-worklog-day-start))))
         (spec1 (mapcar
                 #'(lambda (task)
                     (let* ((td (xwem-worklog-lookup-description
                                 (xwem-worklog-task-name task)))
                            (tt (xwem-worklog-get-today-time task))
                            (ts (+ (* (car tt) 65536.0) (cadr tt)))
                            (per (truncate (* 100.0 (/ ts today-seconds))))
                            (rv (and td (> per 0)
                                     (vector
                                      per (not no-labels)
                                      (plist-get (cadr td) :color) 0 0
                                      (if (and (not no-yoff)
                                               (eq task
                                                   xwem-worklog-current-task))
                                          (- (/ sector-width 2))
                                        0)))))
                       rv))
                 xwem-worklog-task-list))
         spec)

    ;; Remove invalid fields
    (while spec1
      (when (car spec1)
        (setq spec (cons (car spec1) spec)))
      (setq spec1 (cdr spec1)))
    spec))

(defun xwem-worklog-draw-today-diagram ()
  "Draw stuff for today."
  (when (eq (xwem-worklog-pause-type xwem-worklog-pause-p) 'list)
    ;; Do it only in listing
    (let* ((buf-lines (with-current-buffer
                          (xwem-worklog-pause-pbuf xwem-worklog-pause-p)
                        (count-lines (point-min) (point-max))))
           (face-height (font-height (face-font 'default)))
           (t-off 1)                    ; XXX
           (y-off (* face-height (+ t-off buf-lines)))
           (x-off 50)
           (sec-hei 20)
           (wwid (- xwem-worklog-pwin-width (* x-off 2)))
           (whei (- xwem-worklog-pwin-height
                    y-off (* face-height t-off) sec-hei))
           (spec (xwem-worklog-generate-percentage-spec sec-hei)))

      (when spec
        (xwem-diag-draw-percentage
         xwem-worklog-diagram-type spec
         (xwem-worklog-pause-ppix xwem-worklog-pause-p)
         (xwem-face-get-gc 'default) x-off y-off wwid whei sec-hei)))))

(defvar xwem-worklog-dockapp-map
  (let ((map (make-sparse-keymap)))
    (define-key map [button1] 'xwem-worklog-task-list)
    (define-key map [button3] 'xwem-worklog-dockapp-menu)
    map)
  "Keymap used by worklog dockapp.")

(defstruct xwem-worklog-dockapp
  win                                   ; X Window for dockapp
  mask                                  ; Mask Pixmap for dockapp
  pixmap                                ; Pixmap for storer
  update-itimer                         ; itimer to update worklog-dockapp

  ;; dockapp and sector geometry
  width height
  sector-width)

(defconst xwem-worklog-dockapp-event-mask
  (list XM-Exposure XM-StructureNotify XM-ButtonPress XM-ButtonRelease))

(defvar xwem-worklog-default-dockapp nil
  "Default worklog dockapp.")

(defun xwem-worklog-meaning-update-time ()
  "Return seconds."
  (/ (* 60 60 (- (+ (if (> xwem-worklog-day-start xwem-worklog-day-ends) 24 0)
                    xwem-worklog-day-ends)
                 xwem-worklog-day-start))
     xwem-worklog-dockapp-update-rate))

;; TODO:
;;   - Get rid of double call to `xwem-worklog-generate-percentage-spec'
;;   - Implement separated update for current task brick
(define-xwem-deferred xwem-worklog-dockapp-apply-pixmap (&optional dockapp)
  "Apply DOCKAPP's pixmap storer to real window."
  (unless dockapp (setq dockapp xwem-worklog-default-dockapp))
  (XCopyArea (xwem-dpy) (xwem-worklog-dockapp-pixmap dockapp)
             (xwem-worklog-dockapp-win dockapp) (XDefaultGC (xwem-dpy))
             0 0 (X-Pixmap-width (xwem-worklog-dockapp-pixmap dockapp))
             (X-Pixmap-height (xwem-worklog-dockapp-pixmap dockapp))
             0 0))

(define-xwem-deferred xwem-worklog-dockapp-update-task-brick (&optional dockapp)
  "Update DOCKAPP's current task brick."
  (unless dockapp (setq dockapp xwem-worklog-default-dockapp))

  (when (and xwem-worklog-current-task
	     (xwem-worklog-lookup-description
	      (xwem-worklog-task-name xwem-worklog-current-task)))
    (xwem-set-face-foreground
     'xwem-worklog-temp-face
     (or (plist-get (cadr (xwem-worklog-lookup-description
			   (xwem-worklog-task-name xwem-worklog-current-task)))
		    :color) "black"))
    (XFillRectangle (xwem-dpy) (xwem-worklog-dockapp-pixmap dockapp)
		    (xwem-face-get-gc 'xwem-worklog-temp-face) 0 0 6 6)
    (xwem-worklog-dockapp-apply-pixmap dockapp)))

(define-xwem-deferred xwem-worklog-dockapp-update (&optional dockapp)
  "Update worklog dockapp."
  (unless dockapp (setq dockapp xwem-worklog-default-dockapp))
  (when (xwem-worklog-dockapp-p dockapp)
    (let* ((win (xwem-worklog-dockapp-win dockapp))
           (xdpy (X-Win-dpy win))
           (pix (xwem-worklog-dockapp-pixmap dockapp))
           (mask (xwem-worklog-dockapp-mask dockapp))
           (w (xwem-worklog-dockapp-width dockapp))
           (h (xwem-worklog-dockapp-height dockapp))
           (sec-w (xwem-worklog-dockapp-sector-width dockapp))
           (spec (xwem-worklog-generate-percentage-spec sec-w t t))
           (spec-copy (xwem-worklog-generate-percentage-spec sec-w t t))
           td)

      (XFillRectangle xdpy pix (XDefaultGC xdpy)
                      0 0 (X-Pixmap-width pix) (X-Pixmap-height pix))
      (XFillRectangle xdpy mask xwem-misc-mask-bgc
                      0 0 (+ 1 w) (+ 1 h (* 2 sec-w)))
      (when spec
        (xwem-diag-draw-percentage
         xwem-worklog-dockapp-diagram-type
         spec pix (xwem-face-get-gc 'default)
         (/ sec-w 2) (/ sec-w 2) w h sec-w)

        (xwem-diag-draw-percentage
         xwem-worklog-dockapp-diagram-type
         spec-copy mask xwem-misc-mask-fgc
         (/ sec-w 2) (/ sec-w 2) w h sec-w nil nil
         xwem-misc-mask-fgc))

      ;; Draw a task brick
      (when (and xwem-worklog-current-task
                 (setq td (xwem-worklog-lookup-description
                           (xwem-worklog-task-name xwem-worklog-current-task))))
        (xwem-set-face-foreground 'xwem-worklog-temp-face
                                  (or (plist-get (cadr td) :color) "black"))
        (XFillRectangle
	 xdpy pix (xwem-face-get-gc 'xwem-worklog-temp-face) 0 0 6 6)
        (XFillRectangle xdpy mask xwem-misc-mask-fgc 0 0 6 6))

      ;; Apply mask
      (X-XShapeMask xdpy win X-XShape-Bounding X-XShapeSet 0 0 mask)
      (xwem-worklog-dockapp-apply-pixmap dockapp))))

(defun xwem-worklog-dockapp-event-handler (xdpy xwin xev)
  "Event handler for worklog dockapp."
  (let ((dockapp (X-Win-get-prop xwin 'xwem-worklog-dockapp)))
    (when (xwem-worklog-dockapp-p dockapp)
      (X-Event-CASE xev
        (:X-MapNotify (xwem-worklog-dockapp-update dockapp))
        (:X-Expose (xwem-worklog-dockapp-apply-pixmap dockapp))

        ((:X-ButtonPress :X-ButtonRelease)
         (xwem-overriding-local-map xwem-worklog-dockapp-map
           (xwem-dispatch-command-xevent xev)))

        (:X-DestroyNotify
         (xwem-worklog-dockapp-stop dockapp))))))

;;;###autoload
(defun xwem-worklog-start-dockapp (&optional dockid dockgroup dockalign)
  "Start xwem worklog dockapp."
  (let* ((w xwem-worklog-dockapp-width)
         (h xwem-worklog-dockapp-height)
         (sw xwem-worklog-dockapp-sector-width)
         (wd (make-xwem-worklog-dockapp
              :win (XCreateWindow
                    (xwem-dpy) nil 0 0 (+ 1 w) (+ 1 h (* 2 sw))
                    0 nil nil nil
                    :override-redirect t
                    :background-pixel
                    (XAllocNamedColor
                     (xwem-dpy) (XDefaultColormap (xwem-dpy))
                     (face-background-name 'default))) ; XXX
              :width w :height h :sector-width sw)))

    ;; Create mask for worklog dockapp
    (setf (xwem-worklog-dockapp-mask wd)
          (XCreatePixmap (xwem-dpy) (xwem-worklog-dockapp-win wd)
                         1 (+ 1 w) (+ 1 h (* 2 sw))))

    ;; Create storer pixmap
    (setf (xwem-worklog-dockapp-pixmap wd)
          (XCreatePixmap (xwem-dpy) (xwem-worklog-dockapp-win wd)
                         (XDefaultDepth (xwem-dpy)) (+ 1 w) (+ 1 h (* 2 sw))))

    (X-Win-put-prop (xwem-worklog-dockapp-win wd) 'xwem-worklog-dockapp wd)

    ;; Draw initial contents
    (xwem-worklog-dockapp-update wd)

    ;; Install turbo mode
    (when xwem-misc-turbo-mode
      (XSetWindowBackgroundPixmap (xwem-dpy) (xwem-worklog-dockapp-win wd)
                                  (xwem-worklog-dockapp-pixmap wd)))

    ;; Select for incoming events
    (XSelectInput (xwem-dpy) (xwem-worklog-dockapp-win wd)
                  (apply 'Xmask-or xwem-worklog-dockapp-event-mask))
    (X-Win-EventHandler-add
     (xwem-worklog-dockapp-win wd) 'xwem-worklog-dockapp-event-handler nil
     (list X-Expose X-MapNotify X-ButtonPress X-ButtonRelease X-DestroyNotify))

    ;; Initialize wd in sys tray
    (xwem-XTrayInit (xwem-dpy) (xwem-worklog-dockapp-win wd)
                    dockid (or dockgroup "desktop") dockalign)

    ;; Start updater
    (setf (xwem-worklog-dockapp-update-itimer wd)
          (start-itimer "xwem-worklog-dockapp-updater"
                        'xwem-worklog-dockapp-update
                        (xwem-worklog-meaning-update-time)
                        (xwem-worklog-meaning-update-time)
                        t wd))

    ;; Set default worklog dockapp
    (unless xwem-worklog-default-dockapp
      (setq xwem-worklog-default-dockapp wd))

    ;; Add hooks
    (add-hook 'xwem-worklog-task-start-hook
              'xwem-worklog-dockapp-update-task-brick t)
    (add-hook 'xwem-worklog-login-hook
              'xwem-worklog-dockapp-update t)
    (add-hook 'xwem-worklog-logout-hook
              'xwem-worklog-dockapp-update t)
    wd))

(defun xwem-worklog-dockapp-stop (dockapp &optional need-destroy)
  "Stop worklog dockapp."
  (when (xwem-worklog-dockapp-p dockapp)
    (when (eq dockapp xwem-worklog-default-dockapp)
      (setq xwem-worklog-default-dockapp nil))

    (remove-hook 'xwem-worklog-task-start-hook 'xwem-worklog-dockapp-update-task-brick)
    (remove-hook 'xwem-worklog-login-hook 'xwem-worklog-dockapp-update)
    (remove-hook 'xwem-worklog-logout-hook 'xwem-worklog-dockapp-update)

    (delete-itimer (xwem-worklog-dockapp-update-itimer dockapp))
    (X-Win-rem-prop (xwem-worklog-dockapp-win dockapp) 'xwem-worklog-dockapp)

    ;; Release resources
    (XFreePixmap (xwem-dpy) (xwem-worklog-dockapp-mask dockapp))
    (XFreePixmap (xwem-dpy) (xwem-worklog-dockapp-pixmap dockapp))

    ;; Remove events handler
    (X-Win-EventHandler-rem (xwem-worklog-dockapp-win dockapp)
                            'xwem-worklog-dockapp-event-handler)
    (when need-destroy
      (XDestroyWindow (xwem-dpy) (xwem-worklog-dockapp-win dockapp)))

    (X-invalidate-cl-struct dockapp)))

(define-xwem-command xwem-worklog-task-info (task)
  "Show info about TASK."
  (xwem-interactive (list xwem-worklog-current-task))
  (xwem-message 'info "Task: '%s', running %s"
                (xwem-worklog-task-name task)
                (xwem-worklog-last-time-string task 20)))

(define-xwem-command xwem-worklog-dockapp-menu ()
  "Popup menu for dockapp."
  (xwem-interactive)

  (unless (button-event-p xwem-last-event)
    (error 'xwem-error
           "`xwem-worklog-dockapp-menu' must be bound to mouse event"))

  (xwem-popup-menu
   (list "Worklog"
         (vector "Current Task Info"
                 `(xwem-worklog-task-info ,xwem-worklog-current-task))
         (cons "Start task"
               (mapcar #'(lambda (td)
                           (vector
                            (car td) `(xwem-worklog-begin-task ,(car td))))
                       xwem-worklog-tasks-description))
         "---"
         (vector "Pause" `(xwem-worklog-pause nil))
         (vector "List tasks" `(xwem-worklog-task-list nil))
         "---"
         (vector "Destroy" `(xwem-worklog-dockapp-stop
                             (X-Win-get-prop
                              ,(X-Event-xbutton-event xwem-last-xevent)
                              'xwem-worklog-dockapp) t)))))

;;; MISC
(defun xwem-worklog-on-select-cl (&optional cl)
  "New CL just selected, change task if needed.
To be used in `xwem-client-select-hook'."
  (unless cl
    (setq cl (xwem-cl-selected)))

  (when (xwem-cl-selected-p cl)
    (let ((td (xwem-manda-find-match-1 cl xwem-worklog-tasks-description)))
      (when (and td (not (string= (xwem-worklog-task-name
                                   xwem-worklog-current-task) (car td))))
        (xwem-worklog-begin-task (car td))))))

(defun xwem-worklog-init ()
  "Initialise xwem worklog."
  (xwem-message 'init "Initializing worklog ...")

  ;; Add our label to label prefixes
  (add-to-list 'xwem-messages-label-prefixes
               '(worklog "Worklog"))

  (add-hook 'xwem-worklog-login-hook
            #'(lambda ()
                (add-hook 'xwem-client-select-hook 'xwem-worklog-on-select-cl)
                (add-hook 'xwem-cl-change-hook 'xwem-worklog-on-select-cl)
                (when (xwem-cl-p (xwem-cl-selected))
                  (xwem-worklog-on-select-cl))))
  (add-hook 'xwem-worklog-logout-hook
            #'(lambda ()
                (remove-hook 'xwem-client-select-hook 'xwem-worklog-on-select-cl)
                (remove-hook 'xwem-cl-change-hook 'xwem-worklog-on-select-cl)))

  (when xwem-worklog-history-mode
    (xwem-worklog-history-init))

  (when xwem-worklog-auto-login
    (xwem-worklog-login))

  (xwem-message 'init "Initializing worklog ... done"))

;;; Worklog history support
(defvar xwem-worklog-history-buffer nil)

(defun xwem-worklog-history-init ()
  "Initialise worklog history handling."
  ;; Setup worklog buffer
  (let ((find-file-hooks nil))          ; supress any minor modes and stuff
    (setq xwem-worklog-history-buffer
          (find-file-noselect
           (expand-file-name xwem-worklog-file xwem-dir))))

  (with-current-buffer xwem-worklog-history-buffer
    (goto-char (point-max))
    (unless (bolp)
      (newline)
      (set-buffer-modified-p nil)))

  ;; Read some worklog entries from history file
  (xwem-worklog-history-read xwem-worklog-history-lines)

  ;; Install hooks
  (add-hook 'xwem-worklog-task-resume-hook 'xwem-worklog-history-on-start)
  (add-hook 'xwem-worklog-task-pause-hook 'xwem-worklog-history-on-stop)

  (add-hook 'xwem-exit-hook 'xwem-worklog-logout))

(defvar xwem-worklog-date-regex
  "\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)")

(defun xwem-worklog-make-date-time (&optional time)
  "Makes the worklog timestamp."
  (format-time-string "%Y-%m-%d %H:%M:%S" time))

(defun xwem-worklog-history-add-entry (string &optional time)
  "Add entry to histroy buffer."
  (with-current-buffer xwem-worklog-history-buffer
    (goto-char (point-max))
    (insert (xwem-worklog-make-date-time time) " " string "\n")
    (set-buffer-modified-p nil)))

(defun xwem-worklog-history-read-line ()
  "Read current worklog line."
  (declare (special xwem-worklog-rtask))

  (when (looking-at (concat "^" xwem-worklog-date-regex " \\(.*\\)"))
    ;; Got task
    (let ((Y (string-to-int (match-string 1)))
          (M (string-to-int (match-string 2)))
          (D (string-to-int (match-string 3)))
          (h (string-to-int (match-string 4)))
          (m (string-to-int (match-string 5)))
          (s (string-to-int (match-string 6)))
          (task-name (match-string 7))
          time)
      (setq time (encode-time s m h D M Y))

      (cond ((or (string= task-name "login")
                 (string= task-name "logout"))) ; skip them

            ((and (string= task-name "stop")
                  xwem-worklog-rtask)
             (xwem-worklog-pause-task xwem-worklog-rtask time))

            (t
             ;; Start new task
             (setq xwem-worklog-rtask
                   (xwem-worklog-find-task task-name t))
             (xwem-worklog-resume-task xwem-worklog-rtask time)))
      )))

(defun xwem-worklog-history-read (&optional lines)
  "Read worklog history file for today tasks."
  (unless lines
    (setq lines 1000))

  (with-current-buffer xwem-worklog-history-buffer
    (save-excursion
      (goto-char (point-max))
      (condition-case nil
          (previous-line lines)
        (t nil))

      (when (looking-at (concat "^" xwem-worklog-date-regex " " "stop"))
        (forward-line 1))

      (let (xwem-worklog-rtask)
        (declare (special xwem-worklog-rtask))
        (while (not (eobp))
          (xwem-worklog-history-read-line)
          (forward-line 1)))
      )))

(defun xwem-worklog-history-save ()
  "Save worklog history to worklog file."
  (with-current-buffer xwem-worklog-history-buffer
    (set-buffer-modified-p t)
    (save-buffer)))

(defun xwem-worklog-history-on-start ()
  "Called when new task just started.
To be used in `xwem-worklog-task-start-hook'."
  (xwem-worklog-history-add-entry
   (xwem-worklog-task-name (xwem-worklog-current-task)))
  (when (string= (xwem-worklog-task-name (xwem-worklog-current-task))
                 "logout")
    ;; When logging out, also save history
    (xwem-worklog-history-save)))

(defun xwem-worklog-history-on-stop ()
  "Called when task just stoped.
To be used in `xwem-worklog-task-stop-hook'."
  (unless (or (string= (xwem-worklog-task-name (xwem-worklog-current-task))
                       "logout")
              (string= (xwem-worklog-task-name (xwem-worklog-current-task))
                       "login"))
    (xwem-worklog-history-add-entry "stop")))


;;;; On-load actions:
;; - Add custom bindings
(mapc #'(lambda (el)
          (let ((key (plist-get (cadr el) :key)))
            (when key
              (define-key xwem-worklog-map key
                (xwem-worklog-create-cmd (car el))))))
      xwem-worklog-tasks-description)

;; - Initialize worklog
(xwem-worklog-init)


(run-hooks 'xwem-worklog-load-hook)

(provide 'xwem-worklog)

;;; xwem-worklog.el ends here
