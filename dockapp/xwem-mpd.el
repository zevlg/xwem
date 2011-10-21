;;; xwem-mpd.el --- Dock application to interact with MusicPD.

;; Copyright (C) 2005-2007 by XWEM Org.

;; Author: Richard Klinda <ignotus@freemail.hu>
;;         Zajcev Evgeny <zevlg@yandex.ru>
;; Created: 2004
;; Keywords: xwem, music, entertainment

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

;; You need MusicPD - Music Playing Daemon
;; (http://musicpd.sourceforge.net/) to be setuped and running.

;; Install
;;  - Debian: apt-get install mpd
;;  - FreeBSD: pkg_add -r musicpd

;; To start using xwem-mpd add:

;;    (add-hook 'xwem-after-init-hook 'xwem-mpd)

;; to your xwemrc, or just execute `xwem-mpd' as ordinary Emacs
;; command.

;;; Code:

(require 'xwem-osd)

(eval-when-compile
  (autoload 'google-query "google-query" nil t))

(defgroup xwem-mpd nil
  "Group to customize mpd."
  :prefix "xwem-"
  :group 'xwem)

(defcustom xwem-mpd-update-rate 5
  "MPD variables updating rate in seconds."
  :type 'number
  :group 'xwem-mpd)

(defcustom xwem-mpd-lyrics-dir
  (file-name-as-directory
   (expand-file-name "~/musicpd/lyrics"))
  "Directory containing songs lyrics."
  :type 'directory
  :group 'xwem-mpd)

(defcustom xwem-mpd-osd-size '(120 . 30)
  "Width and height of mpd dockapp."
  :type '(cons (number :tag "Width")
               (number :tag "Height"))
  :group 'xwem-mpd)

(defcustom xwem-mpd-osd-setup
  '((artist :place top :depth 2 :font "7x14bold" :color "peru"
            :format (concat (cond (**mpd-var-Album* **mpd-var-Album*)
                                  (**mpd-var-Name* **mpd-var-Name*)) 
                            (when **mpd-var-Date*
                              (concat ", " **mpd-var-Date*))))
    (bar :place middle :depth 2 :width 2 :color "red")
    (ilove :place left :depth 0 :icon "mini-heart.xpm")
    (track :place bottom :depth 2 :font "7x14bold" :color "yellow"
           :format (format "%s%s" (if **mpd-var-Track*
                                      (concat **mpd-var-Track* ". ")
                                    "") **mpd-var-Title*))
    (lyrics :place bottom-right :depth 0 :icon "mini-lyrics.xpm")
    (volume :place right :depth 1 :font "fixed" :color "white")
    (state :place center :depth 1
           :font "-*-helvetica-bold-r-*-*-20-*-*-*-*-*-*-*"
           :color "red3" :format (or (and (mpd-stopped-p) "STOPPED")
				     (and (mpd-paused-p) "PAUSED")
				     (and (mpd-muted-p) "MUTE"))))
  "Mpd dockapp configuration.
:place is not yet implemented."
  :type 'list
  :group 'xwem-mpd)

(defcustom mpd-after-command-hook nil
  "Hooks to run after MPD command is executed.
Executed command name stored in `mpd-this-command'."
  :type 'hook
  :group 'xwem-mpd)

(defcustom mpd-before-variables-update-hook nil
  "Hooks to run before updating mpd variables."
  :type 'hook
  :group 'xwem-mpd)

(defcustom mpd-after-variables-update-hook nil
  "Hooks to run after mpd variables has been updated."
  :type 'hook
  :group 'xwem-mpd)


(defvar mpd-process nil)
(defvar mpd-itimer nil)

(defun mpd-start-connection ()
  "Open connection to MusicPD daemon.
Set `mpd-process' by side effect."
  (when (or (not mpd-process)
            (not (eq mpd-process 'open)))
    (setq mpd-process (open-network-stream "mpd" " *mpd connection*"
                                           "localhost" 6600))
    (when (fboundp 'set-process-coding-system)
      (set-process-coding-system mpd-process 'utf-8 'utf-8))
    (set-process-filter mpd-process 'mpd-process-filter)
    (set-process-sentinel mpd-process 'mpd-process-sentinel)
    (process-kill-without-query mpd-process)

    (add-hook 'mpd-after-command-hook 'mpd-update-variables)
    (setq mpd-itimer
          (start-itimer "mpd-vars-update" #'mpd-update-variables
                        xwem-mpd-update-rate xwem-mpd-update-rate))))

(defun mpd-stop-connection ()
  "Close connection to MusicPD daemon."
  (when mpd-process
    ;; It also will delete `mpd-itimer'
    (mpd-process-sentinel mpd-process)))

;; mpd variables
(defvar mpd-zero-vars-p t)
(defvar mpd-status-update-p nil)

(defvar **mpd-var-* nil)
(defvar **mpd-var-Name* nil)
(defvar **mpd-var-Album* nil)
(defvar **mpd-var-Artist* nil)
(defvar **mpd-var-Date* nil)
(defvar **mpd-var-Genre* nil)
(defvar **mpd-var-Id* nil)
(defvar **mpd-var-Pos* nil)
(defvar **mpd-var-Time* nil)
(defvar **mpd-var-Title* nil)
(defvar **mpd-var-Track* nil)
(defvar **mpd-var-ate* nil)
(defvar **mpd-var-audio* nil)
(defvar **mpd-var-aylistlength* nil)
(defvar **mpd-var-bitrate* nil)
(defvar **mpd-var-ck* nil)
(defvar **mpd-var-de* nil)
(defvar **mpd-var-dom* nil)
(defvar **mpd-var-e* nil)
(defvar **mpd-var-file* nil)
(defvar **mpd-var-g* nil)
(defvar **mpd-var-h* nil)
(defvar **mpd-var-itle* nil)
(defvar **mpd-var-length* nil)
(defvar **mpd-var-ndom* nil)
(defvar **mpd-var-olume* nil)
(defvar **mpd-var-om* nil)
(defvar **mpd-var-peat* nil)
(defvar **mpd-var-playlist* nil)
(defvar **mpd-var-playlistlength* nil)
(defvar **mpd-var-random* nil)
(defvar **mpd-var-repeat* nil)
(defvar **mpd-var-s* nil)
(defvar **mpd-var-song* nil)
(defvar **mpd-var-songid* nil)
(defvar **mpd-var-state* nil)
(defvar **mpd-var-stlength* nil)
(defvar **mpd-var-te* nil)
(defvar **mpd-var-time* nil)
(defvar **mpd-var-volume* nil)
(defvar **mpd-var-xfade* nil)

(defvar mpd-pre-mute-volume nil
  "Holds the value of `**mpd-var-volume* prior to muting.
The purpose of this is so that when you unmute, it goes back to the
volume you had it set to before you muted.")

(defvar mpd-this-command nil
  "The mpd command currently executing.
Useful to use in `mpd-after-command-hook' hooks.")

(defmacro define-mpd-command (cmd args &rest body)
  "Define new mpd command."
  `(defun ,cmd ,args
     ,@body
     (let ((mpd-this-command ',cmd))
       (run-hooks 'mpd-after-command-hook))))

(defun mpd-send (format &rest args)
  "Send formated string into connection.
FORMAT and ARGS are passed directly to `format' as arguments."
  (let ((string (concat (apply #'format format args) "\n")))
    (case (if mpd-process
              (process-status mpd-process)
            'closed)
      (open (process-send-string mpd-process string))
      (closed (mpd-start-connection)
              (process-send-string mpd-process string)))))

(defun mpd-stopped-p ()
  (string= **mpd-var-state* "stop"))
(defun mpd-paused-p ()
  (string= **mpd-var-state* "pause"))
(defun mpd-muted-p ()
  (zerop (string-to-number **mpd-var-volume*)))

;; (mpd-songpos)
(defun mpd-songpos ()
  (if **mpd-var-time*
      (destructuring-bind (a b)
          (mapcar #'string-to-int (split-string **mpd-var-time* ":"))
        (cond ((and (zerop a) (zerop b)) (cons 1 1))
              ((zerop b) (cons a a))
              (t (cons a b))))
    (cons 0 1)))                        ; todo?

(define-xwem-command mpd-volume-up (step)
  "Increase the volume by STEP increments.
STEP can be given via numeric prefix arg and defaults to 1 if omitted."
  (xwem-interactive "p")
  (let* ((oldvol (string-to-number **mpd-var-volume*))
	 (newvol (+ oldvol step))
	 (mpd-this-command 'mpd-volume-down))
    (when (>= newvol 100)
      (setq newvol 100))
    (mpd-send "setvol %d" newvol)
    (run-hooks 'mpd-after-command-hook)))

(define-xwem-command mpd-volume-down (step)
  "Decrease the volume by STEP increments.
STEP can be given via numeric prefix arg and defaults to 1 if omitted."
  (xwem-interactive "p")
  (let* ((oldvol (string-to-number **mpd-var-volume*))
	 (newvol (- oldvol step))
	 (mpd-this-command 'mpd-volume-down))
    (when (<= newvol 0)
      (setq newvol 0))
    (mpd-send "setvol %d" newvol)
    (run-hooks 'mpd-after-command-hook)))

(define-xwem-command mpd-volume-mute (&optional unmute)
  "Mute the volume.
With prefix arg, UNMUTE, let the tunes blast again."
  (xwem-interactive "P")
  (if unmute
      (mpd-send "setvol %s" mpd-pre-mute-volume)
    (setq mpd-pre-mute-volume **mpd-var-volume*)
    (mpd-send "setvol 0"))
  (let ((mpd-this-command 'mpd-volume-mute))
    (run-hooks 'mpd-after-command-hook)))

(define-mpd-command mpd-volume-max ()
  "Set volume to maximum."
  (interactive)
  (mpd-send "setvol 100"))

(define-mpd-command mpd-volume-min ()
  "Set volume to minimum.
Sets state to \"muted\" by side effect."
  (interactive)
  (setq mpd-pre-mute-volume **mpd-var-volume*)
  (mpd-send "setvol 0"))

(define-mpd-command mpd-seek (time)
  "Seek current track to TIME."
  (mpd-send "seekid %s %d" **mpd-var-Id* (+ (car (mpd-songpos)) time)))

(defun mpd-seek-forward ()
  (interactive)
  (mpd-seek 10))

(defun mpd-seek-backward ()
  (interactive)
  (mpd-seek -10))

;; Playing operations
(define-mpd-command mpd-next-track ()
  "Start playing next track."
  (interactive)
  (mpd-send "next"))

(define-mpd-command mpd-previous-track ()
  "Start playing previous track."
  (interactive)
  (mpd-send "previous"))

(define-mpd-command mpd-stop ()
  "Stop playing."
  (interactive)
  (mpd-send "stop"))

(define-mpd-command mpd-playpause ()
  "Resume playing or pause."
  (interactive)
  (if (mpd-stopped-p)
      (mpd-send "play")
    (mpd-send "pause")))

(defun mpd-process-filter (process output)
  "MPD process filter."
  (with-temp-buffer
    (insert output)
    (goto-char (point-min))
    (while (not (eobp))
      (when (looking-at "\\(.*?\\): \\(.*\\)")
        (set (intern (format "**mpd-var-%s*" (match-string 1)))
             (match-string 2)))
      (forward-line 1)))
  (when mpd-status-update-p
    (setq mpd-status-update-p nil)
    (setq mpd-zero-vars-p nil)
    (run-hooks 'mpd-after-variables-update-hook)))

(defun mpd-process-sentinel (proc &optional evstr)
  (when (itimerp mpd-itimer)
    (delete-itimer mpd-itimer))
  (setq mpd-itimer nil
        mpd-process nil)
  (delete-process proc))

(defun mpd-update-variables ()
  "Requests status information."
  (run-hooks 'mpd-before-variables-update-hook)
  (setq mpd-zero-vars-p t)
  (mpd-send "currentsong")
  (setq mpd-status-update-p t)
  (mpd-send "status"))

;;; Lyrics support
(defun mpd-lyric-filename ()
  "Return lyric filename for now playing song."
  (when **mpd-var-file*
    (expand-file-name
     (concat (replace-in-string **mpd-var-file* "\/" "--") ".txt")
     xwem-mpd-lyrics-dir)))

(defun mpd-lyric-check ()
  "Return non-nil if current track has local lyrics."
  (let ((fn (mpd-lyric-filename)))
    (and fn (file-exists-p fn))))

(defun mpd-lyric-save ()
  "Save selected lyric to lyric file."
  (interactive "_")
  (if (mpd-lyric-check)
      (message "There is already a lyric for this song")
    (let ((text (get-selection-no-error)))
      (if (not text)
          (message "You should have selected the lyric first!")
        ;; everything is ok
        (with-current-buffer (find-file-noselect (mpd-lyric-filename))
          (insert text)
          (save-buffer))))))

;; This has to allow for the possibility of being called when there
;; are no SXEmacs frames focused, so it forces a new SXEmacs frame to
;; be created.  Ugly, yes, but I couldn't think of a better way. --SY.
(define-xwem-command mpd-lyric-show ()
  "Show lyrics for now playing song."
  (xwem-interactive)
  (if (mpd-lyric-check)
      (let ((temp-buffer-show-function 'xwem-special-popup-frame)
	    (header (format "\"%s\" (by: %s)"
			    **mpd-var-Title*
			    **mpd-var-Artist*))
	    (title (format "Lyrics: %s" **mpd-var-Title*)))
	(with-output-to-temp-buffer title
	  (set-buffer standard-output)
	  (insert header "\n"
		  (make-string (length header) ?=)
		  "\n\n")
	  (insert-file-contents (mpd-lyric-filename))
	  (toggle-read-only 1)
          (view-mode nil #'(lambda (&rest not-used-buffer)
                             (delete-frame (selected-frame))))))
    (when (and **mpd-var-Artist* **mpd-var-Title*)
      (let ((lyric-frame (new-frame)))
	(select-frame lyric-frame)
	(google-query (format "\"%s\" \"%s\" lyrics"
			      **mpd-var-Artist* **mpd-var-Title*))
	(focus-frame lyric-frame)))))

(define-xwem-command mpd-i-love-this-track ()
  "Mark track as `I love this one'."
  (xwem-interactive)
  (let ((fil (expand-file-name "mpd-ilove-tracks.txt" xwem-dir)))
    (with-current-buffer (find-file-noselect fil)
      (let ((s (format "%s%s"
                       (if **mpd-var-Artist*
                         (concat **mpd-var-Artist* " - ")
                         "")
                       (if **mpd-var-Title*
                           **mpd-var-Title*
                         ""))))
        (goto-char (point-min)
        (if (search-forward s nil t)
            (xwem-message 'info "Track already in list")

          (goto-char (point-max))
          (insert s "\n")
          (save-buffer)
          (xwem-message
           'info "%s added to '%s'"
           (if **mpd-var-Title* **mpd-var-Title* "current track") fil))))
      (kill-buffer (current-buffer)))))


;;;; Dockapp section
(defvar xwem-mpd-keymap
  (let ((map (make-sparse-keymap "xwem-mpd")))
    (define-key map [button3] 'xwem-mpd-popup-menu)
    map))

(defvar xwem-mpd-osd nil)

;;;###autoload
(defun xwem-mpd ()
  "Start xwem dockapp to interact with MusicPD."
  (interactive)

  ;; Start client connection
  (mpd-start-connection)

  ;; Destroy any mpd OSD already running
  (when (xwem-osd-p xwem-mpd-osd)
    (xwem-osd-destroy xwem-mpd-osd))

  ;; Create new OSD dockapp
  (setq xwem-mpd-osd
        (xwem-osd-create-dock
         (xwem-dpy) (car xwem-mpd-osd-size) (cdr xwem-mpd-osd-size)
         (list 'keymap xwem-mpd-keymap)))
  (add-hook 'mpd-after-variables-update-hook 'xwem-mpd-osd-update t)
  (mpd-update-variables))

(defun xwem-mpd-destroy ()
  "Destroy mpd dockapp."
  (interactive)

  (mpd-stop-connection)
  (when (xwem-osd-p xwem-mpd-osd)
    (xwem-osd-destroy xwem-mpd-osd)))

(define-xwem-command xwem-mpd-popup-menu (ev)
  "Popup menu."
  (xwem-interactive (list xwem-last-event))
  (unless (button-event-p ev)
    (error 'xwem-error
           "`xwem-mpd-popup-menu' must be bound to mouse event"))
  (xwem-popup-menu
   (list "XWEM MusicPD"
         "---"
         (vector "Vol+" `(mpd-volume-up 1))
         (vector "Vol-" `(mpd-volume-down 1))
         ["Mute" mpd-volume-mute]
         ["Show Lyric" mpd-lyric-show]
         ["I Love this track" mpd-i-love-this-track]
         "---"
         ["Destroy" xwem-mpd-destroy]
         )))

(defmacro xwem-mpd-unless-in-cache (osin prop valu &rest forms)
  "Do FORMS."
  `(unless (equal (xwem-osd-instance-get-prop ,osin ',prop)
                  ,valu)
     (xwem-osd-instance-put-prop osin ',prop ,valu)
     ,@forms))
(put 'xwem-mpd-unless-in-cache 'lisp-indent-function 3)

(define-xwem-deferred xwem-mpd-osd-update ()
  "Update mpd dockapp if any."
  (when xwem-mpd-osd
    (unless mpd-zero-vars-p
      (let* ((art-plist (cdr (assq 'artist xwem-mpd-osd-setup)))
             (art-str (eval (plist-get art-plist :format)))
             (ilove-plist (cdr (assq 'ilove xwem-mpd-osd-setup)))
             (track-plist (cdr (assq 'track xwem-mpd-osd-setup)))
             (track-str (eval (plist-get track-plist :format)))
             (vol-plist (cdr (assq 'volume xwem-mpd-osd-setup)))
             (bar-plist (cdr (assq 'bar xwem-mpd-osd-setup)))
             (lyr-plist (cdr (assq 'lyrics xwem-mpd-osd-setup)))
             (st-plist (cdr (assq 'state xwem-mpd-osd-setup)))
             (y-off 0) (x-off 0) osin)

        ;; Artist, Album
        (setq osin (xwem-osd-get-prop xwem-mpd-osd 'artist-instance))
        (if (xwem-osd-instance-p osin)
            (xwem-mpd-unless-in-cache osin 'saved-artist art-str
              (xwem-osd-instance-change osin :text art-str))
          (setq osin (xwem-osd-text-add xwem-mpd-osd 0 0 art-str
                                        :depth (plist-get art-plist :depth)
                                        :color (plist-get art-plist :color)
                                        :font (plist-get art-plist :font)
                                        :keymap xwem-mpd-keymap))
          (xwem-osd-put-prop xwem-mpd-osd 'artist-instance osin))
        (setq y-off (X-Text-height (xwem-osd-xdpy xwem-mpd-osd)
                                   (X-Font-get (xwem-osd-xdpy xwem-mpd-osd)
                                               (plist-get art-plist :font))
                                   art-str))

        ;; Progress bar
        (let* ((bw (plist-get bar-plist :width))
               (x0 0) (y0 (+ y-off (/ bw 2)))
               (x1 (destructuring-bind (a . b)
                       (mpd-songpos)
                     (truncate (* (xwem-osd-width xwem-mpd-osd)
                                  (/ (float a) b)))))
               (y1 (+ y-off (/ bw 2))))
          (setq osin (xwem-osd-get-prop xwem-mpd-osd 'bar-instance))
          (if (xwem-osd-instance-p osin)
              (xwem-mpd-unless-in-cache osin 'bar-geom (vector x0 y0 x1 y1)
                (xwem-osd-instance-change osin :x0 x0 :y0 y0 :x1 x1 :y1 y1))
            (setq osin (xwem-osd-line-add
                        xwem-mpd-osd x0 y0 x1 y1 :line-width bw
                        :depth (plist-get bar-plist :depth)
                        :color (plist-get bar-plist :color)
                        :keymap xwem-mpd-keymap))
            (xwem-osd-put-prop xwem-mpd-osd 'bar-instance osin))
          (setq y-off (+ y-off bw)))

        ;; I love icon
        (setq osin (xwem-osd-get-prop xwem-mpd-osd 'ilove-instance))
        (if ilove-plist
            (if (xwem-osd-instance-p osin)
                (setq x-off
                      (+ 2 (X-Pixmap-width
                            (xwem-osd-instance-get-prop osin :pixmap))))
              (setq osin (xwem-osd-icon-file-add
                          xwem-mpd-osd
                          (xwem-icon-find-file (plist-get ilove-plist :icon))
                          :depth (plist-get ilove-plist :depth)
                          :y (+ 2 y-off)
                          :keymap (let ((map (copy-keymap xwem-mpd-keymap)))
                                    (define-key map [button1] 'mpd-i-love-this-track)
                                    map)))
              (xwem-osd-put-prop xwem-mpd-osd 'ilove-instance osin)
              (setq x-off
                    (+ 2 (X-Pixmap-width
                          (xwem-osd-instance-get-prop osin :pixmap)))))
          (when (xwem-osd-instance-p osin)
            (xwem-osd-instance-destroy osin)
            (xwem-osd-put-prop xwem-mpd-osd 'ilove-instance nil)))

        ;; Track
        (setq osin (xwem-osd-get-prop xwem-mpd-osd 'track-instance))
        (if (xwem-osd-instance-p osin)
            (xwem-mpd-unless-in-cache osin 'track (vector track-str y-off)
              (xwem-osd-instance-change osin :text track-str :x x-off :y y-off))
          (setq osin (xwem-osd-text-add xwem-mpd-osd x-off y-off track-str
                                        :depth (plist-get track-plist :depth)
                                        :color (plist-get track-plist :color)
                                        :font (plist-get track-plist :font)
                                        :keymap xwem-mpd-keymap))
          (xwem-osd-put-prop xwem-mpd-osd 'track-instance osin))

        ;; Volume
        (let* ((vol-y (if (> (length art-str) (length track-str)) y-off 0))
               (vtext (concat **mpd-var-volume* "%"))
               (vtwid (X-Text-width (xwem-osd-xdpy xwem-mpd-osd)
                                    (X-Font-get (xwem-osd-xdpy xwem-mpd-osd)
                                                (plist-get vol-plist :font))
                                    vtext)))
          (setq osin (xwem-osd-get-prop xwem-mpd-osd 'volume-instance))
          (if (xwem-osd-instance-p osin)
              (xwem-mpd-unless-in-cache osin 'volume (vector vol-y vtext vtwid)
                (xwem-osd-instance-change
                 osin :text vtext :x (- (xwem-osd-width xwem-mpd-osd) vtwid)
                 :y vol-y))
            (setq osin (xwem-osd-text-add
                        xwem-mpd-osd (- (xwem-osd-width xwem-mpd-osd) vtwid)
                        vol-y vtext
                        :depth (plist-get vol-plist :depth)
                        :color (plist-get vol-plist :color)
                        :font (plist-get vol-plist :font)
                        :keymap xwem-mpd-keymap))
            (xwem-osd-put-prop xwem-mpd-osd 'volume-instance osin)))

        ;; Lyrics
        (setq osin (xwem-osd-get-prop xwem-mpd-osd 'lyric-instance))
        (if (mpd-lyric-check)
            (progn
              (unless (xwem-osd-instance-p osin)
                (setq osin (xwem-osd-icon-file-add
                            xwem-mpd-osd
                            (xwem-icon-find-file (plist-get lyr-plist :icon))
                            :depth (plist-get lyr-plist :depth)
                            :keymap (let ((map (copy-keymap xwem-mpd-keymap)))
                                      (define-key map [button1] 'mpd-lyric-show)
                                      map)))
                (xwem-osd-put-prop xwem-mpd-osd 'lyric-instance osin))
              (let* ((pw (X-Pixmap-width (xwem-osd-instance-get-prop osin :pixmap)))
                     (li-y (if (> (length art-str) (length track-str)) 0 y-off)))
                (xwem-osd-instance-change
                 osin :x (- (xwem-osd-width xwem-mpd-osd) pw) :y li-y))
              ;; Show instance in case it is hidden
              (when (xwem-osd-instance-get-prop osin 'lyric-hidden)
                (xwem-osd-instance-show osin)
                (xwem-osd-instance-put-prop osin 'lyric-hidden nil)))
          ;; Hide instance in case it is show and no lyrics available
          (when (and (xwem-osd-instance-p osin)
                     (not (xwem-osd-instance-get-prop osin 'lyric-hidden)))
            (xwem-osd-instance-hide osin)
            (xwem-osd-instance-put-prop osin 'lyric-hidden t)))
        
        ;; State
        (if (or (mpd-stopped-p) (mpd-paused-p) (mpd-muted-p))
          (let* ((txt (eval (plist-get st-plist :format)))
                 (fnt (X-Font-get (xwem-osd-xdpy xwem-mpd-osd)
                                  (plist-get st-plist :font)))
                 (w (X-Text-width (xwem-osd-xdpy xwem-mpd-osd) fnt txt))
                 (h (X-Text-height (xwem-osd-xdpy xwem-mpd-osd) fnt txt))
                 (x (/ (- (xwem-osd-width xwem-mpd-osd) w) 2))
                 (y (/ (- (xwem-osd-height xwem-mpd-osd) h) 2)))
            (setq osin (xwem-osd-get-prop xwem-mpd-osd 'state-instance))
            (if (xwem-osd-instance-p osin)
                (progn
                  (xwem-mpd-unless-in-cache osin 'state (vector txt x y)
                    (xwem-osd-instance-change osin :text txt :x x :y y))
                  (xwem-osd-instance-show osin))
              (setq osin (xwem-osd-text-add xwem-mpd-osd x y txt
                                            :depth (plist-get st-plist :depth)
                                            :color (plist-get st-plist :color)
                                            :font (plist-get st-plist :font)
                                            :keymap xwem-mpd-keymap))
              (xwem-osd-put-prop xwem-mpd-osd 'state-instance osin)))
          ;; Hide state instance
          (setq osin (xwem-osd-get-prop xwem-mpd-osd 'state-instance))
          (when (xwem-osd-instance-p osin)
            (xwem-osd-instance-hide osin))))
      )))

(provide 'xwem-mpd)

;;; On-load actions

;; Install default bindings
(xwem-global-set-key [(XF86AudioRaiseVolume)] 'mpd-volume-up)
(xwem-global-set-key [(XF86AudioLowerVolume)] 'mpd-volume-down)
(xwem-global-set-key [(XF86AudioMute)] 'mpd-volume-mute)

(xwem-global-set-key [(XF86AudioPlay)] 'mpd-playpause)
(xwem-global-set-key [(XF86AudioStop)] 'mpd-stop)

(xwem-global-set-key [(XF86AudioPrev)] 'mpd-previous-track)
(xwem-global-set-key [(XF86AudioNext)] 'mpd-next-track)

(xwem-global-set-key [(shift control XF86Forward)] 'mpd-seek-forward)
(xwem-global-set-key [(shift control XF86Back)] 'mpd-seek-backward)

;;; xwem-mpd.el ends here
