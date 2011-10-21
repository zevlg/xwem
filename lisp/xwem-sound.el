;;; xwem-sound.el --- Sound support.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Wed Jan 28 22:25:44 MSK 2004
;; Keywords: xwem
;; Time-stamp: <29/11/2006 23:55:55 lg@h1>

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

;; XWEM support sounds. Wooouuhha xwem is multimedia awared WM :).

;; Set `xwem-visible-bell' to non-nil if you dislike beeping.

;;; Code:

(eval-and-compile
  (autoload 'play-sound "sound"))
(require 'xwem-load)

;;; Customisation
(defgroup xwem-sound nil
  "Group to customize XWEM sounds."
  :prefix "xwem-sound-"
  :prefix "xwem-"
  :group 'xwem)

(defcustom xwem-sound-default-alist
  '((default :sound bass)
    (undefined-key :sound drum)
    (command-fail :sound bass)
    (quit :sound quiet :volume 75)
    (ready :sound cuckoo)
    (alarm :sound cuckoo :volume 100)
    (warning :sound clink :volume 70)
    (error :sound bong :volume 100))
  "The alist of sounds and associated error symbols.
Used to set `xwem-sound-alist' in `xwem-sound-load-default'."
  :group 'xwem-sound
  :type '(repeat
          (group (symbol :tag "Name")
                 (checklist :inline t
                            :greedy t
                            (group :inline t
                                   (const :format "" :value :sound)
                                   (symbol :tag "Sound"))
                            (group :inline t
                                   (const :format "" :value :volume)
                                   (integer :tag "Volume"))
                            (group :inline t
                                   (const :format "" :value :pitch)
                                   (integer :tag "Pitch"))
                            (group :inline t
                                   (const :format "" :value :duration)
                                   (integer :tag "Duration"))))))

(defcustom xwem-sound-beeping-alist
  '((default :sound t :pitch 70 :duration 15 :volume 100)
    (undefined-key :sound t :pitch 100 :duration 10 :volume 100)
    (command-fail :sound t :pitch 1000 :duration 40 :volume 100)
    ;; H-g
    (quit :sound t :pitch 70 :duration 5 :volume 100)
    ;; Ready: time cunsuming task has beed done .. compile, cvs,
    ;; etc.
    (ready :sound t :pitch 800 :duration 50 :volume 100)
    ;; alarm: used by reminders
    (alarm :sound t :pitch 2000 :duration 150 :volume 100)

    (warning :sound t :pitch 50 :duration 10 :volume 100)
    (error :sound t :pitch 3000 :duration 50 :volume 100)
    )
  "X Bell oriented candidate for `xwem-sound-alist'.
Format is identical as for `xwem-sound-default-alist'."
  :group 'xwem-sound
  :type '(repeat
          (group (symbol :tag "Name")
                 (checklist :inline t
                            :greedy t
                            (group :inline t
                                   (const :format "" :value :sound)
                                   (symbol :tag "Sound"))
                            (group :inline t
                                   (const :format "" :value :volume)
                                   (integer :tag "Volume"))
                            (group :inline t
                                   (const :format "" :value :pitch)
                                   (integer :tag "Pitch"))
                            (group :inline t
                                   (const :format "" :value :duration)
                                   (integer :tag "Duration"))))))

(defcustom xwem-sound-directory (locate-data-directory "sounds")
  "Default directory to load sound files."
  :type 'directory
  :group 'sound)

(defcustom xwem-sound-directory-list (locate-data-directory-list "sounds")
  "List of directories, which to search for sound files."
  :type '(repeat directory)
  :group 'xwem-sound)

(defcustom xwem-sound-extension-list ".au:"
  "Filename extensions to complete sound file name with. If more than one
   extension is used, they should be separated by \":\". "
  :type 'string
  :group 'xwem-sound)

;;;###autoload
(defcustom xwem-sound-list
  '((xwem-sound-file-load "bass-snap" 'bass 100)
    (xwem-sound-file-load "drum-beep" 'drum 100)
    (xwem-sound-file-load "quiet-beep" 'quiet 100)
    (xwem-sound-file-load "cuckoo" 'cuckoo 100)
    (xwem-sound-file-load "clink" 'clink 100)
    (xwem-sound-file-load "bong" 'bong 100)
    (xwem-sound-file-load "say-beep" 'say-beep 100)
    )
  "A list of calls to `xwem-sound-file-load' to be processed by `xwem-sound-load-default'.
Reference `xwem-sound-file-load' for detailed information."
  :type '(repeat (sexp :tag "Sound"))
  :group 'xwem-sound)

(defcustom xwem-visible-bell nil
  "*If non-nil mean try to flash selected frame to represent a bell."
  :type 'boolean
  :group 'xwem-sound)

;;; Internal variables

;;;###autoload
(defvar xwem-sound-alist nil
  "Sound alist for use by XWEM.
Format is identical as for `sound-alist'.
Error symbols are:
  default -- When nothing else matches.
  quit -- After \\<xwem-global-map>\\[xwem-kbd-quit]
  undefined-key -- Keybinding undefined.
  command-fail -- When execution of command failed.
  warning -- Some one warnings you.
  error -- Some one reports you an error.
  ready -- Time consumed task has been done.
  alarm -- Used by reminders.")

(defun xwem-sound-file-load (filename sound-name &optional volume)
  "Read an audio FILE and return a valid node for use in `xwem-sound-alist'."
  (unless (symbolp sound-name)
    (error 'xwem-error "SOUND-NAME not a symbol"))
  (unless (or (null volume) (integerp volume))
    (error 'xwem-error "VOLUME not an integer or nil"))

  (let ((file (locate-file filename xwem-sound-directory-list
                           xwem-sound-extension-list))
        buf data)
    (unless file
      (error 'xwem-error "Couldn't locate sound file %s" filename))

    (unwind-protect
        (save-excursion
          (set-buffer (setq buf (get-buffer-create " *sound-tmp*")))
          (buffer-disable-undo (current-buffer))
          (erase-buffer)
          (let ((coding-system-for-read 'binary))
            (setq coding-system-for-read coding-system-for-read) ; shut up compiler
            (insert-file-contents file))
          (setq data (buffer-string))
          (erase-buffer))
      (and buf (kill-buffer buf)))

    (nconc (list sound-name) (when volume (list :volume volume))
           (list :sound data))))

(defun xwem-sound-do-visible-bell ()
  "Visible bell."
  (let ((frame (xwem-frame-selected)))
    (xwem-misc-flash-rectangle
     0 0 (xwem-frame-width frame) (xwem-frame-height frame)
     (xwem-frame-xwin frame))))

;;;###autoload
(defun xwem-play-sound (sound &optional volume)
  "Play a sound of provided SOUND type.
If VOLUME is specified, it overrides the value specified in
`xwem-sound-alist'."
  (if xwem-visible-bell
      (xwem-sound-do-visible-bell)

    (let ((sound-alist xwem-sound-alist))
      (play-sound sound volume))))

;;;###autoload
(defun xwem-sound-load-default (&optional x-beep)
  "Loads and install `xwem-sound-default-alist'.
If X-BEEP is non-nil, `xwem-sound-beeping-alist' will be loaded."
  (xwem-message 'info "Loading sounds ...")
  (if x-beep
      (setq xwem-sound-alist (append xwem-sound-beeping-alist
                                     xwem-sound-alist))

    ;; Load sound files
    (setq xwem-sound-alist (append xwem-sound-default-alist
                                   xwem-sound-alist
                                   (mapcar 'eval xwem-sound-list))))
  (xwem-message 'info "Loading sounds ... done"))


(provide 'xwem-sound)

;;; xwem-sound.el ends here
