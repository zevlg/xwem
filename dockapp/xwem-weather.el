;;; xwem-weather.el --- Display weather information in XWEM dock.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Steve Youngs <steve@youngs.au.com>
;; Created: 2004-06-22
;; Keywords: xwem
;; Time-stamp: <30/11/2006 00:00:51 lg@h1>

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

;; Display weather information (current local temp) in the dock area.
;; Optionally display a frame with more detailed weather information.

;; Set this up by adding the following code to your ~/.xwem/xwemrc.el

;;   (require 'xwem-weather)
;;   (customize-set-variable 'xwem-weather-update-frequency 3600)
;;   (add-hook 'xwem-after-init-hook 'xwem-weather-init)

;; You might also consider seting `xwem-weather-fetch-function'.  Set
;; it to `xwem-weather-fetch-direct' if you have direct connection to
;; the internet, or set it to `xwem-weather-fetch-with-url' to use url
;; package in case you connect through proxy

;;; Code:

(eval-when-compile
  (autoload 'url-retrieve "url"))

(require 'itimer)

(require 'xwem-osd)
(require 'xwem-interactive)
(require 'xwem-help)
(require 'xwem-compat)

(defgroup xwem-weather nil
  "XWEM Weather options."
  :prefix "xwem-weather-"
  :group 'xwem)

(defcustom xwem-weather-station-id "ybbn"
  "*The four letter weather station ID.

The default setting is the author's local station, Brisbane,
Australia.  So you can all be jealous of the wonderful weather we
have in Australia. :-P

You should be able to find out what the code is for your nearest
weather station at http://weather.noaa.gov/"
  :type 'string
  :link '(url-link "http://weather.noaa.gov/")
  :group 'xwem-weather)

(defcustom xwem-weather-data-directory xwem-dir
  "*The directory to story weather data files."
  :type '(directory :must-match t)
  :group 'xwem-weather)

(defcustom xwem-weather-temperature-format 'celsius
  "*Display temperature in Celsius or Fahrenheit."
  :type '(choice
          (const :tag "Celsius" celsius)
          (const :tag "Fahrenheit" fahrenheit))
  :group 'xwem-weather)

(defface xwem-weather-osd-face
  '((((class color))
     (:foreground "cyan" :family "fixed" :size "12pt"))
    (t
     (:family "fixed" :size "12pt")))
  "*Face for the weather OSD."
  :group 'xwem-weather)

(defvar xwem-weather-menu
  '(["Show details" xwem-weather-show-details]
    ["Update" xwem-weather-update]
    "---"
    ["Destroy" xwem-weather-remove])
  "Menu to popup on `xwem-weather-popup-menu' command.
NOTE: title for this menu is formated dynamically.")

;;;###autoload(autoload 'xwem-weather-prefix "xwem-weather" nil nil 'keymap)
(xwem-define-prefix-command 'xwem-weather-prefix t)
(defvar xwem-weather-keymap (symbol-function 'xwem-weather-prefix)
  "Keymap for weather commands.
Bindings:
\\{xwem-weather-keymap}")

(define-key xwem-weather-keymap ?d 'xwem-weather-show-details)
(define-key xwem-weather-keymap ?u 'xwem-weather-update)

;;; Internal variables

(defvar xwem-weather-osd nil)

(defvar xwem-weather-frequency 0)

(defun xwem-weather-alter-update-frequency (value)
  "Alters the update frequency of the weather updates.

DO NOT CALL THIS FUNCTION DIRECTLY.  Change the update frequency
by customising `xwem-weather-update-frequency'.  You MUST use
`customize-set-variable' to do so.

Argument SYM is the symbol name of what is changing the frequency.  It
will always be `xwem-weather-update-frequency'.

Argument VALUE is an integer determining how often, in seconds, to
update the weather data."
  (let ((itimer (get-itimer "xwem-weather-itimer")))
    (cond ((and (xwem-osd-p xwem-weather-osd)
                value
                itimer)
           (set-itimer-value itimer value)
           (set-itimer-restart itimer value))
          ((and (xwem-osd-p xwem-weather-osd)
                (eq value 0)
                itimer)
           (delete-itimer itimer))
          ((and (xwem-osd-p xwem-weather-osd)
                (> value 0)
                (not itimer))
           (start-itimer "xwem-weather-itimer"
                         'xwem-weather-update
                         value value))
          (t nil))
    (setq xwem-weather-frequency value)
    (xwem-message 'info "Weather update frequency set to: %d seconds" value)))

;; This is below the internal variable marker so bug reports aren't
;; gigabytes long.  I think we can live without this var being in bug
;; reports. --SY.
(defcustom xwem-weather-update-frequency 0
  "*The number of seconds between updates of the weather info.

Most weather stations only update once/hour so it might not be very
beneficial to set this to lower than an hour.

If this is set to 0 updates will not happen.

If you want to set this outside of the custom interface, you MUST use
`customize-set-variable'."
  :type '(choice
          (integer :tag "Do not update" 0)
          (integer :tag "Every 10 minutes" 600)
          (integer :tag "Every 15 minutes" 900)
          (integer :tag "Every 30 minutes" 1800)
          (integer :tag "Every 60 minutes" 3600)
          (integer :tag "Every 2 hours" 7200)
          (integer :tag "Every 4 hours" 14400)
          (integer :tag "Every 12 hours" 43200)
          (integer :tag "Every 24 hours" 86400)
          (integer :tag "Other"))
  :set '(lambda (sym value)
          (xwem-weather-alter-update-frequency value))
  :initialize 'custom-initialize-default
  :group 'xwem-weather)

(defcustom xwem-weather-fetch-function 'xwem-weather-fetch-default
  "Function used to fetch weather file.
Use `xwem-weather-fetch-with-url' to use W3 package, or
`xwem-weather-fetch-direct' if you have direct connect to internet."
  :type '(choice (function :tag "Direct" xwem-weather-fetch-direct)
                 (function :tag "Using W3" xwem-weather-fetch-with-url)
                 (function :tag "Autoconfigure" xwem-weather-fetch-default)
                 (function :tag "User defined"))
  :group 'xwem-weather)

(defconst xwem-weather-url "http://weather.noaa.gov/pub/data/observations/metar/decoded"
  "URL used to fetch decoded weather files.")

(defun xwem-weather-data-file ()
  "Return filename where to store/read weather info."
  (expand-file-name (concat "weather-" xwem-weather-station-id)
                    xwem-weather-data-directory))

(defun xwem-weather-fetch-with-url (url)
  "Fetch URL using `url' package."
  (require 'url)
  (let ((url-inhibit-mime-parsing t))
    (setq url-inhibit-mime-parsing url-inhibit-mime-parsing) ; shutup compiler
    (get-buffer (cdr (url-retrieve url)))))

(defun xwem-weather-fetch-direct (url)
  "Fetch URL making direct connect to site."
  (let* ((mp (string-match "http://\\([^/:]*\\)\\(.*$\\)" url))
         (host (and mp (match-string 1 url)))
         (path (and mp (match-string 2 url)))
         (user-agent (concat emacs-program-name "-" emacs-program-version))
         (http (open-network-stream "xwem-weather-update"
                                    " *xwem-weather-update-buf*"
                                    host 80))
         (pbuf (process-buffer http)))
    (process-send-string
     http
     (concat "GET /" path " HTTP/1.1\r\n"
             "MIME-Version: 1.0\r\n"
             "Connection: close\r\n"
             "Extension: Security/Digest Security/SSL\r\n"
             "Host: " host "\r\n"
             "Accept: */*\r\n"
             "User-Agent: " user-agent "\r\n\r\n"))
    (while (eq (process-status http) 'open)
      (dispatch-event (next-event)))
    pbuf))

(defun xwem-weather-fetch-default (url)
  "Default autoconfiguring fetcher.
It tries `xwem-weather-fetch-direct' first, and then switches to
`xwem-weather-fetch-with-url' if direct connect is not possible."
  (condition-case nil
      (prog1
          (xwem-weather-fetch-direct url)
        (setq xwem-weather-fetch-function 'xwem-weather-fetch-direct))
    (t (prog1
           (xwem-weather-fetch-with-url url)
         (setq xwem-weather-fetch-function 'xwem-weather-fetch-with-url)))))

(defun xwem-weather-retrieve-update ()
  "Retrieve weather info."
  (let ((pbuf (funcall xwem-weather-fetch-function
                       (concat xwem-weather-url "/"
                               (upcase (concat xwem-weather-station-id ".txt"))))))
    (with-current-buffer pbuf
      (goto-char (point-min))
      (if (re-search-forward "^Content-Length: \\([0-9]+.*$\\)" nil t)
	  (let* ((file-length (string-to-int (match-string 1)))
		 (file-begin (progn
			       (goto-char (point-min))
			       (re-search-forward "^Content-Type:" nil t)
			       (forward-line 2)
			       (point-at-bol))))
	    (goto-char file-begin)
	    (forward-char file-length)
	    (write-region file-begin (point) (xwem-weather-data-file)))
	(write-region (point-min) (point-max) (xwem-weather-data-file))))
    (kill-buffer pbuf)))

(defun xwem-weather-get-temp ()
  "Return the temperature as a string from the weather data file."
  (with-temp-buffer
    (erase-buffer)
    (insert-file-contents-literally (xwem-weather-data-file))
    (goto-char (point-min))
    (when (re-search-forward
           "^Temperature: \\(-?[0-9]+ F\\) (\\(-?[0-9]+ C\\))" nil t)
      (let ((temp-f (match-string 1))
            (temp-c (match-string 2)))
        (if (eq xwem-weather-temperature-format 'celsius)
            temp-c
          temp-f)))))

(defun xwem-weather-osd-text-width (text)
  "Return TEXT width."
  (X-Text-width
   (xwem-dpy)
   (or (and (xwem-osd-p xwem-weather-osd)
            (X-Gc-font (xwem-osd-mask-gc xwem-weather-osd)))
       (X-Font-get (xwem-dpy) (face-font-name 'xwem-weather-osd-face)))
   text))

(defun xwem-weather-osd-text-height (text)
  "Return TEXT width."
  (X-Text-height
   (xwem-dpy)
   (or (and (xwem-osd-p xwem-weather-osd)
            (X-Gc-font (xwem-osd-mask-gc xwem-weather-osd)))
       (X-Font-get (xwem-dpy) (face-font-name 'xwem-weather-osd-face)))
   text))

(defun xwem-weather-display-text (text)
  "Display TEXT in weather dockapp."
  (let ((we-width (xwem-weather-osd-text-width text))
        (height (xwem-weather-osd-text-height text))
        (goff (xwem-weather-osd-text-width
               (substring text 0 (- (length text) 2))))
        (tinst (xwem-osd-get-prop xwem-weather-osd 'text-instance))
        (ainst (xwem-osd-get-prop xwem-weather-osd 'arc-instance)))
    ;; Resize weather osd
    (xwem-osd-set-width xwem-weather-osd we-width)
    (xwem-osd-set-height xwem-weather-osd height)

    ;; Display temperature text and degree sign
    (if (xwem-osd-instance-p tinst)
        (xwem-osd-instance-change tinst :text text)
      (xwem-osd-put-prop xwem-weather-osd 'text-instance
        (xwem-osd-text-add xwem-weather-osd 0 0 text)))

    (let ((arc (make-X-Arc :x goff :y 0
                           :width 3 :height 3 ; XXX
                           :angle1 0 :angle2 360)))
      (if (xwem-osd-instance-p ainst)
          (xwem-osd-instance-change ainst :xarc arc)
        (xwem-osd-put-prop xwem-weather-osd 'arc-instance
          (xwem-osd-arc-add xwem-weather-osd arc))))))

;;;###autoload(autoload 'xwem-weather-update "xwem-weather" nil t)
(define-xwem-command xwem-weather-update ()
  "*Update the weather OSD."
  (xwem-interactive "_")

  (xwem-weather-retrieve-update)
  (when (xwem-osd-p xwem-weather-osd)
    (xwem-weather-display-text (xwem-weather-get-temp))))

;;;###autoload(autoload 'xwem-weather-show-details "xwem-weather" nil t)
(define-xwem-command xwem-weather-show-details ()
  "*Show the details of the current weather information."
  (xwem-interactive)
  (xwem-help-display "weather"
   (insert-file-contents (xwem-weather-data-file))))

;;;###autoload(autoload 'xwem-weather-popup-remove "xwem-weather" nil t)
(define-xwem-command xwem-weather-remove ()
  "*Remove the weather OSD."
  (xwem-interactive)
  (when (xwem-osd-p xwem-weather-osd)
    (xwem-osd-destroy xwem-weather-osd))
  (when (itimerp (get-itimer "xwem-weather-itimer"))
    (delete-itimer (get-itimer "xwem-weather-itimer"))))

;;;###autoload(autoload 'xwem-weather-popup-menu "xwem-weather" nil t)
(define-xwem-command xwem-weather-popup-menu (ev)
  "Popup weather menu."
  (xwem-interactive (list xwem-last-event))
  (unless (button-event-p ev)
    (error 'xwem-error
           "`xwem-weather-popup-menu' must be bound to mouse event"))
  (xwem-popup-menu
   (cons (format "Weather (%s)" (upcase xwem-weather-station-id))
         xwem-weather-menu)))

(defvar xwem-weather-osd-keymap
  (let ((map (make-sparse-keymap 'xwem-weather-osd-keymap)))
    (define-key map [button1] 'xwem-weather-show-details)
    (define-key map [button2] 'xwem-weather-update)
    (define-key map [button3] 'xwem-weather-popup-menu)
    map)
  "Keymap for weather OSD.")

(define-xwem-command xwem-weather-display-temp (&optional force)
  "*Display the current temperature using OSD."
  (xwem-interactive "P")
  (when force
    (xwem-weather-retrieve-update))

  (if (xwem-osd-p xwem-weather-osd)
      (xwem-weather-display-text (xwem-weather-get-temp))

    ;; Create and setup weather osd than display current weather
    (let ((text (xwem-weather-get-temp)))
      (setq xwem-weather-osd
            (xwem-osd-create-dock
             (xwem-dpy)
             (xwem-weather-osd-text-width text)
             (xwem-weather-osd-text-height text)
             (list 'keymap xwem-weather-osd-keymap)))
      (xwem-osd-set-color xwem-weather-osd
                          (face-foreground-name 'xwem-weather-osd-face))
      (xwem-osd-set-font xwem-weather-osd
                         (face-font-name 'xwem-weather-osd-face))
      
      ;; Do it so, `xwem-weather-osd-text-height' and
      ;; `xwem-weather-osd-text-height' will use mask's gc.
      (xwem-osd-create-mask xwem-weather-osd)
      (xwem-weather-display-text text))))

;;;###autoload
(defun xwem-weather (&optional dockid dockgroup dockalign)
  "Initialise the weather dock."
  (interactive)

  (when (xwem-osd-p xwem-weather-osd)
    (xwem-osd-destroy xwem-weather-osd))
  (when (itimerp (get-itimer "xwem-weather-itimer"))
    (delete-itimer (get-itimer "xwem-weather-itimer")))
  (xwem-weather-display-temp 'force)
  (unless (zerop xwem-weather-frequency)
    (start-itimer "xwem-weather-itimer"
                  'xwem-weather-update
                  xwem-weather-frequency
                  xwem-weather-frequency)))

(defalias 'xwem-weather-init 'xwem-weather)


(provide 'xwem-weather)

;;; On-load actions
(xwem-global-set-key (xwem-kbd "H-c W") 'xwem-weather-prefix)

;;; xwem-weather.el ends here
