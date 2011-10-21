;;; xwem-help.el --- Getting help in XWEM.

;; Copyright (C) 2003-2008 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: 1 Sep 2003
;; Keywords: xlib, xwem
;; Time-stamp: <8/8/2008 05:15:30 lg@h1.lan>

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

;; Help subsystem.  Entry point is `H-h'.

;;; Code:

(require 'xwem-load)
(require 'xwem-misc)

(defmacro xwem-help-display (title &rest forms)
  "Evaluate FORMS in special emacs frame and xwem help buffer."
  `(let ((temp-buffer-show-function 'xwem-special-popup-frame))
     (with-displaying-help-buffer
      (lambda ()
        (set-buffer standard-output)
        ,@forms)
      (format "xwem %s" (or ,title "")))))
(put 'xwem-help-display 'lisp-indent-function 'defun)

;;; Some help stuff
;;;###xwem-autoload
(defun xwem-logo-string ()
  "Return textified XWEM's logo string."
  (concat (xwem-str-with-faces "X" (list 'bold-italic))
          (xwem-str-with-faces "W" (list 'bold-italic 'red))
          (xwem-str-with-faces "E" (list 'bold-italic 'green))
          (xwem-str-with-faces "M" (list 'bold-italic 'blue))))

;;;###autoload(autoload 'xwem-help "xwem-help" "" t)
(define-xwem-command xwem-help ()
  "Display some help info."
  (xwem-interactive)

  (xwem-help-display nil
    (insert "Hello, this is help for ")
    (insert (xwem-logo-string))
    (insert "\n\n")

    (insert "TODO: here is some description for ")
    (insert (xwem-logo-string))
    (insert " stuff.\n")
    (insert "\n")

    ;; Frames config
    (insert "---=== Frames Info ===---\n\n")
    (insert (format "You have %d frames now and [%d] frame is selected.\n"
                    (length xwem-frames-list)
                    (xwem-frame-num (xwem-frame-selected))))
    (insert "\n")

    ;; Clients
    ;; Maybe use tree-widget package to display this info?
    (let ((curr-classn "")
          (curr-classi ""))
      (insert "---=== Clients Info ===---\n")
      (mapc
       #'(lambda (el)
           (let ((clclass (xwem-hints-wm-class (xwem-cl-hints el)))
                 (clgeom (xwem-cl-xgeom el)))
             (when (not (string= curr-classn (cdr clclass)))
               (setq curr-classn (cdr clclass))
               (insert (format "\n= Begin for class name: <%s> =\n"
                               curr-classn)))
             (when (not (string= curr-classi (car clclass)))
               (setq curr-classi (car clclass))
               (insert (format "\n- Class instance: <%s> -\n" curr-classi)))
             (insert (format "WM-NAME: <%s>, Geom: %dx%d+%d+%d\n"
                             (xwem-hints-wm-name (xwem-cl-hints el))
                             (X-Geom-width clgeom)
                             (X-Geom-height clgeom)
                             (X-Geom-x clgeom)
                             (X-Geom-y clgeom)))))
       (sort (copy-list xwem-clients)
             #'(lambda (el1 el2)
                 (let ((cl1-clas (xwem-hints-wm-class (xwem-cl-hints el1)))
                       (cl1-name (xwem-hints-wm-name (xwem-cl-hints el1)))
                       (cl2-clas (xwem-hints-wm-class (xwem-cl-hints el2)))
                       (cl2-name (xwem-hints-wm-name (xwem-cl-hints el2))))
                   ;; Sort by class name, than by class
                   ;; instance, than by wm-name.
                   (or (string-lessp (cdr cl1-clas) (cdr cl2-clas))
                       (and (string= (cdr cl1-clas) (cdr cl2-clas))
                            (string-lessp (car cl1-clas) (car cl2-clas)))
                       (and (string= (car cl1-clas) (car cl2-clas))
                            (string-lessp cl1-name cl2-name)))))))
      (insert "\n"))

    ;; Bindings
    (insert "---=== Bindings for `")
    (insert (xwem-str-with-faces "xwem-global-map" 'font-lock-keyword-face))
    (insert "' ===---\n")
    (describe-bindings-internal xwem-global-map)))

;;;###autoload(autoload 'xwem-help-for-help "xwem-help" "" t)
(define-xwem-command xwem-help-for-help ()
  "Help for XWEM's help system."
  (xwem-interactive)

  (xwem-help-display "help-for-help"
   (let ((heading "key             binding\n---             -------\n"))
     (insert "Here should be Help-for-Help!\n\n")

     (insert (format "Help prefix is %s, keys are:\n"
                     (substitute-command-keys
                      "\\<xwem-global-map>\\[xwem-help-prefix]"))
             heading)
     (describe-bindings-internal 'xwem-help-prefix))
   ))

(defun xwem-describe-prefix-bindings-1 (map keys title)
  "For keymap MAP describe prefix KEYS.
If KEYS is nil describe all keymap."
  (setq map (xwem-kbd-fixup-keymap map))
  (when (and (keymapp map)
             (or (null keys)
                 (lookup-key map keys t)))
    (let ((heading "key             binding\n---             -------\n"))
      (insert title "\n" heading)
      (describe-bindings-internal map nil nil keys))))
(put 'xwem-describe-prefix-bindings-1 'lisp-indent-function 2)

;;;###autoload(autoload 'xwem-describe-prefix-bindings "xwem-help" "" t)
(define-xwem-command xwem-describe-prefix-bindings (prefix)
  "Describe the bindings of the PREFIX used to reach this command."
  (xwem-interactive (list xwem-this-command-keys))

  ;; Adjust prefix, cut last key
  (when prefix
    (setq prefix (vconcat (butlast (append prefix nil)))))

  (xwem-help-display (format "%s prefix" (key-description prefix))
    (when prefix
      (insert (format "Key bindings starting with `%s':\n"
                      (key-description prefix))
              "\n"))

    ;; Minor modes bindings
    (mapc #'(lambda (mimap)
              (when (eval (car mimap))
                (xwem-describe-prefix-bindings-1 (eval (cdr mimap)) prefix
                  (format "Minor mode bindings for `%S':" (car mimap)))
                (insert "\n")))
          xwem-minor-mode-map-alist)

    ;; Local bindings
    (xwem-describe-prefix-bindings-1 (xwem-local-map xwem-event-client) prefix
      (format "%s major mode bindings:"
              (upcase (symbol-name (xwem-cl-manage-type xwem-event-client)))))

    ;; Finally global bindings
    (xwem-describe-prefix-bindings-1 xwem-global-map prefix
      (format "\nGlobal bindings:"))

    ;; Frame bindings
    (xwem-describe-prefix-bindings-1 'xwem-frame-prefix prefix
      (format "\nFrame bindings:"))

    ;; Root bindings.  Really need?
    (xwem-describe-prefix-bindings-1 'xwem-root-prefix prefix
      (format "\nRoot bindings:"))
    ))

;;;###autoload(autoload 'xwem-help-describe-bindings "xwem-help" "" t)
(define-xwem-command xwem-help-describe-bindings ()
  "Describe all current bindings."
  (xwem-interactive)
  (xwem-describe-prefix-bindings nil))

;;;###autoload(autoload 'xwem-help-describe-key1 "xwem-help" "" t)
(define-xwem-command xwem-help-describe-key1 (key)
  "Describe KEY"
  (xwem-interactive (list xwem-this-command-keys))

  (let ((dfn (xwem-kbd-get-binding key))
        (keystr (key-description key)))

    (if (or (null dfn) (integerp dfn))
        (xwem-message 'info "%s is undefined." keystr)
      (xwem-help-display (format "key `%s'" keystr)
       (insert keystr)
       (insert " runs ")
       (if (symbolp dfn)
           (insert (format "`%S'" dfn))
         (insert (format "%S" dfn)))
       (insert "\n\n")
       (cond ((or (stringp dfn) (vectorp dfn))
              (let ((cmd (xwem-kbd-get-binding dfn)))
                (if (not cmd)
                    (insert "a keyboard macro")
                  (insert "a keyboard macro which runs the command\n")
                  (insert (format "`%S'" cmd))
                  (insert ":\n\n")
                  (when (documentation cmd)
                    (insert (documentation cmd))))))
             ((and (consp dfn) (not (eq 'lambda (car-safe dfn))))
              (let ((describe-function-show-arglist nil))
                (describe-function-1 (car dfn))))
             ((symbolp dfn)
              (describe-function-1 dfn))
             ((documentation dfn)
              (insert (documentation dfn)))
             (t (insert "not documented"))))
      )))

;;;###autoload(autoload 'xwem-help-mode "xwem-help" "" t)
(define-xwem-command xwem-help-mode (client)
  "Describe client mode."
  (xwem-interactive (list (xwem-cl-selected)))

  (xwem-help-display (format "%S" (xwem-cl-manage-type client))
    (insert (format "%s mode: "
                    (upcase (symbol-name (xwem-cl-manage-type client)))))
    (insert (function-documentation
             (get 'manage (xwem-cl-manage-type client))))

    ;; Minor modes
    (let ((mmodes (filter #'(lambda (mm) (eval (car mm)))
                          xwem-minor-mode-alist)))
      (when mmodes
        (insert "\nMinor modes:")
        (mapc #'(lambda (mm) (insert " ") (insert (cadr mm))) mmodes)))

    (let ((mbinds (filter #'(lambda (mm) (eval (car mm)))
                          xwem-minor-mode-map-alist)))
      (when mbinds
        (insert "\nCommands:\n")
        ;; Minor modes bindings
        (mapc #'(lambda (mimap)
                  (xwem-describe-prefix-bindings-1 (eval (cdr mimap)) nil
                    (format "\n%s minor mode bindings:"
                            (cadr (assq (car mimap) xwem-minor-mode-alist)))))
              mbinds)))
    ))

;;;###autoload(autoload 'xwem-help-describe-key "xwem-help" "" t)
(define-xwem-command xwem-help-describe-key (keys)
  "Describe keysequence."
  (xwem-interactive "KDescribe key: ")

  (xwem-help-describe-key1 keys))

;;;###autoload(autoload 'xwem-help-frames "xwem-help" "" t)
(define-xwem-command xwem-help-frames ()
  "Help for XWEM's frames."
  (xwem-interactive)
  ;; TODO: write me
  (xwem-message 'todo "`xwem-help-frames' is not written yet.")
  )

;;;###autoload(autoload 'xwem-help-wins "xwem-help" "" t)
(define-xwem-command xwem-help-wins ()
  "Help for XWEM's windows."
  (xwem-interactive)
  ;; TODO: write me
  (xwem-message 'todo "`xwem-help-wins' is not written yet.")
  )

;; TODO: we should write something similar to ibuffer or
;; electric-buffer-list to operate on cliets, switching, getting stat,
;; etc.
;;;###autoload(autoload 'xwem-help-clients "xwem-help" "" t)
(define-xwem-command xwem-help-clients ()
  "Help for XWEM's clients."
  (xwem-interactive)
  ;; TODO: write me
  (xwem-message 'todo "`xwem-help-clients' is not written yet.")
  )

(define-xwem-command xwem-help-client-info (cl)
  "Display information about current client."
  (xwem-interactive (list (xwem-cl-selected)))
  (xwem-help-display (format "%s" (xwem-client-name cl))
    (insert (format (concat "Manage Type: %s\n"
                            "Application: %s\n"
                            "Uptime: %s\n"
                            "\n")
                    (upcase (symbol-name (xwem-cl-manage-type cl)))
                    (or (car (xwem-client-application cl)) "UNKNOWN")
                    (xwem-cl-get-uptime cl)))
    (when (xwem-local-map cl)
      (xwem-describe-prefix-bindings-1 (xwem-local-map cl) nil
        (format "Client local bindings:"))
      (insert "\n"))
      
    (let ((xwi (shell-command-to-string 
                (format "xwininfo -all -id 0x%x"
                        (X-Win-id (xwem-cl-xwin cl))))))
      (insert (decode-coding-string xwi (xwem-misc-locale-coding-system)) "\n"))

    (let ((xpr (shell-command-to-string
                (format "xprop -id 0x%x"
                        (X-Win-id (xwem-cl-xwin cl))))))
      (insert (format "xprop: Window id: 0x%x %S\n\n"
                      (X-Win-id (xwem-cl-xwin cl))
                      (xwem-client-name cl)))
      (insert (decode-coding-string xpr (xwem-misc-locale-coding-system)) "\n"))
    ))
    
;;;###autoload(autoload 'xwem-help-where-is "xwem-help" "" t)
(define-xwem-command xwem-help-where-is (dfn &optional paste)
  "Where-is for XWEM."
  (xwem-interactive "CXWEM where is command: \nP")

  (let* ((keys (where-is-internal dfn (list xwem-global-map)))
         (msg (if keys (format "%s is on %s"
                               dfn (sorted-key-descriptions keys))
                (format "%s is not on any keys" dfn))))
    (if paste
        (xwem-kbd-add-pending-keys msg)
      (xwem-message 'info msg))))


(provide 'xwem-help)

;;; xwem-help.el ends here
