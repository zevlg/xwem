;;; xwem-appcollect.el --- Collect an applications.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Oct 29 04:35:18 MSD 2004
;; Keywords: xwem
;; Time-stamp: <30/11/2006 00:05:01 lg@h1>

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

;; Helpfull util to create `xwem-applications-alist'.
;; Usage:
;; 
;;     (xwem-appcollect '("xterm" "mozilla"))

;;     (xwem-appcollect '("xterm" "mozilla") '(name class) 'or)

;;     (xwem-appcollect '("xterm" "mozilla") '(command) 'or)

;;; Code:

(require 'xwem-load)
(require 'xwem-manage)
(require 'xwem-launcher)

(define-xwem-method manage appcollect (cl)
  "Manage method when collecting info about applications."
  (declare (special xwem-app-collection))
  (declare (special xwem-app-collect-wait))

  (let ((app-name (xwem-cl-get-prop cl 'xwem-appcollect-app-name))
        (op (xwem-cl-get-prop cl 'xwem-appcollect-op))
        (params (xwem-cl-get-prop cl 'xwem-appcollect-params))
        mspec)

    (setq mspec (list op))
    (mapc #'(lambda (par)
              (cond ((eq par 'class)
                     (push `(and (class-inst
                                  ,(concat "^" (car (xwem-hints-wm-class
                                                     (xwem-cl-hints cl))) "$"))
                                 (class-name
                                  ,(concat "^" (cdr (xwem-hints-wm-class
                                                     (xwem-cl-hints cl))) "$")))
                           mspec))
                    ((eq par 'name)
                     (push `(name
                             ,(concat "^" (xwem-hints-wm-name
                                           (xwem-cl-hints cl)) "$"))
                           mspec))
                    ((eq par 'command)
                     (push `(command
                             ,(concat "^" (xwem-hints-wm-command
                                           (xwem-cl-hints cl)) "$"))
                           mspec))))
          params)
    (setq mspec (nreverse mspec))
          
    (setq xwem-app-collection (put-alist app-name (list mspec) xwem-app-collection))
    (xwem-client-kill cl t)
    (setq xwem-app-collect-wait nil)))

;;;###autoload(autoload 'xwem-appcollect "xwem-appcollect" nil nil)
(defun xwem-appcollect (app-names &optional params operation)
  "Collect and return applications manage specs.
APP-NAMES is a list of applications to collect.

PARAMS is a list of elements where each element is one of:
  `class'   - Include class-inst/class-name into mspec.
  `name'    - Include app name into mspec.
  `command' - Include command into mspec."
  (let ((xwem-app-collection nil))
    (declare (special xwem-app-collection))

    (mapc #'(lambda (app)
              (let ((cmd (xwem-launcher-normalize-cmd app))
                    (xwem-app-collect-wait t))
                (declare (special xwem-app-collect-wait))
                (xwem-manda-add-expectance
                 `(appcollect
                   (xwem-appcollect-op
                    ,(or operation 'and)
                    xwem-appcollect-params ,(or params '(class))
                    xwem-appcollect-app-name ,app)
                   (t)) 120)
                (xwem-execute-program cmd)
                (while xwem-app-collect-wait
                  (dispatch-event (next-event)))))
          app-names)
    
    (nreverse xwem-app-collection)))


(provide 'xwem-appcollect)

;;; xwem-appcollect.el ends here
