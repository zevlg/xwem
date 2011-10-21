;;; lpath.el --- load path fixer for XWEM.

;; Copyright (C) 2004-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Tue Aug 24 14:57:33 MSD 2004
;; Keywords: xwem
;; Time-stamp: <30/11/2006 00:02:40 lg@h1>

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

;;; Code:


;; Stuff to ensure a cleaner build.
(if (emacs-version>= 21 5)
    (autoload 'setenv "process" nil t)
  (autoload 'setenv "env" nil t))
(autoload 'customize-set-variable "cus-edit" nil t)
(autoload 'defadvice "advice" nil nil 'macro)
(autoload 'make-annotation "annotations")
(autoload 'pp "pp")
(autoload 'put-alist "alist")
(autoload 'read-kbd-macro "edmacro" nil t)
(autoload 'view-mode "view-less" nil t)
(autoload 'view-minor-mode-map "view-less" nil t 'keymap)

;; XWEM
(autoload 'xwem-debug "xwem-misc")
  
;;; lpath.el ends here
