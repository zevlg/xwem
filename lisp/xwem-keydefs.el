;;; xwem-keydefs.el --- Define standard XWEM keybindings.

;; Copyright (C) 2003-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Dec 12 15:47:42 MSK 2003
;; Keywords: xwem, xlib
;; Time-stamp: <30/11/2008 17:58:43 lg@h1>

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

(require 'xwem-load)
(require 'xwem-misc)
(require 'xwem-compat)

(defmacro xwem-keymap-put-prop (keymap prop value)
  "In on of xwem KEYMAP put property PROP with VALUE."
  `(setplist ,keymap (plist-put (symbol-plist ,keymap) ,prop ,value)))

(put 'xwem-keymap-put-prop 'lisp-indent-function 2)

;;;###autoload
(defvar xwem-default-parent-map (make-sparse-keymap 'XWEM-default-parent)
  "Default parent keymap for all override keymaps.")

;;;###autoload
(defvar xwem-global-map (make-sparse-keymap 'XWEM-global-map)
  "XWEM's global keymap.

Bindings:
\\{xwem-global-map}.")
(set-keymap-parents xwem-global-map (list xwem-default-parent-map))

;;; Prefixes

;; XWEM keymap prefixes may have properties:
;;
;;   * 'cursor - Cursor to use when entering keymap
;;   * 'prompt - Prompt to show when entering keymap
;;

;; Extended commands
(xwem-define-prefix-command 'xwem-Hyper-X-prefix t)
(defvar xwem-hyp-x-map (symbol-function 'xwem-Hyper-X-prefix)
  "Keymap for extended (\\<xwem-global-map>\\[xwem-Hyper-X-prefix]) commands.

Bindings:
\\{xwem-hyp-x-map}")
(define-key xwem-global-map (xwem-kbd "H-x") 'xwem-Hyper-X-prefix)

;; Help
(xwem-define-prefix-command 'xwem-help-prefix t)
(defvar xwem-help-map (symbol-function 'xwem-help-prefix)
  "Keymap for help (\\<xwem-global-map>\\[xwem-help-prefix]) commands.

Bindings:
\\{xwem-help-map}")
(xwem-keymap-put-prop 'xwem-help-prefix 'cursor 'xwem-cursor-help)
(xwem-keymap-put-prop 'xwem-help-prefix 'prompt
  '(concat (single-key-description xwem-last-event) " "
           (substitute-command-keys
            "(Type \\<xwem-help-map>\\[xwem-help-for-help] for help)")))
(define-key xwem-global-map (xwem-kbd "H-h") 'xwem-help-prefix)

;;;###autoload(autoload 'xwem-root-prefix "xwem-keydefs" "Keymap for root window." nil 'keymap)
(xwem-define-prefix-command 'xwem-root-prefix t)
(defvar xwem-root-map (symbol-function 'xwem-root-prefix)
  "Keymap for root window.

Bindings:
\\{xwem-root-map}")
(set-keymap-parents xwem-root-map (list xwem-default-parent-map))

;;;###autoload(autoload 'xwem-frame-prefix "xwem-keydefs" "Keymap for frames operations." nil 'keymap)
(xwem-define-prefix-command 'xwem-frame-prefix t)
(defvar xwem-frame-map (symbol-function 'xwem-frame-prefix)
  "Keymap for frames operations.

Bindings:
\\{xwem-frame-map}")
(set-keymap-parents xwem-frame-map (list xwem-default-parent-map))

;; Window commands
(xwem-define-prefix-command 'xwem-Hyper-X-4-prefix t)
(defvar xwem-hyp-x-4-map (symbol-function 'xwem-Hyper-X-4-prefix)
  "Keymap for subcommands of \\<xwem-global-map>\\[xwem-Hyper-X-4-prefix].

Bindings:
\\{xwem-hyp-x-4-map}")
(define-key xwem-global-map (xwem-kbd "H-x 4") 'xwem-Hyper-X-4-prefix)

;; Frame commads
(xwem-define-prefix-command 'xwem-Hyper-X-5-prefix t)
(defvar xwem-hyp-x-5-map (symbol-function 'xwem-Hyper-X-5-prefix)
  "Keymap for subcommands of \\<xwem-global-map>\\[xwem-Hyper-X-5-prefix].

Bindings:
\\{xwem-hyp-x-5-map}")
(define-key xwem-global-map (xwem-kbd "H-x 5") 'xwem-Hyper-X-5-prefix)

;; Client commands
(xwem-define-prefix-command 'xwem-Hyper-X-7-prefix t)
(defvar xwem-hyp-x-7-map (symbol-function 'xwem-Hyper-X-7-prefix)
  "Keymap for subcommands of \\<xwem-global-map>\\[xwem-Hyper-X-7-prefix].

Bindings:
\\{xwem-hyp-x-7-map}")
(define-key xwem-global-map (xwem-kbd "H-x 7") 'xwem-Hyper-X-7-prefix)

;; Launcher keymap
(xwem-define-prefix-command 'xwem-launcher-prefix t)
(defvar xwem-launcher-map (symbol-function 'xwem-launcher-prefix)
  "Keymap for xwem launcher (\\<xwem-global-map>\\[xwem-launcher-prefix]) commands.

Bindings:
\\{xwem-launcher-map}")
(define-key xwem-global-map (xwem-kbd "H-a") 'xwem-launcher-prefix)

(xwem-define-prefix-command 'xwem-launcher-other-win-prefix)
(defvar xwem-launcher-other-win-map
  (symbol-function 'xwem-launcher-other-win-prefix)
  "Keymap for launcher in other window (\\<xwem-global-map>\\[xwem-launcher-other-win-prefix]) commands.

Bindings:
\\{xwem-launcher-other-win-map}")
(define-key xwem-hyp-x-4-map (xwem-kbd "H-a") 'xwem-launcher-other-win-prefix)

(xwem-define-prefix-command 'xwem-launcher-other-frame-prefix)
(defvar xwem-launcher-other-frame-map (symbol-function 'xwem-launcher-other-frame-prefix)
  "Keymap for launcher in other frame (\\<xwem-global-map>\\[xwem-launcher-other-frame-prefix]) commands.

Bindings:
\\{xwem-launcher-other-frame-map}")
(define-key xwem-hyp-x-5-map (xwem-kbd "H-a") 'xwem-launcher-other-frame-prefix)

;; Keyboard macros keymap
;;;###autoload(autoload 'xwem-user-macros-prefix "xwem-keydefs" nil nil 'keymap)
(xwem-define-prefix-command 'xwem-user-macros-prefix t)
(defvar xwem-user-macros-map (symbol-function 'xwem-user-macros-prefix)
  "Keymap of user defined keyboard macros (\\<xwem-global-map>\\[xwem-user-macros-prefix]) commands.

Bindings:
\\{xwem-user-macros-map}")
(set-keymap-default-binding xwem-user-macros-map 'xwem-keymacro-undefined)
(define-key xwem-user-macros-map (xwem-kbd "H-h") xwem-prefix-help-command)
(define-key xwem-global-map (xwem-kbd "H-m") 'xwem-user-macros-prefix)

;; Window Resizing keymap
(xwem-define-prefix-command 'xwem-resize-prefix t)
(defvar xwem-hyp-resize-map (symbol-function 'xwem-resize-prefix)
  "Keymap for window resize (\\<xwem-global-map>\\[xwem-resize-prefix]) commands.

Bindings:
\\{xwem-hyp-resize-map}")
(define-key xwem-global-map (xwem-kbd "H-r") 'xwem-resize-prefix)

(xwem-define-prefix-command 'xwem-Hyper-C-prefix)
(defvar xwem-hype-c-map (symbol-function 'xwem-Hyper-C-prefix)
  "Keymap for H-c commands.

Bindings:
\\{xwem-Hyper-C-prefix}")
(define-key xwem-global-map (xwem-kbd "H-c") 'xwem-Hyper-C-prefix)


;;; Default bindings for `xwem-global-map'
(set-keymap-default-binding xwem-global-map 'xwem-self-insert-or-undefined)

(define-key xwem-global-map (xwem-kbd "H-g") xwem-quit-command)
(define-key xwem-global-map (xwem-kbd "H-G") 'xwem-kbd-quit)

;; Quoting keyboard
(define-key xwem-global-map (xwem-kbd "H-q") 'xwem-kbd-quote-command)

;; help keys
(define-key xwem-help-map (xwem-kbd "H-h") 'xwem-help-for-help)
(define-key xwem-help-map (xwem-kbd "H-c") 'xwem-help-clients)
(define-key xwem-help-map (xwem-kbd "H-f") 'xwem-help-frames)
(define-key xwem-help-map (xwem-kbd "H-w") 'xwem-help-wins)
(define-key xwem-help-map (xwem-kbd "H-y") 'xwem-help-cutbuffers)
(define-key xwem-help-map (xwem-kbd "b") 'xwem-help-describe-bindings)
(define-key xwem-help-map (xwem-kbd "k") 'xwem-help-describe-key)
(define-key xwem-help-map (xwem-kbd "h") 'xwem-help)
(define-key xwem-help-map (xwem-kbd "w") 'xwem-help-where-is)
(define-key xwem-help-map (xwem-kbd "l") 'xwem-show-message-log)
(define-key xwem-help-map (xwem-kbd "s") 'xwem-strokes-list)
(define-key xwem-help-map (xwem-kbd "r") 'xwem-registers-list)
(define-key xwem-help-map (xwem-kbd "m") 'xwem-help-mode)
(define-key xwem-help-map (xwem-kbd "i") 'xwem-help-client-info)

;; Universal argument
(define-key xwem-global-map (xwem-kbd "H-u") 'xwem-universal-argument)

(define-key xwem-global-map (xwem-kbd "H--") 'xwem-universal-minus)
(define-key xwem-global-map (xwem-kbd "H-0") 'xwem-universal-digit)
(define-key xwem-global-map (xwem-kbd "H-1") 'xwem-universal-digit)
(define-key xwem-global-map (xwem-kbd "H-2") 'xwem-universal-digit)
(define-key xwem-global-map (xwem-kbd "H-3") 'xwem-universal-digit)
(define-key xwem-global-map (xwem-kbd "H-4") 'xwem-universal-digit)
(define-key xwem-global-map (xwem-kbd "H-5") 'xwem-universal-digit)
(define-key xwem-global-map (xwem-kbd "H-6") 'xwem-universal-digit)
(define-key xwem-global-map (xwem-kbd "H-7") 'xwem-universal-digit)
(define-key xwem-global-map (xwem-kbd "H-8") 'xwem-universal-digit)
(define-key xwem-global-map (xwem-kbd "H-9") 'xwem-universal-digit)

;; Keyboard macros
(define-key xwem-global-map (xwem-kbd "H-x (") 'xwem-keymacro-begin)
(define-key xwem-global-map (xwem-kbd "H-x )") 'xwem-keymacro-end)
(define-key xwem-global-map (xwem-kbd "H-x e") 'xwem-keymacro-play-last)
(define-key xwem-global-map (xwem-kbd "H-x q") 'xwem-keymacro-recursive-edit)
(define-key xwem-global-map (xwem-kbd "H-M-c") 'xwem-keymacro-exit-recursive-edit)
(define-key xwem-global-map (xwem-kbd "H-x H-k") 'xwem-edmacro-edit-kbd-macro)

;; Moving across windows
(define-key xwem-global-map (xwem-kbd "H-x o") 'xwem-other-window)
(define-key xwem-global-map (xwem-kbd "H-n") 'xwem-frame-goto-next)
(define-key xwem-global-map (xwem-kbd "H-p") 'xwem-frame-goto-prev)

(define-key xwem-global-map (xwem-kbd "H-left") 'xwem-winmove-left)
(define-key xwem-global-map (xwem-kbd "H-right") 'xwem-winmove-right)
(define-key xwem-global-map (xwem-kbd "H-up") 'xwem-winmove-up)
(define-key xwem-global-map (xwem-kbd "H-down") 'xwem-winmove-down)

(define-key xwem-global-map (xwem-kbd "H-x +") 'xwem-balance-windows)

;; Frames operations
(define-key xwem-global-map (xwem-kbd "H-x 5 5") 'xwem-make-frame)
(define-key xwem-global-map (xwem-kbd "H-x 5 b") 'xwem-cl-switch-other-frame)
(define-key xwem-global-map (xwem-kbd "H-x 5 r") 'xwem-launch-program-other-frame)
(define-key xwem-global-map (xwem-kbd "H-x 5 Z") 'xwem-frame-showroot)
(define-key xwem-global-map (xwem-kbd "H-x 5 H-a x") 'xwem-launch-xterm-other-frame)
(define-key xwem-global-map (xwem-kbd "H-x 5 H-a m") 'xwem-launch-lupe-other-frame)
(define-key xwem-global-map (xwem-kbd "H-x 5 s") 'xwem-frame-switch)

(define-key xwem-global-map (xwem-kbd "H-x 5 0") 'xwem-frame-destroy)
(define-key xwem-global-map (xwem-kbd "H-x 5 1") 'xwem-frame-fit-screen)
(define-key xwem-global-map (xwem-kbd "H-x 5 2") 'xwem-frame-sbs-vert-split)
(define-key xwem-global-map (xwem-kbd "H-x 5 3") 'xwem-frame-sbs-hor-split)
(define-key xwem-global-map (xwem-kbd "H-x 5 n") 'xwem-frame-set-name)
(define-key xwem-global-map (xwem-kbd "H-x 5 C-l") 'xwem-frame-lower)
(define-key xwem-global-map (xwem-kbd "H-x 5 C-r") 'xwem-frame-raise)
(define-key xwem-global-map (xwem-kbd "H-x 5 z") 'xwem-frame-hide)
(define-key xwem-global-map (xwem-kbd "H-x H-n") 'xwem-frame-next)
(define-key xwem-global-map (xwem-kbd "H-x H-p") 'xwem-frame-previous)
(define-key xwem-global-map (xwem-kbd "H-x 5 H-t") 'xwem-transpose-frames)

;; Window commands
(define-key xwem-global-map (xwem-kbd "H-x 4 0") 'xwem-kill-cl-and-window)
(define-key xwem-global-map (xwem-kbd "H-x 4 b") 'xwem-cl-switch-other-win)
(define-key xwem-global-map (xwem-kbd "H-x 4 H-o") 'xwem-cl-switch-other-win)
(define-key xwem-global-map (xwem-kbd "H-x 4 r") 'xwem-launch-program-other-win)
(define-key xwem-global-map (xwem-kbd "H-x 4 H-M-l") 'xwem-cl-switch-to-other-in-other-win)
(define-key xwem-global-map (xwem-kbd "H-x 4 H-a x") 'xwem-launch-xterm-other-win)
(define-key xwem-global-map (xwem-kbd "H-x 4 H-a m") 'xwem-launch-lupe-other-win)
(define-key xwem-global-map (xwem-kbd "H-x 4 H-t") 'xwem-transpose-windows)

(define-key xwem-global-map (xwem-kbd "H-o") 'xwem-Hyper-X-4-prefix)

;; Clients commands
(define-key xwem-global-map (xwem-kbd "H-x 7 z") 'xwem-client-iconify)
(define-key xwem-global-map (xwem-kbd "H-x 7 Z") 'xwem-client-iconify-every)
(define-key xwem-global-map (xwem-kbd "H-x 7 0") 'xwem-client-kill)
(define-key xwem-global-map (xwem-kbd "H-x 7 1") 'xwem-client-iconify-others)
(define-key xwem-global-map (xwem-kbd "H-x 7 2") 'xwem-client-run-copy)
(define-key xwem-global-map (xwem-kbd "H-x 7 4") 'xwem-client-run-copy-other-win)
(define-key xwem-global-map (xwem-kbd "H-x 7 5") 'xwem-client-run-copy-other-frame)
(define-key xwem-global-map (xwem-kbd "H-x 7 i") 'xwem-client-info)
(define-key xwem-global-map (xwem-kbd "H-x 7 H-o") 'xwem-cl-switch-to-other)
(define-key xwem-global-map (xwem-kbd "H-x 7 p") 'xwem-cl-pop-to-client)
(define-key xwem-global-map (xwem-kbd "H-x 7 t") 'xwem-client-set-title)
(define-key xwem-global-map (xwem-kbd "H-x 7 H-t") 'xwem-cl-transpose)
(define-key xwem-global-map (xwem-kbd "H-x 7 f") 'xwem-toggle-fullscreen)

(define-key xwem-global-map (xwem-kbd "H-=") 'xwem-client-info)

(define-key xwem-global-map (xwem-kbd "H-@") 'xwem-client-set-mark)
(define-key xwem-global-map (xwem-kbd "H-SPC") 'xwem-client-set-mark)
(define-key xwem-global-map (xwem-kbd "H-x H-x") 'xwem-client-exchange-selected-and-mark)
(define-key xwem-global-map (xwem-kbd "H-x k") 'xwem-client-query-kill)

(define-key xwem-global-map (xwem-kbd "H-x z") 'xwem-client-iconify)

;; Launcher commands
(define-key xwem-global-map (xwem-kbd "H-a H-s") 'xwem-launcher-toggle-split-type)
(define-key xwem-global-map (xwem-kbd "H-a H-f") 'xwem-launcher-toggle-frame-type)
(define-key xwem-global-map (xwem-kbd "H-a l") 'xwem-launch-xlock)
(define-key xwem-global-map (xwem-kbd "H-a x") 'xwem-launch-xterm)
(define-key xwem-global-map (xwem-kbd "H-a m") 'xwem-launch-lupe) ; Syn for 'm' is magnifier
(define-key xwem-global-map (xwem-kbd "H-a e") 'make-frame)

;; Window Resizing
(define-key xwem-global-map (xwem-kbd "H-r h") 'xwem-window-enlarge-horizontally)
(define-key xwem-global-map (xwem-kbd "H-r v") 'xwem-window-enlarge-vertically)

;; Various commands
(define-key xwem-global-map (xwem-kbd "H-C-0") 'xwem-frame-fast-switch)
(define-key xwem-global-map (xwem-kbd "H-C-1") 'xwem-frame-fast-switch)
(define-key xwem-global-map (xwem-kbd "H-C-2") 'xwem-frame-fast-switch)
(define-key xwem-global-map (xwem-kbd "H-C-3") 'xwem-frame-fast-switch)
(define-key xwem-global-map (xwem-kbd "H-C-4") 'xwem-frame-fast-switch)
(define-key xwem-global-map (xwem-kbd "H-C-5") 'xwem-frame-fast-switch)
(define-key xwem-global-map (xwem-kbd "H-C-6") 'xwem-frame-fast-switch)
(define-key xwem-global-map (xwem-kbd "H-C-7") 'xwem-frame-fast-switch)
(define-key xwem-global-map (xwem-kbd "H-C-8") 'xwem-frame-fast-switch)
(define-key xwem-global-map (xwem-kbd "H-C-9") 'xwem-frame-fast-switch)

(define-key xwem-global-map (xwem-kbd "H-S") 'xwem-frame-switch-nth)
(define-key xwem-global-map (xwem-kbd "H-s") 'xwem-frame-switch-nth-linkage)

(define-key xwem-global-map (xwem-kbd "H-x 0") 'xwem-window-delete)
(define-key xwem-global-map (xwem-kbd "H-x 3") 'xwem-window-split-horizontally)
(define-key xwem-global-map (xwem-kbd "H-x 2") 'xwem-window-split-vertically)
(define-key xwem-global-map (xwem-kbd "H-x 1") 'xwem-window-delete-others)

(define-key xwem-global-map (xwem-kbd "H-t") 'xwem-cl-transpose)
(define-key xwem-global-map (xwem-kbd "H-o H-t") 'xwem-transpose-windows)
(define-key xwem-global-map (xwem-kbd "H-x H-t") 'xwem-transpose-frames)

(define-key xwem-global-map (xwem-kbd "H-M-l") 'xwem-cl-switch-to-other)
(define-key xwem-global-map (xwem-kbd "H-x H-l") 'xwem-switch-other-client)

(define-key xwem-global-map (xwem-kbd "H-x b") 'xwem-switch-client)
(define-key xwem-global-map (xwem-kbd "H-x B") 'xwem-switch-iconified-client)
(define-key xwem-global-map (xwem-kbd "H-x a") 'xwem-attach-client)
(define-key xwem-global-map (xwem-kbd "H-x H-b") 'xwem-ixwem)
(define-key xwem-global-map (xwem-kbd "H-x H-s") 'xwem-strokes-execute-last-stroke)

(define-key xwem-global-map (xwem-kbd "H-x r") 'xwem-launch-program)
(define-key xwem-global-map (xwem-kbd "H-:") 'xwem-eval-expression)
(define-key xwem-global-map (xwem-kbd "H-!") 'xwem-shell-command)
(define-key xwem-global-map (xwem-kbd "H-M-x") 'xwem-execute-extended-command)
(define-key xwem-global-map (xwem-kbd "H-#") 'xwem-mini-calc)
(define-key xwem-global-map (xwem-kbd "H-TAB") 'xwem-minibuffer-activate)

(define-key xwem-global-map (xwem-kbd "H-f12") 'xwem-misc-make-screenshot)
(define-key xwem-global-map (xwem-kbd "H-z") 'xwem-misc-pause)
(define-key xwem-global-map (xwem-kbd "H-pause") 'xwem-misc-pause)

;; Register commands
(define-key xwem-global-map (xwem-kbd "H-x 6") 'xwem-register-win-config)
(define-key xwem-global-map (xwem-kbd "H-x 8") 'xwem-register-frame-config)
(define-key xwem-global-map (xwem-kbd "H-x 9") 'xwem-register-client-config)
(define-key xwem-global-map (xwem-kbd "H-x /") 'xwem-register-client)
(define-key xwem-global-map (xwem-kbd "H-x j") 'xwem-register-jump)
(define-key xwem-global-map (xwem-kbd "H-j") 'xwem-register-jump)

;; Client switcher
(define-key xwem-global-map (xwem-kbd "H-f") 'xwem-forward-application)
(define-key xwem-global-map (xwem-kbd "H-b") 'xwem-backward-application)
(define-key xwem-global-map (xwem-kbd "H-x s") 'xwem-select-application)

(define-key xwem-global-map (xwem-kbd "H-]") 'xwem-clswi-next)
(define-key xwem-global-map (xwem-kbd "H-[") 'xwem-clswi-prev)
(define-key xwem-global-map (xwem-kbd "H-}") 'xwem-clswi-next-other-window)
(define-key xwem-global-map (xwem-kbd "H-{") 'xwem-clswi-prev-other-window)
(define-key xwem-global-map (xwem-kbd "H-x 4 H-]") 'xwem-clswi-next-other-window)
(define-key xwem-global-map (xwem-kbd "H-x 4 H-[") 'xwem-clswi-prev-other-window)
(define-key xwem-global-map (xwem-kbd "H-o H-]") 'xwem-clswi-next-other-window)
(define-key xwem-global-map (xwem-kbd "H-o H-[") 'xwem-clswi-prev-other-window)

;; Cutbuffers
(define-key xwem-global-map (xwem-kbd "H-w") 'xwem-copy-cutbuffer)
(define-key xwem-global-map (xwem-kbd "H-y") 'xwem-paste-cutbuffer)

;;; Misc bindings
(define-key xwem-global-map (xwem-kbd "H-x H-f") 'xwem-open-file)
(define-key xwem-global-map (xwem-kbd "H-x f") 'xwem-open-recent-file)
;; Making holer in frames
(define-key xwem-global-map (xwem-kbd "H-x h") 'xwem-holer-prefix)
;; Log your work
(define-key xwem-global-map (xwem-kbd "H-W") 'xwem-worklog-prefix)
;; Interactively edit client's property
(define-key xwem-global-map (xwem-kbd "H-c H-e") 'xwem-edit-client-properties)

;; Compat layer, must move to xwem-compat.el
(if xwem-gnuemacs-p
    (progn
      (define-key xwem-global-map [s-mouse-1] 'xwem-strokes-begin)
      (define-key xwem-global-map [h-mouse-1] 'xwem-strokes-cmplx-begin))
  (progn
    ;;; Root keymap
    (define-key xwem-root-map (xwem-kbd "<button1>") 'xwem-popup-auto-menu)
    (define-key xwem-root-map (xwem-kbd "<button3>") 'xwem-popup-clients-menu)
    ;; No-need to grab button1/button3
    (put 'xwem-popup-auto-menu 'xwem-no-grab t)
    (put 'xwem-popup-clients-menu 'xwem-no-grab t)

    ;; Strokes bindings
    (define-key xwem-root-map (xwem-kbd "<button2>") 'xwem-strokes-begin)
    (define-key xwem-root-map (xwem-kbd "M-<button1>") 'xwem-strokes-begin)
    (put 'xwem-strokes-begin 'pgrab-mode X-GrabModeSync)
    (define-key xwem-root-map (xwem-kbd "H-<button1>") 'xwem-strokes-cmplx-begin)

    ;;; Frame keymap
    (define-key xwem-frame-map (xwem-kbd "<button1>") 'xwem-frame-on-delim-resize)
    (define-key xwem-frame-map (xwem-kbd "<button3>") 'xwem-frame-on-delim-menu)
    ;; No-need to grab button1/button3
    (put 'xwem-frame-on-delim-resize 'xwem-no-grab t)
    (put 'xwem-frame-on-delim-menu 'xwem-no-grab t)
    ;; Frame imove/iresize
    (define-key xwem-frame-map (xwem-kbd "Sh-C-button1") 'xwem-frame-imove)
    (define-key xwem-frame-map (xwem-kbd "Sh-C-button3") 'xwem-frame-iresize)
    ))


(provide 'xwem-keydefs)

;;; xwem-keydefs.el ends here
