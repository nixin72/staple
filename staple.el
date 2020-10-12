;;; staple.el --- Jump across indentation levels for quick navigation -*- lexical-binding: t; -*-
;;
;; Copyright © 2020 Philip Dumaresq
;;
;; Authors: Philip Dumaresq <phdumaresq@protonmail.com>
;; Maintainer: Philip Dumaresq <phdumaresq@protonmail.com>
;; URL: http://github.com/nixin72/block-nav.el
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; License:
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;; This package aims to provide users with a modern method of organizing
;; buffers and windows.
;;
;; Instead of having a global list of buffers that you can access from any
;; window, this package provides an abstraction over this by grouping
;; buffers by windows, which allows you to kill all buffers "stapled" to a window
;; at once, move buffers between windows, and isolate special buffers so that
;; you can better control where they are.
;; 
;; For example, if we have 4 buffers named buffer-1 through buffer-4, and the
;; following layout: 
;;
;;; Code:

(require 'cl-lib)

;;; Customization options
(defgroup staple nil
  "Customization options for staple-mode."
  :group 'environment)

(defcustom staple-init-multiple-window-strategy 'split-50/50
  "When staple-mode start and there are multiple windows open, how do we divide 
these buffers among the windows?

Possible values:
- split-evenly
- group-by-projects
- group-by-extension
- kill-other-buffers
- kill-other-windows
- prompt"
  :type 'symbol
  :group 'staple)


;; Copied from winum
(defcustom staple-scope 'global
  "Frames affected by a number set."
  :group 'staple
  :type  '(choice
           (const :tag "frame local" frame-local)
           (const :tag "visible frames" visible)
           (const :tag "global" global)))

(defcustom staple-ignored-buffers '(" *scratch*")
  "List of buffers to ignore when sorting buffers between windows."
  :group 'staple
  :type  '(repeat string))

(defcustom staple-ignored-buffers-regexp '()
  "List of regexps for buffer names to ignore sorting buffers between windows.
See Info node `(emacs) Regexps' or Info node `(elisp) Regular Expressions'"
  :group 'staple
  :type '(repeat string)
  :risky t)

;;; Private variables

(defvar staple--special-buffers '()
  "Maintains a list of all buffers not associated with files")

(defvar staple--file-buffers '()
  "Maintains a list of all buffers that are associated to files")

;;; Helper functions

;; BEGIN: copied from winum
(defun staple--list-windows-in-frame (&optional f)
  "List windows in frame F using natural Emacs ordering."
  (window-list f 0 (frame-first-window f)))

(defun staple--ignore-window-p (window)
  "Non-nil if WINDOW should be ignored for numbering."
  (let ((f (window-frame window)))
    (or (not (and (frame-live-p f)
                  (frame-visible-p f)))
        (string= "initial_terminal" (terminal-name f))
        (member (buffer-name (window-buffer window)) winum-ignored-buffers)
        (cl-some
         (lambda (regex) (string-match regex (buffer-name (window-buffer window))))
         staple-ignored-buffers-regexp))))

(defun staple--window-list ()
  "Return a list of interesting windows."
  (cl-remove-if
   #'staple--ignore-window-p
   (cl-case staple-scope
     (global
      (cl-mapcan 'staple--list-windows-in-frame (frame-list)))
     (visible
      (cl-mapcan 'staple--list-windows-in-frame (visible-frame-list)))
     (frame-local (staple--list-windows-in-frame))
     (t
      (error "Invalid `staple-scope': %S" staple-scope)))))

;; END

(defun staple--kill-other-buffers ()
  "Kill all other buffers. 
Taken from EmacsWiki: https://www.emacswiki.org/emacs/KillingBuffers#toc2"
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun staple--split-evently ()
  "Will split all the buffers evenly across the open windows."
  (let* ((windows (staple--window-list))
         (buffers (buffer-list))
         (count (length buffers)))
       ()))

(defun staple--group-by-projects ()
  (error "Not implemented yet"))

(defun staple--group-by-extension ()
  (error "Not implemented yet"))

(defun staple--kill-other-buffers ()
  (staple--kill-other-buffers))

(defun staple--kill-other-windows ()
  (delete-other-windows))

(defun staple--prompt ()
  "Organization function to run when staple wants you to prompt for your 
organization method each time."
  (let* ((choices '(split-50/05       close-windows
                    group-by-projects group-by-extension
                    close-other-buffers))           
         (choice
           (completing-read "Enter organization strategy:" choices)))
       (staple--organize-buffers (make-symbol choice))))

(defun staple--organize-buffers (&optional strategy)
  "Staples buffers to a window given an organization strategy. 
Uses `staple-init-multiple-window-strategy' to determine how to organize."
  (pcase (or strategy
             staple-init-multiple-window-strategy)
    ('split-evenly (staple--group-evenly))
    ('group-by-project (staple--group-by-project))
    ('group-by-extension (staple--group-by-extension))
    ('kill-other-buffers (staple--kill-other-buffers))
    ('kill-other-windows (staple--kill-other-windows))
    ('prompt (staple--prompt))))

(defun staple--init ()
  (dolist (buffer (buffer-list))
    (if (buffer-file-name buffer)
        (add-to-list 'staple--file-buffers buffer)
        (add-to-list 'staple--special-buffers buffer)))
  (staple--organize-buffers))

(defun staple--exit ())

(defun staple--new-buffer-fun ()
  "Add any new buffers to their appropriate list when opened.
1. Check if there's any new buffers.
2. If there are new buffers, find out where they should be.
3. Take the new buffer(s) and add them to the correct buffer list."
  (interactive)
  (let ((opened-buffers (buffer-list))
        (sorted-buffers (append special-buffers file-buffers)))
    (dolist (buffer opened-buffers)
      ;; Figure out how to determine if two buffers are the same buffer...
      (when (member buffer sorted-buffers)
        (message (buffer-file-name buffer))))))

;;; Public and Interactive functions

(define-minor-mode staple-mode
  ""
  :init-value nil
  :global t
  (if staple-mode
      (staple--exit)
      (staple--init)))

(defun staple-next-buffer (&optional window)
  "Moves to the next buffer that's been stapled to the current window.
if WINDOW is not provided, do it for the current window."
  (interactive)
  (error "Not implemented yet!"))

(defun staple-previous-buffer (&optional window)
  "Moves to the previous buffer that's been stapled to the current window.
if WINDOW is not provided, do it for the current window."
  (interactive)
  (error "Not implemented yet!"))

(defun staple-kill-window (&optional window)
  "Kills the WINDOW and all buffers inside of it. 
If WINDOW is not provided, kill the current window"
  (interactive)
  (error "Not implemented yet!"))

(defun staple-move-buffer (&optional buffer)
  "Staples a buffer to another window.
if BUFFER is not provided, do it for the current buffer."
  (interactive)
  (error "Not implemented yet!"))

(defun staple-move-buffer-up ()
  "Staples a buffer to window above the current window."
  (interactive)
  (error "Not implemented yet!"))

(defun staple-move-buffer-left ()
  "Staples a buffer to window on the left of the current window."
  (interactive)
  (error "Not implemented yet!"))

(defun staple-move-buffer-down ()
  "Staples a buffer to window below the current window."
  (interactive)
  (error "Not implemented yet!"))

(defun staple-move-buffer-right ()
  "Staples a buffer to window on the right of the current window."
  (interactive)
  (error "Not implemented yet!"))

;(add-hook 'buffer-list-update-hook 'run-fun)
