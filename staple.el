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
;; +--------------------------+--------------------------+
;; | Window-1                 | Window-2                 |
;; |                          |                          |
;; |                          |                          |
;; |                          |                          |
;; |                          |                          |
;; |                          |                          |
;; |                          |                          |
;; |                          |                          |
;; |  Buffer-1 contents here  |  Buffer-3 contents here  |
;; |                          |                          |
;; |                          |                          |
;; |                          |                          |
;; |                          |                          |
;; |                          |                          |
;; |                          |                          |
;; |                          |                          |
;; |                          |                          |
;; |                          |                          |
;; +--------------------------+--------------------------+
;;
;; Then the normal behaviour for `previous-buffer' when in Window-1 would be to
;; cycle back to buffer-4.
;;
;; However, if buffer-1 and 2 are stapled to window-1, and buffer-3 and 4 are
;; stapled to window-2, then `staple-previous-buffer' in window-1 would cycle
;; back to buffer-2 instead of buffer-4.
;;
;; If you were in window-2, the same behaviour applies. If your current buffer
;; is buffer-3, and you call `staple-previous-buffer' in window-2, then
;; buffer-4 will become the selected buffer.
;;
;; Without being explicit, you cannot have a single buffer available in multiple
;; windows. If you call `staple-copy-buffer-to-window', then you can staple the
;; buffer to additional windows and have it cycle through in those windows.
;;
;;
;; In addition to this, it also provides any special buffers with their own
;; window. This way, when cycling through buffers in a file window, you won't
;; need to cycle through your compilation or dired buffers for example. Like in
;; a modern editor, you can have a dedicated space for that in a window across
;; the bottom of the frame for example:
;;
;; +--------------------------+--------------------------+
;; | Window-1                 | Window-2                 |
;; |                          |                          |
;; |                          |                          |
;; |                          |                          |
;; |                          |                          |
;; |  Buffer-1 contents here  |  Buffer-3 contents here  |
;; |                          |                          |
;; |                          |                          |
;; |                          |                          |
;; |                          |                          |
;; |                          |                          |
;; +--------------------------+--------------------------+
;; | Special window-3                                    |
;; |                                                     |
;; |       Special buffers, like term go down here       |
;; |                                                     |
;; |                                                     |
;; +--------------------------+--------------------------+
;;
;; That window can be docked to the whichever side of the frame you prefer,
;; again, like in most modern text editors. If you'd like that on the right-hand
;; side, you simply need to change the value of the `staple-special-window-side'
;; variable to 'right.
;;
;;; Code:

(require 'cl-lib)

;;;###autoload
(define-minor-mode staple-mode
  ""
  :init-value nil
  :global t
  (if staple-mode
      (staple--init)
      (staple--exit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup staple nil
  "Customization options for `staple-mode'."
  :group 'environment)

(defcustom staple-init-multiple-window-strategy 'kill-other-windows
  "Sorting method to use when command `staple-mode' is called.

These strategies are used when command `staple-mode' starts and theres more than
one window open, so this helps dictate how to staple the existing file buffers
to the given window."
  :group 'staple
  :type '(choice
          (const :tag "Split evenly" split-evenly)
          (const :tag "Group by projects" group-by-projects)
          (const :tag "Group by extensions" group-by-extensions)
          (const :tag "Kill other buffers" kill-other-buffers)
          (const :tag "Kill other windows" kill-other-windows)
          (const :tag "prompt" prompt)))

(setq staple-init-multiple-window-strategy 'kill-other-windows)

(defcustom staple-special-window-dock-side 'below
  "The side of the frame that the special window should dock to."
  :group 'staple
  :type '(choice
           (const :tag "Above" above)
           (const :tag "Below" below)
           (const :tag "Left" left)
           (const :tag "Right" right)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar staple--special-buffers '()
  "Maintains a list of all buffers not associated with files.")

(defvar staple--file-buffers '()
  "Maintains a list of all file-buffers and the window they're stapled to.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  "Will group the buffers together by project.
The number of projects will be split as evenly as possible among windows."
  (error "Not implemented yet"))

(defun staple--group-by-extension ()
  "Will group the buffers together by extension.
The number of extensions will be split as evenly as possible among windows."
  (error "Not implemented yet"))

(defun staple--kill-other-buffers ()
  "Will simply kill all other buffers and windows.
Once killed, it will staple the current buffer to the current window."
  (staple--kill-other-buffers))

(defun staple--kill-other-windows ()
  "Will kill all other windows.  It will staple all buffers to the one window."
  (delete-other-windows)
  (let ((buffers (buffer-list)))))


(defun staple--to-window (buffer window)
  "Staples BUFFER to WINDOW."
  ())

(defun staple--prompt ()
  "Prompts user to enter an organization method from a list of options.
Then re-calls `staple--organize-buffers' with the new organization method."
  (let* ((choices '(split-evenly      close-windows
                    group-by-projects group-by-extension
                    close-other-buffers))
         (choice
           (completing-read "Enter organization strategy:" choices)))
       (staple--organize-buffers (make-symbol choice))))

(defun staple--organize-buffers (&optional strategy)
  "Staples buffers to a window given an organization STRATEGY.
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
  "Initialized staple.

Sort all buffers into the file-buffer or special-buffer lists.
Then, call `staple--organize-buffers' to sort all file-buffers."
  (dolist (buffer (buffer-list))
    (if (buffer-file-name buffer)
        (add-to-list 'staple--file-buffers buffer)
        (add-to-list 'staple--special-buffers buffer)))
  (staple--organize-buffers))

(defun staple--exit ()
  "Cleans up when command `staple-mode' is turned off."
  (setf staple--special-buffers nil
        staple--file-buffers nil))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public and Interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun staple-next-buffer (&optional window)
  "Move to the next buffer that's been stapled to the current window.
if WINDOW is not provided, do it for the current window."
  (interactive)
  (error "Not implemented yet!"))

(defun staple-previous-buffer (&optional window)
  "Move to the previous buffer that's been stapled to the current window.
if WINDOW is not provided, do it for the current window."
  (interactive)
  (error "Not implemented yet!"))

(defun staple-kill-window (&optional window)
  "Kill the WINDOW and all buffers inside of it.
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

(defun staple-list-buffers (&optional window)
  "Lists all the buffers stapled to the current window.
If WINDOW is non-nil, list buffers stapled to that window instead."
  (interactive)
  (error "Not implementated yet!"))

(provide 'staple)

;;; staple.el ends here
