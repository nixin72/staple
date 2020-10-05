;;; block-nav.el --- Jump across indentation levels for quick navigation -*- lexical-binding: t; -*-
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
;; buffers by windows, which allows you to kill all buffers bound to a window
;; at once, move buffers between windows, and isolate special buffers so that
;; you can better control where they are.
;; 
;; Provides a number of interactive functions for easily navigating
;; between blocks of code at the same level of indentation.
;;
;; This package does not bind any keys for you.
;; Here are some example bindings for evil-mode:
;;
;;   (define-key evil-motion-state-map "H" 'block-nav-previous-indentation-level)
;;   (define-key evil-motion-state-map "J" 'block-nav-next-block)
;;   (define-key evil-motion-state-map "K" 'block-nav-previous-block)
;;   (define-key evil-motion-state-map "L" 'block-nav-next-indentation-level)
;;
;;   ;; Although this may not be desirable since it overrides vim keys you may use.
;;
;;; Code:

(defvar special-buffers '()
  "Maintains a list of all buffers not associated with files")
(defvar file-buffers '()
  "Maintains a list of all buffers that are associated to files")

(defun run-fun ()
  "Add any new buffers to their appropriate list when opened.
1. Check if there's any new buffers.
2. If there are new buffers, find out where they should be.
3. Take the new buffer(s) and add them to the correct buffer list."
  (interactive)
  (let ((opened-buffers (buffer-list))
        (sorted-buffers (append special-buffers file-buffers)))
    (dolist (buffer opened-buffers)
      ;; Figure out how to determine if two buffers are the same buffer...
      (when (equal (buffer-name buffer) (buffer-name (car sorted-buffers)))
        (message (buffer-file-name buffer))))))
          

;(add-hook 'buffer-list-update-hook 'run-fun)
