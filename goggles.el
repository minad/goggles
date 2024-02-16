;;; goggles.el --- Pulse modified regions -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 Daniel Mendler

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Created: 2020
;; Version: 0.4
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/minad/goggles
;; Keywords: convenience, text

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Pulse modified regions

;;; Code:

(require 'pulse)

;;;; Faces

(defgroup goggles nil
  "Pulse modified regions."
  :link '(url-link :tag "Homepage" "https://github.com/minad/goggles")
  :link '(emacs-library-link :tag "Library Source" "goggles.el")
  :group 'editing
  :prefix "goggles-")

(defface goggles-changed
  '((((class color) (min-colors 88) (background dark))
     :background "#1a0f44")
    (((class color) (min-colors 88) (background light))
     :background "#f2f0ff")
    (t :background "blue"))
  "Face for highlighting changed text.")

(defface goggles-removed
  '((default :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#31101f")
    (((class color) (min-colors 88) (background light))
     :background "#ffedf2")
    (t :background "red"))
  "Face for highlighting removed text.")

(defface goggles-added
  '((((class color) (min-colors 88) (background dark))
     :background "#1f461a")
    (((class color) (min-colors 88) (background light))
     :background "#e0ffe0")
    (t :background "green"))
  "Face for highlighting added text.")

;;;; Customization

(defcustom goggles-pulse-iterations pulse-iterations
  "Number of iterations in a pulse operation."
  :type 'number)

(defcustom goggles-pulse-delay pulse-delay
  "Delay between face lightening iterations."
  :type 'number)

(defcustom goggles-pulse t
  "Enable pulsing."
  :type 'boolean)

;;;; Internal variables

(defvar-local goggles--active nil
  "Goggles are active.")

(defvar-local goggles--changes nil
  "List of changed regions (change log).
Each element is a pair of start/end markers.
In order to show the highlighting, the change log is used
to compute the overall start and end position.")

(defvar-local goggles--delta 0
  "Total number of changed characters.
Positive if characters have been added.
Negative if characters have been deleted.
Zero if characters have been modified.")

;;;; Hooks for logging the changes and pulsing the changed region

(defun goggles--post-command ()
  "Highlight change after command."
  (when goggles--changes
    (let ((start most-positive-fixnum)
          (end 0)
          (pulse-delay goggles-pulse-delay)
          (pulse-iterations goggles-pulse-iterations)
          (pulse-flag goggles-pulse))
      (dolist (change goggles--changes)
        (setq start (min start (car change))
              end (max end (cdr change)))
        (set-marker (car change) nil)
        (set-marker (cdr change) nil))
      (pulse-momentary-highlight-region
       start end
       (cond
        ((> goggles--delta 0) 'goggles-added)
        ((< goggles--delta 0) 'goggles-removed)
        (t 'goggles-changed)))
      (setq goggles--changes nil
            goggles--delta 0))))

(defun goggles--after-change (start end len)
  "Remember changed region between START and END.
The endpoints of the changed region are pushed to
the change log `goggles--changes'.
LEN is the length of the replaced string."
  (when goggles--active
    (setq goggles--delta (+ goggles--delta (- end start len)))
    (when (and (/= len 0) (= start end))
      (when (> start (buffer-size))
        (setq start (- start 1)))
      (setq end (1+ start)))
    (push (cons (copy-marker start) (copy-marker end)) goggles--changes)))

(defun goggles--advice(&rest args)
  "Advice around original function with ARGS."
  (let ((goggles--active t))
    (apply args)))

;;;; Define goggles

(defmacro goggles-define (name &rest funs)
  "Define goggles with NAME which is activated by the functions FUNS.

For example (goggles-define kill `kill-region') defines
the goggles function `goggles-kill' which is only activated
by the `kill-region' operation.

The function `goggles-kill' takes an optional argument DISABLE.
If called without argument, the goggles are activated,
if called with the argument t, the goggles are deactivated.

This allows to individually define goggles based on operations
and activate/deactivate them separately."
  (let ((name (intern (format "goggles-%s" name))))
    `(progn
       (defun ,name (&optional disable)
         (interactive)
         (if disable
             ,(macroexp-progn (mapcar (lambda (f) `(advice-remove #',f #'goggles--advice)) funs))
           ,@(mapcar (lambda (f) `(advice-add #',f :around  #'goggles--advice)) funs))
         nil)
       (,name))))

;;;; Define some standard goggles

(goggles-define undo primitive-undo)
(goggles-define yank yank yank-pop)
(goggles-define kill kill-region)
(goggles-define delete delete-region)
(goggles-define transpose transpose-words transpose-chars transpose-lines)

;;;; Goggles mode which activates all the defined goggles

;;;###autoload
(define-minor-mode goggles-mode
  "The goggles local minor mode pulses modified regions.
The defined goggles (see `goggles-define') can be enabled/disabled individually
in case you prefer to have goggles only for certain operations."
  :lighter " Goggles"
  (remove-hook 'post-command-hook #'goggles--post-command 'local)
  (remove-hook 'after-change-functions #'goggles--after-change 'local)
  (when goggles-mode
    (add-hook 'post-command-hook #'goggles--post-command nil 'local)
    (add-hook 'after-change-functions #'goggles--after-change nil 'local)))

(provide 'goggles)

;;; goggles.el ends here
