;;; goggles.el --- Pulse modified regions -*- lexical-binding: t -*-

;; Author: Daniel Mendler
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/minad/goggles

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Pulse modified regions

;;; Code:

(require 'pulse)

;;;; Faces

(defgroup goggles nil
  "Pulse modified regions."
  :group 'editing)

(defface goggles-changed
  '((((class color) (background dark))
     :background "#1A1A3A")
    (t
     :background "#CBD6FF"))
  "Face for highlighting changed text."
  :group 'goggles)

(defface goggles-removed
  '((((class color) (background dark))
     :background "#3A1A1A" :extend t)
    (t
     :background "#FFCBCB" :extend t))
  "Face for highlighting removed text."
  :group 'goggles)

(defface goggles-added
  '((((class color) (background dark))
     :background "#203A1A")
    (t
     :background "#CBFFDC"))
  "Face for highlighting added text."
  :group 'goggles)

;;;; Customization

(defcustom goggles-pulse-iterations pulse-iterations
  "Number of iterations in a pulse operation."
  :group 'goggles
  :type 'number)

(defcustom goggles-pulse-delay pulse-delay
  "Delay between face lightening iterations."
  :group 'goggles
  :type 'number)

(defcustom goggles-pulse t
  "Enable pulsing."
  :group 'goggles
  :type 'boolean)

;;;; Internal variables

(defvar-local goggles--active 0
  "Number of active goggles, which can be nested.
If the number is greater than zero, the changes are tracked
in order to pulse the changed region in the end.")

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

(defvar goggles--list nil
  "List of defined goggles, see `goggles-define'.")

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
  (when (> goggles--active 0)
    (setq goggles--delta (+ goggles--delta (- end start len)))
    (when (and (/= len 0) (= start end))
      (when (> start (buffer-size))
        (setq start (- start 1)))
      (setq end (1+ start)))
    (push (cons (set-marker (make-marker) start)
                (set-marker (make-marker) end))
          goggles--changes)))

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
       ,@(mapcar
          (lambda (f)
            `(defun ,(intern (format "goggles--adv-%s" f)) (orig &rest args)
               (when (or goggles-mode goggles-global-mode)
                 (setq goggles--active (1+ goggles--active))
                 (unwind-protect (apply orig args)
                   (setq goggles--active (- goggles--active 1))))))
          funs)
       (defun ,name (&optional local disable)
         (interactive)
         (if disable
             (progn ,@(mapcar (lambda (f) `(remove-function ',f #',(intern (format "goggles--adv-%s" f)))) funs))
           (if local
               ,@(mapcar (lambda (f) `(add-function :around (local ',f) #',(intern (format "goggles--adv-%s" f)))) funs)
             ,@(mapcar (lambda (f) `(add-function :around ',f #',(intern (format "goggles--adv-%s" f)))) funs)))
         nil)
       (push #',name goggles--list))))

;;;; Define some standard goggles

(goggles-define undo primitive-undo)
(goggles-define yank yank yank-pop)
(goggles-define kill kill-region)
(goggles-define delete delete-region)

;;;; Goggles mode which activates all the defined goggles

;;;###autoload
(define-minor-mode goggles-global-mode
  "The goggles global minor mode pulses modified regions.
The defined goggles (see `goggles-define') can be enabled/disabled individually
in case you prefer to have goggles only for certain operations."
  :global t
  :lighter " Goggles"
  (remove-hook 'post-command-hook #'goggles--post-command)
  (remove-hook 'after-change-functions #'goggles--after-change)
  (mapc (lambda (f) (funcall f nil 'disable)) goggles--list)
  (when goggles-global-mode
    (add-hook 'post-command-hook #'goggles--post-command)
    (add-hook 'after-change-functions #'goggles--after-change)
    (mapc #'funcall goggles--list)))

;;;###autoload
(define-minor-mode goggles-mode
  "The goggles minor mode pulses modified regions.
The defined goggles (see `goggles-define') can be enabled/disabled individually
in case you prefer to have goggles only for certain operations."
  :lighter " Goggles"
  (remove-hook 'post-command-hook #'goggles--post-command 'local)
  (remove-hook 'after-change-functions #'goggles--after-change 'local)
  (mapc (lambda (f) (funcall f 'local 'disable)) goggles--list)
  (when goggles-mode
    (add-hook 'post-command-hook #'goggles--post-command nil 'local)
    (add-hook 'after-change-functions #'goggles--after-change nil 'local)
    (mapc (lambda (f) (funcall f 'local)) goggles--list)))

(provide 'goggles)

;;; goggles.el ends here
