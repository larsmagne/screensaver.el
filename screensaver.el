;;; screensaver.el --- an Emacs-based screensaver for X -*- lexical-binding: t -*-
;; Copyright (C) 2019 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: extensions, processes

;; screensaver.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; screensaver.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defvar screensaver--timer nil)
(defvar screensaver--timeout (* 5 60))
(defvar screensaver--action nil)

(defun screensaver-start (&optional timeout action)
  "Start saving the screen after TIMEOUT seconds.
Perform ACTION when saving the screen.  The default is to display
a blank Emacs frame."
  (interactive)
  (unless (executable-find "xssstate")
    (error "The xssstate executable is not installed on this system"))
  (screensaver-stop)
  (when timeout
    (setq screensaver--timeout timeout))
  (setq screensaver--action action)
  (setq screensaver-timer (screensaver--schedule)))

(defun screensaver-stop ()
  "Stop saving the screen."
  (interactive)
  (when screensaver--timer
    (cancel-timer screensaver--timer)
    (setq screensaver--timer nil)))

(defun screensaver-activate ()
  "Activate the screensaver immediately."
  (interactive)
  (screensaver--activate))

(defun screensaver--schedule ()
  "Compute the first likely time that screen saving can happen."
  (let ((idle (with-temp-buffer
		(call-process "xssstate" nil t nil "-i")
		(string-to-number (buffer-string)))))
    (if (>= (/ idle 1000) screensaver--timeout)
	;; We've already been activated.
	(unless (get-buffer "*screensaver*")
	  (screensaver--activate))
      (setq screensaver--timer
	    (run-at-time (- screensaver--timeout (/ idle 1000)) nil
			 (lambda ()
			   (screensaver--schedule)))))))

(defun screensaver--activate ()
  (let ((selected (selected-frame))
	(frame
	 (make-frame
	  `((title . "Screensaver")
	    (left . 0)
	    (top . 0)
	    (width . 400)
	    (height . 200)
	    (user-position . t)
	    (background-color . "blue")
	    (vertical-scroll-bars . nil)))))
    (select-frame-set-input-focus frame)
    (let ((buffer (switch-to-buffer "*screensaver*")))
      (erase-buffer)
      (when screensaver--action
	(funcall screensaver--action))
      ;; Wait until we get some event (mouse movement, keyboard action).
      (track-mouse (read-event))
      (delete-frame frame)
      (kill-buffer buffer)
      (screensaver--schedule)
      (select-frame-set-input-focus selected))))

(provide 'screensaver)

;;; screensaver.el ends here
