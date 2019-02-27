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

;; (push "~/src/screensaver.el" load-path)
;; (autoload 'screensaver-start "screensaver" nil t)


;; (screensaver-start 300 (lambda (seconds) (unless seconds (screensaver-display-image "/music/repository/Four Tet/New Energy/sleeve.jpg"))))

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
			 'screensaver--schedule)))))

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
	    (background-color . "black")
	    (vertical-scroll-bars . nil)
	    (minibuffer . nil)
	    (horizontal-scroll-bars . nil)))))
    (set-frame-width frame (x-display-pixel-width) nil t)
    (set-frame-height frame (x-display-pixel-height) nil t)
    (select-frame-set-input-focus frame)
    (let ((buffer (switch-to-buffer "*screensaver*")))
      (setq truncate-lines t)
      (erase-buffer)
      (setq mode-line-format nil)
      (when screensaver--action
	(funcall screensaver--action nil))
      ;; Wait until we get some event (mouse movement, keyboard
      ;; action), but ignore events the first second, because popping
      ;; up frames and stuff generates events, apparently.
      (let ((start (float-time)))
	(while (or (null (track-mouse (read-event "" 5)))
		   (< (- (float-time) start) 1))
	  (when (> (- (float-time) start) 1)
	    (funcall screensaver--action (- (float-time) start)))))
      ;; Restore the old setup.
      (delete-frame frame)
      (kill-buffer buffer)
      (screensaver-stop)
      (screensaver--schedule)
      (when selected
	(select-frame-set-input-focus selected)))))

(defun screensaver-display-image (file)
  "Example action that can be performed when the screensaver activates."
  (let* ((window-height (window-size nil nil t))
	 (window-width (min (window-size nil t t)
			    (x-display-pixel-width)))
	 (svg (svg-create window-width window-height))
	 (image-size (image-size (create-image file) t))
	 ;; Fit the image to the window.
	 (image-height window-height)
	 (image-width (* (car image-size)
			 (/ window-height (float (cdr image-size))))))
    (when (> image-width window-width)
      (setq image-width window-width
	    image-height (* (cdr image-size)
			    (/ window-width (float (car image-size))))))
    (svg-rectangle svg 0 0 window-width window-height
		   :fill "#000000")
    (svg-embed svg file (mailcap-file-name-to-mime-type file) nil
	       :y (/ (- window-height image-height) 2)
	       :x (/ (- window-width image-width) 2)
	       :width image-width
	       :height image-height)
    (insert-image (svg-image svg))))

(provide 'screensaver)

;;; screensaver.el ends here
