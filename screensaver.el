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

;; This package requires the xelb library to talk to X:
;; git clone https://github.com/ch11ng/xelb.git

;; (push "~/src/screensaver.el" load-path)
;; (autoload 'screensaver-start "screensaver" nil t)
;; (autoload 'screensaver-hide-and-start "screensaver" nil t)

;; (screensaver-start 300 (lambda (seconds) (unless seconds (screensaver-display-image "/music/repository/Four Tet/New Energy/sleeve.jpg"))))

;; To start from the command line, you'd typically say something like:
;;
;; $ emacs --funcall screensaver-hide-and-start
;;
;; which will start a normal Emacs frame and then hide it. 
;;
;; $ emacs --eval "(screensaver-hide-and-start 60 'my-display-function)"
;;
;; to make it trigger after 60 seconds and call the function mentioned
;; to screensave.

;;; Code:

(require 'xcb)
(require 'xcb-screensaver)
(require 'xcb-ewmh)

(defvar screensaver--timer nil)
(defvar screensaver--timeout (* 5 60))
(defvar screensaver--action nil)
(defvar screensaver--hidden nil)

(defun screensaver-hide-and-start (&optional timeout action)
  "Like `screensaver-start', but hide the selected frame first."
  (let ((frame (selected-frame)))
    (make-frame-invisible frame t)
    (set-frame-width frame 0 nil t)
    (set-frame-height frame 0 nil t)
    (set-frame-position (selected-frame) -200 -200))
  (setq screensaver--hidden t)
  (screensaver-start timeout action))

(defun screensaver-start (&optional timeout action)
  "Start saving the screen after TIMEOUT seconds.
Perform ACTION when saving the screen.  The default is to display
a blank Emacs frame.

ACTION should be a function that takes a single parameter.  When
TIMEOUT has passed, the function is called with nil as the
parameter, and it will then be called about every five seconds
with the number of elapsed seconds as the parameter.  The
function is called in the buffer that's displayed on the screen,
and the function is free to do whatever it wants in that buffer."
  (interactive)
  (screensaver-stop)
  (when timeout
    (setq screensaver--timeout timeout))
  (setq screensaver--action action)
  (setq screensaver--timer (screensaver--schedule)))

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
  (let ((idle (plist-get (screensaver--get-idle) :idle)))
    (if (>= (/ idle 1000) screensaver--timeout)
	;; We've already been activated.
	(unless (get-buffer "*screensaver*")
	  (screensaver--activate))
      (setq screensaver--timer
	    (run-at-time (- screensaver--timeout (/ idle 1000)) nil
			 'screensaver--schedule)))))

(defmacro screensaver--with-x (&rest body)
  `(let* ((x (xcb:connect ":0"))
	  (root (slot-value (car (slot-value (xcb:get-setup x) 'roots))
                            'root)))
     (unwind-protect
	 (progn
	   ,@body)
       (xcb:flush x)
       (xcb:disconnect x))))

(defun screensaver--get-active-window ()
  (screensaver--with-x
   (destructuring-bind (res err)
       (xcb:+request+reply x
	   (make-instance 'xcb:GetInputFocus))
     (if res
	 (screensaver--find-real-window x (slot-value res 'focus))
       (screensaver--error (car err))))))

(defun screensaver--find-real-window (x id)
  "Return ID if it's a real window, or the first parent it finds that's real."
  (if (screensaver--real-window-p x id)
      id
    (let ((parent
	   (slot-value
	    (xcb:+request-unchecked+reply x
		(make-instance 'xcb:QueryTree
			       :window id))
	    'parent)))
      (if parent
	  (screensaver--find-real-window x parent)
	id))))

(defun screensaver--real-window-p (x id)
  "Determine if ID is a real window based on whether it has a WM_CLASS."
  (plusp
   (length
    (slot-value
     (xcb:+request-unchecked+reply x
	 (make-instance 'xcb:GetProperty
			:delete 0
			:window id
			:type xcb:Atom:STRING
			:property xcb:Atom:WM_CLASS
			:long-length 256
			:long-offset 0))
     'value))))

(defun screensaver--set-active-window (id)
  (screensaver--with-x
   (xcb:+request x
       (make-instance 'xcb:SetInputFocus
		      :revert-to xcb:InputFocus:Parent
		      :focus id
		      :time xcb:Time:CurrentTime))))

(defun screensaver--activate ()
  (let ((active-window (screensaver--get-active-window))
	(selected (selected-frame))
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
      (let ((start (float-time))
	    (times 0)
	    x resized)
	(unwind-protect
	    (progn
	      (setq x (xcb:connect ":0"))
	      (xcb:ewmh:init x t)
	      ;; Put a transparent window on top of the Emacs frame.
	      (let ((id (screensaver--make-window x))
		    (event-triggered nil))
		(screensaver--set-active-window id)
		(dolist (event '(xcb:ButtonPress
				 xcb:MotionNotify
				 xcb:KeyPress))
		  ;; Stop saving the screen when something happens.
		  (xcb:+event x event
			      (lambda (&rest _)
				(setq event-triggered t))))
		(while (not event-triggered)
		  (when (and (> (- (float-time) start) 1)
			     (not resized))
		    (setq resized t)
		    (xcb:-+request
		     x
		     (make-instance 'xcb:ConfigureWindow
				    :window id
				    :value-mask (logior xcb:ConfigWindow:Width
							xcb:ConfigWindow:Height)
				    :width (+ (x-display-pixel-width) 100)
				    :height (+ (x-display-pixel-height) 100)))
		    (xcb:flush x))
		  (sleep-for 0.1)
		  ;; Allow updating every fifth second.
		  (when (and (> (- (float-time) start) 1)
			     (zerop (mod (incf times) 50)))
		    (funcall screensaver--action (- (float-time) start))))))
	  (xcb:disconnect x)))
      ;; Restore the old setup.
      (delete-frame frame)
      (kill-buffer buffer)
      (screensaver-stop)
      (screensaver--schedule)
      (if screensaver--hidden
	  (progn
	    (make-frame-invisible (selected-frame) t)
	    (screensaver--set-active-window active-window))
	(when selected
	  (select-frame-set-input-focus selected))))))

(defun screensaver-display-image (file)
  "Example action that can be performed when the screensaver activates."
  (let* ((window-height (min (window-size nil nil t)
			     (x-display-pixel-height)))
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

(defun screensaver--get-idle ()
  (screensaver--with-x
   (when (zerop (slot-value (xcb:get-extension-data x 'xcb:screensaver)
			    'present))
     (error "No screensaver present in the X server"))
   (destructuring-bind (res err)
       (xcb:+request+reply x
           (make-instance 'xcb:screensaver:QueryInfo
			  :drawable root))
     (if res
	 (list :idle (slot-value res 'ms-since-user-input))
       (screensaver--error (car err))))))

(defun screensaver--error (error)
  (loop for slot in (object-slots error)
	collect (cons slot (slot-value error slot))))

(defun screensaver--get-events (x)
  (xcb:+event x 'xcb:ButtonPress
	      (lambda (a b)
		(message "Button: %s %s" a b)))
  (xcb:+event x 'xcb:MotionNotify
	      (lambda (a b)
		(message "Notify: %s %s" a b)))
  (xcb:+event x 'xcb:KeyPress
	      (lambda (a b)
		(message "Key: %s %s" a b))))

(defun screensaver--get-color (name)
  "Return the value of the color specified by NAME."
  (let* ((color (mapcar #'float (color-values name)))
	 (white (mapcar #'float (color-values "white")))
	 (hex (color-rgb-to-hex
	       (/ (nth 0 color) (nth 0 white))
	       (/ (nth 1 color) (nth 1 white))
	       (/ (nth 2 color) (nth 2 white)))))
    (string-to-number (substring hex 1) 16)))

(defun screensaver--setup-expose (x id)
  (xcb:+event x 'xcb:Expose (lambda (&rest _)
			      (screensaver--draw x id))))

(defun screensaver--draw (x window)
  (message "%s Got exposed" id)
  (let ((gid (xcb:generate-id x)))
    (xcb:-+request
     x
     (make-instance 'xcb:CreateGC
		    :cid gid
		    :drawable window
		    :value-mask (logior xcb:GC:Foreground
					xcb:GC:Background
					xcb:GC:GraphicsExposures
					xcb:GC:LineWidth)
		    :foreground 0
		    :background 0
		    :line-width 10
		    :graphics-exposures xcb:GX:clear))
    (xcb:-+request
     x
     (make-instance 'xcb:PolyFillRectangle
		    :drawable window
		    :gc gid
		    :rectangles (list
				 (make-instance
				  'xcb:RECTANGLE
				  :x 50 
				  :y 50 
				  :width 100
				  :height 100))))
    (xcb:flush x)))

(defun screensaver--make-window (x)
  (let ((root (slot-value (car (slot-value (xcb:get-setup x) 'roots))
                          'root))
	(id (xcb:generate-id x))
	(n "test"))
    (xcb:-+request
     x
     (make-instance 'xcb:CreateWindow
		    :depth xcb:WindowClass:CopyFromParent
		    :wid id
		    :parent root
		    :x 0
		    :y 0
		    :width 100
		    :height 100
		    :border-width 1
		    :class xcb:WindowClass:InputOutput
		    :visual 0
		    :value-mask (logior xcb:CW:EventMask
					;;xcb:CW:BackPixel
					)
		    :event-mask (logior xcb:EventMask:Exposure
					xcb:EventMask:ButtonPress
					xcb:EventMask:ButtonRelease
					xcb:EventMask:StructureNotify
					xcb:EventMask:PointerMotion
					xcb:EventMask:KeyPress
					xcb:EventMask:KeyRelease)
		    :background-pixel (screensaver--get-color "blue")
		    :override-redirect 0))
    (xcb:-+request
     x
     (make-instance 'xcb:ChangeProperty
		    :mode xcb:PropMode:Replace
		    :window id
		    :property xcb:Atom:WM_NAME
		    :type xcb:Atom:STRING
		    :format 8
		    :data-len (length n)
		    :data n))
    (xcb:-+request x
        (make-instance 'xcb:MapWindow :window id))
    (xcb:-+request
     x
     (make-instance 'xcb:ConfigureWindow
                    :window id
                    :value-mask (logior xcb:ConfigWindow:X
					xcb:ConfigWindow:Y)
		    :x -100
		    :y -100))
    (xcb:flush x)
    id))

(provide 'screensaver)

;;; screensaver.el ends here
