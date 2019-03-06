;;; xdemo.el --- testing xcb -*- lexical-binding: t -*-
;; Copyright (C) 2019 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: extensions, processes

;; xdemo.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; xdemo.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file is a "simple" demonstration of how you open an xcb window
;; from Emacs and display a rectangle in it.

;; There xcb interface is documented well here, and the concepts map
;; 1:1 to the xelb/xcb library:

;; https://www.x.org/releases/X11R7.5/doc/libxcb/tutorial/#notions

;; This file isn't meant to be used for anything other than a
;; demonstration of how to get started working with xcb concepts in
;; Emacs, which I couldn't find anywhere else.  The xelb/xcb library
;; is really nice, but it doesn't have much in the way of examples or
;; documentation.

;; To use:

;; M-: (make-xcb-window)

;; This should display a 500x500 teal window on your screen.  It
;; should draw a black rectangle in the window.

;; (xcb:disconnect x)

;; This will shut down the connection and remove the window.

;;; Code:

(require 'xcb)
(require 'xcb-screensaver)
(require 'xcb-ewmh)
(require 'color)

(defun make-xcb-window ()
  (setq x (xcb:connect ":0"))
  (xcb:ewmh:init x t)
  (xdemo--make-window x)
  (xdemo--get-events x)
  (xdemo--setup-expose x)
  nil)

(defun xdemo--make-window (x)
  "Create the window that'll get the events from X."
  (let ((root (slot-value (car (slot-value (xcb:get-setup x) 'roots))
                          'root))
	(id (xcb:generate-id x))
	(name "Test"))
    (xcb:-+request
     x (make-instance 'xcb:CreateWindow
		      :depth xcb:WindowClass:CopyFromParent
		      :wid id
		      :parent root
		      :x 0
		      :y 0
		      :width 500
		      :height 500
		      :border-width 1
		      :class xcb:WindowClass:InputOutput
		      :visual 0
		      :value-mask (logior xcb:CW:EventMask
					  ;; Without the following mask,
					  ;;the background of the window
					  ;;will be blank (transparent).
					  xcb:CW:BackPixel
					  xcb:CW:OverrideRedirect)
		      :event-mask (logior xcb:EventMask:Exposure
					  xcb:EventMask:ButtonPress
					  xcb:EventMask:ButtonRelease
					  xcb:EventMask:StructureNotify
					  xcb:EventMask:PointerMotion
					  xcb:EventMask:KeyPress
					  xcb:EventMask:KeyRelease)
		      :background-pixel (xdemo--get-color "blue")
		      :override-redirect 1))
    ;; Give the window a name (not really necessary).
    (xcb:-+request x (make-instance 'xcb:ChangeProperty
				    :mode xcb:PropMode:Replace
				    :window id
				    :property xcb:Atom:WM_NAME
				    :type xcb:Atom:STRING
				    :format 8
				    :data-len (length name)
				    :data name))
    ;; Display the window on the screen.
    (xcb:-+request x (make-instance 'xcb:MapWindow :window id))
    ;; Flush all the commands, which will make X actually do the
    ;; preceding actions.
    (xcb:flush x)
    ;; Return the window id.
    id))

(defun xdemo--move-window (x id)
  (xcb:-+request x (make-instance 'xcb:ConfigureWindow
				  :window id
				  :value-mask (logior xcb:ConfigWindow:X
						      xcb:ConfigWindow:Y)
				  :x 100
				  :y 100)))

(defun xdemo--get-events (x)
  (xcb:+event x 'xcb:ButtonPress
	      (lambda (a b)
		(message "Button: %s %s" a b)))
  (xcb:+event x 'xcb:MotionNotify
	      (lambda (a b)
		(message "Notify: %s %s" a b)))
  (xcb:+event x 'xcb:KeyPress
	      (lambda (a b)
		(message "Key: %s %s" a b))))

(defun xdemo--get-color (name)
  "Return the value of the color specified by NAME."
  (let* ((color (mapcar #'float (color-values name)))
	 (white (mapcar #'float (color-values "white")))
	 (hex (color-rgb-to-hex
	       (/ (nth 0 color) (nth 0 white))
	       (/ (nth 1 color) (nth 1 white))
	       (/ (nth 2 color) (nth 2 white)))))
    (string-to-number (substring hex 1) 16)))

;; If we want the xcd window to be visible, we do the drawing of the
;; data from this event.
(defun xdemo--setup-expose (x)
  (xcb:+event x 'xcb:Expose
	      (lambda (data fake)
		(let ((ev (make-instance 'xcb:Expose)))
		  (xcb:unmarshal ev data)
		  (let ((id (slot-value ev 'window)))
		    (xdemo--draw x id))))))

(defun xdemo--draw (x window)
  (let ((gid (xcb:generate-id x)))
    ;; Set up a "graphics context", which is just a bag of
    ;; colour/line/etc settings used when drawing.
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
    ;; Example drawing we could be doing, but we'd probably do
    ;; something else here...
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

(provide 'xdemo-test)

;;; xdemo-test.el ends here
