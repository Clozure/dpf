(in-package "DPF")

(defconstant $black-titlebar-view-tag 100)

;;; A black titlebar, meant to resemble the one used in the QuickTime
;;; player or in iTunes.  Unfortunately, there's no system-provided way
;;; to do this, so this is only an approximation.
(defclass dpf-titlebar-view (ns:ns-view)
  ((tag :initform $black-titlebar-view-tag)
   (tracking-area :foreign-type :id)
   (close-button :foreign-type :id)
   (mouse-inside-p :accessor mouse-inside-p :initform nil))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/dealloc :void) ((self dpf-titlebar-view))
  (#/release (slot-value self 'tracking-area))
  (#/release (slot-value self 'close-button))
  (objc:remove-lisp-slots self)
  (call-next-method))

(objc:defmethod (#/tag #>NSInteger) ((self dpf-titlebar-view))
  (slot-value self 'tag))

(objc:defmethod #/initWithFrame: ((self dpf-titlebar-view) (frame #>NSRect))
  (call-next-method frame)
  (let ((close (#/standardWindowButton:forStyleMask:
                ns:ns-window #$NSWindowCloseButton #$NSTitledWindowMask))
        (miniaturize (#/standardWindowButton:forStyleMask:
		      ns:ns-window #$NSWindowMiniaturizeButton
		      #$NSTitledWindowMask))
        (zoom (#/standardWindowButton:forStyleMask:
	       ns:ns-window #$NSWindowZoomButton #$NSTitledWindowMask))
        (full-screen (if (lionp)
                       (#/standardWindowButton:forStyleMask:
			;; 7 would be #$NSWindowFullScreenButton if we had
			;; that in the interface database.
                        ns:ns-window 7 #$NSTitledWindowMask))))
    (setf (slot-value self 'close-button) (#/retain close))
    ;; All these magic numbers were empirically determined by using
    ;; Pixie to measure how the system positions these buttons on
    ;; other windows.
    (let ((button-frame (#/frame close)))
      (setf (ns:ns-rect-x button-frame) 8
	    (ns:ns-rect-y button-frame) 4)
      (#/setFrame: close button-frame)
      (#/addSubview: self close)
      (incf (ns:ns-rect-x button-frame) (+ (ns:ns-rect-width button-frame) 6))
      (#/setFrame: miniaturize button-frame)
      (#/addSubview: self miniaturize)
      (incf (ns:ns-rect-x button-frame) (+ (ns:ns-rect-width button-frame) 6))
      (#/setFrame: zoom button-frame)
      (#/addSubview: self zoom)
      (when full-screen
        (setf (ns:ns-rect-x button-frame) (- (ns:ns-rect-width frame)
                                             (ns:ns-rect-width button-frame)
                                             4))
        (#/setFrame: full-screen button-frame)
        (#/setAutoresizingMask: full-screen (logior #$NSViewMinXMargin))
        (#/addSubview: self full-screen)))
    ;; We've added the standard window buttons.  Unfortunately, we
    ;; have to track when the mouse is over them by hand.
    (rlet ((rect #>NSRect))
      (let* ((f0 (#/frame close))
	     (f1 (#/frame zoom)))
	(setf (ns:ns-rect-x rect) (#_CGRectGetMinX f0)
	      (ns:ns-rect-y rect) (#_CGRectGetMinY f0)
	      (ns:ns-rect-width rect) (- (#_CGRectGetMaxX f1)
					 (#_CGRectGetMinX f0))
	      (ns:ns-rect-height rect) (#_CGRectGetMaxY f0)))
      (let* ((ta (#/initWithRect:options:owner:userInfo:
		  (#/alloc ns:ns-tracking-area)
		  rect
		  (logior #$NSTrackingMouseEnteredAndExited
			  #$NSTrackingActiveInActiveApp)
		  self
		  +null-ptr+)))
	(setf (slot-value self 'tracking-area) ta)
	(#/addTrackingArea: self ta))))
  self)

;; This undocumented method should return YES if the traffic light
;; buttons should be drawn with the markings inside (as when the
;; mouse pointer is hovering over them).
;;
;; Unfortunately, something seems to be preventing this from working
;; now that the title bar view is layer-backed.
(objc:defmethod (#/_mouseInGroup: #>BOOL) ((self dpf-titlebar-view))
  (slot-value self 'mouse-inside-p))

;; These next two methods are called by the tracking area that
;; the titlebar-view set up for the traffic lights.
(objc:defmethod (#/mouseEntered: :void) ((self dpf-titlebar-view) event)
  (declare (ignore event))
  (setf (slot-value self 'mouse-inside-p) t)
  (#/setNeedsDisplay: self t))

(objc:defmethod (#/mouseExited: :void) ((self dpf-titlebar-view) event)
  (declare (ignore event))
  (setf (slot-value self 'mouse-inside-p) nil)
  (#/setNeedsDisplay: self t))

(objc:defmethod (#/drawRect: :void) ((self dpf-titlebar-view) (rect #>NSRect))
  (let* ((r (#/bounds self))
         (radius (cgfloat 4)) ;1 smaller than underlying view
         (gradient (#/initWithStartingColor:endingColor:
                    (#/alloc ns:ns-gradient)
                    (#/colorWithDeviceWhite:alpha: ns:ns-color
                                                       (cgfloat 0.13)
                                                       (cgfloat 1))
                    (#/colorWithDeviceWhite:alpha: ns:ns-color
                                                       (cgfloat 0.33)
                                                       (cgfloat 1)))))
    ;; Set up a clipping path so to get the rounded corners at the top
    ;; of the window.
    (let ((p (#/bezierPath ns:ns-bezier-path)))
      (rlet ((pt #>NSPoint)
             (ri #>NSRect))
        (#_NSInsetRect ri r radius radius)
        (setf (pref pt #>NSPoint.x) (#_CGRectGetMinX r)
              (pref pt #>NSPoint.y) (#_CGRectGetMinY r))
        (#/moveToPoint: p pt)
        (setf (pref pt #>NSPoint.x) (#_CGRectGetMaxX r)
              (pref pt #>NSPoint.y) (#_CGRectGetMinY r))
        (#/lineToPoint: p pt)
        (setf (pref pt #>NSPoint.x) (#_CGRectGetMaxX ri)
              (pref pt #>NSPoint.y) (#_CGRectGetMaxY ri))
        (#/appendBezierPathWithArcWithCenter:radius:startAngle:endAngle:
         p pt radius (cgfloat 0) (cgfloat 90))
        (setf (pref pt #>NSPoint.x) (#_CGRectGetMinX ri)
              (pref pt #>NSPoint.y) (#_CGRectGetMaxY ri))
        (#/appendBezierPathWithArcWithCenter:radius:startAngle:endAngle:
         p pt radius (cgfloat 90) (cgfloat 180))
        (#/closePath p)
	(#/set (#/colorWithDeviceWhite:alpha: ns:ns-color (cgfloat 0.05)
                                              (cgfloat 1)))
        (#/fill p)
        (#/addClip p)))
    ;; Draw a gradient on the top half of the titlebar.
    (setf (pref r #>NSRect.size.height) (cgfloat 11))
    (setf (pref r #>NSRect.origin.y) (cgfloat 11))
    (#/drawInRect:angle: gradient r (cgfloat 90))
    (#/release gradient)
    (#_NSFrameRect (#/bounds self))
    ;; draw the title
    (rlet ((title-rect #>NSRect))
      (#_NSInsetRect title-rect (#/bounds self) (cgfloat 30) (cgfloat 0))
      (incf (ns:ns-rect-y title-rect) (cgfloat 7))
      (decf (ns:ns-rect-height title-rect) (cgfloat 7))
      (let* ((title (#/title (#/window self)))
             (style (#/init (#/alloc ns:ns-mutable-paragraph-style)))
             (attrs (#/dictionaryWithObjectsAndKeys:
		     ns:ns-dictionary
		     style
		     #&NSParagraphStyleAttributeName
		     (#/systemFontOfSize: ns:ns-font (cgfloat 13))
		     #&NSFontAttributeName
		     (#/whiteColor ns:ns-color)
		     #&NSForegroundColorAttributeName
		     +null-ptr+)))
        (#/setAlignment: style #$NSCenterTextAlignment)
        (#/drawWithRect:options:attributes: title title-rect 0 attrs)
        (#/release style)))))



;;; A window with an overlay title bar that fades in and out when the
;;; window is/isn't the main window.  We might fade in and out based
;;; on mouse motion also, but let's try this first.
(defclass dpf-window (ns:ns-window)
  ((titlebar-view :foreign-type :id)
   (tag :foreign-type #>NSInteger))
  (:metaclass ns:+ns-object))

(objc:defmethod #/initWithContentRect:styleMask:backing:defer:
                ((self dpf-window)
                 (content-rect #>NSRect)
                 (style-mask #>NSUInteger)
                 (backing #>NSBackingStoreType)
                 (defer #>BOOL))
  (declare (ignore style-mask))
  (let ((w (call-next-method content-rect
                             (logior #$NSBorderlessWindowMask 
                                     #$NSResizableWindowMask)
                             backing defer)))
    (#/setOpaque: w nil)
    (#/setBackgroundColor: w (#/clearColor ns:ns-color))
    (#/setMovableByWindowBackground: w t)
    (when (lionp)
      (objc:objc-message-send w "setCollectionBehavior:"
			      #>NSUInteger (ash 1 7)))
    (ns:with-ns-rect (r)
      (setf (ns:ns-rect-x r) 0
	    (ns:ns-rect-y r) (- (ns:ns-rect-height content-rect) 23)
	    (ns:ns-rect-width r) (ns:ns-rect-width content-rect)
	    (ns:ns-rect-height r) 23)
      (let ((v (#/initWithFrame: (#/alloc (objc:@class "DPFTitlebarView")) r)))
	(#/setAutoresizingMask: v (logior #$NSViewWidthSizable
					  #$NSViewMinYMargin))
	(setf (slot-value self 'titlebar-view) v)))
    w))

(objc:defmethod (#/dealloc :void) ((self dpf-window))
  (#/release (slot-value self 'titlebar-view))
  (call-next-method))

;;; These next two methods are a work-around to make command-W work.
(objc:defmethod (#/validateMenuItem: #>BOOL) ((self dpf-window) menu-item)
  (if (eql (#/action menu-item)
	   (objc:@selector #/performClose:))
    (#/respondsToSelector: self (objc:@selector #/performClose:))
    (call-next-method menu-item)))

(objc:defmethod (#/performClose: :void) ((self dpf-window) sender)
  (let ((close t))
    (when (#/respondsToSelector: (#/delegate self)
				 (objc:@selector #/windowShouldClose:))
      (setq close (#/windowShouldClose: (#/delegate self) sender)))
    (when close
      (#/close self))))

(defun fade-titlebar (titlebar in-or-out)
  (unless (%null-ptr-p titlebar)
    (ecase in-or-out
      (:in (#/setHidden: (#/animator titlebar) nil))
      (:out (#/setHidden: (#/animator titlebar) t)))))

(defmethod find-titlebar (w)
  (#/viewWithTag: (#/contentView w) $black-titlebar-view-tag))

(defmethod show-titlebar (w)
  (fade-titlebar (find-titlebar w) :in))

(defun hide-titlebar (w)
  (fade-titlebar (find-titlebar w) :out))

(objc:defmethod (#/canBecomeKeyWindow #>BOOL) ((self dpf-window))
  t)

(objc:defmethod (#/canBecomeMainWindow #>BOOL) ((self dpf-window))
  t)

(objc:defmethod (#/becomeKeyWindow :void) ((self dpf-window))
  (call-next-method)
  (#/setNeedsDisplay: (slot-value self 'titlebar-view) t)
  (show-titlebar self))

(objc:defmethod (#/resignKeyWindow :void) ((self dpf-window))
  (call-next-method)
  (#/setNeedsDisplay: (slot-value self 'titlebar-view) t)
  (hide-titlebar self))
