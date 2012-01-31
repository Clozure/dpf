(cl:defpackage "DPF"
  (:use "CL" "CCL")
  (:import-from "CCL" "ENSURE-DIRECTORY-PATHNAME"))

(in-package "DPF")

(defun choose-directory-dialog (dir)
  (gui::execute-in-gui
   #'(lambda ()
       (let ((op (#/openPanel ns:ns-open-panel)))
         (#/setAllowsMultipleSelection: op nil)
         (#/setCanChooseDirectories: op t)
         (#/setCanChooseFiles: op nil)
         (with-cfurl (u dir)
           (#/setDirectoryURL: op u))
         (when (eql (#/runModalForTypes: op +null-ptr+) #$NSOKButton)
           (let* ((u (#/directoryURL op))
                  (path (#_CFURLCopyFileSystemPath u #$kCFURLPOSIXPathStyle)))
             (prog1
                 (ensure-directory-pathname (%get-cfstring path))
               (#_CFRelease path))))))))

(defmacro with-fsref ((sym pathname) &body body)
  (let ((s (gensym))
	(err (gensym))
	(namestring (gensym)))
    `(if (not (probe-file ,pathname))
       (error 'file-error :error-type "~s not found" :pathname ,pathname)
       (let ((,namestring (native-translated-namestring ,pathname)))
	 (rlet ((,sym #>FSRef))
	   (with-encoded-cstrs :utf-8 ((,s ,namestring))
	     (let ((,err (#_FSPathMakeRef ,s ,sym +null-ptr+)))
	       (if (= ,err #$noErr)
		 (progn ,@body)
		 (error "FSPathMakeRef on ~s returned ~d" ,namestring
			,err)))))))))

(defun nshuffle (sequence)
  (loop for i downfrom (1- (length sequence)) to 1
        do (rotatef (elt sequence (random (1+ i)))
                    (elt sequence i)))
  sequence)

;;; Note that these constants are also used as tags for NSMenuItem
;;; instances.

(defconstant $order-by-name 0)
(defconstant $order-by-date 1)
(defconstant $order-shuffle 2)
(defparameter *valid-orders* (list $order-by-name $order-by-date
				   $order-shuffle))

(defconstant $transition-fade 10)
(defconstant $transition-move-in 11)
(defconstant $transition-push 12)
(defconstant $transition-reveal 13)
(defparameter *valid-transitions* (list $transition-fade $transition-move-in
					$transition-push $transition-reveal))

(defconstant $2-seconds 20)
(defconstant $5-seconds 21)
(defconstant $10-seconds 22)
(defconstant $30-seconds 23)
(defconstant $1-minute 24)
(defconstant $5-minutes 25)
(defconstant $30-minutes 26)
(defparameter *valid-durations* (list $2-seconds $5-seconds $10-seconds
				      $30-seconds $1-minute $5-minutes
				      $30-minutes))

(defconstant $keep-on-top 30)

(defun duration-to-seconds (tag)
  (cond ((= tag $2-seconds) 2)
	((= tag $5-seconds) 5)
	((= tag $10-seconds) 10)
	((= tag $30-seconds) 30)
	((= tag $1-minute) 60)
	((= tag $5-minutes) 300)
	((= tag $30-minutes) 1800)
	(t (error "bad duration tag ~d" tag))))

(defun transition-code-to-type (code)
  (cond ((= code $transition-fade) #&kCATransitionFade)
	((= code $transition-move-in) #&kCATransitionMoveIn)
	((= code $transition-push) #&kCATransitionPush)
	((= code $transition-reveal) #&kCATransitionReveal)))

(defparameter *slide-duration* $2-seconds)
(defparameter *slide-order* $order-by-name)
(defparameter *slide-transition* $transition-fade)
(defparameter *slide-on-top-p* nil)

(defclass dpf-preferences-controller (ns:ns-window-controller)
  ((order-popup :foreign-type :id :accessor order-popup)
   (transition-popup :foreign-type :id :accessor transition-popup)
   (duration-popup :foreign-type :id :accessor duration-popup)
   (on-top-checkbox :foreign-type :id :accessor on-top-checkbox))
  (:metaclass ns:+ns-object))

(objc:defmethod #/init ((self dpf-preferences-controller))
  (#/initWithWindowNibName: self #@"dpf-preferences"))

(objc:defmethod (#/showWindow: :void) ((self dpf-preferences-controller)
				       sender)
  (#/center (#/window self))
  (with-slots (order-popup transition-popup
	       duration-popup on-top-checkbox) self
    (#/selectItemWithTag: order-popup *slide-order*)
    (#/selectItemWithTag: transition-popup *slide-transition*)
    (#/selectItemWithTag: duration-popup *slide-duration*)
    (#/setState: on-top-checkbox (if *slide-on-top-p* 1 0)))
  (call-next-method sender))

(objc:defmethod (#/updatePreferences: :void) ((self dpf-preferences-controller)
					      sender)
  (let ((defaults (#/standardUserDefaults ns:ns-user-defaults)))
    (cond ((eql sender (order-popup self))
	   (let* ((item (#/selectedItem sender))
		  (order (#/tag item)))
	     (setq *slide-order* order)
	     (#/setInteger:forKey: defaults order #@"order")))
	  ((eql sender (transition-popup self))
	   (let* ((item (#/selectedItem sender))
		  (transition (#/tag item)))
	     (setq *slide-transition* transition)
	     (#/setInteger:forKey: defaults transition #@"transition")))
	  ((eql sender (duration-popup self))
	   (let* ((item (#/selectedItem sender))
		  (duration (#/tag item)))
	     (setq *slide-duration* duration)
	     (#/setInteger:forKey: defaults duration #@"duration")))
	  ((eql sender (on-top-checkbox self))
	   (let ((new-state (#/state (on-top-checkbox self))))
	     (setq *slide-on-top-p* (= new-state 1))
	     (#/setBool:forKey: defaults new-state #@"on-top-p"))))))


;;; We go to some trouble here to determine whether the system knows
;;; how to display a particular image file format.
;;;
;;; Using (member file-type '("jpeg" "gif" "tif" ...) :test 'string=)
;;; would be a lot more obvious, but this way we will include all the
;;; image files that the system knows how to read, not just the list
;;; of types that we happen to wire in.
;;;
;;; See the documentation about UTIs (uniform type identifiers).

(defvar *supported-image-types*)

(defun init-supported-image-types ()
  (let* ((array (#/imageTypes ns:ns-image))
	 (len (#_CFArrayGetCount array))
	 (types nil))
    (dotimes (i len)
      (push (%get-cfstring (#_CFArrayGetValueAtIndex array i)) types))
    (setq *supported-image-types* (nreverse types))))

(defun image-file-p (pathname)
  (with-fsref (fsref pathname)
    (rlet ((pp (:* :address)))
      (#_LSCopyItemAttribute fsref #$kLSRolesViewer #&kLSItemContentType pp)
      (unless (%null-ptr-p pp)
	(let* ((p (%get-ptr pp))
	       (uti (%get-cfstring p)))
	  (#_CFRelease p)
	  (member uti *supported-image-types* :test 'string=))))))

(defun image-files-in-directory (pathname)
  (let* ((dir (ensure-directory-pathname pathname))
	 (wild (make-pathname :name :wild :type :wild :defaults dir)))
    (directory wild :directories nil :files t :test 'image-file-p)))


(defclass asset ()
  ((pathname :accessor asset-pathname :initarg :pathname)
   (name :accessor asset-name :initarg :name :initform nil)
   (date :accessor asset-date :initarg :date :initform nil)
   (properties :reader asset-properties)))

(defmethod asset-name :before ((x asset))
  (with-slots (pathname name) x
    (when (and (null name)
	       (pathnamep pathname))
      (setq name (pathname-name pathname)))))

(defmethod asset-date :before ((x asset))
  (with-slots (pathname date) x
  (when (null date)
    (if pathname
      (setf date (file-write-date pathname))
      (setf date -1)))))

(defmethod print-object ((x asset) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream "~s" (asset-name x))))

(defun make-asset (pathname)
  (make-instance 'asset :pathname pathname))

(defclass iphoto-library ()
  ((master-image-table)
   (albums :initarg :albums :initform nil :accessor iphoto-library-albums)
   (album-data-write-date)
   (album-data-pathname :initarg :album-data-pathname
			:accessor iphoto-library-album-data-pathname)))

(defconstant $cfdate-epoch (encode-universal-time 0 0 0 1 1 2001 0))

(defvar *iphoto-library* nil)

(defmethod load-master-image-table ((x iphoto-library) cfdict)
  (let ((keys (#/allKeys cfdict))
	(count (#_CFDictionaryGetCount cfdict))
	(hash (make-hash-table :test 'equal)))
    (dotimes (i count)
      (let* ((key (#_CFArrayGetValueAtIndex keys i))
	     (image-dict (#_CFDictionaryGetValue cfdict key))
	     (path (#_CFDictionaryGetValue image-dict #@"ImagePath"))
	     (caption (#_CFDictionaryGetValue image-dict #@"Caption"))
	     (date (#_CFDictionaryGetValue image-dict #@"DateAsTimerInterval"))
	     (asset (make-instance 'asset)))
	(setf (asset-pathname asset) (%get-cfstring path)
	      (asset-name asset) (%get-cfstring caption))
	(rlet ((d :double))
	  (#_CFNumberGetValue date #$kCFNumberDoubleType d)
	  (setf (asset-date asset) (round (+ (%get-double-float d)
					     $cfdate-epoch))))
	(setf (gethash (%get-cfstring key) hash) asset)))
    (setf (slot-value x 'master-image-table) hash)))

(defmethod load-albums ((x iphoto-library) cfarray)
  (if (%null-ptr-p cfarray)
    (setf (slot-value x 'albums) nil)
    (let ((count (#_CFArrayGetCount cfarray)))
      (if (<= count 0)
	(setf (slot-value x 'albums) nil)
	(with-slots (master-image-table albums) x
	  (dotimes (i count)
	    (let* ((dict (#_CFArrayGetValueAtIndex cfarray i))
		   (name (#_CFDictionaryGetValue dict #@"AlbumName"))
		   (array (#_CFDictionaryGetValue dict #@"KeyList"))
		   (album (make-instance 'iphoto-album
					 :name (%get-cfstring name))))
	      (dotimes (i (#_CFArrayGetCount array))
		(let ((k (%get-cfstring (#_CFArrayGetValueAtIndex array i))))
		  (push (gethash k master-image-table)
			(iphoto-album-photos album))))
	      (push album albums)))
	  (setq albums (nreverse albums)))))))

(defmethod load-iphoto-data ((x iphoto-library) pathname)
  (when (probe-file pathname)
    (setf (slot-value x 'album-data-write-date) (file-write-date pathname))
    (with-cfstring (p (native-translated-namestring pathname))
      (let* ((d (#/dictionaryWithContentsOfFile: ns:ns-dictionary p))
	     (cfdict (#_CFDictionaryGetValue d #@"Master Image List"))
	     (cfarray (#_CFDictionaryGetValue d #@"List of Albums")))
	(load-master-image-table x cfdict)
	(load-albums x cfarray)))))

(defmethod initialize-instance :after ((x iphoto-library) &rest initargs)
  (declare (ignore initargs))
  (load-iphoto-data x (slot-value x 'album-data-pathname)))


(defclass iphoto-album ()
  ((name :initarg :name :accessor iphoto-album-name)
   (photos :initarg :photos :initform nil :accessor iphoto-album-photos)))

(defmethod print-object ((x iphoto-album) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream "~s" (iphoto-album-name x))))


(defvar *dpf-controller*)
(defvar *preferences-controller* +null-ptr+)

(defclass dpf-controller (ns:ns-object)
  ((view-menu :accessor view-menu :foreign-type :id))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/initialize :void) ((self +dpf-controller))
  (let ((defaults (#/standardUserDefaults ns:ns-user-defaults))
	(dict (#/dictionaryWithObjectsAndKeys:
	       ns:ns-dictionary
	       (#/numberWithInt: ns:ns-number $2-seconds)
	       #@"duration"
	       (#/numberWithInt: ns:ns-number $transition-fade)
	       #@"transition"
	       (#/numberWithInt: ns:ns-number $order-by-name)
	       #@"order"
	       (#/numberWithBool: ns:ns-number nil)
	       #@"on-top-p"
	       +null-ptr+)))
    (#/registerDefaults: defaults dict)
    (setq *slide-duration* (#/integerForKey: defaults #@"duration")
	  *slide-transition* (#/integerForKey: defaults #@"transition")
	  *slide-order* (#/integerForKey: defaults #@"order")
	  *slide-on-top-p* (#/boolForKey: defaults #@"on-top-p"))))

(defconstant $from-folder-tag 0)
(defconstant $from-iphoto-tag 1)

(objc:defmethod (#/newSlideshow: :void) ((self dpf-controller) sender)
  (let ((tag (#/tag sender)))
    (cond ((= tag $from-folder-tag)
	   (let ((dir (choose-directory-dialog "~/Pictures/")))
	     (when dir
	       (make-slideshow-from-folder dir))))
	  ((= tag $from-iphoto-tag)
	   (#_NSBeep)))))

(objc:defmethod (#/newSlideshowFromAlbum: :void) ((self dpf-controller) sender)
  (let* ((title (%get-cfstring (#/title sender)))
	 (album (find title (iphoto-library-albums *iphoto-library*)
		      :key #'iphoto-album-name
		      :test #'string=)))
    (if album
      (make-slideshow-from-album album)
      (#_NSBeep))))

(objc:defmethod (#/showPreferences: :void) ((self dpf-controller) sender)
  (declare (ignore sender))
  (when (%null-ptr-p *preferences-controller*)
    (setq *preferences-controller*
	  (make-instance 'dpf-preferences-controller)))
  (#/showWindow: *preferences-controller* +null-ptr+))

(defun clear-menu-item-states (menu)
  (let ((items (#/array menu)))
    (dotimes (i (#_CFArrayGetCount items))
      (#/setState: (#_CFArrayGetValueAtIndex items i) 0))))

(objc:defmethod (#/menuNeedsUpdate: :void) ((self dpf-controller) sender)
  (flet ((clear-state (menu)
	   (let ((items (#/itemArray menu)))
	     (dotimes (i (#_CFArrayGetCount items))
	       (#/setState: (#_CFArrayGetValueAtIndex items i) 0))))
	 (set-default-state (menu)
	   (#/setState: (#/itemWithTag: menu *slide-duration*) 1)
	   (#/setState: (#/itemWithTag: menu *slide-transition*) 1)
	   (#/setState: (#/itemWithTag: menu *slide-order*) 1)
	   (#/setState: (#/itemWithTag: menu $keep-on-top) (if *slide-on-top-p*
							     1 0)))
	 (set-state-from-controller (menu controller)
	   (with-slots (duration transition order on-top-p) controller
	     (#/setState: (#/itemWithTag: menu duration) 1)
	     (#/setState: (#/itemWithTag: menu transition) 1)
	     (#/setState: (#/itemWithTag: menu order) 1)
	     (#/setState: (#/itemWithTag: menu $keep-on-top) (if on-top-p
							       1 0)))))
    (when (eql sender (view-menu self))
      (let ((key-window (#/keyWindow
			  (#/sharedApplication ns:ns-application)))
	    (view-menu sender))
	(clear-state view-menu)
	(if (%null-ptr-p key-window)
	  (set-default-state view-menu)
	  (let ((controller (#/windowController key-window)))
	    (if (or (%null-ptr-p controller)
		    (not (typep controller 'slideshow-window-controller)))
	      (set-default-state view-menu)
	      (set-state-from-controller view-menu controller))))))))
	      
(defun init-dpf-controller ()
  (setq *dpf-controller* (make-instance 'dpf-controller)))

;;; This is pointless right now, but if we want some custom
;;; window borders, then we'd do it here.
(defclass slideshow-window (ns:ns-panel)
  ()
  (:metaclass ns:+ns-object))

;;; Instances of this class manage a running slideshow.
(defclass slideshow-window-controller (ns:ns-window-controller)
  ((view :foreign-type :id :accessor slideshow-view)
   (timer :foreign-type :id :accessor slideshow-timer)
   (duration :initform $2-seconds :accessor slideshow-duration)
   (transition :initform $transition-fade :accessor slideshow-transition)
   (order :initform $order-by-name :accessor slideshow-order)
   (on-top-p :initform nil :accessor slideshow-on-top-p)
   (assets :initform nil :accessor slideshow-assets)
   (current-index :initform 0 :accessor slideshow-current-index))
  (:metaclass ns:+ns-object))

;;; This could also be done in an #/initWithWindow: override
(defmethod initialize-instance :after ((x slideshow-window-controller)
				       &rest initargs)
  (declare (ignore initargs))
  (with-slots (duration transition order on-top-p) x
    (setf duration *slide-duration*
	  transition *slide-transition*
	  order *slide-order*
	  on-top-p *slide-on-top-p*)
    (when on-top-p
      (#/setLevel: (#/window x)
		   (#_CGWindowLevelForKey #$kCGFloatingWindowLevelKey)))))

(objc:defmethod (#/dealloc :void) ((self slideshow-window-controller))
  ;;(#_NSLog #@"slideshow-window-controller dealloc")
  (objc:remove-lisp-slots self)
  (#/stopTimer self)
  (call-next-method))

(objc:defmethod (#/stopTimer :void) ((self slideshow-window-controller))
  (with-slots (timer) self
    (#/invalidate timer)
    (#/release timer)
    (setq timer +null-ptr+)))

(objc:defmethod (#/startTimer :void) ((self slideshow-window-controller))
  (with-slots (timer) self
    (unless (%null-ptr-p timer)
      (#/stopTimer self))
    (setq timer (#/timerWithTimeInterval:target:selector:userInfo:repeats:
                 ns:ns-timer
		 (float (duration-to-seconds (slideshow-duration self)) 0d0)
		 self (objc:@selector #/nextSlide:) +null-ptr+ #$YES))
    (#/addTimer:forMode: (#/currentRunLoop ns:ns-run-loop)
			 timer #&NSRunLoopCommonModes)))

(objc:defmethod (#/windowWillClose: :void) ((self slideshow-window-controller) notification)
  (declare (ignore notification))
  ;; This timer retains us (its target), so get rid of it here.
  (#/stopTimer self)
  (#/autorelease self))

(objc:defmethod (#/setDuration: :void) ((self slideshow-window-controller)
                                        (duration :int))
  (#/stopTimer self)
  (setf (slideshow-duration self) duration)
  (#/startTimer self))

(objc:defmethod (#/advanceSlideBy: :void) ((self slideshow-window-controller)
                                           (n #>NSInteger))
  (with-slots (view assets current-index) self
    (let ((nassets (length assets)))
      (when (and (plusp nassets)
                 (#/isVisible (#/window self)))
        (setf current-index (mod (+ current-index n) nassets))
        (let* ((asset (elt assets current-index))
               (pathname (asset-pathname asset))
               (image nil))
          (with-cfstring (s (native-translated-namestring pathname))
            (setq image (#/initWithContentsOfFile: (#/alloc ns:ns-image) s)))
          (#/showImage: view image)
          (#/release image))))))

(objc:defmethod (#/nextSlide: :void) ((self slideshow-window-controller) timer)
  (declare (ignore timer))
  (#/advanceSlideBy: self 1))

;;; These next three methods are action methods that are invoked by
;;; selecting menu items.  The actions are nil-targeted so that they
;;; will apply to the current slideshow window.  (A window's window
;;; controller is on the responder chain.)

(objc:defmethod (#/changeDuration: :void) ((self slideshow-window-controller)
					   sender)
  (let ((tag (#/tag sender)))
    (unless (member tag *valid-durations*)
      (#_NSLog #@"bad duration tag %d, using 5 seconds" :int tag)
      (setq tag $5-seconds))
    (setf (slideshow-duration self) tag)
    (#/stopTimer self)
    (#/startTimer self)))

(objc:defmethod (#/changeTransition: :void) ((self slideshow-window-controller)
					     sender)
  (let* ((tag (#/tag sender)))
    (unless (member tag *valid-transitions*)
      (#_NSLog #@"bad transition tag %d, using fade" :int tag)
      (setq tag $transition-fade))
    (setf (slideshow-transition self) tag)
    (#/setTransition: (slideshow-view self) tag)))

(defun sort-assets-by (assets how)
  (cond ((= how $order-by-name)
	 (setq assets (sort assets 'string< :key 'asset-name)))
	((= how $order-by-date)
	 (setq assets (sort assets '< :key 'asset-date)))
	((= how $order-shuffle)
	 (setq assets (nshuffle assets))))
  assets)

(objc:defmethod (#/changeOrder: void) ((self slideshow-window-controller)
				       sender)
  (let ((tag (#/tag sender)))
    (unless (member tag *valid-orders*)
      (#_NSLog #@"bad order tag %d, using by name" :int tag)
      (setq tag $order-by-name))
    (unless (= tag (slideshow-order self))
      (setf (slideshow-order self) tag)
      (with-slots (assets current-index) self
	(let ((current-asset (elt assets current-index)))
	  (setq assets (sort-assets-by assets tag))
	  (setq current-index (position current-asset assets)))))))

(objc:defmethod (#/changeOnTop: :void) ((self slideshow-window-controller)
					sender)
  (let ((tag (#/tag sender))
	(window (#/window self)))
    (when (= tag $keep-on-top)
      (with-slots (on-top-p) self
	;; toggle floating/normal state
	(setq on-top-p (not on-top-p))
	;; Weird C Preprocessor Tricks prevent the interface
	;; translator from figuring out what #$NSNormalWindowLevel etc.
	;; should be.  (See CGWindow.h)
	(#/setLevel: window 
		     (if on-top-p
		       (#_CGWindowLevelForKey #$kCGFloatingWindowLevelKey)
		       (#_CGWindowLevelForKey #$kCGNormalWindowLevelKey)))))))

(defclass dpf-image-view (ns:ns-image-view)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/mouseDownCanMoveWindow #>BOOL) ((self dpf-image-view))
  #$YES)
  

;;; This thing has a Core Animation layer associated with it.
;;; The way we show slides is:
;;;  * create an NSImageView with the image
;;;  * add it to ourself as a subview
;;;
;;; Now, it would be cool if, instead of just changing the slide
;;; abruptly, we could show a fancy transition animation.
;;; 
;;; An Objective-C protocol called NSAnimatablePropertyContainer
;;; lets us animate any "property" that is KVC compliant.  NSView
;;; conforms to this protocol, and has a "property" called subviews,
;;; which is an array of the view's subviews.
;;; 
;;; To animate the manipulation of subviews, we send messages to the
;;; animation proxy instead of the view itself.  For example, instead
;;; of saying (#/addSubview: view new-subview), we just say
;;; (#/addSubview: (#/animator view) new-subview).
;;;
;;; We can configure the type of animation used by setting up a
;;; dictionary mapping the property keys ("subviews" in our case)
;;;; to an appropriate CAAnimation instance.  We use a fade in.

(defclass slideshow-view (ns:ns-view)
  ((image-view :accessor image-view :foreign-type :id))
  (:metaclass ns:+ns-object))

(objc:defmethod #/initWithFrame: ((self slideshow-view) (frame #>NSRect))
  (call-next-method frame)
  (#/setWantsLayer: self #$YES)
  (#/setTransition: self *slide-transition*)
  self)

(objc:defmethod (#/dealloc :void) ((self slideshow-view))
  ;;(#_NSLog #@"slideshow-view dealloc")
  (with-slots (image-view) self
    (#/release image-view))
  (call-next-method))

(objc:defmethod (#/setTransition: :void) ((self slideshow-view)
					  (transition-code #>NSInteger))
  (let ((transition (#/animation ns:ca-transition)))
    (#/setType: transition (transition-code-to-type transition-code))
    (#/setSubtype: transition #&kCATransitionFromLeft)
    (#/setAnimations: self (#/dictionaryWithObject:forKey: ns:ns-dictionary
                                                           transition
                                                           #@"subviews"))))

(objc:defmethod (#/drawRect: :void) ((self slideshow-view) (r #>NSRect))
  (#/set (#/blackColor ns:ns-color))
  (#_NSRectFill (#/bounds self)))

(objc:defmethod (#/mouseDownCanMoveWindow #>BOOL) ((self slideshow-view))
  #$YES)

(objc:defmethod (#/acceptsFirstResponder #>BOOL) ((self slideshow-view))
  #$YES)

(objc:defmethod (#/mouseUp: :void) ((self slideshow-view) e)
  (if (> (#/clickCount e) 1)
    (#/miniaturize: (#/window self) self)
    (call-next-method e)))

;;; left/right arrow keys show the previous/next slide
(objc:defmethod (#/keyDown: :void) ((self slideshow-view) event)
  (let* ((chars (#/charactersIgnoringModifiers event))
         (unichar (and (plusp (#_CFStringGetLength chars))
                       (#_CFStringGetCharacterAtIndex chars 0)))
         (wc (#/windowController (#/window self))))
    (cond ((= unichar #$NSLeftArrowFunctionKey)
           (#/advanceSlideBy: wc -1)
           (#/setDuration: wc (slideshow-duration wc)))
          ((= unichar #$NSRightArrowFunctionKey)
           (#/advanceSlideBy: wc 1)
           (#/setDuration: wc (slideshow-duration wc)))
          (t 
           (#_NSBeep)))))
  
(objc:defmethod (#/showImage: :void) ((self slideshow-view) image)
  (if (%null-ptr-p image)
    (format t "~&hey, no image here.")
    (let ((iv (#/initWithFrame: (#/alloc (objc:@class "DPFImageView"))
				(#/bounds self))))
      (#/setImage: iv image)
      (#/setAutoresizingMask: iv (logior #$NSViewWidthSizable
					 #$NSViewHeightSizable))
      (if (and (not (%null-ptr-p (image-view self)))
               (not (%null-ptr-p iv)))
	(#/replaceSubview:with: (#/animator self) (image-view self) iv)
        (progn
          (unless (%null-ptr-p (image-view self))
            (#/removeFromSuperview (#/animator (image-view self))))
          (unless (%null-ptr-p iv)
            (#/addSubview: (#/animator self) iv))))
      (#/release (image-view self))
      (setf (image-view self) iv))))

(defun add-slideshow-menu ()
  (let* ((main-menu (#/mainMenu (#/sharedApplication ns:ns-application)))
         (file-menu (#/submenu (#/itemWithTitle: main-menu #@"File")))
         (item nil))
    (setq item (#/insertItemWithTitle:action:keyEquivalent:atIndex:
                file-menu
                #@"Slideshow From Folder..."
                (objc:@selector #/newSlideshow:)
                #@""
                0))
    (#/setTag: item $from-folder-tag)
    (#/setTarget: item *dpf-controller*)
    (setq item (#/insertItemWithTitle:action:keyEquivalent:atIndex:
		file-menu
		#@"Slideshow From iPhoto Album"
		+null-ptr+
		#@""
		1))
    (#/setTag: item $from-iphoto-tag)
    (#/setSubmenu: item (make-albums-menu))
    #+dpf-in-ide
    (progn
      (setq item (#/insertItemWithTitle:action:keyEquivalent:atIndex:
		  file-menu
		  #@"Slideshow Preferences..."
		  (objc:@selector #/showPreferences:)
		  #@""
		  2))
      (#/setTarget: item *dpf-controller*))))

(defun make-view-menu ()
  (let ((main-menu (#/mainMenu (#/sharedApplication ns:ns-application)))
	(view-menu (make-instance 'ns:ns-menu :with-title #@"View"))
	(item nil))
    (loop for name in (list #@"By Name" #@"By Date" #@"Shuffled")
	  for tag in *valid-orders*
	  do (setq item (#/addItemWithTitle:action:keyEquivalent:
			 view-menu
			 name
			 (objc:@selector #/changeOrder:)
			 #@""))
	     (#/setTag: item tag)
	     (#/setEnabled: item t))
    (#/addItem: view-menu (#/separatorItem ns:ns-menu-item))
    (loop for name in (list #@"Fade" #@"Move In" #@"Push" #@"Reveal")
	  for tag in *valid-transitions*
	  do (setq item (#/addItemWithTitle:action:keyEquivalent:
			 view-menu
			 name
			 (objc:@selector #/changeTransition:)
			 #@""))
	     (#/setTag: item tag)
	     (#/setEnabled: item t))
    (#/addItem: view-menu (#/separatorItem ns:ns-menu-item))
    (loop for name in (list #@"2 seconds" #@"5 seconds" #@"10 seconds"
			    #@"30 seconds" #@"1 minute" #@"10 minutes"
			    #@"30 minutes")
	  for tag in *valid-durations*
	  do (setq item (#/addItemWithTitle:action:keyEquivalent:
			 view-menu
			 name
			 (objc:@selector #/changeDuration:)
			 #@""))
	     (#/setTag: item tag)
	     (#/setEnabled: item t))
    (#/addItem: view-menu (#/separatorItem ns:ns-menu-item))
    (setq item (#/addItemWithTitle:action:keyEquivalent:
			 view-menu
			 #@"Keep Window On Top"
			 (objc:@selector #/changeOnTop:)
			 #@""))
    (#/setTag: item $keep-on-top)
    (#/setEnabled: item t)

    (setq item (#/insertItemWithTitle:action:keyEquivalent:atIndex: main-menu
							 #@"View"
							 +null-ptr+
							 #@""
							 2))
    (#/setSubmenu:forItem: main-menu view-menu item)
    (#/release view-menu)
    (setf (slot-value *dpf-controller* 'view-menu) view-menu)
    (#/setDelegate: view-menu *dpf-controller*)))

(defun make-albums-menu ()
  (let* ((albums (iphoto-library-albums *iphoto-library*))
	 (names (mapcar #'iphoto-album-name albums))
	 (menu (make-instance 'ns:ns-menu :with-title #@"iPhoto Albums")))
    (dolist (n names)
      (with-cfstring (s n)
	(let ((item (#/addItemWithTitle:action:keyEquivalent:
		     menu
		     s
		     (objc:@selector #/newSlideshowFromAlbum:)
		     #@"")))
	  (#/setTarget: item *dpf-controller*))))
    (#/autorelease menu)))

(defconstant $prefs-item-tag 100)

(defun retarget-preferences-menu-item ()
  (let* ((main-menu (#/mainMenu (#/sharedApplication ns:ns-application)))
	 (app-menu (#/submenu (#/itemAtIndex: main-menu 0)))
	 (prefs-item (#/itemWithTag: app-menu $prefs-item-tag)))
    (#/setAction: prefs-item (objc:@selector #/showPreferences:))
    (#/setTarget: prefs-item *dpf-controller*)))

(defun init-slideshow ()
  (init-supported-image-types)
  (init-dpf-controller)
  (setq *iphoto-library*
	(make-instance 'iphoto-library
		       :album-data-pathname
		       (merge-pathnames "Pictures/iPhoto Library/AlbumData.xml"
					(user-homedir-pathname))))
  (gui::execute-in-gui #'(lambda ()
			   (retarget-preferences-menu-item)
			   (make-view-menu)
                           (add-slideshow-menu))))

(defun make-slideshow (assets title)
  (ns:with-ns-rect (r 0 0 500 310)
    (let* ((w (#/initWithContentRect:styleMask:backing:defer:
	       (#/alloc (objc:@class "SlideshowWindow"))
	       r
	       (logior ;#$NSHUDWindowMask
		       #$NSUtilityWindowMask
		       #$NSTitledWindowMask
		       #$NSClosableWindowMask
		       #$NSResizableWindowMask)
	       #$NSBackingStoreBuffered
	       #$NO))
	   (wc (#/initWithWindow:
		(#/alloc (objc:@class "SlideshowWindowController"))
		w)))
      (#/release w)
      (#/setMovableByWindowBackground: w t)
      (#/setHidesOnDeactivate: w nil)
      (#/setFloatingPanel: w nil)
      (#/setDelegate: w wc)
      (if (directory-pathname-p title)
	(with-cfstring (s (native-translated-namestring title))
	  (#/setTitleWithRepresentedFilename: w s)
	  (#/setFrameAutosaveName: w s)
	  (unless (#/setFrameUsingName: w s)
	    (#/center w)))
	(with-cfstring (s (native-translated-namestring title))
	  (#/setTitle: w s)
	  (#/setFrameAutosaveName: w s)
	  (unless (#/setFrameUsingName: w s)
	    (#/center w))))
      (let* ((v (#/initWithFrame: (#/alloc (objc:@class "SlideshowView"))
				  (#/bounds (#/contentView w)))))
	(#/setAutoresizingMask: v (logior #$NSViewWidthSizable #$NSViewHeightSizable))
	(#/setContentView: w v)
	(#/release v)
	(setf (slideshow-view wc) v))
      (setq assets (sort-assets-by assets *slide-order*))
      (setf (slideshow-assets wc) assets)
      (#/showWindow: wc +null-ptr+)
      (#/advanceSlideBy: wc 0)
      (#/startTimer wc))))

(defun make-slideshow-from-folder (dir)
  (let ((image-pathnames (image-files-in-directory dir)))
    (if (null image-pathnames)
      (let ((message (format nil "No readable image files were found in \"~a\"" (native-translated-namestring dir))))
	(with-cfstring (m message)
	  (#_NSRunAlertPanel #@"No Images Found" m
			     #@"OK" +null-ptr+ +null-ptr+)))
      (let ((assets (make-array (length image-pathnames))))
	(map-into assets #'(lambda (p)
			     (make-instance 'asset :pathname p
					    :date (file-write-date p)))
		  image-pathnames)
	(make-slideshow assets dir)))))

(defun make-slideshow-from-album (album)
  (let ((photos (iphoto-album-photos album)))
    (if (null photos)
      (let ((message (format nil "No photos were found in the album \"~a\""
			     (iphoto-album-name album))))
	(with-cfstring (m message)
	  (#_NSRunAlertPanel #@"No Images Found" m
			     #@"OK" +null-ptr+ +null-ptr+)))
      (let ((assets (coerce photos 'vector)))
	(make-slideshow assets (iphoto-album-name album))))))

;;; hack-o-rama.  better than having to alter the ide sources, though.

(defclass dpf-application (gui::cocoa-application)
  ())

(defmethod toplevel-function ((a dpf-application) init-file)
  (declare (ignore init-file))
  (process-run-function "initialize DPF"
			#'(lambda ()
			    (wait-on-semaphore
			     gui::*cocoa-application-finished-launching*)
			    (objc:with-autorelease-pool 
				(init-slideshow))))
  (call-next-method))
