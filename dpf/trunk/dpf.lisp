(in-package "DPF")

(defun choose-directory-dialog (&optional dir)
  (let* ((r (ns:make-ns-rect 0 0 200 25))
	 (b (#/initWithFrame: (#/alloc ns:ns-button) r)))
    (#/setButtonType: b #$NSSwitchButton)
    (#/setTitle: b #@"Look for pictures in subfolders")
    (ccl::call-in-initial-process
     #'(lambda ()
	 (let ((op (#/openPanel ns:ns-open-panel)))
	   (#/retain op)
	   (#/setAllowsMultipleSelection: op nil)
	   (#/setCanChooseDirectories: op t)
	   (#/setCanChooseFiles: op nil)
	   (#/setDirectoryURL: op +null-ptr+)
	   (#/setAccessoryView: op b)
	   (#/release b)
	   (when dir
	     (with-cfurl (u dir)
	       (#/setDirectoryURL: op u)))
	   (when (eql (#/runModalForTypes: op +null-ptr+) #$NSOKButton)
	     (let* ((include-subdirs (= 1 (#/state b)))
		    (u (#/directoryURL op))
		    (path (#_CFURLCopyFileSystemPath u
						     #$kCFURLPOSIXPathStyle)))
	       (#/release op)
	       (multiple-value-prog1
		   (values
		    (ensure-directory-pathname (%get-cfstring path))
		    include-subdirs)
		 (#_CFRelease path)))))))))

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

(defvar *slide-duration*)
(defvar *slide-order*)
(defvar *slide-transition*)
(defvar *slide-on-top-p*)

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


(defvar *help-window-controller* nil)

(defclass dpf-help-window-controller (ns:ns-window-controller)
  ((checkbox :foreign-type :id)
   (text-view :foreign-type :id))
  (:metaclass ns:+ns-object))

(objc:defmethod #/init ((self dpf-help-window-controller))
  (#/initWithWindowNibName: self #@"help"))

(objc:defmethod (#/windowDidLoad :void) ((self dpf-help-window-controller))
  (let* ((defaults (#/standardUserDefaults ns:ns-user-defaults))
	 (show-window (#/boolForKey: defaults #@"show-help-window"))
	 (bundle (#/mainBundle ns:ns-bundle))
	 (url (#/URLForResource:withExtension: bundle #@"help" #@"html")))
  (#/setState: (slot-value self 'checkbox)
	       (if show-window #$NSOnState #$NSOffState))
  (#/center (#/window self))
  (unless (%null-ptr-p url)
    (let ((string (#/initWithURL:documentAttributes:
		   (#/alloc ns:ns-attributed-string)
		   url
		   +null-ptr+))
	  (text-storage (#/textStorage (slot-value self 'text-view))))
      (#/setAttributedString: text-storage string)
      (#/release string)))))

(objc:defmethod (#/closeHelpWindow: :void) ((self dpf-help-window-controller)
					    sender)
  (declare (ignore sender))
  (#/close (#/window self)))

(objc:defmethod (#/toggleHelpWindowCheckbox: :void)
    ((self dpf-help-window-controller) sender)
  (let ((state (#/state sender))
	(defaults (#/standardUserDefaults ns:ns-user-defaults)))
    (#/setBool:forKey: defaults state #@"show-help-window")))

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

(defun image-files-in-directory (pathname &key recursive)
  (let* ((dir (ensure-directory-pathname pathname))
	 (wild nil))
    (when recursive
      (setq dir (merge-pathnames #p"**/" dir)))
    (setq wild (make-pathname :name :wild :type :wild :defaults dir))
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

(defclass iphoto-asset (asset)
  ())

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
	     (asset (make-instance 'iphoto-asset)))
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
	       (#/numberWithInt: ns:ns-number $5-seconds)
	       #@"duration"
	       (#/numberWithInt: ns:ns-number $transition-fade)
	       #@"transition"
	       (#/numberWithInt: ns:ns-number $order-by-name)
	       #@"order"
	       (#/numberWithBool: ns:ns-number t)
	       #@"on-top-p"
	       (#/numberWithBool: ns:ns-number t)
	       #@"show-help-window"
	       +null-ptr+)))
    (#/registerDefaults: defaults dict)
    (setq *slide-duration* (#/integerForKey: defaults #@"duration")
	  *slide-transition* (#/integerForKey: defaults #@"transition")
	  *slide-order* (#/integerForKey: defaults #@"order")
	  *slide-on-top-p* (#/boolForKey: defaults #@"on-top-p"))))

(objc:defmethod (#/dealloc :void) ((self dpf-controller))
  (let ((nc (#/defaultCenter ns:ns-notification-center)))
    (#/removeObserver: nc self))
  (call-next-method))

(defconstant $from-folder-tag 0)
(defconstant $from-iphoto-tag 1)

(objc:defmethod (#/newSlideshow: :void) ((self dpf-controller) sender)
  (let ((tag (#/tag sender))
	(plist nil))
    (cond ((= tag $from-folder-tag)
	   (multiple-value-bind (dir include-subdirs)
	       (choose-directory-dialog)
	     (when include-subdirs
	       (setq plist (list :include-subdirs t)))
	     (when dir
	       (make-slideshow-from-folder dir plist))))
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

(objc:defmethod (#/showHelpWindow: :void) ((self dpf-controller) sender)
  (declare (ignore sender))
  (when (null *help-window-controller*)
    (setq *help-window-controller*
	  (#/init (#/alloc (objc:@class "DPFHelpWindowController")))))
  (#/showWindow: *help-window-controller* +null-ptr+))

(defparameter *saved-state-directory*
  #p"home:Library;Application Support;com.clozure.PictureWindow;")

(defparameter *saved-state-filename* "state.sexp")

(defparameter *restoring-slideshow-state* nil
  "bound to t when restoring slideshow state")

(defun maybe-show-help-window ()
  (let ((defaults (#/standardUserDefaults ns:ns-user-defaults)))
    (when (#/boolForKey: defaults #@"show-help-window")
      (#/showHelpWindow: *dpf-controller* +null-ptr+)))
  #+nil
  (unless (probe-file (merge-pathnames *saved-state-filename*
				       *saved-state-directory*))
    (let ((defaults (#/standardUserDefaults ns:ns-user-defaults)))
      (when (#/boolForKey: defaults #@"show-help-window")
	(#/showHelpWindow: *dpf-controller* +null-ptr+)))))

(defun save-slideshow-state ()
  (let ((path *saved-state-directory*))
    (unless (probe-file path)
      (ensure-directories-exist path))
    (if (not (directoryp path))
      (with-cstrs ((s path))
	(#_NSLog #@"Path %s is not a directory, not saving state."
		 :address s))
      (let ((slideshows (slideshow-window-controllers)))
	(if (null slideshows)
	  (ignore-errors
	    (delete-file (merge-pathnames *saved-state-filename* path)))
	  (with-open-file (output (merge-pathnames *saved-state-filename* path)
				  :direction :output :if-exists :supersede)
	    (with-standard-io-syntax
	      (dolist (s slideshows)
		(print (list (slideshow-source s)
			     (slideshow-duration s)
			     (slideshow-transition s)
			     (slideshow-order s)
			     (slideshow-on-top-p s)
			     (slideshow-current-index s)
			     (slideshow-include-subdirs s))
		       output)))))))))

(defun restore-slideshow (state)
  (destructuring-bind (source duration transition order on-top-p current-index
			      &optional include-subdirs)
      state
    (if (and (member duration *valid-durations*)
	     (member transition *valid-transitions*)
	     (member order *valid-orders*)
	     (typep on-top-p 'boolean)
	     (typep current-index '(integer 0))
	     (typep include-subdirs 'boolean))
      (let ((plist (list :duration duration :transition transition
			 :order order :on-top-p on-top-p
			 :current-index current-index
			 :include-subdirs include-subdirs)))
	(if (pathnamep source)
	  (make-slideshow-from-folder source plist)
	  (let ((album (find source (iphoto-library-albums *iphoto-library*)
			     :key #'iphoto-album-name
			     :test #'string=)))
	    (when album
	      (make-slideshow-from-album album plist))))))))

(defun restore-slideshow-state ()
  (let ((path (merge-pathnames *saved-state-filename* *saved-state-directory*)))
    (if (shift-key-now-p)
      (ignore-errors (delete-file path))
      (when (probe-file path)
	(with-open-file (input path)
	  (with-standard-io-syntax
	    (let ((*read-eval* nil)
		  (*restoring-slideshow-state* t))
	      (loop for x = (read input nil)
		    while x
		    do (restore-slideshow x)))))))))
  
(objc:defmethod (#/applicationWillTerminate: :void) ((self dpf-controller)
						     notification)
  (declare (ignore notification))
  (multiple-value-bind (result condition)
      (ignore-errors (save-slideshow-state))
    (declare (ignore result))
    (if condition
      (ignore-errors (delete-directory *saved-state-directory*)))))

(objc:defmethod (#/applicationDidBecomeActive: :void) ((self dpf-controller)
						       notification)
  (declare (ignore notification))
  ;;(#_NSLog #@"applicationDidBecomeActive:")
  (dolist (wc (slideshow-window-controllers))
    (#/setAlphaValue: (#/animator (#/window wc))
		      (float 1.0 ccl::+cgfloat-zero+))))

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
  (setq *dpf-controller* (make-instance 'dpf-controller))
  (let ((nc (#/defaultCenter ns:ns-notification-center)))
    (#/addObserver:selector:name:object:
     nc *dpf-controller* (objc:@selector #/applicationWillTerminate:)
     #&NSApplicationWillTerminateNotification +null-ptr+)
    (#/addObserver:selector:name:object:
     nc *dpf-controller* (objc:@selector #/applicationDidBecomeActive:)
     #&NSApplicationDidBecomeActiveNotification +null-ptr+)))


(defclass slideshow-window (ns:ns-window)
  ()
  (:metaclass ns:+ns-object))

;;; Instances of this class manage a running slideshow.
(defclass slideshow-window-controller (ns:ns-window-controller)
  ((view :foreign-type :id :accessor slideshow-view)
   (timer :foreign-type :id :accessor slideshow-timer)
   (remaining :accessor slideshow-remaining :initform nil)
   (duration :accessor slideshow-duration)
   (user-paused-p :initform nil :accessor slideshow-user-paused-p)
   (transition :accessor slideshow-transition)
   (order :accessor slideshow-order)
   (on-top-p :reader slideshow-on-top-p)
   (assets :initform nil :accessor slideshow-assets)
   (current-index :initform 0 :accessor slideshow-current-index)
   (source :initform nil :accessor slideshow-source)
   (include-subdirs :initform nil :accessor slideshow-include-subdirs))
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
    (when (lionp)
      ;; enable fullscreen
      (objc:objc-message-send (#/window x)
			      "setCollectionBehavior:"
			      #>NSUInteger
			      ;; NSWindowCollectionBehaviorFullScreenPrimary
			      (logior (ash 1 7)
				      #$NSWindowCollectionBehaviorManaged)))
    (#/setHasShadow: (#/window x) nil)
    (when on-top-p
      (#/setLevel: (#/window x)
		   (#_CGWindowLevelForKey #$kCGFloatingWindowLevelKey)))))

(defmethod (setf slideshow-on-top-p) (new (x slideshow-window-controller))
  (with-slots (on-top-p) x
    (unless (eq new on-top-p)
      (setf on-top-p new)
      ;; Weird C Preprocessor Tricks prevent the interface
      ;; translator from figuring out what #$NSNormalWindowLevel etc.
      ;; should be.  (See CGWindow.h)
      (#/setLevel: (#/window x)
		   (if on-top-p
		     (#_CGWindowLevelForKey #$kCGFloatingWindowLevelKey)
		     (#_CGWindowLevelForKey #$kCGNormalWindowLevelKey))))))

(defmethod slideshow-current-asset ((x slideshow-window-controller))
  (with-slots (assets current-index) x
    (elt assets current-index)))

(objc:defmethod (#/dealloc :void) ((self slideshow-window-controller))
  ;;(#_NSLog #@"slideshow-window-controller dealloc")
  (objc:remove-lisp-slots self)
  (#/stopTimer self)
  (call-next-method))

(objc:defmethod (#/stopTimer :void) ((self slideshow-window-controller))
  (with-slots (timer) self
    (#/invalidate timer)
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

(objc:defmethod (#/resumeSlideshowHelper: :void)
    ((self slideshow-window-controller) timer)
  (#/stopTimer self)
  (#/nextSlide: self timer)
  (#/startTimer self))

(objc:defmethod (#/resumeSlideshow :void) ((self slideshow-window-controller))
  (with-slots (timer elapsed duration remaining) self
    (if (%null-ptr-p timer)
      (let* ((secs (duration-to-seconds duration)))
	;; only wait the remaining time
	(when remaining
	  (setq secs remaining)
	  (setq remaining nil))
	(setq timer (#/timerWithTimeInterval:target:selector:userInfo:repeats:
		     ns:ns-timer
		     (float secs 0d0)
		     self (objc:@selector #/resumeSlideshowHelper:)
		     +null-ptr+
		     #$NO))
	(#/addTimer:forMode: (#/currentRunLoop ns:ns-run-loop)
			     timer #&NSRunLoopCommonModes)))))

(objc:defmethod (#/pauseSlideshow :void) ((self slideshow-window-controller))
  (with-slots (timer remaining) self
    (when (#/isValid timer)
      (let* ((fire-date (#/fireDate timer))
	     (time-left (#/timeIntervalSinceNow fire-date)))
	(setq remaining time-left)))
    (#/stopTimer self)))

(objc:defmethod (#/windowWillClose: :void) ((self slideshow-window-controller) notification)
  (declare (ignore notification))
  ;; This timer retains us (its target), so get rid of it here.
  (#/stopTimer self)
  (#/autorelease self))

(objc:defmethod (#/setSlideshowDuration: :void) ((self slideshow-window-controller)
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
          (let ((rep (#/objectAtIndex: (#/representations image) 0)))
            (when (typep rep 'ns:ns-bitmap-image-rep)
              (ns:with-ns-size (s (#/pixelsWide rep) (#/pixelsHigh rep))
                (#/setSize: image s))))
          (#/showImage: view image)
          (#/release image))))))

(objc:defmethod (#/nextSlide: :void) ((self slideshow-window-controller) timer)
  (declare (ignore timer))
  (#/advanceSlideBy: self 1))

(objc:defmethod (#/showInFinder: :void) ((self slideshow-window-controller)
					 sender)
  (declare (ignore sender))
  (let ((p (asset-pathname (slideshow-current-asset self))))
    (when (pathnamep p)
      (with-cfstring (s (native-translated-namestring p))
	(let* ((workspace (#/sharedWorkspace ns:ns-workspace))
	       (file (#/stringByResolvingSymlinksInPath s))
	       (dir (#/stringByDeletingLastPathComponent file)))
	  (#/selectFile:inFileViewerRootedAtPath: workspace file dir))))))

(objc:defmethod (#/openInPreview: :void) ((self slideshow-window-controller)
					 sender)
  (declare (ignore sender))
  (let ((p (asset-pathname (slideshow-current-asset self))))
    (when (pathnamep p)
      (with-cfstring (s (native-translated-namestring p))
	(let* ((workspace (#/sharedWorkspace ns:ns-workspace))
	       (file (#/stringByResolvingSymlinksInPath s)))
	  (#/openFile:withApplication: workspace file #@"Preview.app"))))))

(objc:defmethod (#/validateMenuItem: #>BOOL) ((self slideshow-window-controller)
					      item)
  (cond ((or (eql (#/action item) (objc:@selector #/showInFinder:))
	     (eql (#/action item) (objc:@selector #/openInPreview:)))
	 (not (typep (slideshow-current-asset self) 'iphoto-asset)))
	(t t)))
    
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
  (when (= (#/tag sender) $keep-on-top)
    (setf (slideshow-on-top-p self) (not (slideshow-on-top-p self)))))

(defun slideshow-window-controllers ()
  (let ((array (#/windows (#/sharedApplication ns:ns-application)))
	(controllers nil))
    (dotimes (i (#_CFArrayGetCount array) controllers)
      (let* ((w (#_CFArrayGetValueAtIndex array i))
	     (wc (#/windowController w)))
	(when (typep wc 'slideshow-window-controller)
	  (push wc controllers))))))

(objc:defmethod (#/windowWillEnterFullScreen: :void) 
                ((self slideshow-window-controller) notification)
  (declare (ignore notification))
  (let ((w (#/window self)))
    (#/setBackgroundColor: w *black-color*)
    (hide-titlebar (#/window self) :now)))

(objc:defmethod (#/windowWillExitFullScreen: :void)
                ((self slideshow-window-controller) notification)
  (declare (ignore notification))
  (let ((w (#/window self)))
    (#/setBackgroundColor: w *clear-color*)))

(objc:defmethod (#/windowDidExitFullScreen: :void)
                ((self slideshow-window-controller) notification)
  (declare (ignore notification))
  (maybe-show-titlebar (#/window self)))

(objc:defmethod (#/menuWillOpen: :void) ((self slideshow-window-controller)
					 menu)
  (declare (ignore menu))
  (unless (slideshow-user-paused-p self)
    (#/pauseSlideshow self)))

(objc:defmethod (#/menuDidClose: :void) ((self slideshow-window-controller)
					 menu)
  (declare (ignore menu))
  (unless (slideshow-user-paused-p self)
    (#/resumeSlideshow self)))

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
  ((image-view :accessor image-view :foreign-type :id)
   (tracking-area :accessor tracking-area :foreign-type :id))
  (:metaclass ns:+ns-object))

(objc:defmethod #/initWithFrame: ((self slideshow-view) (frame #>NSRect))
  (call-next-method frame)
  (#/setTransition: self *slide-transition*)
  (let* ((ta (#/initWithRect:options:owner:userInfo:
	      (#/alloc ns:ns-tracking-area)
	      frame
	      (logior #$NSTrackingMouseEnteredAndExited
		      #$NSTrackingActiveAlways
		      #$NSTrackingEnabledDuringMouseDrag)
	      self
	      +null-ptr+)))
    (#/addTrackingArea: self ta)
    (setf (tracking-area self) ta))
  self)

(objc:defmethod (#/updateTrackingAreas :void) ((self slideshow-view))
  ;;(#_NSLog #@"updateTrackingAreas")
  (let* ((ta (#/initWithRect:options:owner:userInfo:
	      (#/alloc ns:ns-tracking-area)
	      (#/bounds self)
	      (logior #$NSTrackingMouseEnteredAndExited
		      #$NSTrackingActiveAlways
		      #$NSTrackingEnabledDuringMouseDrag)
	      self
	      +null-ptr+)))
    (#/removeTrackingArea: self (tracking-area self))
    (#/release (tracking-area self))
    (setf (tracking-area self) ta)
    (#/addTrackingArea: self ta)))

(objc:defmethod (#/mouseEntered: :void) ((self slideshow-view) e)
  (declare (ignore e))
  (block method
    (let* ((w (#/window self))
	   (wc (#/windowController w))
	   (active-p (#/isActive (#/sharedApplication ns:ns-application))))
      (when (fullscreen-window-p w)
	(return-from method))
      (when active-p
	(show-titlebar w))
      (when (and (slideshow-on-top-p wc)
		 (not active-p))
	(#/setAlphaValue: (#/animator w) (float 0.1 ccl::+cgfloat-zero+))))))

(objc:defmethod (#/mouseExited: :void) ((self slideshow-view) e)
  (declare (ignore e))
  (block method
    (let* ((w (#/window self))
	   (wc (#/windowController w))
	   (active-p (#/isActive (#/sharedApplication ns:ns-application))))
      (when (fullscreen-window-p w)
	(return-from method))
      (when (and (slideshow-on-top-p wc)
		 (not active-p))
	(#/setAlphaValue: (#/animator w) (float 1.0 ccl::+cgfloat-zero+)))
      (hide-titlebar w))))

(objc:defmethod (#/dealloc :void) ((self slideshow-view))
  (with-slots (image-view tracking-area) self
    (#/release image-view)
    (#/release tracking-area))
  (call-next-method))

(objc:defmethod (#/setTransition: :void) ((self slideshow-view)
					  (transition-code #>NSInteger))
  (let ((transition (#/animation ns:ca-transition)))
    (#/setDuration: transition 1d0)
    (#/setType: transition (transition-code-to-type transition-code))
    (#/setSubtype: transition #&kCATransitionFromLeft)
    (#/setAnimations: self (#/dictionaryWithObject:forKey: ns:ns-dictionary
                                                           transition
                                                           #@"subviews"))))

(objc:defmethod (#/drawRect: :void) ((self slideshow-view) (dirty #>NSRect))
  (#/set *clear-color*)
  (#_NSRectFill (#/bounds self)))
  
(objc:defmethod (#/mouseDownCanMoveWindow #>BOOL) ((self slideshow-view))
  #$YES)

(objc:defmethod (#/acceptsFirstResponder #>BOOL) ((self slideshow-view))
  #$YES)

(objc:defmethod (#/mouseUp: :void) ((self slideshow-view) e)
  ;;(#_NSLog #@"mouseUp:")
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
           (#/setSlideshowDuration: wc (slideshow-duration wc)))
          ((= unichar #$NSRightArrowFunctionKey)
           (#/advanceSlideBy: wc 1)
           (#/setSlideshowDuration: wc (slideshow-duration wc)))
	  ((= unichar (char-code #\space))
	   (if (slideshow-user-paused-p wc)
	     (progn
	       (setf (slideshow-user-paused-p wc) nil)
	       (#/resumeSlideshow wc))
	     (progn
	       (setf (slideshow-user-paused-p wc) t)
	       (#/pauseSlideshow wc))))
          (t 
           (call-next-method event)))))
  
(objc:defmethod (#/showImage: :void) ((self slideshow-view) image)
  (if (%null-ptr-p image)
    (format t "~&hey, no image here.")
    (let* ((bounds (#/bounds self))
	   (iv (#/initWithFrame: (#/alloc (objc:@class "DPFImageView"))
				 bounds))
	   (menu (#/initWithTitle: (#/alloc ns:ns-menu) #@"image view menu"))
	   (wc (#/windowController (#/window self)))
	   (item nil))
      (setq item (#/addItemWithTitle:action:keyEquivalent: menu #@"Show in Finder..." (objc:@selector #/showInFinder:) #@""))
      (#/setTarget: item wc)
      (setq item (#/addItemWithTitle:action:keyEquivalent: menu #@"Open in Preview..." (objc:@selector #/openInPreview:) #@""))
      (#/setTarget: item wc)
      (#/setMenu: iv menu)
      (#/setDelegate: menu wc)
      (#/release menu)
      (#/setImage: iv image)
      (#/setAutoresizingMask: iv (logior #$NSViewWidthSizable
					 #$NSViewHeightSizable))
      (if (and (not (%null-ptr-p (image-view self)))
               (not (%null-ptr-p iv)))
	(progn
	  (#/replaceSubview:with: (#/animator self) (image-view self) iv)
	  (#/setContentSize: (#/animator (#/window self))
			     (pref bounds #>NSRect.size)))
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
                #@"New Slideshow From Folder..."
                (objc:@selector #/newSlideshow:)
                #@"n"
                0))
    (#/setTag: item $from-folder-tag)
    (#/setTarget: item *dpf-controller*)
    (when *iphoto-library*
      (setq item (#/insertItemWithTitle:action:keyEquivalent:atIndex:
		  file-menu
		  #@"New Slideshow From iPhoto Album"
		  +null-ptr+
		  #@""
		  1))
      (#/setTag: item $from-iphoto-tag)
      (#/setSubmenu: item (make-albums-menu)))
    #+dpf-in-ide
    (progn
      (setq item (#/insertItemWithTitle:action:keyEquivalent:atIndex:
		  file-menu
		  #@"Slideshow Preferences..."
		  (objc:@selector #/showPreferences:)
		  #@""
		  2))
      (#/setTarget: item *dpf-controller*))))

(defun configure-help-menu ()
  (let* ((main-menu (#/mainMenu (#/sharedApplication ns:ns-application)))
	 (help-menu (#/submenu (#/itemWithTitle: main-menu #@"Help")))
	 (item (#/itemAtIndex: help-menu 0)))
    (#/setAction: item (objc:@selector #/showHelpWindow:))
    (#/setTarget: item *dpf-controller*)))

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
    (#/addItem: view-menu (#/separatorItem ns:ns-menu-item))
    (setq item (#/addItemWithTitle:action:keyEquivalent:
			 view-menu
			 #@"Enter Full Screen"
			 (objc:@selector #/toggleFullScreen:)
			 #@"f"))
    (#/setKeyEquivalentModifierMask: item (logior #$NSCommandKeyMask
						  #$NSControlKeyMask))

    (setq item (#/insertItemWithTitle:action:keyEquivalent:atIndex: main-menu
							 #@"View"
							 +null-ptr+
							 #@""
							 2))
    (#/setSubmenu:forItem: main-menu view-menu item)
    (setf (slot-value *dpf-controller* 'view-menu) view-menu)
    (#/setDelegate: view-menu *dpf-controller*)
    (#/release view-menu)))

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

(defun iphoto-root-directory ()
  (let ((p (#_CFPreferencesCopyAppValue #@"RootDirectory"
					#@"com.apple.iPhoto")))
    (if (%null-ptr-p p)
      (probe-file "~/Pictures/iPhoto Library/")
      (prog1
	  (probe-file (%get-cfstring p))
	(#_CFRelease p)))))

(defun init-slideshow ()
  (init-supported-image-types)
  (init-dpf-controller)
  (let ((dir (iphoto-root-directory)))
    (when dir
      (setq *iphoto-library*
	    (make-instance 'iphoto-library
			   :album-data-pathname
			   (merge-pathnames "AlbumData.xml" dir)))))
  (ccl::call-in-initial-process #'(lambda ()
				    (retarget-preferences-menu-item)
				    (make-view-menu)
				    (add-slideshow-menu)
				    (configure-help-menu)
				    (maybe-show-help-window)
				    (restore-slideshow-state))))

(defun make-slideshow (assets title source &optional plist)
  (ns:with-ns-rect (r 0 0 500 310)
    (let* ((w (#/initWithContentRect:styleMask:backing:defer:
	       (#/alloc (objc:@class "DPFWindow"))
	       r
	       (logior #$NSTitledWindowMask
		       #$NSClosableWindowMask
		       #$NSMiniaturizableWindowMask
		       #$NSResizableWindowMask)
	       #$NSBackingStoreBuffered
	       #$NO))
	   (wc (#/initWithWindow:
		(#/alloc (objc:@class "SlideshowWindowController"))
		w)))
      (#/release w)
      (#/setMovableByWindowBackground: w t)
      (#/setHidesOnDeactivate: w nil)
      (#/setDelegate: w wc)
      ;; set window title
      (if (directory-pathname-p title)
	(with-cfstring (s (native-translated-namestring title))
	  (#/setTitleWithRepresentedFilename: w s)
	  (#/addWindowsItem:title:filename: ccl::*nsapp* w
					    (#/lastPathComponent s) nil)
	  (#/setFrameAutosaveName: w s)
	  (unless (#/setFrameUsingName: w s)
	    ;; lower left corner
	    (#/setFrameOrigin: w #&NSZeroPoint)))
	(with-cfstring (s (native-translated-namestring title))
	  (#/setTitle: w s)
	  (#/addWindowsItem:title:filename: ccl::*nsapp* w s nil)
	  (#/setFrameAutosaveName: w s)
	  (unless (#/setFrameUsingName: w s)
	    ;; lower left corner
	    (#/setFrameOrigin: w #&NSZeroPoint))))
      ;; Use a plain black view as our content view.  The idea is
      ;; to avoid seeing through the window on certain transitions.
      (let* ((v (#/initWithFrame: (#/alloc (objc:@class "DPFBlackView"))
				  (#/bounds (#/contentView w)))))
	(#/setAutoresizingMask: v (logior #$NSViewWidthSizable
					  #$NSViewHeightSizable))
	(#/setContentView: w v)
	(#/release v))
      ;; We want our picture window to have rounded corners.  Do this
      ;; by creating a layer-hosting view and having it cover the
      ;; window's content view.  The layer is configured to have the
      ;; rounded corners and mask the sublayers.
      (let ((mask-view (#/initWithFrame: (#/alloc (objc:@class "DPFMaskView"))
					 (#/bounds (#/contentView w)))))
	(#/setAutoresizingMask: mask-view (logior #$NSViewWidthSizable
						  #$NSViewHeightSizable))
	(#/addSubview: (#/contentView w) mask-view)
	(#/release mask-view)
	;; Now add further subviews to the mask-view.
	(let* ((v (#/initWithFrame: (#/alloc (objc:@class "SlideshowView"))
				    (#/bounds (#/contentView w)))))
	  (#/setAutoresizingMask: v (logior #$NSViewWidthSizable
					    #$NSViewHeightSizable))
	  (#/addSubview: mask-view v)
	  (setf (slideshow-view wc) v)
	  (#/release v)
	  ;; add the window's titlebar as a sibling of the content, but
	  ;; ordered above it.
	  (let ((titlebar (slot-value w 'titlebar-view))
		(content-rect (#/bounds mask-view)))
	    (ns:with-ns-rect (r)
	      (setf (ns:ns-rect-x r) 0
		    (ns:ns-rect-y r) (- (ns:ns-rect-height content-rect) 23)
		    (ns:ns-rect-width r) (ns:ns-rect-width content-rect)
		    (ns:ns-rect-height r) 23)
	      (#/setFrame: titlebar r))
	    (#/addSubview:positioned:relativeTo: (#/contentView w)
						 titlebar
						 #$NSWindowAbove
						 v)
	    (#/setHidden: titlebar t))))
      (maybe-show-titlebar w)
      (setf (slideshow-source wc) source)
      (when plist
	(let (val)
	  (when (setq val (getf plist :include-subdirs))
	    (setf (slideshow-include-subdirs wc) val))
	  (when (setq val (getf plist :duration))
	    (setf (slideshow-duration wc) val))
	  (when (setq val (getf plist :transition))
	    (setf (slideshow-transition wc) val)
	    (#/setTransition: (slideshow-view wc) val))
	  (when (setq val (getf plist :order))
	    (setf (slideshow-order wc) val))
	  (when (setq val (getf plist :on-top-p))
	    (setf (slideshow-on-top-p wc) val))
	  (when (and (setq val (getf plist :current-index))
		     (< (length (slideshow-assets wc)) val))
	    (setf (slideshow-current-index wc) val))))
      (setq assets (sort-assets-by assets *slide-order*))
      (setf (slideshow-assets wc) assets)
      (#/showWindow: wc +null-ptr+)
      (#/advanceSlideBy: wc 0)
      (#/startTimer wc))))

(defun make-slideshow-from-folder (dir &optional plist)
  (let* ((recursive (getf plist :include-subdirs))
	 (image-pathnames (image-files-in-directory dir :recursive recursive)))
    (if (null image-pathnames)
      (unless *restoring-slideshow-state*
	(let ((message (format nil "No readable image files were found in \"~a\"" (native-translated-namestring dir))))
	  (with-cfstring (m message)
	    (#_NSRunAlertPanel #@"No Images Found" m
			       #@"OK" +null-ptr+ +null-ptr+))))
      (let ((assets (make-array (length image-pathnames))))
	(map-into assets #'(lambda (p)
			     (make-instance 'asset :pathname p
					    :date (file-write-date p)))
		  image-pathnames)
	(make-slideshow assets dir dir plist)))))

(defun make-slideshow-from-album (album &optional plist)
  (let ((photos (iphoto-album-photos album)))
    (if (null photos)
      (unless *restoring-slideshow-state*
	(let ((message (format nil "No photos were found in the album \"~a\""
			       (iphoto-album-name album))))
	  (with-cfstring (m message)
	    (#_NSRunAlertPanel #@"No Images Found" m
			       #@"OK" +null-ptr+ +null-ptr+))))
      (let ((assets (coerce photos 'vector)))
	(make-slideshow assets (iphoto-album-name album)
			(iphoto-album-name album) plist)))))



(defmethod ccl::initialize-user-interface :after ((a ccl::cocoa-application))
  (init-slideshow))
