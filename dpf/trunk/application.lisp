(in-package "CCL")

(defclass cocoa-event-process (process)
  ())

(defmethod process-interrupt ((process cocoa-event-process) function
			      &rest args)
  (if (eq process *current-process*)
    (apply function args)
    (if (and *nsapp* (#/isRunning *nsapp*))
      (queue-for-event-process #'(lambda () (apply function args)))
      (call-next-method))))

(defmethod process-exit-application :before ((process cocoa-event-process)
					     (thunk t))
  (when (eq process *initial-process*)
    (#/terminate: *nsapp* +null-ptr+)))

(defclass cocoa-ui-object (ui-object)
  ())

(defclass cocoa-application (application)
  ((standalone-p :initform nil :accessor cocoa-application-standalone-p)))

(defparameter *cocoa-application*
  (make-instance 'cocoa-application
		 :ui-object (make-instance 'cocoa-ui-object)))

(defmethod application-error ((a cocoa-application) condition error-pointer)
  (break-loop-handle-error condition error-pointer))

(defmethod parse-application-arguments ((a cocoa-application))
  (values nil nil nil nil))

(defmethod toplevel-function ((a cocoa-application) init-file)
  (declare (ignore init-file))
  (setf (cocoa-application-standalone-p a) t)
  (prepare-cocoa-application a)
  (start-cocoa-application a))

(defmethod prepare-cocoa-application ((a cocoa-application))
  (call-in-initial-process #'(lambda ()
			       (setq *nsapp* (load-cocoa-application a))
			       (initialize-user-interface a))))

(defun install-menus ()
  (let* ((main-menu (make-instance ns:ns-menu :with-title #@"MainMenu"))
	 (item nil)
	 (app-menu (make-instance ns:ns-menu :with-title #@"Apple")))
    (setq item (#/addItemWithTitle:action:keyEquivalent: app-menu
							 #@"Quit"
							 (objc:@selector
							  #/terminate:)
							 #@"q"))
    (#/setTarget: item *nsapp*)
    (setq item (#/addItemWithTitle:action:keyEquivalent: main-menu
							 #@"Apple" +null-ptr+
							 #@""))
    (#/setSubmenu:forItem: main-menu app-menu item)
    (#/setMainMenu: *nsapp* main-menu)
    (#/release main-menu)
    (#/release app-menu)))

(defmethod initialize-user-interface ((a cocoa-application))
  (with-autorelease-pool
    (let* ((bundle (#/mainBundle ns:ns-bundle))
	   (info (#/infoDictionary bundle))
	   (mainnibname (#/objectForKey: info #@"NSMainNibFile")))
      (when (%null-ptr-p mainnibname)
        (install-menus)))))

(defmethod load-cocoa-application ((a cocoa-application))
  (with-autorelease-pool
    (let* ((bundle (#/mainBundle ns:ns-bundle))
	   (info (#/infoDictionary bundle))
	   (classname (#/objectForKey: info #@"NSPrincipalClass"))
	   (mainnibname (#/objectForKey: info #@"NSMainNibFile"))
	   (progname (#/objectForKey: info #@"CFBundleName")))
      (when (%null-ptr-p classname)
	(setq classname #@"NSApplication"))
      (unless (%null-ptr-p progname)
	(#/setProcessName: (#/processInfo ns:ns-process-info) progname))
      (let* ((appclass (#_NSClassFromString classname))
	     (app (#/sharedApplication appclass)))
	(when (%null-ptr-p app)
	  (error "Could not create shared instance of ~s" (%get-cfstring
							   classname)))
	(unless (%null-ptr-p mainnibname)
          (#/loadNibNamed:owner:topLevelObjects: bundle mainnibname app +null-ptr+))
	app))))

(defun become-foreground-application ()
  #+apple-objc
  (rlet ((psn #>ProcessSerialNumber))
    (#_GetCurrentProcess psn)
    (#_TransformProcessType psn #$kProcessTransformToForegroundApplication)))

(defun event-loop ()
  (loop
    (with-simple-restart (abort "Process the next event")
      (#/run *nsapp*))))

(defun run-event-loop ()
  (%set-toplevel nil)
  (assert (eq *current-process* *initial-process*))
  (change-class *initial-process* 'cocoa-event-process)
  (setf (process-name *initial-process*) "Cocoa event loop")
  (with-process-whostate ("Active")
    (event-loop)))

;; After the lisp starts up, it creates a listener thread.  The
;; initial thead then goes to sleep, waking up about 3 times a second
;; to do some housekeeping tasks.
;;
;; Because most of Cocoa needs to run on the initial thread, we
;; interrupt the initial thread, and use %set-toplevel and toplevel
;; (which are basically process-preset and process-reset for the
;; initial process) to make it start running the Cocoa event loop.  We
;; create a new thread to do the housekeeping.

(defmethod start-cocoa-application ((a cocoa-application))
  (flet ((startup ()
           (with-standard-initial-bindings
             (process-run-function "housekeeping" #'housekeeping-loop)
             (become-foreground-application)
	     (run-event-loop))))
    (process-interrupt *initial-process*
                       #'(lambda ()
                           (%set-toplevel #'startup)
                           (toplevel)))))
