(in-package "CL-USER")

(setq ccl:*save-source-locations* nil)

(require "COCOA")

(defparameter *source-dir* (make-pathname :name nil :type nil
					  :defaults *load-truename*))
(defparameter *build-dir* (merge-pathnames "build/" *source-dir*))

(defparameter *dpf-files* '("package" "misc" "window" "dpf"))

(defvar *bundle-dir*)
(defvar *contents-dir*)
(defvar *resources-dir*)
(defvar *macos-dir*)

(defvar *ibtool-program* "/Developer/usr/bin/ibtool")

(defparameter *resource-files* '("Credits.html"
				 "help.html"
				 "DPF.icns"))

(defun compile-xib (xib &optional output-nib)
  (unless output-nib
    (setq output-nib (make-pathname :type "nib" :defaults xib)))
  (with-output-to-string (s)
    (let* ((nib-namestring (native-translated-namestring output-nib))
           (xib-namestring (native-translated-namestring xib))
           (p (run-program *ibtool-program*
                           (list "--errors" "--warnings" "--notices"
                                 "--output-format" "human-readable-text"
                                 "--compile" nib-namestring
                                 xib-namestring)
                           :output s
                           :error :output)))
      (multiple-value-bind (status exit-code)
          (external-process-status p)
        (unless (and (eq :exited status)
                     (zerop exit-code))
          (error "Error compiling xib file ~s:~%~a" xib
                 (or (ccl::describe-external-process-failure p "no ibtool?")
                     (get-output-stream-string s))))))))

(defun build-dpf (&optional (build-dir *build-dir*))
  (let* ((*build-dir* build-dir)
	 (*bundle-dir* (merge-pathnames "DPF.app/" *build-dir*))
	 (*contents-dir* (merge-pathnames "Contents/" *bundle-dir*))
	 (*resources-dir* (merge-pathnames "Resources/" *contents-dir*))
	 (*macos-dir* (merge-pathnames "MacOS/" *contents-dir*))
	 (*default-pathname-defaults* *source-dir*))
    (format t "~&Building from ~s, output to ~s" *source-dir* *build-dir*)
    (ensure-directories-exist *resources-dir*)
    (ensure-directories-exist (merge-pathnames "ccl/" *resources-dir*))
    (ensure-directories-exist *macos-dir*)
    (copy-file "Info.plist" (merge-pathnames "Info.plist" *contents-dir*)
	       :if-exists :supersede)
    (dolist (x (directory (merge-pathnames "en.lproj/*.xib")))
      (let ((dest (make-pathname :name (pathname-name x)
				 :type "nib"
				 :defaults (merge-pathnames "en.lproj"
							    *resources-dir*))))
	(compile-xib x dest)))
    (dolist (f *resource-files*)
      (copy-file f (merge-pathnames f *resources-dir*)
		 :if-exists :supersede))
    (dolist (f *dpf-files*)
      (let* ((src (make-pathname :name f
				 :type (pathname-type *.lisp-pathname*)
				 :defaults *source-dir*))
	       
	     (dst (make-pathname :name f
				 :type (pathname-type *.fasl-pathname*)
				 :defaults *build-dir*)))
	(compile-file src :output-file dst :verbose t :load t)))
    (copy-file (ccl::kernel-path) (merge-pathnames "DPF" *macos-dir*)
	       :if-exists :supersede
	       :preserve-attributes t)
    (format t "~&saving...~%")
    (finish-output t)
    (save-application (merge-pathnames "ccl/DPF.image" *resources-dir*)
		      :application-class (find-symbol "DPF-APPLICATION"
						      "DPF"))))

(require 'cocoa)
(ccl::define-special-objc-word "DPF")
;; Core Animation lives in QuartzCore
(objc:load-framework "QuartzCore" :quartzcore)

(build-dpf)
