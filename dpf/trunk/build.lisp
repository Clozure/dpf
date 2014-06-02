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

(defvar *ibtool-program* "ibtool")

(defparameter *resource-files* '("Credits.html"
				 "help.html"
				 "app.icns"))

(defun build-dpf (&optional (build-dir *build-dir*))
  (let* ((*build-dir* build-dir)
	 (*bundle-dir* (merge-pathnames "Picture Window.app/" *build-dir*))
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
    (copy-file (ccl::kernel-path) (merge-pathnames "Picture Window"
						   *macos-dir*)
	       :if-exists :supersede
	       :preserve-attributes t)
    (format t "~&saving...~%")
    (finish-output t)
    (save-application (merge-pathnames "ccl/Picture Window.image"
				       *resources-dir*)
		      :application-class (find-symbol "DPF-APPLICATION"
						      "DPF"))))

(require 'cocoa)
(ccl::define-special-objc-word "DPF")
;; Core Animation lives in QuartzCore
(objc:load-framework "QuartzCore" :quartzcore)

(build-dpf)
