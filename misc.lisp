(in-package "DPF")

(defun cgfloat (n)
  (float n #+32-bit-target 0s0
           #+64-bit-target 0d0))

;;; #$NSAppKitVersionNumber10_6 isn't in the interface database
(defconstant snow-leopard-appkit-version 1038d0)

(defun lionp ()
  (> (floor #$NSAppKitVersionNumber) snow-leopard-appkit-version))

(defconstant $fullscreen-window-mask-bit 14)

(defloadvar *black-color* (#/retain (#/blackColor ns:ns-color)))
(defloadvar *clear-color* (#/retain (#/clearColor ns:ns-color)))
