

#|

Meld PIGPIO-FFI and PIGPIOD-FFI, using the concpept of a PI-DEVICE that represents either
a local hardware connection (pigpio) or a socket connection to a daemon (pigpiod)

|#


(defpackage pigpio-ffi-universal
  (:use #:cl)
  (:shadowing-import-from #:pigpio-codes
			  #:*pigpio-errorcodes*
			  #:pigpio-error-code-name
			  #:handle-pigpio-error)
  (:export
   ;; export the imports first
   #:pigpio-codes
   #:pigpio-error-code-name
   #:handle-pigpio-error
   
   #:*default-pidev*
   #:pidev/hardware #:pidev/daemon 

   #:open-pidev/hardware
   #:open-pidev/daemon

   ;; the following functions 
   #:pigpio-gpiosetmode
   #:pigpio-gpiogetmode
   #:pigpio-gpiosetpullupdown
   #:pigpio-gpioread
   #:pigpio-gpiowrite
   #:pigpio-gpiopwm
   #:pigpio-gpiogetpwmdutycycle
   #:pigpio-gpiosetpwmrange
   #:pigpio-gpiogetpwmrange
   #:pigpio-gpiogetpwmrealrange
   #:pigpio-gpiosetpwmfrequency
   #:pigpio-gpiogetpwmfrequency
   #:pigpio-gpioservo
   #:pigpio-gpiogetservopulsewidth
   #:pigpio-gpionotifyopen
   #:pigpio-gpionotifybegin
   #:pigpio-gpionotifypause
   #:pigpio-gpionotifyclose
   #:pigpio-gpioglitchfilter
   #:pigpio-gpionoisefilter
   #:pigpio-gpiotrigger
   #:pigpio-gpiosetwatchdog
   #:pigpio-spiopen
   #:pigpio-spiclose
   #:pigpio-spiread
   #:pigpio-spiwrite
   #:pigpio-spixfer
   #:pigpio-bbspiopen
   #:pigpio-bbspiclose
   #:pigpio-bbspixfer
   #:pigpio-i2copen
   #:pigpio-i2cclose
   #:pigpio-i2cwritequick
   #:pigpio-i2cwritebyte
   #:pigpio-i2creadbyte
   #:pigpio-i2cwritebytedata
   #:pigpio-i2cwriteworddata
   #:pigpio-i2creadbytedata
   #:pigpio-i2creadworddata
   #:pigpio-i2cprocesscall
   #:pigpio-i2cwriteblockdata
   #:pigpio-i2creadblockdata
   #:pigpio-i2cblockprocesscall
   #:pigpio-i2creadi2cblockdata
   #:pigpio-i2cwritei2cblockdata
   #:pigpio-i2creaddevice
   #:pigpio-i2cwritedevice
   #:pigpio-i2czip
   #:pigpio-bbi2c2open
   #:pigpio-bbi2c2close
   #:pigpio-bbi2czip
   #:pigpio-gpioserialreadopen
   #:pigpio-gpioserialreadinvert
   #:pigpio-gpioserialread
   #:pigpio-gpioserialreadclose
   #:pigpio-seropen
   #:pigpio-serclose
   #:pigpio-serwritebyte
   #:pigpio-serreadbyte
   #:pigpio-serwrite
   #:pigpio-serread
   #:pigpio-serdataavailable

   ;; this is a special function applicable only to hardware device
   #:pigpio-i2cswitchcombined
   
   ))

#| 

the pigpio-XXX  symbols for export are generated using

(loop for (func args) in pigpio-ffi-universal::*exported-functions*
	       for sym = (string-downcase (string func))
	       do (format t  "  #:~A~%" sym))
  
|# 

(in-package pigpio-ffi-universal)

(defvar *default-pidev* nil)

(defstruct %pidev) ;; a parent class to make PIDEVs of same type
(defstruct (pidev/hardware (:include %pidev)))
(defstruct (pidev/daemon (:include %pidev))
  (host "localhost")
  (port "8888")
  (pigpio-pi nil)) ;; the connection id to pass to pigpiod ffi routines


       
(defun open-pidev/hardware (&key (set-default t))
  "Initialize hardware version of pigpio library, and return a
PIDEV/HARWARE object.  If SET-DEFAULT is true, then set
*DEFAULT-PIDEV* to the new device."
  (pigpio-ffi:pigpio-initialize)
  (let ((dev (make-pidev/hardware)))
    (when set-default (setf *default-pidev* dev))
    dev))
   

(defun open-pidev/daemon (&key (host "localhost") (port "8888") (set-default t))
  "Initialize daemon version of pigpio, and return a PIDEV/DAEMON
object.  If SET-DEFAULT is true, then set
*DEFAULT-PIDEV* to the new device."
  (let ((dev
	  (make-pidev/daemon
	   :host host
	   :port port
	   :pigpio-pi
	   (pigpiod-ffi:pigpiod-connect ;; will throw error on failure
	    :host host :port port))))
    (when set-default (setf *default-pidev* dev))
    dev))


(defun close-pidev (pidev &key (close-library nil))
  "Close a PIDEV.  For a hardware device, it shuts the library down only
if CLOSE-LIBRARY is true.   For the daemon device, it terminates
the connection."
  (declare (type %pidev pidev))
  (cond ((and (pidev/hardware-p pidev) close-library)
	 (pigpio-ffi:pigpio-shutdown))
	((pidev/daemon-p pidev)
	 (pigpiod-ffi:pigpiod-disconnect (pidev/daemon-pigpio-pi pidev)))))

(defparameter *exported-functions* nil)
 

;; using the pigpiod-name, pigpio-name, and argument list
;; of a universal pigpio/pigpiod function FUNC, construct a function of the form
;;
;; (pigpio-FUNC (arg1 arg2 ... &key pidev)
;;              (cond ((pidev/hardware-p pidev)
;;                    (.. call pigpio version of FUNC ..)
;;                   ((pidev/daemon-p pidev)
;;                    (... call pigpiod version of FUNC ..))))
;;                   
(defmacro construct-universal-pigpio-function
    (pigpiod-func-sym pigpio-func-sym argument-list)
  (let ((shared-name (intern
		      (string-upcase
		       (format nil "pigpio-~A" pigpio-func-sym))))
	(args ;; turn the arguments into symbols in this package
	  (mapcar
	   (lambda (sym) (intern (string sym)))
	   argument-list)))
    
    `(progn
       ;; save function description in *EXPORTED-FUNCTIONS*
       (push (list ',shared-name ',args) *exported-functions*)
       (defun ,shared-name (,@args &key (pidev *default-pidev*))
	 (declare (type %pidev pidev))
	 (cond ((pidev/hardware-p pidev)
		;(format t "(~A ~{~A ~})~%" ',pigpio-func-sym  (list ,@args))
		(,pigpio-func-sym ,@args))
	       ((pidev/daemon-p pidev)
		;(format t "(~A ~A ~{~A ~})~%" ',pigpiod-func-sym (pidev/daemon-pigpio-pi pidev) (list ,@args))
		(,pigpiod-func-sym (pidev/daemon-pigpio-pi pidev)  ,@args))
	       (t (error "ERROR - PIDEV is ~A - expect a PIDEV/HARDWARE or PIDEV/DAEMON" pidev)))))))

;; macro to loop through all functions in pigpiod-ffi:*function-list*
;; and make universal functions out of them
(defmacro construct-all-universal-pigpio-functions ()
  `(progn
    ,@(loop for ((pigpiod-func-sym pigpio-func-sym) args) in  pigpiod-ffi:*function-list*
	    collect `(construct-universal-pigpio-function
		      ,pigpiod-func-sym ,pigpio-func-sym ,args))))


;; and build all the functions
(construct-all-universal-pigpio-functions)

;; this function works only for local (hardware) device
(defun pigpio-i2cswitchcombined (setting &key pidev)
  (cond ((typep pidev 'pidev/hardware)
	 (pigpio-ffi:i2cswitchcombined setting))
	(t
	 (error "i2cSwitchCombined works only for local hardware PIDEV"))))

