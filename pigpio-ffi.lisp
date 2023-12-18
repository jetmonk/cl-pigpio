

(defpackage pigpio-ffi
  (:use #:cl #:pigpio-ffi-common-symbols)
  (:export
   #:pigpio-initialize
   #:pigpio-shutdown
   
   #:gpioinitialise
   #:gpiocfggetinternals
   #:gpiocfgsetinternals
   #:gpioterminate

   #:gpiosetmode
   #:gpiogetmode
   #:gpiosetpullupdown
   #:gpioread
   #:gpiowrite

   #:gpiopwm
   #:gpiogetpwmdutycycle
   #:gpiosetpwmrange
   #:gpiogetpwmrange
   #:gpiogetpwmrealrange
   #:gpiosetpwmfrequency
   #:gpiogetpwmfrequency


   #:gpioservo
   #:gpiogetservopulsewidth
   #:gpiosetalertfunc
   #:gpiosetalertfuncex
   #:gpiosetisrfunc
   #:gpiosetisrfuncex
   
   #:spiopen
   #:spiclose
   #:spiread
   #:spiwrite
   #:spixfer

   #:bbspiopen
   #:bbspiclose
   #:bbspixfer

   #:i2copen
   #:i2cclose
   #:i2cwritequick
   #:i2cwritebyte
   #:i2creadbyte
   #:i2cwritebytedata
   #:i2cwriteworddata
   #:i2creadbytedata
   #:i2creadworddata
   #:i2cprocesscall 
   #:i2cwriteblockdata
   #:i2creadblockdata
   #:i2cwritei2cblockdata
   #:i2creadi2cblockdata
   #:i2cblockprocesscall
   #:i2creaddevice
   #:i2cwritedevice
   #:i2cswitchcombined
   #:i2csegments
   #:i2czip

   #:bbi2c2open
   #:bbi2c2close
   #:bbi2czip

   #:gpioserialreadopen
   #:gpioserialreadinvert
   #:gpioserialread
   #:gpioserialreadclose

   #:seropen
   #:serclose
   #:serwritebyte
   #:serreadbyte
   #:serwrite
   #:serread
   #:serdataavailable

   #:gpiotrigger
   #:gpiosetwatchdog
   #:gpionoisefilter

   #:gpionotifyopen
   #:gpionotifybegin
   #:gpionotifypause
   #:gpionotifyclose
   #:gpiotrigger
   #:gpiosetwatchdog
   #:gpioglitchfilter
   #:gpionoisefilter
   
   ))
   
   
   
   

(in-package pigpio-ffi)


(cffi:define-foreign-library pigpio
  (:unix "libpigpio.so")
  (t (:default "libpigpio")))


(cffi:define-foreign-library pigpiod
  (:unix "libpigpiod_if.so")
  (t (:default "libpigpio_if")))

(defvar *pigpio-initialized* nil)


;; ensure that it is loaded just once, ever
(eval-when (:load-toplevel)
  (when (not *pigpio-initialized*)
    (cffi:load-foreign-library 'pigpio)))


(cffi:defcfun ("gpioInitialise" gpioinitialise) :int)
(cffi:defcfun ("gpioCfgGetInternals" gpiocfggetinternals) :int)
(cffi:defcfun ("gpioCfgSetInternals" gpiocfgsetinternals) :int
  (cfg :uint32))

;; resets DMA channels, terminates threads
(cffi:defcfun ("gpioTerminate" gpioterminate) :void) 


(defun pigpio-shutdown ()
  "Shut down pigpio using gpioterminate; resets DMA channels and
  terminates threads"
  (when (not *pigpio-initialized*)
    (gpioterminate)
    (setf *pigpio-initialized* nil)))

;; set up init of pigpio library, then set up termination exit hook
(defun pigpio-initialize ()
  "Initialize pigpio using gpioinitialized, but before this shuts
down gpio interrupts for compatability with Lisp."
  (when (not *pigpio-initialized*)
    
    ;; turn off interrupts because this messes up sbcl
    (let* ((cfg (gpiocfggetinternals))
	   (new-cfg (logior (ash 1 10) cfg)))
      (gpiocfgsetinternals new-cfg))
    
    (let ((retval (gpioinitialise)))
      (when (minusp retval)
	(error "gpioInitialize returned error ~A" retval)))

    #+sbcl (push #'pigpio-shutdown sb-ext:*exit-hooks*)
    #+ccl  (push #'pigpio-shutdown ccl:*lisp-cleanup-functions*)
    #+ecl  (push #'pigpio-shutdown si:*exit-hooks*)
    #+allegro (push (list 'funcall #'pigpio-shutdown)
		    sys:*exit-cleanup-forms*)
    #+clisp  (push #'pigpio-shutdown custom:*fini-hooks*)
    #+cmu    (push #'pigpio-shutdown lisp::*cleanup-functions*)
    
    (defvar *pigpio-initialized* nil) 
    (setf  *pigpio-initialized* t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *function-list* nil)

;; echo '#include <stdlib.h>' | cpp -I/usr/include  | grep size_t
;; ==> typedef long unsigned int size_t;
(cffi:defctype size_t :unsigned-long)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GPIO routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See
;; http://www.raspberrypi.org/documentation/hardware/raspberrypi/bcm2835/BCM2835-ARM-Peripherals.pdf
;; page 102 for an overview of the modes.
(cffi:defcfun ("gpioSetMode" gpiosetmode) :int
  (gpio   :unsigned-int)
  (mode   :unsigned-int)) ;; 0-7 

(cffi:defcfun ("gpioGetMode" gpiogetmode) :int
  (gpio   :unsigned-int))

(cffi:defcfun ("gpioSetPullUpDown" gpiosetpullupdown) :int
  (gpio   :unsigned-int)
  (pud    :unsigned-int)) ;; 0-2

(cffi:defcfun ("gpioRead" gpioread) :int
  (gpio   :unsigned-int))

(cffi:defcfun ("gpioWrite" gpiowrite) :int
  (gpio   :unsigned-int)
  (level  :unsigned-int)) ;; 0-1

(cffi:defcfun ("gpioPWM" gpiopwm) :int
  (gpio       :unsigned-int)
  (dutycycle  :unsigned-int)) ;; 0-255, unless reset using gpioSetPWMrange

(cffi:defcfun ("gpioGetPWMdutycycle" gpiogetpwmdutycycle) :int
  (gpio   :unsigned-int))

(cffi:defcfun ("gpioSetPWMrange" gpiosetpwmrange) :int
  (gpio   :unsigned-int)
  (range  :unsigned-int)) ;; 25-40000

(cffi:defcfun ("gpioGetPWMrange" gpiogetpwmrange) :int
  (gpio   :unsigned-int))

(cffi:defcfun ("gpioGetPWMrealRange" gpiogetpwmrealrange) :int
  (gpio   :unsigned-int))

(cffi:defcfun ("gpioSetPWMfrequency" gpiosetpwmfrequency) :int
  (gpio       :unsigned-int)
  (frequency  :unsigned-int))

(cffi:defcfun ("gpioGetPWMfrequency" gpiogetpwmfrequency) :int
  (gpio       :unsigned-int))

(cffi:defcfun ("gpioServo" gpioservo) :int
  (gpio       :unsigned-int)
  (pulsewidth :unsigned-int))

(cffi:defcfun ("gpioGetServoPulsewidth" gpiogetservopulsewidth) :int
  (gpio       :unsigned-int))

(cffi:defcfun ("gpioSetAlertFunc" gpiosetalertfunc) :int
  (gpio       :unsigned-int)
  (function   :pointer)) ;; function(int gpio, int level, uint32_t tick)

(cffi:defcfun ("gpioSetAlertFuncEx" gpiosetalertfuncex) :int
  (gpio       :unsigned-int)
  (function   :pointer) ;; function(int gpio, int level, uint32_t tick, void *userdata)
  (userdata   :pointer))

(cffi:defcfun ("gpioSetISRFunc" gpiosetisrfunc) :int
  (gpio       :unsigned-int)
  (edge       :unsigned-int) ;;  RISING_EDGE, FALLING_EDGE, or EITHER_EDGE
  (function   :pointer)) ;; function(int gpio, int level, uint32_t tick)

(cffi:defcfun ("gpioSetISRFuncEx" gpiosetisrfuncex) :int
  (gpio       :unsigned-int)
  (edge       :unsigned-int) ;;  RISING_EDGE, FALLING_EDGE, or EITHER_EDGE
  (function   :pointer) ;; function(int gpio, int level, uint32_t tick, void *userdata)
  (userdata   :pointer))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SPI routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:defcfun ("spiOpen" spiopen) :int
  (spichan :unsigned-int)
  (baud :unsigned-int)
  (spiflags :unsigned-int))

(cffi:defcfun ("spiClose" spiclose) :int
  (handle :unsigned-int))

(cffi:defcfun ("spiRead" spiread) :int
  (handle :unsigned-int)
  (buf (:pointer :unsigned-char))
  (count :unsigned-int))

(cffi:defcfun ("spiWrite" spiwrite) :int
  (handle :unsigned-int)
  (buf (:pointer :unsigned-char))
  (count :unsigned-int))


(cffi:defcfun ("spiXfer" spixfer) :int
  (handle :unsigned-int)
  (txbuf (:pointer :unsigned-char))
  (rxbuf (:pointer :unsigned-char))
  (count :unsigned-int))

;; bit-banged SPI 

(cffi:defcfun ("bbSPIOpen" bbspiopen) :int
  (cs   :unsigned-int)
  (miso :unsigned-int)
  (mosi :unsigned-int)
  (sclk :unsigned-int)
  (spiflags :unsigned-int))

(cffi:defcfun ("bbSPICLose" bbspiclose) :int
  (cs   :unsigned-int))

(cffi:defcfun ("bbspiXfer" bbspixfer) :int
  (s     :unsigned-int)
  (txbuf (:pointer :unsigned-char))
  (rxbuf (:pointer :unsigned-char))
  (count :unsigned-int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I2C
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun ("i2cOpen" i2copen) :int
  (i2cbus   :unsigned-int)
  (i2caddr  :unsigned-int)
  (i2cflags :unsigned-int))

(cffi:defcfun ("i2cClose" i2cclose) :int
  (handle :unsigned-int))

(cffi:defcfun ("i2cWriteQuick" i2cwritequick) :int
  (handle :unsigned-int)
  (bit    :unsigned-int))

(cffi:defcfun ("i2cWriteByte" i2cwritebyte) :int
  (handle :unsigned-int)
  (bval   :unsigned-int))

(cffi:defcfun ("i2cReadByte" i2creadbyte) :int
  (handle :unsigned-int))

(cffi:defcfun ("i2cWriteByteData" i2cwritebytedata) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int)
  (bval   :unsigned-int))

(cffi:defcfun ("i2cWriteWordData" i2cwriteworddata) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int)
  (wval   :unsigned-int))

(cffi:defcfun ("i2cReadByteData" i2creadbytedata) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int))

(cffi:defcfun ("i2cReadWordData" i2creadworddata) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int))

(cffi:defcfun ("i2ProcessCall" i2cprocesscall) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int)
  (wval   :unsigned-int))

(cffi:defcfun ("i2cWriteBlockData" i2cwriteblockdata) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int)
  (buf    (:pointer :char))
  (count  :unsigned-int))

(cffi:defcfun ("i2cReadBlockData" i2creadblockdata) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int)
  (buf    (:pointer :char))
  (count  :unsigned-int))

(cffi:defcfun ("i2cBlockProcessCall" i2cblockprocesscall) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int)
  (buf    (:pointer :char))
  (count  :unsigned-int))
 
;; differs from above i2cReadBlockData because it has COUNT
(cffi:defcfun ("i2cReadI2CBlockData" i2creadi2cblockdata) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int)
  (buf    (:pointer :char))
  (count  :unsigned-int))

;; differs from above i2cWriteBlockData because it has COUNT
(cffi:defcfun ("i2cWriteI2CBlockData" i2cwritei2cblockdata) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int)
  (buf    (:pointer :char))
  (count  :unsigned-int))


(cffi:defcfun ("i2cReadDevice" i2creaddevice) :int
  (handle :unsigned-int)
  (buf    (:pointer :char))
  (count  :unsigned-int))

(cffi:defcfun ("i2cWriteDevice" i2cwritedevice) :int
  (handle :unsigned-int)
  (buf    (:pointer :char))
  (count  :unsigned-int))

(cffi:defcfun ("i2cSwitchCombined" i2cswitchcombined) :void
  (setting :int))

(cffi:defcfun ("i2cSegments" i2csegments) :int
  (handle :unsigned-int)
  (segs :pointer) ;; an array of I2C segments
  (numsegs :unsigned-int))

(cffi:defcfun ("i2cZip" i2czip) :int
  (handle :unsigned-int)
  (inbuf  (:pointer :char))
  (inlen  :unsigned-int)
  (outbuf (:pointer :char))
  (outlen :unsigned-int)) 

(cffi:defcfun ("bbI2COpen" bbi2c2open) :int
  (sda :unsigned-int)
  (scl :unsigned-int)
  (baud :unsigned-int))

(cffi:defcfun ("bbI2Close" bbi2c2close) :int
  (sda :unsigned-int))

(cffi:defcfun ("bbi2cZip" bbi2czip) :int
  (handle :unsigned-int)
  (inbuf  (:pointer :char))
  (inlen  :unsigned-int)
  (outbuf (:pointer :char))
  (outlen :unsigned-int))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serial bitbang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun ("gpioSerialReadOpen" gpioserialreadopen) :int
  (user-gpio :unsigned-int)
  (baud      :unsigned-int)
  (data-bits :unsigned-int))

(cffi:defcfun ("gpioSerialReadInvert" gpioserialreadinvert) :int
  (user-gpio :unsigned-int)
  (invert    :unsigned-int)) ;; 0-1



(cffi:defcfun ("gpioSerialRead" gpioserialread) :int
  (user-gpio :unsigned-int)
  (buf       :pointer)
  (bufsize   size_t))

(cffi:defcfun ("gpioSerialReadClose" gpioserialreadclose) :int
  (user-gpio :unsigned-int))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serial
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:defcfun ("serOpen" seropen) :int
  (sertty (:pointer :char))
  (baud   :unsigned-int)
  (serflags :unsigned-int))

(cffi:defcfun ("serClose" serclose) :int
  (handle :unsigned-int))

(cffi:defcfun ("serWriteByte" serwritebyte) :int
  (handle :unsigned-int)
  (bval   :unsigned-int))

(cffi:defcfun ("serReadByte" serreadbyte) :int
  (handle :unsigned-int))

(cffi:defcfun ("serWrite" serwrite) :int
  (handle :unsigned-int)
  (buf    (:pointer :char))
  (count :unsigned-int))
	  
(cffi:defcfun ("serRead" serread) :int
  (handle :unsigned-int)
  (buf    (:pointer :char))
  (count :unsigned-int))

(cffi:defcfun ("serDataAvailable" serdataavailable) :int
  (handle :unsigned-int))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc GPIO functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun ("gpioNotifyOpen" gpionotifyopen) :int
    )

(cffi:defcfun ("gpioNotifyBegin" gpionotifybegin) :int
  (handle     :unsigned-int)
  (nbits :uint32))

(cffi:defcfun ("gpioNotifyPause" gpionotifypause) :int
  (handle     :unsigned-int))

(cffi:defcfun ("gpioNotifyClose" gpionotifyclose) :int
  (handle     :unsigned-int))


(cffi:defcfun ("gpioTrigger" gpiotrigger) :int
  (user-gpio :unsigned-int)
  (pulse-len :unsigned-int)
  (level     :unsigned-int))

(cffi:defcfun ("gpioSetWatchdog" gpiosetwatchdog) :int
  (user-gpio :unsigned-int)
  (timeout   :unsigned-int))

(cffi:defcfun ("gpioGlitchFilter" gpioglitchfilter) :int
  (user-gpio :unsigned-int)
  (steady    :unsigned-int))

(cffi:defcfun ("gpioNoiseFilter" gpionoisefilter) :int
  (user-gpio :unsigned-int)
  (steady    :unsigned-int)    ;; microsec
  (active    :unsigned-int))   ;; microsec



