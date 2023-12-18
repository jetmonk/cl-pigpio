

(defpackage pigpiod-ffi
  (:use #:cl #:pigpio-ffi-common-symbols) 
  (:export
   #:*function-list*  ;; list of exported functions by their pigpio/pigpiod common name

   ;; high level functions to connect and disconnect from daemon
   #:pigpiod-connect
   #:pigpiod-disconnect

   ;; underscores are kept in CFFI mappings of libpigpiod functions
   #:pigpio_start
   #:pigpio_stop
   #:set_mode
   #:get_mode
   #:set_pull_up_down
   #:gpio_read
   #:gpio_write
   
   #:set_PWM_dutycycle
   #:get_PWM_dutycycle
   #:set_PWM_range
   #:get_PWM_range
   #:get_PWM_real_range
   #:set_PWM_frequency
   #:get_PWM_frequency
   #:set_servo_pulsewidth
   #:get_servo_pulsewidth
   #:notify_open
   #:notify_pause
   #:notify_close
   #:set_watchdog
   #:set_noise_filter
   #:spi_open
   #:spi_read
   #:spi_write
   #:spi_xfer

   #:bb_spi_open
   #:bb_spi_close
   #:bb_spi_xfer

   #:i2c_open
   #:i2c_close
   #:i2c_write_quick
   #:i2c_write_byte
   #:i2c_read_byte
   #:i2c_write_byte_data
   #:i2c_write_word_data
   #:i2c_read_byte_data
   #:i2c_read_word_data
   #:i2_process_call
   #:i2c_write_block_data
   #:i2c_read_block_data
   #:i2c_block_process_call
   #:i2c_read_i2c_block_data
   #:i2c_write_i2c_block_data
   #:i2c_read_device
   #:i2c_write_device
   #:i2c_zip
   #:bb_i2c_open
   #:bb_i2c_close
   #:bb_i2c_zip
   
   #:bb_serial_read_open
   #:bb_serial_read_close
   #:bb_serial_invert
   #:bb_serial_read

   #:serial_open
   #:serial_close
   #:serial_write_byte
   #:serial_read_byte
   #:serial_write
   #:serial_read
   #:serial_data_available

   #:gpio_trigger
   #:set_watchdog
   #:set_noise_filter
   

 

   ))
   
   
   
   

(in-package pigpiod-ffi)


(cffi:define-foreign-library pigpiod
  (:unix "libpigpiod_if2.so")
  (t (:default "libpigpiod_if2")))


;; we try to ensure that the library is loaded just once
;; because it might contain saved state that is lost,
;; causing a crash
;; apparently, (cffi:use-foreign-library pigpiod)
;; will load it on every file reload
(defvar *pigpiod-initialized* nil)
(eval-when (:load-toplevel)
  (when (not *pigpiod-initialized*)
    (cffi:load-foreign-library 'pigpiod)))




;; this is the PI that pigpiod functions need, which
;; is the value that pigpio_start returns
(defvar *pigpiod-pi* nil)
(defvar *pigpiod-host*
  (or (when (find-symbol "PIGPIO-HOST" :cl-user)
	(symbol-value 'cl-user::pigpiod-host))
      (uiop/os:getenv "PIGPIO-HOST")
      "localhost")) ;; default
(defvar *pigpiod-port*
  (or (when (find-symbol "PIGPIO-PORT" :cl-user)
	(symbol-value 'cl-user::pigpiod-port))
      (uiop/os:getenv "PIGPIO-PORT")
      "8888")) ;; default

(cffi:defcfun ("pigpio_start" pigpio_start) :int
  (host :string)
  (port :string))

(cffi:defcfun ("pigpio_stop" pigpio_stop) :int
  (pigpi :int))
 

;; start pigpiod using host and port and fail on error
(defun pigpiod-connect (&key (host *pigpiod-host*) (port *pigpiod-port*))
  "Start a pigpiod daemon connection on HOST and PORT.
HOST defaults to CL-USER::PIGPIO-HOST, then environment variable
PIGPIO-HOST, then localhost.   PORT defaults to CL-USER::PIGPIO-PORT,
then environment PIGPIO-PORT, then 8888"
  (let ((pigpiod-pi (pigpio_start host (format nil "~A" port))))
    (when (minusp pigpiod-pi)
      (error "pigpio_start(~A,~A) returned ERROR ~A"
	     host port pigpiod-pi))
    pigpiod-pi))


;; shut down pigpiod using *pigpio-d* variable
(defun pigpiod-disconnect (pigpiod-pi &key (throw-error t))
  "Shut down PIGPIOD connection designated by PIGPIO-PI.
If THROW-ERROR is TRUE, then throw an error on failure.
This may be disabled for exit-hook shutdown."
  (let ((ret (pigpio_stop pigpiod-pi)))
  (when (and (minusp ret)
	     throw-error)
    (error "ERROR ~A on pigpio_stop(~A)" ret pigpiod-pi))))
	
  




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get the defconstants ausing
;;   cat /usr/include/pigpio.h  | grep "#define" |  \
;;      awk '{printf("(defparameter +%s+ %s)\n", $2,$3,$4)}'
;; as needed  (yes, could use cffi-grovel)
;;
(defconstant +PI_INPUT+    0)
(defconstant +PI_OUTPUT+   1)
(defconstant +PI_ALT0+     4)
(defconstant +PI_ALT1+     5)
(defconstant +PI_ALT2+     6)
(defconstant +PI_ALT3+     7)
(defconstant +PI_ALT4+     3)
(defconstant +PI_ALT5+     2)
;;
(defconstant +PI_PUD_OFF+  0)
(defconstant +PI_PUD_DOWN+ 1)
(defconstant +PI_PUD_UP+   2)


;; echo '#include <stdlib.h>' | cpp -I/usr/include  | grep size_t
;; ==> typedef long unsigned int size_t;
(cffi:defctype size_t :unsigned-long)

;; list of functions, their common pigpio synonyms, and their args as
;; (((func1 synonym) (arg1a arg1b ..))  (func2 (arg2a ..)))

(defvar *function-list* nil)

(eval-when (:compile-toplevel)
  (setf *function-list* nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

Macro to define PIGPIOD functions in a version that adds a leading pigpi variable


for example, "foo_bar(int pigpi, int arg1, double arg2)" is a C
function turns into FOO_BAR Lisp FFI function, and is is associated
with a common name FOOBAR (shared between pigpio and pigpiod)

(macroexpand '(pigpiod-ffi::defpigdfun ("foo_bar" foobar) :int
			(arg1 :int)
			(arg2 :double)))

expands into

(progn
 (pushnew (list (list 'foo_bar 'foobar) '(arg1 arg2)) ;; FOO_BAR is pigpiod name; FOOBAR is pigpio
          pigpiod-ffi:*function-list* :key #'caar)    ;; push description onto *FUNCTION-LIST*
 (cffi:defcfun ("foo_bar" foo_bar)      :int
   (pigpiod-ffi::pigpi :int)  ;; automatically added this pigpio specific variable
   (arg1 :int)
   (arg2 :double)))



|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defpigdfun ((c-pigpiod-name pigpio-name) return-type &rest args-desc)
  (let ((lisp-name ;; lisp name of CFFI C function
	  (intern (format nil "~A" (string-upcase c-pigpiod-name))))
	(args (mapcar 'first args-desc)))

    `(progn

       (pushnew (list (list ',lisp-name ',pigpio-name) ',args)
		*function-list*
		:key #'caar)
	   
       (cffi:defcfun (,c-pigpiod-name ,lisp-name) ,return-type
	 ,@(append (list (list 'pigpi :int)) args-desc)))))

     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GPIO routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;; See
;; http://www.raspberrypi.org/documentation/hardware/raspberrypi/bcm2835/BCM2835-ARM-Peripherals.pdf
;; page 102 for an overview of the modes.
(defpigdfun ("set_mode" gpiosetmode) :int
  (gpio   :unsigned-int)
  (mode   :unsigned-int)) ;; 0-7 

(defpigdfun ("get_mode" gpiogetmode) :int
  (gpio   :unsigned-int))

(defpigdfun ("set_pull_up_down" gpiosetpullupdown) :int
  (gpio   :unsigned-int)
  (pud    :unsigned-int)) ;; 0-2

(defpigdfun ("gpio_read" gpioread) :int
  (gpio   :unsigned-int))

(defpigdfun ("gpio_write" gpiowrite) :int
  (gpio   :unsigned-int)
  (level  :unsigned-int)) ;; 0-1

(defpigdfun ("set_PWM_dutycycle" gpiopwm) :int
  (gpio       :unsigned-int)
  (dutycycle  :unsigned-int)) ;; 0-255, unless reset using gpioSetPWMrange

(defpigdfun ("get_PWM_dutycycle" gpiogetpwmdutycycle) :int
  (gpio   :unsigned-int))

(defpigdfun ("set_PWM_range" gpiosetpwmrange) :int
  (gpio   :unsigned-int)
  (range  :unsigned-int)) ;; 25-40000

(defpigdfun ("get_PWM_range" gpiogetpwmrange) :int
  (gpio   :unsigned-int))

(defpigdfun ("get_PWM_real_range" gpiogetpwmrealrange) :int
  (gpio   :unsigned-int))

(defpigdfun ("set_PWM_frequency" gpiosetpwmfrequency) :int
  (gpio       :unsigned-int)
  (frequency  :unsigned-int))

(defpigdfun ("get_PWM_frequency" gpiogetpwmfrequency) :int
  (gpio       :unsigned-int))

(defpigdfun ("set_servo_pulsewidth" gpioservo) :int
  (gpio       :unsigned-int)
  (pulsewidth :unsigned-int))

(defpigdfun ("get_servo_pulsewidth" gpiogetservopulsewidth) :int
  (gpio       :unsigned-int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the following are NOT the same as pigpio
(defpigdfun ("notify_open" gpionotifyopen) :int
	   )

(defpigdfun ("notify_begin" gpionotifybegin) :int
  (handle     :unsigned-int)
  (nbits :uint32))

(defpigdfun ("notify_pause" gpionotifypause) :int
  (handle     :unsigned-int))

(defpigdfun ("notify_close" gpionotifyclose) :int
  (handle     :unsigned-int))

(defpigdfun ("set_glitch_filter" gpioglitchfilter) :int
  (user-gpio :unsigned-int)
  (steady    :unsigned-int))

(defpigdfun ("set_noise_filter" gpionoisefilter) :int
  (user-gpio :unsigned-int)
  (steady    :unsigned-int)
  (active    :unsigned-int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc GPIO functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpigdfun ("gpio_trigger" gpiotrigger) :int
  (user-gpio :unsigned-int)
  (pulse-len :unsigned-int)
  (level     :unsigned-int))

(defpigdfun ("set_watchdog" gpiosetwatchdog) :int
  (user-gpio :unsigned-int)
  (timeout   :unsigned-int))

;; end of functions not in pigpio
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;; (defpigdfun ("gpioSetISRFunc" gpiosetisrfunc) :int
;;   (gpio       :unsigned-int)
;;   (edge       :unsigned-int) ;;  RISING_EDGE, FALLING_EDGE, or EITHER_EDGE
;;   (function   :pointer)) ;; function(int gpio, int level, uint32_t tick)

;; (defpigdfun ("gpioSetISRFuncEx" gpiosetisrfuncex) :int
;;   (gpio       :unsigned-int)
;;   (edge       :unsigned-int) ;;  RISING_EDGE, FALLING_EDGE, or EITHER_EDGE
;;   (function   :pointer) ;; function(int gpio, 
;;                         ;;   int level, uint32_t tick, void *userdata)
;;   (userdata   :pointer))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SPI routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpigdfun ("spi_open" spiopen) :int
  (spichan :unsigned-int)
  (baud :unsigned-int)
  (spiflags :unsigned-int))

(defpigdfun ("spi_close" spiclose) :int
  (handle :unsigned-int))

(defpigdfun ("spi_read" spiread) :int
  (handle :unsigned-int)
  (buf (:pointer :unsigned-char))
  (count :unsigned-int))

(defpigdfun ("spi_write" spiwrite) :int
  (handle :unsigned-int)
  (buf (:pointer :unsigned-char))
  (count :unsigned-int))


(defpigdfun ("spi_xfer" spixfer) :int
  (handle :unsigned-int)
  (txbuf (:pointer :unsigned-char))
  (rxbuf (:pointer :unsigned-char))
  (count :unsigned-int))

;; bit-banged SPI 

(defpigdfun ("bb_spi_open" bbspiopen) :int
  (cs   :unsigned-int)
  (miso :unsigned-int)
  (mosi :unsigned-int)
  (sclk :unsigned-int)
  (spiflags :unsigned-int))

 (defpigdfun ("bb_spi_close" bbspiclose) :int
   (cs   :unsigned-int))

 (defpigdfun ("bb_spi_xfer" bbspixfer) :int
   (s     :unsigned-int)
   (txbuf (:pointer :unsigned-char))
   (rxbuf (:pointer :unsigned-char))
   (count :unsigned-int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I2C
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpigdfun ("i2c_open" i2copen) :int
  (i2cbus   :unsigned-int)
  (i2caddr  :unsigned-int)
  (i2cflags :unsigned-int))


(defpigdfun ("i2c_close" i2cclose) :int
  (handle :unsigned-int))

(defpigdfun ("i2c_write_quick" i2cwritequick) :int
  (handle :unsigned-int)
  (bit    :unsigned-int))

(defpigdfun ("i2c_write_byte" i2cwritebyte) :int
  (handle :unsigned-int)
  (bval   :unsigned-int))

(defpigdfun ("i2c_read_byte" i2creadbyte) :int
  (handle :unsigned-int))

(defpigdfun ("i2c_write_byte_data" i2cwritebytedata) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int)
  (bval   :unsigned-int))

(defpigdfun ("i2c_write_word_data" i2cwriteworddata) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int)
  (wval   :unsigned-int))

(defpigdfun ("i2c_read_byte_data" i2creadbytedata) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int))

(defpigdfun ("i2c_read_word_data" i2creadworddata) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int))

(defpigdfun ("i2_process_call" i2cprocesscall) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int)
  (wval   :unsigned-int))

(defpigdfun ("i2c_write_block_data" i2cwriteblockdata) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int)
  (buf    (:pointer :char))
  (count  :unsigned-int))

(defpigdfun ("i2c_read_block_data" i2creadblockdata) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int)
  (buf    (:pointer :char))
  (count  :unsigned-int))

(defpigdfun ("i2c_block_process_call" i2cblockprocesscall) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int)
  (buf    (:pointer :char))
  (count  :unsigned-int))

;; differs from above i2cReadBlockData because it has COUNT
(defpigdfun ("i2c_read_i2c_block_data" i2creadi2cblockdata) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int)
  (buf    (:pointer :char))
  (count  :unsigned-int))

;; differs from above i2cWriteBlockData because it has COUNT
(defpigdfun ("i2c_write_i2c_block_data" i2cwritei2cblockdata) :int
  (handle :unsigned-int)
  (i2creg :unsigned-int)
  (buf    (:pointer :char))
  (count  :unsigned-int))


(defpigdfun ("i2c_read_device" i2creaddevice) :int
  (handle :unsigned-int)
  (buf    (:pointer :char))
  (count  :unsigned-int))

(defpigdfun ("i2c_write_device" i2cwritedevice) :int
  (handle :unsigned-int)
  (buf    (:pointer :char))
  (count  :unsigned-int))

;;(defpigdfun ("i2cSwitchCombined" i2cswitchcombined) :void
;;  (setting :int))

;; (defpigdfun ("i2cSegments" i2csegments) :int
;;   (handle :unsigned-int)
;;   (segs :pointer) ;; an array of I2C segments
;;   (numsegs :unsigned-int))

(defpigdfun ("i2c_zip" i2czip) :int
  (handle :unsigned-int)
  (inbuf  (:pointer :char))
  (inlen  :unsigned-int)
  (outbuf (:pointer :char))
  (outlen :unsigned-int)) 

;; bitbang I2C

(defpigdfun ("bb_i2c_open" bbi2c2open) :int
  (sda :unsigned-int)
  (scl :unsigned-int)
  (baud :unsigned-int))

(defpigdfun ("bb_i2c_close" bbi2c2close) :int
  (sda :unsigned-int))

(defpigdfun ("bb_i2c_zip" bbi2czip) :int
  (handle :unsigned-int)
  (inbuf  (:pointer :char))
  (inlen  :unsigned-int)
  (outbuf (:pointer :char))
  (outlen :unsigned-int))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serial bitbang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpigdfun ("bb_serial_read_open" gpioserialreadopen) :int
  (user-gpio :unsigned-int)
  (baud      :unsigned-int)
  (data-bits :unsigned-int))

(defpigdfun ("bb_serial_invert" gpioserialreadinvert) :int
  (user-gpio :unsigned-int)
  (invert    :unsigned-int)) ;; 0-1



(defpigdfun ("bb_serial_read" gpioserialread) :int
  (user-gpio :unsigned-int)
  (buf       :pointer)
  (bufsize   size_t))

(defpigdfun ("bb_serial_read_close" gpioserialreadclose) :int
  (user-gpio :unsigned-int))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serial
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpigdfun ("serial_open" seropen) :int
  (sertty (:pointer :char))
  (baud   :unsigned-int)
  (serflags :unsigned-int))

(defpigdfun ("serial_close" serclose) :int
  (handle :unsigned-int))

(defpigdfun ("serial_write_byte" serwritebyte) :int
  (handle :unsigned-int)
  (bval   :unsigned-int))

(defpigdfun ("serial_read_byte" serreadbyte) :int
  (handle :unsigned-int))

(defpigdfun ("serial_write" serwrite) :int
  (handle :unsigned-int)
  (buf    (:pointer :char))
  (count :unsigned-int))
	  
(defpigdfun ("serial_read" serread) :int
  (handle :unsigned-int)
  (buf    (:pointer :char))
  (count :unsigned-int))

(defpigdfun ("serial_data_available" serdataavailable) :int
  (handle :unsigned-int))







