

(defpackage cl-pigpio
  (:use #:cl #:pigpio-ffi-universal #:pigpio-codes)
  (:import-from
   #:pigpio-ffi-universal
   #:open-pidev/hardware
   #:open-pidev/daemon)
      
  (:export
   #:*default-pidev*
   #:pidev/hardware #:pidev/daemon 

   #:open-pidev/hardware
   #:open-pidev/daemon
   ;;
   ;; cl-pigpio-gpio.lisp
   #:set-gpio-mode
   #:get-gpio-mode
   #:set-gpio-pullupdown
   #:set-gpio-pullupdown
   #:gpio-read
   #:gpio-write
   
   ;; cl-pigpio-spi.lisp
   #:spi-open
   #:spi-close
   #:spi-read-into-array
   #:spi-read-into-integer
   #:spi-write-from-array
   #:spi-write-from-integer
   #:spi-xfer
   #:spi-xfer-with-integers

   ;; cl-pigpio-i2c.lisp
   #:i2c-open
   #:i2c-close
   #:i2c-write-bit
   #:i2c-write-byte
   #:i2c-read-byte
   #:i2c-write-byte-to-register
   #:i2c-read-byte-from-register
   #:i2c-write-word-to-register
   #:i2c-read-word-from-register
   #:i2c-read-and-write-word-for-register
   #:i2c-write-bytes-to-register
   #:i2c-read-bytes-from-register
   #:i2c-write-and-read-bytes-for-register
   #:i2c-write-bytes
   #:i2c-read-bytes
   #:i2c-switch-combined
   #:i2c-zip
   
   )) 

