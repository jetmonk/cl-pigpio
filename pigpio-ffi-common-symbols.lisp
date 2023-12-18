

;; this is a package designed to hold a set symbols that are common
;; to pigpio-ffi and pigpiod-ffi, so they are identical

(defpackage pigpio-ffi-common-symbols
  (:export

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
