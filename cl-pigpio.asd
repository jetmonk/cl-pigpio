
(asdf:defsystem cl-pigpio
  :depends-on (cffi pigpio-ffi)
  :components 
  ((:file "cl-pigpio-package")
   (:file "cl-pigpio-utils" :depends-on ("cl-pigpio-package"))
   (:file "cl-pigpio-gpio"  :depends-on ("cl-pigpio-package"))
   (:file "cl-pigpio-spi" :depends-on ("cl-pigpio-package" "cl-pigpio-utils"))
   (:file "cl-pigpio-i2c" :depends-on ("cl-pigpio-package" "cl-pigpio-utils"))))

