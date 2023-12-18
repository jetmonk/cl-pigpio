
(asdf:defsystem pigpio-ffi
  :depends-on (cffi)
  :components ((:file "pigpio-ffi-common-symbols")
	       (:file "pigpio-ffi" :depends-on ("pigpio-ffi-common-symbols"))
	       (:file "pigpiod-ffi" :depends-on ("pigpio-ffi-common-symbols"))
	       (:file "pigpio-codes")
	       (:file "pigpio-ffi-universal" :depends-on ("pigpio-ffi" "pigpiod-ffi" "pigpio-codes"))
	       ))

