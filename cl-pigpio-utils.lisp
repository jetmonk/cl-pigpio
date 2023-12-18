

(in-package cl-pigpio)


(declaim (inline %integer-to-foreign %foreign-to-integer))

;; put an integer into foreign memory, MSB first
(defun %integer-to-foreign (n byte-ptr nbytes)
  (declare (type (unsigned-byte 64) n)
	   (type cffi:foreign-pointer byte-ptr)
	   (type (integer 1 8) nbytes)
	   (optimize speed))
  (loop with nn of-type (unsigned-byte 64) = n
	for i from (1- nbytes) downto 0
	for byte = (logand nn #xff)
	do
	   (setf (cffi:mem-aref byte-ptr :unsigned-char i) byte)
	   (setf nn (ash nn -8))))


;; put an integer into foreign memory, MSB first
(defun %foreign-to-integer (byte-ptr nbytes)
  (declare (type cffi:foreign-pointer byte-ptr)
	   (type (integer 1 8) nbytes)
	   (optimize speed))
  (loop with n of-type (unsigned-byte 64) = 0
	for i below nbytes
	for jbyte = (- nbytes i 1)
	for byte = (cffi:mem-aref byte-ptr :unsigned-char i) 
	do
	   (setf n (logior n (ash byte (* 8 jbyte))))
	finally (return n)))
  
#|

Test of above:

(cffi:with-foreign-object (ptr :unsigned-char 8)
	   (cl-pigpio::%integer-to-foreign 12345678 ptr 5)
	   (cl-pigpio::%foreign-to-integer ptr 5))

|#
